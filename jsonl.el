;;; jsonl.el --- Local editing of JSON files -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Ian Wahbe

;; Author: Ian Wahbe
;; URL: https://github.com/iwahbe/jsonl
;; Version: 0.1.0
;; Package-Requires: ((emacs "26.1"))

;;; Commentary:

;; Provides functions to edit JSON files. So far these include
;; - `jsonl-path': Display the path to a point in a JSON file.
;; - `jsonl-edit-string': A connivance function for editing a JSON string in a separate buffer.

;;; Code:

(require 'cl-lib)

(defvar jsonl-string-face 'font-lock-string-face
  "The face by which `jsonl' can identify JSON string values.")

(defvar jsonl-key-face 'font-lock-keyword-face
  "The face by which `jsonl' can identify JSON keys.")

(defvar jsonl-ignore-font-lock nil
  "Always ignore symbol `font-lock-mode'.
If non-nil, `jsonl-string-face' and `jsonl-key-face' are
ignored.")

(defun jsonl-path (&optional pos buffer)
  "Return the JSON path (as a list) of POINT in BUFFER.
It is assumed that BUFFER is entirely JSON and that the json is
valid from POS to `point-min'.

For example
    { \"foo\": [ { \"bar\": █ }, { \"fizz\": \"buzz\" } ] }
with pos at █ should yield '(\"foo\" 0 \"bar\")

`jsonl-path' is optimized to work on very large json files (35 MiB+).
This optimization is achieved by
a. parsing as little of the file as necessary to find the path and
b. leveraging C code whenever possible."
  (interactive)
  (with-current-buffer (or buffer (current-buffer))
    (save-excursion
      (goto-char (or pos (point)))
      (jsonl--correct-starting-path)
      (let ((result (jsonl--reconstruct-path (jsonl--path t))) display)
        (when (called-interactively-p 'interactive)
          (setq display (jsonl--display-path result))
          (message "Path: %s" display)
          (kill-new display))
        result))))

(defmacro jsonl--defun-back (name arg)
  "Define a =-back-NAME (NAME) function with predicate condition ARG."
  (declare (indent defun))
  `(defun ,(intern (format "jsonl--back-%s" name)) ()
     (while (and (not (bobp)) ,arg)
       (backward-char))))

(jsonl--defun-back whitespace
  (or (= (char-after) ?\ )
      (= (char-after) ?\t)
      (= (char-after) ?\n)
      (= (char-after) ?\r)))

(jsonl--defun-back number
  (and (<= (char-after) ?9)
       (>= (char-after) ?0)))

(defun jsonl--back-string (&optional expected-face)
  "Move back a string, starting at the ending \".
If the string is highlighted with the `face' EXPECTED-FACE, then
use the face to define the scope of the region. If the string
does not have face EXPECTED-FACE, the string is manually parsed."
  (unless (= (char-after) ?\")
    (error "Expected to be at \""))
  (let ((match (and expected-face (jsonl--get-font-lock-region nil nil 'face expected-face))))
    (if match
        ;; The region is highlighted, so just jump to the beginning of that.
        (progn (goto-char (1- (car match))) match)
      ;; The region is not highlighted
      (setq match (point))
      (backward-char)
      (jsonl--string-scan-back)
      (cons (1+ (point)) (1+ match)))))

(defun jsonl--string-scan-back ()
  "Scan backwards from `point' looking for the beginning of a string.
`jsonl--string-scan-back' will not move between lines. A non-nil
result is returned if a string beginning was found."
  (let (done)
    (while (not (or done (bolp)))
      ;; Backtrack through the string until an unescaped " is found.
      (if (not (= (char-after) ?\"))
          (backward-char)
        (let (escaped (anchor (point)))
          (while (= (char-before) ?\\)
            (backward-char)
            (setq escaped (not escaped)))
          (if escaped
              (backward-char)
            (goto-char (1- anchor))
            (setq done t)))))
    done))

(defun jsonl--string-scan-forward ()
  "Find the front of the current string.
`jsonl--string-scan-back' is called internally. When a string is found
the position of the final \" is returned and the point is moved
to just past that. When no string is found, nil is returned."
  (let ((start (jsonl--pos-in-stringp))
        escaped
        done)
    (when start
      (goto-char (1+ start))
      (while (not (or done (eolp)))
        (cond
         ((= (char-after) ?\\)
          (setq escaped (not escaped)))
         ((and (= (char-after) ?\") (not escaped))
          (setq done (point)))
         (t (setq escaped nil)))
        (forward-char))
      (and done (>= done start) done))))

(defun jsonl--pos-in-stringp ()
  "Determine if `point' is in a string (either a key or a value).
`=-pos-in-string' will only examine between `point' and
`beginning-of-line'. When non-nil, the starting position of the
discovered string is returned."
  (save-excursion
    (let (in-string start)
      (while (jsonl--string-scan-back)
        (when (not start)
          (setq start (1+ (point))))
        (setq in-string (not in-string)))
      (when in-string start))))

(defun jsonl--pos-in-keyp ()
  "Determine if `point' is a JSON string key.
If a non-nil, the position of the end of the string is returned."
  ;; A string is considered to be a key iff it is a string followed by some
  ;; amount of whitespace (maybe none) and then a :.
  (save-excursion
    (when (jsonl--string-scan-forward)
      (let ((end (point)))
        (while (or (= (char-after) ?\ )
                   (= (char-after) ?\t)
                   (= (char-after) ?\n)
                   (= (char-after) ?\r))
          (forward-char))
        (and (= (char-after) ?:) end)))))

(defun jsonl--pos-in-valuep ()
  "Determine if `point' is a JSON string value.
If a non-nil, the position of the beginning of the string is
returned."
  (and (not (jsonl--pos-in-keyp)) (jsonl--pos-in-stringp)))

(defun jsonl--string-at-pos (&optional pos)
  "Return (start . end) for a string at POS if it exists.
Otherwise nil is returned. POS defaults to `ponit'."
  (save-excursion
    (when pos
      (goto-char pos))
    (let ((start (jsonl--pos-in-stringp)) end)
      (when start
        (setq end (jsonl--string-scan-forward)))
      (when (and start end)
        (cons start (1+ end))))))

(defun jsonl--reconstruct-path (input)
  "Cleanup INPUT as the result of `jsonl--path'."
  (let (path seen-key)
    (seq-do (lambda (element)
              (cond
               ((stringp element)
                (unless seen-key (setq path (cons element path)
                                       seen-key t)))
               ((equal element 'object) ;; A marker element, does nothing
                (setq seen-key nil))
               (t
                (setq seen-key nil
                      path (cons element path)))
               ))
            input)
    path))

(defun jsonl--display-path (path)
  "Convert the reconstructed JSON path PATH to a string."
  (mapconcat
   (lambda (el)
     (cond
      ((numberp el) (format "[%d]" el))
      ((stringp el) (format "[\"%s\"]" el))
      (t (error "Unknown path element %s" path))))
   path ""))

(defun jsonl--correct-starting-path ()
  "Move point to a valid place to start searching for a path.
It is illegal to start searching for a path inside a string or a tag."
  ;;
  (let (match)
    (when (= (char-after) ?,)
      (backward-char))
    ;; Move before string values
    (when (setq match (or
                       (jsonl--get-font-lock-region (point) nil 'face jsonl-string-face)
                       (let ((s (jsonl--pos-in-valuep))) (when s (cons s nil)))))
      (goto-char (1- (car match))))
    ;; Move after string tags
    (when (setq match (or
                       (jsonl--get-font-lock-region (point) nil 'face jsonl-key-face)
                       (let ((e (jsonl--pos-in-keyp))) (when e (cons nil e)))))
      (goto-char (1+ (cdr match))))))

(defun jsonl--path (allow-tags)
  "Helper function for `jsonl-path'.
Will pick up object level tags at the current level of if
ALLOW-TAGS is non nil."
  ;; The number of previously encountered objects in this list (if we
  ;; are in a list).
  (let ((index 0) close)
    ;; We are not in the middle of a string, so we can now safely check for
    ;; the string property without false positives.
    (cl-loop 'while (not (bobp))
             (jsonl--back-whitespace)
             (cond
              ;; Enclosing object
              ((= (char-after) ?\{)
               (cl-return (cons 'object
                                (unless (bobp)
                                  (backward-char)
                                  (jsonl--path t)))))
              ;; Enclosing array
              ((= (char-after) ?\[)
               (cl-return (cons index
                                (unless (bobp)
                                  (backward-char)
                                  (jsonl--path t)))))
              ;; Skipping over a complete node (either a array or a object)
              ((or
                (= (char-after) ?\])
                (= (char-after) ?\}))
               (when (and (eolp) (not (bolp)))
                 (backward-char))
               (setq close (1- (scan-lists (point) 1 1)))
               (when (< close (line-end-position))
                 (goto-char (1+ close))
                 (backward-list))
               (backward-char))
              ;; In a list or object
              ((= (char-after) ?,)
               (when (bobp)
                 (user-error "Before ',' expected something, found beginning of buffer"))
               (backward-char)
               (setq index (1+ index)))
              ;; Object tag
              ((= (char-after) ?:)
               (when (bobp)
                 (user-error "Before ':' expected '\"', found beginning of buffer"))
               (backward-char)
               (jsonl--back-whitespace)
               (unless (= (char-after) ?\")
                 (user-error "Before ':' expected '\"', found '%c'" (char-after)))
               (let* ((tag-region (jsonl--back-string jsonl-key-face))
                      (tag-text (when tag-region
                                  (buffer-substring-no-properties (1+ (car tag-region)) (1- (cdr tag-region))))))
                 (unless tag-region
                   (error "Could not find tag"))
                 (when (= (car tag-region) (point-min))
                   (user-error "Before tag '\"%s\"' expected something, found beginning of buffer" tag-text))
                 (goto-char (1- (car tag-region)))
                 (when allow-tags
                   ;; To avoid blowing the recursion limit, we only collect tags
                   ;; (and recurse on them) when we need to.
                   (cl-return (cons tag-text (jsonl--path nil))))))
              ;; Found a number value, ignore
              ((and (<= (char-after) ?9) (>= (char-after) ?0))
               (jsonl--back-number))
              ;; Found a string value, ignore
              ((= (char-after) ?\")
               (jsonl--back-string jsonl-string-face))
              (t  (user-error "Unexpected character '%c'" (char-after)))))))

(defun jsonl--get-string-region (type &optional pos buffer)
  "Find the bounds of the string at POS in BUFFER.
Valid options for TYPE are `jsonl-string-face' and `jsonl-key-face'."
  (or (jsonl--get-font-lock-region pos buffer 'face type)
      (save-excursion
        (when buffer
          (set-buffer buffer))
        (when pos
          (goto-char pos))
        (cond
         ((eq type jsonl-string-face)
          (and (jsonl--pos-in-valuep) (jsonl--string-at-pos)))
         ((eq type jsonl-key-face)
          (and (jsonl--pos-in-keyp) (jsonl--string-at-pos)))
         (t (error "'%s' is not a valid type" type))))))

(defun jsonl--get-font-lock-region (&optional pos buffer property property-value)
  "Find the bounds of the font-locked region surrounding POS in BUFFER.
If PROPERTY-VALUE is set, the returned region has that value.
POS defaults to `point'. BUFFER defaults to `current-buffer'. PROPERTY defaults to `face'."
  (when (not jsonl-ignore-font-lock)
    (let ((pos (or pos (point)))
          (property (or property 'face))
          found)
      (with-current-buffer (or buffer (current-buffer))
        (setq found (get-text-property pos property))
        (when (and found (if property-value (equal property-value found) t))
          (cons
           (previous-single-property-change pos property)
           (next-single-property-change pos property)))))))

(cl-defstruct jsonl--edit-return
  "Information necessary to return from `jsonl-edit-mode'."
  match back-buffer overlay)

(defun jsonl--replace-text-in (start end text &optional buffer)
  "Set the content of the region (START to END) to TEXT in BUFFER.
BUFFER defaults to the current buffer."
  (with-current-buffer (or buffer (current-buffer))
    (goto-char start)
    (save-excursion
      (delete-region start end)
      (insert text))))

(defvar-local jsonl-edit-return-var nil
  "Information necessary to jump back from `jsonl-edit-mode'.")

(defun json-edit-string ()
  "Edit the string at point in another buffer."
  (interactive)
  (let ((cbuffer (current-buffer))
        (match (jsonl--get-string-region jsonl-string-face)))
    (if (not match)
        (user-error "No string at point")
      (let* ((buffer (generate-new-buffer (concat "edit-string:" (buffer-name))))
             (overlay (make-overlay (car match) (cdr match) cbuffer))
             (match (cons (1+ (car match)) (1- (cdr match))))
             (text (buffer-substring-no-properties (car match) (cdr match))))
        (overlay-put overlay 'face (list :background "white"))
        (read-only-mode +1)
        (with-current-buffer buffer
          (insert text)
          (jsonl--unintern-special-chars (current-buffer))
          (setq-local jsonl-edit-return-var (make-jsonl--edit-return
                                                  :match match
                                                  :back-buffer cbuffer
                                                  :overlay overlay)))
        (select-window (display-buffer buffer #'display-buffer-use-least-recent-window))
        (jsonl--edit-mode +1)))))

(defun jsonl--intern-special-chars (buffer)
  "Translates whitespace operators to their ansi equivalents in BUFFER.
This means replacing '\n' with '\\n', '\t' with '\\t'."
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-min))
      (while (search-forward "\n" nil t)
        (replace-match "\\\\n"))
      (goto-char (point-min))
      (while (search-forward "\t" nil t)
        (replace-match "\\\\t"))
      (goto-char (point-min))
      (while (search-forward "\"" nil t)
        (replace-match "\\\\\"")))))

(defun jsonl--unintern-special-chars (buffer)
  "Translate special characters to their unescaped equivalents in BUFFER.
This means replacing '\\n' with '\n' and '\\t' with '\t'."
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-min))
      (while (search-forward "\\n" nil t)
        (replace-match "\n"))
      (goto-char (point-min))
      (while (search-forward "\\t" nil t)
        (replace-match "\t"))
      (goto-char (point-min))
      (while (search-forward "\\\"" nil t)
        (replace-match "\"")))))

(defun jsonl--edit-mode-return ()
  "Jump back from `json-edit-string', actualizing the change made."
  (interactive)
  (jsonl--edit-mode-ensure)
  (jsonl--intern-special-chars (current-buffer))
  (let ((text (buffer-substring-no-properties (point-min) (point-max)))
        (back-buffer (jsonl--edit-return-back-buffer jsonl-edit-return-var))
        (back-match (jsonl--edit-return-match jsonl-edit-return-var)))
    (jsonl--edit-mode-cancel)
    (jsonl--replace-text-in (car back-match) (cdr back-match) text back-buffer)))

(defun jsonl--edit-mode-cancel ()
  "Jump back from `json-edit-string' without making a change."
  (interactive)
  (jsonl--edit-mode-ensure)
  (let ((back-buffer (jsonl--edit-return-back-buffer jsonl-edit-return-var))
        (overlay (jsonl--edit-return-overlay jsonl-edit-return-var)))
    (delete-overlay overlay)
    (kill-current-buffer)
    (select-window (get-buffer-window back-buffer))
    (read-only-mode -1)))

(define-minor-mode jsonl--edit-mode
  "Toggle edit-string-at-point mode.
This mode is used to setup editing functions for strings at point.
It should *not* be toggled manually."
  :global nil
  :lighter "edit-string"
  :keymap (list
           (cons (kbd "C-c C-c") #'jsonl--edit-mode-return)
           (cons (kbd "C-c C-k") #'jsonl--edit-mode-cancel)))

(defun jsonl--edit-mode-ensure ()
  "Throw an error if edit-string-at-point-mode is not setup correctly."
  (unless jsonl--edit-mode
    (error "`jsonl--edit-mode' is not set"))
  (unless jsonl-edit-return-var
    (error "`jsonl--edit-mode' is set but jsonl-edit-return-var is not")))



(provide 'jsonl)

;;; jsonl.el ends here
