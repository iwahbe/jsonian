;;; jsonf.el --- Local editing of JSON files -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Ian Wahbe

;; Author: Ian Wahbe
;; URL: https://github.com/iwahbe/json-fixer
;; Version: 0.1.0
;; Package-Requires: ((emacs "26.1"))

;;; Commentary:

;; Provides functions to edit JSON files. So far these include
;; - `jsonf-path': Display the path to a point in a JSON file.
;; - `jsonf-edit-string': A connivance function for editing a JSON string in a separate buffer.

;;; Code:

(require 'cl-lib)

(defvar jsonf-string-face 'font-lock-string-face
  "The face by which `jsonf' can identify JSON string values.")

(defvar jsonf-key-face 'font-lock-keyword-face
  "The face by which `jsonf' can identify JSON keys.")

(defvar jsonf-ignore-font-lock nil
  "Always ignore symbol `font-lock-mode'.
If non-nil, `jsonf-string-face' and `jsonf-key-face' are
ignored.")

(defun jsonf-path (&optional pos buffer)
  "Return the JSON path (as a list) of POINT in BUFFER.
It is assumed that BUFFER is entirely JSON and that the json is
valid from POS to `point-min'.

For example
    { \"foo\": [ { \"bar\": █ }, { \"fizz\": \"buzz\" } ] }
with pos at █ should yield '(\"foo\" 0 \"bar\")

`jsonf-path' is optimized to work on very large json files (35 MiB+).
This optimization is achieved by
a. parsing as little of the file as necessary to find the path and
b. leveraging C code whenever possible."
  (interactive)
  (with-current-buffer (or buffer (current-buffer))
    (save-excursion
      (goto-char (or pos (point)))
      (jsonf--correct-starting-path)
      (let ((result (jsonf--reconstruct-path (jsonf--path t))) display)
        (when (called-interactively-p 'interactive)
          (setq display (jsonf--display-path result))
          (message "Path: %s" display)
          (kill-new display))
        result))))

(defmacro jsonf--defun-back (name arg)
  "Define a =-back-NAME (NAME) function with predicate condition ARG."
  (declare (indent defun))
  `(defun ,(intern (format "jsonf--back-%s" name)) ()
     (while (and (not (bobp)) ,arg)
       (backward-char))))

(jsonf--defun-back whitespace
  (or (= (char-after) ?\ )
      (= (char-after) ?\t)
      (= (char-after) ?\n)
      (= (char-after) ?\r)))

(jsonf--defun-back number
  (and (<= (char-after) ?9)
       (>= (char-after) ?0)))

(defun jsonf--back-string (&optional expected-face)
  "Move back a string, starting at the ending \".
If the string is highlighted with the `face' EXPECTED-FACE, then
use the face to define the scope of the region. If the string
does not have face EXPECTED-FACE, the string is manually parsed."
  (unless (= (char-after) ?\")
    (error "Expected to be at \""))
  (let ((match (and expected-face (jsonf--get-font-lock-region nil nil 'face expected-face))))
    (if match
        ;; The region is highlighted, so just jump to the beginning of that.
        (progn (goto-char (1- (car match))) match)
      ;; The region is not highlighted
      (setq match (point))
      (backward-char)
      (jsonf--string-scan-back)
      (cons (1+ (point)) (1+ match)))))

(defun jsonf--string-scan-back ()
  "Scan backwards from `point' looking for the beginning of a string.
`jsonf--string-scan-back' will not move between lines. A non-nil
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

(defun jsonf--string-scan-forward ()
  "Find the front of the current string.
`jsonf--string-scan-back' is called internally. When a string is found
the position of the final \" is returned and the point is moved
to just past that. When no string is found, nil is returned."
  (let ((start (jsonf--pos-in-stringp))
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

(defun jsonf--pos-in-stringp ()
  "Determine if `point' is in a string (either a key or a value).
`=-pos-in-string' will only examine between `point' and
`beginning-of-line'. When non-nil, the starting position of the
discovered string is returned."
  (save-excursion
    (let (in-string start)
      (while (jsonf--string-scan-back)
        (when (not start)
          (setq start (1+ (point))))
        (setq in-string (not in-string)))
      (when in-string start))))

(defun jsonf--pos-in-keyp ()
  "Determine if `point' is a JSON string key.
If a non-nil, the position of the end of the string is returned."
  ;; A string is considered to be a key iff it is a string followed by some
  ;; amount of whitespace (maybe none) and then a :.
  (save-excursion
    (when (jsonf--string-scan-forward)
      (let ((end (point)))
        (while (or (= (char-after) ?\ )
                   (= (char-after) ?\t)
                   (= (char-after) ?\n)
                   (= (char-after) ?\r))
          (forward-char))
        (and (= (char-after) ?:) end)))))

(defun jsonf--pos-in-valuep ()
  "Determine if `point' is a JSON string value.
If a non-nil, the position of the beginning of the string is
returned."
  (and (not (jsonf--pos-in-keyp)) (jsonf--pos-in-stringp)))

(defun jsonf--string-at-pos (&optional pos)
  "Return (start . end) for a string at POS if it exists.
Otherwise nil is returned. POS defaults to `ponit'."
  (save-excursion
    (when pos
      (goto-char pos))
    (let ((start (jsonf--pos-in-stringp)) end)
      (when start
        (setq end (jsonf--string-scan-forward)))
      (when (and start end)
        (cons start (1+ end))))))

(defun jsonf--reconstruct-path (input)
  "Cleanup INPUT as the result of `jsonf--path'."
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

(defun jsonf--display-path (path)
  "Convert the reconstructed JSON path PATH to a string."
  (mapconcat
   (lambda (el)
     (cond
      ((numberp el) (format "[%d]" el))
      ((stringp el) (format "[\"%s\"]" el))
      (t (error "Unknown path element %s" path))))
   path ""))

(defun jsonf--correct-starting-path ()
  "Move point to a valid place to start searching for a path.
It is illegal to start searching for a path inside a string or a tag."
  ;;
  (let (match)
    (when (= (char-after) ?,)
      (backward-char))
    ;; Move before string values
    (when (setq match (or
                       (jsonf--get-font-lock-region (point) nil 'face jsonf-string-face)
                       (let ((s (jsonf--pos-in-valuep))) (when s (cons s nil)))))
      (goto-char (1- (car match))))
    ;; Move after string tags
    (when (setq match (or
                       (jsonf--get-font-lock-region (point) nil 'face jsonf-key-face)
                       (let ((e (jsonf--pos-in-keyp))) (when e (cons nil e)))))
      (goto-char (1+ (cdr match))))))

(defun jsonf--path (allow-tags)
  "Helper function for `jsonf-path'.
Will pick up object level tags at the current level of if
ALLOW-TAGS is non nil."
  ;; The number of previously encountered objects in this list (if we
  ;; are in a list).
  (let ((index 0) close)
    ;; We are not in the middle of a string, so we can now safely check for
    ;; the string property without false positives.
    (cl-loop 'while (not (bobp))
             (jsonf--back-whitespace)
             (cond
              ;; Enclosing object
              ((= (char-after) ?\{)
               (cl-return (cons 'object
                                (unless (bobp)
                                  (backward-char)
                                  (jsonf--path t)))))
              ;; Enclosing array
              ((= (char-after) ?\[)
               (cl-return (cons index
                                (unless (bobp)
                                  (backward-char)
                                  (jsonf--path t)))))
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
               (jsonf--back-whitespace)
               (unless (= (char-after) ?\")
                 (user-error "Before ':' expected '\"', found '%c'" (char-after)))
               (let* ((tag-region (jsonf--back-string jsonf-key-face))
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
                   (cl-return (cons tag-text (jsonf--path nil))))))
              ;; Found a number value, ignore
              ((and (<= (char-after) ?9) (>= (char-after) ?0))
               (jsonf--back-number))
              ;; Found a string value, ignore
              ((= (char-after) ?\")
               (jsonf--back-string jsonf-string-face))
              (t  (user-error "Unexpected character '%c'" (char-after)))))))

(defun jsonf--get-string-region (type &optional pos buffer)
  "Find the bounds of the string at POS in BUFFER.
Valid options for TYPE are `jsonf-string-face' and `jsonf-key-face'."
  (or (jsonf--get-font-lock-region pos buffer 'face type)
      (save-excursion
        (when buffer
          (set-buffer buffer))
        (when pos
          (goto-char pos))
        (cond
         ((eq type jsonf-string-face)
          (and (jsonf--pos-in-valuep) (jsonf--string-at-pos)))
         ((eq type jsonf-key-face)
          (and (jsonf--pos-in-keyp) (jsonf--string-at-pos)))
         (t (error "'%s' is not a valid type" type))))))

(defun jsonf--get-font-lock-region (&optional pos buffer property property-value)
  "Find the bounds of the font-locked region surrounding POS in BUFFER.
If PROPERTY-VALUE is set, the returned region has that value.
POS defaults to `point'. BUFFER defaults to `current-buffer'. PROPERTY defaults to `face'."
  (when (not jsonf-ignore-font-lock)
    (let ((pos (or pos (point)))
          (property (or property 'face))
          found)
      (with-current-buffer (or buffer (current-buffer))
        (setq found (get-text-property pos property))
        (when (and found (if property-value (equal property-value found) t))
          (cons
           (previous-single-property-change pos property)
           (next-single-property-change pos property)))))))

(cl-defstruct jsonf--edit-return
  "Information necessary to return from `jsonf-edit-mode'."
  match back-buffer overlay)

(defun jsonf--replace-text-in (start end text &optional buffer)
  "Set the content of the region (START to END) to TEXT in BUFFER.
BUFFER defaults to the current buffer."
  (with-current-buffer (or buffer (current-buffer))
    (goto-char start)
    (save-excursion
      (delete-region start end)
      (insert text))))

(defvar-local jsonf-edit-return-var nil
  "Information necessary to jump back from `jsonf-edit-mode'.")

(defun jsonf-edit-string ()
  "Edit the string at point in another buffer."
  (interactive)
  (let ((cbuffer (current-buffer))
        (match (jsonf--get-string-region jsonf-string-face)))
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
          (jsonf--unintern-special-chars (current-buffer))
          (setq-local jsonf-edit-return-var (make-jsonf--edit-return
                                                  :match match
                                                  :back-buffer cbuffer
                                                  :overlay overlay)))
        (select-window (display-buffer buffer #'display-buffer-use-least-recent-window))
        (jsonf--edit-mode +1)))))

(defun jsonf--intern-special-chars (buffer)
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

(defun jsonf--unintern-special-chars (buffer)
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

(defun jsonf--edit-mode-return ()
  "Jump back from `json-edit-string', actualizing the change made."
  (interactive)
  (jsonf--edit-mode-ensure)
  (jsonf--intern-special-chars (current-buffer))
  (let ((text (buffer-substring-no-properties (point-min) (point-max)))
        (back-buffer (jsonf--edit-return-back-buffer jsonf-edit-return-var))
        (back-match (jsonf--edit-return-match jsonf-edit-return-var)))
    (jsonf--edit-mode-cancel)
    (jsonf--replace-text-in (car back-match) (cdr back-match) text back-buffer)))

(defun jsonf--edit-mode-cancel ()
  "Jump back from `json-edit-string' without making a change."
  (interactive)
  (jsonf--edit-mode-ensure)
  (let ((back-buffer (jsonf--edit-return-back-buffer jsonf-edit-return-var))
        (overlay (jsonf--edit-return-overlay jsonf-edit-return-var)))
    (delete-overlay overlay)
    (kill-current-buffer)
    (select-window (get-buffer-window back-buffer))
    (read-only-mode -1)))

(define-minor-mode jsonf--edit-mode
  "Toggle edit-string-at-point mode.
This mode is used to setup editing functions for strings at point.
It should *not* be toggled manually."
  :global nil
  :lighter "edit-string"
  :keymap (list
           (cons (kbd "C-c C-c") #'jsonf--edit-mode-return)
           (cons (kbd "C-c C-k") #'jsonf--edit-mode-cancel)))

(defun jsonf--edit-mode-ensure ()
  "Throw an error if edit-string-at-point-mode is not setup correctly."
  (unless jsonf--edit-mode
    (error "`jsonf--edit-mode' is not set"))
  (unless jsonf-edit-return-var
    (error "`jsonf--edit-mode' is set but jsonf-edit-return-var is not")))



(provide 'jsonf)

;;; jsonf.el ends here
