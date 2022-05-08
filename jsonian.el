;;; jsonian.el --- Local editing of JSON files -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Ian Wahbe

;; Author: Ian Wahbe
;; URL: https://github.com/iwahbe/json-fixer
;; Version: 0.1.0
;; Package-Requires: ((emacs "26.1"))

;;; Commentary:

;; Provides functions to edit JSON files. So far these include
;; - `jsonian-path': Display the path to a point in a JSON file.
;; - `jsonian-edit-string': A connivance function for editing a JSON string in a separate buffer.

;;; Code:

(require 'cl-lib)

(defvar jsonian-string-face 'font-lock-string-face
  "The face by which `jsonian' can identify JSON string values.")

(defvar jsonian-key-face 'font-lock-keyword-face
  "The face by which `jsonian' can identify JSON keys.")

(defvar jsonian-ignore-font-lock nil
  "Always ignore symbol `font-lock-mode'.
If non-nil, `jsonian-string-face' and `jsonian-key-face' are
ignored.")

(defun jsonian-path (&optional pos buffer)
  "Return the JSON path (as a list) of POINT in BUFFER.
It is assumed that BUFFER is entirely JSON and that the json is
valid from POS to `point-min'.

For example
    { \"foo\": [ { \"bar\": █ }, { \"fizz\": \"buzz\" } ] }
with pos at █ should yield '(\"foo\" 0 \"bar\")

`jsonian-path' is optimized to work on very large json files (35 MiB+).
This optimization is achieved by
a. parsing as little of the file as necessary to find the path and
b. leveraging C code whenever possible."
  (interactive)
  (with-current-buffer (or buffer (current-buffer))
    (save-excursion
      (goto-char (or pos (point)))
      (jsonian--correct-starting-point)
      (let ((result (jsonian--reconstruct-path (jsonian--path t nil))) display)
        (when (called-interactively-p 'interactive)
          (setq display (jsonian--display-path result))
          (message "Path: %s" display)
          (kill-new display))
        result))))

(defmacro jsonian--defun-traverse (name &optional arg)
  "Define a functions to traverse literals or predicate defined range.
Generated functions are `jsonian--forward-NAME' and
`jsonian--backward-NAME'. If NAME is a string literal, generate a
function to parse the literal NAME. It is illegal to supply ARG
if NAME is a string literal. If NAME is a symbol generated
functions determine that a character `c' is part of the parse
group by calling ARG on `c'. If NAME is a symbol, it is illegal
not to supply ARG."
  (declare (indent defun))
  (if (stringp name)
      (progn
        (when arg
          (error "Unnecessary argument ARG"))
        `(progn
           (defun ,(intern (format "jsonian--backward-%s" name)) ()
             ,(format "Move backward over the literal \"%s\"" name)
             (if (and (> (- (point) ,(length name)) (point-min))
                      ,@(let ((i 0) l)
                          (while (< i (length name))
                            (setq l (cons (list '= (list 'char-after (list '- '(point) (- (length name) i 1))) (aref name i)) l)
                                  i (1+ i)))
                          l))
                 (backward-char ,(length name))
               (user-error ,(format "Expected \"%s\", found %s" name "%s\"%s\"")
                           (if (< (- (point) ,(length name)) (point-min))
                               "(BOB) " "")
                           (buffer-substring-no-properties (max (point-min) (- (point) ,(length name))) (point)))))
           (defun ,(intern (format "jsonian--forward-%s" name)) ()
             ,(format "Move forward over the literal \"%s\"" name) ;
             (if (and (< (+ (point) ,(length name)) (point-max))
                      ,@(let ((i 0) l)
                          (while (< i (length name))
                            (setq l (cons (list '= (list 'char-after (list '+ '(point) i)) (aref name i)) l)
                                  i (1+ i)))
                          l))
                 (dotimes (_ ,(length name))
                   (if (eolp) (forward-line) (forward-char)))
               (user-error ,(format "Expected \"%s\", found %s" name "\"%s\"%s")
                           (buffer-substring-no-properties (point) (min (+ (point) ,(length name)) (point-max)))
                           (if (>= (+ (point) ,(length name)) (point-max))
                               " (EOF)"
                             ""))))))
    (unless arg (error "Missing ARG"))
    `(progn
       (defun ,(intern (format "jsonian--backward-%s" name)) ()
         (while (and (not (bobp)) ,arg)
           (backward-char)))
       (defun ,(intern (format "jsonian--forward-%s" name)) ()
         (while (and (not (eobp)) ,arg)
           (if (eolp) (forward-line) (forward-char)))))))

(jsonian--defun-traverse "true")
(jsonian--defun-traverse "false")

(jsonian--defun-traverse whitespace
  (or (= (char-after) ?\ )
      (= (char-after) ?\t)
      (= (char-after) ?\n)
      (= (char-after) ?\r)))

(jsonian--defun-traverse number
  (and (<= (char-after) ?9)
       (>= (char-after) ?0)))

(defun jsonian--backward-string (&optional expected-face)
  "Move back a string, starting at the ending \".
If the string is highlighted with the `face' EXPECTED-FACE, then
use the face to define the scope of the region. If the string
does not have face EXPECTED-FACE, the string is manually parsed."
  (unless (= (char-after) ?\")
    (error "Expected to be at \""))
  (let ((match (and expected-face (jsonian--get-font-lock-region nil nil 'face expected-face))))
    (if match
        ;; The region is highlighted, so just jump to the beginning of that.
        (progn (goto-char (1- (car match))) match)
      ;; The region is not highlighted
      (setq match (point))
      (backward-char)
      (jsonian--string-scan-back)
      (cons (1+ (point)) (1+ match)))))

(defun jsonian--forward-string (&optional expected-face)
  "Move forward a string, starting at the beginning \".
If the string is highlighted with the `face' EXPECTED-FACE, use
the face to define the scope of the string. Otherwise the string
is manually parsed."
  (unless (= (char-after) ?\")
    (error "Expected to start at \""))
  (let ((match (and expected-face (jsonian--get-font-lock-region nil nil 'face expected-face))))
    (if match
        (progn (goto-char (cdr match)) match)
      (setq match (point))
      (forward-char)
      (jsonian--string-scan-forward t)
      (cons match (point)))))

(defun jsonian--string-scan-back ()
  "Scan backwards from `point' looking for the beginning of a string.
`jsonian--string-scan-back' will not move between lines. A non-nil
result is returned if a string beginning was found."
  (let (done exit)
    (while (not (or done exit))
      (when (bolp) (setq exit t))
      ;; Backtrack through the string until an unescaped " is found.
      (if (not (= (char-after) ?\"))
          (when (not (bobp)) (backward-char))
        (let (escaped (anchor (point)))
          (while (and (char-before) (= (char-before) ?\\))
            (backward-char)
            (setq escaped (not escaped)))
          (if escaped
              (when (not (bobp)) (backward-char))
            (goto-char (1- anchor))
            (setq done t)))))
    done))

(defun jsonian--string-scan-forward (&optional at-beginning)
  "Find the front of the current string.
`jsonian--string-scan-back' is called internally. When a string is found
the position of the final \" is returned and the point is moved
to just past that. When no string is found, nil is returned.

If AT-BEGINNING is non-nil, `jsonian--string-scan-forward' assumes
it is at the beginning of the string. Otherwise it scans
backwards to ensure that the end of a string is not escaped."
  (let ((start (if at-beginning (point) (jsonian--pos-in-stringp)))
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

(defun jsonian--pos-in-stringp ()
  "Determine if `point' is in a string (either a key or a value).
`=-pos-in-string' will only examine between `point' and
`beginning-of-line'. When non-nil, the starting position of the
discovered string is returned."
  (save-excursion
    (let (in-string start done)
      (while (and (jsonian--string-scan-back) (not done))
        (when (not start)
          (setq start (1+ (point))))
        (setq in-string (not in-string))
        (setq done (bobp)))
      (when in-string start))))

(defun jsonian--pos-in-keyp (&optional at-beginning)
  "Determine if `point' is a JSON string key.
If a non-nil, the position of the end of the string is returned.

If AT-BEGINNING is non-nil `jsonian--pos-in-keyp' assumes it is at
the beginning of a string."
  ;; A string is considered to be a key iff it is a string followed by some
  ;; amount of whitespace (maybe none) and then a :.
  (save-excursion
    (when (jsonian--string-scan-forward at-beginning)
      (let ((end (point)))
        (while (or (= (char-after) ?\ )
                   (= (char-after) ?\t)
                   (= (char-after) ?\n)
                   (= (char-after) ?\r))
          (forward-char))
        (and (= (char-after) ?:) end)))))

(defun jsonian--pos-in-valuep ()
  "Determine if `point' is a JSON string value.
If a non-nil, the position of the beginning of the string is
returned."
  (and (not (jsonian--pos-in-keyp)) (jsonian--pos-in-stringp)))

(defun jsonian--string-at-pos (&optional pos)
  "Return (start . end) for a string at POS if it exists.
Otherwise nil is returned. POS defaults to `ponit'."
  (save-excursion
    (when pos
      (goto-char pos))
    (let ((start (jsonian--pos-in-stringp)) end)
      (when start
        (setq end (jsonian--string-scan-forward)))
      (when (and start end)
        (cons start (1+ end))))))

(defun jsonian--reconstruct-path (input)
  "Cleanup INPUT as the result of `jsonian--path'."
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

(defun jsonian--display-path (path)
  "Convert the reconstructed JSON path PATH to a string."
  (mapconcat
   (lambda (el)
     (cond
      ((numberp el) (format "[%d]" el))
      ((stringp el) (format "[\"%s\"]" el))
      (t (error "Unknown path element %s" path))))
   path ""))

(defun jsonian--correct-starting-point ()
  "Move point to a valid place to start searching for a path.
It is illegal to start searching for a path inside a string or a tag."
  (let (match)
    ;; Move before string values
    (when (setq match (or
                       (jsonian--get-font-lock-region (point) nil 'face jsonian-string-face)
                       (let ((s (jsonian--pos-in-valuep))) (when s (cons s nil)))))
      (goto-char (1- (car match))))
    ;; Move after string tags
    (when (setq match (or
                       (jsonian--get-font-lock-region (point) nil 'face jsonian-key-face)
                       (let ((e (jsonian--pos-in-keyp))) (when e (cons nil e)))))
      (goto-char (cdr match))))
  ;; Move before boolean literals
  (while (and (char-after) (>= (char-after) ?a) (<= (char-after) ?z))
    (forward-char))
  (when (and (char-before) (= (char-after) ?,) (= (char-before) ?\ ))
      (backward-char)))

(defun jsonian--path (allow-tags stop-at-valid)
  "Helper function for `jsonian-path'.
Will pick up object level tags at the current level of if
ALLOW-TAGS is non nil. When STOP-AT-VALID is non-nil,
`jsonian-path' will parse back to the enclosing object or array.
Otherwise it will parse back to the beginning of the file."
  ;; The number of previously encountered objects in this list (if we
  ;; are in a list).
  (let ((index 0) close)
    ;; We are not in the middle of a string, so we can now safely check for
    ;; the string property without false positives.
    (cl-loop 'while (not (bobp))
             (jsonian--backward-whitespace)
             (cond
              ;; Enclosing object
              ((= (char-after) ?\{)
               (cl-return (cons 'object
                                (unless (or (bobp) stop-at-valid)
                                  (backward-char)
                                  (jsonian--path t stop-at-valid)))))
              ;; Enclosing array
              ((= (char-after) ?\[)
               (cl-return (cons index
                                (unless (or (bobp) stop-at-valid)
                                  (backward-char)
                                  (jsonian--path t stop-at-valid)))))
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
               (jsonian--backward-whitespace)
               (unless (= (char-after) ?\")
                 (user-error "Before ':' expected '\"', found '%c'" (char-after)))
               (let* ((tag-region (jsonian--backward-string jsonian-key-face))
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
                   (cl-return (cons tag-text (jsonian--path nil stop-at-valid))))))
              ;; Found a string value, ignore
              ((= (char-after) ?\")
               (jsonian--backward-string jsonian-string-face))

              ;; NOTE: I'm making a choice to parse non-string literals instead of ignoring
              ;; other characters. This ensures the partial parse is strict.

              ;; Found a number value, ignore
              ((and (<= (char-after) ?9) (>= (char-after) ?0))
               (jsonian--backward-number))
              ;; Boolean literal: true
              ((and (= (char-after) ?e)
                    (= (char-before) ?u))
               (jsonian--backward-true))
              ;; Boolean literal: false
              ((= (char-after) ?e) (jsonian--backward-false))
              (t  (user-error "Unexpected character '%c'" (char-after)))))))

(defun jsonian--get-string-region (type &optional pos buffer)
  "Find the bounds of the string at POS in BUFFER.
Valid options for TYPE are `jsonian-string-face' and `jsonian-key-face'."
  (or (jsonian--get-font-lock-region pos buffer 'face type)
      (save-excursion
        (when buffer
          (set-buffer buffer))
        (when pos
          (goto-char pos))
        (cond
         ((eq type jsonian-string-face)
          (and (jsonian--pos-in-valuep) (jsonian--string-at-pos)))
         ((eq type jsonian-key-face)
          (and (jsonian--pos-in-keyp) (jsonian--string-at-pos)))
         (t (error "'%s' is not a valid type" type))))))

(defun jsonian--get-font-lock-region (&optional pos buffer property property-value)
  "Find the bounds of the font-locked region surrounding POS in BUFFER.
If PROPERTY-VALUE is set, the returned region has that value.
POS defaults to `point'. BUFFER defaults to `current-buffer'. PROPERTY defaults to `face'."
  (when (not jsonian-ignore-font-lock)
    (let ((pos (or pos (point)))
          (property (or property 'face))
          found)
      (with-current-buffer (or buffer (current-buffer))
        (setq found (get-text-property pos property))
        (when (and found (if property-value (equal property-value found) t))
          (cons
           (previous-single-property-change pos property)
           (next-single-property-change pos property)))))))

(cl-defstruct jsonian--edit-return
  "Information necessary to return from `jsonian--edit-mode'."
  match          ;; The (start . end) region of text being operated on
  back-buffer    ;; The buffer to return back to
  overlay        ;; The overlay used to highlight `match' text
  delete-window) ;; If the hosting `window' should be deleted upon exit.

(defun jsonian--replace-text-in (start end text &optional buffer)
  "Set the content of the region (START to END) to TEXT in BUFFER.
BUFFER defaults to the current buffer."
  (with-current-buffer (or buffer (current-buffer))
    (goto-char start)
    (save-excursion
      (delete-region start end)
      (insert text))))

(defvar-local jsonian-edit-return-var nil
  "Information necessary to jump back from `jsonian--edit-mode'.")

(defun jsonian-edit-string ()
  "Edit the string at point in another buffer."
  (interactive)
  (let ((cbuffer (current-buffer))
        (match (jsonian--get-string-region jsonian-string-face)))
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
          (jsonian--unintern-special-chars (current-buffer))
          (setq-local jsonian-edit-return-var (make-jsonian--edit-return
                                                  :match match
                                                  :back-buffer cbuffer
                                                  :overlay overlay)))
        (let ((windows (length (window-list-1))))
          ;; We observe the number of existing windows
          (select-window (display-buffer buffer #'display-buffer-use-least-recent-window))
          ;; Then we display the new buffer
          (when (length> (window-list-1) windows)
            ;; If we have added a new window, we note to delete that window when
            ;; when we kill the display buffer
            (with-current-buffer buffer
              (setf (jsonian--edit-return-delete-window jsonian-edit-return-var) t))))
        (jsonian--edit-mode +1)))))

(defun jsonian--intern-special-chars (buffer)
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

(defun jsonian--unintern-special-chars (buffer)
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

(defun jsonian-edit-mode-return ()
  "Jump back from `json-edit-string', actualizing the change made."
  (interactive)
  (jsonian--edit-mode-ensure)
  (jsonian--intern-special-chars (current-buffer))
  (let ((text (buffer-substring-no-properties (point-min) (point-max)))
        (back-buffer (jsonian--edit-return-back-buffer jsonian-edit-return-var))
        (back-match (jsonian--edit-return-match jsonian-edit-return-var)))
    (jsonian-edit-mode-cancel)
    (jsonian--replace-text-in (car back-match) (cdr back-match) text back-buffer)))

(defun jsonian-edit-mode-cancel ()
  "Jump back from `json-edit-string' without making a change."
  (interactive)
  (jsonian--edit-mode-ensure)
  (let ((back-buffer (jsonian--edit-return-back-buffer jsonian-edit-return-var))
        (overlay (jsonian--edit-return-overlay jsonian-edit-return-var))
        (kill-window (jsonian--edit-return-delete-window jsonian-edit-return-var)))
    (delete-overlay overlay)

    ;; Kill the display buffer
    (if kill-window
      (kill-buffer-and-window)
      (kill-current-buffer))
    (select-window (get-buffer-window back-buffer))
    (read-only-mode -1)))

(define-minor-mode jsonian--edit-mode
  "Toggle edit-string-at-point mode.
This mode is used to setup editing functions for strings at point.
It should *not* be toggled manually."
  :global nil
  :lighter "edit-string"
  :keymap (list
           (cons (kbd "C-c C-c") #'jsonian-edit-mode-return)
           (cons (kbd "C-c C-k") #'jsonian-edit-mode-cancel)))

(defun jsonian--edit-mode-ensure ()
  "Throw an error if edit-string-at-point-mode is not setup correctly."
  (unless jsonian--edit-mode
    (error "`jsonian--edit-mode' is not set"))
  (unless jsonian-edit-return-var
    (error "`jsonian--edit-mode' is set but jsonian-edit-return-var is not")))


(defvar jsonian-syntax-table
  (let ((s (make-syntax-table)))
        ;; Objects
    (modify-syntax-entry ?\{ "(}" s)
    (modify-syntax-entry ?\} "){" s)
    ;; Arrays
    (modify-syntax-entry ?\[ "(]" s)
    (modify-syntax-entry ?\] ")[" s)
    ;; Strings
    (modify-syntax-entry ?\" "\"" s)
    ;; Syntax Escape
    (modify-syntax-entry ?\\ "\\" s)
    s)
  "The syntax table for JSON.")

(defun jsonian--syntactic-face (state)
  "Return syntactic face function for the position represented by STATE.
STATE is a `parse-partial-sexp' state, and the returned function is the
JSON font lock syntactic face function."
  (cond
   ((nth 3 state)
      ;; This might be a string or a name
    (let ((startpos (nth 8 state)))
      (save-excursion
        (goto-char startpos)
        (if (jsonian--pos-in-keyp t)
            font-lock-keyword-face
          font-lock-string-face))))
   ((nth 4 state) font-lock-comment-face)))


(defun jsonian--enclosing-object-or-array ()
  "Go to the enclosing object/array of `point'."
  (jsonian--correct-starting-point)
  (unless (bobp) (backward-char))
  (let ((result (car-safe (jsonian--path nil t))))
    (when (or (numberp result)
              (equal result 'object))
      result)))

(defun jsonian--enclosing-item ()
  "Go the the enclosing item start."
  (when (jsonian--enclosing-object-or-array)
    (let ((opening (point)))
      (when (not (bobp))
        (backward-char)
        (jsonian--backward-whitespace)
        (if (not (= (char-after) ?:))
            (goto-char opening)
          (when (not (bobp))
            (backward-char)
            (jsonian--backward-string)
            (forward-char)))))
    t))

(defun jsonian--traverse-forward (&optional n)
  "Go forward N elements in a list or array."
  (let ((n (or n 1))
        (start (point))
        done)
    (when (< n 0) (error "N must be positive"))
    (jsonian--correct-starting-point)
    (while (and (> n 0) (not done))
      (jsonian--forward-whitespace)
      (cond
       ((= (char-after) ?\") (jsonian--forward-string 'font-lock-string-face))
       ((= (char-after) ?:)
        (forward-char)
        (jsonian--forward-whitespace)
        (jsonian--forward-string 'font-lock-keyword-face))
       ((= (char-after) ?t) (jsonian--forward-true))
       ((= (char-after) ?f) (jsonian--forward-false))
       ((= (char-after) ?\{) (forward-list))
       ((= (char-after) ?\[) (forward-list))
       ((and (>= (char-after) ?0) (<= (char-after) ?9))
        (forward-char))
       ((= (char-after) ?,) (setq n (1- n))
        (forward-char))
       ((or (= (char-after) ?\])
            (= (char-after) ?\}))
        (setq done t))
       (t (user-error "Unexpected character '%c'" (char-after)))))
    (jsonian--forward-whitespace)
    (if (= n 0)
        t
      (goto-char start) nil)))

(defun jsonian-beginning-of-defun (&optional arg)
  "Move to the beginning of the smallest object/array enclosing `POS'.
The jsonian-mode value of `beginning-of-defun-function' (see
`beginning-of-defun' for details). With ARG, do it that many
times. Negative ARG means move forward to the ARGth following
beginning of defun."
  (setq arg (or arg 1))
  (jsonian--correct-starting-point)
  (jsonian--enclosing-object-or-array)
  nil)

(defun jsonian-narrow-to-defun (&optional arg)
  "Narrows to region for `jsonian-mode'. ARG is ignored."
  (let (start end)
    (save-excursion
      (jsonian--correct-starting-point)
      (setq end (point))
      (when (jsonian--enclosing-item)
        (setq start (point)))
      (goto-char end)
      (when (jsonian--enclosing-object-or-array)
        (forward-list)
        (setq end (point))))
    (when (and start end)
      (narrow-to-region start end))))

(defun jsonian--correct-narrow-to-defun (&optional arg)
  "Correct `narrow-to-defun' for `jsonian-mode' via the advice system.
ARG is passed onto `jsonian-narrow-to-defun'. This function is
designed to be installed with `advice-add' and `:before-until'."
  (interactive)
  (let ((correct (eq major-mode 'jsonian-mode)))
    (when correct
      (jsonian-narrow-to-defun arg))
    correct))

(advice-add 'narrow-to-defun :before-until #'jsonian--correct-narrow-to-defun)

(defvar jsonian-mode-map (make-sparse-keymap))
(define-key jsonian-mode-map (kbd "C-c C-p") #'jsonian-path)
(define-key jsonian-mode-map (kbd "C-c C-s") #'jsonian-edit-string)

(define-derived-mode jsonian-mode prog-mode "JSON"
  "Major mode for editing JSON files."
  :syntax-table jsonian-syntax-table
  (set (make-local-variable 'beginning-of-defun-function)
       #'jsonian-beginning-of-defun)
  (set (make-local-variable 'font-lock-defaults)
       '(json-font-lock-keywords-1
         nil nil nil nil
         (font-lock-syntactic-face-function . jsonian--syntactic-face))))

(provide 'jsonian)

;;; jsonian.el ends here
