;;; jsonian.el --- A major mode for editing JSON files -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Ian Wahbe

;; Author: Ian Wahbe
;; URL: https://github.com/iwahbe/jsonian
;; Version: 0.1.0
;; Package-Requires: ((emacs "26.1"))

;; License:

;; GNU GENERAL PUBLIC LICENSE
;;    Version 3, 29 June 2007
;; For the full boilerplate, see LICENSE

;;; Commentary:

;; Provides functions to edit JSON files.  So far these include
;; - `jsonian-path': Display the path to a point in a JSON file.
;; - `jsonian-edit-string': A connivance function for editing a JSON string in a separate buffer.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'seq)

(defcustom jsonian-ignore-font-lock nil
    "Prevent `font-lock' based optimizations.
Don't use `font-lock-string-face' and `font-lock-keyword-face' to
determine string and key values respectively."
    :type 'boolean
    :group 'jsonian)

(defcustom jsonian-spaces-per-indentation 4
  "The number of spaces each increase in indentation level indicates."
  :type 'integer
  :group 'jsonian)

(defun jsonian-path (&optional plain pos buffer)
  "Find the JSON path of POINT in BUFFER.
If called interactively, then the path is printed to the
minibuffer and pre-appended to the kill ring.  If called
non-interactively, then the path is returned as a list of strings
and numbers.  It is assumed that BUFFER is entirely JSON and that
the json is valid from POS to `point-min'.  PLAIN indicates that
the path should be formated using only indexes.  Otherwise index
notation is used.

For example
    { \"foo\": [ { \"bar\": █ }, { \"fizz\": \"buzz\" } ] }
with pos at █ should yield \".foo[0].bar\".

`jsonian-path' is optimized to work on very large json files (35 MiB+).
This optimization is achieved by
a. parsing as little of the file as necessary to find the path and
b. leveraging C code whenever possible."
  (interactive "P")
  (with-current-buffer (or buffer (current-buffer))
    (save-excursion
      (when pos (goto-char pos))
      (jsonian--correct-starting-point)
      (let ((result (jsonian--reconstruct-path (jsonian--path t nil))) display)
        (when (called-interactively-p 'interactive)
          (setq display (jsonian--display-path result (not plain)))
          (message "Path: %s" display)
          (kill-new display))
        result))))


(defmacro jsonian--defun-predicate-traversal (name arg-list predicate)
  "Define `jsonian--forward-NAME' and `jsonian--backward-NAME'.
NAME is an unquoted symbol.  ARG-LIST defines a single variable
name which will be bound to the value of the character to
examine.  PREDICATE is the function body to call.  A non-nil value
determines that the argument is in the category NAME."
  (declare (indent defun))
  `(progn
     (defun ,(intern (format "jsonian--backward-%s" name)) ()
       (let ((,@arg-list))
         (while (and (not (bobp)) (setq ,(car arg-list) (char-before)) ,predicate)
           (backward-char))))
     (defun ,(intern (format "jsonian--forward-%s" name)) ()
       (let ((,@arg-list))
         (while (and (not (eobp)) (setq ,(car arg-list) (char-after)) ,predicate)
           (if (eolp) (forward-line) (forward-char)))))))

(defmacro jsonian--defun-literal-traversal (literal)
  "Define `jsonian--forward-LITERAL' and `jsonian--backward-LITERAL'.
LITERAL is the string literal to be traversed."
  (declare (indent defun))
  `(progn
     (defun ,(intern (format "jsonian--backward-%s" literal)) ()
       ,(format "Move backward over the literal \"%s\"" literal)
       (if (and (> (- (point) ,(length literal)) (point-min))
                ,@(let ((i 0) l)
                    (while (< i (length literal))
                      (setq l (cons (list 'eq (list 'char-before (list '- '(point) (- (length literal) i 1))) (aref literal i)) l)
                            i (1+ i)))
                    l))
           (backward-char ,(length literal))
         (user-error ,(format "jsonian--backward-%s: expected \"%s\", found %s" literal literal "%s\"%s\"")
                     (if (< (- (point) ,(length literal)) (point-min))
                         "(BOB) " "")
                     (buffer-substring-no-properties (max (point-min) (- (point) ,(length literal))) (point)))))
     (defun ,(intern (format "jsonian--forward-%s" literal)) ()
       ,(format "Move forward over the literal \"%s\"" literal) ;
       (if (and (< (+ (point) ,(length literal)) (point-max))
                ,@(let ((i 0) l)
                    (while (< i (length literal))
                      (setq l (cons (list '= (list 'char-after (list '+ '(point) i)) (aref literal i)) l)
                            i (1+ i)))
                    l))
           (dotimes (_ ,(length literal))
             (if (eolp) (forward-line) (forward-char)))
         (user-error ,(format "jsonian--forward-%s: expected \"%s\", found %s" literal literal "\"%s\"%s")
                     (buffer-substring-no-properties (point) (min (+ (point) ,(length literal)) (point-max)))
                     (if (>= (+ (point) ,(length literal)) (point-max))
                         " (EOF)"
                       ""))))))

(defmacro jsonian--defun-traverse (name &optional arg arg2)
  "Define a functions to traverse literals or predicate defined range.
Generated functions are `jsonian--forward-NAME' and
`jsonian--backward-NAME'.  If NAME is a string literal, generate a
function to parse the literal NAME.  It is illegal to supply ARG
or ARG2 if NAME is a string literal.  If NAME is a symbol
generated functions determine that a character (car ARG) is part
of the parse group by calling ARG with (car ARG) bound to a
character.  If NAME is a symbol, it is illegal not to supply ARG
and ARG2."
  (declare (indent defun))
  (if (stringp name)
      (progn
        (when arg
          (error "Unnecessary argument ARG"))
        `(jsonian--defun-literal-traversal ,name))
    (unless arg (error "Missing ARG"))
    `(jsonian--defun-predicate-traversal ,name ,arg ,arg2)))

(jsonian--defun-traverse "true")
(jsonian--defun-traverse "false")

(jsonian--defun-traverse whitespace (x)
  (or (= x ?\ )
      (= x ?\t)
      (= x ?\n)
      (= x ?\r)))

(jsonian--defun-traverse number (x)
  (and (<= x ?9)
       (>= x ?0)))

(defun jsonian--backward-string (&optional expected-face)
  "Move back a string, starting at the ending \".
If the string is highlighted with the `face' EXPECTED-FACE, then
use the face to define the scope of the region.  If the string
does not have face EXPECTED-FACE, the string is manually parsed."
  (unless (eq (char-before) ?\")
    (error "`jsonian--backward-string': Expected to start at \""))
  (let ((match (and expected-face (jsonian--get-font-lock-region nil nil 'face expected-face))))
    (if match
        ;; The region is highlighted, so just jump to the beginning of that.
        (progn (goto-char (1- (car match))) match)
      ;; The region is not highlighted
      (setq match (point))
      (backward-char)
      (jsonian--string-scan-back)
      (cons (point) match))))

(defun jsonian--forward-string (&optional expected-face)
  "Move forward a string, starting at the beginning \".
If the string is highlighted with the `face' EXPECTED-FACE, use
the face to define the scope of the string.  Otherwise the string
is manually parsed."
  (unless (eq (char-after) ?\")
    (error "`jsonian--forward-string': Expected to start at \", instead found %s"
           (if (char-after) (char-to-string (char-after)) "EOF")))
  (let ((match (and expected-face (jsonian--get-font-lock-region nil nil 'face expected-face))))
    (if match
        (progn (goto-char (cdr match)) match)
      (setq match (point))
      (forward-char)
      (jsonian--string-scan-forward t)
      (cons match (point)))))

(defun jsonian--string-scan-back ()
  "Scan backwards from `point' looking for the beginning of a string.
`jsonian--string-scan-back' will not move between lines.  A non-nil
result is returned if a string beginning was found."
  (let (done exit)
    (while (not (or done exit))
      (when (bolp) (setq exit t))
      ;; Backtrack through the string until an unescaped " is found.
      (if (not (eq (char-before) ?\"))
          (when (not (bobp)) (backward-char))
        (let (escaped (anchor (point)))
          (while (eq (char-before (1- (point))) ?\\)
            (backward-char)
            (setq escaped (not escaped)))
          (if escaped
              (when (not (bobp)) (backward-char))
            (goto-char (1- anchor))
            (setq done t)))))
    done))

(defun jsonian--string-scan-forward (&optional at-beginning)
  "Find the front of the current string.
`jsonian--string-scan-back' is called internally.  When a string is found
the position of the final \" is returned and the point is moved
to just past that.  When no string is found, nil is returned.

If AT-BEGINNING is non-nil, `jsonian--string-scan-forward' assumes
it is at the beginning of the string.  Otherwise it scans
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
`jsonian--pos-in-string' will only examine between `point' and
`beginning-of-line'.  When non-nil, the starting position of the
discovered string is returned."
  (save-excursion
    (let (in-string start done)
      (while (and (jsonian--string-scan-back) (not done))
        (when (not start)
          (setq start (point)))
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
Otherwise nil is returned.  POS defaults to `ponit'."
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
                      path (cons element path)))))
            input)
    path))

(defun jsonian--display-path (path &optional pretty)
  "Convert the reconstructed JSON path PATH to a string.
If PRETTY is non-nil, format for human readable."
  (mapconcat
   (lambda (el)
     (cond
      ((numberp el) (format "[%d]" el))
      ((stringp el) (format
                     (if (and pretty (jsonian--simple-path-segment-p el))
                         ".%s" "[\"%s\"]")
                     el))
      (t (error "Unknown path element %s" path))))
   path ""))

(defun jsonian--simple-path-segment-p (segment)
  "If SEGMENT can be displayed simply, or if it needs to be escaped.
A segment is considered simple if and only if it does not contain any
- blanks
- period
- quotes
- square brackets"
  (not (string-match-p "\\([[:blank:].\"\\[]\\|\\]\\)" segment)))

(defun jsonian--correct-starting-point ()
  "Move point to a valid place to start searching for a path.
It is illegal to start searching for a path inside a string or a tag."
  (let (match)
    ;; Move before string values
    (when (setq match (or
                       (jsonian--get-font-lock-region (point) nil 'face 'font-lock-string-face)
                       (let ((s (jsonian--pos-in-valuep))) (when s (cons s nil)))))
      (goto-char (car match)))
    ;; Move after string tags
    (when (setq match (or
                       (jsonian--get-font-lock-region (point) nil 'face 'font-lock-keyword-face)
                       (let ((e (jsonian--pos-in-keyp))) (when e (cons nil e)))))
      (goto-char (cdr match))))
  ;; Move before literals
  (while (and (char-before) (>= (char-before) ?a) (<= (char-before) ?z))
    (backward-char))
  ;; Move after : to be sure we see it. Not doing this leads to confusing keys
  ;; and string values when scanning backwards
  (when (eq (char-after) ?:)
    (forward-char)))

(defun jsonian--path (allow-tags stop-at-valid)
  "Helper function for `jsonian-path'.
Will pick up object level tags at the current level of if
ALLOW-TAGS is non nil.  When STOP-AT-VALID is non-nil,
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
              ((eq (char-before) ?\{)
               (cl-return (cons 'object
                                (unless stop-at-valid
                                  (backward-char)
                                  (jsonian--path t stop-at-valid)))))
              ;; Enclosing array
              ((eq (char-before) ?\[)
               (cl-return (cons index
                                (unless stop-at-valid
                                  (backward-char)
                                  (jsonian--path t stop-at-valid)))))
              ;; Skipping over a complete node (either a array or a object)
              ((or
                (eq (char-before) ?\])
                (eq (char-before) ?\}))
               (backward-char)
               (setq close (1- (scan-lists (point) 1 1)))
               (when (< close (line-end-position))
                 (goto-char (1+ close))
                 (backward-list))
               (backward-char))

              ;; In a list or object
              ((eq (char-before) ?,)
               (backward-char)
               (setq index (1+ index)))

              ;; Object tag
              ((eq (char-before) ?:)
               (backward-char)
               (jsonian--backward-whitespace)
               (unless (eq (char-before) ?\")
                 (user-error "Before ':' expected '\"', found '%s'" (if (bobp) "BOB" (char-before))))
               (let* ((tag-region (jsonian--backward-string 'font-lock-keyword-face))
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
              ((eq (char-before) ?\")
               (jsonian--backward-string 'font-lock-string-face))

              ;; NOTE: I'm making a choice to parse non-string literals instead of ignoring
              ;; other characters. This ensures the partial parse is strict.

              ;; Found a number value, ignore
              ((and (char-before) (<= (char-before) ?9) (>= (char-before) ?0))
               (jsonian--backward-number))
              ;; Boolean literal: true
              ((and (eq (char-before) ?e)
                    (eq (char-before (1- (point))) ?u))
               (jsonian--backward-true))
              ;; Boolean literal: false
              ((eq (char-before) ?e) (jsonian--backward-false))
              ((bobp) (cl-return nil))
              (t  (user-error "Unexpected character '%s'" (if (bobp) "BOB" (format "%c" (char-before)))))))))

(defun jsonian--get-string-region (type &optional pos buffer)
  "Find the bounds of the string at POS in BUFFER.
Valid options for TYPE are `font-lock-string-face' and `font-lock-keyword-face'."
  (or (jsonian--get-font-lock-region pos buffer 'face type)
      (save-excursion
        (when buffer
          (set-buffer buffer))
        (when pos
          (goto-char pos))
        (cond
         ((eq type 'font-lock-string-face)
          (and (jsonian--pos-in-valuep) (jsonian--string-at-pos)))
         ((eq type 'font-lock-keyword-face)
          (and (jsonian--pos-in-keyp) (jsonian--string-at-pos)))
         (t (error "'%s' is not a valid type" type))))))

(defun jsonian--get-font-lock-region (&optional pos buffer property property-value)
  "Find the bounds of the font-locked region surrounding POS in BUFFER.
If PROPERTY-VALUE is set, the returned region has that value.  POS
defaults to `point'.  BUFFER defaults to `current-buffer'.
PROPERTY defaults to `face'."
  (when (not jsonian-ignore-font-lock)
    (let ((pos (or pos (point)))
          (property (or property 'face))
          found)
      (with-current-buffer (or buffer (current-buffer))
        (setq found (get-text-property pos property))
        (when (and
               found
               (if property-value (equal property-value found) t)
               ;; We do this check so other font effects like todo highlighting
               ;; in strings don't break get string. It is a heuristic check, so
               ;; may be wrong if we find the string/key `"foo\"BAR"'. and BAR
               ;; is highlighted in another face.
               (eq (char-before (next-single-property-change pos property)) ?\"))
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
        (match (jsonian--get-string-region 'font-lock-string-face)))
    (unless match (user-error "No string at point"))
    (let* ((edit-buffer (generate-new-buffer (concat "edit-string:" (buffer-name))))
           (overlay (make-overlay (car match) (cdr match) cbuffer))
           (match (cons (1+ (car match)) (1- (cdr match))))
           (text (buffer-substring-no-properties (car match) (cdr match))))
      (overlay-put overlay 'face (list :background "white"))
      (read-only-mode +1)
      (with-current-buffer edit-buffer
        (insert text)
        (jsonian--unintern-special-chars (current-buffer))
        (goto-char (point-min))
        (setq-local jsonian-edit-return-var (make-jsonian--edit-return
                                             :match match
                                             :back-buffer cbuffer
                                             :overlay overlay)))
      (let ((windows (length (window-list-1))))
        ;; We observe the number of existing windows
        (select-window (display-buffer edit-buffer #'display-buffer-pop-up-window))
        ;; Then we display the new buffer
        (when (> (length (window-list-1)) windows)
          ;; If we have added a new window, we note to delete that window when
          ;; when we kill the display buffer
          (with-current-buffer edit-buffer
            (setf (jsonian--edit-return-delete-window jsonian-edit-return-var) t))))
      (jsonian--edit-mode +1)
      (setq header-line-format
            (substitute-command-keys
             "Edit, then exit with `\\[jsonian-edit-mode-return]' or abort with \
`\\[jsonian-edit-mode-cancel]'")))))

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
  :lighter " edit-string"
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

(defun jsonian--after-key (pos)
  "Detect if POS are preceded by a key (as defined by face)."
  (let ((x (char-before pos)))
    (while (and (not (bobp))
                (or (= x ?\ )
                    (= x ?\t)
                    (= x ?\n)
                    (= x ?\r)))
      (setq pos (1- pos)
            x (char-before pos)))
    (eq (char-before pos) ?:)))

(defun jsonian--syntactic-face (state)
  "Return syntactic face function for the position represented by STATE.
STATE is a `parse-partial-sexp' state, and the returned function is the
JSON font lock syntactic face function."
  (cond
   ((nth 3 state)
    ;; This might be a string or a name
    (if (or (jsonian--after-key (nth 8 state))
            (not (save-excursion
                   (goto-char (nth 8 state))
                   (jsonian--pos-in-keyp t))))
        font-lock-string-face
      font-lock-keyword-face))
   ((nth 4 state) font-lock-comment-face)))


(defun jsonian--enclosing-object-or-array ()
  "Go to the enclosing object/array of `point'."
  (jsonian--correct-starting-point)
  (let ((result (car-safe (jsonian--path nil t))))
    (when (or (numberp result)
              (equal result 'object))
      (unless (bobp) (backward-char))
      result)))

;;;###autoload
(defun jsonian-enclosing-item (&optional arg)
  "Move point to the item enclosing the current point.
If ARG is not nil, move to the ARGth enclosing item."
  (interactive "P")
  (dotimes (_ (or arg 1))
    (jsonian--enclosing-item))
  (when (and (not (eq arg 0)) (jsonian--after-key (point)))
    (jsonian--backward-whitespace)
    (backward-char)
    (jsonian--backward-whitespace)
    (jsonian--backward-string)))

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

;;;###autoload
(defun jsonian-find ()
  "Navigate to a item in a JSON document."
  (interactive)
  (let* ((sibs (jsonian--find-siblings))
         (map (make-hash-table
               :test 'equal
               :size (length sibs)))
         (keys (mapcar (lambda (kv)
                         (puthash (car kv) (cdr kv) map)
                         (car kv))
                       sibs))
         (selection (completing-read "Select element: " keys nil t)))
    (when selection
      (goto-char (gethash selection map)))))

(defun jsonian--find-completion (str predicate type)
  "The function passed to `completing-read' to handle navigating the JSON document.
STR is the string to be completed.
PREDICATE is a function by which to filter possible matches.
TYPE is a flag specifying the type of completion."
  ;; See 21.6.7 Programmed Completion in the manual for more details
  ;; (elisp)Programmed Completion
  ;; TODO
  (ignore str)
  (ignore predicate)
  (cond
   ((eq type nil) (error "`nil' mode not supported"))
   ((eq type t) (error "`t' mode not supported"))
   ((eq type 'lambda) (error "`lambda' mode not supported"))
   ((eq (car-safe type) 'boundaries) (error "`boundaries' mode not supported"))
   ;; We specify an empty alist right now.
   ((eq type 'metadata) (cons 'metadata nil))))

(defun jsonian--parse-path (str)
  "Parse STR as a JSON path.
A list of elements is returned."
  (unless (stringp str) (error "`jsonian--parse-path': Input not a string"))
  (cond
   ((string= str "") nil)
   ((string-match "^\[[0-9]+\]" str)
    (cons (string-to-number (substring str 1 (1- (match-end 0))))
          (jsonian--parse-path (substring str (match-end 0)))))
   ((string-match-p (regexp-quote "[\"") str)
    (let ((s (with-temp-buffer
               (insert (substring str 1)) (goto-char (point-min))
               (buffer-substring 2 (1- (cdr (jsonian--forward-string)))))))
      (cons s (jsonian--parse-path (substring str (+ (length s) 4))))))
   ((string= "." (substring str 0 1))
    (if (not (string-match "[\.\[]" (substring str 1)))
        ;; We have found nothing to indicate another sequence, so this is the last node
        (list (substring str 1))
      (cons (substring str 1 (match-end 0)) (jsonian--parse-path (substring str (match-end 0))))))
   (t (user-error "Unexpected input: %s" str))))

(defun jsonian--find-siblings ()
  "Return a list of the elements in the enclosing scope.
Elements are of the form ( key . point ) where key is either a
string or a integer.  Point is a char location."
  (save-excursion
    ;; Go to the beginning of the enclosing item, if we are at the end of the
    ;; buffer, there is nothing there, so stop.
    (when (and (jsonian--enclosing-item) (not (eobp)))
      (forward-char) ;; Go the the beginning of the first element in the enclosing object
      (jsonian--forward-whitespace)
      ;; If we are at the end of the object, stop
      (unless (or (eq (char-after) ?\})
                  (eq (char-after) ?\]))
        (let ((elements
               (list (cons
                      (if-let ((end (save-excursion (forward-char) (jsonian--pos-in-keyp))))
                          (buffer-substring-no-properties (point) end)
                        0)
                      (point)))))
          (while (jsonian--traverse-forward)
            (setq elements (cons
                            (cons (if-let ((end (save-excursion (forward-char) (jsonian--pos-in-keyp))))
                                      (buffer-substring-no-properties (point) end)
                                    (length elements))
                                  (point))
                            elements)))
          elements)))))

(defun jsonian--traverse-forward (&optional n)
  "Go forward N elements in an object or array."
  (let ((n (or n 1)) done)
    (when (<= n 0) (error "N must be positive"))
    (jsonian--correct-starting-point)
    (when (eq (char-after) ?,) (setq n (1+ n)))
    (while (and (> n 0) (not done))
      (jsonian--forward-whitespace)
      (cond
       ((= (char-after) ?\") (jsonian--forward-string))
       ((= (char-after) ?:) (forward-char))
       ((= (char-after) ?t) (jsonian--forward-true))
       ((= (char-after) ?f) (jsonian--forward-false))
       ((= (char-after) ?\{) (forward-list))
       ((= (char-after) ?\[) (forward-list))
       ((and (>= (char-after) ?0) (<= (char-after) ?9)) (jsonian--forward-number))
       ((= (char-after) ?,) (setq n (1- n)) (forward-char) (jsonian--forward-whitespace))
       ((or (= (char-after) ?\]) (= (char-after) ?\})) (setq done t))
       (t (user-error "Unexpected character '%c'" (char-after)))))
    (jsonian--forward-whitespace)
    (not done)))

(defun jsonian-beginning-of-defun (&optional arg)
  "Move to the beginning of the smallest object/array enclosing `POS'.
ARG is currently ignored."
  (setq arg (or arg 1)) ;; TODO use ARG correctly
  (jsonian--correct-starting-point)
  (jsonian--enclosing-object-or-array)
  nil)

(defun jsonian-end-of-defun (&optional arg)
  "Move to the end of the smallest object/array enclosing `POS'.
ARG is currently ignored."
  (setq arg (or arg 1))
  (jsonian--correct-starting-point)
  (jsonian--enclosing-object-or-array)
  (forward-list)
  nil)

(defun jsonian-narrow-to-defun (&optional arg)
  "Narrows to region for `jsonian-mode'.  ARG is ignored."
  ;; Arg is present to comply with the function signature of `narrow-to-defun'.
  ;; Its value is ignored.
  (setq arg (or arg))
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
ARG is passed onto `jsonian-narrow-to-defun'.  This function is
designed to be installed with `advice-add' and `:before-until'."
  (interactive)
  (let ((correct (eq major-mode 'jsonian-mode)))
    (when correct
      (jsonian-narrow-to-defun arg))
    correct))

(advice-add 'narrow-to-defun :before-until #'jsonian--correct-narrow-to-defun)

(defun jsonian--get-indent-level ()
  "Find the indentation level of the current line by examining the previous line."
  (if (= (line-number-at-pos) 1)
      0
    (let ((level 0))
      (save-excursion ;; The previous line - end
        (end-of-line 0) ;;Roll backward to the end of the previous line
        (jsonian--backward-whitespace)
        (when (or (eq (char-before) ?\{)
                  (eq (char-before) ?\[))
          ;; If it is a opening \{ or \[, the next line should be indented by 1 unit
          (cl-incf level jsonian-spaces-per-indentation))
        (beginning-of-line)
        (while (or (eq (char-after) ?\ )
                   (eq (char-after) ?\t))
          (forward-char))
        (cl-incf level (current-column)))
      (save-excursion ;; Make sure that we account for any closing brackets in front of (point)
        (beginning-of-line)
        (jsonian--forward-whitespace)
        (when (or (eq (char-after) ?\})
                  (eq (char-after) ?\]))
          (cl-decf level jsonian-spaces-per-indentation)))
      level)))

;;;###autoload
(defun jsonian-indent-line ()
  "Indent a single line.
The indent is determined by examining the previous line.  The
number of spaces is determined by
`jsonian-spaces-per-indentation'."
  (interactive)
  (indent-line-to (jsonian--get-indent-level)))

(defvar jsonian-mode-map (make-sparse-keymap))
(define-key jsonian-mode-map (kbd "C-c C-p") #'jsonian-path)
(define-key jsonian-mode-map (kbd "C-c C-s") #'jsonian-edit-string)
(define-key jsonian-mode-map (kbd "C-c C-e") #'jsonian-enclosing-item)
(define-key jsonian-mode-map (kbd "C-c C-f") #'jsonian-find)

(add-to-list 'hs-special-modes-alist '(jsonian-mode "{" "}" "/[*/]" nil))

(add-to-list 'magic-fallback-mode-alist '("^[{[]$" . jsonian-mode))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.json\\'" . jsonian-mode))

(defvar jsonian--font-lock-keywords
  (list (cons (regexp-opt '("true" "false")) 'font-lock-constant-face))
  "Keywords in JSON (true|false).")

;;;###autoload
(define-derived-mode jsonian-mode prog-mode "JSON"
  "Major mode for editing JSON files."
  :syntax-table jsonian-syntax-table
  (set (make-local-variable 'comment-start) "")
  (set (make-local-variable 'comment-end) "")
  (set (make-local-variable 'indent-line-function)
       #'jsonian-indent-line)
  (set (make-local-variable 'beginning-of-defun-function)
       #'jsonian-beginning-of-defun)
  (set (make-local-variable 'end-of-defun-function)
       #'jsonian-end-of-defun)
  (set (make-local-variable 'font-lock-defaults)
       '(jsonian--font-lock-keywords
         nil nil nil nil
         (font-lock-syntactic-face-function . jsonian--syntactic-face))))

;;;###autoload
(defun jsonian-enable-flycheck ()
  "Enable `jsonian-mode' for all checkers where `json-mode' is enabled."
  (interactive)
  (unless (boundp 'flycheck-checkers)
    (error "Flycheck needs to be loaded"))
  (defvar flycheck-checkers)
  (declare-function flycheck-checker-get "flycheck")
  (declare-function flycheck-add-mode "flycheck")
  (let ((checkers flycheck-checkers))
    (while checkers
      (when (seq-some (apply-partially #'eq 'json-mode) (flycheck-checker-get (car checkers) 'modes))
        (flycheck-add-mode (car checkers) 'jsonian-mode))
      (setq checkers (cdr checkers)))))

;;;###autoload
(defun jsonian-no-so-long-mode ()
  "Prevent `so-long-mode' from supplanting `jsonian-mode'."
  (interactive)
  (unless (boundp 'so-long-predicate)
    (user-error "`so-long' mode needs to be loaded"))
  (defvar so-long-predicate)
  (setq so-long-predicate
        (lambda ()
          (unless (eq major-mode 'jsonian-mode)
            (funcall so-long-predicate)))))

(provide 'jsonian)

;;; jsonian.el ends here
