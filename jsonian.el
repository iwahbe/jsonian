;;; jsonian.el --- A major mode for editing JSON files -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Ian Wahbe

;; Author: Ian Wahbe
;; URL: https://github.com/iwahbe/jsonian
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see
;; <http://www.gnu.org/licenses/>.

;;; Commentary:

;; `jsonian' provides a fully featured `major-mode' to view, navigate and edit JSON files.
;; Notable features include:
;; - `jsonian-path': Display the path to the JSON object at point.
;; - `jsonian-edit-string': Edit the uninterned string at point cleanly in a separate buffer.
;; - `jsonian-enclosing-item': Move point to the beginning of the collection enclosing point.
;; - `jsonian-find': A `find-file' style interface to navigating a JSON document.
;; - Automatic indentation discovery via `jsonian-indent-line'.
;;
;; When `jsonian' is loaded, it adds `jsonian-mode' and `jsonian-c-mode' to `auto-mode-alist'.
;; This will overwrite `javascript-mode' by default when opening a .json file.  It will
;; overwrite `fundamental-mode' when opening a .jsonc file
;;
;; To have `jsonian-mode' activate when any JSON like buffer is opened,
;; regardless of the extension, add
;;  (add-to-list 'magic-fallback-mode-alist '("^[{[]$" . jsonian-mode))
;; to your config after loading `jsonian'.


;;; Code:

(require 'cl-lib)
(require 'json)
(require 'seq)

(defgroup jsonian nil
  "A major mode for editing JSON."
  :prefix "jsonian-" :group 'languages
  :link `(url-link :tag "GitHub" "https://github.com/iwahbe/jsonian"))

(defcustom jsonian-ignore-font-lock (>= emacs-major-version 29)
  "This variable doesn't do anything anymore.

It will be removed in a future version of jsonian."
  :type 'boolean
  :group 'jsonian)

(define-obsolete-variable-alias 'jsonian-spaces-per-indentation 'jsonian-indentation "27.1")
(defcustom jsonian-indentation nil
  "The number of spaces each increase in indentation level indicates.
nil means that `jsonian-mode' will infer the correct indentation."
  :type '(choice (const nil) integer)
  :group 'jsonian)

(defcustom jsonian-default-indentation 4
  "The default number of spaces per indent for when it cannot be inferred."
  :type 'integer
  :group 'jsonian)

(defgroup jsonian-c nil
  "A major mode for editing JSON with comments."
  :prefix "jsonian-c-" :group 'jsonian)

;; Hoisted because it must be declared before use.
(defvar-local jsonian--cache nil
  "The buffer local cache of known locations in the current JSON file.
`jsonian--cache' is invalidated on buffer change.")


;; Manipulating and verifying JSON paths.
;;
;; A JSON Path is a unique identifier for a node in the buffer.  Internally, JSON
;; Paths are lists of strings and integers.  JSON Paths are unique, but multiple
;; string representations may parse into the same JSON Path.  For example
;; 'foo[3].bar' and '["foo"][3]["bar"]' both parse into '("foo" 3 "bar").

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
      (jsonian--snap-to-node)
      (let ((result (jsonian--reconstruct-path (jsonian--path))) display)
        (when (called-interactively-p 'interactive)
          (setq display (jsonian--display-path result (not plain)))
          (message "Path: %s" display)
          (kill-new display))
        result))))

(defun jsonian--cached-path (point head)
  "Compute `jsonian-path' with assistance from `jsonian--cache'.
HEAD is the path segment for POINT."
  (jsonian--ensure-cache)
  (if-let* ((node (gethash point (jsonian--cache-locations jsonian--cache))))
      ;; We have retrieved a cached value, so return it
      (reverse (jsonian--cached-node-path node))
    ;; Else cache the value and return it
    (let ((r (cons head (jsonian--path))))
      (jsonian--cache-node point (reverse r))
      r)))

(defun jsonian--path ()
  "Helper function for `jsonian-path'.
`jsonian--path' will parse back to the beginning of the file,
assembling the path it traversed as it goes.

The caller is responsible for ensuring that `point' begins on a valid node."
  ;; The number of previously encountered objects in this list (if we
  ;; are in a list).
  (cond
   ;; We are at a key
   ((and (eq (char-after) ?\")
         (save-excursion
           (and
            (jsonian--forward-token)
            (eq (char-after) ?:))))
    (when-let ((s (jsonian--string-at-pos (1+ (point)))))
      ;; If `s' is nil, it means that the string was invalid
      (jsonian--cached-path (prog1 (point)
                              (jsonian--up-node))
                            (buffer-substring-no-properties
                             (1+ (car s)) (1- (cdr s))))))
   ;; We are not at a key but we are not at the beginning, so we must be in an array
   ((save-excursion (jsonian--backward-token))
    (let ((index 0) done (p (point)))
      (while (not done)
        (when-let (back (jsonian--backward-node))
          (if (eq back 'start)
              (setq done t)
            (cl-incf index))))
      (jsonian--cached-path (prog1 p
                              (jsonian--up-node))
                            index)))
   ;; We are not in a array or object, so we must be at the top level
   (t nil)))

(defun jsonian--down-node ()
  "Move `point' into a container node.

Given the example with point at $:

 $\"foo\": {
    \"bar\": 3
  }

`jsonian--down-node' will move point so `char-after' is at \"bar\":

  \"foo\": {
   $\"bar\": 3
  }

This function assumes we are at the start of a node."
  (let ((start (point))
        (ret (pcase (char-after)
               ((or ?\[ ?\{)
                (and
                 (jsonian--forward-token)
                 ;; Prevent going into containers with no elements
                 (not (memq (char-after) '(?\] ?\})))))
               (?\" ;; We might be in a key, so lets check
                (jsonian--forward-token)
                (when (equal (char-after) ?:)
                  (progn
                    (jsonian--forward-token)
                    (jsonian--down-node)))))))
    (unless (eq ret t)
      (goto-char start))
    ret))

(defun jsonian--up-node ()
  "Move `point' to the enclosing node.

Given the example with point at $:

  {
    \"a\": 1,
   $\"b\": 2
  }

`jsonian--up-node' will move point so `char-after' is at the opening {:

 ${
    \"a\": 1,
   \"b\": 2
  }

This function assumes we are at the start of a node."
  (let* ((start (point))
         ;; Move to the enclosing container
         (ret (when-let ((enclosing (nth 1 (syntax-ppss))))
                (goto-char enclosing)
                (if (memq (char-after) '(?\{ ?\[))
                    t
                  (goto-char start)
                  nil))))
    ;; We have found an enclosing container and moved there. We now need only
    ;; deal with an associated key.
    (when ret
      (setq start (point))
      (unless (and (jsonian--backward-token)
                   (eq (char-after) ?:)
                   (jsonian--backward-token))
        (goto-char start))
      ret)))

(defun jsonian--forward-node ()
  "Move `point' forward a node.
`jsonian--forward-node' will not move up or down within a tree.

This function assumes we are at the start of a node."
  (let ((start (point))
        ;; We are starting at a valid node, which means one of:
        ;; - A plain value
        ;; - A key in an object
        (ret (pcase (char-after)
               ((or ?\[ ?\{) ; We are at the start of a list
                (forward-list)
                (jsonian--skip-chars-forward "\s\n\t")
                (if (eobp) 'eob (jsonian--forward-token-comma)))
               (?\"
                (jsonian--forward-token)
                (if (equal (char-after) ?\:)  ; `equal' to obviate the `eobp' check
                    ;; We are looking at a key, so traverse the key and the value.
                    (and (jsonian--forward-token) ; traverse the :
                         (jsonian--forward-node)) ; traverse the value node
                  ;; We are just looking at a string
                  (jsonian--forward-token-comma)))
               ;; Just a normal scalar value
               (_
                (jsonian--forward-token)
                (jsonian--forward-token-comma)))))
    (unless (eq ret t)
      (goto-char start))
    ret))

(defun jsonian--backward-node ()
  "Move `point' backward over one node.
`jsonian--backward-node' will not move up or down within a tree.

This function assumes we are at the start of a node."
  (let ((start (point))
        (ret (if (not (jsonian--backward-token))
                 'bob
               (pcase (char-after)
                 ;; This was a valid entry in a list or map, so keep going backwards
                 (?,
                  ;; Traverse back over the token
                  (jsonian--backward-token)
                  (when (if (memq (char-after) '(?\} ?\]))
                            (progn
                              (forward-char)
                              (backward-list)
                              t)
                          t)
                    (if (save-excursion (and (jsonian--backward-token)
                                             (eq (char-after) ?:)))
                        ;; We are at a key in an object, so traverse back the key as well.
                        (and (jsonian--backward-token) (jsonian--backward-token))
                      t)))
                 ((or ?\[ ?\{) 'start)
                 (_ (jsonian--unexpected-char :backward "one of '[', '{' or ','"))))))
    (unless (eq ret t)
      (goto-char start))
    ret))

(defun jsonian--forward-token-comma ()
  "Move `point' over a separating ','.

If the end of a container or the buffer is reached, then `eob'
or `end' will be sent, respectively.

If the JSON is invalid then `jsonian--unexpected-char' will be called."
  (pcase (char-after)
    ((or ?\] ?\}) 'end)
    (?, (jsonian--forward-token))
    (_ (jsonian--unexpected-char :forward "one of ']', '}' or ','"))))

(defun jsonian--backward-token ()
  "Move `point' to the previous JSON token.

`jsonian--backward-token' will skip over any whitespace it finds.

It is assumed that `point' starts at a JSON token."
  (jsonian--skip-chars-backward "\s\n\t")
  (let* ((needs-seperator t)
         (v (pcase (char-before)
              ;; No previous token, so do nothing
              ((pred null) nil)
              ;; Found a single char token, so move behind it
              ((or ?: ?, ?\[ ?\] ?\{ ?\})
               (setq needs-seperator nil)
               (backward-char) t)
              ;; Found a string, so traverse it
              (?\" (jsonian--backward-string) t)
              (?l (jsonian--backward-null) t)
              (?e (pcase (char-before (1- (point)))
                    (?u (jsonian--backward-true) t)
                    (?s (jsonian--backward-false) t)
                    (_ (save-excursion (backward-char)
                                       (jsonian--unexpected-char :backward "\"u\" or \"s\"")))))
              ((pred (lambda (c) (and (<= c ?9) (>= c ?0))))
               (jsonian--backward-number) t)
              (_ (jsonian--unexpected-char :backward "one of ':,[]{}\"le0123456789'")))))
    (when (and needs-seperator
               (not (memq (char-before) '(nil ?: ?, ?\[ ?\] ?\{ ?\} ?\s ?\t ?\n))))
      (jsonian--unexpected-char :backward "one of ':,[]{}\\s\\t\\n' or BOF"))
    v))

(defvar-local jsonian--last-token-end nil
  "The end of the last token that `jsonian--forward-token' parsed.

For example, given the following string with point at the
?| (`char-after' will be refer to ?,):

  1.2|,   3.4

`jsonian--forward-token' will move point to ?|:

   1.2,   |3.4

It will set the value of `jsonian--last-token-end' to

   1.2,|  3.4

If `jsonian--forward-token' returned nil, the value of
`jsonian--last-token-end' is undefined.")

(defun jsonian--forward-token (&optional stop-at-comments)
  "Move `point' to the next JSON token.

`jsonian--forward-token' will skip over any whitespace it finds.

By default, `jsonian--forward-token' skips over comments when in
`jsonian-c-mode' or errors on comments in plain `jsonian-mode'.
If STOP-AT-COMMENTS is non-nil and a comment is encountered in
`jsonian-c-mode', then comments are treated like tokens by
`jsonian--forward-token'.

It is assumed that `point' starts at a JSON token.

t is returned if `jsonian--forward-token' successfully traversed
a token, otherwise nil is returned."
  (let ((needs-seperator t))
    (pcase (char-after)
      ;; We are at the end of the buffer, so we can't do anything
      ((pred null) nil)
      ;; Found a single char token, so move ahead of it
      ((or ?: ?, ?\[ ?\] ?\{ ?\})
       (setq needs-seperator nil)
       (forward-char))
      ;; Found a string, so traverse it
      (?\" (jsonian--forward-string))
      ;; Otherwise we are looking at a non-string scalar token, so parse forward
      ;; until we find a separator or whitespace (which implies that the token is
      ;; over).
      (?t (jsonian--forward-true))
      (?f (jsonian--forward-false))
      (?n (jsonian--forward-null))
      ((pred (lambda (c) (and stop-at-comments
                             (derived-mode-p 'jsonian-c-mode)
                             (eq c ?/)
                             (memq (char-after (1+ (point))) '(?/ ?*)))))
            (forward-comment 1))
      ((pred (lambda (c) (or (and (<= c ?9) (>= c ?0)) (eq c ?-))))
       (jsonian--forward-number))
      ;; This is the set of chars that can start a token
      (_ (jsonian--unexpected-char :forward "one of ':,[]{}\"tfn0123456789-'")))
    (setq jsonian--last-token-end (point))
    ;; Skip forward over whitespace and comments
    (when (and (= (jsonian--skip-chars-forward "\s\n\t" stop-at-comments) 0)
               needs-seperator
               (not (memq (char-after) '(nil ?: ?, ?\[ ?\] ?\{ ?\} ?\s ?\t ?\n))))
      (jsonian--unexpected-char :forward "one of ':,[]{}\\s\\t\\n' or EOF")))
  (not (eobp)))

(defun jsonian--snap-to-node ()
  "Position `point' before a node.
This function moves forward through whitespace but backwards through the node.
nil is returned if `jsonian--snap-to-node' failed to move `point' to
before a node."
  (when (jsonian--snap-to-token)
    (pcase (char-after)
      ;; The token indicates that we are the second token within a "key: value"
      ;; node.
      (?: (jsonian--backward-token))
      ;; We are at the end of a node, but its not clear how far from the
      ;; front. Move back one token and try again.
      (?,
       (jsonian--backward-token)
       (jsonian--snap-to-node))
      ;; We are at the end of a container, so move back inside the container and
      ;; try again
      ((or ?\] ?\})
       (skip-chars-backward "\s\n\t}]") ; Skip out of enclosing nodes
       (backward-char)                  ; Skip into the last node being enclosed
       (jsonian--snap-to-node)) ; Return that node
      ;; We are either at the front of a node, or prefixed with a key
      (_  (if (save-excursion (and (jsonian--backward-token) (eq (char-after) ?:)))
              (progn
                (jsonian--backward-token)  ;; Move behind the :
                (jsonian--backward-token)) ;; Move behind the string
            t)))))

(defun jsonian--skip-chars-backward (chars)
  "Skip CHARS backwards in a comment aware way."
  (let ((start (point)))
    (while (or
            (> (skip-chars-backward chars) 0)
            (jsonian--backward-comment)))
    (- start (point))))

(defun jsonian--skip-chars-forward (chars &optional stop-at-comments)
  "Skip CHARS forward in a comment aware way.

If STOP-AT-COMMENTS is non-nil, then (comment . traveled) is
returned when a comment is encountered."
  (let ((start (point)))
    (while (or
            (> (skip-chars-forward chars) 0)
            (and (not stop-at-comments)
                 (jsonian--forward-comment))))
    (- (point) start)))

(defun jsonian--snap-to-token ()
  "Position `point' at the \"nearest\" token.
If `point' is within a token, it is moved to point at that token.
Otherwise, `point' is moved to point at the nearest token on the
same line.  Otherwise `point' is moved to point to the nearest
token period.

Nearest is defined to be point that minimizes (abs (- (point)
previous)).

Consider the following example, with `point' starting at $:

    { \"foo\":    \"fizz $buzz\" }

`jsonian--snap-to-token' will move the point so `char-after' is the ?\"
that begins \"fizz buzz\".

With the same example and different cursor position, we will see the same
result:

    { \"foo\": $   \"fizz buzz\" }

The cursor will move so `char-after' will give the ?:.  If we
move the starting point over:

    { \"foo\":   $ \"fizz buzz\" }

we instead move so that `char-after' gives the ?\" that begins
\"fizz buzz\"."
  ;; We are looking for the "nearest" token to position the cursor at.
  ;;
  ;; We do this by looking for the nearest token on the left and the right.  If we find
  ;; tokens on the left and the right, we take whichever is closest to `center', which is
  ;; where we started looking from.
  (let* ((center (point))
         left-end
         (left
          ;; Find the left most valid starting token
          (if-let (start (jsonian--pos-in-stringp))
              start
            (when-let (start (jsonian--enclosing-comment-p (point)))
              (goto-char start))

            (jsonian--skip-chars-backward "\s\t\n")
            (unless (bobp)
              (pcase (char-before)
                ((or ?: ?, ?\{ ?\} ?\[ ?\]) (1- (point)))
                (?\" (jsonian--backward-string)
                     (point))
                (_ (while (not (or (bobp)
                                   (memq (char-before) '(?: ?, ?\s ?\t ?\n ?\{ ?\} ?\[ ?\]))))
                     (backward-char))
                   (unless (bobp)
                     (point)))))))
         (right (cond
                 ;; If left=center, there is no point in trying to calculate `right',
                 ;; since it cannot be better then left.
                 ((eq left center) nil)
                 (left
                  ;; If we have a left token, we can just traverse forward from the left
                  ;; token to get the right token.
                  (goto-char left)
                  (when (and (jsonian--forward-token)
                             (>= center (setq left-end jsonian--last-token-end)))
                    ;; If center is within the node found by left, we take that
                    ;; token regardless of distance. This is necessary to ensure
                    ;; idenpotency for tightly packed tokens.
                    (point)))
                 (t
                  ;; We have no left token, so we need to parse to the right token.
                  (goto-char center)
                  (when-let (start (jsonian--enclosing-comment-p (point)))
                    (goto-char start))
                  (jsonian--skip-chars-forward "\s\t\n")
                  (unless (eobp)
                    (point))))))
    (goto-char
     (or
      (if (and left right)
          ;; If we have both left and right, we look at their line positions.
          (let ((center-line (line-number-at-pos center))
                (left-line (line-number-at-pos left))
                (right-line (line-number-at-pos right)))
            (cond
             ;; If `left' ^ `right' is on the same line as `center' we take that token.
             ((and (= center-line left-line)
                   (not (= center-line right-line)))
              left)
             ((and (= center-line right-line)
                   (not (= center-line left-line)))
              right)
             (t
              ;; If the tokens are on different lines, we set check against the end of the
              ;; left token instead of the left token itself.
              (if (<= (- center (if (and (not (= center-line left-line right-line)) left-end)
                                    left-end left))
                      (- right center))
                  left
                right))))
        (or left right))
      center))))

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

(defconst jsonian--complex-segment-regex "\\([[:blank:].\"\\[]\\|\\]\\)"
  "The set of characters that make a path complex.")

(defun jsonian--parse-path (str)
  "Parse STR as a JSON path.
A list of elements is returned."
  (unless (stringp str) (error "`jsonian--parse-path': Input not a string"))
  (setq str (substring-no-properties str))
  (cond
   ((string= str "") nil)
   ((string-match "^\\[[0-9]+\]" str)
    (cons (string-to-number (substring str 1 (1- (match-end 0))))
          (jsonian--parse-path (substring str (match-end 0)))))
   ((string-match-p "^\\[\"" str)
    (if-let* ((str-end (with-temp-buffer
                         (insert (substring str 1)) (goto-char (point-min))
                         (when (jsonian--forward-string)
                           (point))))
              (str-length (- str-end 3)))
        (cons (substring str 2 (1- str-end))
              (jsonian--parse-path
               (string-trim-left (substring str (+ str-length 2)) "\"\\]?")))
      (cons (string-trim-left str "\\[\"") nil)))
   ((string= "." (substring str 0 1))
    (if (not (string-match "[\.\[]" (substring str 1)))
        ;; We have found nothing to indicate another sequence, so this is the last node
        (cons (string-trim (substring str 1)) nil)
      (cons
       (string-trim (substring str 1 (match-end 0)))
       (jsonian--parse-path (substring str (match-end 0))))))
   ((string= " " (substring str 0 1))
    ;; We have found a leading whitespace not part of a segment, so ignore it.
    (jsonian--parse-path (substring str 1)))
   ;; There are no more fully valid parses, so look at invalid parses
   ((string-match "^\\[[0-9]+$" str)
    ;; A number without a closing ]
    (cons (string-to-number (substring str 1)) nil))
   ((string-match-p "^\\[" str)
    ;; We have found a string starting with [, it isn't a number, so parse it
    ;; like a string
    (if (string-match "\\]" str 1)
        ;; Found a terminator
        (cons (substring str 1 (1- (match-end 0)))
              (jsonian--parse-path (substring str (match-end 0))))
      ;; Did not find a terminator
      (cons (substring str 1) nil)))
   ((not (eq (string-match-p jsonian--complex-segment-regex str) 0))
    ;; If we are not at a character that cannot be part of a simple path,
    ;; attempt making it one.
    (jsonian--parse-path (concat "." str)))
   (t (user-error "Unexpected input: %s" str))))

(defun jsonian--simple-path-segment-p (segment)
  "If the string SEGMENT can be displayed simply, or if it needs to be escaped.
A segment is considered simple if and only if it does not contain any
- blanks
- period
- quotes
- square brackets"
  (not (string-match-p jsonian--complex-segment-regex segment)))

(defun jsonian--reconstruct-path (input)
  "Cleanup INPUT as the result of `jsonian--path'."
  (let (path)
    (seq-do (lambda (element)
              (if (or (stringp element) (numberp element))
                  (setq path (cons element path))
                (error "Unexpected element %s of type %s" element (type-of element))))
            input)
    path))

(defun jsonian--valid-path (path)
  "Check if PATH is a valid path in the current JSON buffer.
PATH should be a list of segments.  A path is considered valid if
it traverses existing structures in the buffer JSON.  It does not
need to be a leaf path."
  (save-excursion
    (goto-char (point-min))
    (jsonian--snap-to-token)
    (let (failed leaf current-segment traversed)
      (while (and path (not failed) (not leaf))
        (unless (seq-some
                 (lambda (x)
                   (when (equal (car x) (car path))
                     (cl-assert (car x) t "Found nil car")
                     (goto-char (cdr x))
                     (setq leaf (not (jsonian--at-collection (point))))
                     t))
                 (jsonian--cached-find-children traversed :segment current-segment))
          (setq failed t))
        (setq current-segment (car path)
              traversed (append traversed (list current-segment))
              path (cdr path)))
      ;; We reject if we have noticed a failure or exited early by hitting a
      ;; leaf node
      (when (and (not failed) (not path))
        (jsonian--cached-find-children traversed :segment current-segment)
        (point)))))


;; Traversal functions
;;
;; A set of utility functions for moving around a JSON buffer by the structured text.

;;;###autoload
(defun jsonian-enclosing-item (&optional arg)
  "Move point to the item enclosing the current point.
If ARG is not nil, move to the ARGth enclosing item."
  (interactive "P")
  (if arg
      (cl-assert (wholenump arg) t "Invalid input to `jsonian-enclosing-item'.")
    (setq arg 1))
  (unless (jsonian--snap-to-node)
    (user-error "Failed to find a JSON node at point"))
  (while (and (> arg 0) (jsonian--up-node))
    (cl-decf arg 1))
  (= arg 0))

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
         (jsonian--unexpected-char :backward ,(format "literal value \"%s\"" literal))))
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
         (jsonian--unexpected-char :forward ,(format "literal value \"%s\"" literal))))))

(jsonian--defun-literal-traversal "true")
(jsonian--defun-literal-traversal "false")
(jsonian--defun-literal-traversal "null")

(defun jsonian--forward-number ()
  "Parse a JSON number forward.

For the definition of a number, see https://www.json.org/json-en.html"
  (let ((point (point)) (valid t))
    (when (equal (char-after point) ?-) (setq point (1+ point))) ;; Sign
    ;; Whole number
    (if (equal (char-after point) ?0)
        (setq point (1+ point)) ;; Found a zero, the whole part is done
      (if (and (char-after point)
               (>= (char-after point) ?1)
               (<= (char-after point) ?9))
          (setq point (1+ point)) ;; If valid, increment over the first number.
        (setq valid nil)) ;; Otherwise, the number is not valid.
      ;; Parse the remaining whole part of the number
      (while (and (char-after point)
                  (>= (char-after point) ?0)
                  (<= (char-after point) ?9))
        (setq point (1+ point))))
    ;; Fractional
    (when (equal (char-after point) ?.)
      (setq point (1+ point))
      (unless (and (char-after point)
                   (>= (char-after point) ?0)
                   (<= (char-after point) ?9))
        (setq valid nil))
      (while (and (char-after point)
                  (>= (char-after point) ?0)
                  (<= (char-after point) ?9))
        (setq point (1+ point))))
    ;; Exponent
    (when (memq (char-after point) '(?e ?E))
      (setq point (1+ point))
      (when (memq (char-after point) '(?- ?+)) ;; Exponent sign
        (setq point (1+ point)))
      (unless (and (char-after point)
                   (>= (char-after point) ?0)
                   (<= (char-after point) ?9))
        (setq valid nil))
      (while (and (char-after point)
                  (>= (char-after point) ?0)
                  (<= (char-after point) ?9))
        (setq point (1+ point))))
    (when valid
      (goto-char point)
      t)))

(defun jsonian--backward-number ()
  "Parse a JSON number backward.

Here we execute the reverse of the flow chart described at
https://www.json.org/json-en.html:

                                     +------+    !=====!    !===!    !===!
>>--+-----+------------------+------>| 0-9* |--->| 1-9 |--->| - |<---| 0 |
    |     |                  |       +------+    !=====!    !===!    !===!
    |     |                  |          |           ^                  ^
    |     v                  |          v           |                  |
    |  +------+  +-----+  +-----+     +---+     +------+               |
    |  | 0-9* |->| +|- |->| e|E |  +--| . |---->| 0-9* |               |
    |  +------+  +-----+  +-----+  |  +---+     +------+               |
    |                              |                                   |
    |       exponent component     |  fraction component    sign       |
    |  --------------------------  | --------------------  ------      |
    |                              v                                   |
    +------------------------------+-----------------------------------+

The above diagram denotes valid stopping locations with boxes
outlined with = and !.  The flow starts with the >> at the middle
left."
  (when-let ((valid-stops
              (seq-filter
               #'identity
               (list
                (jsonian--backward-exponent (point))
                (jsonian--backward-fraction (point))
                (jsonian--backward-integer (point))))))
    (goto-char (seq-min valid-stops))))

(defun jsonian--backward-exponent (point)
  "Parse backward from POINT assuming an exponent segment of a JSON number."
  (let (found-number done)
    (while (and (not done) (char-before point)
                (<= (char-before point) ?9)
                (>= (char-before point) ?0))
      (if (= point (1+ (point-min)))
          (setq done t)
        (setq point (1- point)
              found-number t)))
    (when found-number ;; We need to see a number for an exponent
      (when (memq (char-before point) '(?+ ?-))
        (setq point (1- point)))
      (when (memq (char-before point) '(?e ?E))
        (or (jsonian--backward-fraction (1- point))
            (jsonian--backward-integer (1- point)))))))

(defun jsonian--backward-fraction (point)
  "Parse backward from POINT assuming no exponent segment of a JSON number."
  (let (found-number done)
    (while (and (not done) (char-before point)
                (<= (char-before point) ?9)
                (>= (char-before point) ?0))
      (if (= point (1+ (point-min)))
          (setq done t)
        (setq point (1- point)
              found-number t)))
    (when (and found-number (= (char-before point) ?.))
      (jsonian--backward-integer (1- point)))))

(defun jsonian--backward-integer (point)
  "Parse backward from POINT assuming you will only find a simple integer."
  (let (found-number done leading-valid)
    (when (equal (char-before point) ?0)
      (setq leading-valid (1- point)))
    (while (and (not done) (char-before point)
                (<= (char-before point) ?9)
                (>= (char-before point) ?0))
      (setq found-number (char-before point))
      (unless (eq found-number ?0)
        (setq leading-valid (1- point)))
      (if (= point (1+ (point-min)))
          (setq done t)
        (setq point (1- point))))
    (when leading-valid
      (if (and (char-before leading-valid)
               (eq (char-before leading-valid) ?-))
          (1- leading-valid)
        leading-valid))))

(defun jsonian--enclosing-comment-p (pos)
  "Check if POS is inside comment delimiters.
If in a comment, the first char before the comment deliminator is
returned."
  (when (and (derived-mode-p 'jsonian-c-mode)
             (>= pos (point-min))
             (<= pos (point-max)))
    (save-excursion
;; The behavior of `syntax-ppss' is worth considering.
;; This is confusing behavior.  For example:
;;   [ 1, 2, /* 42 */ 3 ]
;;           ^
;; is not in a comment, since it is part of the comment deliminator.
      (let ((s (syntax-ppss pos)))
        (cond
         ;; We are in a comment body
         ((nth 4 s) (nth 8 s))
         ;; We are between the characters of a two character comment opener.
         ((and
           (eq (char-before pos) ?/)
           (or
            (eq (char-after pos) ?/)
            (eq (char-after pos) ?*))
           (< pos (point-max)))
          ;; we still do the syntax check, because we might be in a string
          (setq s (syntax-ppss (1+ pos)))
          (when (nth 4 s)
            (nth 8 s)))
         ;; We are between the ending characters of a comment.
         ((and
           (eq (char-before pos) ?*)
           (eq (char-after pos) ?/)
           (> pos (point-min)))
          ;; we still do the syntax check, because we might be in a string
          (setq s (syntax-ppss (1- pos)))
          (when (nth 4 s)
            (nth 8 s))))))))

(defun jsonian--backward-comment ()
  "Traverse backward out of a comment."
  ;; In the body of a comment
  (when-let (start (or (jsonian--enclosing-comment-p (point))
                       (jsonian--enclosing-comment-p (1- (point)))))
    (goto-char start)))

(defun jsonian--forward-comment ()
  "Traverse forward out of a comment.
Must be at the comment boundary."
  (when (and
         (derived-mode-p 'jsonian-c-mode)
         (eq (char-after) ?/)
         (memq (char-after (1+ (point))) '(?/ ?*)))
    (forward-comment 1)))

(defun jsonian--backward-string ()
  "Move back a string, starting at the ending \"."
  (unless (eq (char-before) ?\")
    (error "`jsonian--backward-string': Expected to start at \""))
  (let ((end (point)))
    (backward-char) ; Skip over the previous "
    (jsonian--string-scan-back)
    (cons (point) end)))

(defun jsonian--forward-string ()
  "Move forward a string, starting at the beginning \"."
  (unless (eq (char-after) ?\")
    (error "`jsonian--forward-string': Expected to start at \", instead found %s"
           (if (char-after) (char-to-string (char-after)) "EOF")))
  (let ((start (point)))
    (when (jsonian--string-scan-forward t)
      (cons start (point)))))

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
            (setq done (point))))))
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
        done)
    (when start
      (goto-char (1+ start))
      (while (not (or done (eolp)))
        (cond
         ((= (char-after) ?\\)
          (forward-char 2))
         ((= (char-after) ?\")
          (setq done (point))
          (forward-char))
         ;; We are in the string, and not looking at a significant character. Scan forward
         ;; (in C) for an interesting character.
         (t (skip-chars-forward "^\"\\\\\n"))))
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
        (jsonian--skip-chars-forward "\s\t\n")
        (and (= (char-after) ?:) end)))))

(defun jsonian--after-key (pos)
  "Detect if POS are preceded by a key.
This is a short-cut version of `jsonian--pos-in-keyp' to improve
syntax highlighting time."
  (let ((x (char-before pos)))
    (while (and (not (bobp))
                (or (= x ?\ )
                    (= x ?\t)
                    (= x ?\n)
                    (= x ?\r)))
      (setq pos (1- pos)
            x (char-before pos)))
    (eq (char-before pos) ?:)))

(defun jsonian--pos-in-valuep ()
  "Determine if `point' is a JSON string value.
If a non-nil, the position of the beginning of the string is
returned."
  (and (not (jsonian--pos-in-keyp)) (jsonian--pos-in-stringp)))

(defun jsonian--string-at-pos (&optional pos)
  "Return (start . end) for a string at POS if it exists.
Otherwise nil is returned.  POS defaults to `point'."
  (save-excursion
    (when pos
      (goto-char pos))
    (let ((start (jsonian--pos-in-stringp)) end)
      (when start
        (setq end (jsonian--string-scan-forward)))
      (when (and start end)
        (cons start (1+ end))))))

(defun jsonian--get-string-region (type &optional pos)
  "Find the bounds of the string at POS in BUFFER.
Valid options for TYPE are `font-lock-string-face' and `font-lock-keyword-face'."
  (save-excursion
    (when pos
      (goto-char pos))
    (cond
     ((eq type 'font-lock-string-face)
      (and (jsonian--pos-in-valuep) (jsonian--string-at-pos)))
     ((eq type 'font-lock-keyword-face)
      (and (jsonian--pos-in-keyp) (jsonian--string-at-pos)))
     (t (error "'%s' is not a valid type" type)))))

(defun jsonian--at-collection (pos)
  "Check if POS is before a collection.
POS must be a valid node location."
  (save-excursion
    (goto-char pos)
    (jsonian--down-node)))


;; Supporting commands for `jsonian-edit-string'.
;;
;; This is the infrastructure for un-interning and re-interning strings to edit,
;; as well as the major mode used to do so.

(cl-defstruct jsonian--edit-return
  "Information necessary to return from `jsonian--edit-mode'."
  (match         nil :documentation "The (start . end) region of text being operated on.")
  (back-buffer   nil :documentation "The buffer to return back to.")
  (overlay       nil :documentation "The overlay used to highlight `match' text.")
  (delete-window nil :documentation "If the hosting `window' should be deleted upon exit."))

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

(defun jsonian--replace-text-in (start end text &optional buffer)
  "Set the content of the region (START to END) to TEXT in BUFFER.
BUFFER defaults to the current buffer."
  (with-current-buffer (or buffer (current-buffer))
    (goto-char start)
    (save-excursion
      (delete-region start end)
      (insert text))))

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
  ;; TODO: Should be a major mode
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


;; Caching JSON nodes and their locations
;;
;; All cached data is stored in the buffer local variable `jsonian--cache'.  It
;; is invalidated after the buffer is changed.

(defun jsonian--handle-change (&rest args)
  "Handle a change in the buffer.
`jsonian--handle-change' is designed to be called from the
`before-change-functions' hook.  ARGS is ignored."
  (ignore args)
  (setq jsonian--cache nil))

(cl-defstruct (jsonian--cache (:copier nil))
  "The jsonian node cache.  O(1) lookup is supported via either location or path."
  (locations (make-hash-table :test 'eql)   :documentation "A map of locations to nodes.")
  (paths     (make-hash-table :test 'equal) :documentation "A map of paths to locations."))

(cl-defstruct jsonian--cached-node
  "Information about a specific node in a JSON buffer."
  (children nil :documentation "A list of the locations of child nodes.
If non-nil, the child nodes should exist in cache.
If the node is a leaf node, CHILDREN may be set to `'leaf'.")
  (path nil :documentation "The full path to this node.")
  (segment nil :documentation "The last segment in the path to this node. `segment' should
be equal to the last element of `path'.")
  (type nil :documentation "The type of the node (as a string), used for display purposes.")
  (preview nil :documentation "A preview of the value, containing test properties."))

(cl-defun jsonian--cache-node (location path &key children segment type preview)
  "Cache information about a node.
LOCATION defines the primary key in the cache.
PATH is a secondary key in the cache.
Accepts the following optional keys:
CHILDREN is a list of child nodes in the form ( key . point).
SEGMENT is segment by which this node is accessed.  If PATH is
supplied, then segment should equal (car (butlast path)).
TYPE is the type of the JSON node (as a string).
PREVIEW is a (fontified) string preview of the node."
  (cl-assert
   (integerp location) t
   "Invalid location")
  (jsonian--ensure-cache)
  (puthash path location (jsonian--cache-paths jsonian--cache))
  (let ((existing (or
                   (gethash location
                            (jsonian--cache-locations jsonian--cache))
                   (make-jsonian--cached-node :path path))))
  (when children
    (setf (jsonian--cached-node-children existing) (mapcar #'cdr children)))
  (if segment
      (setf (jsonian--cached-node-segment existing) segment)
    (if path
        (setf (jsonian--cached-node-segment existing) (car (butlast path)))))
  (when type
    (setf (jsonian--cached-node-type existing) type))
  (when preview
    (setf (jsonian--cached-node-preview existing) preview))
  (puthash location existing (jsonian--cache-locations jsonian--cache))))

(defun jsonian--ensure-cache ()
  "Ensure that a valid cache exists, creating one if necessary."
  (cl-pushnew #'jsonian--handle-change before-change-functions)
  (unless jsonian--cache
    (setq jsonian--cache (make-jsonian--cache))))

(cl-defun jsonian--cached-find-children (path &key segment)
  "Call `jsonian--find-children' and cache the result.
If the result is already in the cache, just return it.  PATH and
SEGMENT refer to the parent.  Either PATH or SEGMENT must be
supplied."
  (jsonian--ensure-cache)
  (if-let* ((node (gethash (point) (jsonian--cache-locations jsonian--cache)))
            (children (jsonian--cached-node-children node)))
      (unless (eq children 'leaf)
        (seq-map
         (lambda (x)
           (cons
            (jsonian--cached-node-segment (gethash x (jsonian--cache-locations jsonian--cache)))
            x))
         children))
    (let ((result (jsonian--find-children)))
      (mapc
       (lambda (kv)
         (jsonian--cache-node (cdr kv) (append path (list (car kv)))
                              :segment (car kv)
                              :type (jsonian--node-type (cdr kv))
                              :preview (jsonian--node-preview (cdr kv))))
       result)
      (jsonian--cache-node (point) path
                           :children result
                           :segment segment
                           :type (jsonian--node-type (point))
                           :preview (jsonian--node-preview (point)))
      result)))


;; The `jsonian-find' function.
;;
;; `jsonian-find' is implemented on top of `completing-read'.

(defvar jsonian--find-buffer nil
  "The buffer in which `jsonian-find' is currently operating in.")

;;;###autoload
(defun jsonian-find (&optional path)
  "Navigate to a item in a JSON document.
If PATH is supplied, navigate to it."
  (interactive)
  (setq jsonian--find-buffer (current-buffer))
  (if-let ((selection
            (or path
                (completing-read "Select Element: " #'jsonian--find-completion nil t
                                 (save-excursion
                                   (jsonian--snap-to-node)
                                   (when-let* ((path (jsonian--reconstruct-path (jsonian--path)))
                                               (display (jsonian--display-path path t)))
                                     display))))))
      ;; We know that the path is valid since we chose it from the list of valid paths presented
      (goto-char (jsonian--valid-path (jsonian--parse-path selection)))))

(defun jsonian--find-completion (str predicate type)
  "The function passed to `completing-read' to handle navigating the buffer.
STR is the string to be completed.
PREDICATE is a function by which to filter possible matches.
TYPE is a flag specifying the type of completion."
  ;; See 21.6.7 Programmed Completion in the manual for more details
  ;; (elisp)Programmed Completion
  (with-current-buffer jsonian--find-buffer
    (jsonian--ensure-cache)
    (cond
     ((eq type nil)
      (jsonian--completing-nil (jsonian--parse-path str) predicate))
     ((eq type t)
      (jsonian--completing-t (jsonian--parse-path str) predicate))
     ((eq type 'lambda)
      (when (jsonian--valid-path (jsonian--parse-path str)) t))
     ((eq (car-safe type) 'boundaries)
      (cons 'boundaries (jsonian--completing-boundary str (cdr type))))
     ((eq type 'metadata)
      (cons 'metadata `((display-sort-function . ,(apply-partially #'jsonian--completing-sort str))
                       (affixation-function .
                             ,(apply-partially #'jsonian--completing-affixation str jsonian--cache)))))
     (t (error "Unexpected type `%s'" type)))))

(defun jsonian--completing-affixation (prefix cache paths)
  "Map each element in PATHS to (list <path> <type> <value>).
<type> and <value> may be nil if the necessary information is not cached.
PREFIX is the string currently being completed against.
CACHE is the value of `jsonian--cache' for the buffer being completed against."
  (let ((max-value (+ 8 (seq-reduce #'max (seq-map #'length paths) 0))))
    (mapcar (lambda (path)
              (let* ((is-index (string-match-p "^[0-9]+\\]$" path))
                     (full-path (append
                                 (butlast (jsonian--parse-path prefix))
                                 (jsonian--parse-path
                                  (if is-index
                                      (concat "[" path)
                                    path))))
                     (node (gethash
                            (gethash
                             full-path
                             (jsonian--cache-paths cache))
                            (jsonian--cache-locations cache)))
                     (type (and node (jsonian--cached-node-type node))))
                (list
                 (jsonian--pad-string (- max-value 4) (if is-index (concat "[" path) path) t)
                 (propertize
                  (jsonian--pad-string
                   10 (or type "") t)
                  'face 'font-lock-comment-face)
                 (or (and node (jsonian--cached-node-preview node)) ""))))
            paths)))

(defun jsonian--completing-sort (prefix paths)
  "The completing sort function for `jsonian--find-completion'.
PREFIX is the string to compare against.
PATHS is the list of returned paths."
  (if-let* ((segment (car-safe (last (jsonian--parse-path prefix))))
            (prefix (jsonian--display-segment-end segment)))
      (sort
       (seq-filter (apply-partially #'string-prefix-p prefix) paths)
       (if (seq-every-p (apply-partially #'string-match-p "^[0-9]+\]$") paths)
           ;; We are in an array, and indexes are numbers like "42]". We should sort them low to high.
           (lambda (x y) (< (string-to-number x) (string-to-number y)))
         ;; We are in a map, our keys are arbitrary strings, we should sort by edit distance.
         (lambda (x y) (< (string-distance prefix x) (string-distance prefix y)))))
    paths))

(defun jsonian--completing-t (path predicate)
  "Compute the set of all possible completions for PATH that satisfy PREDICATE."
  (if-let* ((parent-loc (jsonian--valid-path (butlast path)))
            (is-collection (jsonian--at-collection parent-loc)))
    (let ((result (seq-map
                   (lambda (x)
                     ;; We trim of the leading "[" or "." since it already exists
                     (let ((path (jsonian--display-path (list (car x)) t)))
                       (if (> (length path) 0)
                           (substring path 1)
                         path)))
                   (save-excursion
                     (goto-char parent-loc)
                     (jsonian--cached-find-children path)))))
      (if predicate
          (seq-filter predicate result)
        result))))

(defun jsonian--completing-nil (path &optional predicate)
  "The nil component of `jsonian--find-completion'.
PATH is a a list of path segments.  PREDICATE is a function that
filters values.  It takes a string as argument.  According to the
docs: The function should return nil if there are no matches; it
should return t if the specified string is a unique and exact
match; and it should return the longest common prefix substring
of all matches otherwise."
  (save-excursion
    (let* ((final (car-safe (last path)))
           (final-str (if final
                          (if (numberp final)
                              (number-to-string final)
                            final)
                        ""))
           (result
            (if-let* ((parent-loc (jsonian--valid-path (butlast path)))
                      (is-collection (jsonian--at-collection parent-loc)))
                (save-excursion
                  (goto-char parent-loc)
                  (seq-filter
                   (lambda (kv)
                     (let ((k (if (car kv)
                                  (if (numberp (car kv))
                                      (number-to-string (car kv))
                                    (car kv)))))
                       (string= final-str (substring k 0 (min (length final-str) (length k))))))
                   (jsonian--cached-find-children path))))))
      (setq result
            (if predicate
                (seq-filter predicate result)
              result))
      (cond
       ((not result) nil)
       ((= 1 (length result)) t)
       (t (substring
           ;; We trim of the leading "[" or "." since it already exists
           (jsonian--display-path
            (list (jsonian--longest-common-substring (mapcar #'car result))) t)
           1))))))

(defun jsonian--completing-boundary (str suffix)
  "Calculate the completion boundary for `jsonian--find-completion'.
Here STR represents the completing string and SUFFIX the string after point."
  ;; We first check if we are inside a string segment: ["INSIDE"]
  (with-temp-buffer
    (insert str suffix)
    (goto-char (1+ (length str)))
    (if-let ((str-start (jsonian--pos-in-stringp)))
        (cons
         str-start
         (progn
           (jsonian--string-scan-forward)
           (-  (point) (length str) 1)))
      ;; Not in a string, so we can look backward and forward for dividing chars
      ;; `?\[', `?\]', `?\"' and `?.'
      (cons
       (save-excursion
         (while (and
                 (char-before)
                 (not (eq (char-before) ?\[))
                 (not (eq (char-before) ?\"))
                 (not (eq (char-before) ?.)))
           (backward-char))
         (1- (point)))
       (- (progn (while (and
                         (char-after)
                         (not (eq (char-after) ?\]))
                         (not (eq (char-after) ?\"))
                         (not (eq (char-after) ?.)))
                   (forward-char))
                 (point))
          (length str) 1)))))

(defun jsonian--node-type (pos)
  "Find the type of the node at POS.
POS must be at the beginning of a node.  If no type is found, nil
is returned."
  (save-excursion
    (goto-char pos)
    ;; Skip past a key if present
    (when (eq (char-after) ?\")
      (unless (and (jsonian--forward-token)
                   (eq (char-after) ?:)
                   (jsonian--forward-token))
        (goto-char pos)))
    (pcase (char-after)
      (?\" "string")
      ((or ?t ?f) "boolean")
      (?n "null")
      (?\[ "array")
      (?\{ "object")
      ((pred (lambda (n)
               (and (<= n ?9)
                    (>= n ?0))))
       "number"))))

(defun jsonian--node-preview (pos)
  "Provide a preview of the value of the node at POS.

POS must be a valid node."
  (save-excursion
    (goto-char pos)
    ;; Skip past a key if present
    (when (eq (char-after) ?\")
      (if (and (jsonian--forward-token) (eq (char-after) ?:))
          (jsonian--forward-token)
        (goto-char pos)))
    (pcase (char-after)
      ;; We preview arrays and objects specially, since they are often arbitrarily large.
      (?\[ (propertize "[ array ]" 'face 'font-lock-type-face))
      (?\{ (propertize "{ object }" 'face 'font-lock-type-face))
      (_ (buffer-substring (point) (and
                                    (jsonian--forward-token)
                                    jsonian--last-token-end))))))

(defun jsonian--find-children ()
  "Return a list of elements in the collection at point.
nil is returned if the object at point is not a collection."
  (save-excursion
    (when (jsonian--down-node)
      (let (elements done
                     (obj-p (save-excursion (and (jsonian--forward-token)
                                                 (eq (char-after) ?:))))
                     (count 0))
        (while (not done)
          (setq elements
                (cons
                 (cons
                  (if obj-p
                      (let ((end (save-excursion (forward-char) (jsonian--pos-in-keyp t))))
                        (buffer-substring-no-properties (1+ (point)) (1- end)))
                    (prog1 count (cl-incf count)))
                  (point))
                 elements))
          (setq done (eq (jsonian--forward-node) 'end)))
        elements))))


;; The jsonian major mode and the basic functions that support it.
;; Most functions in this page hook into existing emacs functionality.

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

(defvar jsonian-mode-map
  (let ((km (make-sparse-keymap)))
    (define-key km (kbd "C-c C-p") #'jsonian-path)
    (define-key km (kbd "C-c C-s") #'jsonian-edit-string)
    (define-key km (kbd "C-c C-e") #'jsonian-enclosing-item)
    (define-key km (kbd "C-c C-f") #'jsonian-find)
    (define-key km (kbd "C-c C-w") #'jsonian-format-region)
    km)
  "The mode-map for `jsonian-mode'.")

;;;###autoload
(define-derived-mode jsonian-mode prog-mode "JSON"
  "Major mode for editing JSON files."
  :syntax-table jsonian-syntax-table
  :group 'jsonian
  (set (make-local-variable 'comment-start) "")
  (set (make-local-variable 'comment-end) "")
  (set (make-local-variable 'indent-line-function)
       #'jsonian-indent-line)
  (set (make-local-variable 'indent-region-function)
       #'jsonian-indent-region)
  (set (make-local-variable 'beginning-of-defun-function)
       #'jsonian-beginning-of-defun)
  (set (make-local-variable 'end-of-defun-function)
       #'jsonian-end-of-defun)
  (set (make-local-variable 'font-lock-defaults)
       '(jsonian--font-lock-keywords
         nil nil nil nil
         (font-lock-syntactic-face-function . jsonian--syntactic-face)))
  (cl-pushnew #'jsonian--handle-change before-change-functions)
  (advice-add #'narrow-to-defun :before-until #'jsonian--correct-narrow-to-defun))

(defun jsonian--syntactic-face (state)
  "The syntactic face function for the position represented by STATE.
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

(add-to-list 'hs-special-modes-alist '(jsonian-mode "{" "}" "/[*/]" nil))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.json\\'" . jsonian-mode))

(defvar jsonian--font-lock-keywords
  (list (cons (regexp-opt '("true" "false" "null")) 'font-lock-constant-face))
  "Keywords in JSON (true|false|null).")

(defun jsonian--infer-indentation ()
  "Infer the level of indentation at point."
  (save-excursion
    (forward-line 0)
    (let ((indent nil)
          (origin (point))
          (done nil)
          parent-position)
      (while (not done)
        (setq parent-position (nth 1 (syntax-ppss)))
        (if parent-position
            (progn
              (setq indent (jsonian--infer-indentation-from-container
                            parent-position
                            origin))
              (if indent
                  (setq done t)
                (goto-char parent-position)))
          (setq done t)))
      (unless indent
        (goto-char (point-min))
        (forward-comment (point-max))
        (when (memq (char-after) '(?\[ ?{))
          (setq indent (jsonian--infer-indentation-from-container (point)))))
      indent)))

(defun jsonian--infer-indentation-from-container
    (container-position &optional end)
  "Infer the level of indentation from array/object at CONTAINER-POSITION.

If END is non-nil, inspect only before it."
  (save-excursion
    (let (indent)
      (goto-char container-position)
      (forward-char)
      ;; TODO: Should we ignore comments?
      (skip-chars-forward "\s\t")
      (when (eolp)
        (skip-chars-forward "\s\t\n")
        (when (and (not (memq (char-after) '(?\] ?})))
                   (or (not end) (< (point) end)))
          (setq indent (- (current-column)
                          (progn
                            (goto-char container-position)
                            (current-column))))
          (and (< 0 indent) indent))))))

(defun jsonian--indentation-spaces ()
  "The number of spaces per indentation level.
Either set or inferred."
  (or
   jsonian-indentation
   (if-let* ((indent (jsonian--infer-indentation))
             (not-zero (> indent 0)))
       indent
     jsonian-default-indentation)))

;;;###autoload
(defun jsonian-indent-line ()
  "Indent a single line.
The indent is determined by examining the previous line.  The
number of spaces is determined by `jsonian-indentation' if it is
set, otherwise it is inferred from the document."
  (interactive)
  (let* ((indent (jsonian--indentation-spaces))
         (indent-level (jsonian--get-indent-level indent))
         (current-indent
          (save-excursion (back-to-indentation) (current-column))))
    (if (<= (current-column) current-indent)
        ;; The cursor is on the left margin.  Moving to the new indent.
        (indent-line-to indent-level)
      ;; Keeps current relative position.
      (save-excursion (indent-line-to indent-level)))))

(defun jsonian--get-indent-level (indent &optional previous-level parent-level)
  "Find the indentation level of the current line.
The indentation level of the current line is derived from the
indentation level of the previous line.  INDENT is the number of
spaces in each indentation level.

If PREVIOUS-LEVEL is non-nil, it is used as the indentation column of
the previous member.

If PARENT-LEVEL is non-nil, it is used as the indentation column of
the parent member."
  (save-excursion
    (forward-line 0)
    (if (jsonian--enclosing-comment-p (point))
        ;; Inside comments.  Keep as is.
        (current-indentation)
      (skip-chars-forward "\s\t")
      (let ((next-char (char-after))
            previous-char)
        (cond
         ;; Indenting a close bracket.
         ((memq next-char '(?\] ?}))
          (or parent-level
              (progn
                (forward-char)
                (jsonian--current-indentation))))

         ;; Indenting a colon.
         ((eq next-char ?:)
          (+ (or previous-level
                 (jsonian--current-indentation))
             indent))

         ;; Otherwise.
         (t
          (setq previous-char (save-excursion
                                (forward-comment (- (point)))
                                (char-before)))
          (if (eq previous-char ?:)
              ;; After a colon.
              ;;
              ;; {
              ;;   "aaa":
              ;;     111
              ;; }
              (+ (or previous-level
                     (jsonian--current-indentation))
                 indent)
            ;; Indening a value.
            (or previous-level
                (if (progn
                      (jsonian--backward-member)
                      (eq (char-before) ?,))
                    ;; The current member isn't the first member.
                    ;; Align to the preceding sibling.
                    (progn
                      (backward-char)
                      (jsonian--current-indentation))
                  (if (memq (char-before) '(?\[ ?{))
                      ;; The current member is the first member.
                      ;; Align to the parent.
                      (+ (or parent-level
                             (progn
                               (backward-char)
                               (jsonian--current-indentation)))
                         indent)
                    ;; Beginning of the buffer.
                    0))))))))))

(defun jsonian--backward-member ()
  "Move point to the end of the previous member or open bracket.

After returning from this function, `char-before' should return a comma,
open brackets, or nil (beginning of the buffer)."
  (let ((done nil))
    (while (not done)
      (skip-chars-backward "^,[]{}\"/\n")
      (cond
       ;; Found it.
       ((or (bobp)
            (memq (char-before) '(?, ?\[ ?{)))
        (setq done t))

       ;; Close brackets or strings.
       ((memq (char-before) '(?\] ?} ?\"))
        (backward-sexp))

       ;; Maybe comments.
       ((memq (char-before) '(?/ ?\n))
        (if (jsonian--enclosing-comment-p (1- (point)))
            (jsonian--backward-comment)
          (backward-char)))))))

(defun jsonian--current-indentation ()
  "Return the indentation level of the current member.

It is the indentation level of the current or preceding member which
is either at the beginning of a line or at the beginning of the
containing array/object."
  (save-excursion
    ;; FIXME: maybe, we should align to comments at the beginning of a
    ;; line if any.
    (jsonian--backward-member)
    (while (and (save-excursion
                  (forward-comment (point-max))
                  (skip-chars-backward "\s\t")
                  (not (bolp)))
                (eq (char-before) ?,))
      (backward-char)
      (jsonian--backward-member))
    (forward-comment (point-max))
    (current-column)))

;;;###autoload
(defun jsonian-indent-region (start end)
  "Indent the region from START to END."
  (interactive "r")
  (save-excursion
    (let ((indent (jsonian--indentation-spaces))
          ;; Indent levels of siblings, parent, grand parent, and so on.
          (levels '())
          progress
          next-char
          parser-state)
      (setq end (copy-marker end))
      (goto-char start)
      (jsonian-indent-line)
      (when (jsonian--enclosing-comment-p (point))
        (jsonian--backward-comment))
      (setq parser-state (syntax-ppss))
      ;; Exit from a string.
      (when (nth 3 parser-state)
        (goto-char (nth 8 parser-state)))
      (setq progress (make-progress-reporter "Indenting region..." (point) end))
      ;; Scan forward and indent lines.
      (while (< (point) end)
        (progress-reporter-update progress (point))
        (skip-chars-forward "^[]{}\"/\n")
        (setq next-char (char-after))
        (cond
         ;; Found a new line.  Indent it.  Use cache if available.
         ;; Otherwise, indent as normal and cache it.
         ((eq next-char ?\n)
          (forward-char)
          (skip-chars-forward "\s\t")
          ;; Do not indent empty lines.
          (when (and (not (eolp)) (< (point) end))
            (if levels
                (indent-line-to (jsonian--get-indent-level indent
                                                           (nth 0 levels)
                                                           (nth 1 levels)))
              (jsonian-indent-line)
              (push (jsonian--current-indentation) levels))))

         ;; Open brackets.
         ((memq next-char '(?\[ ?{))
          (push
           ;; If the bracket is at the end of the line, current
           ;; indentation level + `indent' is the indentation level of
           ;; children.
           (if (save-excursion
                 (forward-char)
                 (skip-chars-forward "\s\t")
                 (eolp))
               (prog1
                   (+ (if levels
                          (car levels)
                        (jsonian--current-indentation))
                      indent)
                 (forward-char))
             ;; Otherwise, this line have the first child, so record
             ;; its column to the cache.
             ;;
             ;; Example:
             ;; [ 1,
             ;;   2,
             ;;   3 ]
             (forward-char)
             (skip-chars-forward "\s\t")
             (current-column))
           levels))

         ;; Close brackets.
         ((memq next-char '(?\] ?}))
          (pop levels)
          (forward-char))

         ;; Strings.
         ((eq next-char ?\")
          (forward-sexp))

         ;; Maybe comments.
         ((eq next-char ?/)
          (if (forward-comment 1)
              (when (eq (char-before) ?\n)
                (backward-char))
            (forward-char)))))
      (progress-reporter-done progress))
    (set-marker end nil nil)))

(defmacro jsonian--huge-edit (start end &rest body)
  "Evaluate form BODY with optimizations for huge edits.
Run the change hooks just once like `combine-change-calls'.
Create undo entries as if the contents from START to END are replaced at once.
BODY must not modify buffer outside the region (START END), nor move any markers
out of the region."
  (declare (debug (form form def-body)) (indent 2))
  (let ((start-value (make-symbol "start"))
        (end-value (make-symbol "end")))
    `(let ((,start-value ,start)
           (,end-value ,end))
       ;; WORKAROUND: If buffer-undo-list is nil, combine-change-calls shows
       ;; unnecessary message.
       ;; https://git.savannah.gnu.org/cgit/emacs.git/commit/?id=977630b5285809a57e50ff5f38d9c34247b549a7
       (unless buffer-undo-list
         (push (point) buffer-undo-list))
       (,(if (fboundp 'combine-change-calls)
             'combine-change-calls
           'combine-after-change-calls)
        ,start-value
        ,end-value
        (jsonian--huge-edit-1 ,start-value ,end-value (lambda () ,@body))))))

(defun jsonian--huge-edit-1 (start end body)
  "Evaluate a function BODY with optimizations for huge edits.
Create undo entries as if the contents from START to END are replaced at once.
BODY must not modify buffer outside the region (START END), nor move any markers
out of the region."
  (let ((old-undo-list buffer-undo-list)
        (undo-inhibit-record-point t)
        deletion-undo-list)
    ;; Clear the undo list.
    (buffer-disable-undo)
    (buffer-enable-undo)
    (unwind-protect
        (atomic-change-group
          (delete-region start end)
          ;; This contains restoreing the region and markers inside it.
          (setq deletion-undo-list buffer-undo-list)
          (primitive-undo (length deletion-undo-list) deletion-undo-list))
      (setq buffer-undo-list old-undo-list))
    (setq start (copy-marker start))
    (setq end (copy-marker end))
    (buffer-disable-undo)
    (unwind-protect
        (funcall body)
      ;; Note that setting `buffer-undo-list' enables undo again.
      (setq buffer-undo-list
            (append (cons
                     (cons (jsonian--free-marker start)
                           (jsonian--free-marker end))
                     deletion-undo-list)
                    old-undo-list)))))

(defun jsonian--free-marker (marker)
  "Make MARKER pointing nowhere and return the old position."
  (prog1 (marker-position marker)
    (set-marker marker nil nil)))

;;;###autoload
(defun jsonian-format-region (start end &optional minimize)
  "Format the region (START . END).

If MINIMIZE is non-nil, minimize the region instead of expanding it."
  (interactive "*r\nP")
  (let ((current-point (point-marker)))
    (jsonian--huge-edit start end
      (let ((end (progn (goto-char end) (point-marker))))
        (goto-char start)
        (jsonian--snap-to-token)
        (let* ((indent (jsonian--indentation-spaces))
               (indent-level (jsonian--get-indent-level indent))
               (undo-inhibit-record-point t)
               (next-token (make-marker))
               ;; Don't allocate a new string each time you add indentation.
               ;;
               ;; In effect, this is where we intern strings on behalf of elisp.
               (indent-strings '("\n"))
               (progress (make-progress-reporter "Formatting region..." start (* (- end start) 1.5))))
          (set-marker-insertion-type next-token t)
          (while (and
                  (<= (point) end)
                  (jsonian--forward-token t))
            (progress-reporter-update progress (point))
            ;; Delete the whitespace between the old token and the next token.
            (set-marker next-token (point))
            (delete-region jsonian--last-token-end (point))
            (unless minimize
              ;; Unless we are minimizing, insert the appropriate whitespace.
              (cond
               ;; A space separates : from the next token
               ;;
               ;;    "foo": bar
               ;;          ^space
               ((eq (char-before jsonian--last-token-end) ?:)
                (goto-char jsonian--last-token-end)
                (insert " ")
                (goto-char next-token))
               ;; If the second of the abutting tokens is a ",", then we don't make any
               ;; adjustments.
               ((memq (char-after) '(?, ?:)))

               ;; Empty objects and arrays are formatted as {} and [], respectively.
               ((and (eq (char-before) ?\[) (eq (char-after) ?\])))
               ((and (eq (char-before) ?\{) (eq (char-after) ?\})))

               ;; All other items are separated by a new line, then the appropriate indentation.
               (t
                (when (memq (char-after) '(?\] ?\}))
                  (cl-decf indent-level indent))
                (when (memq (char-before jsonian--last-token-end) '(?\[ ?\{))
                  (cl-incf indent-level indent))
                (while (<= (length indent-strings) indent-level)
                  (setq indent-strings
                        (append indent-strings
                                (list (concat
                                       "\n"
                                       (make-string (length indent-strings)
                                                    ?\s))))))
                (insert (nth indent-level indent-strings))
                (goto-char next-token)))))
          (progress-reporter-done progress))))
    (goto-char current-point)))

(defun jsonian-beginning-of-defun (&optional arg)
  "Move to the beginning of the smallest object/array enclosing `POS'.
ARG is currently ignored."
  (ignore arg) ;; TODO use ARG correctly
  (and
   (jsonian--snap-to-node)
   (jsonian--up-node)))

(defun jsonian-end-of-defun (&optional arg)
  "Move to the end of the smallest object/array enclosing `POS'.
ARG is currently ignored."
  (ignore arg)
  (when (and
         (jsonian--snap-to-node)
         (jsonian--up-node))
    (pcase (char-after)
      ((or ?\[ ?\{)
       (forward-list))
      (?\"
       (and
        (jsonian--forward-token)
        (eq (char-after) ?:)
        (jsonian--forward-token)
        (when (memq (char-after) '(?\[ ?\{))
          (forward-list)))))
    t))

(defun jsonian-narrow-to-defun (&optional arg)
  "Narrows to region for `jsonian-mode'.  ARG is ignored."
  ;; Arg is present to comply with the function signature of `narrow-to-defun'.
  ;; Its value is ignored.
  (ignore arg)
  (let (start end)
    (when (setq start (save-excursion (and (jsonian-beginning-of-defun) (point))))
      (setq end (save-excursion (and (jsonian-end-of-defun) (point)))))
    (when (and start end)
      (narrow-to-region start end))))

(defun jsonian--correct-narrow-to-defun (&optional arg)
  "Correct `narrow-to-defun' for `jsonian-mode' via the advice system.
ARG is passed onto `jsonian-narrow-to-defun'.  This function is
designed to be installed with `advice-add' and `:before-until'."
  (interactive)
  (when (derived-mode-p 'jsonian-mode)
    (jsonian-narrow-to-defun arg)
    t))

(defvar jsonian--so-long-predicate nil
  "The function originally assigned to `so-long-predicate'.")

(defun jsonian-unload-function ()
  "Unload `jsonian'."
  (advice-remove #'narrow-to-defun #'jsonian--correct-narrow-to-defun)
  (defvar so-long-predicate)
  (when jsonian--so-long-predicate
    (setq so-long-predicate jsonian--so-long-predicate)))


;; The major mode for jsonian-c mode.

(defvar jsonian-c-syntax-table
  (let ((s (make-syntax-table jsonian-syntax-table)))
    ;; We set / to be a punctuation character with the following additional
    ;; properties:
    ;; 1 -> The first character to begin a (class a|b) comment
    ;; 2 -> The second character to begin a (class a) comment
    ;; 4 -> The second character to end a (class a|b) comment
    (modify-syntax-entry ?/ ". 124" s)
    ;; \n ends (class a) comments
    (modify-syntax-entry ?\n "> " s)
    ;; * is a punctuation character as well as:
    ;; 2 -> The second character to begin a (class b) comment
    ;; 3 -> The first character to end a (class b) comment
    ;; b -> Only effect class b
    (modify-syntax-entry ?* ". 23b" s)
    s)
  "The syntax table for jsonian-c-mode.")

;;;###autoload
(define-derived-mode jsonian-c-mode jsonian-mode "JSONC"
  "A major mode for editing JSON documents with comments."
  :syntax-table jsonian-c-syntax-table
  :group 'jsonian-c
  (set (make-local-variable 'comment-start) "// ")
  (set (make-local-variable 'comment-add) 1)
  (set (make-local-variable 'font-lock-syntax-table)
       jsonian-c-syntax-table))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.jsonc\\'" . jsonian-c-mode))


;; Foreign integration

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
      (when (seq-some (apply-partially #'eq 'json-mode)
                      (flycheck-checker-get (car checkers) 'modes))
        (flycheck-add-mode (car checkers) 'jsonian-mode))
      (setq checkers (cdr checkers)))))

;;;###autoload
(defun jsonian-no-so-long-mode ()
  "Prevent `so-long-mode' from supplanting `jsonian-mode'."
  (interactive)
  (unless (boundp 'so-long-predicate)
    (user-error "`so-long' mode needs to be loaded"))
  (defvar so-long-predicate)
  (setq jsonian--so-long-predicate so-long-predicate)
  (setq so-long-predicate
        (lambda ()
          (unless (eq major-mode 'jsonian-mode)
            (funcall jsonian--so-long-predicate)))))


;; Miscellaneous utility functions

(defun jsonian--pad-string (len string &optional pad-right)
  "Pad STRING to LEN by prefixing it with spaces."
  (cl-assert (wholenump len) nil "jsonian--pad-string")
  (if (<= len (length string))
      string
    (if pad-right
        (concat
         string
         (make-string (- len (length string)) ?\ ))
      (concat
       (make-string (- len (length string)) ?\ )
       string))))

(defun jsonian--type-index-string (type)
  "Return the string necessary to index into TYPE.
If TYPE does not support some form of indexing, then nil is
returned."
  (cond
   ((equal type "array") "[")
   ((equal type "object") ".")))

(defun jsonian--display-segment-end (segment)
  "Displays SEGMENT with it's closer.
For example the segment \"foo\" ends as \"foo\", while 3 ends as \"3]\".
The segment \"foo bar\" would end as \"foo bar\\\"]."
  (cond
   ((numberp segment) (format "%d]" segment))
   ((jsonian--simple-path-segment-p segment) segment)
   (t (format "[\"%s\"]" segment))))

(defun jsonian--longest-common-substring (strings)
  "Find the longest common sub-string among the list STRINGS."
  (let* ((sorted (sort strings #'string<))
         (first (car-safe sorted))
         (last (car-safe (last sorted)))
         (i 0) result)
    (while (and (< i (length first))
                (< i (length last))
                (not result))
      (if (= (aref first i) (aref last i))
          (setq i (1+ i))
        (setq result t)))
    (substring first 0 i)))

(defun jsonian--unexpected-char (direction &optional expecting)
  "Signal a `user-error' that EXPECTING was expected, but not found.
DIRECTION indicates if parsing is forward (:forward) or backward (:backward)."
  (user-error
   "%s: unexpected character '%s' at %d:%d%s\n%s"
   (jsonian--enclosing-public-frame)
   (let ((bound
          (cond
           ((eq direction :backward) (list #'bobp "BOB" #'char-before))
           ((eq direction :forward) (list #'eobp "EOB" #'char-after))
           (t (error "Expecting :forward or :backward, found %s" direction)))))
     (if (funcall (car bound))
         (cadr bound)
       (let ((c (funcall (caddr bound))))
         (cond
          ((eq c ?\n) "\\n")
          ((eq c ?\t) "\\t")
          (t (format "%c" c))))))
   (line-number-at-pos) (if (and (eq direction :backward) (> (current-column) 0))
                            (1- (current-column))
                          (current-column))
   (if expecting
       (format ": expecting %s" expecting)
     "")
   (let* ((column-start-pos (save-excursion (beginning-of-line) (point)))
          (column-end-pos (save-excursion (end-of-line) (point)))
          (window-start (max column-start-pos (- (point) 40)))
          (window-end (min column-end-pos (+ (point) 40))))
     (concat
      (buffer-substring window-start window-end) "\n"
      (make-string (let ((pos (- (point) window-start)))
                     (if (and (eq direction :backward) (> pos 0))
                         (1- pos)
                       pos))
                   ? )
      "^"))))

(defun jsonian--enclosing-public-frame ()
  "The public jsonian- function that directly encloses the current stack frame."
  ;; i=3 gets us to the function that called `jsonian--enclosing-public-frame'.
  (let* ((i 3) (frame (backtrace-frame i))
         (disp (lambda (x) (if (symbolp x) (symbol-name x) (format "%s" x))))
         ;; We take that function as a backup value
         (ret-val (funcall disp (nth 1 frame))))
    (while frame
      (let ((fn-name (funcall disp (nth 1 frame))))
        (if (and (string-prefix-p "jsonian-" fn-name)
                 (not (string-prefix-p "jsonian--" fn-name)))
            (setq ret-val fn-name
                  frame nil)
          (setq i (1+ i)
                frame (backtrace-frame i)))))
    ret-val))

(provide 'jsonian)

;;; jsonian.el ends here
