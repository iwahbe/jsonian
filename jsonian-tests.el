;;; jsonian-tests.el --- Tests for jsonian.el -*- lexical-bindings: t; -*-

;; Copyright (C) 2022 Ian Wahbe

;;; Commentary:

;; Tests for jsonian.el

;;; Code:

(require 'jsonian)
(require 'ert)

(defun jsonian--test-with-options (path point test setups)
  "Run a test against `jsonian-mode'.

PATH is the path to the file where the test takes place.

POINT is the starting location of the point during the test.

TEST is a function to run the body of the test.

SETUPS is a list with elements of the form (condition . action).
The test will be run for each setup given that calling the
predicate on PATH return a non-nil value."
  (dolist (s setups)
    (with-temp-buffer
      (insert-file-contents-literally path)
      (when (funcall (car s) path)
        (funcall (cdr s))
        (setq indent-tabs-mode nil)
        (goto-char point)
        (funcall test)))))

(defun jsonian--is-strict-json (path)
  "Check if PATH ends in .json."
  (string-suffix-p ".json" path))

(defun jsonian--force-lock ()
  "Force a font lock on the buffer.
This function should be used only in testing."
  (setq font-lock-major-mode nil)
  (syntax-ppss-flush-cache -1)
  (font-lock-set-defaults)
  (save-excursion
    (font-lock-fontify-region (point-min) (point-max))))

(defconst jsonian--test-setups
  (list
   (cons #'jsonian--is-strict-json #'jsonian-mode)
   (cons #'jsonian--is-strict-json (lambda ()
				     (setq jsonian-ignore-font-lock nil)
                                     (jsonian-mode)
                                     (jsonian--force-lock)))
   (cons #'identity #'jsonian-c-mode)
   (cons #'identity (lambda ()
		      (setq jsonian-ignore-font-lock nil)
                      (jsonian-c-mode)
                      (jsonian--force-lock))))
  "The standard test scenarios to run against.
We run the JSONC setup for all files, but we only run the JSON
setup for strictly JSON files.")

(defun jsonian--test-against-text (text actions &optional backward)
  "Call ACTIONS with TEXT as the current buffer.

`point' will correspond to each instance of $ in the buffer, and
there must be one action per instance of $.

Only the last action may edit the buffer.  Tests will be called on
a shared buffer.

If BACKWARD is non-nil, $ will be traversed backwards."
  (let (points)
    (with-temp-buffer
      (insert text) (goto-char (point-min))
      (while (search-forward "$" nil t)
        (delete-char -1)
        (setq points
              (cons (point) points)))
      (setq points (funcall (if backward #'identity #'reverse) points))
      (setq text (buffer-string)))
    ;; NOTE: `length=' is not available in Emacs 27.1
    (cl-assert (= (length actions) (length points)) nil
               (format
                "%s != %s: Action list didn't match the number of points"
                (length actions) (length points)))
    (with-temp-buffer
      (insert text)
      (jsonian-mode)
      (dolist (p points)
        (should (string= (buffer-string) text))
        (goto-char p)
        (funcall (prog1 (car actions)
                   (setq actions (cdr actions))))))))

(defmacro should-point (num)
  "Assert that NUM equals `point'."
  `(should (= (point) ,num)))

(defmacro with-file-and-point (file point &rest body)
  "Open the test file named FILE and go to POINT, then execute BODY."
  (declare (indent defun))
  `(jsonian--test-with-options
    (format "./test-assets/%s" ,file) ,point
    (lambda ()
      ,@body)
    jsonian--test-setups))

(ert-deftest jsonian--display-path ()
  (should (string=
           "[\"fo]o\"][\"bar\"][3][2][\"buzz\"]"
           (jsonian--display-path '("fo]o" "bar" 3 2 "buzz"))))
  (should (string=
           "[\"fo]o\"].bar[3][2].buzz"
           (jsonian--display-path '("fo]o" "bar" 3 2 "buzz") t))))

(ert-deftest jsonian--defun-traverse-literal ()
  (with-temp-buffer
    ;; Position the cursor at the `e'.
    (insert "\"foo\": true,") (backward-char)
    (should (= (char-before) ?e))
    (jsonian--backward-true)
    (backward-char)
    (should (= (char-after) ?\ ))
    (should-error (jsonian--forward-true))
    (should (= (char-after) ?\ ))
    (forward-char)
    (jsonian--forward-true)
    (should (= (char-after) ?,))))

(ert-deftest jsonian--traverse-string ()
  ;; Simple
  (with-temp-buffer
    (insert "s\"foo\"e") (goto-char (1+ (point-min)))
    (jsonian--forward-string)
    (should (= (char-after) ?e))
    (should (= (char-before) ?\"))
    (jsonian--backward-string)
    (should (= (char-before) ?s)))
  ;; Escaped "
  (with-temp-buffer
    (insert "s\" fizz \\\" buzz \\\"fizzbuzz\"e") (goto-char (1+ (point-min)))
    (jsonian--forward-string)
    (should (= (char-after) ?e))
    (jsonian--backward-string)
    (should (= (char-before) ?s)))
  ;; Escaped \
  (with-temp-buffer
    (insert "s\" pop \\\" goes \\\\\"weazel\"e") (goto-char (1+ (point-min)))
    (jsonian--forward-string)
    (should (= (char-after) ?w))
    (jsonian--backward-string)
    (should (= (char-before) ?s))))

(ert-deftest jsonian--enclosing-item ()
  (with-file-and-point "path1.json" 75
    (jsonian-enclosing-item)
    (should (= (point) 64))
    (jsonian-enclosing-item)
    (should (= (point) 34))
    (jsonian-enclosing-item)
    (should (= (point) (point-min)))))

(ert-deftest jsonian--string-scan-back ()
  (with-temp-buffer
    ;; beginning of buffer
    (insert "\"foo\"")
    (goto-char 2)
    (should (jsonian--string-scan-back))
    (should-point (point-min))
    (insert "12345 ")
    (goto-char 8)
    (should (jsonian--string-scan-back))
    (goto-char 3)
    (should-not (jsonian--string-scan-back))))

(ert-deftest jsonian--pos-in-keyp ()
  (cl-flet ((is (bool)
              (apply-partially
               (lambda (bool)
                 (if bool
                     (should (jsonian--pos-in-keyp))
                   (should (not (jsonian--pos-in-keyp)))))
               bool)))
    (jsonian--test-against-text
     "{
$\"$k1$\"$: $3,
\"$k2\"$  :  \"$value\",
\"$k3\"
        $:
                \"true\",
\"$\" : null
  }"
     (list
      (is nil) (is t) (is t) (is nil) (is nil)
      (is t) (is nil) (is nil)
      (is t) (is nil)
      (is t)))))

(ert-deftest jsonian-indent-specified ()
  "Load `indent1' and indent each line.
We test that all lines are unchanged"
  (let ((inhibit-message t))
    (dolist (file '("indent1.json" "pathological.jsonc"))
      (with-file-and-point file (point-min)
                           (let ((jsonian-indentation 4)
                                 (file-contents (buffer-string)))
                             (indent-region-line-by-line (point-min) (point-max))
                             (should (string= (buffer-string) file-contents))
                             (indent-region (point-min) (point-max))
                             (should (string= (buffer-string) file-contents)))))
    (with-file-and-point "path1.json" (point-min)
                         (let ((jsonian-indentation 4))
                           (dotimes (l (count-lines (point-min) (point-max)))
                             (jsonian-indent-line)
                             (forward-line))
                           (should (string= "{
    \"foo\": {
        \"bar\": 3
    },
    \"fizz\": [true, 2, \"3\", false, { \"some\": \"object\" }],
    \"thing1\": \"thing2\"
}
" (buffer-substring-no-properties (point-min) (point-max))))))))

(ert-deftest jsonian-path ()
  (jsonian--test-against-text
   "${
  \"f$oo\": {
    \"b$ar\": $3
  },
  $\"empty\": nu$ll,
  \"fizz\": [tr$ue, 2$, $\"3\", false, { \"some\": \"object\"$ }],
  \"thing1\": \"thing2\"
}
"
   (mapcar (lambda (expected)
             (apply-partially
              (lambda (e)
                (should
                 (equal (jsonian-path) e)))
              expected))
           '(nil
             ("foo")
             ("foo" "bar")
             ("foo" "bar")
             ("empty")
             ("empty")
             ("fizz" 0)
             ("fizz" 1)
             ("fizz" 2)
             ("fizz" 4 "some")))))

(ert-deftest null-values ()
  (jsonian--test-against-text
   "$${
  \"a\": null,
  \"b\": true
}
"
   (list
    (lambda () (should (= (jsonian-find ".b") 18)))
    (lambda () (should (= (jsonian-find ".a") 5))))))

(defun jsonian--test-traverse-tokens (text mode)
  "Check that `jsonian--forward-token' & `jsonian--backward-token' work.

TEXT is a string with each token start annotated with a ?$.

MODE is the `major-mode' to run the test under.

The test works by checking that each `jsonian--forward-token'
jumps from token start to token start. We use same strategy in
reverse to test `jsonian--backward-token'."
  (let* ((tokens (with-temp-buffer
                   (insert text) (goto-char (point-min))
                   (count-matches "\\$")))
         actual-token done initalized)
    (jsonian--test-against-text
     text
     ;; For each match, assert that the last jump got to this token and mark the current
     ;; jump as the destination of the next token.
     (make-list tokens
                (lambda ()
                  (unless initalized
                    (funcall mode)
                    (setq initalized t))
                  (when actual-token
                    (should (= actual-token (point))))
                  (should (not done))
                  (setq done (not (jsonian--forward-token))
                        actual-token (point)))))
    (should done)

    ;; Reset the test and now walk backwards
    (setq done nil
          actual-token nil
          initalized nil)
    (jsonian--test-against-text
     text
     ;; For each match, assert that the last jump got to this token and mark the current
     ;; jump as the destination of the next token.
     (make-list tokens
                (lambda ()
                  (unless initalized
                    (funcall mode)
                    (setq initalized t))
                  (when actual-token
                    (should (= actual-token (point))))
                  (should (not done))
                  (setq done (not (jsonian--backward-token))
                        actual-token (point))))
     'backwards)
    (should done)))

(ert-deftest traverse-tokens ()
  "Check that `jsonian--forward-token' & `jsonian--backward-token' work.

This is for non-commented `jsonian-mode' code."
  (jsonian--test-traverse-tokens
   "$[
    $\"[\"$,
    $true$, $false
    $\"]\"$,
    $false
$,    ${
        $\"a\"$: $1.3e8$,
        $\"neg\"$: $-123.456$,
        $\"b\"
            $:
            $2

        $,

        $\"c\"$: $3
    $}      $, $null

$]
" #'jsonian-mode))

(ert-deftest traverse-tokens-compressed ()
  "Like `traverse-tokens' but for a compressed string."
  (jsonian--test-traverse-tokens
   ;; This is the string:
   ;;
   ;; {"default":""},"isCmekEnabled":{"type":"boolean"}
   "${$\"default\"$:$\"\"$}$,$\"isCmekEnabled\"$:${$\"type\"$:$\"boolean\"$}" #'jsonian-mode))


(ert-deftest traverse-tokens-with-comments ()
  "Test token traversal with `jsonian-c-mode'.

We confirm that `jsonian--forward-token' and
`jsonian--backward-token' work with comments.

This test employs the same strategy as `traverse-tokens'."
  (jsonian--test-traverse-tokens
   "$[
    /* [ */
    // {
    $\"[\"$,
    $true$,
    $\"]\"$,
    // }
    /* ] */
    $false
    ${
        //
        $\"a\"$: $1$,
        //
        $\"b\"
            $:
            $2
        //
        $,
        //
        $\"c\"$: $3
        //
    $}
    /*
       a
         b
        c
     */
$]
" #'jsonian-c-mode))

(ert-deftest traverse-invalid-tokens ()
  "Assert that traversing an invalid node gives a helpful error message."
  (jsonian--test-against-text
   "[ $truefalse$, $3.1.4 $, $nil$]"
   (mapcar (lambda (f)
             (apply-partially
              (lambda (f) (should-error (funcall f) :type 'user-error))
              f))
           (list #'jsonian--forward-token
                 #'jsonian--backward-token
                 #'jsonian--forward-token
                 #'jsonian--backward-token
                 #'jsonian--forward-token
                 #'jsonian--backward-token))))

(ert-deftest position-before-token ()
  "Check that we are able to move `point' to position before a token."
  (cl-flet* ((test-mode (text mode)
               (let* ((result (with-temp-buffer
                                (insert text)
                                (goto-char (point-min))
                                (search-forward "|")
                                (cons
                                 ;; The desired point is one less then the index of |,
                                 ;; representing the char before |.
                                 ;;
                                 ;;If $ is before it, then we need one less because the $
                                 ;;will also be removed.
                                 (let ((end-pos (1- (point))))
                                   ;; `string-search' is not available on Emacs 27.1
                                   (if (save-excursion (goto-char (point-min))
                                                       (search-forward "$" end-pos t))
                                       (1- end-pos)
                                     end-pos))
                                 (progn
                                   (delete-char -1)
                                   (buffer-string)))))
                      (end-pos (car result))
                      (text (cdr result)))
                 (jsonian--test-against-text
                  text
                  (list (lambda ()
                          (funcall mode)
                          ;; We do the test twice. The first asserts that we traveled
                          ;; correctly. The second asserts that when we are before a
                          ;; token, we don't move on the next call.
                          (dotimes (_ 2)
                            (should (jsonian--snap-to-token))
                            (should (= end-pos (point)))))))))
             (test (text)
               (test-mode text #'jsonian-mode))
             (w-comments (text)
               (test-mode text #'jsonian-c-mode)))
    ;; Each test starts at $ and asserts that `point' ends at |.
    (test "{ \"foo\": |\"in$string\" }")
    (test "$\n|{\n }")
    (test "$  |{ \n }")
    (test "[  |${ \n } ]")
    (test "[ |3$.14 ]")
    (test "[ true$ |, false ]")
    (test "[ |tru$e , false ]")
    (test "[ true |$, false ]")
    (w-comments "[ true /* comment$ */ |]")
    (w-comments "[
/*false*/
|true
/*fa$lse*/
]")
    (w-comments "[
/*false*/
|true
/*fa$lse*/
]")
    (w-comments "|[
/*$false*/
true
/*false*/
]")
    (w-comments "[
/*false*/
true
/*false$*/
|]")
    (w-comments "{\"key\"// foo
/* separator: */ |: // separator end$
\"value\"
}")))

(ert-deftest traverse-nodes ()
  (jsonian--test-against-text
   "${ $$$\"one\": [ $$\"two\", $${ $$\"three\": 4 }, $$$[ ] ] }"
   (list
    ;; Beginning
    (lambda ()
      (should (eq (jsonian--down-node) t))
      (should (= (point) 3)))
    ;; "one":
    (lambda ()
      (should (eq (jsonian--forward-node) 'end))
      (should (= (point) 3)))
    (lambda ()
      (should (eq (jsonian--backward-node) 'start))
      (should (= (point) 3)))
    (lambda ()
      (should (eq (jsonian--down-node) t))
      (should (= (point) 12)))
    ;; "two"
    (lambda ()
      (should (eq (jsonian--forward-node) t))
      (should (= (point) 19)))
    (lambda ()
      (should (eq (jsonian--down-node) nil))
      (should (= (point) 12)))
    ;; {
    (lambda ()
      (should (eq (jsonian--down-node) t))
      (should (= (point) 21)))
    (lambda ()
      (should (eq (jsonian--forward-node) t))
      (should (= (point) 35)))
    ;; "three":
    (lambda ()
      (should (eq (jsonian--up-node) t))
      (should (= (point) 19)))
    (lambda ()
      (should (eq (jsonian--down-node) nil))
      (should (= (point) 21)))
    ;; [
    (lambda ()
      (should (eq (jsonian--down-node) nil))
      (should (eq (point) 35)))
    (lambda ()
      (should (eq (jsonian--forward-node) 'end))
      (should (eq (point) 35)))
    (lambda ()
      (should (eq (jsonian--up-node) t))
      (should (eq (point) 3))))))

(ert-deftest jsonian-simple-segment ()
  "Check that we correctly identify simple segments."
  (mapc
   (lambda (x)
     (should (eq (jsonian--simple-path-segment-p (car x)) (cdr x))))
   '(
     ("foo" . t)
     ("foo:bar" . t)
     ("fizz/buz" . t)
     ("bar." . nil)
     ("has space" . nil)
     ("has\ttab" . nil)
     ("no\"quotes\"" . nil)
     ("[squares" . nil)
     ("other]" . nil))))

(ert-deftest jsonian--parse-path ()
  "Check that we can parse paths."
  (mapc (lambda (x)
          (should (equal (jsonian--parse-path (car x)) (cdr x))))
        '(("." . (""))
          ("" . ())
          (".foo.bar" . ("foo" "bar"))
          ("[\"foo\"][123][\"bar\"]" . ("foo" 123 "bar"))
          (".foo[\"fizz.buzz\"]" . ("foo" "fizz.buzz"))
          (".foo[\"$ref\"].bar" . ("foo" "$ref" "bar")))))

(ert-deftest jsonian--partial-parse-paths ()
  "Because `jsonian--parse-path' is used by interactive contexts, we must succeed on partial paths as well."
  (mapc (lambda (x)
          (should (equal (jsonian--parse-path (car x)) (cdr x))))
        '((".bar[1pos][3]"    . ("bar" "1pos" 3))
          (".bar[1pos]"       . ("bar" "1pos"))
          (".bar[1pos"        . ("bar" "1pos"))
          (".foo[1"           . ("foo" 1))
          (".foo[\"f"         . ("foo" "f"))
          (".foo[\" e"        . ("foo" " e"))
          (".foo["            . ("foo" ""))
          (".foo[\""          . ("foo" ""))
          (".foo. e"          . ("foo" "e"))
          (".foo.e "          . ("foo" "e"))
          (".foo.bar."        . ("foo" "bar" ""))
          ("[0].foo.bar."     . (0 "foo" "bar" ""))
          ("[\"0\"].foo.bar." . ("0" "foo" "bar" ""))
          (".foo[\"bar\""     . ("foo" "bar"))
          ("$ref.bar"         . ("$ref" "bar")))))

(ert-deftest jsonian--completing-boundary ()
  "Check that completing boundary works as expected.
Specifically, we need to comply with what `completion-boundaries' describes."
  (mapc (lambda (x)
          (let ((result (jsonian--completing-boundary (caar x) (cdar x))))
            (should (equal result (cdr x)))))
        '((("foo.bar"     . ".baz")    . (4 . 0))
          (("foo.bar."    . ".baz")    . (8 . 0))
          (("foo.bar."    . "")        . (8 . 0))
          ((".bar"        . "baz")     . (1 . 3))
          (("foo.bar"     . "")        . (4 . 0))
          ((".foo[\"fizz" . "buzz\"]") . (6 . 5))
          ((".foo[" . ""             ) . (5 . 0)))))

(ert-deftest jsonian--array-find-children ()
  "Check that we can find the children of arrays correctly."
  (with-file-and-point "children1.json" 7
    (should (equal
             (jsonian--find-children)
             '((1 . 58) (0 . 35))))))

(ert-deftest jsonian-indent-line ()
  (with-file-and-point
   "path1.json" 107
   (insert ",")
   (let ((inhibit-message t))
     (funcall-interactively #'newline-and-indent))
   (should (= (point) 111))))

(ert-deftest jsonian-indered-indent ()
  "Check that we correctly infer the indentation of our test files."
  (mapc (lambda (file)
          (with-file-and-point (concat file ".json") (point-min)
            (let ((file-contents (buffer-string))
                  (jsonian-default-indentation 7)
                  (inhibit-message t))
              (indent-region-line-by-line (point-min) (point-max))
              (should (string= (buffer-string) file-contents)))))
        '("indent1" "children1" "path1")))

(ert-deftest jsonian-c-path ()
  "Check that we can get the path in JSONC files."
  (with-file-and-point "basic.jsonc" 184
    (should (equal (jsonian-path) '("fizz" "rule")))))

(ert-deftest jsonian-nested-array-path ()
  "Check that `jsonian-path' works in heavily nested arrays."
  (with-file-and-point "nested1.json" 55
    (should (equal (jsonian-path) '("arrays" 1 1 1 1 1 1 1 1 1 0)))))

(ert-deftest jsonian-nested-object-path ()
  "Check that `jsonian-path' works in heavily nested objects."
  (with-file-and-point "nested1.json" 125
    (should (equal (jsonian-path) '("objects" "1" "2" "3" "4" "5" "6" "7")))))

(ert-deftest jsonian-nested-mixed-path ()
  "Check that `jsonian-path' works within objects and arrays."
  (with-file-and-point "nested1.json" 187
    (should (equal (jsonian-path) '("mixed" 0 "1" "2" 1 1 "5" 1 "7")))))

(ert-deftest jsonian-compact-path ()
  "Check that `jsonian-path' works within compact JSON."
  (with-file-and-point "compact1.json" 36
    (should (equal (jsonian-path) '(0 "abc" 1 1 "final")))))

(ert-deftest jsonian-expanded-path ()
  "Check that `jsonian-path' works with more then normal spacing."
  (with-file-and-point "compact1.json" 91
    (should (equal (jsonian-path) '(1 "abc" 1 1 "final")))))

(ert-deftest jsonian-cache-match ()
  "Check that `jsonian-path' and `jsonian-find' have matching cache keys."
  (with-file-and-point "path1.json" 1
    (should (equal (jsonian-find "fizz[4].some") 66))
    (forward-char)
    (let ((path-size (hash-table-count (jsonian--cache-paths jsonian--cache)))
          (location-size (hash-table-count (jsonian--cache-locations jsonian--cache))))
      (should (equal (jsonian-path) '("fizz" 4 "some")))
      (should (equal path-size
                     (hash-table-count (jsonian--cache-paths jsonian--cache))))
      (should (equal location-size
                     (hash-table-count (jsonian--cache-locations jsonian--cache)))))))

(ert-deftest jsonian-parse-numbers ()
  "Check that `jsonian--backward-integer' parses correctly."
  (seq-do
   (lambda (test)
     (let ((s (car test))
           (expected (cadr test))
           (point (caddr test)))
       (with-temp-buffer
         (insert s)
         (let* ((starting (point))
               (got (not (not (jsonian--backward-number)))))
           (unless (equal expected got)
             (ert-fail (format "Test %s failed with output %s" test got)))
           (should (equal (point) point))
           (when got
             (jsonian--forward-number)
             (should (equal starting (point))))))))
   '(("123" t 1)
     ("123e456" t 1)
     ("123.456e789" t 1)
     ("1 2" t 3)
     ("3e" nil 3)
     (" 1.0" t 2)
     (" 01.2" t 3)
     ("00" t 2)
     ("100.00e00" t 1))))

(ert-deftest jsonian-node-preview ()
  "Test `jsonian--node-preview'."
  (jsonian--test-against-text
   "$[ $1.23e4 , $true ,$null, $false
  , ${ $\"key\" : $\"value\" }
]"
   (mapcar
    (lambda (expected)
      (apply-partially
       (lambda (e)
         (should (equal
                  (jsonian--node-preview (point))
                  e)))
       expected))
    (list
     (propertize "[ array ]" 'face 'font-lock-type-face)
     "1.23e4" "true" "null" "false"
     (propertize "{ object }" 'face 'font-lock-type-face)
     "\"value\"" "\"value\""))))

(ert-deftest jsonian-font-lock ()
  "Assert that syntax highlighting works as expected."
  (cl-flet ((face (expected-face text)
              (jsonian--test-against-text
               text
               (list (lambda ()
                       (jsonian-c-mode)
                       (jsonian--force-lock)
                       (should (eq (get-text-property (point) 'face)
                                   expected-face)))))))
    (face 'font-lock-keyword-face "{ \"fo$o\" // bar\n:null }")
    (face 'font-lock-string-face  "[ \"\\\"f$oo\" ]")))

(defun jsonian--format-string (s)
  "Call `jsonian-format-region' S. To be used in testing."
  (with-temp-buffer
    (insert s)
    (jsonian-format-region (point-min) (point-max))
    (buffer-string)))

(defun jsonian--test-format (input expected)
  "Check that calling `jsonian-format-region' on INPUT yields EXPECTED."
  (let ((inhibit-message t))
    ;; Validate that we get the expected result.
    (should (string= (jsonian--format-string input)
                     expected))
    ;; Validate that once formatted, calling format again is a no-op.
    (should (string= (jsonian--format-string expected)
                     expected))
    ;; Validate that `jsonian--format-string' matches the behavior of `json-pretty-print'.
    ;; Because that `json-pretty-print-buffer' defaults to an indentation of 2, we set
    ;; that for ourselves.
    ;;
    ;; Emacs major versions before 28 indent { } as {\n} instead of {}. This makes us
    ;; unable to verify our formatting against `json-pretty-print' since we target
    ;; different results.
    (when (> emacs-major-version 27)
      (let ((jsonian-indentation 2))
        (should (string= (jsonian--format-string input)
                         (with-temp-buffer
                           (insert input)
                           (json-pretty-print-buffer)
                           (buffer-string))))))))

(ert-deftest jsonian-format-region ()
  "Test `jsonian-format-region'."
  (jsonian--test-format
   "[false,null,true,\"abc\",-3.14]"
   "[
    false,
    null,
    true,
    \"abc\",
    -3.14
]")
  (jsonian--test-format "[{\"null\":null}, [ [ {
} ], [     ] ] ]
" "[
    {
        \"null\": null
    },
    [
        [
            {}
        ],
        []
    ]
]
"))

(provide 'jsonian-tests)
;;; jsonian-tests.el ends here
