;;; interface.el --- A script to find the public interface of jsonian.el -*- levical-binding: t; -*-

;;; Commentary:
;;; This is a script intended to be used as part of the build
;;; process. It is not part of the published package.

;;; Code:

(require 'jsonian)
(require 'seq)

(defun interface-public ()
  "Return the list of publically defined functions in jsonian.el."
  (seq-filter
   (lambda (x)
     (not (or (eq 'require (car-safe x))
              (eq 'provide (car-safe x))
              (and (eq 'defun (car-safe x))
                   ;; private defuns
                   (string-prefix-p
                    "jsonian--"
                    (string-trim-left (symbol-name (cdr x)) (regexp-opt '("make-" "copy-"))))))))
   (alist-get (symbol-file 'jsonian) load-history)))

(defun interface-format-fn (fn-symbol)
  "Format a defined symbol (FN-SYMBOL) for consumption in markdown."
  (let* ((split-body (split-string (documentation fn-symbol) "\n"))
         (last-line (car-safe (last split-body)))
         (signature-p (string-prefix-p "(fn " last-line))
         (body (if signature-p
                   (mapconcat #'identity (butlast split-body 1) "\n")
                 (documentation fn-symbol))))
    (string-trim-right
     (format "#### %s%s\n%s"
             (symbol-name fn-symbol)
             (if signature-p
                 (format " (%s" (string-trim-left last-line (regexp-quote "(fn ")))
               "")
             body))))

(defun interface ()
  "Print the interface section of the README.md."
  (let* ((symbols (interface-public))
         (fns (seq-map #'cdr (seq-filter (lambda (x) (eq (car-safe x) 'defun)) symbols))))
    (princ (format "## Package Interface\n\n### Functions\n\n%s"
                   (mapconcat #'interface-format-fn fns "\n\n")))))

(provide 'interface)
;;; interface.el ends here
