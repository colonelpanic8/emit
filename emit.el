;;; emit.el --- Tools for configuration -*- lexical-binding: t; -*-

;; Copyright (C) 2016 Ivan Malison

;; Author: Ivan Malison <IvanMalison@gmail.com>
;; Keywords: init utility general library macro
;; URL: https://github.com/IvanMalison/emit
;; Version: 0.0.0
;; Package-Requires: ((dash "2.10.0") (emacs "24") (cl-lib "0.5"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides functions that are generally useful for
;; configuring Emacs.

;;; Code:

(require 'dash)
(require 'cl-lib)


;; named-builder

(eval-and-compile
  (defvar emit-named-builder-suffix "-fn"))

(defmacro emit-named-build (name builder &rest args)
  "Alias NAME to the function produced by applying BUILDER to ARGS."
  `(defalias (quote ,name) (,builder ,@args)))
(put 'emit-named-build 'lisp-indent-function 1)

(defmacro emit-build-named-builder (name builder-name)
  "Create a macro NAME that invokes BUILDER-NAME with `emit-named-build'."
  `(progn
       (defmacro ,name (function-name &rest args)
         (cons 'emit-named-build
               (cons function-name
                     (cons (quote ,builder-name) args))))
       (put (quote ,builder-name) 'lisp-indent-function 1)))

(defmacro emit-named-builder (name)
  "Create a naming builder NAME from NAME with `emit-named-builder-suffix'."
  `(emit-build-named-builder
    ,name ,(intern (concat (symbol-name name) emit-named-builder-suffix))))

;; compose

;; This implementation of compose may seem quite complicated, but it
;; has the advantage that it works with macros it performs quite well
;; because it results in only one additional stack frame.

(defun emit-help-function-arglist (fn)
  (let ((result (help-function-arglist fn)))
    (if (eq result t) '(&rest args) result)))

(defmacro emit-compose-fn (&rest funcs)
  "Build a new lambda function that is the composition of FUNCS."
  (let* ((last-function (car (last funcs)))
         (arguments (emit-help-function-arglist last-function))
         (call-arguments (remq '&optional arguments)))
    ;; When we have an &rest arguments there is no point in taking any
    ;; of the arguments by name, so we simply pass them all as an
    ;; argument list. See the comment below to understand how this
    ;; impacts the evaluation of the last function.
    (when (memq '&rest arguments)
      (setq arguments '(&rest args))
      (setq call-arguments '(args)))
    `(emit-compose-argspec ,arguments ,call-arguments ,@funcs)))

(defmacro emit-compose-argspec (arguments call-arguments &rest funcs)
  "Build a lambda with CALL-ARGUMENTS passed as ARGUMENTS that composes FUNCS."
  (let ((interactive-form (when (and funcs (listp funcs))
                             (interactive-form (car (last funcs))))))
    `(lambda ,arguments
       ,(format "The composition of %s" funcs)
       ,@(when interactive-form (list interactive-form))
       (emit-compose-helper ,funcs ,call-arguments))))

(defmacro emit-compose-helper (funcs arguments)
  (if (equal (length funcs) 1)
      (let ((last-function (car funcs)))
        ;; This hideous clause is here because it is the only way to
        ;; handle functions that take &rest args.
        (when (memq '&rest (emit-help-function-arglist last-function))
          (setq last-function (apply-partially 'apply last-function)))
        `(,last-function ,@arguments))
    `(,(car funcs)
      (emit-compose-helper ,(cdr funcs) ,arguments))))

(emit-named-builder emit-compose)

;; prefix-selector

(defun emit-interpret-prefix-as-number (prefix)
  (cond
   ((numberp prefix) prefix)
   ((and (-non-nil prefix) (listp prefix))
    (truncate (log (car prefix) 4)))
   (0)))

(defmacro emit-prefix-selector-fn (&rest functions)
  "Build a lambda to dispatch to FUNCTIONS based on the prefix argument."
  (let* ((selector-number 0)
         (conditions
          (cl-loop for fn in functions
                   collect `(,selector-number (quote ,fn))
                   do (cl-incf selector-number))))
    `(lambda (arg)
       ,(format "Call one of %s depending the prefix argument.\nCall `%s' by default."
                (mapconcat (lambda (fn-or-symbol)
                                  (cond
                                   ((symbolp fn-or-symbol)
                                    (format "`%s'" (symbol-name fn-or-symbol)))
                                   ((format "%s" fn-or-symbol))))
                           functions ", ") (car functions))
       (interactive "P")
       (setq arg (emit-interpret-prefix-as-number arg))
       (let ((selection (pcase arg ,@conditions (_ (quote ,(car functions))))))
         (setq current-prefix-arg nil)
         (call-interactively selection)))))

(emit-named-builder emit-prefix-selector)

;; let-around

(defmacro emit-let-around-fn (orig-func &rest forms)
  "Build a lambda that calls ORIG-FUNC with the bindings in FORMS set."
  (let* ((orig-interactive-form (interactive-form orig-func))
         (docstring-form (format "Call `%s' with bindings: %s." orig-func forms))
         (additional-forms (list docstring-form)))
    `(lambda (&rest args)
       ,@additional-forms
       ,@(when orig-interactive-form (list orig-interactive-form))
       (let ,forms
         (apply (quote ,orig-func) args)))))

(emit-named-builder emit-let-around)


(provide 'emit)
;;; emit.el ends here
