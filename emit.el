;;; emit.el --- Tools for configuration -*- lexical-binding: t; -*-

;; Copyright (C) 2016 Ivan Malison

;; Author: Ivan Malison <IvanMalison@gmail.com>
;; Keywords: init utility general library
;; URL: https://github.com/IvanMalison/emit
;; Version: 0.0.0
;; Package-Requires: ((dash "2.10.0") (emacs "24"))

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

;; This package provides functions that are generally useful for configuring
;; Emacs.

;;; Code:

;; named-build

(defmacro emit-named-build (name builder &rest args)
  "Alias NAME to the function produced by applying BUILDER to ARGS."
  `(defalias (quote ,name) (,builder ,@args)))
(put 'emit-named-build 'lisp-indent-function 1)

(defmacro emit-build-named-builder (name builder-name)
  "Create a macro NAME that invokes BUILDER-NAME with `emit-named-build'."
  `(defmacro ,name (function-name &rest args)
     (cons 'emit-named-build
           (cons function-name
                 (cons (quote ,builder-name) args)))))

(defmacro emit-named-builder (name)
  "Create a naming builder macro NAME from (NAME + -fn)."
  `(emit-build-named-builder
    ,name ,(intern (concat (symbol-name name) "-fn"))))

(defun emit-help-function-arglist (fn)
  (let ((result (help-function-arglist fn)))
    (if (eq result t) '(&rest args) result)))

;; compose

(defmacro emit-compose-fn (&rest funcs)
  (let* ((last-function (car (last funcs)))
         (arguments (emit-help-function-arglist last-function))
         (call-arguments (delq '&optional arguments)))
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
  `(lambda ,arguments
     ,(format "The composition of %s" funcs)
     (emit-compose-helper ,funcs ,call-arguments)))

(defmacro emit-compose-helper (funcs arguments)
  "Builds funcalls of FUNCS eventually applied to ARGUMENTS."
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

(provide 'emit)
;;; emit.el ends here
