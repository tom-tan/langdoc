;;; bf-mode.el --- Brainfuck mode for Emacs

;; Copyright (C) 2013  by Tomoya Tanjo

;; Author: Tomoya Tanjo <ttanjo@gmail.com>
;; Keywords: brainfuck, langdoc

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

;; This library helps you to write brainfuck in Emacs.
;; It is also an example to define eldoc functions by using langdoc.
;;
;; Requirement:
;;    langdoc.el
;;
;; To use this package, add the following line to your .emacs file:
;;     (require 'bf-mode)
;; bf-mode highlights some keywords for usability.
;; By using M-x eldoc-mode, you can see the help string in minibuffer.
;; Also, by using M-x langdoc:describe-symbol (or C-c f), you can see
;; more documents for each command.

;;; Code:

(require 'langdoc)
(require 'generic)

(define-generic-mode bf-mode
  nil
  nil
  '(("[^]><+.,[-]+" . font-lock-comment-face)
    ("\\]\\|\\["    . font-lock-keyword-face))
  '("\\.bf\\'")
  '(define-bf-keymap bfdoc-fun)
  "Major mode for brainfuck")

(defvar bf-local-map nil "Keymap for bf-mode")

(defun define-bf-keymap ()
  (setq brainfuck-local-map (make-keymap))
  (define-key brainfuck-local-map
      "\C-cf" #'langdoc:describe-symbol)
  (use-local-map brainfuck-local-map))

(defun bfdoc-fun ()
  (make-local-variable 'eldoc-documentation-function)
  (setq eldoc-documentation-function
        #'bf-minibuffer-help-string)
  (custom-set-variables
   '(langdoc:pointed-symbol-fn #'bf-sym-called-at-point)
   '(langdoc:symbols '(">" "<" "+" "-" "." "," "[" "]"))
   '(langdoc:helpbuf "*Brainfuck Help*")
   '(langdoc:make-document-fn #'bf-help-string)))

(defun bf-sym-called-at-point ()
  (unless (eobp)
    (buffer-substring-no-properties (point) (1+ (point)))))

(defun bf-minibuffer-help-string ()
  (interactive)
  (let* ((sym (bf-sym-called-at-point))
         (doc (when sym (bf-lookup-doc sym))))
    (when doc (bf-summerize-doc sym doc))))

(defun bf-help-string (sym)
  (let ((doc (bf-lookup-doc sym)))
    doc))

(defun bf-lookup-doc (sym)
  (cond
    ((equal sym ">") "Increment the data pointer (to point to the next cell to the right).")
    ((equal sym "<") "Decrement the data pointer (to point to the next cell to the left).")
    ((equal sym "+") "Increment (increase by one) the byte at the data pointer.")
    ((equal sym "-") "Decrement (decrease by one) the byte at the data pointer.")
    ((equal sym ".") "Output the byte at the data pointer.")
    ((equal sym ",") "Accept one byte of input, storing its value in the byte at the data pointer.")
    ((equal sym "[") "If the byte at the data pointer is zero, then instead of moving the instruction pointer
forward to the next command, jump it forward to the command after the matching ] command.")
    ((equal sym "]") "If the byte at the data pointer is nonzero, then instead of moving the instruction pointer
forward to the next command, jump it back to the command after the matching [ command.")))

(defun bf-summerize-doc (sym doc)
  (concat sym " : " (car (split-string doc "[\n\r]+"))))

(provide 'bf-mode)
;;; bf-mode.el ends here
