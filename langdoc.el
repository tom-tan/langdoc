;;; langdoc.el --- Help to define eldoc functions for various languages

;; Copyright (C) 2013  by Tomoya Tanjo

;; Author: Tomoya Tanjo <ttanjo@gmail.com>
;; Keywords: convenience, eldoc

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

;; This library helps you to define eldoc functions for various languages.
;; Here are the variables to be assigned.
;;
;; * `eldoc-documentation-function'
;;    If non-nil, function to call to return doc string.
;;    The function of no args should return a one-line string for displaying
;;    doc about a function etc. (quoted from docstring of this variable)
;; * `langdoc:pointed-symbol-fn'
;;    Function name which returns the string pointed by
;;    the cursor.  This function recieves no arguments.
;; * `langdoc:symbols'
;;    List of strings which is used to complete words.
;; * `langdoc:helpbuf'
;;    Buffer name to show the help string.
;; * `langdoc:make-document-fn'
;;    Function name which return the help string.
;;    This function recieves the string to show help.
;;
;; Furthermore, here are the variables to make links in help buffers.
;;
;; * `langdoc:link-regexp'
;;   Regexp string to make links.
;;   If nil, langdoc does not make links in help buffers.
;; * `langdoc:linked-str-fn'
;;   Function name which returns the string to be linked.
;;   This function recieves substrings matched by parenthesis
;;   in `langdoc:link-regexp'.
;; * `langdoc:make-link-fn'
;;   Function name which returns the string and function symbol to make link.
;;   This function recieves substrings matched by parenthesis
;;   in `langdoc:link-regexp' and returns a string or a cons pair (SYM . FUN).
;;   SYM is the string to be linked and FUN is the function to jump to SYM help string.
;;   If it returns a string, `langdoc:describe-symbol' is used to jump to SYM.
;; * `langdoc:linked-prefix'
;;   Prefix of the string returned from `langdoc:linked-str-fn'.
;; * `langdoc:linked-postfix'
;;   Postfix of the string returned from `langdoc:linked-str-fn'.
;;
;; If you need a concrete example, see the definition of `bf-mode:doc-fun' in bf-mode.el.

;;; Code:

(require 'button)
(require 'view)

(defvar langdoc:pointed-symbol-fn nil
  "Function name which returns the string pointed by
the cursor.  This function recieves no arguments.")
(defvar langdoc:symbols nil
  "List of strings which is used to complete words.")
(defvar langdoc:helpbuf nil
  "Buffer name to show the help string.")
(defvar langdoc:make-document-fn nil
  "Function name which return the help string.
This function recieves the string to show help.")

(defvar langdoc:link-regexp nil
  "Regexp string to make links.
If nil, langdoc does not make links in help buffers.")
(defvar langdoc:linked-str-fn #'identity
  "Function name which returns the string to be linked.
This function recieves substrings matched by parenthesis
in `langdoc:link-regexp'.")
(defvar langdoc:make-link-fn #'identity
  "Function name which returns the string and function symbol to make link.
This function recieves substrings matched by parenthesis
in `langdoc:link-regexp' and returns a string or a cons pair (SYM . FUN).
SYM is the string to be linked and FUN is the function to jump to SYM help string.
If it returns a string, `langdoc:describe-symbol' is used to jump to SYM.")
(defvar langdoc:linked-prefix ""
  "Prefix of the string returned from `langdoc:linked-str-fn'.")
(defvar langdoc:linked-postfix ""
  "Postfix of the string returned from `langdoc:linked-str-fn'.")

(make-variable-buffer-local 'langdoc:pointed-symbol-fn)
(make-variable-buffer-local 'langdoc:symbols)
(make-variable-buffer-local 'langdoc:helpbuf)
(make-variable-buffer-local 'langdoc:make-document-fn)

(make-variable-buffer-local 'langdoc:link-regexp)
(make-variable-buffer-local 'langdoc:link-str-fn)
(make-variable-buffer-local 'langdoc:make-link-fn)
(make-variable-buffer-local 'langdoc:linked-prefix)
(make-variable-buffer-local 'langdoc:linked-postfix)

(defun langdoc:describe-symbol (sym)
  "Display the full documentation of Sym (a string)."
  (interactive
   (let* ((s (funcall langdoc:pointed-symbol-fn))
          (enable-recursive-minibuffers t)
          (val (completing-read
                (if s
                    (format "Describe symbol (default %s): " s)
                    "Describe symbol: ")
                langdoc:symbols 'stringp nil nil nil s)))
     (list (if (equal val "") s val))))
  (if (null sym)
      (message "You didn't specify a symbol")
      (let ((buf (get-buffer-create langdoc:helpbuf)))
        (with-current-buffer buf
          (setq buffer-read-only nil)
          (let ((doc-str (funcall langdoc:make-document-fn sym)))
            (erase-buffer)
            (insert doc-str)
            (when langdoc:link-regexp
              (langdoc:make-links buf)))
          (goto-char (point-min))
          (set-buffer-modified-p nil)
          (setq buffer-read-only t)
          (view-mode t)
          (display-buffer buf)))))

(defun langdoc:call-fun (b)
  (funcall (button-get b 'fun) (button-get b 'link)))

(defun langdoc:insert-link (str to fun)
  (insert-text-button str
                      'follow-link t
                      'help-echo (concat "mouse-1, RET: describe this symbol")
                      'fun fun
                      'action #'langdoc:call-fun
                      'link to))

(defun langdoc:make-links (buf)
  (with-current-buffer buf
    (goto-char (point-min))
    (while (re-search-forward langdoc:link-regexp nil t)
      (let ((beg (match-beginning 0))
            (args (langdoc:matched-strings)))
        (replace-match "" nil nil)
        (goto-char beg)
        (let ((str (apply langdoc:linked-str-fn args))
              (link (apply langdoc:make-link-fn args)))
          (insert langdoc:linked-prefix)
          (langdoc:insert-link str
                               (if (consp link) (car link) link)
                               (if (consp link)
                                   (cdr link) #'langdoc:describe-symbol))
          (insert langdoc:linked-postfix))))))

(defun langdoc:matched-strings ()
  "Return a list of strings parenthesized expression in the last regexp search."
  (let ((i 1) ret)
    (langdoc:while-let (str (match-string-no-properties i))
               (incf i)
               (add-to-list 'ret str t (lambda (a b) nil)))
    ret))

(defmacro langdoc:if-let (lst then &rest else)
  (let ((value (car lst))
                (cnd   (cadr lst)))
    `(let ((,value ,cnd))
       (if ,value
           ,then
           ,@else))))

(defmacro langdoc:while-let (lst &rest body)
  `(while (if-let ,lst
                  (progn ,@body t))))

(provide 'langdoc)
;;; langdoc.el ends here
