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

(make-variable-buffer-local 'langdoc:pointed-symbol-fn)
(make-variable-buffer-local 'langdoc:symbols)
(make-variable-buffer-local 'langdoc:helpbuf)
(make-variable-buffer-local 'langdoc:make-document-fn)

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
            (insert doc-str))
          (goto-char (point-min))
          (set-buffer-modified-p nil)
          (setq buffer-read-only t)
          (view-mode t)
          (display-buffer buf)))))

(defun langdoc:replace-to-link (beg end to &optional msg)
  (make-text-button beg end
                    'follow-link t
                    'help-echo (concat "mouse-1, RET: "
                                       (or msg "describe this symbol"))
                    'action #'langdoc:goto-link
                    'link to))

(defun langdoc:insert-link (str to &optional msg)
  (insert-text-button str
                      'follow-link t
                      'help-echo (concat "mouse-1, RET: "
                                         (or msg "describe this symbol"))
                      'action #'langdoc:goto-link
                      'link to))

(defun langdoc:goto-link (b)
  (langdoc:describe-symbol (button-get b 'link)))

(provide 'langdoc)
;;; langdoc.el ends here
