# langdoc.el

This library helps you to define eldoc functions for various languages.
Here are the variables to be assigned.

* eldoc-documentation-function

   If non-nil, function to call to return doc string.
   The function of no args should return a one-line string for displaying
   doc about a function etc. (quoted from docstring of this variable)
* langdoc:pointed-symbol-fn

   Function name which returns the string pointed by
   the cursor.  This function recieves no arguments.
* langdoc:symbols

   List of strings which is used to complete words.
* langdoc:helpbuf

   Buffer name to show the help string.
* langdoc:make-document-fn

   Function name which return the help string.
   This function recieves the string to show help.

Furthermore, here are the variables to make links in help buffers.

* `langdoc:link-regexp'
  Regexp string to make links.
  If nil, langdoc does not make links in help buffers.
* `langdoc:linked-str-fn'
  Function name which returns the string to be linked.
  This function recieves substrings matched by parenthesis
  in `langdoc:link-regexp'.
* `langdoc:make-link-fn'
  Function name which returns the string and function symbol to make link.
  This function recieves substrings matched by parenthesis
  in `langdoc:link-regexp' and returns a string or a cons pair (SYM . FUN).
  SYM is the string to be linked and FUN is the function to jump to SYM help string.
  If it returns a string, `langdoc:describe-symbol' is used to jump to SYM.
* `langdoc:linked-prefix'
  Prefix of the string returned from `langdoc:linked-str-fn'.
* `langdoc:linked-postfix'
  Postfix of the string returned from `langdoc:linked-str-fn'.

If you need a concrete example, see the definition of [bf-mode:doc-fun in bf-mode.el](https://github.com/tom-tan/langdoc/blob/master/bf-mode.el#L58).
