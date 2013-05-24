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

If you need a concrete example, see the definition of [bf-mode](https://github.com/tom-tan/langdoc/blob/master/bf-mode.el) in bf-mode.el.
