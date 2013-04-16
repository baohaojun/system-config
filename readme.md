With `skeleton-complete`, you need type any long expression only once,
then you can type as little as possible (much less than
`hippie-expand`), and `skeleton-complete` to the long expression.

It has 2 flavors: identifier completion and single-line string
completion.

To taste the 1st flavor, say in an emacs-lisp buffer, you have a long
variable name, `this-is-a-very-long-var-name`, and a long function
name, `this-is-a-very-long-function-name`, you can complete the first
name with typing `tvv` into the buffer where you wanted, and press
`M-g <return>`; or `tvf` and `M-g <return>` to complete the second
name.

For the 2nd flavor, in the buffer I'm editting this readme, I can type
`(')` and press `M-s <return>` to get the following expression:

    (require 'skeleton-complete)

You can consider it as `abbrev-mode` or `yasnippet` on the go (with no
need to prepare the long completion into an abbrev or yasnippet before
using them for completion/expand: it's more free style!).

# Usage

1.  `(require 'skeleton-complete)`

2.  Type a skeleton and press `M-g <return>` or `M-s <return>` based on
    the flavor you wanted.

For more detailed discussion about this dark magic, see [this github
page](http://baohaojun.github.io/skeleton-complete.html).
