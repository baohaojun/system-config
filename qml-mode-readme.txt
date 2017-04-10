qml-mode is major-mode for editing Qt Declarative (QML) code.


Installation:

If you have `melpa` and `emacs24` installed, simply type:

     M-x package-install qml-mode

Add following lines to your init file:

     (autoload 'qml-mode "qml-mode" "Editing Qt Declarative." t)
     (add-to-list 'auto-mode-alist '("\\.qml$" . qml-mode))

ChangeLog

0.4

  * Add QML component id highlighter
  * Add basic types provided by QML modules as property keyword

0.3

  * rewrite based on js-mode.

0.2

  * rewrite based on generic-mode.

0.1

  * first version fork from cataska/qml-mode.
