Use this mode for editing files in the dot-language (www.graphviz.org and
http://www.research.att.com/sw/tools/graphviz/).

To use graphviz-dot-mode, add
(load-file "PATH_TO_FILE/graphviz-dot-mode.el")
to your ~/.emacs(.el) or ~/.xemacs/init.el

The graphviz-dot-mode will do font locking, indentation, preview of graphs
and eases compilation/error location. There is support for both GNU Emacs
and XEmacs.

Font locking is automatic, indentation uses the same commands as
other modes, tab, M-j and C-M-q.  Insertion of comments uses the
same commands as other modes, M-; .  You can compile a file using
M-x compile or C-c c, after that M-x next-error will also work.
There is support for viewing an generated image with C-c p.
