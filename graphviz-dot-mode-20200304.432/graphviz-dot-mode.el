;;; graphviz-dot-mode.el --- Mode for the dot-language used by graphviz (att).

;; Copyright (C) 2002 - 2020 Pieter Pareit <pieter.pareit@gmail.com>

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA

;; Authors: Pieter Pareit <pieter.pareit@gmail.com>
;;          Rubens Ramos <rubensr AT users.sourceforge.net>
;;          Eric Anderson http://www.ece.cmu.edu/~andersoe/
;; Maintainer: Pieter Pareit <pieter.pareit@gmail.com>
;; Homepage: https://ppareit.github.io/graphviz-dot-mode/
;; Created: 28 Oct 2002
;; Last modified: 25 Januari 2020
;; Version: 0.4.2
;; Package-Requires: ((emacs "25.0"))
;; Keywords: mode dot dot-language dotlanguage graphviz graphs att

;;; Commentary:
;; Use this mode for editing files in the dot-language, see
;; https://www.graphviz.org.
;;
;; To use graphviz-dot-mode, add
;; (use-package graphviz-dot-mode
;;   :ensure t)
;; to your ~/.emacs.el file.
;;
;; The graphviz-dot-mode will do font locking, indentation, preview of
;; graphs and eases compilation/error location.  Font locking is
;; automatic, indentation uses the same commands as other modes, tab,
;; M-j and C-M-q.  Insertion of comments uses the same commands as
;; other modes, M-; .  You can compile a file using M-x compile or C-c
;; c, after that M-x next-error will also work.  There is support for
;; viewing an generated image with C-c p.
;;
;;; Todo:
;;
;;; History:
;; Version 0.4.2 Pieter Pareit
;; 25/01/2020: * Fix issues
;;             * Improve font-locking
;;             * Improve completion by implementing company mode
;;             * Rewrote basic documentation
;; Version 0.4.1 Pieter Pareit
;; 28/09/2019: * Maintenance, checking documentation, fixing flycheck errors.
;;             * Solve next-error for gaphviz
;;             * Tag new version
;; Version 0.3.11 Olli Piepponen
;; 29/01/2016: * use define-derived-mode for the mode-definition
;;             * add support for a auto-loading live preview work flow
;; Version 0.3.10 Kevin Ryde
;; 25/05/2015: * shell-quote-argument for safety
;;             * use read-shell-command whenever available, don't set novaproc
;; Version 0.3.9 Titus Barik <titus AT barik.net>
;; 28/08/2012: * compile-command uses -ofile instead of >
;; Version 0.3.8 new home
;; 27/06/2012: * put graphviz-dot-mode into git, updated links
;; Version 0.3.7 Tim Allen
;; 09/03/2011: * fix spaces in file names when compiling
;; Version 0.3.6 maintenance
;; 19/02/2011: * .gv is the new extension  (Pander)
;;             * comments can start with # (Pander)
;;             * highlight of new keywords (Pander)
;; Version 0.3.5 bug (or at least feature I dislike) fix
;; 11/11/2010:  Eric Anderson http://www.ece.cmu.edu/~andersoe/
;;             * Preserve indentation across blank (whitespace-only) lines
;; Version 0.3.4 bug fixes
;; 24/02/2005: * fixed a bug in graphviz-dot-preview
;; Version 0.3.3 bug fixes
;; 13/02/2005: Reuben Thomas <rrt AT sc3d.org>
;;             * add graphviz-dot-indent-width
;; Version 0.3.2 bug fixes
;; 25/03/2004: Rubens Ramos <rubensr AT users.sourceforge.net>
;;             * semi-colons and brackets are added when electric
;;               behaviour is disabled.
;;             * electric characters do not behave electrically inside
;;               comments or strings.
;;             * default for electric-braces is disabled now (makes more
;;               sense I guess).
;;             * using read-from-minibuffer instead of read-shell-command
;;               for emacs.
;;             * Fixed test for easymenu, so that it works on older
;;               versions of XEmacs.
;;             * Fixed indentation error when trying to indent last brace
;;               of an empty graph.
;;             * region-active-p does not exist in emacs (21.2 at least),
;;               so removed from code
;;             * Added uncomment menu option
;; Version 0.3.1 bug fixes
;; 03/03/2004: * backward-word needs argument for older emacs
;; Version 0.3 added features and fixed bugs
;; 10/01/2004: fixed a bug in graphviz-dot-indent-graph
;; 08/01/2004: Rubens Ramos <rubensr AT users.sourceforge.net>
;;             * added customization support
;;             * Now it works on XEmacs and Emacs
;;             * Added support to use an external Viewer
;;             * Now things do not break when dot mode is entered
;;               when there is no buffer name, but the side effect is
;;               that in this case, the compilation command is not
;;               correct.
;;             * Preview works on XEmacs and emacs.
;;             * Electric indentation on newline
;;             * Minor changes to indentation
;;             * Added keyword completion (but could be A LOT better)
;;             * There are still a couple of ugly hacks. Look for 'RR'.
;; Version 0.2 added features
;; 11/11/2002: added preview support.
;; 10/11/2002: indent a graph or subgraph at once with C-M-q.
;; 08/11/2002: relaxed rules for indentation, the may now be extra chars
;;             after beginning of graph (comment's for example).
;; Version 0.1.2 bug fixes and naming issues
;; 06/11/2002: renamed dot-font-lock-defaults to dot-font-lock-keywords.
;;             added some documentation to dot-colors.
;;             provided a much better way to handle my max-specpdl-size
;;             problem.
;;             added an extra autoload cookie (hope this helps, as I don't
;;             yet use autoload myself)
;; Version 0.1.1 bug fixes
;; 06/11/2002: added an missing attribute, for font-locking to work.
;;             fixed the regex generating, so that it only recognizes
;;             whole words
;; 05/11/2002: there can now be extra whitespace chars after an '{'.
;; 04/11/2002: Why I use max-specpdl-size is now documented, and old value
;;             gets restored.
;; Version 0.1 initial release
;; 02/11/2002: implemented parser for *compilation* of a .dot file.
;; 01/11/2002: implemented compilation of an .dot file.
;; 31/10/2002: added syntax-table to the mode.
;; 30/10/2002: implemented indentation code.
;; 29/10/2002: implemented all of font-lock.
;; 28/10/2002: derived graphviz-dot-mode from fundamental-mode, started
;;             implementing font-lock.

;;; Code:

(require 'compile)
(require 'subr-x)

(defconst graphviz-dot-mode-version "0.4.2"
  "Version of `graphviz-dot-mode.el'.")

(defgroup graphviz nil
  "Major mode for editing Graphviz Dot files"
  :group 'tools)

(defun graphviz-dot-customize ()
  "Run \\[customize-group] for the `graphviz' group."
  (interactive)
  (customize-group 'graphviz))

(defvar graphviz-dot-mode-abbrev-table nil
  "Abbrev table in use in Graphviz Dot mode buffers.")
(define-abbrev-table 'graphviz-dot-mode-abbrev-table ())

(defcustom graphviz-dot-dot-program "dot"
  "*Location of the dot program.  This is used by `compile'."
  :type 'string
  :group 'graphviz)

(defcustom graphviz-dot-layout-programs
  '("dot" "neato" "fdp" "sfdp" "twopi" "twopi" "circo")
  "*List of layout programs for the user to choose from."
  :type 'list
  :group 'graphviz)

(defcustom graphviz-dot-view-command "dotty %s"
  "*External program to run on the buffer.
You can use `%s' in this string, and it will be substituted by the buffer name."
  :type 'string
  :group 'graphviz)

(defcustom graphviz-dot-view-edit-command nil
  "*Whether to allow the user to edit the command to run an external viewer."
  :type 'boolean
  :group 'graphviz)

(defcustom graphviz-dot-save-before-view t
  "*If not nil, \\[graphviz-dot-view] saves the current buffer before running the command."
  :type 'boolean
  :group 'graphviz)

(defcustom graphviz-dot-indent-width tab-width
  "*Indentation width in Graphviz Dot mode buffers."
  :type 'integer
  :group 'graphviz)

(defcustom graphviz-dot-preview-extension "png"
  "*The extension to use for the compilation and preview commands.
The default format for the compilation command is `dot -T png
file.dot -o file.png'."
  :type 'string
  :group 'graphviz)

(defcustom graphviz-dot-auto-preview-on-save nil
  "*Determines if saving the buffer should automatically trigger preview."
  :type 'boolean
  :group 'graphviz)

(defcustom graphviz-dot-revert-delay 300
  "*Amount of time to sleep before attempting to display the rendered image."
  :type 'number
  :group 'graphviz)

(defcustom graphviz-dot-attr-keywords
  '("graph" "digraph" "subgraph" "node" "edge" "strict" "rankdir"
    "size" "page" "Damping" "Epsilon" "URL" "arrowhead" "arrowsize"
    "arrowtail" "bb" "bgcolor" "bottomlabel" "center" "clusterrank"
    "color" "colorscheme" "comment" "compound"
    "concentrate" "constraint" "decorate"
    "dim" "dir" "distortion" "fillcolor" "fixedsize" "fontcolor"
    "fontname" "fontpath" "fontsize" "group" "headURL" "headlabel"
    "headport" "height" "label" "labelangle" "labeldistance" "labelfloat"
    "labelfontcolor" "labelfontname" "labelfontsize" "labeljust"
    "labelloc" "layer" "layers" "len" "lhead" "lp" "ltail" "margin"
    "maxiter" "mclimit" "minlen" "model" "nodesep" "normalize" "nslimit"
    "nslimit1" "ordering" "orientation" "overlap" "pack" "pagedir"
    "pencolor" "peripheries" "pin" "pos" "quantum" "rank" "ranksep"
    "ratio" "rects" "regular" "remincross" "rotate" "samehead" "sametail"
    "samplepoint" "searchsize" "sep" "shape" "shapefile" "showboxes"
    "sides" "skew" "splines" "start" "style" "stylesheet" "tailURL"
    "taillabel" "tailport" "toplabel" "vertices" "voro_margin" "weight"
    "z" "width" "penwidth" "mindist" "scale" "patch" "root")
  "*Keywords for attribute names in a graph.
This is used by the auto completion code.  The actual completion
tables are built when the mode is loaded, so changes to this are
not immediately visible."
  :type '(repeat (string :tag "Keyword"))
  :group 'graphviz)

(defvar graphviz-attributes-type-arrow
  '("arrowhead" "arrowtail")
  "The attributes that are of type `arrow'.
See https://graphviz.gitlab.io/_pages/doc/info/attrs.html for
more information about possible attributes.")

(defvar graphviz-values-type-arrow
  '("box" "lbox" "rbox" "obox" "olbox" "orbox"
    "crow" "lcrow" "rcrow"
    "diamond" "ldiamond" "rdiamond" "odiamond" "oldiamond" "ordiamond"
    "dot" "odot"
    "inv" "linv" "rinv" "oinv" "olinv" "orinv"
    "none"
    "normal" "lnormal" "rnormal" "onormal" "olnormal" "ornormal"
    "tee" "ltee" "rtee"
    "vee" "lvee" "rvee"
    "curve" "lcurve" "rcurve" "ocurve" "olcurve" "orcurve")
  "The possible values that an attribute of type `arrow' can have.
See https://graphviz.gitlab.io/_pages/doc/info/arrows.html for
more information about the arrow shape.")

(defvar graphviz-attributes-type-shape
  '("shape")
  "The attributes that are of type `shape'.
See https://graphviz.gitlab.io/_pages/doc/info/attrs.html for
more information about possible attributes.")

(defvar graphviz-values-type-shape
  '("box" "polygon" "ellipse" "oval" "circle" "point" "egg" "triangle" "plaintext"
    "plain" "diamond" "trapezium" "parallelogram" "house" "pentagon" "hexagon"
    "septagon" "octagon" "doublecircle" "doubleoctagon" "tripleoctagon"
    "invtriangle" "invtrapezium" "invhouse" "Mdiamond" "Msquare" "Mcircle" "rect"
    "rectangle" "square" "star" "none" "underline" "cylinder" "note" "tab" "folder"
    "box3d" "component" "promoter" "cds" "terminator" "utr" "primersite"
    "restrictionsite" "fivepoverhang" "threepoverhang" "noverhang" "assembly"
    "signature" "insulator" "ribosite" "rnastab" "proteasesite" "proteinstab"
    "rpromoter" "rarrow" "larrow" "lpromoter")
  "The possible values that an attribute of type `shape' can have.
See https://graphviz.gitlab.io/_pages/doc/info/shape.html for
more information about the node shapes.")

(defvar graphviz-attributes-type-style
  '("style")
  "The attributes that are of type `style'.
See https://graphviz.gitlab.io/_pages/doc/info/attrs.html for
more information about possible attributes.")

(defvar graphviz-values-type-style
  '("dashed" "dotted" "solid" "invis" "bold" "tapered" "filled" "striped"
    "wedged" "diagonals" "rounded" "filled" "striped" "rounded" "radial")
  "The possible values that an attribute of type `style' can have.
See https://graphviz.gitlab.io/_pages/doc/info/attrs.html#k:style for
more information about possible styles.")

(defvar graphviz-attributes-type-dir
  '("dir")
  "The attributes that are of type `bool'.
See https://graphviz.gitlab.io/_pages/doc/info/attrs.html for
more information about possible attributes.")

(defvar graphviz-values-type-dir
  '("forward" "back" "both" "none")
  "The possible values that an attribute of type `dir' can have.
See https://graphviz.gitlab.io/_pages/doc/info/attrs.html#k:dirType for
more information about the direction that edges can have.")

(defvar graphviz-attributes-type-outputmode
  '("outputorder")
  "The attributes that are of type `outputMode'.
See https://graphviz.gitlab.io/_pages/doc/info/attrs.html for
more information about possible attributes.")

(defvar graphviz-values-type-outputmode
  '("breadthfirst" "nodesfirst" "edgesfirst")
  "The possible values that an attribute of type `outputMode' can have.
See https://graphviz.gitlab.io/_pages/doc/info/attrs.html#k:outputMode for
more information.")

(defvar graphviz-attributes-type-packmode
  '("packmode")
  "The attributes that are of type `packMode'.
See https://graphviz.gitlab.io/_pages/doc/info/attrs.html for
more information about possible attributes.")

(defvar graphviz-values-type-packmode
  '("node" "clust" "array")
  "The possible values that an attribute of type `packMode' can have.
See https://graphviz.gitlab.io/_pages/doc/info/attrs.html#k:packMode for
more information.")

(defvar graphviz-attributes-type-pagedir
  '("pagedir")
  "The attributes that are of type `pagedir'.
See https://graphviz.gitlab.io/_pages/doc/info/attrs.html for
more information about possible attributes.")

(defvar graphviz-values-type-pagedir
  '("BL" "BR" "TL" "TR" "RB" "RT" "LB" "LT")
  "The possible values that an attribute of type `pagedir' can have.
See https://graphviz.gitlab.io/_pages/doc/info/attrs.html#k:pagedir for
more information.")

(defvar graphviz-attributes-type-bool
  '("center" "compound" "concentrate" "constraint" "decorate"
    "diredgeconstraints" "fixedsize" "forcelabels" "headclip" "imagescale"
    "labelfloat" "landscape" "mosek" "newrank" "nojustify" "normalize"
    "notranslate" "overlap" "overlap_shrink" "pack" "pin" "quadtree" "regular"
    "remincross" "root" "splines" "tailclip" "truecolor")
  "The attributes that are of type `bool'.
See https://graphviz.gitlab.io/_pages/doc/info/attrs.html for
more information about possible attributes.")

(defvar graphviz-values-type-bool
  '("true" "false" "yes" "no" "1" "0")
  "The possible values that an attribute of type `bool' can have.")

(defvar graphviz-attributes-type-portpos
  '("headport" "tailport")
  "The attributes that are of type `portPos'.
See https://graphviz.gitlab.io/_pages/doc/info/attrs.html for
more information about possible attributes.")

(defvar graphviz-values-type-portpos
  '("n" "ne" "e" "se" "s" "sw" "w" "nw" "c" "_")
  "The possible values that an attribute of type `portPos' can have.
The can also be used on the edge as a compass point.  See
https://graphviz.gitlab.io/_pages/doc/info/attrs.html#k:portPos
for more information.")

(defcustom graphviz-dot-value-keywords
  '("true" "false" "normal" "inv" "dot" "invdot" "odot" "invodot"
    "none" "tee" "empty" "invempty" "diamond" "odiamond" "box" "obox"
    "open" "crow" "halfopen" "local" "global" "none" "forward" "back"
    "both" "none" "BL" "BR" "TL" "TR" "RB" "RT" "LB" "LT" ":n" ":ne" ":e"
    ":se" ":s" ":sw" ":w" ":nw" "same" "min" "source" "max" "sink" "LR"
    "box" "polygon" "ellipse" "circle" "point" "egg" "triangle"
    "plaintext" "diamond" "trapezium" "parallelogram" "house" "hexagon"
    "octagon" "doublecircle" "doubleoctagon" "tripleoctagon" "invtriangle"
    "invtrapezium" "invhouse" "Mdiamond" "Msquare" "Mcircle" "record"
    "Mrecord" "dashed" "dotted" "solid" "invis" "bold" "filled"
    "diagonals" "rounded" )
  "*Keywords for attribute values.
This is used by the auto completion code.  The actual completion
tables are built when the mode is loaded, so changes to this are
not immediately visible."
  :type '(repeat (string :tag "Keyword"))
  :group 'graphviz)

;;; Font-locking:
(defvar graphviz-dot-color-keywords
  '("aliceblue" "antiquewhite" "antiquewhite1" "antiquewhite2"
    "antiquewhite3" "antiquewhite4" "aquamarine" "aquamarine1"
    "aquamarine2" "aquamarine3" "aquamarine4" "azure" "azure1"
    "azure2" "azure3" "azure4" "beige" "bisque" "bisque1" "bisque2"
    "bisque3" "bisque4" "black" "blanchedalmond" "blue" "blue1"
    "blue2" "blue3" "blue4" "blueviolet" "brown" "brown1" "brown2"
    "brown3" "brown4" "burlywood" "burlywood1" "burlywood2"
    "burlywood3" "burlywood4" "cadetblue" "cadetblue1"
    "cadetblue2" "cadetblue3" "cadetblue4" "chartreuse"
    "chartreuse1" "chartreuse2" "chartreuse3" "chartreuse4"
    "chocolate" "chocolate1" "chocolate2" "chocolate3" "chocolate4"
    "coral" "coral1" "coral2" "coral3" "coral4" "cornflowerblue"
    "cornsilk" "cornsilk1" "cornsilk2" "cornsilk3" "cornsilk4"
    "crimson" "cyan" "cyan1" "cyan2" "cyan3" "cyan4" "darkgoldenrod"
    "darkgoldenrod1" "darkgoldenrod2" "darkgoldenrod3"
    "darkgoldenrod4" "darkgreen" "darkkhaki" "darkolivegreen"
    "darkolivegreen1" "darkolivegreen2" "darkolivegreen3"
    "darkolivegreen4" "darkorange" "darkorange1" "darkorange2"
    "darkorange3" "darkorange4" "darkorchid" "darkorchid1"
    "darkorchid2" "darkorchid3" "darkorchid4" "darksalmon"
    "darkseagreen" "darkseagreen1" "darkseagreen2"
    "darkseagreen3" "darkseagreen4" "darkslateblue"
    "darkslategray" "darkslategray1" "darkslategray2"
    "darkslategray3"  "darkslategray4" "darkslategrey"
    "darkturquoise" "darkviolet" "deeppink" "deeppink1"
    "deeppink2" "deeppink3" "deeppink4" "deepskyblue"
    "deepskyblue1" "deepskyblue2" "deepskyblue3" "deepskyblue4"
    "dimgray" "dimgrey"  "dodgerblue" "dodgerblue1" "dodgerblue2"
    "dodgerblue3"  "dodgerblue4" "firebrick" "firebrick1"
    "firebrick2" "firebrick3" "firebrick4" "floralwhite"
    "forestgreen" "gainsboro" "ghostwhite" "gold" "gold1" "gold2"
    "gold3" "gold4" "goldenrod" "goldenrod1" "goldenrod2"
    "goldenrod3" "goldenrod4" "gray" "gray0" "gray1" "gray10" "gray100"
    "gray11" "gray12" "gray13" "gray14" "gray15" "gray16" "gray17"
    "gray18" "gray19" "gray2" "gray20" "gray21" "gray22" "gray23"
    "gray24" "gray25" "gray26" "gray27" "gray28" "gray29" "gray3"
    "gray30" "gray31" "gray32" "gray33" "gray34" "gray35" "gray36"
    "gray37" "gray38" "gray39" "gray4" "gray40" "gray41" "gray42"
    "gray43" "gray44" "gray45" "gray46" "gray47" "gray48" "gray49"
    "gray5" "gray50" "gray51" "gray52" "gray53" "gray54" "gray55"
    "gray56" "gray57" "gray58" "gray59" "gray6" "gray60" "gray61"
    "gray62" "gray63" "gray64" "gray65" "gray66" "gray67" "gray68"
    "gray69" "gray7" "gray70" "gray71" "gray72" "gray73" "gray74"
    "gray75" "gray76" "gray77" "gray78" "gray79" "gray8" "gray80"
    "gray81" "gray82" "gray83" "gray84" "gray85" "gray86" "gray87"
    "gray88" "gray89" "gray9" "gray90" "gray91" "gray92" "gray93"
    "gray94" "gray95" "gray96" "gray97" "gray98" "gray99" "green"
    "green1" "green2" "green3" "green4" "greenyellow" "grey" "grey0"
    "grey1" "grey10" "grey100" "grey11" "grey12" "grey13" "grey14"
    "grey15" "grey16" "grey17" "grey18" "grey19" "grey2" "grey20"
    "grey21" "grey22" "grey23" "grey24" "grey25" "grey26" "grey27"
    "grey28" "grey29" "grey3" "grey30" "grey31" "grey32" "grey33"
    "grey34" "grey35" "grey36" "grey37" "grey38" "grey39" "grey4"
    "grey40" "grey41" "grey42" "grey43" "grey44" "grey45" "grey46"
    "grey47" "grey48" "grey49" "grey5" "grey50" "grey51" "grey52"
    "grey53" "grey54" "grey55" "grey56" "grey57" "grey58" "grey59"
    "grey6" "grey60" "grey61" "grey62" "grey63" "grey64" "grey65"
    "grey66" "grey67" "grey68" "grey69" "grey7" "grey70" "grey71"
    "grey72" "grey73" "grey74" "grey75" "grey76" "grey77" "grey78"
    "grey79" "grey8" "grey80" "grey81" "grey82" "grey83" "grey84"
    "grey85" "grey86" "grey87" "grey88" "grey89" "grey9" "grey90"
    "grey91" "grey92" "grey93" "grey94" "grey95" "grey96" "grey97"
    "grey98" "grey99" "honeydew" "honeydew1" "honeydew2" "honeydew3"
    "honeydew4" "hotpink" "hotpink1" "hotpink2" "hotpink3" "hotpink4"
    "indianred" "indianred1" "indianred2" "indianred3" "indianred4"
    "indigo" "ivory" "ivory1" "ivory2" "ivory3" "ivory4" "khaki" "khaki1"
    "khaki2" "khaki3" "khaki4" "lavender" "lavenderblush"
    "lavenderblush1" "lavenderblush2" "lavenderblush3"
    "lavenderblush4" "lawngreen" "lemonchiffon" "lemonchiffon1"
    "lemonchiffon2" "lemonchiffon3" "lemonchiffon4" "lightblue"
    "lightblue1" "lightblue2" "lightblue3" "lightblue4"
    "lightcoral" "lightcyan" "lightcyan1" "lightcyan2" "lightcyan3"
    "lightcyan4" "lightgoldenrod" "lightgoldenrod1"
    "lightgoldenrod2" "lightgoldenrod3" "lightgoldenrod4"
    "lightgoldenrodyellow" "lightgray" "lightgrey" "lightpink"
    "lightpink1" "lightpink2" "lightpink3" "lightpink4"
    "lightsalmon" "lightsalmon1" "lightsalmon2" "lightsalmon3"
    "lightsalmon4" "lightseagreen" "lightskyblue" "lightskyblue1"
    "lightskyblue2" "lightskyblue3" "lightskyblue4"
    "lightslateblue" "lightslategray" "lightslategrey"
    "lightsteelblue" "lightsteelblue1" "lightsteelblue2"
    "lightsteelblue3" "lightsteelblue4" "lightyellow"
    "lightyellow1" "lightyellow2" "lightyellow3" "lightyellow4"
    "limegreen" "linen" "magenta" "magenta1" "magenta2" "magenta3"
    "magenta4" "maroon" "maroon1" "maroon2" "maroon3" "maroon4"
    "mediumaquamarine" "mediumblue"  "mediumorchid"
    "mediumorchid1" "mediumorchid2" "mediumorchid3"
    "mediumorchid4" "mediumpurple" "mediumpurple1"
    "mediumpurple2" "mediumpurple3" "mediumpurple4"
    "mediumseagreen" "mediumslateblue" "mediumspringgreen"
    "mediumturquoise" "mediumvioletred" "midnightblue"
    "mintcream" "mistyrose" "mistyrose1" "mistyrose2" "mistyrose3"
    "mistyrose4" "moccasin" "navajowhite" "navajowhite1"
    "navajowhite2" "navajowhite3" "navajowhite4" "navy" "navyblue"
    "oldlace" "olivedrab" "olivedrap" "olivedrab1" "olivedrab2"
    "olivedrap3" "oragne" "palegoldenrod" "palegreen" "palegreen1"
    "palegreen2" "palegreen3" "palegreen4" "paleturquoise"
    "paleturquoise1" "paleturquoise2" "paleturquoise3"
    "paleturquoise4" "palevioletred" "palevioletred1"
    "palevioletred2" "palevioletred3" "palevioletred4"
    "papayawhip" "peachpuff" "peachpuff1" "peachpuff2"
    "peachpuff3" "peachpuff4" "peru" "pink" "pink1" "pink2" "pink3"
    "pink4" "plum" "plum1" "plum2" "plum3" "plum4" "powderblue"
    "purple" "purple1" "purple2" "purple3" "purple4" "red" "red1" "red2"
    "red3" "red4" "rosybrown" "rosybrown1" "rosybrown2" "rosybrown3"
    "rosybrown4" "royalblue" "royalblue1" "royalblue2" "royalblue3"
    "royalblue4" "saddlebrown" "salmon" "salmon1" "salmon2" "salmon3"
    "salmon4" "sandybrown" "seagreen" "seagreen1" "seagreen2"
    "seagreen3" "seagreen4" "seashell" "seashell1" "seashell2"
    "seashell3" "seashell4" "sienna" "sienna1" "sienna2" "sienna3"
    "sienna4" "skyblue" "skyblue1" "skyblue2" "skyblue3" "skyblue4"
    "slateblue" "slateblue1" "slateblue2" "slateblue3" "slateblue4"
    "slategray" "slategray1" "slategray2" "slategray3" "slategray4"
    "slategrey" "snow" "snow1" "snow2" "snow3" "snow4" "springgreen"
    "springgreen1" "springgreen2" "springgreen3" "springgreen4"
    "steelblue" "steelblue1" "steelblue2" "steelblue3" "steelblue4"
    "tan" "tan1" "tan2" "tan3" "tan4" "thistle" "thistle1" "thistle2"
    "thistle3" "thistle4" "tomato" "tomato1" "tomato2" "tomato3"
    "tomato4" "transparent" "turquoise" "turquoise1" "turquoise2"
    "turquoise3" "turquoise4" "violet" "violetred" "violetred1"
    "violetred2" "violetred3" "violetred4" "wheat" "wheat1" "wheat2"
    "wheat3" "wheat4" "white" "whitesmoke" "yellow" "yellow1" "yellow2"
    "yellow3" "yellow4" "yellowgreen")
  "Possible color constants in the dot language.
The list of constant is available at http://www.research.att.com/~erg/graphviz\
/info/colors.html")


;;; Key map
(defvar graphviz-dot-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-\M-q"  'graphviz-dot-indent-graph)
    (define-key map "\C-c\C-p" 'graphviz-dot-preview)
    (define-key map "\C-c\C-c" 'compile)
    (define-key map "\C-c\C-v" 'graphviz-dot-view)
    map)
  "Keymap used in Graphviz Dot mode.")

;;; Syntax table
(defvar graphviz-dot-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?/  ". 124b" st)
    (modify-syntax-entry ?*  ". 23"   st)
    (modify-syntax-entry ?\n "> b"    st)
    (modify-syntax-entry ?=  "."      st)
    (modify-syntax-entry ?_  "_"      st)
    (modify-syntax-entry ?-  "_"      st)
    (modify-syntax-entry ?>  "."      st)
    (modify-syntax-entry ?\[ "(]"     st)
    (modify-syntax-entry ?\] ")["     st)
    (modify-syntax-entry ?\" "\""     st)
    st)
  "Syntax table for `graphviz-dot-mode'.")

(defvar graphviz-dot-syntax-propertize-function
  (syntax-propertize-rules
   ("^#" (0 "< b"))))

(defvar graphviz-dot-font-lock-keywords
  ;; See https://graphviz.gitlab.io/_pages/doc/info/lang.html.
  `(;; Match ID, first case
    ("\\(?:di\\|sub\\)?graph\\(?:[[:space:]]+\\)\\([a-zA-Z_]+[a-zA-Z0-9_]*\\)"
     (1 font-lock-function-name-face))
    ;; Match ID, second case
    ("\\(?:di\\|sub\\)?graph\\(?:[[:space:]]+\\)\\(-?[0-9]*\\(\\.[0-9]*\\)?\\)"
     (1 font-lock-function-name-face))
    (,(regexp-opt graphviz-dot-value-keywords 'words)
     . font-lock-reference-face)
    ;; to build the font-locking for the colors,
    ;; we need more room for max-specpdl-size,
    ;; after that we take the list of symbols,
    ;; convert them to a list of strings, and make
    ;; an optimized regexp from them
    (,(let ((max-specpdl-size (max max-specpdl-size 1200)))
        (regexp-opt graphviz-dot-color-keywords 'words))
     . font-lock-string-face)
    (,(concat
       (regexp-opt graphviz-dot-attr-keywords 'words)
       "[ \\t\\n]*=")
     ;; RR - ugly, really, but I don't know why xemacs does not work
     ;; if I change the next car to "1"...
     (0 font-lock-variable-name-face))
    ;; The 'graph' nonterminal
    ("\\(\\_<\\(?:strict\\)?[[:space:]]*\\(?:\\(?:di\\)?graph\\)\\_>\\)"
     (1 'font-lock-keyword-face))
    ;; The 'attr_stmt'
    ("\\_<\\(edge\\|graph\\|node\\)\\_>[[:space:]]*\\["
     1 'font-lock-keyword-face)
    ;; The 'subgraph' nonterminal
    ("\\_<subgraph\\_>" . 'font-lock-keyword-face))
  "Keyword highlighting specification for `graphviz-dot-mode'.")

(defun graphviz-output-file-name (f-name)
  "Return the filename of the preview, using F-NAME."
  (concat (file-name-sans-extension f-name)
          "." graphviz-dot-preview-extension))

(defun graphviz-compile-command (f-name)
  "Shell command to compile F-NAME.
By default this is `dot -T png file.dot -o file.png', the used
program to compile can be changed by setting
`graphviz-dot-dot-program', the output format and extension can
be changed with `graphviz-dot-preview-extension'."
  (when f-name
    (setq compile-command
          (concat graphviz-dot-dot-program
                  " -T" graphviz-dot-preview-extension " "
                  (shell-quote-argument f-name)
                  " -o "
                  (shell-quote-argument
                   (graphviz-output-file-name f-name))))))

(defvar dot-menu nil
  "Menu for Graphviz Dot Mode.
This menu will get created automatically if you have the `easymenu'
package.")

;;;###autoload
(define-derived-mode graphviz-dot-mode prog-mode "dot"
  "Major mode for the dot language. \\<graphviz-dot-mode-map>
TAB indents for graph lines.

\\[graphviz-dot-indent-graph]\t- Indentation function.
\\[graphviz-dot-preview]\t- Previews graph in a buffer.
\\[graphviz-dot-view]\t- Views graph in an external viewer.
\\[graphviz-dot-indent-line]\t- Indents current line of code.

Variables specific to this mode:

  `graphviz-dot-dot-program'                   (default `dot')
       Program used to compile the graphs.
  `graphviz-dot-preview-extension'             (default `png')
       File type to use for output.
  `graphviz-dot-view-command'                  (default `dotty %s')
       Command to run when `graphviz-dot-view' is executed.
  `graphviz-dot-view-edit-command'             (default nil)
       If the user should be asked to edit the view command.
  `graphviz-dot-save-before-view'              (default t)
       Automatically save current buffer berore `graphviz-dot-view'."
  :group 'graphviz
  (setq-local font-lock-defaults '(graphviz-dot-font-lock-keywords))
  (setq-local comment-start "//")
  (setq-local comment-start-skip "/\\*+ *\\|//+ *")
  (setq-local indent-line-function 'graphviz-dot-indent-line)
  (setq-local syntax-propertize-function
              graphviz-dot-syntax-propertize-function)
  (when (buffer-file-name)
    (setq-local compile-command
                (graphviz-compile-command (buffer-file-name))))
  (when dot-menu (easy-menu-add dot-menu))
  (add-to-list 'compilation-error-regexp-alist 'dot)
  (add-to-list 'compilation-error-regexp-alist-alist
	       '(dot "^Error: \\(.+\\): .*error in line \\([0-9]+\\).*" 1 2))
  (add-hook 'after-save-hook 'graphviz-live-reload-hook)
  (run-hooks 'graphviz-dot-mode-hook))

;;;; Menu definitions

(and (condition-case nil
         (require 'easymenu)
       (error nil))
     (easy-menu-define
       dot-menu graphviz-dot-mode-map "Graphviz Mode menu"
       '("Graphviz"
	 ["Indent Graph"       graphviz-dot-indent-graph     t]
	 ["Comment Out Region" comment-region                (mark)]
	 ["Uncomment Region"   uncomment-region              (mark)]
	 "-"
	 ["Compile"            compile                       t]
	 ["Preview"            graphviz-dot-preview
	  (and (buffer-file-name)
	       (not (buffer-modified-p)))]
	 ["External Viewer"    graphviz-dot-view             (buffer-file-name)]
	 "-"
	 ["Customize..."       graphviz-dot-customize        t]
	 )))

;;;;
;;;; Indentation
;;;;

(defun graphviz-dot-indent-line ()
  "Indent current line of dot code."
  (interactive)
  (if (bolp)
      (graphviz-dot-real-indent-line)
    (save-excursion
      (graphviz-dot-real-indent-line))))

(defun graphviz-dot-real-indent-line ()
  "Indent current line of dot code."
  (beginning-of-line)
  (cond
   ((bobp)
    ;; simple case, indent to 0
    (indent-line-to 0))
   ((looking-at "^[ \t]*}[ \t]*$")
    ;; block closing, deindent relative to previous line
    (indent-line-to (save-excursion
                      (forward-line -1)
                      (if (looking-at "\\(^.*{[^}]*$\\)")
                          ;; previous line opened a block
                          ;; use same indentation
                          (current-indentation)
                        (max 0 (- (current-indentation) graphviz-dot-indent-width))))))
   ;; other cases need to look at previous lines
   (t
    (indent-line-to (save-excursion
                      (forward-line -1)
                      (cond
                       ((looking-at "\\(^.*{[^}]*$\\)")
                        ;; previous line opened a block
                        ;; indent to that line
                        (+ (current-indentation) graphviz-dot-indent-width))
                       ((and (not (looking-at ".*\\[.*\\].*"))
                             (looking-at ".*\\[.*")) ; TODO:PP : can be 1 regex
                        ;; previous line started filling
                        ;; attributes, intend to that start
                        (search-forward "[")
                        (current-column))
                       ((and (not (looking-at ".*\\[.*\\].*"))
                             (looking-at ".*\\].*")) ; TODO:PP : "
                        ;; previous line stopped filling
                        ;; attributes, find the line that started
                        ;; filling them and indent to that line
                        (while (or (looking-at ".*\\[.*\\].*")
                                   (not (looking-at ".*\\[.*"))) ; TODO:PP : "
                          (forward-line -1))
                        (current-indentation))
                       (t
                        ;; default case, indent the
                        ;; same as previous NON-BLANK line
                        ;; (or the first line, if there are no previous non-blank lines)
                        (while (and (< (point-min) (point))
                                    (looking-at "^\[ \t\]*$"))
                          (forward-line -1))
                        ;; if we find a closing square bracket, don't indent
                        ;; to the level of its attributes, but instead
                        ;; find the opening bracket and indent to that
                        (if (looking-at ".*\\].*")
                            (while (not (looking-at ".*\\[.*"))
                              (forward-line -1)))
                        (current-indentation)) ))) )))

(defun graphviz-dot-indent-graph ()
  "Indent the graph/digraph/subgraph where point is at.
This will first teach the beginning of the graph were point is at, and
then indent this and each subgraph in it."
  (interactive)
  (save-excursion
    ;; position point at start of graph
    (while (not (or (looking-at "\\(^.*{[^}]*$\\)") (bobp)))
      (forward-line -1))
    ;; bracket { one +; bracket } one -
    (let ((bracket-count 0))
      (while
          (progn
            (cond
             ;; update bracket-count
             ((looking-at "\\(^.*{[^}]*$\\)")
              (setq bracket-count (+ bracket-count 1)))
             ;; update bracket-count
             ((looking-at "^[ \t]*}[ \t]*$")
              (setq bracket-count (- bracket-count 1))))
            ;; indent this line and move on
            (graphviz-dot-indent-line)
            (forward-line 1)
            ;; as long as we are not completed or at end of buffer
            (and (> bracket-count 0) (not (eobp))))))))

;;;###autoload
(defun graphviz-dot-preview ()
  "Compile the graph and preview it in an other buffer."
  (interactive)
  (save-buffer)
  (let ((windows (window-list))
        (f-name (graphviz-output-file-name (buffer-file-name)))
        (command-result (string-trim (shell-command-to-string compile-command))))
    (if (string-prefix-p "Error:" command-result)
        (message command-result)
      (progn
        (sleep-for 0 graphviz-dot-revert-delay)
        (with-selected-window (selected-window)
          (switch-to-buffer-other-window (find-file-noselect f-name t))
          ;; I get "changed on disk; really edit the buffer?" prompt w/o this
          (sleep-for 0 50)
          (revert-buffer t t))))))

;;;###autoload
(defun graphviz-turn-on-live-preview ()
  "Turn on live preview.
This will update the preview on every save."
  (interactive)
  (setq graphviz-dot-auto-preview-on-save t)
  (add-hook 'after-save-hook 'graphviz-live-reload-hook))

;;;###autoload
(defun graphviz-turn-off-live-preview ()
  "Turn off live preview.
Saving the file will no longer also update the preview."
  (interactive)
  (setq graphviz-dot-auto-preview-on-save nil)
  (remove-hook 'after-save-hook 'graphviz-live-reload-hook))

(defun graphviz-live-reload-hook ()
  "Hook to run in `after-save-hook' for live preview to work."
  (when (and (eq major-mode 'graphviz-dot-mode) graphviz-dot-auto-preview-on-save)
    (graphviz-dot-preview)))

;;;;
;;;; View
;;;;
(defun graphviz-dot-view ()
  "Run an external viewer.
This creates an external process every time it is executed.  If
`graphviz-dot-save-before-view' is set, the current buffer is
saved before the command is executed."
  (interactive)
  (let ((cmd (if graphviz-dot-view-edit-command
                 (if (fboundp 'read-shell-command)
                     (read-shell-command "View command: "
                                         (format graphviz-dot-view-command
                                                 (shell-quote-argument (buffer-file-name))))
                   ;; read-shell-command not available in GNU Emacs 21
                   (read-from-minibuffer "View command: "
                                         (format graphviz-dot-view-command
                                                 (shell-quote-argument (buffer-file-name)))))
               (format graphviz-dot-view-command
                       (shell-quote-argument (buffer-file-name))))))
    (if graphviz-dot-save-before-view
        (save-buffer))
    (start-process-shell-command (downcase mode-name) nil cmd)
    (message (format "Executing `%s'..." cmd))))

(defun graphviz-dot-set-layout ()
  "Change the value of `graphviz-dot-dot-program'."
  (interactive)
  (setq graphviz-dot-dot-program
        (completing-read "Layout: " graphviz-dot-layout-programs)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.dot\\'" . graphviz-dot-mode))
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.gv\\'" . graphviz-dot-mode))

;; Support org-mode, when adding a code block for dot, use this mode
(with-eval-after-load 'org-src
  (defvar org-src-lang-modes)
  (add-to-list 'org-src-lang-modes  '("dot" . graphviz-dot)))

(provide 'graphviz-dot-mode)
;;; graphviz-dot-mode.el ends here
