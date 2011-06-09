#!/bin/sh

version=`grep em:version install.rdf | sed 's/[^.0-9]//g'`

FILES="
chrome.manifest
chrome/content/config-name.js
chrome/content/config.js
chrome/content/config.xul
chrome/content/firemacs.js
chrome/content/firemacs.xul
chrome/content/init.js
chrome/content/keybinder.js
chrome/content/keybinding.js
chrome/content/keyhandler.js
chrome/content/status.js
chrome/content/statusbar.xul
chrome/content/subfunc.js
chrome/skin/config.css
chrome/skin/icon16.png
chrome/skin/icon16gray.png
chrome/skin/icon32.png
components/firemacs-service.js
install.rdf
"

rm -rf work
mkdir work
zip work/firemacs-$version.xpi $FILES

#
# End
#
