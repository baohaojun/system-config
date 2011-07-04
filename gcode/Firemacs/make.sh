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

CHROME="
content/config-name.js
content/config.js
content/config.xul
content/firemacs.js
content/firemacs.xul
content/init.js
content/keybinder.js
content/keybinding.js
content/keyhandler.js
content/status.js
content/statusbar.xul
content/subfunc.js
skin/config.css
skin/icon16.png
skin/icon16gray.png
skin/icon32.png
"

FMX="
chrome.manifest
chrome/firemacs.jar
components/firemacs-service.js
install.rdf
"

rm -rf work
mkdir work
tar cf - $FILES | (cd work; tar xvf -)
cd work/chrome
zip firemacs.jar $CHROME
rm -rf content
rm -rf skin
cd ..
zip firemacs-$version.xpi $FMX
rm -rf $FMX chrome components

#
# End
#
