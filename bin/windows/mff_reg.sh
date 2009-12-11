#!/bin/bash
. regfuncs.sh

regSetVal -s set '\HKEY_CLASSES_ROOT\.mff\' 'mff_auto_file'
regSetVal -s set '\HKEY_CLASSES_ROOT\mff_auto_file\shell\open\command\' "$(qq_cmdout cygpath -alw "$(which firebolt)"; echo -n ' "%1" %*')"

