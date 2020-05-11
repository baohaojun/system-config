import os, sys
from os.path import join, exists, isdir, isfile, dirname, abspath, expanduser

import xdg.BaseDirectory
import gtk, gtk.gdk

# Autotools set the actual data_dir in defs.py
from defs import VERSION, DATA_DIR

from glipper.Clipboards import *
