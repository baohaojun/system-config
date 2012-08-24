#!/usr/bin/env python

import unicodedata
import sys
print unicodedata.normalize('NFKD', sys.argv[1].decode('utf-8')).encode('ascii','ignore')
