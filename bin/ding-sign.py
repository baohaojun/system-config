#!/usr/bin/env python

import time
import hmac
import hashlib
import base64

# make this little script support both python2 and python3
try:
    import urllib.parse as urllib
except ImportError:
    import urllib

import sys
timestamp = int(round(time.time() * 1000))
secret = sys.argv[1]
string_to_sign = '{0}\n{1}'.format(timestamp, secret)

hmac_code = hmac.new(
    secret.encode('utf-8'),
    string_to_sign.encode('utf-8'),
    digestmod=hashlib.sha256
).digest()

sign = urllib.quote_plus(base64.b64encode(hmac_code))
print("&timestamp=%s&sign=%s" % (timestamp, sign))
