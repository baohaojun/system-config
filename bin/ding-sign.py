#!/usr/bin/env python

import time
import hmac
import hashlib
import base64
import urllib
import sys
timestamp = int(round(time.time() * 1000))
secret = sys.argv[1]
secret_enc = bytes(secret).encode('utf-8')
string_to_sign = '{0}\n{1}'.format(timestamp, secret)
string_to_sign_enc = bytes(string_to_sign).encode('utf-8')
hmac_code = hmac.new(
    secret_enc,
    string_to_sign_enc,
    digestmod=hashlib.sha256
).digest()

sign = urllib.quote_plus(base64.b64encode(hmac_code))
print("&timestamp=%s&sign=%s" % (timestamp, sign))
