# -*- coding: utf-8 -*-

import opencc
cc = opencc.OpenCC('s2j.json')
print (cc.convert('亚'))
