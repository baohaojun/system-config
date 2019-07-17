# -*- coding: utf-8 -*-

g_quail_map = {
    "$ " : ("￥",),
    ':" ' : ("：“”",),
    "< " : ("《",),
    "<<" : ("《",),
    "> " : ("》",),
    ">>" : ("》",),
    "\ " : ("、",),
    "^ " : ("…",),
    "^^" : ("……",),
    "^^ " : ("……",),
    "_ " : ("—",),
    "__" : ("——",),
    "~ " : ("~",), # cd ~ ?
}

_char_map = {}
for c in ':', ',', '(', ')', '{', '}', '[', ']', '?':
    _char_map[c] = chr(ord(u'Ａ') - ord('A') + ord(c))

_char_map['.'] = "。"
_char_map['`'] = "‘"
_char_map['\''] = "’"
_char_map["''"] = "”"
_char_map["``"] = "“"
_char_map['`['] = "「"
_char_map['\']'] = "」"
_char_map['`[['] = '『'
_char_map['\']]'] = '』'
_char_map['``['] = '『'
_char_map['\'\']'] = '』'

for c in _char_map:
    g_quail_map[c + ' '] = (_char_map[c],)
    for ex_c in ',', '.', ':':
        g_quail_map[c + ex_c] = (_char_map[c] + _char_map[ex_c],)
        g_quail_map[c + ex_c + ' '] = (_char_map[c] + _char_map[ex_c],)

        g_quail_map[ex_c + c] = (_char_map[ex_c] + _char_map[c],)
        g_quail_map[ex_c + c + ' '] = (_char_map[ex_c] + _char_map[c],)

for c in range(ord('A'), ord('Z') + 1):
    g_quail_map[chr(c) + "  "] = (chr(ord(u'Ａ') - ord('A') + c),)

_extra_map = {}
_extra_map['`\''] = ('‘’',)
_extra_map['``\''] = ('“”',)
_extra_map['`]'] = ('「」',)
_extra_map['[\''] = ('「」',)
_extra_map['``]'] = ('『』',)
_extra_map['`]]'] = ('『』',)
_extra_map['[[\''] = ('『』',)
_extra_map['[\'\''] = ('『』',)

for c in _extra_map:
    g_quail_map[c + " "] = _extra_map[c]
