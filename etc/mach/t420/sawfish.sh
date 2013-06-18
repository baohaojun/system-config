#!/bin/bash

sawfish-client -e '(bind-keys global-keymap "Super-Down" (lambda () (system "~/bin/douban info&")))'
sawfish-client -e '(bind-keys global-keymap "Super-Up" (lambda () (system "~/bin/douban lyrics&")))'
sawfish-client -e '(bind-keys global-keymap "Super-Right" (lambda () (system "~/bin/douban next&")))'
sawfish-client -e '(bind-keys global-keymap "Super-Left" (lambda () (system "~/bin/douban prev&")))'
