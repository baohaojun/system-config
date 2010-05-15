;; mule-gbk-bc.el
;; Prepare for byte compiling the source codes which contain
;; GBK characters.

;; Usage:
;;     emacs -batch -q -no-site-file -l mule-gbk-bc.el -f batch-byte-compile foo.el

(load-file "chinese-gbk.el")

(set-language-environment 'chinese-gbk)
(setq locale-coding-system 'chinese-gbk)
(setq current-language-environment "Chinese-GBK")


