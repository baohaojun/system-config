(defun synthesize-multiple-events (&rest events)
  (mapc (lambda (e)
          (synthesize-event e (input-focus)))
        events))
(defun bind-both-global&window-keymaps (key map)
  (bind-keys global-keymap key map)
  (bind-keys window-keymap key map))
(bind-both-global&window-keymaps "KP_End"  '(system "netease-music loop&"))
(bind-both-global&window-keymaps "KP_Up"  '(system "netease-music nloop&"))
(bind-both-global&window-keymaps "KP_Down"  '(system "netease-music info&"))
(bind-both-global&window-keymaps "KP_Prior"  '(system "netease-music lyrics&"))
(bind-both-global&window-keymaps "KP_Next"  '(system "netease-music show&"))
(bind-both-global&window-keymaps "KP_Right"  '(system "netease-music next&"))
(bind-both-global&window-keymaps "KP_Left"  '(system "netease-music prev&"))
(bind-both-global&window-keymaps "KP_Begin"  '(system "netease-music ba&"))
(bind-both-global&window-keymaps "KP_6" '(system "netease-music rchan&"))
(bind-both-global&window-keymaps "KP_Home"  '(system "netease-music replay&"))
(bind-both-global&window-keymaps "KP_Add"  '(system "adjust-volume&"))
(bind-both-global&window-keymaps "KP_Subtract"  '(system "adjust-volume&"))
(bind-both-global&window-keymaps "KP_Insert" '(system "firefox-goto-next-prev-page prev&"))
(bind-both-global&window-keymaps "KP_Delete" '(system "firefox-goto-next-prev-page next&"))
(bind-both-global&window-keymaps "XF86Back" '(system "netease-music back&"))
(bind-both-global&window-keymaps "XF86Forward" '(system "netease-music forward&"))
(bind-both-global&window-keymaps "KP_Enter" '(system "emacsclient -e '(with-current-buffer (window-buffer (selected-window))(netease-music-jump-into))'&"))
