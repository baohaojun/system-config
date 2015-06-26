;; Happy hacking bhj - Emacs ♥ you!

(setq socks-noproxy '("127.0.0.1"))
(setq socks-server '("Default server" "127.0.0.1" 9090 5))
(setq url-gateway-method 'socks)

(with-current-buffer (url-retrieve-synchronously "http://172.16.0.9/setup-system-config.sh")
  (buffer-substring (point-min) (point-max)))
