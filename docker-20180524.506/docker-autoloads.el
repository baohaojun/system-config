;;; docker-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "docker" "docker.el" (0 0 0 0))
;;; Generated autoloads from docker.el

(autoload 'docker-mode "docker" "\
Minor mode to manage docker.

\(fn &optional ARG)" t nil)

(defvar docker-global-mode nil "\
Non-nil if Docker-Global mode is enabled.
See the `docker-global-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `docker-global-mode'.")

(custom-autoload 'docker-global-mode "docker" nil)

(autoload 'docker-global-mode "docker" "\
Toggle Docker mode in all buffers.
With prefix ARG, enable Docker-Global mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Docker mode is enabled in all buffers where
`docker-mode' would do it.
See `docker-mode' for more information on Docker mode.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "docker" '("docker-")))

;;;***

;;;### (autoloads nil "docker-containers" "docker-containers.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from docker-containers.el

(autoload 'docker-start "docker-containers" "\
Start the container named NAME.

\(fn NAME)" t nil)

(autoload 'docker-stop "docker-containers" "\
Stop the container named NAME.

TIMEOUT is the number of seconds to wait for the container to stop before killing it.

\(fn NAME &optional TIMEOUT)" t nil)

(autoload 'docker-restart "docker-containers" "\
Restart the container named NAME.

TIMEOUT is the number of seconds to wait for the container to stop before killing it.

\(fn NAME &optional TIMEOUT)" t nil)

(autoload 'docker-pause "docker-containers" "\
Pause the container named NAME.

\(fn NAME)" t nil)

(autoload 'docker-unpause "docker-containers" "\
Unpause the container named NAME.

\(fn NAME)" t nil)

(autoload 'docker-rm "docker-containers" "\
Remove the container named NAME.

With prefix argument, sets FORCE to true.

Force the removal even if the container is running when FORCE is set.
Remove the specified link and not the underlying container when LINK is set.
Remove the volumes associated with the container when VOLUMES is set.

\(fn NAME &optional FORCE LINK VOLUMES)" t nil)

(autoload 'docker-kill "docker-containers" "\
Kill the container named NAME using SIGNAL.

\(fn NAME &optional SIGNAL)" t nil)

(autoload 'docker-inspect "docker-containers" "\
Inspect the container named NAME.

\(fn NAME)" t nil)

(autoload 'docker-container-find-file "docker-containers" "\
Inside CONTAINER open FILE.

\(fn CONTAINER FILE)" t nil)

(autoload 'docker-container-dired "docker-containers" "\
Inside CONTAINER open DIRECTORY.

\(fn CONTAINER DIRECTORY)" t nil)

(autoload 'docker-container-eshell "docker-containers" "\
Open `eshell' in CONTAINER.

\(fn CONTAINER)" t nil)

(autoload 'docker-container-shell "docker-containers" "\
Open `shell' in CONTAINER.

\(fn CONTAINER)" t nil)

(autoload 'docker-containers-rename "docker-containers" "\
Rename a container.

\(fn)" t nil)

(autoload 'docker-containers "docker-containers" "\
List docker containers.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "docker-containers" '("docker-")))

;;;***

;;;### (autoloads nil "docker-images" "docker-images.el" (0 0 0 0))
;;; Generated autoloads from docker-images.el

(autoload 'docker-pull "docker-images" "\
Pull the image named NAME.  If ALL is set, use \"-a\".

\(fn NAME &optional ALL)" t nil)

(autoload 'docker-push "docker-images" "\
Push the image named NAME.

\(fn NAME)" t nil)

(autoload 'docker-rmi "docker-images" "\
Destroy or untag the image named NAME.

Force removal of the image when FORCE is set.
Do not delete untagged parents when NO-PRUNE is set.

\(fn NAME &optional FORCE NO-PRUNE)" t nil)

(autoload 'docker-images-tag-entry "docker-images" "\


\(fn)" t nil)

(autoload 'docker-images "docker-images" "\
List docker images.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "docker-images" '("docker-")))

;;;***

;;;### (autoloads nil "docker-machine" "docker-machine.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from docker-machine.el

(autoload 'docker-machine-config "docker-machine" "\
Print the connection config for the machine NAME.

\(fn NAME)" t nil)

(autoload 'docker-machine-inspect "docker-machine" "\
Inspect information about the machine NAME.

\(fn NAME)" t nil)

(autoload 'docker-machine-ip "docker-machine" "\
Get the IP address of the machine NAME.

\(fn NAME)" t nil)

(autoload 'docker-machine-status "docker-machine" "\
Get the status of the machine NAME.

\(fn NAME)" t nil)

(autoload 'docker-machine-upgrade "docker-machine" "\
Upgrade the machine NAME to the latest version of Docker.

\(fn NAME)" t nil)

(autoload 'docker-machine-kill "docker-machine" "\
Kill the machine NAME.

\(fn NAME)" t nil)

(autoload 'docker-machine-create "docker-machine" "\
Create a machine NAME using DRIVER.

\(fn NAME DRIVER)" t nil)

(autoload 'docker-machine-start "docker-machine" "\
Start the machine NAME.

\(fn NAME)" t nil)

(autoload 'docker-machine-env "docker-machine" "\
Parse and set environment variables from \"docker-machine env NAME\" output.

\(fn NAME)" t nil)

(autoload 'docker-machine-stop "docker-machine" "\
Stop the machine NAME.

\(fn NAME)" t nil)

(autoload 'docker-machine-restart "docker-machine" "\
Restart the machine NAME.

\(fn NAME)" t nil)

(autoload 'docker-machine-rm "docker-machine" "\
Destroy or uncommand the machine NAME.  If FORCE is set, use \"--force\".

\(fn NAME &optional FORCE)" t nil)

(autoload 'docker-machines "docker-machine" "\
List docker machines.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "docker-machine" '("docker-")))

;;;***

;;;### (autoloads nil "docker-networks" "docker-networks.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from docker-networks.el

(autoload 'docker-network-rm "docker-networks" "\
Destroy the network named NAME.

\(fn NAME)" t nil)

(autoload 'docker-networks "docker-networks" "\
List docker networks.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "docker-networks" '("docker-")))

;;;***

;;;### (autoloads nil "docker-process" "docker-process.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from docker-process.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "docker-process" '("docker")))

;;;***

;;;### (autoloads nil "docker-utils" "docker-utils.el" (0 0 0 0))
;;; Generated autoloads from docker-utils.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "docker-utils" '("docker-utils-")))

;;;***

;;;### (autoloads nil "docker-volumes" "docker-volumes.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from docker-volumes.el

(autoload 'docker-volume-rm "docker-volumes" "\
Destroy the volume named NAME.

\(fn NAME)" t nil)

(autoload 'docker-volumes "docker-volumes" "\
List docker volumes.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "docker-volumes" '("docker-")))

;;;***

;;;### (autoloads nil nil ("docker-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; docker-autoloads.el ends here
