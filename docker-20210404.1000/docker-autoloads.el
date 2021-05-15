;;; docker-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "docker" "docker.el" (0 0 0 0))
;;; Generated autoloads from docker.el
 (autoload 'docker "docker" nil t)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "docker" '("docker-read-")))

;;;***

;;;### (autoloads nil "docker-compose" "docker-compose.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from docker-compose.el
 (autoload 'docker-compose "docker-compose" nil t)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "docker-compose" '("docker-compose-")))

;;;***

;;;### (autoloads nil "docker-container" "docker-container.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from docker-container.el

(autoload 'docker-container-eshell "docker-container" "\
Open `eshell' in CONTAINER.

\(fn CONTAINER)" t nil)

(autoload 'docker-container-find-directory "docker-container" "\
Inside CONTAINER open DIRECTORY.

\(fn CONTAINER DIRECTORY)" t nil)

(autoload 'docker-container-find-file "docker-container" "\
Open FILE inside CONTAINER.

\(fn CONTAINER FILE)" t nil)

(autoload 'docker-container-shell "docker-container" "\
Open `shell' in CONTAINER.  When READ-SHELL is not nil, ask the user for it.

\(fn CONTAINER &optional READ-SHELL)" t nil)

(autoload 'docker-container-shell-env "docker-container" "\
Open `shell' in CONTAINER with the environment variable set
and default directory set to workdir. When READ-SHELL is not
nil, ask the user for it.

\(fn CONTAINER &optional READ-SHELL)" t nil)

(autoload 'docker-containers "docker-container" "\
List docker containers." t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "docker-container" '("docker-container-")))

;;;***

;;;### (autoloads nil "docker-core" "docker-core.el" (0 0 0 0))
;;; Generated autoloads from docker-core.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "docker-core" '("docker-")))

;;;***

;;;### (autoloads nil "docker-image" "docker-image.el" (0 0 0 0))
;;; Generated autoloads from docker-image.el

(autoload 'docker-image-pull-one "docker-image" "\
Pull the image named NAME.  If ALL is set, use \"-a\".

\(fn NAME &optional ALL)" t nil)

(autoload 'docker-images "docker-image" "\
List docker images." t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "docker-image" '("docker-")))

;;;***

;;;### (autoloads nil "docker-machine" "docker-machine.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from docker-machine.el

(autoload 'docker-machine-create "docker-machine" "\
Create a machine NAME using DRIVER.

\(fn NAME DRIVER)" t nil)

(autoload 'docker-machine-env-one "docker-machine" "\
Parse and set environment variables from \"docker-machine env NAME\" output.

\(fn NAME)" t nil)

(autoload 'docker-machines "docker-machine" "\
List docker machines." t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "docker-machine" '("docker-machine-")))

;;;***

;;;### (autoloads nil "docker-network" "docker-network.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from docker-network.el

(autoload 'docker-networks "docker-network" "\
List docker networks." t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "docker-network" '("docker-network-")))

;;;***

;;;### (autoloads nil "docker-utils" "docker-utils.el" (0 0 0 0))
;;; Generated autoloads from docker-utils.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "docker-utils" '("docker-utils-")))

;;;***

;;;### (autoloads nil "docker-volume" "docker-volume.el" (0 0 0 0))
;;; Generated autoloads from docker-volume.el

(autoload 'docker-volume-dired "docker-volume" "\
Enter `dired' in the volume named NAME.

\(fn NAME)" t nil)

(autoload 'docker-volumes "docker-volume" "\
List docker volumes." t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "docker-volume" '("docker-volume-")))

;;;***

;;;### (autoloads nil nil ("docker-faces.el" "docker-pkg.el") (0
;;;;;;  0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; docker-autoloads.el ends here
