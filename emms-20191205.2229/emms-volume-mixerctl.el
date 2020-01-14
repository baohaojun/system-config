;;; emms-volume-mixerctl.el --- a mode for changing volume using mixerctl

;; Copyright (C) 2006, 2007, 2008, 2009, 2019 Free Software Foundation, Inc.

;; Authors: Martin Schoenmakers <aiviru@diamond-age.net>
;;          Bruno Félix Rezende Ribeiro <oitofelix@gnu.org>

;; This file is part of EMMS.

;; EMMS is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; EMMS is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with EMMS; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin St, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This file defines a few simple functions to raise or lower the volume
;; using mixerctl. It can be used stand-alone, though it's meant for usage
;; with EMMS, particularly with emms-volume.el

;;; History:

;; Jul 06 2019: Based on emms-volume-amixer.el by Martin Schoenmakers

;;; Todo:

;; There probably needs to be more configurability, which may in turn
;; mean adding some more functions.
;; Some of this could benefit from adding customize interfaces.

;;; Code:

(defcustom emms-volume-mixerctl-control "master"
  "The control to change the volume with.
Controls includes \"master\", \"mono\", etc. For a full list of available
controls, run `mixerctl -a' in a shell."
  :type '(choice (const :tag "master" "master")
                 (const :tag "mono" "mono")
                 (string :tag "Something else: "))
  :group 'emms-volume)

(defcustom emms-volume-mixerctl-card 0
  "The card number to change volume.
The card is identified by a number. For a full list run `ls
/dev/mixer?*' in a shell."
  :type 'integer
  :group 'emms-volume)

;;;###autoload
(defun emms-volume-mixerctl-change (amount)
  "Change mixerctl master volume by AMOUNT."
  (message "Playback channels: %s"
           (with-temp-buffer
             (when (zerop
                    (call-process "mixerctl" nil (current-buffer) nil
				  "-f"
				  (format "/dev/mixer%d" emms-volume-mixerctl-card)
				  (let ((amount-str
					 (format "%s%d" (if (< amount 0) "-" "+")
						 (abs amount))))
                                    (format "outputs.%s=%s,%s"
					    emms-volume-mixerctl-control
					    amount-str amount-str))))
               (if (and (forward-line -1)
			(re-search-forward "^\\(.*\\):.*->.*,\\(.*\\)$" nil t))
                   (format "%s -> %s" (match-string 1) (match-string 2)))))))

(provide 'emms-volume-mixerctl)

;;; emms-volume-mixerctl.el ends here
