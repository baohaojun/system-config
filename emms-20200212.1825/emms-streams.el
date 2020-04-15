;; emms-streams.el -- A collection of online streaming audio

;; Copyright (C) 2019  Free Software Foundation, Inc.

;; Authors: Yoni Rabkin <yrk@gnu.org>

;; This file is part of EMMS.

;; EMMS is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or
;; (at your option) any later version.

;; EMMS is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.

;; You should have received a copy of the GNU General Public License
;; along with EMMS; if not, write to the Free Software Foundation,
;; Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301, USA.

;;; Commentary:

;; 2019-11-05 - This is a complete re-write of the emms-streams.el,
;; using a different approach.

;; This includes the built-in list of streams which come with Emms by
;; default. Emms has no affiliation of any kind with the streaming
;; audio stations listed below, nor is this an endorsement of these
;; stations. Instead, this is a collection of stations submitted to
;; the project over the years by people who enjoy Emms. We hope you
;; will enjoy them too.

;;; Code:

(require 'emms)
(require 'emms-source-playlist)
(require 'emms-playlist-mode)


;;; ------------------------------------------------------------------
;;; definitions
;;; ------------------------------------------------------------------

(defvar emms-streams-buffer-name "Emms Streams"
  "Name for creating a streams buffer.")

(defvar emms-streams-built-in-disclaimer
  ";; This includes the built-in list of streams which come with Emms by
;; default. Emms has no affiliation of any kind with the streaming
;; audio stations listed below, nor is this an endorsement of these
;; stations. Instead, this is a collection of stations submitted to
;; the project over the years by people who enjoy Emms. We hope you
;; will enjoy them too."
  "Explaining the relationship between Emms and these streams.")

(defvar emms-streams-built-in-list
  '((*track* (type . streamlist)
	     (name . "http://www.somafm.com/beatblender.pls")
	     (metadata "SomaFM: Beatblender" "http://www.somafm.com/beatblender.pls" 1 streamlist))
    (*track* (type . streamlist)
	     (name . "http://www.somafm.com/secretagent.pls")
	     (metadata "SomaFM: Secret Agent" "http://www.somafm.com/secretagent.pls" 1 streamlist))
    (*track* (type . streamlist)
	     (name . "http://www.somafm.com/groovesalad.pls")
	     (metadata "SomaFM: Groove Salad" "http://www.somafm.com/groovesalad.pls" 1 streamlist))
    (*track* (type . streamlist)
	     (name . "http://www.somafm.com/dronezone.pls")
	     (metadata "SomaFM: Drone Zone" "http://www.somafm.com/dronezone.pls" 1 streamlist))
    (*track* (type . streamlist)
	     (name . "http://www.somafm.com/tagstrance.pls")
	     (metadata "SomaFM: Tag's Trance" "http://www.somafm.com/tagstrance.pls" 1 streamlist))
    (*track* (type . streamlist)
	     (name . "http://www.somafm.com/indiepop.pls")
	     (metadata "SomaFM: Indie Pop Rocks" "http://www.somafm.com/indiepop.pls" 1 streamlist))
    (*track* (type . streamlist)
	     (name . "http://www.somafm.com/doomed.pls")
	     (metadata "SomaFM: Doomed" "http://www.somafm.com/doomed.pls" 1 streamlist))
    (*track* (type . url) (name . "http://listen.radionomy.com:80/-PHILOSOMATIKAPROGRESSIVE-")
	     (metadata "P H I L O S O M A T I K A - Progressive Psytrance" "http://listen.radionomy.com:80/-PHILOSOMATIKAPROGRESSIVE-" 1 url))
    (*track* (type . streamlist)
	     (name . "http://www.bassdrive.com/BassDrive.m3u")
	     (metadata "Drum and Bass Radio, BassDrive" "http://www.bassdrive.com/BassDrive.m3u" 1 streamlist))
    (*track* (type . streamlist)
	     (name . "http://www.ibiblio.org/wcpe/wcpe.pls")
	     (metadata "WCPE, Classical Music" "http://www.ibiblio.org/wcpe/wcpe.pls" 1 streamlist))
    (*track* (type . streamlist)
	     (name . "http://stream.nute.net/kohina/stream.ogg.m3u")
	     (metadata "Kohina - Old school game and demo music" "http://stream.nute.net/kohina/stream.ogg.m3u" 1 streamlist))
    (*track* (type . streamlist)
	     (name . "http://privat.is-by.us:8000/necta192.mp3.m3u")
	     (metadata "Nectarine, Demoscene Radio, DE Continuum's relay 192 mp3" "http://privat.is-by.us:8000/necta192.mp3.m3u" 1 streamlist))
    (*track* (type . streamlist)
	     (name . "http://nectarine.from-de.com/necta192.m3u")
	     (metadata "Nectarine, Demoscene Radio, DE stream (High Bitrate)" "http://nectarine.from-de.com/necta192.m3u" 1 streamlist))
    (*track* (type . streamlist)
	     (name . "http://www.wfmu.org/wfmu.pls")
	     (metadata "WFMU, Freeform radio" "http://www.wfmu.org/wfmu.pls" 1 streamlist))
    (*track* (type . streamlist)
	     (name . "http://wfmu.org/wfmu_rock.pls")
	     (metadata "WFMU, Rock'n'Soul Ichiban!" "http://www.wfmu.org/wfmu.pls" 1 streamlist))
    (*track* (type . streamlist)
	     (name . "http://wfmu.org/wfmu_drummer.pls")
	     (metadata "WFMU, Give the Drummer Radio" "http://www.wfmu.org/wfmu.pls" 1 streamlist))
    (*track* (type . streamlist)
	     (name . "http://wfmu.org/wfmu_sheena.pls")
	     (metadata "WFMU, Sheena's Jungle Room" "http://www.wfmu.org/wfmu.pls" 1 streamlist))
    (*track* (type . streamlist)
	     (name . "http://nyc01.egihosting.com:6232/listen.pls")
	     (metadata "WBCR-LP - Berkshire Community Radio" "http://nyc01.egihosting.com:6232/listen.pls" 1 streamlist))
    (*track* (type . streamlist)
	     (name . "http://199.244.85.125:8000/wxhq1")
	     (metadata "WXHQ-LP - Newport Radio" "http://199.244.85.125:8000/wxhq1" 1 streamlist))))

(defcustom emms-streams-file (concat (file-name-as-directory emms-directory)
				     "streams.emms")
  "A file used to store the built-in streams."
  :group 'emms
  :type 'file)


;;; ------------------------------------------------------------------
;;; private functions
;;; ------------------------------------------------------------------

(defun emms-streams-install-file (file)
  "Install FILE, containing streams."
  (when (not (file-directory-p (file-name-directory emms-streams-file)))
    (make-directory (file-name-directory emms-streams-file)))
  (if (or (not (file-exists-p file))
	  (and (file-exists-p file)
	       (y-or-n-p (format "overwrite existing %s?" file))))
      (progn
	(message "writing %s" file)
	(with-temp-buffer
	  (insert emms-source-playlist-native-header-line)
	  (insert (format "\n%s\n\n" emms-streams-built-in-disclaimer))
	  (insert
	   (concat "("
		   (mapconcat
		    #'(lambda (e)
			(format "%S" e))
		    emms-streams-built-in-list "\n")
		   ")"))
	  (write-region (point-min) (point-max) file))
	(message "writing %s... done" file))
    (message "aborting")))


;;; ------------------------------------------------------------------
;;; interface
;;; ------------------------------------------------------------------

(defun emms-streams-install ()
  (interactive)
  "Install the built-in streams file."
  (emms-streams-install-file emms-streams-file))

(defun emms-streams ()
  (interactive)
  "Create or switch to the built-in streaming audio playlist."
  (when (and (not (file-exists-p emms-streams-file))
	     (y-or-n-p "Emms' built-in streams file hasn't been installed yet. Install it now?"))
    (emms-streams-install))
  (let ((buf (get-buffer emms-streams-buffer-name)))
    (when (not buf)
      (with-current-buffer (get-buffer-create emms-streams-buffer-name)
	(setq buf (current-buffer))
	(emms-playlist-mode)
	(setq emms-playlist-buffer-p t)
	(emms-playlist-set-playlist-buffer (current-buffer))
	(emms-add-native-playlist emms-streams-file)))
    (switch-to-buffer buf)))


(provide 'emms-streams)

;;; emms-streams.el ends here
