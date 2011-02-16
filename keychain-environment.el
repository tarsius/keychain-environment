;;; keychain-environment.el --- Loads keychain environment variables into emacs
 
;; Copyright (C) 2008-2010 Paul Tipper
 
;; Author:  Paul Tipper <bluefoo at googlemail dot com>
;; Keywords: keychain, ssh
;; Created: 18 Dec 2008
;; Updated: 16 Feb 2011

;; Version: 1.0.2-git

;; This file is not part of GNU Emacs.
 
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
 
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary: 
;; 
;; Designed for use with Keychain, see:
;; (http://www.gentoo.org/proj/en/keychain/) a tool for loading the
;; SSH Agent and keeping it running and accessible on a machine for
;; longer than a single login seession.
;; 
;; This library loads the file "$HOME/.keychain/$HOSTNAME-sh" and parses it for
;; the SSH_AUTH_SOCK, SSH_AUTH_PID and GPG_AGENT_INFO variables, placing these
;; into the environment of Emacs.
;;
;; This is useful for situations where you are running Emacs under X, not
;; directly from a terminal, and its inheriting its environment from the
;; window manager, which doesn't have these variables as you started keychain
;; after you logged in (say as part of your .bashrc)
;;
;; The function (keychain-refresh-environment) can also be run at any time
;; these variables change.

;;; Installation:
;;
;; Put the file in your load-path then use:
;; 
;;   (require 'keychain-environment)
;;   (keychain-refresh-environment)
 
;;; History:
;;
;; 2008-12-18 Initial development.
;; 2009-02-25 Fixed bug with system-name being evaluated to the full hostname
;; 2010-07-27 Added GPG_AGENT support
;;            (by Michael Markert: markert dot michael at googlemail dot com)

;;; Code: 

(defvar keychain-ssh-file
  (concat (getenv "HOME") "/.keychain/"
	  (car (split-string system-name "\\." t)) "-sh")
  "Stores the location of the keychain ssh file to load.
Normally found in the '$HOME/.keychain' directory and called '$HOSTNAME-sh'.")

(defvar keychain-gpg-file
  (concat (getenv "HOME") "/.keychain/"
	  (car (split-string system-name "\\." t)) "-sh-gpg")
  "Stores the location of the keychain gpg file to load.
Normally found in the '$HOME/.keychain' directory and called '$HOSTNAME-sh-gpg'.")

;; Really there should be an easier method of doing this surely?
(defun keychain-read-file (filename)
  "Takes a filename, reads the data from it and returns it as a string"
  (let* ((real-filename (expand-file-name filename))
	 (visited (find-buffer-visiting real-filename))
	 (orig-buffer (current-buffer))
	 (buf (find-file-noselect real-filename))
	 (data (save-excursion
		 (set-buffer buf)
		 (let ((data (buffer-substring-no-properties (point-min)
							     (point-max))))
		   (set-buffer orig-buffer)
		   data))))
    (unless visited
      (kill-buffer buf))
    data)))

(defun keychain-refresh-environment ()
  "Set ssh and gpg environment variables based on information from keychain.

The environment variables SSH_AUTH_SOCK, SSH_AGENT_PID and GPG_AGENT are
set in variable `process-environment' based on information retrieved from
keychain."
  (interactive)
  (let* ((ssh-data  (when (file-exists-p keychain-ssh-file)
		      (keychain-read-file keychain-ssh-file)))
         (gpg-data  (when (file-exists-p keychain-gpg-file)
		      (keychain-read-file keychain-gpg-file)))
         (auth-sock (when ssh-data
                      (string-match "SSH_AUTH_SOCK=\\(.*?\\);" ssh-data)
                      (match-string 1 ssh-data)))
         (auth-pid  (when ssh-data
		      (string-match "SSH_AGENT_PID=\\([0-9]*\\)?;" ssh-data)
		      (match-string 1 ssh-data)))
	 (gpg-agent (when gpg-data
		      (string-match "GPG_AGENT_INFO=\\(.*?\\);" gpg-data)
		      (match-string 1 gpg-data))))
    (when auth-sock
      (setenv "SSH_AUTH_SOCK" auth-sock))
    (when auth-pid
      (setenv "SSH_AUTH_PID" auth-pid))
    (when gpg-agent
      (setenv "GPG_AGENT_INFO" gpg-agent))
    (list auth-sock auth-pid gpg-agent)))

(provide 'keychain-environment)
