;;; keychain-environment.el --- loads keychain environment variables

;; Copyright (C) 2011-2012  Jonas Bernoulli
;; Copyright (C) 2008-2011  Paul Tipper

;; Author: Paul Tipper <bluefoo at googlemail dot com>
;; Maintainer: Jonas Bernoulli <jonas@bernoul.li>
;; Keywords: gnupg, pgp, ssh
;; Version:
;; Created: 20081218

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Designed for use with Keychain (see http://docs.funtoo.org/wiki/Keychain)
;; a tool for loading the SSH Agent and keeping it running and accessible on
;; a machine for longer than a single login seession.
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

;;; Code:

(defvar keychain-ssh-file
  (concat (getenv "HOME") "/.keychain/"
          (car (split-string system-name "\\." t)) "-sh")
  "The location of the keychain ssh file.")

(defvar keychain-gpg-file
  (concat (getenv "HOME") "/.keychain/"
          (car (split-string system-name "\\." t)) "-sh-gpg")
  "The location of the keychain gpg file.")

(defun keychain-read-file (filename)
  "Read the content of file FILENAME and return it as a string"
  (let* ((old-buffer (find-buffer-visiting filename))
         old-buffer-name)
    (with-current-buffer (let ((find-file-visit-truename t))
                           (or old-buffer (find-file-noselect filename)))
      (when old-buffer
        (setq old-buffer-name (buffer-file-name))
        (set-visited-file-name (file-chase-links filename)))
      (prog1 (buffer-substring-no-properties (point-min) (point-max))
        (if old-buffer
            (progn
              (set-visited-file-name old-buffer-name)
              (set-buffer-modified-p nil))
          (kill-buffer (current-buffer)))))))

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
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; keychain-environment.el ends here
