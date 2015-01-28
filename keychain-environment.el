;;; keychain-environment.el --- load keychain environment variables

;; Copyright (C) 2011-2014  Jonas Bernoulli
;; Copyright (C) 2008-2011  Paul Tipper

;; Author: Paul Tipper <bluefoo at googlemail dot com>
;; Maintainer: Jonas Bernoulli <jonas@bernoul.li>
;; Created: 20081218
;; Homepage: https://github.com/tarsius/keychain-environment
;; Keywords: gnupg, pgp, ssh

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

;; Keychain is a script that manages ssh-agent and gpg-agent.  It is
;; typically run from the shell's initialization file.  It allows your
;; shells and cron jobs to share a single ssh-agent and/or gpg-agent.

;; When keychain is run, it checks for running agent, otherwise it
;; starts them.  It saves the agents' environment variables to files
;; inside ~/.keychain/, so that subsequent shells can source these
;; files.

;; When Emacs is started under X11 and not directly from a terminal
;; these variables are not set.  This library looks for these files
;; created by keychain and then sets Emacs' environment variables
;; accordingly.  It does not actually run keychain, so you still
;; have to run that from a login shell first.

;; To use run the function `keychain-refresh-environment' in your
;; init file.  If keychain has not been run yet when you start Emacs
;; you can also later call that function interactively.

;; Also see: http://www.funtoo.org/wiki/Keychain

;;; Code:

(defvar keychain-directory
  (convert-standard-filename (expand-file-name "~/.keychain/"))
  "The directory where keychain saves environment variables.")

;;;###autoload
(defun keychain-refresh-environment (&optional host-name)
  "Set ssh-agent and gpg-agent environment variables.

Set the environment variables SSH_AUTH_SOCK, SSH_AGENT_PID
and GPG_AGENT in Emacs' `process-environment' according to
information retrieved from files created by the keychain
script with optional HOST-NAME."
  (interactive)
  (let* ((host     (if (null host-name) (system-name) host-name))
         (ssh-file (expand-file-name (concat host "-sh")     keychain-directory))
         (gpg-file (expand-file-name (concat host "-sh-gpg") keychain-directory))
         (ssh-data (and (file-exists-p ssh-file) (keychain--read-file ssh-file)))
         (gpg-data (and (file-exists-p gpg-file) (keychain--read-file gpg-file))))
    (list (and ssh-data
               (string-match "SSH_AUTH_SOCK=\\(.*?\\);" ssh-data)
               (setenv "SSH_AUTH_SOCK" (match-string 1 ssh-data)))
          (and ssh-data
               (string-match "SSH_AGENT_PID=\\([0-9]*\\)?;" ssh-data)
               (setenv "SSH_AGENT_PID" (match-string 1 ssh-data)))
          (and gpg-data
               (string-match "GPG_AGENT_INFO=\\(.*?\\);" gpg-data)
               (setenv "GPG_AGENT_INFO" (match-string 1 gpg-data))))))

(defun keychain--read-file (filename)
  "Read the content of file FILENAME and return it as a string."
  (with-temp-buffer
    (insert-file-contents filename)
    (buffer-substring-no-properties (point-min) (point-max))))

(provide 'keychain-environment)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; keychain-environment.el ends here
