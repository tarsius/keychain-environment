;;; keychain-environment.el --- load keychain environment variables

;; Copyright (C) 2011-2012  Jonas Bernoulli
;; Copyright (C) 2008-2011  Paul Tipper

;; Author: Paul Tipper <bluefoo at googlemail dot com>
;; Maintainer: Jonas Bernoulli <jonas@bernoul.li>
;; Created: 20081218
;; Version: 2.0.0
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
;; these variables are not.  This library looks for the files created
;; by keychain and sets Emacs' environment variables accordingly.  It
;; does not actually run keychain, so you still have to run that from
;; a login shell first.

;; To use run the function `keychain-refresh-environment' in your init
;; file.  If keychain has not been run yet when you start Emacs you
;; can also later call that function interactively.

;; Also see: http://www.funtoo.org/wiki/Keychain

;;; Code:

(defvar keychain-directory
  (convert-standard-filename (expand-file-name "~/.keychain/"))
  "The directory where keychain saves environment variables.")

;;;###autoload
(defun keychain-refresh-environment ()
  "Set ssh-agent and gpg-agent environment variables.

Set the environment variables SSH_AUTH_SOCK, SSH_AGENT_PID
and GPG_AGENT in Emacs' `process-environment' according to
information retrieved from files created by the keychain
script."
  (interactive)
  (let* ((host      (car (split-string system-name "\\." t)))
         (ssh-file  (expand-file-name (concat host "-sh")
                                      keychain-directory))
         (ssh-data  (when (file-exists-p ssh-file)
                      (keychain-read-file ssh-file)))
         (auth-sock (when ssh-data
                      (string-match "SSH_AUTH_SOCK=\\(.*?\\);" ssh-data)
                      (match-string 1 ssh-data)))
         (auth-pid  (when ssh-data
                      (string-match "SSH_AGENT_PID=\\([0-9]*\\)?;" ssh-data)
                      (match-string 1 ssh-data)))
         (gpg-file  (expand-file-name (concat host "-sh-gpg")
                                      keychain-directory))
         (gpg-data  (when (file-exists-p gpg-file)
                      (keychain-read-file gpg-file)))
         (gpg-agent (when gpg-data
                      (string-match "GPG_AGENT_INFO=\\(.*?\\);" gpg-data)
                      (match-string 1 gpg-data))))
    (when auth-sock
      (setenv "SSH_AUTH_SOCK" auth-sock))
    (when auth-pid
      (setenv "SSH_AGENT_PID" auth-pid))
    (when gpg-agent
      (setenv "GPG_AGENT_INFO" gpg-agent))
    (list auth-sock auth-pid gpg-agent)))

(defun keychain-read-file (filename)
  "Read the content of file FILENAME and return it as a string"
  (let ((old-buffer (find-buffer-visiting filename))
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

(provide 'keychain-environment)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; keychain-environment.el ends here
