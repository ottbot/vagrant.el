;;; vagrant.el --- Manage a vagrant box from emacs

;;; Version: 0.5.0
;;; Author: Robert Crim <rob@servermilk.com>
;;; Url: https://github.com/ottbot/vagrant.el
;;; Keywords: vagrant chef
;;; Created: 08 August 2013

;;; Commentary:

;; This package lets you send vagrant commands while working within a
;; project containing a Vagrantfile.
;;
;; It will traverse the directory tree until a Vagrantfile is found
;; and assume this is the box you want to work with. It can be handy
;; to bring a box up, (re)provision, or even ssh to without leaving
;; emacs.
;;
;; The emacs command `vagrant-up` will run `vagrant up` in a shell,
;; other commands follow the pattern `vagrant-X` emacs command runs
;; `vagrant X` in the shell. An exception is vagrant-edit, which will
;; open the Vagrantfile for editing.

(defvar vagrant-dir)

(defun vagrant-up ()
  "Bring up the vagrant box"
  (interactive)
  (vagrant-command "vagrant up"))

(defun vagrant-provision ()
  "Provision the vagrant box"
  (interactive)
  (vagrant-command "vagrant provision"))

(defun vagrant-destroy ()
  "Destroy the vagrant box"
  (interactive)
  (vagrant-command "vagrant destroy"))

(defun vagrant-reload ()
  "Reload the vagrant box"
  (interactive)
  (vagrant-command "vagrant reload"))

(defun vagrant-resume ()
  "Resume the vagrant box"
  (interactive)
  (vagrant-command "vagrant resume"))

(defun vagrant-ssh ()
  "SSH to the vagrant box"
  (interactive)
  (vagrant-command "vagrant ssh"))

(defun vagrant-status ()
  "Show the vagrant box status"
  (interactive)
  (vagrant-command "vagrant status"))

(defun vagrant-suspend ()
  "Suspend the vagrant box"
  (interactive)
  (vagrant-command "vagrant suspend"))

(defun vagrant-halt ()
  "Halt the vagrant box"
  (interactive)
  (vagrant-command "vagrant halt"))

(defun vagrant-edit ()
  "Edit the Vagrantfile"
  (interactive)
  (setq vagrant-dir (vagrant-locate-vagrantfile))
  (when vagrant-dir
    (find-file (concat vagrant-dir "/Vagrantfile"))))


(defun* vagrant-locate-vagrantfile (&optional (dir default-directory))
  (let ((has-vagrantfile (directory-files dir nil "^Vagrantfile$"))
        (is-root (equal dir "/")))
    (cond
     (has-vagrantfile dir)
     (is-root
      (print (format
              "No Vagrantfile found in either %s or any parent directory!"
              default-directory))
      nil)
     ((vagrant-locate-vagrantfile (expand-file-name ".." dir))))))

(defun vagrant-command (cmd)
  "Run a vagrant command in an async buffer"
  (setq vagrant-dir (vagrant-locate-vagrantfile))
  (when vagrant-dir
    (cd vagrant-dir)
    (async-shell-command cmd "*Vagrant*")))

(provide 'vagrant)

;;; vagrant.el ends here
