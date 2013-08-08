

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
