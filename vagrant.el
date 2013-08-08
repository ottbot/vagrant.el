

(defun vagrant-up ()
  "Bring up the vagrant box"
  (interactive)
  (vagrant-command "vagrant up"))

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
