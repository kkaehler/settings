
;;Copy current line
(defun jao-copy-line ()
  "Copy current line in the kill ring"
  (interactive)
  (kill-ring-save (line-beginning-position)
                  (line-beginning-position 2))
  (message "Line copied"))
(global-set-key (kbd "C-p") 'jao-copy-line)

(defun duplicate-line()
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (open-line 1)
  (next-line 1)
  (yank))
(global-set-key (kbd "C-d") 'duplicate-line)

(defun destroy-line()
  (interactive)
  (move-beginning-of-line 1)
  (kill-line))
(global-set-key (kbd "C-j") 'destroy-line)

(defun nd-run-tests-get-command ()
  "Return a test run command after prompting user to edit the test target."
  (read-string "Test command: "
               (concat "python manage.py test --nocapture "
                       (let ((file-name (buffer-file-name))
                             (method-name (python-current-defun 100)))
                         (if file-name
                             (if method-name (concat file-name ":" method-name)
                               file-name))))))

(setq compilation-scroll-output :first-error)
(defun nd-run-tests (command)
  "Run Nextdoor unit tests."
  (interactive (list (nd-run-tests-get-command)))
  (let ((dir default-directory))
    (cd "~/src/nextdoor.com/apps/nextdoor")
    (compile command)
    (cd dir)))

(defun nd-debug-tests (command)
  "Run Nextdoor unit tests under pdb."
  (interactive (list (nd-run-tests-get-command)))
  (cd "~/src/nextdoor.com/apps/nextdoor")
  (let ((dir default-directory))
    (cd "~/src/nextdoor.com/apps/nextdoor")
    (pdb command)
    (cd dir)))

(defun nd-debug-frontend ()
  "Run the Nextdoor frontend under pdb."
  (interactive)
  (let ((dir default-directory))
    (cd "~/src/nextdoor.com/apps/nextdoor")
    (pdb "python manage.py runserver 0.0.0.0:8000")
    (cd dir)))

(defun nd-django-shell ()
  "Run the Django Shell."
  (interactive)
  (let ((dir default-directory))
    (cd "~/src/nextdoor.com/apps/nextdoor")
    ("python manage.py shell")
    (cd dir)))


(defun nd-debug-frontend-apache ()
  "Run the Nextdoor frontend under pdb and apache."
  (interactive)
  (pdb "/usr/local/apache2/bin/httpd -X"))

(add-hook 'python-mode-hook
     '(lambda ()
        (highlight-lines-matching-regexp "import pdb; pdb.set_trace()")))

;; Set key bindings
(global-set-key [f5] 'recompile)
(global-set-key [f6] 'next-error)
(global-set-key [f7] 'previous-error)
(global-set-key [f8] 'rotate-among-files)

;; Set command to run for python-check (C-c C-v).
(setq python-check-command "~/bin/pycheck.sh")

;; Functions for rotating between python files and their unit tests.
(defun find-non-test-file (file-name-base test-pattern)
  "Find the non-test file corresponding to FILE-NAME-BASE."
  (let ((target-file-name
         (concat (replace-regexp-in-string test-pattern "." file-name-base)
                 extension)))
    (if (file-exists-p target-file-name)
        (find-file target-file-name)
      (message (concat target-file-name " does not exist")))))

(defun find-test-file (file-name-base test-pattern)
  "Find the test file corresponding to FILE-NAME-BASE."
  (let ((target-file-name
         (concat file-name-base test-pattern "." extension)))
    (if (file-exists-p target-file-name)
        (find-file target-file-name)
      (message (concat target-file-name " does not exist")))))

(defun rotate-among-files ()
  "Rotate among related files (e.g., Python files and their test files)."
  (interactive)
  (let* ((test-pattern "_test")
         (file-name-base (file-name-sans-extension buffer-file-name))
         (extension (file-name-extension buffer-file-name)))
    (if (string= extension "py")
        (cond ((string-match (concat test-pattern "$") file-name-base)
               (find-non-test-file file-name-base test-pattern))
              ((find-test-file file-name-base test-pattern)))
      (message "no file to switch to"))))
