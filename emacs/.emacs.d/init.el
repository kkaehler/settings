;; Init file for emacs
;; Author: Kip Kaehler 5/30/2012
;; TODO: Add all the old goodies lost in the great crash

(add-hook 'write-file-hooks 'delete-trailing-whitespace)

(setq-default indent-tabs-mode nil)
(setq require-final-newline t)
(menu-bar-mode -1)
(fset 'yes-or-no-p 'y-or-n-p)
(setq column-number-mode 1)

(when (fboundp 'windmove-default-keybindings)
      (windmove-default-keybindings))

(defun font-lock-width-keyword (width)
  "Return a font-lock style keyword for a string beyond width WIDTH
  that uses 'font-lock-warning-face'."
  `((,(format "^%s\\(.+\\)" (make-string width ?.))
     (1 font-lock-warning-face t))))

(font-lock-add-keywords 'python-mode (font-lock-width-keyword 100))
(font-lock-add-keywords 'espresso-mode (font-lock-width-keyword 80))

(global-set-key (kbd "C-c c") 'copy-to-clipboard)
(global-set-key (kbd "C-c p") 'copy-to-paste-buffer)
(global-set-key (kbd "C-c d") 'duplicate-line)

(defvar paste-buffer-file "/tmp/screen-exchange"
  "The name of the file `copy-to-paste-buffer' writes to.")

;; The paste-buffer-copy alias is less ambiguous than copy-to-paste-buffer
;; when running interactively (i.e., M-x ...).
(defalias 'paste-buffer-copy 'copy-to-paste-buffer)

;;NOTE: sudo apt-get install xclip for this to work
(defun copy-to-paste-buffer (start end)
  "Copy the text of the region to the screen paste buffer. The paste
buffer file is /tmp/screen-exchange by default, but can be changed
by setting paste-buffer-file.

The text is inserted into that file, replacing existing text there.

When calling from a program, give two arguments: START and END.
START and END specify the portion of the current buffer to be copied."
  (interactive "r")
  (write-region start end paste-buffer-file))

(cond ((equal system-type 'gnu/linux)
       (defalias 'copy-to-clipboard 'copy-via-xclip))
      ((equal system-type 'darwin)
       (defalias 'copy-to-clipboard 'copy-via-pbcopy)))

(defun copy-via-xclip (start end)
  "Copy the text of the region to the X clipboard via xclip.

When calling from a program, give two arguments: START and END.
START and END specify the portion of the current buffer to be copied."
  ;; Note: running xclip with shell-command-on-region caused it to
  ;; hang, and I wasn't able to figure out why.
  (interactive "r")
  (let* ((process-connection-type nil)
          (proc (start-process
                 "xclip" nil "xclip" "-selection" "clipboard")))
    (process-send-string proc (buffer-substring start end))
    (process-send-eof proc)
    (message "Copied region via xclip")))

(defun copy-via-pbcopy (start end)
  "Copy the text of the region to the OS X pasteboard via pbcopy.

When calling from a program, give two arguments: START and END.
START and END specify the portion of the current buffer to be copied."
  ;; Note: running xclip with shell-command-on-region caused it to
  ;; hang, and I wasn't able to figure out why.
  (interactive "r")
  (let* ((process-connection-type nil)
          (proc (start-process
                 "pbcopy" nil "pbcopy")))
    (process-send-string proc (buffer-substring start end))
    (process-send-eof proc)
    (message "Copied region via pbcopy")))

(defun smart-beginning-of-line (&optional n)
  "Move point to the beginning of the current line or, if it's
already there, move point to the first non-whitespace character
on this line."
  (interactive "^p")
  (let ((pos (point)))
    (beginning-of-line)
    (if (= pos (point))
        (back-to-indentation))))

;;TODO: add debug goodies to emacs-local.el
;; Append local elisp dir to load path.
(add-to-list 'load-path "~/emacs.d" t)
;; Load local init file if it exists.
(load "~/.emacs.d/emacs-local.el")
