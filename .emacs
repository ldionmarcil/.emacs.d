(custom-set-variables
 '(ansi-color-names-vector ["black" "#d55e00" "#009e73" "#f8ec59" "#0072b2" "#cc79a7" "#56b4e9" "white"])
 '(custom-enabled-themes (quote (deeper-blue)))
 '(erc-input-line-position -1)
 '(erc-modules (quote (autojoin button completion dcc fill irccontrols keep-place list match menu move-to-prompt netsplit networks noncommands readonly ring stamp track)))
 '(inhibit-startup-screen t)
 '(keyboard-coding-system (quote cp1252))
 '(menu-bar-mode nil)
 '(selection-coding-system (quote utf-16le-dos))
 '(server-use-tcp t)
 '(tool-bar-mode nil))

(custom-set-faces)

(setq utf-translate-cjk-mode nil)
(set-language-environment 'utf-8)
(setq locale-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(require 'linum nil 'noerror)
(require 'cl)
(require 'ls-lisp)
(require 'tramp)
(require 'smex)

;;slime
(add-to-list 'load-path "/home/maden/Programming/slime")  ; your SLIME directory
(setq inferior-lisp-program "/usr/bin/clisp") ; your Lisp system
(require 'slime)
(slime-setup)



(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
;;(global-set-key (kbd "M-x") 'smex-major-mode-commands)
(global-set-key (kbd "C-c M-x") 'smex-update)
(setq tramp-default-method "scp")

(add-to-list 'load-path "~/.emacs.d/elisp/")
(require 'dired-details)
(dired-details-install)

(setq ls-lisp-use-insert-directory-program nil)
(setq-default truncate-lines t)
(menu-bar-mode -99)

(setq ediff-split-window-function 'split-window-horizontally)
;;(global-set-key 'load-file "~/.emacs")
(global-set-key "\M-p" 'reload-init-file)
(global-set-key "\M-P" 'switch-environ)
(column-number-mode)
(blink-cursor-mode -1)


(global-set-key (kbd "<f1>") 'start-irc)
(global-set-key (kbd "C-M-q") 'indent-code-rigidly)
(global-set-key (kbd "<f5>") 'other-window)
(defun batch-process ()
  (interactive)
  (shell-command "clean.bat") 
  (message "Files Reverted"))

(defun start-firefox ()
  (interactive)
  (browse-url-firefox "about:blank"))

(global-set-key (kbd "C-:") 'insert-eval-value)
(defun insert-eval-value (exp)
  (interactive "aEval: ")
  (insert (eval-expression exp)))

(defun reload-init-file ()
  (interactive)
  (load-file "~/.emacs"))

(defun switch-environ (x)
  (interactive "aWhat environment? [1:dev4/2:qa4]: ")
  ;;(setq x (read-from-minibuffer "What environment? [1:dev4/2:qa4]: "))
  (case (string-to-number x) 
	(1 (change-conn "dev4"))
	(2 (change-conn "qa4"))
	(otherwise (message "found nothing")))
;;  (message env)
  ;;(message "@<b>test@</b>")
)
(defun change-conn (filename)
  (message (concatenate 'string "inserting" " " filename))
  (goto-line 12)
  (dotimes (i 9) (kill-line))
  (insert-file (concatenate 'string "~/tools/db/" filename))
  (save-buffer)
)

(defun xml-fix-indent (begin end)
  (interactive "r")
  (save-excursion
      (nxml-mode)
      (goto-char begin)
      (while (search-forward-regexp "\>[ \\t]*\<" nil t) 
        (backward-char) (insert "\n"))
      (indent-region begin end)))

(defun comment-current-line ()
  (interactive)
  (setq cur-pos (point))
)




(setq display-time-day-and-date t
      display-time-24hr-format t)
(display-time)

(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)


(setq erc-autojoin-channels-alist
      '(("swiftirc\\.net" "#papillons" "#chaos-team")
	("freenode.net\\.in" "#emacs","##linux" "#lisp" "#python" "#r_netsec" "#scheme" "#space" "#raspberrypi")))

(require 'erc-join)
(erc-autojoin-mode t)

(defun start-irc ()
  (interactive)
  (erc-tls :server "irc.swiftirc.net" :port 6697 :nick "ldionmarcil")
  (erc-tls :server "irc.freenode.net" :port 6697 :nick "ldionmarcil"))

(setq erc-timestamp-only-if-changed-flag nil
      erc-timestamp-format "%R:"
      erc-insert-timestamp-function 'erc-insert-timestamp-left
      erc-fill-prefix ""
      erc-flood-protect nil)

(add-hook 'erc-after-connect
	  '(lambda (SERVER NICK)
	     (cond
	      ((string-match "swiftirc\\.net" SERVER)
	       (erc-message "PRIVMSG" "NickServ identify "))

	      ((string-match "freenode\\.net" SERVER)
	       (erc-message "PRIVMSG" "NickServ id maden ")))))
