;; -*-  eval: (folding-mode 0); -*-

;;{{{includes

(add-to-list 'load-path "~/.emacs.d/elisp/")
(require 'cl)
(require 'tramp)
(require 'smex)
(require 'private-conf)
(require 'flyspell)
(require 'package)
(require 'org)
(autoload 'folding-mode "folding" "Folding mode" t)

;;}}}

;;{{{evil

(add-to-list 'load-path "~/.emacs.d/evil")
(global-evil-surround-mode 1)
(require 'evil)
(evil-mode 1)

(require 'general)

(general-define-key
 :states '(normal visual insert emacs)
 :prefix "SPC"
 :non-normal-prefix "M-SPC"
 "c" 'org-capture

 "p" 'previous-multiframe-window
 "o" 'next-multiframe-window
 "|" 'toggle-window-split

 "1" 'delete-other-windows
 "2" 'split-window-below
 "3" 'split-window-right

 "e" 'eval-defun

 "f" 'ido-find-file
 "s" 'save-buffer

 "b" 'ido-switch-buffer
 "q" 'previous-buffer
 "k" 'kill-buffer
 "0" 'delete-window

 "<" 'beginning-of-buffer
 ">" 'end-of-buffer)

;;}}}

;;{{{misc settings

(setq ring-bell-function 'ignore)
(global-hl-line-mode)
(put 'erase-buffer 'disabled nil)
(setq confirm-kill-emacs (lambda (interactive) (yes-or-no-p "Do you really want to exit emacs? ")))
(server-start) ;;server path is ~/.emacs.d/server/server
(global-set-key (kbd "C-=") 'maximize-window)
(global-set-key (kbd "C--") 'minimize-window)
(setq delete-selection-mode t)

;;}}}

;;{{{er/expand-region
(global-set-key (kbd "M-n") 'er/expand-region)
;;}}}

;;{{{browse-kill-ring

(require 'browse-kill-ring)
(global-set-key (kbd "M-y") 'browse-kill-ring)

;;}}}

;;{{{mode hooks

(add-hook 'emacs-lisp-mode-hook 'paredit-mode)
(add-hook 'paredit-mode-hook 'show-paren-mode)
(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)

;;}}}

;;{{{OS-specific instructions

(when (eq system-type 'windows-nt)
  (set-default 'tramp-default-method "plink")
  (setq default-directory (concat (getenv "HOME") "Documents/")))
(when (eq system-type 'gnu/linux)
  (set-frame-font "Ubuntu Mono 16"))
(when (eq system-type 'darwin)
  (setq mac-command-modifier 'control)
  (setq python-shell-interpreter "/usr/local/bin/python3")
  (setq exec-path (append exec-path '("/usr/local/bin")))
  (set-default-font "Menlo 17")
  (global-set-key (kbd "<f5>") (lambda () (interactive) (shell-command (concat "open -a "
									       (shell-quote-argument "/Applications/iTerm.app/Contents/MacOS/iTerm2")
									       " "
									       (shell-quote-argument (file-truename default-directory)))))))

;;}}}

;;{{{languages

(require 'init-c)

;; java
(add-hook 'java-mode-hook '(lambda () (setq-local parens-require-spaces nil)))

;; python
(eval-after-load "python-mode"
  '(progn
     ;; Do whatever you need to do here, it will only get executed after python-mode.el has loaded
     (load-file "~/.emacs.d/elisp/emacs-for-python/epy-init.el")
     (require 'pymacs)
     (pymacs-load "ropemacs" "rope-")
     (setq ropemacs-enable-autoimport t)
     (add-hook 'python-mode-hook '(lambda ()
				    (define-key python-mode-map (kbd "C-M-q") 'indent-buffer)
				    (setq-local parens-require-spaces nil)))))

;;}}}

;;{{{doc-view
(eval-after-load 'doc-view
  '(progn
     (define-key doc-view-mode-map (kbd "j") 'doc-view-next-page)
     (define-key doc-view-mode-map (kbd "k") 'doc-view-previous-page)))
;;}}}

;;{{{image-view
(eval-after-load 'image-mode
  '(progn
     (define-key image-mode-map (kbd "j") 'image-next-file)
     (define-key image-mode-map (kbd "k") 'image-previous-file)))
;;}}}

;;{{{ido+helm

(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t)
(ido-everywhere t)
(setq ido-create-new-buffer 'always)
(setq confirm-nonexistent-file-or-buffer nil)
(setq ido-use-virtual-buffers t)

;; (add-to-list 'load-path "~/.emacs.d/elisp/helm/")
;; (require 'helm-config)
;; (global-set-key (kbd "C-x C-f") 'helm-find-files)
;; (global-set-key (kbd "C-x b") 'helm-buffers-list)
;; (define-key helm-find-files-map (kbd "<right>") ')

;;}}}

;;{{{password manager

(add-to-list 'load-path "~/.emacs.d/elisp/elisp-password-manager/")
(require 'password-manager)
(global-set-key (kbd "<f2>") 'my-password-manager)
(defun my-password-manager ()
  (interactive)
  (load-private-conf "password-manager")
  (password-manager))

;;}}}

;;{{{rectangle mode

(require 'rect-mark)
(global-set-key (kbd "C-x r C-SPC") 'rm-set-mark)
(global-set-key (kbd "C-x r C-x")   'rm-exchange-point-and-mark)
(global-set-key (kbd "C-x r C-w")   'rm-kill-region)
(global-set-key (kbd "C-x r M-w")   'rm-kill-ring-save)

;;}}}

;;{{{multiple cursors

(require 'multiple-cursors)

;;}}}

;;{{{web-mode test

(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js-mode))
(add-to-list 'auto-mode-alist '("\\.jsp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))

;;}}}

;;{{{frame display settings

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/zenburn-emacs/")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/my-wombat/")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/emacs-deviant-theme/")
(when (eq custom-enabled-themes nil) 
  (load-theme 'Deviant t))
(setq display-time-day-and-date t
      display-time-24hr-format t
      display-time-default-load-average nil)
(display-time)
(menu-bar-mode -99)
(blink-cursor-mode -1)
(setq ediff-split-window-function 'split-window-horizontally)

;;}}}

;;{{{ace-jump-mode

(require 'ace-jump-mode)
(global-set-key (kbd "C-c C-SPC") 'ace-jump-mode)
(global-set-key (kbd "<menu>") 'ace-jump-mode)

;;}}}

;;{{{smex

(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
;;(global-set-key (kbd "M-x") 'smex-major-mode-commands) ;; only suggest major-mode related commands
(global-set-key (kbd "C-c M-x") 'smex-update)

;;}}}

;;{{{dired

(require 'dired-details)
(dired-details-install)
(add-hook 'dired-mode-hook 'auto-revert-mode) ;; auto-refresh dired on file change
(setq dired-listing-switches "-alht")
(setq dired-dwim-target t) ;;suggest copy to other dired window buffer
(put 'dired-find-alternate-file 'disabled nil)
(put 'dired-do-copy 'ido 'find-file)
(put 'dired-do-rename 'ido 'find-file)

;;}}}

;;{{{magit

					;(add-to-list 'load-path "~/.emacs.d/elisp/magit/")
(add-to-list 'load-path "~/.emacs.d/elpa/git-commit-mode-20140313.1504")
(add-to-list 'load-path "~/.emacs.d/elpa/git-rebase-mode-20140313.1504")

(require 'magit)
(prefer-coding-system 'utf-8) ;; fixes magit/issues/32

;;}}}

;;{{{registers

(if (not (boundp 'my-current-register))
  (setq my-current-register 0)) ;;initial register

(defun toggle-register-switch ()
  (interactive)
  (if (eq 1 my-current-register)
      (progn 
	(frame-configuration-to-register 1) 
	(setq my-current-register 0))
    (progn
      (frame-configuration-to-register 0)
      (setq my-current-register 1)))
  (jump-to-register my-current-register)
  (setq my-current-register-format (number-to-string my-current-register)))
(global-set-key (kbd "<f12>") 'toggle-register-switch)

;;}}}

;;{{{org-mode

(require 'org-install)
(add-hook 'org-mode-hook
          (lambda ()
	    (define-key org-mode-map (kbd "C-`") 'org-export-as-html-and-open)
	    (define-key org-mode-map (kbd "C-c a") 'org-insert-subheading)
	    (define-key org-mode-map (kbd "C-x C-a") 'org-insert-subheading)
	    (define-key org-mode-map (kbd "C-c l") 'org-store-link)
	    (define-key org-mode-map (kbd "C-`") 'org-export-as-html-and-open)))
(setq org-export-html-postamble t
      org-export-with-section-numbers nil
      org-src-fontify-natively t
      org-log-done t
      org-export-htmlize-output-type 'css
      org-todo-keywords '("TODO" "In Process" "TBD" "WONTFIX" "DONE")
      org-hide-leading-stars t)
;; (setq org-export-html-postamble-format '(("en" "<p class=\"author\">Last modification made by: %a <span style=\"font-size:12px\">(%e)</span></p>\n<p class=\"date\">Date: %d</p>\n<p class=\"creator\">%c</p>\n")))

(setq org-capture-templates
    '(("t" "Todo" entry (file "~/Documents/todo.org")
       "* TODO %?\n%U" :empty-lines 1)

      ("c" "Marker" entry (file "~/Documents/marker.org")
       "* %? %u
[[file:%F::%(with-current-buffer (org-capture-get :original-buffer) (number-to-string (line-number-at-pos)))][%F]]
#+BEGIN_SRC %(with-current-buffer (org-capture-get :original-buffer) (replace-regexp-in-string (regexp-quote \"\-mode\") \"\" (prin1-to-string major-mode) nil 'literal))
%i
#+END_SRC" :empty-lines 1)))

;;}}}

;;{{{abbrevs
(setq abbrev-file-name "~/.emacs.d/abbrev_defs")
(setq default-abbrev-mode t)
(read-abbrev-file abbrev-file-name)
;;}}}

;;{{{irc block

(add-to-list 'load-path "~/.emacs.d/elisp/circe/lisp")
(require 'circe)
(add-hook 'circe-chat-mode-hook 'my-circe-chat-mode-setup)
(defun my-circe-chat-mode-setup () 
  (setq	truncate-lines nil
	circe-format-server-topic "*** Topic change by {origin}: {topic-diff}"
	circe-channel-killed-confirmation nil
	circe-reduce-lurker-spam t
	circe-nowait-on-connect nil
	circe-default-part-message "Bye"
	circe-default-quit-message "Bye"
	lui-time-stamp-format "%H%M"
	lui-time-stamp-position 'left
	lui-fill-type nil
	lui-scroll-behavior 'post-scroll
	lui-time-stamp-only-when-changed-p nil
	lui-time-stamp-only-when-changed-p nil)
  flyspell-mode)

(defun start-irc ()
  (interactive)
  (require 'circe-color-nicks)
  (enable-circe-color-nicks)
  (setq circe-auto-query-p t
	circe-new-buffer-behavior 'display)
  (circe-set-display-handler "301" (lambda (&rest ignored))) ;; don't need away reminders
  (load-private-conf "IRC"))

;;}}}

;;{{{key bindings

(global-set-key "\M-p" '(lambda () (interactive) (load-file user-init-file)))
(global-set-key (kbd "<f1>") 'start-irc)
(global-set-key (kbd "C-M-q") 'indent-code-rigidly)
(global-set-key (kbd "S-C-<left>") (lambda () (interactive) (shrink-window-horizontally 3)))
(global-set-key (kbd "S-C-<right>") (lambda () (interactive) (enlarge-window-horizontally 3)))
(global-set-key (kbd "S-C-<down>") (lambda () (interactive) (shrink-window 2)))
(global-set-key (kbd "S-C-<up>") (lambda () (interactive) (enlarge-window 2)))
(global-set-key (kbd "C-c i") (lambda () (interactive) (find-file user-init-file)))
(global-set-key (kbd "C-c o") 'eval-buffer)
(global-set-key (kbd "C-\\") 'toggle-register-switch)
(define-key global-map (kbd "C-x p") 'previous-multiframe-window)
(define-key global-map (kbd "C-x o") 'next-multiframe-window)
(global-unset-key (kbd "C-x C-z")) ;;unbinds the annoying minimize kbd macro
(define-key global-map (kbd "<pause>") 'folding-toggle-show-hide)
(define-key global-map (kbd "C-x g") 'magit-status)
(global-set-key "\C-cz" 'goto-line)
(global-set-key (kbd "<C-tab>") 'lisp-complete-symbol)
(global-set-key (kbd "M-[") 'my-capitalize-next-char)

;;}}}

;;{{{tramp

;; useful proxy when needing to edit files with sudo:
;; simply find-file with /sudo:root@host#port:/path/
;; note: does not log in as root (no root login necessary)
(set-default 'tramp-default-proxies-alist (quote ((".*" "\\`root\\'" "/%h:"))))

;;}}}

;;{{{package archives

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))

;;}}}

;;{{{elfeed

(global-set-key (kbd "<f5>") 'start-rss)
(defun start-rss ()
  (interactive)
  (require 'elfeed)
  (load-private-conf "rss"))

;;}}}

;;{{{calc

(setq calc-group-digits t) ;; group digits

;;}}}

;;{{{experimental code and misc. funcs/configs 

(defun indent-buffer ()
      (interactive)
      (save-excursion
        (indent-region (point-min) (point-max) nil)))
;; (add-hook 'python-mode
;;           (lambda ()

;;))
(defun my-days-to-date (date)
  (interactive)
  (number-to-string
   (round
    (time-to-number-of-days (time-subtract
			     (date-to-time (format "%s EST" date))
			     (current-time))))))
(setq global-mode-string (list "" 'display-time-string
			       " [R:" 'my-current-register-format "]"))

(setq fill-flowed-disaply-column nil)

;; (require 'flymake)
;; (add-hook 'java-mode-hook 'flymake-mode-on)

;; (defun my-java-flymake-init ()
;;   (list "javac" (list (flymake-init-create-temp-buffer-copy
;;                        'flymake-create-temp-with-folder-structure))))

(defun find-overlays-specifying (prop pos) 
  (let ((overlays (overlays-at pos)) 
	found) 
    (while overlays 
      (let ((overlay (car overlays))) 
	(if (overlay-get overlay prop) 
	    (setq found (cons overlay found)))) 
      (setq overlays (cdr overlays))) 
    found))
(defun highlight-or-dehighlight-line () 
  (interactive) 
  (if (find-overlays-specifying 
       'line-highlight-overlay-marker 
       (line-beginning-position)) 
      (remove-overlays (line-beginning-position) (+ 1 (line-end-position))) 
    (let ((overlay-highlight (make-overlay 
			      (line-beginning-position) 
			      (+ 1 (line-end-position))))) 
      (overlay-put overlay-highlight 'face '(:background "DarkOrange4")) 
      (overlay-put overlay-highlight 'line-highlight-overlay-marker t))))
(global-set-key [f8] 'highlight-or-dehighlight-line)


;; (put 'ido-exit-minibuffer 'disabled nil)
(put 'downcase-region 'disabled nil)

(global-set-key (kbd "C-z") 'hangouts-start-conversation)
(defun hangouts-start-conversation (nick)
  (interactive (list
		(ido-completing-read "User: " (with-current-buffer "&bitlbee"
						(circe-channel-nicks)))))
  (with-current-buffer "&bitlbee"
    (circe-command-QUERY nick)))

(defun decode-hex-string (hex-string)
  (let ((res nil))
    (dotimes (i (/ (length hex-string) 2) (apply #'concat (reverse res)))
      (let ((hex-byte (substring hex-string (* 2 i) (* 2 (+ i 1)))))
        (push (format "%c" (string-to-number hex-byte 16)) res)))))

(defun my-capitalize-next-char ()
  "Capitalize first word as capitalize-word, but camel-case friendly."
  (interactive)
  (save-excursion (capitalize-region (point) (+ (point) 1))))

(defun chomp (str)
  "Chomp leading and tailing whitespace from STR."
  (replace-regexp-in-string (rx (or (: bos (* (any " \t\n")))
				    (: (* (any " \t\n")) eos)))
			    ""
			    str))

;;}}}

;;{{{highlights

(defun find-overlays-specifying (prop pos) 
  (let ((overlays (overlays-at pos)) 
	found) 
    (while overlays 
      (let ((overlay (car overlays))) 
	(if (overlay-get overlay prop) 
	    (setq found (cons overlay found)))) 
      (setq overlays (cdr overlays))) 
    found))
(defun highlight-or-dehighlight-line () 
  (interactive) 
  (if (find-overlays-specifying 
       'line-highlight-overlay-marker 
       (line-beginning-position)) 
      (remove-overlays (line-beginning-position) (+ 1 (line-end-position))) 
    (let ((overlay-highlight (make-overlay 
			      (line-beginning-position) 
			      (+ 1 (line-end-position))))) 
      (overlay-put overlay-highlight 'face '(:background "DarkOrange4")) 
      (overlay-put overlay-highlight 'line-highlight-overlay-marker t))))
(global-set-key [f8] 'highlight-or-dehighlight-line)

;;}}}

;;{{{infosec utils

(defun unhexify-inplace ()
  (interactive)
  (if (use-region-p)
      (let* ((boundaries (cons (region-beginning) (region-end)))
	     (encoded-content (buffer-substring-no-properties (car boundaries)
							      (cdr boundaries))))
	(delete-region (car boundaries) (cdr boundaries))
	(insert (url-unhex-string encoded-content)))))

(defun hexify-inplace ()
  (interactive)
  (if (use-region-p)
      (let* ((boundaries (cons (region-beginning) (region-end)))
	     (decoded-content (buffer-substring-no-properties (car boundaries)
							      (cdr boundaries))))
	(delete-region (car boundaries) (cdr boundaries))
	(insert (url-hexify-string decoded-content)))))

;;}}}


(defun list-non-matching-lines ()
  "Show lines *not* matching the regexp."
  (interactive)
  (let ((orig-buf (current-buffer))
        (new-buf "*List Non-matching Lines*"))
    (switch-to-buffer new-buf nil :force-same-window)
    (insert-buffer-substring orig-buf)
    (goto-char (point-min))
    (let ((inhibit-read-only t)) ; Always make the buffer editable
      (call-interactively #'flush-lines))
    (special-mode)))


;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["black" "#d55e00" "#009e73" "#f8ec59" "#0072b2" "#cc79a7" "#56b4e9" "white"])
 '(auto-revert-verbose nil)
 '(backup-directory-alist (quote (("." . "/home/ldionmarcil/.saves/"))))
 '(browse-url-text-browser "elinks")
 '(circe-auto-query-p nil)
 '(circe-default-part-message "Bye")
 '(circe-default-quit-message "Bye")
 '(circe-new-buffer-behavior (quote switch))
 '(circe-reduce-lurker-spam t)
 '(custom-safe-themes
   (quote
    ("aaffceb9b0f539b6ad6becb8e96a04f2140c8faa1de8039a343a4f1e009174fb" "b0fc95a71c0d988dbb9a147ae30b11748d87987f8f818fbff84484f6bb7892d7" "f7621073cbf2a6b593d13f06794db755d199575bf3edef7b91a522ebdb1ffc53" "1bcbd52f7c918921eff6d2fb4759149844f354db8a5487e809571a9456405a5d" "551f59aa2126c40ccee02d72db7e73c27b641c0ae7cd263af4add4c77a36768f" "fe20c1ea61a2836a5cea69963865b5b8df8c480ccaf3f11ad7f2e1f543f6c274" default)))
 '(debug-on-error nil)
 '(delete-selection-mode nil)
 '(electric-pair-mode t)
 '(find-file-visit-truename t)
 '(inhibit-startup-screen t)
 '(initial-scratch-message "")
 '(ispell-highlight-face (quote flyspell-incorrect))
 '(ispell-program-name "/usr/bin/hunspell")
 '(keyboard-coding-system (quote cp1252))
 '(ls-lisp-use-insert-directory-program nil)
 '(mark-even-if-inactive t)
 '(menu-bar-mode nil)
 '(message-log-max 500)
 '(package-selected-packages
   (quote
    (evil-surround general goto-chg undo-tree s jabber expand-region request web-mode multiple-cursors dracula-theme paredit csv-mode magit)))
 '(scroll-bar-mode nil)
 '(send-mail-function (quote smtpmail-send-it))
 '(server-use-tcp nil)
 '(setq x-select-enable-primary t)
 '(tool-bar-mode nil)
 '(tramp-default-method "ssh" nil (tramp))
 '(transient-mark-mode 1)
 '(truncate-lines nil)
 '(user-full-name "Louis Dion-Marcil")
 '(user-mail-address "maden.ldm@gmail.com"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(lui-time-stamp-face ((t (:foreground "SeaGreen" :weight bold))))
 '(widget-button ((t nil))))


(add-hook 'mpc-mode-hook
          (lambda ()
	    (define-key mpc-mode-map (kbd "C-c C-p") 'mpc-play)
	    (define-key mpc-mode-map (kbd "C-c C-s") 'mpc-pause)))
