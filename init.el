;; -*-  eval: (folding-mode 1); -*-
;;{{{includes

(add-to-list 'load-path "~/.emacs.d/elisp/")
(require 'cl)
(require 'tramp)
(require 'smex)
(autoload 'folding-mode "folding" "Folding mode" t)

;;}}}

;;{{{misc settings
(global-hl-line-mode)
(put 'erase-buffer 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)
(setq confirm-kill-emacs (lambda (interactive) (yes-or-no-p "Do you really want to exit emacs? ")))
(server-start) ;;server path is ~/.emacs.d/server/server
;;}}}

;;{{{UTF8 support

(setq utf-translate-cjk-mode nil)
(set-language-environment 'utf-8)
(setq locale-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;;}}}

;;{{{browse-kill-ring
(require 'browse-kill-ring)
(global-set-key (kbd "M-y") 'browse-kill-ring)
;;}}}

;;{{{mode hooks
(add-hook 'emacs-lisp-mode-hook 'show-paren-mode)
(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
;;}}}

;;{{{OS-specific instructions
(when (eq system-type 'windows-nt)
  (setq tramp-default-method "scp")
  (set-default 'tramp-default-method "plink")
  (setq default-directory (concat (getenv "HOME") "Documents/")))
(when (eq system-type 'gnu/linux)
  (set-default-font "Monospace 11"))
;;}}}

;;{{{languages
(require 'init-c)
;; java
(add-hook 'java-mode-hook '(lambda () (setq-local parens-require-spaces nil)))
;;}}}

;;{{{ido
(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(setq ido-create-new-buffer 'always)
(setq confirm-nonexistent-file-or-buffer nil)
(setq ido-use-virtual-buffers t)
;;}}}

;;{{{multiple cursors
(add-to-list 'load-path "~/.emacs.d/elisp/multiple-cursors-master")
(require 'multiple-cursors)
;;}}}

;;{{{web-mode test
(add-to-list 'load-path "~/.emacs.d/elisp/web-mode")
(require 'web-mode)
;;}}}

;;{{{frame display settings
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/zenburn-emacs/")
(when (eq custom-enabled-themes nil) 
  (load-theme 'zenburn t)
  (message "Theme enabled"))
(setq display-time-day-and-date t
      display-time-24hr-format t)
(display-time)
(menu-bar-mode -99)
(blink-cursor-mode -1)
(setq ediff-split-window-function 'split-window-horizontally)
;;}}}

;;{{{ace-jump-mode
(require 'ace-jump-mode)
(global-set-key (kbd "C-c C-SPC") 'ace-jump-mode)
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
      org-todo-keywords '("TODO" "In Process" "DONE"))
;; (setq org-export-html-postamble-format '(("en" "<p class=\"author\">Last modification made by: %a <span style=\"font-size:12px\">(%e)</span></p>\n<p class=\"date\">Date: %d</p>\n<p class=\"creator\">%c</p>\n")))
;;}}}

;;{{{irc block

;; (add-to-list 'load-path "~/.emacs.d/elisp/circe/lisp")
;; (require 'circe)
;; (require 'circe-lagmon) ;; do i need to enable this?
(setq circe-format-server-topic "*** Topic change by {origin}: {topic-diff}"
      circe-channel-killed-confirmation nil
      circe-reduce-lurker-spam t
      circe-nowait-on-connect nil
      circe-new-buffer-behavior 'switch)
(defun start-irc ()
  (interactive)
  (require 'pwd)
  (setq circe-network-options
	`(("freenode"
	   :nick "ldionmarcil"
	   :realname "Louis Dion-Marcil"
	   :nickserv-nick ,irc-freenode-nickserv-nick
	   :channels ("#emacs" "##linux" "#lisp" "#nsec" "#r_netsec" "#polymtl" "#dci" "#archlinux" "#archlinux-arm" :after-auth "#python" "#raspberrypi")
	   :nickserv-password ,irc-freenode-pwd
	   :port 7000
	   :tls t
	   :reduce-lurker-spam t)
	  ))
  (circe "freenode"))

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
(global-set-key (kbd "C-\\") 'erase-buffer)
(define-key global-map (kbd "C-x p") 'previous-multiframe-window)
(define-key global-map (kbd "C-x o") 'next-multiframe-window)
(global-unset-key (kbd "C-z")) ;;unbinds the annoying minimize kbd macro
(global-unset-key (kbd "C-x C-z")) ;;unbinds the annoying minimize kbd macro
(define-key global-map (kbd "<pause>") 'folding-toggle-show-hide)
(global-set-key "\C-cz" 'goto-line)
(global-set-key (kbd "<C-tab>") 'lisp-complete-symbol)
;;}}}

;;{{{tramp

;; useful proxy when needing to edit files with sudo:
;; simply find-file with /sudo:root@host#port:/path/
;; note: does not log in as root (no root login necessary)
(set-default 'tramp-default-proxies-alist (quote ((".*" "\\`root\\'" "/%h:"))))

;;}}}

;;{{{package archives
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))
;;}}}

;;{{{experimental code and misc. funcs/configs 

(defun my-days-to-date (date)
  (interactive)
  (number-to-string
   (round
    (time-to-number-of-days (time-subtract
			     (date-to-time (format "%s EST" date))
			     (current-time))))))
(setq global-mode-string (list "" 'display-time-string
			       " [R:" 'my-current-register-format "]"))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector ["black" "#d55e00" "#009e73" "#f8ec59" "#0072b2" "#cc79a7" "#56b4e9" "white"])
 '(browse-url-text-browser "elinks")
 '(debug-on-error nil)
 '(delete-selection-mode nil)
 '(display-time-default-load-average nil)
 '(find-file-visit-truename t)
 '(inhibit-startup-screen t)
 '(initial-scratch-message "")
 '(keyboard-coding-system (quote cp1252))
 '(ls-lisp-use-insert-directory-program nil)
 '(mark-even-if-inactive t)
 '(menu-bar-mode nil)
 '(message-log-max 500)
 '(scroll-bar-mode (quote right))
 '(selection-coding-system (quote utf-16le-dos))
 '(server-use-tcp t)
 '(tool-bar-mode nil)
 '(transient-mark-mode 1)
 '(truncate-lines t)
 '(user-mail-address "louis.dionmarcil@gmail.com"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(widget-button ((t nil))))

(setq fill-flowed-disaply-column nil)

(add-hook 'gnus-article-mode-hook
	  (lambda ()
	    (setq
	     truncate-lines nil
	     world-wrap t)))
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
  p  (interactive)                                                                                                            
  (if (find-overlays-specifying                                                                                            
       'line-highlight-overlay-marker                                                                                      
       (line-beginning-position))                                                                                          
      (remove-overlays (line-beginning-position) (+ 1 (line-end-position)))                                                
    (let ((overlay-highlight (make-overlay                                                                                 
			      (line-beginning-position)                                                                    
			      (+ 1 (line-end-position)))))                                                                 
      (overlay-put overlay-highlight 'face '(:background "lightcoral"))                                                  
      (overlay-put overlay-highlight 'line-highlight-overlay-marker t))))
(global-set-key [f8] 'highlight-or-dehighlight-line)


(put 'ido-exit-minibuffer 'disabled nil)
(put 'downcase-region 'disabled nil)
(require 'rect-mark)
(global-set-key (kbd "C-x r C-SPC") 'rm-set-mark)
(global-set-key (kbd "C-x r C-x")   'rm-exchange-point-and-mark)
(global-set-key (kbd "C-x r C-w")   'rm-kill-region)
(global-set-key (kbd "C-x r M-w")   'rm-kill-ring-save)
(add-to-list 'load-path "~/.emacs.d/elisp/magit")
(require 'magit)

;;}}}

