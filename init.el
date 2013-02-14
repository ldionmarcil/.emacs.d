;; -*-  eval: (folding-mode 1); -*-
;;misc _settings_
;;{{{


  ;; UTF8 support
  ;;{{{

(setq utf-translate-cjk-mode nil)
(set-language-environment 'utf-8)
(setq locale-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;;}}}

(setq find-file-visit-truename t)
(setq ls-lisp-use-insert-directory-program nil)
(setq-default truncate-lines t)
;;}}}

;;mode associations
;;{{{
(add-hook 'emacs-lisp-mode-hook 'show-paren-mode)
;;}}}

;;windows
;;{{{
(when (eq system-type 'windows-nt)
  (setq default-directory "C:/Users/maden/Documents/"))
;;}}}

;;display
;;{{{
(setq display-time-day-and-date t
      display-time-24hr-format t)
(display-time)
(menu-bar-mode -99)
(blink-cursor-mode -1)
(setq ediff-split-window-function 'split-window-horizontally)
;;}}}

;;includes
;;{{{
(add-to-list 'load-path "~/.emacs.d/elisp/")
(require 'cl)
;;(require 'ls-lisp) ;; what is this?
(require 'tramp)
(require 'smex)
(autoload 'folding-mode "folding" "Folding mode" t)
;;}}}

;;slime
;;{{{

(when (not (eq system-type 'windows-nt))
  (add-to-list 'load-path "/home/maden/Programming/slime")  ; your SLIME directory
  (setq inferior-lisp-program "/usr/bin/clisp") ; your Lisp system
  (require 'slime)
  (slime-setup))

;;}}}

;;smex
;;{{{
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
;;(global-set-key (kbd "M-x") 'smex-major-mode-commands) ;; only suggest major-mode related commands
(global-set-key (kbd "C-c M-x") 'smex-update)
(setq tramp-default-method "scp")
;;}}}

;;dired detail-simplifier mode
;;{{{
(require 'dired-details)
(dired-details-install)
;;}}}

;;erc block
;;{{{

(require 'erc-join)
(erc-autojoin-mode t)

(setq scroll-conservatively 10000000) ;;possible fix for ERC mad scrolling

(defun start-irc ()
  (interactive)
  (erc-tls :server "irc.swiftirc.net" :port 6697 :nick "ldionmarcil")
  (erc-tls :server "irc.freenode.net" :port 6697 :nick "ldionmarcil"))

(setq erc-autojoin-channels-alist
      '(("swiftirc\\.net" "#papillons" "#chaos-team")
	("freenode\\.net" "#emacs" "##linux" "#lisp" "#python" "#r_netsec" "#scheme" "#raspberrypi" "#space")))

(setq erc-timestamp-only-if-changed-flag nil
      erc-timestamp-format "%R:"
      erc-insert-timestamp-function 'erc-insert-timestamp-left
      erc-fill-prefix ""
      erc-flood-protect nil)

(add-hook 'erc-after-connect
	  '(lambda (SERVER NICK)
	     (require 'pwd)
	     (cond
	      ((string-match "swiftirc\\.net" SERVER)
	       (erc-message "PRIVMSG" (concat "NickServ identify " irc-swift-pw)))

	      ((string-match "freenode\\.net" SERVER)
	       (erc-message "PRIVMSG" (concat "NickServ id maden " irc-freenode-pw))))))



;;possible erc-scroll-fixes... 
;;{{{
;;not concluent...
(defun erc-display-buffer-list (buffer)
  "Sanitize a 'buffer' name or list, and convert to a buffer-name list."
  (cond ((bufferp buffer) (list buffer))
        ((listp buffer) buffer)
        ((processp buffer) (list (process-buffer buffer)))
        ((eq 'all buffer)
         ;; Hmm, or all of the same session server?
         (erc-buffer-list nil erc-server-process))
        ((and (eq 'active buffer) (erc-active-buffer))
         (list (erc-active-buffer)))
        ((erc-server-buffer-live-p)
         (list (process-buffer erc-server-process)))
        (t (list (current-buffer)))))

(defun erc-display-message (parsed type buffer msg &rest args)
  "Display MSG in BUFFER.

ARGS, PARSED, and TYPE are used to format MSG sensibly.

See also `erc-format-message' and `erc-display-line'."
  (let ((string (if (symbolp msg)
                    (apply 'erc-format-message msg args)
                  msg)))
    (setq string
          (cond
           ((null type)
            string)
           ((listp type)
            (mapc (lambda (type)
                    (setq string
                          (erc-display-message-highlight type string)))
                  type)
            string)
           ((symbolp type)
            (erc-display-message-highlight type string))))

    (if (not (erc-response-p parsed))
        (erc-display-line string buffer)
      (erc-put-text-property 0 (length string) 'erc-parsed parsed string)
      (erc-put-text-property 0 (length string) 'rear-sticky t string)
      (dolist (buf (erc-display-buffer-list buffer))
        (unless (member (erc-response.command parsed)
                        (with-current-buffer buf
                          erc-hide-list))
          (erc-display-line string buffer))))))

;;testing
(defun buffer-major-mode (buf)
  "Returns the of `major-mode' for buffer BUF."
  (with-current-buffer buf major-mode))

(defun buffers-with-mode (mode &optional frame)
  "Return a list of all existing live buffers whose mode is MODE.
If the optional arg FRAME is a frame, we return the buffer list in the
proper order for that frame: the buffers show in FRAME come first,
followed by the rest of the buffers."
  (remove-if-not (lambda (buf)
                   (eq mode (buffer-major-mode buf)))
                 (buffer-list frame)))

(defmacro map-buffer (exp buffer-list)
  (declare (indent 1))
  `(mapcar (lambda (buf)
             (with-current-buffer buf ,exp))
           ,buffer-list))

(defun can-make-flush-p (window)
  (and (> (window-start window)
          (point-min))
       (< 2 (empty-lines-visible window))))

(defun empty-lines-visible (&optional window)
  (max 0 (- (window-text-height window)
	    (- (line-number-at-pos (point-max))
	       (line-number-at-pos (window-start window))))))

(defun erc-idle-scroll ()
  (mapcar (lambda (buffer)
            (let ((window (first (ignore-errors (get-buffer-window-list buffer)))))
              (when (and window (can-make-flush-p window))
                (with-selected-window window
                  (erc-scroll-to-bottom)))))
          (buffers-with-mode 'erc-mode)))

(setq scroll-fudge 1)
(defun recenter-top-bottom-sticky (&optional arg)
  (interactive "P")
  (recenter-top-bottom arg)
  (and (> (line-number-at-pos (point-max))
          (window-text-height))
       (< scroll-fudge (- (window-text-height)
                          (- (line-number-at-pos (point-max))
                             (line-number-at-pos (window-start)))))
       (save-excursion (goto-char (point-max))
                       (recenter -1))))

(defun erc-idle-scroll-mode ()
  (interactive)
  (map-buffer
      (progn (set (make-local-variable 'scroll-conservatively)
                  101)
             (local-set-key (kbd "C-l") 'recenter-top-bottom-sticky))
    (buffers-with-mode 'erc-mode))
  (add-hook 'erc-mode-hook
            (lambda ()
              (set (make-local-variable 'scroll-conservatively) 101)
              (local-set-key (kbd "C-l") 'recenter-top-bottom-sticky)))
  (ignore-errors (cancel-timer scroll-erc-timer))
  (setq scroll-erc-timer (run-with-idle-timer 1 t 'erc-idle-scroll)))

(eval-after-load "erc-pcomplete"
  '(progn
     (define-key erc-mode-map (kbd "TAB") 'pcomplete)
     (defun pcomplete/erc-mode/complete-command ()
       (when (erc-button-next) (throw 'pcompleted t))
       (pcomplete-here
        (append
         (pcomplete-erc-commands)
         (pcomplete-erc-nicks erc-pcomplete-nick-postfix t))))))
;;}}}

;;}}}

;;random defuns
;;{{{
(global-set-key "\M-p" '(lambda () (interactive) (load-file user-init-file)))
(global-set-key (kbd "<f1>") 'start-irc)
(global-set-key (kbd "C-M-q") 'indent-code-rigidly)
(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)
(global-set-key (kbd "C-c i") (lambda () (interactive) (find-file user-init-file)))
(global-set-key (kbd "C-c o") 'eval-buffer)
(global-set-key (kbd "C-\\") (lambda () (interactive) (delete-region (point-min) (point-max))))
(global-set-key (kbd "C-x p") (lambda () (interactive) (other-window -1)))
(global-unset-key (kbd "C-z")) ;;unbinds the annoying minimize kbd macro

(defun xml-fix-indent (begin end)
  (interactive "r")
  (save-excursion
      (nxml-mode)
      (goto-char begin)
      (while (search-forward-regexp "\>[ \\t]*\<" nil t) 
        (backward-char) (insert "\n"))
      (indent-region begin end)))
;;}}}

;;free block
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector ["black" "#d55e00" "#009e73" "#f8ec59" "#0072b2" "#cc79a7" "#56b4e9" "white"])
 '(custom-enabled-themes (quote (deeper-blue)))
 '(delete-selection-mode nil)
 '(erc-autojoin-delay 0)
 '(erc-autojoin-mode t)
 '(erc-autojoin-timing (quote indent))
 '(erc-button-mode t)
 '(erc-dcc-mode t)
 '(erc-fill-mode t)
 '(erc-fill-prefix "")
 '(erc-input-line-position -1)
 '(erc-insert-timestamp-function (quote erc-insert-timestamp-left))
 '(erc-interpret-controls-p nil)
 '(erc-irccontrols-mode t)
 '(erc-keep-place-mode t)
 '(erc-list-mode t)
 '(erc-match-mode t)
 '(erc-menu-mode t)
 '(erc-modules (quote (autojoin button completion dcc fill irccontrols keep-place list match menu move-to-prompt netsplit networks noncommands readonly ring stamp track)))
 '(erc-move-to-prompt-mode t)
 '(erc-netsplit-mode t)
 '(erc-networks-mode t)
 '(erc-noncommands-mode t)
 '(erc-pals nil)
 '(erc-pcomplete-mode t)
 '(erc-pcomplete-nick-postfix ": ")
 '(erc-readonly-mode t)
 '(erc-ring-mode t)
 '(erc-scrolltobottom-mode t)
 '(erc-stamp-mode t)
 '(erc-timestamp-format "%R:")
 '(erc-timestamp-only-if-changed-flag nil)
 '(erc-track-exclude-server-buffer t)
 '(erc-track-minor-mode t)
 '(erc-track-mode t)
 '(erc-track-position-in-mode-line (quote before-modes))
 '(erc-track-shorten-start 4)
 '(erc-track-showcount nil)
 '(erc-user-full-name "Louis Dion-Marcil")
 '(erc-warn-about-blank-lines nil)
 '(inhibit-startup-screen t)
 '(keyboard-coding-system (quote cp1252))
 '(mark-even-if-inactive t)
 '(menu-bar-mode nil)
 '(scroll-bar-mode (quote right))
 '(selection-coding-system (quote utf-16le-dos))
 '(server-use-tcp t)
 '(tool-bar-mode nil)
 '(transient-mark-mode 1))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(erc-current-nick-face ((t (:background "red" :foreground "black" :weight bold))))
 '(erc-default-face ((t nil)))
 '(erc-nick-msg-face ((t (:background "red" :foreground "black" :weight bold))))
 '(erc-notice-face ((t (:foreground "orange red" :height 0.8))))
 '(erc-timestamp-face ((t nil))))
