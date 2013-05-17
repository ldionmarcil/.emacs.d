(add-hook 'c-mode-common-hook 
          (lambda ()
	    (define-key c-mode-base-map (kbd "C-c C-c") 'recompile)
	    (define-key c-mode-base-map (kbd "C-c x")
	      (lambda () (interactive)
		(shell-command "a.exe" (get-buffer-create "*C*"))))
	    (set
	     (make-local-variable 'compile-command) "gcc *.c")))

(provide 'init-c)
