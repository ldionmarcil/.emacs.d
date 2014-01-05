(defun load-private-conf (library)
  (let ((path "~/.config/private/elisp/"))
    (load-library (format "%s.el.gpg" (expand-file-name library path)))))
(provide 'private-conf)
