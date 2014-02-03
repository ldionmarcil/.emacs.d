;;; my-wombat.el --- Custom face theme for Emacs  -*-coding: utf-8 -*-

;; Copyright (C) 2011-2014 Free Software Foundation, Inc.

;; Author: Kristoffer Grönlund <krig@koru.se>

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(deftheme my-wombat
  "Medium-contrast faces with a dark gray background.
Adapted, with permission, from a Vim color scheme by Lars H. Nielsen.
Basic, Font Lock, Isearch, Gnus, Message, and Ansi-Color faces
are included.")

(let ((class '((class color) (min-colors 89))))
  (custom-theme-set-faces
   'my-wombat
   `(default ((,class (:background "#242424" :foreground "#f6f3e8"))))
   `(cursor ((,class (:background "#656565"))))
   ;; Highlighting faces
   `(fringe ((,class (:background "#303030"))))
   `(highlight ((,class (:background "#454545" :underline nil))))
   `(region ((,class (:background "#5c5c5c"))))
   `(secondary-selection ((,class (:background "#333366"))))
   `(isearch ((,class (:background "#343434" :foreground "#857b6f"))))
   `(lazy-highlight ((,class (:background "#384048" :foreground "#a0a8b0"))))
   ;; Mode line faces
   `(mode-line ((,class (:background "#666666" :foreground "#8af2ea"))));;857b6f
   `(mode-line-inactive ((,class (:background "#444444" :foreground "#857b6f"))))
   ;; Escape and prompt faces
   `(minibuffer-prompt ((,class (:foreground "#e5786d"))))
   `(escape-glyph ((,class (:foreground "#ddaa6f" :weight bold))))
   ;; Button and link faces
   `(link ((,class (:foreground "#8ac6f2" :underline nil))))
   `(link-visited ((,class (:foreground "#e5786d" :underline nil))))
   `(button ((,class (:background "#333333" :foreground "#f6f3e8"))))
   `(header-line ((,class (:background "#303030" :foreground "#e7f6da"))))
   ;; Font lock faces
   ;; Font lock faces
   `(font-lock-builtin-face ((,class (:foreground "#e5786d"))))
   `(font-lock-constant-face ((,class (:foreground "#e5786d"))))
   `(font-lock-function-name-face ((,class (:foreground "#cae682"))))
   `(font-lock-keyword-face ((,class (:foreground "#8ac6f2" :weight bold))))
   `(font-lock-string-face ((,class (:foreground "#95e454"))))
   `(font-lock-type-face ((,class (:foreground "#92a65e" :weight bold))))
   `(font-lock-variable-name-face ((,class (:foreground "#cae682"))))
   `(font-lock-warning-face ((,class (:foreground "#ccaa8f"))))
   `(font-lock-comment-face ((t (:foreground "#7F9F7F"))))
   `(font-lock-comment-delimiter-face ((t (:foreground "#5F7F5F"))))
   `(font-lock-doc-face ((t (:foreground "#9FC59F"))))
   `(font-lock-negation-char-face ((t (:foreground "#F0DFAF" :weight bold))))
   `(font-lock-preprocessor-face ((t (:foreground "#94BFF3"))))
   `(font-lock-regexp-grouping-construct ((t (:foreground "#F0DFAF" :weight bold))))
   `(font-lock-regexp-grouping-backslash ((t (:foreground "#7F9F7F" :weight bold))))
   `(font-lock-string-face ((t (:foreground "#CC9393"))))
   ;; Message faces
   `(message-header-name ((,class (:foreground "#8ac6f2" :weight bold))))
   `(message-header-cc ((,class (:foreground "#95e454"))))
   `(message-header-other ((,class (:foreground "#95e454"))))
   `(message-header-subject ((,class (:foreground "#cae682"))))
   `(message-header-to ((,class (:foreground "#cae682"))))
   `(message-cited-text ((,class (:foreground "#99968b"))))
   `(message-separator ((,class (:foreground "#e5786d" :weight bold))))))

(custom-theme-set-variables
 'my-wombat
 '(ansi-color-names-vector ["#242424" "#e5786d" "#95e454" "#cae682"
			    "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"]))

(provide-theme 'my-wombat)

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; my-wombat-theme.el ends here
