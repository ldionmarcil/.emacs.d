(require 'url)

(defconst rs-hiscore-url "http://services.runescape.com/m=hiscore_oldschool/hiscorepersonal.ws?user1="
  "URL used to pull the data off the hiscores")

(defvar rs-single-skill-format "%s level for %s
Level: %s
Exp : %s
Rank: %s")

(defvar rs-skill-list
  (list "Overall" "Attack" "Defence" "Strength" "Hitpoints" "Ranged" "Prayer" "Magic" "Cooking" "Woodcutting" "Fletching" "Fishing" "Firemaking" "Crafting" "Smithing" "Mining" "Herblore" "Agility" "Thieving" "Slayer" "Farming" "Runecraft" "Hunter" "Construction"))

(defun rs-pull-stats ()
  "Pulls the HTML response from the highscores"
  (interactive)
  (let ((username (read-from-minibuffer "Username: "))
	(skill (ido-completing-read "Skill: " rs-skill-list)))
    (url-retrieve (format "%s%s" rs-hiscore-url username) 'rs-pull-cb (list username skill))))


(defun rs-pull-cb (status username skill)
  (goto-char url-http-end-of-headers)
  (let ((stats))
    (while (re-search-forward ">
\\([a-zA-Z]+\\)
</a></td>
<td align=\\\"right\\\">\\([0-9,]*\\)</td>
<td align=\\\"right\\\">\\([0-9,]*\\)</td>
<td align=\\\"right\\\">\\([0-9,]*\\)</td>" nil t)
      (add-to-list 'stats
		   (apply 'rs-set-skill (loop for x to 3
					      collect (buffer-substring
						       (match-beginning (+ x 1))
						       (match-end (+ x 1)))))))
    (rs-format-stats username stats skill)))


(defun rs-format-stats (username stats skill)
  (interactive)
  (let ((requested-skill-data (assoc skill stats)))
    (message (format rs-single-skill-format
		     skill
		     username
		     (cdr (assoc 'level requested-skill-data))
		     (cdr (assoc 'exp requested-skill-data))
		     (cdr (assoc 'rank requested-skill-data))))))

(defun rs-set-skill (name rank level exp)
  (cons name (list 
	      (cons 'rank rank)
	      (cons 'level level)
	      (cons 'exp exp))))

;;("Attack" (rank . "32,431") (level . "60") (exp . "273,787"))

;; ">
;; \\([a-zA-Z]+\\)
;; <"             
 
;;(list "Overall" "Attack" "Defence" "Strength" "Hitpoints" "Ranged" "Prayer" "Magic" "Cooking" "Woodcutting" "Fletching" "Fishing" "Firemaking" "Crafting" "Smithing" "Mining" "Herblore" "Agility" "Thieving" "Slayer" "Farming" "Runecraft" "Hunter" "Construction")
