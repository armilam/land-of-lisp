
(defstruct player
	health
	agility
	strength)

(defun player-move-count (player)
	(k (1+ (truncate (/ (max 0 (player-agility player)) 15)))))


(defun init-monsters (monster-num)
	(let ((monster-builders (list #'make-orc #'make-hydra #'make-slime-mold #'make-brigand)))
		(map 'vector
			(lambda (x)
				(funcall (nth (random (length monster-builders)) monster-builders)))
			(make-array monster-num))))

(defun orc-battle (monster-num)
	(let* ((monsters (init-monsters monster-num))
		   (player (make-player :health 30 :agility 30 :strength 30))
		   (won (game-loop player monsters)))
		(cond (won (fresh-line) (princ "Congratulations! You have vanquished all of your foes."))
			  (t (fresh-line) (princ "You have been killed. Game over.")))))

(defun game-loop (player monsters)
	(cond ((player-dead player)
		   nil)
		  ((monsters-dead monsters)
		   'you-win)
		  (t
		   (show-player player)
;TODO: The following line needs to "mutate" the player... functionally.
(game-loop (monsters-move (player-move player monsters (player-move-count player))))
		   (player-move player monsters (player-move-count player)))))

(defun player-move (player monsters moves)
	)

(defun player-dead (player)
  (<= (player-health player) 0))

(defun monster-dead (monster)
  (<= (monster-health monster) 0))

(defun monsters-dead (monsters)
  (every #'monster-dead monsters))

(defun show-player (player)
  (fresh-line)
  (princ "You are a valiant knight with a health of ")
  (princ (player-health player))
  (princ ", an agility of ")
  (princ (player-agility player))
  (princ ", and a strength of ")
  (princ (player-strength player)))







(defun game-loop1 ()
  (unless (or (player-dead) (monsters-dead))
;    (show-player)
    (dotimes (player-move-count)
      (unless (monsters-dead)
        (show-monsters)
        (player-attack)))
    (fresh-line)
    (map 'list
         (lambda(m)
           (or (monster-dead m) (monster-attack m)))
         *monsters*)
    (game-loop)))



(defun player-attack ()
  (fresh-line)
  (princ "Attack style: [s]tab [d]ouble swing [r]oundhouse:")
  (case (read)
    (s (monster-hit (pick-monster)
                    (+ 2 (randval (ash *player-strength* -1)))))
    (d (let ((x (randval (truncate (/ *player-strength* 6)))))
         (princ "Your double swing has a strength of ")
         (princ x)
         (fresh-line)
         (monster-hit (pick-monster) x)
         (unless (monsters-dead)
           (monster-hit (pick-monster) x))))
    (otherwise (dotimes (x (1+ (randval (truncate (/ *player-strength* 3)))))
                 (unless (monsters-dead)
                   (monster-hit (random-monster) 1))))))

(defun randval (n)
  (1+ (random (max 1 n))))

(defun random-monster ()
  (let ((m (aref *monsters* (random (length *monsters*)))))
    (if (monster-dead m)
        (random-monster)
      m)))

(defun pick-monster ()
  (fresh-line)
  (princ "Monster #:")
  (let ((x (read)))
    (if (not (and (integerp x) (>= x 1) (<= x *monster-num*)))
        (progn (princ "That is not a valid monster number.")
               (pick-monster))
      (let ((m (aref *monsters* (1- x))))
        (if (monster-dead m)
            (progn (princ "That monster is alread dead.")
                   (pick-monster))
          m)))))

(defun show-monsters ()
  (fresh-line)
  (princ "Your foes:")
  (let ((x 0))
    (map 'list
         (lambda (m)
           (fresh-line)
           (princ "   ")
           (princ (incf x))
           (princ ". ")
           (if (monster-dead m)
               (princ "**dead**")
             (progn (princ "(Health=")
                    (princ (monster-health m))
                    (princ ") ")
                    (monster-show m))))
         *monsters*)))

(defstruct monster (health (randval 10)))

(defmethod monster-hit (m x)
  (decf (monster-health m) x)
  (if (monster-dead m)
      (progn (princ "You killed the ")
             (princ (type-of m))
             (princ "! "))
    (progn (princ "You hit the ")
           (princ (type-of m))
           (princ ", knocking off ")
           (princ x)
           (princ " health points! "))))

(defmethod monster-show (m)
  (princ "A fierce ")
  (princ (type-of m)))

(defmethod monster-attack (m))

(defstruct (orc (:include monster)) (club-level (randval 8)))

(defmethod monster-show ((m orc))
  (princ "A wicked orc with a level ")
  (princ (orc-club-level m))
  (princ " club"))

(defmethod monster-attack ((m orc))
  (let ((x (randval (orc-club-level m))))
    (princ "An orc swings his club at you and knocks off ")
    (princ x)
    (princ " of your health points. ")
    (decf *player-health* x)))

(defstruct (hydra (:include monster)))

(defmethod monster-show ((m hydra))
  (princ "A malicious hydra with ")
  (princ (monster-health m))
  (princ " heads."))

(defmethod monster-hit ((m hydra) x)
  (decf (monster-health m) x)
  (if (monster-dead m)
      (princ "The corpse of the fully decapitated and decapacitated hydra
falls to the floor!")
    (progn (princ "You lop off ")
           (princ x)
           (princ " of the hydra's heads! "))))

(defmethod monster-attack ((m hydra))
  (let ((x (randval (ash (monster-health m) -1))))
    (princ "A hydra attacks you with ")
    (princ x)
    (princ " of its heads! It also grows back one more head! ")
    (incf (monster-health m))
    (decf *player-health* x)))

(defstruct (slime-mold (:include monster)) (sliminess (randval 5)))

(defmethod monster-show ((m slime-mold))
  (princ "A slime mold with a sliminess of ")
  (princ (slime-mold-sliminess m)))

(defmethod monster-attack ((m slime-mold))
  (let ((x (randval (slime-mold-sliminess m))))
    (princ "A slime mold wraps around your legs and decreases your agility
by ")
    (princ x)
    (princ "! ")
    (decf *player-agility* x)
    (when (zerop (random 2))
      (princ "It also squirts in your face, taking away a health point! ")
      (decf *player-health*))))

(defstruct (brigand (:include monster)))

(defmethod monster-attack ((m brigand))
  (let ((x (max *player-health* *player-agility* *player-strength*)))
    (cond ((= x *player-health*)
           (princ "A brigand hits you with his slingshot, taking off 2 health points! ")
           (decf *player-health* 2))
          ((= x *player-agility*)
           (princ "A brigand catches your leg with his whip, taking off 2
agility points! ")
           (decf *player-agility* 2))
          ((= x *player-strength*)
           (princ "A brigand cuts your arm with his whip, taking off 2 strength points! ")
           (decf *player-strength* 2)))))
