
;PLAYER

(defstruct player
	health
	agility
	strength)

(defun player-move-count (player)
	(1+ (truncate (/ (max 0 (player-agility player)) 15))))


(defun init-monsters (monster-num)
	(let ((monster-builders (list #'make-orc #'make-hydra #'make-slime-mold #'make-brigand)))
		(map 'vector
			(lambda (x)
				(funcall (nth (random (length monster-builders)) monster-builders)))
			(make-array monster-num))))

(defun orc-battle (monster-num)
	(let* ((monsters (init-monsters monster-num))
		   (player (make-player :health 30 :agility 30 :strength 30))
		   (won (game-loop (make-game-state :player player :monsters monsters))))
		(cond (won (fresh-line) (princ "Congratulations! You have vanquished all of your foes."))
			  (t (fresh-line) (princ "You have been killed. Game over.")))))


;GAME STATE

(defstruct game-state
	player
	monsters)

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


;MONSTERS

(defstruct monster (health (randval 10)))

(defstruct (brigand (:include monster)))

(defmethod monster-show (m)
  (princ "A fierce ")
  (princ (type-of m)))

(defstruct (orc (:include monster)) (club-level (randval 8)))

(defmethod monster-show ((m orc))
  (princ "A wicked orc with a level ")
  (princ (orc-club-level m))
  (princ " club"))

(defstruct (hydra (:include monster)))

(defmethod monster-show ((m hydra))
  (princ "A malicious hydra with ")
  (princ (monster-health m))
  (princ " heads."))

(defstruct (slime-mold (:include monster)) (sliminess (randval 5)))

(defmethod monster-show ((m slime-mold))
  (princ "A slime mold with a sliminess of ")
  (princ (slime-mold-sliminess m)))

(defun show-monsters (monsters)
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
         monsters)))


;GAMEPLAY

(defun randval (n)
  (1+ (random (max 1 n))))

(defun random-monster (monsters)
  (let ((m (aref monsters (random (length monsters)))))
    (if (monster-dead m)
        (random-monster monsters)
      m)))

;TODO: Instead of decf, return a new monster object with x less health
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

;TODO: Instead of decf, return a new monster object with x less health
(defmethod monster-hit ((m hydra) x)
  (decf (monster-health m) x)
  (if (monster-dead m)
      (princ "The corpse of the fully decapitated and decapacitated hydra falls to the floor!")
    (progn (princ "You lop off ")
           (princ x)
           (princ " of the hydra's heads! "))))

(defun pick-monster (monsters)
  (fresh-line)
  (princ "Monster #:")
  (let ((x (read)))
    (if (not (and (integerp x) (>= x 1) (<= x (length monsters))))
        (progn (princ "That is not a valid monster number.")
               (pick-monster monsters))
      (let ((m (aref monsters (1- x))))
        (if (monster-dead m)
            (progn (princ "That monster is alread dead.")
                   (pick-monster monsters))
          m)))))

;TODO: Must return a game-state
(defun player-attack ((state game-state))
	(let ((player (game-state-player state))
		  (monsters (game-state-monsters state)))
		(fresh-line)
		(princ "Attack style: [s]tab [d]ouble swing [r]oundhouse:")
		(case (read)
			(s (monster-hit (pick-monster monsters)
							(+ 2 (randval (ash (player-strength player) -1)))))
			(d (let ((x (randval (truncate (/ (player-strength player) 6)))))
				(princ "Your double swing has a strength of ")
				(princ x)
				(fresh-line)
				(monster-hit (pick-monster monsters) x)
				(unless (monsters-dead monsters)
						(monster-hit (pick-monster monsters) x))))
			(otherwise (dotimes (x (1+ (randval (truncate (/ (player-strength player) 3)))))
				(unless (monsters-dead monsters)
						(monster-hit (random-monster monsters) 1)))))))

(defun player-move ((state game-state) move-count)
	(let ((player (game-state-player state))
		  (monsters (game-state-monsters state)))
		(cond ((monsters-dead monsters) state)
			  (t (show-monsters monsters)
			  	 (player-attack state)))))

;TODO: MUST RETURN A game-state
(defun monsters-move ((state game-state))
	state)

(defun game-loop ((state game-state))
	(let ((player (game-state-player state))
		  (monsters (game-state-monsters state)))
		(cond ((player-dead player)
			   nil)
			  ((monsters-dead monsters)
			   'you-win)
			  (t
			   (show-player player)
			   (game-loop (monsters-move (player-move state (player-move-count player))))))))







(defun game-loop1 ()
  (unless (or (player-dead) (monsters-dead))
;    (show-player)
    (dotimes (k (player-move-count *player*))
      (unless (monsters-dead)
        (show-monsters)
        (player-attack)))
    (fresh-line)
    (map 'list
         (lambda(m)
           (or (monster-dead m) (monster-attack m)))
         *monsters*)
    (game-loop)))





(defmethod monster-attack (m))

(defmethod monster-attack ((m orc))
  (let ((x (randval (orc-club-level m))))
    (princ "An orc swings his club at you and knocks off ")
    (princ x)
    (princ " of your health points. ")
    (decf *player-health* x)))

(defmethod monster-attack ((m hydra))
  (let ((x (randval (ash (monster-health m) -1))))
    (princ "A hydra attacks you with ")
    (princ x)
    (princ " of its heads! It also grows back one more head! ")
    (incf (monster-health m))
    (decf *player-health* x)))

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

