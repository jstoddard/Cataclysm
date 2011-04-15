;;;;---------------------------------------------------------------------------
;;;; cataclysm.lisp
;;;; An adventure game based in a dystopian future
;;;; Copyright (C) 2011 Jeremiah Stoddard
;;;;
;;;;     This program is free software: you can redistribute it and/or modify
;;;;     it under the terms of the GNU General Public License as published by
;;;;     the Free Software Foundation, either version 3 of the License, or
;;;;     (at your option) any later version.
;;;;
;;;;     This program is distributed in the hope that it will be useful,
;;;;     but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;;     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;;     GNU General Public License for more details.
;;;;
;;;;     You should have received a copy of the GNU General Public License
;;;;     along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;;---------------------------------------------------------------------------

(in-package #:cataclysm)

(defparameter *screen-width* 640)
(defparameter *screen-height* 480)
(defparameter *data-directory* "/home/jeremiah/src/lisp/cataclysm/")

(defparameter *player-hp* 15)
(defparameter *player-shells* 100)
(defparameter *player-dir* 'down)
(defparameter *player-step* 0)
(defparameter *game-counter* 0)
(defparameter *player-keys* 0)
(defparameter *player-won* nil)
(defparameter *message* nil)

;;; These contain the details of the player's position and the current
;;; world map. Each map file will set its own default values at loadtime.
;;; Tileset should be a 640x480 png file, and tiles will be read in as
;;; numbers according to their position, e.g., 0 will be the tile starting
;;; at 0,0; 1 will be the tile at 32,0; 2 will be the tile at 64,0; 20 will
;;; be the tile at 0,32; and so forth. These numbers will be used by
;;; *floor-map*, *wall-map*, *obstacle-map*, and *object-map*, which are
;;; two dimensional arrays of *map-width*, *map-height* size. *portals*
;;; is also a two dimensional array of the same size, containing strings
;;; of the map filename to be loaded when a player steps on the
;;; corresponding tile (usually "" since most tiles aren't portals).
(defvar *tileset* "images/sprites.png")
(defvar *map-width* 20)
(defvar *map-height* 15)
(defvar *player-x* 0)
(defvar *player-y* 0)
(defvar *floor-map*)
(defvar *wall-map*)
(defvar *obstacle-map*)
(defvar *object-map*)
(defvar *portals*)
(defvar *messages*)

(defvar *enemies* '()) ; enemy positions, health, etc.
(defvar *projectiles* '()) ; projectile positions, directions, ttl

(defvar *tiles*) ; This contains the current tileset as an sdl surface
(defvar *characters*) ; This contains the character sprites
(defvar *projectile*) ; This contains the bullet

(defun load-characters ()
  "Load 'images/characters.png' into *characters*. We can't do it at startup
b/c we need lispbuilder-sdl to be started, so we do it in this function which
is called before the main game loop is run."
  (setf *characters* (sdl:convert-to-display-format
		      :surface (sdl:load-image
				(concatenate 'string *data-directory*
					     "images/characters.png")
				:image-type :ping)
		      :pixel-alpha t)))

(defun load-projectile ()
  "Load 'images/projectile.png' into *projectile*."
  (setf *projectile* (sdl:convert-to-display-format
		      :surface (sdl:load-image
				(concatenate 'string *data-directory*
					     "images/projectile.png")
				:image-type :ping)
		      :pixel-alpha t)))

(defun load-map (map-file)
  "Load the map given by map-file, and load the png image of the map's tileset
into *tiles*. Returns sdl surface of the map's tileset."
  (load (concatenate 'string *data-directory* map-file))
  (setf *tiles* (sdl:convert-to-display-format
		 :surface (sdl:load-image
			   (concatenate 'string *data-directory* *tileset*)
			   :image-type :png)
		 :pixel-alpha t)))
		 
(defun draw-tile (tile-set tile-number x y)
  "Draw the tile given by tile-number to the display surface at x, y.
tile-number should be an integer between 0 and 299, inclusive."
  (multiple-value-bind (tile-y tile-x) (floor tile-number 20)
    (sdl:set-cell (sdl:rectangle :x(* tile-x 32)
				 :y (* tile-y 32)
				 :w 32 :h 32)
		  :surface tile-set)
    (sdl:draw-surface-at tile-set (sdl:point :x x :y y))))

(defun draw-status ()
  "Draw the player's life and firepower on the screen."
  (let ((output-string (concatenate 'string "HP: "
					(write-to-string *player-hp*)
					"     FIREPOWER: "
					(write-to-string *player-shells*)
					"     KEYS: "
					(write-to-string *player-keys*))))
    (sdl:draw-string-solid-* output-string 3 3 :color sdl:*black*)
    (sdl:draw-string-solid-* output-string 2 2 :color sdl:*white*)))

(defun word-wrap (string num-chars)
  "Add newlines to string to keep each line <= num-chars."
  (let ((string (remove #\Newline string)))
    (cond ((<= (length string) num-chars) string)
          (t
           (let ((string1 (subseq string 0 num-chars))
                 (string2 (subseq string num-chars)))
             (setf string1 (substitute #\Newline #\Space string1
                                       :count 1 :from-end t))
             (setf string2 (concatenate
                            'string
                            (subseq string1 (or (position #\Newline string1)
                                                num-chars))
                            string2))
             (setf string1 (subseq string1 0 (1+ (or (position #\Newline string1)
                                                     (1- num-chars)))))
             (concatenate 'string string1 (word-wrap string2 num-chars)))))))

(defun draw-message ()
  "Draw the message in *message* on the screen."
  (let* ((message (word-wrap *message* 37))
	 (lines (1+ (count #\Newline message)))
	 (y-pos (- (floor *screen-height* 2) (* 5 lines)))
	 (start 0))
    (sdl:draw-box (sdl:rectangle-from-midpoint-*
		   (floor *screen-width* 2) (floor *screen-height* 2)
		   320 200)
		  :color sdl:*blue*)
    (dotimes (i lines)
      (sdl:draw-string-solid-* (subseq message start
				       (position #\Newline message :start start))
			       (- (floor *screen-width* 2) 150) y-pos)
      (setf start (1+ (or (position #\Newline message :start start) 0)))
      (incf y-pos 10))
    (sdl:draw-string-solid-* "Press ENTER..."
			     (+ (floor *screen-width* 2) 40)
			     (+ (floor *screen-height* 2) 90))))

(defun display-map ()
  "Place tiles from the map on the display surface so that player is centered
on the screen."
  (let ((top-x (- *player-x* (floor *screen-width* 2)))
	(top-y (- *player-y* (floor *screen-height* 2)))
	start-x start-y tile-x tile-y)
    (sdl:clear-display sdl:*black*)
    (setf start-x (- 0 (mod top-x 32)))
    (setf start-y (- 0 (mod top-y 32)))
    (loop for x upfrom start-x by 32
       when (>= x *screen-width*) return nil
       do (loop for y upfrom start-y by 32
	     when (>= y *screen-height*) return nil
	     when (and (>= (+ top-x x) 0) (>= (+ top-y y) 0))
	     do (progn
		  (setf tile-x (floor (+ top-x x) 32)
			tile-y (floor (+ top-y y) 32))
		  (when (and (< tile-x *map-width*)
			     (< tile-y *map-height*))		    
		    (draw-tile *tiles* (aref *floor-map* tile-x tile-y) x y)
		    (when (<= 0 (aref *wall-map* tile-x tile-y))
		      (draw-tile *tiles* (aref *wall-map* tile-x tile-y) x y))
		    (when (<= 0 (aref *obstacle-map* tile-x tile-y))
		      (draw-tile *tiles* (aref *obstacle-map* tile-x tile-y) x y))
		    (when (<= 0 (aref *object-map* tile-x tile-y))
		      (draw-tile *tiles* (aref *object-map* tile-x tile-y) x y))))))
    (draw-tile *characters* 0
	       (- (floor *screen-width* 2) 16)
	       (- (floor *screen-height* 2) 16))))

(defun detect-projectile-collision (projectile)
  (dolist (enemy *enemies*)
    (when (and (> (first projectile) (- (first enemy) 16))
	       (< (first projectile) (+ (first enemy) 16))
	       (> (second projectile) (- (second enemy) 16))
	       (< (second projectile) (+ (second enemy) 16)))
      (decf (sixth enemy))
      (setf (fourth projectile) 0)))
  (when (and (> (first projectile) (- *player-x* 16))
	     (< (first projectile) (+ *player-x* 16))
	     (> (second projectile) (- *player-y* 16))
	     (< (second projectile) (+ *player-y* 16)))
    (decf *player-hp*)
    (setf (fourth projectile) 0)))

(defun update-projectiles ()
  "Loop through the projectiles, updating position, looking for collisions, and
finally remove old and collided projectiles."
  (dolist (projectile *projectiles*)
    (decf (fourth projectile))
    (cond ((eq (third projectile) 'up)
	   (decf (second projectile) 3))
	  ((eq (third projectile) 'down)
	   (incf (second projectile) 3))
	  ((eq (third projectile) 'left)
	   (decf (first projectile) 3))
	  ((eq (third projectile) 'right)
	   (incf (first projectile) 3)))
    (detect-projectile-collision projectile)
    (sdl:draw-surface-at *projectile*
			 (sdl:point :x (+ (- (first projectile) 16 *player-x*)
					  (floor *screen-width* 2))
				    :y (+ (- (second projectile) 16 *player-y*)
					  (floor *screen-height* 2)))))
  (setf *projectiles* (remove-if-not #'(lambda (projectile)
					 (> (fourth projectile) 0))
				     *projectiles*)))

(defun new-projectile (x y dir)
  (let (ttl)
    (cond ((eq dir 'up)
	   (setf ttl (floor (+ (floor *screen-height* 2) (- y *player-y*)) 3)))
	  ((eq dir 'down)
	   (setf ttl (floor (+ (floor *screen-height* 2) (- *player-y* y)) 3)))
	  ((eq dir 'left)
	   (setf ttl (floor (+ (floor *screen-width* 2) (- x *player-x*)) 3)))
	  ((eq dir 'right)
	   (setf ttl (floor (+ (floor *screen-width* 2) (- *player-x* x)) 3))))
    (when (null ttl) (setf ttl 0))
    (push (list x y dir ttl) *projectiles*)))

(defun enemy-up (enemy)
  "If enemy is not at the top of the map or against an obstacle, move enemy up.
Otherwise, change enemy direction to right."
  (setf (seventh enemy) (mod (1+ (seventh enemy)) 128))
  (decf (second enemy) 2)
  (let ((tile-x (floor (first enemy) 32))
	(tile-y (truncate (- (second enemy) 16) 32)))
    (unless (and (= -1 (aref *wall-map* tile-x tile-y))
		 (= -1 (aref *obstacle-map* tile-x tile-y)))
      (incf (second enemy) 2)
      (setf (third enemy) 'right)))
  (when (< (second enemy) 16)
    (setf (second enemy) 16)))

(defun enemy-down (enemy)
  "If enemy is not at the bottom of the map or against an obstacle, move enemy
down. Otherwise, change enemy direction to left."
  (setf (seventh enemy) (mod (1+ (seventh enemy)) 128))
  (incf (second enemy) 2)
  (let ((tile-x (floor (first enemy) 32))
	(tile-y (min (floor (+ (second enemy) 16) 32) (1- *map-height*))))
    (unless (and (= -1 (aref *wall-map* tile-x tile-y))
		 (= -1 (aref *obstacle-map* tile-x tile-y)))
      (decf (second enemy) 2)
      (setf (third enemy) 'left)))
  (when (> (second enemy) (- (* 32 *map-height*) 16))
    (setf (second enemy) (- (* 32 *map-height*) 16))))

(defun enemy-left (enemy)
  "If enemy is not at the left of the map or against an obstacle, move enemy left.
Otherwise, change enemy direction to up."
  (setf (seventh enemy) (mod (1+ (seventh enemy)) 128))
  (decf (first enemy) 2)
  (let ((tile-x (truncate (- (first enemy) 16) 32))
	(tile-y (floor (second enemy) 32)))
    (unless (and (= -1 (aref *wall-map* tile-x tile-y))
		 (= -1 (aref *obstacle-map* tile-x tile-y)))
      (incf (first enemy) 2)
      (setf (third enemy) 'up)))
  (when (< (first enemy) 16)
    (setf (first enemy) 16)))

(defun enemy-right (enemy)
  "If enemy is not at the right of the map or against an obstacle, move enemy
to the right. Otherwise, change enemy direction to down."
  (setf (seventh enemy) (mod (1+ (seventh enemy)) 128))
  (incf (first enemy) 2)
  (let ((tile-x (min (floor (+ (first enemy) 16) 32) (1- *map-width*)))
	(tile-y (floor (second enemy) 32)))
    (unless (and (= -1 (aref *wall-map* tile-x tile-y))
		 (= -1 (aref *obstacle-map* tile-x tile-y)))
      (decf (first enemy) 2)
      (setf (third enemy) 'down)))
  (when (> (first enemy) (- (* 32 *map-width*) 16))
    (setf (first enemy) (- (* 32 *map-width*) 16))))


(defun enemy-shoot(enemy)
  "Create a new projectile based on which direction the enemy is facing."
  (let ((x (first enemy)) (y (second enemy)))
    (cond ((eq (third enemy) 'up) (decf y 16))
	  ((eq (third enemy) 'down) (incf y 16))
	  ((eq (third enemy) 'left) (decf x 16))
	  ((eq (third enemy) 'right) (incf x 16)))
    (new-projectile x y (third enemy))))

(defun update-enemies ()
  "Loop through the enemies, updating position, removing dead guys."
  (dolist (enemy *enemies*)
    (when (and (< (abs (- (first enemy) *player-x*)) (floor *screen-width* 2))
	       (< (abs (- (second enemy) *player-y*)) (floor *screen-height* 2)))
      (if (fourth enemy)
	  (progn (setf (seventh enemy) (mod (1+ (seventh enemy)) 128))
		 (cond ((eq (third enemy) 'up) (enemy-up enemy))
		       ((eq (third enemy) 'down) (enemy-down enemy))
		       ((eq (third enemy) 'left) (enemy-left enemy))
		       ((eq (third enemy) 'right) (enemy-right enemy)))
		 (when (> (random 100) 80) (setf (fourth enemy) nil)))
	  (when (> (random 100) 80) (setf (fourth enemy) t)))
      (when (> (random 100) 80)
	(let ((new-dir (random 4)))
	  (cond ((= new-dir 0) (setf (third enemy) 'up))
		((= new-dir 1) (setf (third enemy) 'down))
		((= new-dir 2) (setf (third enemy) 'left))
		((= new-dir 3) (setf (third enemy) 'right)))))
      (when (and (< (abs (- (first enemy) *player-x*)) 16)
		 (< (abs (- (second enemy) *player-y*)) 16))
	(decf *player-hp*)
	(cond ((< (first enemy) *player-x*) (player-right) (player-right))
	      ((> (first enemy) *player-x*) (player-left) (player-left))
	      ((< (second enemy) *player-y*) (player-down) (player-down))
	      ((> (second enemy) *player-y*) (player-up) (player-up))))
      (when (> (random 100) 98)
	(enemy-shoot enemy))
      (draw-tile *characters* (fifth enemy)
		 (+ (- (first enemy) 16 *player-x*) (floor *screen-width* 2))
		 (+ (- (second enemy) 16 *player-y*)
					  (floor *screen-height* 2)))))
  (setf *enemies* (remove-if-not #'(lambda (enemy) (> (sixth enemy) 0))
				 *enemies*)))

(defun get-object (x y)
  "Remove object at tile x, y. Apply its effects to the player."
  (let ((object (mod (aref *object-map* x y) 20)))
    (setf (aref *object-map* x y) -1)
    (cond
      ((= object 0) (incf *player-keys*))
      ((= object 1) (incf *player-shells* 20))
      ((= object 2) (incf *player-hp*))
      ((= object 3) (incf *player-hp* 2))
      ((= object 4) (incf *player-hp* 5))
      ((= object 5) (setf *player-hp* 15))
      ((= object 19) (setf *player-won* t)))
    (when (> *player-hp* 15)
      (setf *player-hp* 15))))

(defun player-collision-detect (tile-x tile-y)
  "Takes location of a tile and looks up any walls or obstacles. If obstacle
map has 0 (mod 20) and player has a key, decrement *player-keys* and remove.
If *portals* is non-nil, load the new map. If object map does not contain -1
for this tile, call get-object to act on the object. Return t if there is a
wall or obstacle in this tile, or nil otherwise."
  (setf *message* (aref *messages* tile-x tile-y))
  (when (and (= 0 (mod (aref *obstacle-map* tile-x tile-y) 20))
	     (> *player-keys* 0))
    (decf *player-keys*)
    (setf (aref *obstacle-map* tile-x tile-y) -1))
  (when (aref *portals* tile-x tile-y)
    (load-map (aref *portals* tile-x tile-y))
    (return-from player-collision-detect t))
  (unless (= -1 (aref *object-map* tile-x tile-y))
    (get-object tile-x tile-y))
  (cond ((and (= -1 (aref *wall-map* tile-x tile-y))
	      (= -1 (aref *obstacle-map* tile-x tile-y))) nil)
	(t t)))

(defun player-up ()
  "If player is not at top of map or just below an obstacle, move player
upwards. Otherwise do nothing."
  (setf *player-step* (mod (1+ *player-step*) 128))
  (setf *player-dir* 'up)
  (decf *player-y* 2)
  (let ((tile-x (floor *player-x* 32))
	(tile-y (truncate (- *player-y* 16) 32)))
    (when (player-collision-detect tile-x tile-y)
      (incf *player-y* 2)))
  (when (< *player-y* 16)
    (setf *player-y* 16)))

(defun player-down ()
  "If player is not at bottom of map or just above an obstacle, move player
downwards. Otherwise do nothing."
  (setf *player-step* (mod (1+ *player-step*) 128))
  (setf *player-dir* 'down)
  (incf *player-y* 2)
  (let ((tile-x (floor *player-x* 32))
	(tile-y (min (floor (+ *player-y* 16) 32) (1- *map-height*))))
    (when (player-collision-detect tile-x tile-y)
      (decf *player-y* 2)))
  (when (> *player-y* (- (* 32 *map-height*) 16))
    (setf *player-y* (- (* 32 *map-height*) 16))))

(defun player-left ()
  "If player is not at far left of map or just right of an obstacle, move
player left. Otherwise do nothing."
  (setf *player-step* (mod (1+ *player-step*) 128))
  (setf *player-dir* 'left)
  (decf *player-x* 2)
  (let ((tile-x (truncate (- *player-x* 16) 32))
	(tile-y (floor *player-y* 32)))
    (when (player-collision-detect tile-x tile-y)
      (incf *player-x* 2)))
  (when (< *player-x* 16)
    (setf *player-x* 16)))

(defun player-right ()
  "If player is not at far right of map or just left of an obstacle, move
player right. Otherwise do nothing."
  (setf *player-step* (mod (1+ *player-step*) 128))
  (setf *player-dir* 'right)
  (incf *player-x* 2)
  (let ((tile-x (min (floor (+ *player-x* 16) 32) (1- *map-width*)))
	(tile-y (floor *player-y* 32)))
    (when (player-collision-detect tile-x tile-y)
      (decf *player-x* 2)))
  (when (> *player-x* (- (* 32 *map-width*) 16))
    (setf *player-x* (- (* 32 *map-width*) 16))))

(defun player-shoot ()
  "Create a new projectile based on which direction the player is facing."
  (when (> *player-shells* 0)
    (cond
      ((eq *player-dir* 'up)
       (new-projectile *player-x* (- *player-y* 16) 'up))
      ((eq *player-dir* 'down)
       (new-projectile *player-x* (+ *player-y* 16) 'down))
      ((eq *player-dir* 'left)
       (new-projectile (- *player-x* 16) *player-y* 'left))
      ((eq *player-dir* 'right)
       (new-projectile (+ *player-x* 16) *player-y* 'right)))
    (decf *player-shells*)))

(defun main ()
  (setf *player-hp* 15 *player-shells* 100 *player-keys* 0 *player-won* nil)
  (sdl:with-init ()
    (sdl:window *screen-width* *screen-height*
		:double-buffer t 
		:title-caption "Cataclysm")
    (setf (sdl:frame-rate) 30)
    (sdl:initialise-default-font)
    (load-characters)
    (load-projectile)
    (load-map "overworld.map")
    (sdl:with-events ()
      (:quit-event () t)
      (:key-down-event (:key key)
	(cond ((sdl:key= key :sdl-key-escape) (sdl:push-quit-event))))
      (:idle ()
	(cond
	  (*message*
	   (draw-message)
	   (when (sdl:get-key-state :sdl-key-return) (setf *message* nil)))
	  (*player-won*
	   (sdl:draw-string-solid-* "You won! Press ESC to quit..."
				    (- (floor *screen-width* 2) 125)
				    (- (floor *screen-height* 2) 4)
				    :color sdl:*black*)
	   (sdl:draw-string-solid-* "You won! Press ESC to quit..."
				    (- (floor *screen-width* 2) 126)
				    (- (floor *screen-height* 2) 5)
				    :color sdl:*white*))
	  ((> *player-hp* 0)
	   (setf *game-counter* (mod (1+ *game-counter*) 128))
	   (when (sdl:get-key-state :sdl-key-up) (player-up))
	   (when (sdl:get-key-state :sdl-key-down) (player-down))
	   (when (sdl:get-key-state :sdl-key-left) (player-left))
	   (when (sdl:get-key-state :sdl-key-right) (player-right))
	   (when (and (= 0 (mod *game-counter* 8))
		      (sdl:get-key-state :sdl-key-space))
	     (player-shoot))
	   (display-map)
	   (update-enemies)
	   (update-projectiles)
	   (draw-status))
	  (t
	   (sdl:draw-string-solid-* "You're dead! Press ESC to quit..."
				    (- (floor *screen-width* 2) 128)
				    (- (floor *screen-height* 2) 4)
				    :color sdl:*black*)
	   (sdl:draw-string-solid-* "You're dead! Press ESC to quit..."
				    (- (floor *screen-width* 2) 129)
				    (- (floor *screen-height* 2) 5)
				    :color sdl:*white*)))
	(sdl:update-display)))))
