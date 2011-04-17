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

(defparameter *player-max-hp* 15)
(defparameter *player-hp* 15)
(defparameter *player-shells* 100)
(defparameter *player-dir* 'down)
(defparameter *player-step* 0)
(defparameter *game-counter* 0)
(defparameter *player-keys* 0)
(defparameter *player-won* nil)
(defparameter *message* nil)

(declaim (inline get-collision-tiles))
(defun get-collision-tiles (x y &optional (bbox 13))
  "Return the tiles that a character in position (x,y) would collide with,
assuming a 26x26 bounding box."
  (remove-duplicates
   (list
    (list (ash (- x bbox) -5) (ash (- y bbox) -5))
    (list (ash (+ x bbox) -5) (ash (- y bbox) -5))
    (list (ash (+ x bbox) -5) (ash (+ y bbox) -5))
    (list (ash (- x bbox) -5) (ash (+ y bbox) -5)))
   :test #'equal))

(defclass game-char ()
  ((x-position :initarg :x-position :accessor x-position)
   (y-position :initarg :y-position :accessor y-position)
   (direction :initarg :direction :accessor direction)
   (step-count :initarg :step-count :initform 0 :accessor step-count)
   (hit-points :initarg :hit-points :accessor hit-points)
   (sprite :initarg :sprite :accessor sprite)))

(defmethod collisionp ((character game-char))
  (cond ((or (>= (+ (x-position character) 16) (ash (map-width *current-map*) 5))
	     (< (- (x-position character) 16) 0)
	     (>= (+ (y-position character) 16) (ash (map-height *current-map*) 5))
	     (< (- (y-position character) 16) 0)) t)
	((and (not (position nil 
			     (mapcar #'(lambda (x)
					 (and
					  (= -1 (aref (wall-map *current-map*)
						      (first x) (second x)))
					  (= -1 (aref (obstacle-map *current-map*)
						      (first x) (second x)))))
				     (get-collision-tiles
				      (x-position character)
				      (y-position character))))))
	 nil)
	(t t)))

(defmethod move ((character game-char) direction)
  (case direction
    (up (setf (direction character) 'up)
	(decf (y-position character) 2)
	(when (collisionp character) (incf (y-position character) 2)))
    (down (setf (direction character) 'down)
	  (incf (y-position character) 2)
	  (when (collisionp character) (decf (y-position character) 2)))
    (left (setf (direction character) 'left)
	  (decf (x-position character) 2)
	  (when (collisionp character) (incf (x-position character) 2)))
    (right (setf (direction character) 'right)
	   (incf (x-position character) 2)
	   (when (collisionp character) (decf (x-position character) 2))))
  (setf (step-count character) (mod (1+ (step-count character)) 128)))

(defmethod shoot ((character game-char))
  (let ((x (x-position character)) (y (y-position character)))
    (cond ((eq (direction character) 'up) (decf y 16))
	  ((eq (direction character) 'down) (incf y 16))
	  ((eq (direction character) 'left) (decf x 16))
	  ((eq (direction character) 'right) (incf x 16)))
    (new-projectile x y (direction character))))

(defclass enemy (game-char)
  ((moving :initarg :moving :initform nil :accessor moving)))

(defclass player (game-char)
  ((shells :initarg :shells :initform 100 :accessor shells)
   (keys :initarg :keys :initform 0 :accessor keys)
   (max-hp :initarg :max-hp :initform 15 :accessor max-hp)))

(defun get-object (tile)
  "Remove object at tile. Apply its effects to the player."
  (let ((object (aref (object-map *current-map*)
			   (first tile) (second tile))))
    (when (> object 0)
      (setf object (mod object 20)))
    (setf (aref (object-map *current-map*) (first tile) (second tile)) -1)
    (cond
      ((= object 0) (incf (keys *player*)))
      ((= object 1) (incf (shells *player*) 20))
      ((= object 2) (incf (hit-points *player*)))
      ((= object 3) (incf (hit-points *player*) 2))
      ((= object 4) (incf (hit-points *player*) 5))
      ((= object 5) (setf (hit-points *player*) (max-hp *player*)))
      ((= object 19) (setf *player-won* t)))
    (when (> (hit-points *player*) (max-hp *player*))
      (setf (hit-points *player*) (max-hp *player*)))))

(defmethod move :after ((player player) direction)
  (let ((collision-tiles (get-collision-tiles (x-position player)
					      (y-position player) 15)))
    (dolist (tile collision-tiles)
      (get-object tile)
      (when (not *message*)
	(setf *message* (aref *messages* (first tile) (second tile))))
      (when (and (= 0 (mod (aref *obstacle-map* (first tile) (second tile)) 20))
		 (> (keys *player*) 0))
	(decf (keys *player*))
	(setf (aref *obstacle-map* (first tile) (second tile)) -1))
      (when (aref *portals* (first tile) (second tile))
	(load-map (aref *portals* (first tile) (second tile)))
	(return-from move t)))))

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
(defvar *current-map* nil)
(defvar *world-maps* nil)

(defclass world-map ()
  ((map-name :initarg :map-name :accessor map-name)
   (tileset :initarg :tileset :initform "images/tiles.png" :accessor tileset)
   (map-width :initarg :map-width :accessor map-width)
   (map-height :initarg :map-height :accessor map-height)
   (floor-map :initarg :floor-map :accessor floor-map)
   (wall-map :initarg :wall-map :accessor wall-map)
   (obstacle-map :initarg :obstacle-map :accessor obstacle-map)
   (object-map :initarg :object-map :accessor object-map)
   (portals :initarg :portals :accessor portals)
   (messages :initarg :messages :accessor messages)
   (tiles :accessor tiles)))

(defmethod initialize-instance :after ((world-map world-map) &rest rest)
  (declare (ignore rest))
  (setf (tiles world-map)
	(sdl:convert-to-display-format
	 :surface (sdl:load-image
		   (concatenate 'string *data-directory* *tileset*)
		   :image-type :png)
	 :pixel-alpha t))
  (acons (map-name world-map) world-map *world-maps*))

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
					(write-to-string (hit-points *player*))
					"     FIREPOWER: "
					(write-to-string (shells *player*))
					"     KEYS: "
					(write-to-string (keys *player*)))))
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
	 (y-pos (- (ash *screen-height* -1) (* 5 lines)))
	 (start 0))
    (sdl:draw-box (sdl:rectangle-from-midpoint-*
		   (ash *screen-width* -1) (ash *screen-height* -1)
		   320 200)
		  :color sdl:*blue*)
    (dotimes (i lines)
      (sdl:draw-string-solid-* (subseq message start
				       (position #\Newline message :start start))
			       (- (floor *screen-width* 2) 150) y-pos)
      (setf start (1+ (or (position #\Newline message :start start) 0)))
      (incf y-pos 10))
    (sdl:draw-string-solid-* "Press ENTER..."
			     (+ (ash *screen-width* -1) 40)
			     (+ (ash *screen-height* -1) 90))))

(defun display-map ()
  "Place tiles from the map on the display surface so that player is centered
on the screen."
  (let ((top-x (- (x-position *player*) (floor *screen-width* 2)))
	(top-y (- (y-position *player*) (floor *screen-height* 2)))
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
		  (when (and (< tile-x (map-width *current-map*))
			     (< tile-y (map-height *current-map*)))		    
		    (draw-tile *tiles* (aref (floor-map *current-map*)
					     tile-x tile-y) x y)
		    (when (<= 0 (aref (wall-map *current-map*) tile-x tile-y))
		      (draw-tile *tiles* (aref (wall-map *current-map*)
					       tile-x tile-y) x y))
		    (when (<= 0 (aref (obstacle-map *current-map*) tile-x tile-y))
		      (draw-tile *tiles* (aref (obstacle-map *current-map*)
					       tile-x tile-y) x y))
		    (when (<= 0 (aref (object-map *current-map*) tile-x tile-y))
		      (draw-tile *tiles* (aref (object-map *current-map*)
					       tile-x tile-y) x y))))))
    (draw-tile *characters* (sprite *player*)
	       (- (ash *screen-width* -1) 16)
	       (- (ash *screen-height* -1) 16))))

(defun detect-projectile-collision (projectile)
  (dolist (enemy *enemies*)
    (when (and (> (first projectile) (- (x-position enemy) 16))
	       (< (first projectile) (+ (x-position enemy) 16))
	       (> (second projectile) (- (y-position enemy) 16))
	       (< (second projectile) (+ (y-position enemy) 16)))
      (decf (hit-points enemy))
      (setf (fourth projectile) 0)))
  (when (and (> (first projectile) (- (x-position *player*) 16))
	     (< (first projectile) (+ (x-position *player*) 16))
	     (> (second projectile) (- (y-position *player*) 16))
	     (< (second projectile) (+ (y-position *player*) 16)))
    (decf (hit-points *player*))
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
			 (sdl:point :x (+ (- (first projectile) 16 
					     (x-position *player*))
					  (ash *screen-width* -1))
				    :y (+ (- (second projectile) 16
					     (y-position *player*))
					  (ash *screen-height* -1)))))
  (setf *projectiles* (remove-if-not #'(lambda (projectile)
					 (> (fourth projectile) 0))
				     *projectiles*)))

(defun new-projectile (x y dir)
  (let (ttl)
    (cond ((eq dir 'up)
	   (setf ttl (floor (+ (floor *screen-height* 2)
			       (- y (y-position *player*))) 3)))
	  ((eq dir 'down)
	   (setf ttl (floor (+ (floor *screen-height* 2)
			       (- (y-position *player*) y)) 3)))
	  ((eq dir 'left)
	   (setf ttl (floor (+ (floor *screen-width* 2)
			       (- x (x-position *player*))) 3)))
	  ((eq dir 'right)
	   (setf ttl (floor (+ (floor *screen-width* 2)
			       (- (x-position *player*) x)) 3))))
    (when (null ttl) (setf ttl 0))
    (push (list x y dir ttl) *projectiles*)))

(defun update-enemies ()
  "Loop through the enemies, updating position, removing dead guys."
  (dolist (enemy *enemies*)
    (when (and (< (abs (- (x-position enemy) (x-position *player*)))
		  (ash *screen-width* -1))
	       (< (abs (- (y-position enemy) (y-position *player*)))
		  (ash *screen-height* -1)))
      (if (moving enemy)
	  (progn (move enemy (direction enemy))
		 (when (> (random 100) 80) (setf (moving enemy) nil)))
	  (when (> (random 100) 80) (setf (moving enemy) t)))
      (when (> (random 100) 80)
	(let ((new-dir (random 4)))
	  (cond ((= new-dir 0) (setf (direction enemy) 'up))
		((= new-dir 1) (setf (direction enemy) 'down))
		((= new-dir 2) (setf (direction enemy) 'left))
		((= new-dir 3) (setf (direction enemy) 'right)))))
      (when (and (< (abs (- (x-position enemy) (x-position *player*))) 16)
		 (< (abs (- (y-position enemy) (y-position *player*))) 16))
	(decf (hit-points *player*))
	(cond ((< (x-position enemy) (x-position *player*))
	       (move *player* 'right) (move *player 'right))
	      ((> (x-position enemy) (x-position *player*))
	       (move *player* 'left) (move *player 'left))
	      ((< (y-position enemy) (y-position *player*))
	       (move *player* 'down) (move *player* 'down))
	      ((> (y-position enemy) (y-position *player*))
	       (move *player* 'up) (move *player* 'up))))
      (when (> (random 100) 98)
	(shoot enemy))
      (draw-tile *characters* (sprite enemy)
		 (+ (- (x-position enemy) 16 (x-position *player*))
		    (ash *screen-width* -1))
		 (+ (- (y-position enemy) 16 (y-position *player*))
		    (ash *screen-height* -1)))))
  (setf *enemies* (remove-if-not #'(lambda (enemy) (> (hit-points enemy) 0))
				 *enemies*)))

(defun main ()
  (defparameter *player* (make-instance 'player
					:x-position 1 :y-position 1
					:direction 'down
					:hit-points 15
					:sprite 0))
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
				    (- (ash *screen-width* -1) 125)
				    (- (ash *screen-height* -1) 4)
				    :color sdl:*black*)
	   (sdl:draw-string-solid-* "You won! Press ESC to quit..."
				    (- (ash *screen-width* -1) 126)
				    (- (ash *screen-height* -1) 5)
				    :color sdl:*white*))
	  ((> (hit-points *player*) 0)
	   (setf *game-counter* (mod (1+ *game-counter*) 128))
	   (when (sdl:get-key-state :sdl-key-up) (move *player* 'up))
	   (when (sdl:get-key-state :sdl-key-down) (move *player* 'down))
	   (when (sdl:get-key-state :sdl-key-left) (move *player* 'left))
	   (when (sdl:get-key-state :sdl-key-right) (move *player* 'right))
	   (when (and (= 0 (mod *game-counter* 8))
		      (sdl:get-key-state :sdl-key-space))
	     (shoot *player*))
	   (display-map)
	   (update-enemies)
	   (update-projectiles)
	   (draw-status))
	  (t
	   (sdl:draw-string-solid-* "You're dead! Press ESC to quit..."
				    (- (ash *screen-width* -1) 128)
				    (- (ash *screen-height* -1) 4)
				    :color sdl:*black*)
	   (sdl:draw-string-solid-* "You're dead! Press ESC to quit..."
				    (- (ash *screen-width* -1) 129)
				    (- (ash *screen-height* -1) 5)
				    :color sdl:*white*)))
	(sdl:update-display)))))
