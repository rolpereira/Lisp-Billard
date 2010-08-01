(require 'lisprebuilder-sdl)

(defstruct ball x y r direction-x direction-y vel-x vel-y color)

(defun invert-direction-ball (ball &key x y)
  "Invert the direction of a ball.

Keys `x' and `y' should be activated according to the direction the
ball should invert."
  (when x
    (setf (ball-direction-x ball) (* -1 (ball-direction-x ball))))
  (when y
    (setf (ball-direction-y ball) (* -1 (ball-direction-y ball)))))

(defun move-ball (ball)
  (incf (ball-x ball) (* (ball-vel-x ball) (ball-direction-x ball)))
  (incf (ball-y ball) (* (ball-vel-y ball) (ball-direction-y ball))))

(defun move-balls (list-balls)
  (dolist (ball list-balls)
    (move-ball ball)))

(defun draw-ball (ball)
  (sdl:draw-circle-* (ball-x ball) (ball-y ball) (ball-r ball) :color (ball-color ball)))

(defun draw-balls (list-balls)
  (dolist (ball list-balls)
    (draw-ball ball)))
  
(defun billard ()
  (let* ((bola1 (make-ball :x 50 :y 50 :r 10 :direction-x 1 :direction-y 1 :vel-x 5 :vel-y 5 :color sdl:*yellow*))
         (bola2 (make-ball :x 300 :y 220 :r 10 :direction-x -1 :direction-y -1 :vel-x 5 :vel-y 5 :color sdl:*cyan*))
          (bolas (list bola1 bola2))
         (window-width 800)
         (window-height 600))
    (sdl:with-init ()
      (sdl:window window-width window-height :position t
        :title-caption "Billard")
      (draw-balls bolas)
      (sdl:with-events ()
        (:quit-event () t)
        (:key-down-event ()
          (when (sdl:key-down-p :sdl-key-escape)
            (sdl:push-quit-event)))
        (:idle ()
          (sdl:clear-display sdl:*black*)

          ;(maybe-change-direction-ball bola1)
          ;; TODO: This should probably be a macro, but it also works like this
          (mapcar #'(lambda (ball)
                     (when (or (> (+ (ball-y ball) (ball-r ball)) window-height)
                             (< (ball-y ball) 0))
                       (invert-direction-ball ball :y 1))
                     (when (or (> (+ (ball-x ball) (ball-r ball)) window-width)
                             (< (ball-x ball) 0))
                       (invert-direction-ball ball :x 1)))
            bolas)

          (move-balls bolas)
          (draw-balls bolas)
          (sdl:update-display))))))

        
