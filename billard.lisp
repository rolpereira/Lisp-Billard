(require 'lisprebuilder-sdl)

(defstruct ball x y r direction-x direction-y vel-x vel-y)

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

(defun draw-ball (ball)
  (sdl:draw-circle-* (ball-x ball) (ball-y ball) (ball-r ball)))
  
(defun billard ()
  (let ((bola (make-ball :x 50 :y 50 :r 10 :direction-x 1 :direction-y 1 :vel-x 5 :vel-y 5))
         (window-width 800)
         (window-height 600))
    (sdl:with-init ()
      (sdl:window window-width window-height :position t
        :title-caption "Billard")
      (draw-ball bola)
      (sdl:with-events ()
        (:quit-event () t)
        (:key-down-event ()
          (when (sdl:key-down-p :sdl-key-escape)
            (sdl:push-quit-event)))
        (:idle ()
          (sdl:clear-display sdl:*black*)
          (when (or (> (+ (ball-y bola) (ball-r bola)) window-height)
                  (< (ball-y bola) 0))
            (invert-direction-ball bola :y 1))
          (when (or (> (+ (ball-x bola) (ball-r bola)) window-width)
                  (< (ball-x bola) 0))
            (invert-direction-ball bola :x 1))
          (move-ball bola)
          (draw-ball bola)
          (sdl:update-display))))))

        
