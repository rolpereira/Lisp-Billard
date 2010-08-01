(require 'lisprebuilder-sdl)

(defstruct ball x y r direction-x direction-y)

(defun invert-direction-ball (ball &key x y)
  "Invert the direction of a ball.

Keys `x' and `y' should be activated according to the direction the
ball should invert."
  (when x
    (setf (ball-direction-x ball) (* -1 (ball-direction-x ball))))
  (when y
    (setf (ball-direction-y ball) (* -1 (ball-direction-y ball)))))

(defun billard ()
  (let ((bola (make-ball :x 50 :y 50 :r 10 :direction-x 1 :direction-y 1))
         (window-width 800)
         (window-height 600)
         (vel-x 10)
         (vel-y 10))
    (sdl:with-init ()
      (sdl:window window-width window-height :position t
        :title-caption "Billard")
      (sdl:draw-circle-* (ball-x bola) (ball-y bola) (ball-r bola))
      (sdl:with-events ()
        (:quit-event () t)
        (:key-down-event ()
          (when (sdl:key-down-p :sdl-key-escape)
            (sdl:push-quit-event)))
        (:idle ()
          (sdl:clear-display sdl:*black*)
          (incf (ball-x bola) (* vel-x (ball-direction-x bola)))
          (incf (ball-y bola) (* vel-y (ball-direction-y bola)))
          (when (or (> (+ (ball-y bola) (ball-r bola)) window-height)
                  (< (ball-y bola) 0))
            (invert-direction-ball bola :y 1))
          (when (or (> (+ (ball-x bola) (ball-r bola)) window-width)
                  (< (ball-x bola) 0))
            (invert-direction-ball bola :x 1))
          (sdl:draw-circle-* (ball-x bola) (ball-y bola) (ball-r bola))
          (sdl:update-display))))))

        
