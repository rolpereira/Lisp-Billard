(require 'lisprebuilder-sdl)

(defun billard ()
  (let ((x-rect 50)
         (y-rect 50)
         (w-rect 50)
         (h-rect 50)
         (direction-x 1)
         (direction-y 1)
         (window-width 800)
         (window-height 600))
    (sdl:with-init ()
      (sdl:window window-width window-height :position t
        :title-caption "Billard")
      (sdl:draw-rectangle-* x-rect y-rect w-rect h-rect)
      (sdl:with-events ()
        (:quit-event () t)
        (:key-down-event ()
          (when (sdl:key-down-p :sdl-key-escape)
            (sdl:push-quit-event)))
        (:idle ()
          (sdl:clear-display sdl:*black*)
          (incf x-rect (* 5 direction-x))
          (incf y-rect (* 5 direction-y))
          (when (> (+ y-rect h-rect) window-height)
            (setf direction-y -1))
          (when (< y-rect 0)
            (setf direction-y 1))
          (when (> (+ x-rect w-rect) window-width)
            (setf direction-x -1))
          (when (< x-rect 0)
            (setf direction-x 1))
          (sdl:draw-rectangle-* x-rect y-rect w-rect h-rect)
          (sdl:update-display))))))

        