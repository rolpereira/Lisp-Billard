(require 'lispbuilder-sdl)

(defstruct ball x y r direction-x direction-y vel-x vel-y color)

(defun invert-direction-ball (ball &key x y)
  "Invert the direction of a ball.

Keys `x' and `y' should have a value of 1 which will change the
direction of the ball on the axis."
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

(defun right-side (ball)
  (+ (ball-x ball) (ball-r ball)))

(defun left-side (ball)
  (- (ball-x ball) (ball-r ball)))

(defun bottom-side (ball)
  (+ (ball-y ball) (ball-r ball)))

(defun top-side (ball)
  (- (ball-y ball) (ball-r ball)))

(defun is-above-p (y-value-1 y-value-2)
  "Check if value y-value-1 is above (in terms of y coordinates on the screen) the y-value-2.

This is used because SDL uses the top-left corner as the
coordinates (0,0), so to check if something is above another thing, it
is necessary to use <= on the y coordinates."
  (<= y-value-1 y-value-2))

(defun is-below-p (y-value-1 y-value-2)
  "Check if value y-value-1 is below (in terms of y coordinates on the screen) the y-value-2.

This is used because SDL uses the top-left corner as the
coordinates (0,0), so to check if something is below another thing, it
is necessary to use >= on the y coordinates."
  (>= y-value-1 y-value-2))

(defun delta (x y)
  (abs (- x y)))

(defun verify-collision-between-2-balls (ball-1 ball-2)
  ;; Can't do anything until the balls overlap
  (when (overlap-balls ball-1 ball-2)
    (cond
      ;; First check for the situation similar to this one:
      ;;       +---+             
      ;;       |   |             
      ;;       | 1 |             
      ;;       |  ++--+          
      ;;       +--++  |          
      ;;          | 2 |
      ;;          |   |
      ;;          +---+
      ((and (is-above-p (top-side ball-2) (bottom-side ball-1))
         (<= (left-side ball-2) (right-side ball-1))
                                        ;       (is-above-p (top-side ball-1) (top-side ball-2))
                                        ;       (is-above-p (bottom-side ball-1) (bottom-side ball-2))
                                        ;(<= (left-side ball-1) (left-side ball-2))
                                        ;(<= (right-side ball-1) (right-side ball-2)))
         )
        (cond
          ;; Check to see if the following situation happened:
          ;;    +---+   
          ;;    |   |   
          ;;    | 1 |   
          ;;    |+--++  
          ;;    ++--+|  
          ;;     | 2 |  
          ;;     |   |  
          ;;     +---+  
          ;;
          ;; In this case, we should only change the direction-y of the balls
          ((> (delta (right-side ball-1) (left-side ball-2))
             (delta (top-side ball-2) (bottom-side ball-1)))
            (invert-direction-ball ball-1 :y 1)
            (invert-direction-ball ball-2 :y 1))
          ;; Check to see if the following situation happened:
          ;; +---+   
          ;; |  ++--+
          ;; | 1||  |
          ;; |  ||2 |
          ;; +--++  |
          ;;    +---+
          ;;
          ;; In this case, we should only change the direction-x of the balls
          ((> (delta (top-side ball-2) (bottom-side ball-1))
             (delta (right-side ball-1) (left-side ball-2)))
            (invert-direction-ball ball-1 :x 1)
            (invert-direction-ball ball-2 :x 1))
          ;; Assume that the following situation happened:
          ;; +---+   
          ;; |   |   
          ;; | 1 |   
          ;; |  ++--+
          ;; +--++  |
          ;;    | 2 |
          ;;    |   |
          ;;    +---+      
          ;;
          ;; In this case, we change the values of direction-x and direction-y on both balls
          (t
            (invert-direction-ball ball-1 :x 1 :y 1)
            (invert-direction-ball ball-2 :x 1 :y 1))))

      ;; First check for the situation similar to this one:
      ;;    +---+
      ;;    |   |
      ;;    | 1 |  
      ;; +--++  |
      ;; |  ++--+
      ;; | 2 |   
      ;; |   |   
      ;; +---+   
      ((and (is-above-p (top-side ball-2) (bottom-side ball-1))
         (<= (left-side ball-1) (right-side ball-2)))
                                        ;       (is-above-p (top-side ball-1) (top-side ball-2))
                                        ;       (is-above-p (bottom-side ball-2) (bottom-side ball-2))
                                        ;(<= (left-side ball-2) (left-side ball-1))
                                        ;(<= (right-side ball-1) (right-side ball-2)))
        (cond
          ;; Check to see if the following situation happened:
          ;;  +---+  
          ;;  |   |  
          ;;  | 1 |  
          ;; ++--+|  
          ;; |+--++  
          ;; | 2 |   
          ;; |   |   
          ;; +---+   
          ;; In this case, we should only change the direction-y of the balls
          ((> (delta (left-side ball-1) (right-side ball-2))
             (delta (top-side ball-2) (bottom-side ball-1)))
            (invert-direction-ball ball-1 :y 1)
            (invert-direction-ball ball-2 :y 1))
          ;; Check to see if the following situation happened:
          ;;    +---+
          ;; +--++  |
          ;; |  ||1 |
          ;; | 2||  |
          ;; |  ++--+
          ;; +---+   
          ;; In this case, we should only change the direction-x of the balls
          ((> (delta (top-side ball-2) (bottom-side ball-2))
             (delta (left-side ball-1) (right-side ball-2)))
            (invert-direction-ball ball-1 :x 1)
            (invert-direction-ball ball-2 :y 1))
          ;; Assume that the following situation happened:
          ;;    +---+
          ;;    |   |
          ;;    | 1 |
          ;; +--++  |
          ;; |  ++--+
          ;; | 2 |   
          ;; |   |   
          ;; +---+   
          ;; In this case, we change the values of direction-x and direction-y on both balls
          (t
            (invert-direction-ball ball-1 :x 1 :y 1)
            (invert-direction-ball ball-2 :x 1 :y 1)))))))


(defun overlap-balls (ball-1 ball-2)
  "Indicates if ball-2 overlaps ball-1"
  ;; There is a need to check the overlap for the 4 corners of the
  ;; "rectangle" that contains the ball.
  ;; The value of the `CONS' is (X Y) of the point.
  ;; FIXME: WHY DO I NEED TO DO THIS AND CAN'T USE A CONS?
  (let ((point-1x (left-side ball-2))
         (point-1y (top-side ball-2))
         (point-2x (left-side ball-2))
         (point-2y (bottom-side ball-2))
         (point-3x (right-side ball-2))
         (point-3y (bottom-side ball-2))
         (point-4x (right-side ball-2))
         (point-4y (top-side ball-2)))
    (or
      (and
        ;; Check the values in X
        (and (<= (left-side ball-1) point-1x)
          (<= point-1x (right-side ball-1)))
        ;; Check the values in Y
        (and (<= (top-side ball-1) point-1y)
          (<= point-1y (bottom-side ball-1))))
      (and
        ;; Check the values in X
        (and (<= (left-side ball-1) point-2x)
          (<= point-2x (right-side ball-1)))
        ;; Check the values in Y
        (and (<= (top-side ball-1) point-2y)
          (<= point-2y (bottom-side ball-1))))
      (and
        ;; Check the values in X
        (and (<= (left-side ball-1) point-3x)
          (<= point-3x (right-side ball-1)))
        ;; Check the values in Y
        (and (<= (top-side ball-1) point-3y)
          (<= point-3y (bottom-side ball-1))))
      (and
        ;; Check the values in X
        (and (<= (left-side ball-1) point-4x)
          (<= point-4x (right-side ball-1)))
        ;; Check the values in Y
        (and (<= (top-side ball-1) point-4y)
          (<= point-4y (bottom-side ball-1)))))))




(defun billard ()
  (let* ((bola1 (make-ball :x 50 :y 50 :r 10 :direction-x 1 :direction-y 1 :vel-x 5 :vel-y 5 :color sdl:*yellow*))
          (bola2 (make-ball :x 300 :y 300 :r 10 :direction-x -1 :direction-y -1 :vel-x 5 :vel-y 5 :color sdl:*cyan*))
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
          ;; TODO: This should probably be a macro, but it also works like this
          (mapcar #'(lambda (ball)
                      (when (or (>= (bottom-side ball) window-height)
                              (<= (top-side ball) 0))
                        (invert-direction-ball ball :y 1))
                      (when (or (>= (right-side ball) window-width)
                              (<= (left-side ball) 0))
                        (invert-direction-ball ball :x 1)))
            bolas)
          (verify-collision-between-2-balls (car bolas) (cadr bolas))
          (move-balls bolas)
          (draw-balls bolas)
          (sdl:update-display))))))
