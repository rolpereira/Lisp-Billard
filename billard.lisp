(require 'lispbuilder-sdl)

(defstruct ball x y r direction-x direction-y vel-x vel-y color)

(defvar *inertia* 0.9
  "Value to remove to the velocity of the balls at every frame")

(defun invert-direction-ball (ball &key x y)
  "Invert the direction of a ball.

Keys `x' and `y' should have a value of 1 which will change the
direction of the ball on the axis."
  (when x
    (setf (ball-direction-x ball) (* -1 (ball-direction-x ball))))
  (when y
    (setf (ball-direction-y ball) (* -1 (ball-direction-y ball)))))

(defun move-ball (ball)
  (if (> (ball-vel-x ball) 0)
    (setf (ball-vel-x ball) (- (ball-vel-x ball) *inertia*))
    (setf (ball-vel-x ball) 0))
  (if (> (ball-vel-y ball) 0)
    (setf (ball-vel-y ball) (- (ball-vel-y ball) *inertia*))
    (setf (ball-vel-y ball) 0))
  (incf (ball-x ball) (* (ball-vel-x ball) (ball-direction-x ball)))
  (incf (ball-y ball) (* (ball-vel-y ball) (ball-direction-y ball))))

(defun move-balls (list-balls)
  (dolist (ball list-balls)
    (move-ball ball)))

(defun draw-ball (ball)
  (sdl:draw-circle-* (round (ball-x ball)) (round (ball-y ball)) (ball-r ball)
    :color (ball-color ball)))

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
         (<= (left-side ball-2) (right-side ball-1)))
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
            (invert-direction-ball ball-2 :x 1 :y 1))))

      ((and (is-above-p (top-side ball-1) (bottom-side ball-2))
         (<= (left-side ball-1) (right-side ball-2)))
        (cond
          ((> (delta (left-side ball-1) (right-side ball-2))
             (delta (top-side ball-1) (bottom-side ball-2)))
            (invert-direction-ball ball-1 :y 1)
            (invert-direction-ball ball-2 :y 1))
          ((> (delta (top-side ball-1) (bottom-side ball-2))
             (delta (left-side ball-1) (right-side ball-2)))
            (invert-direction-ball ball-1 :x 1)
            (invert-direction-ball ball-2 :x 1))
          (t
            (invert-direction-ball ball-1 :x 1 :y 1)
            (invert-direction-ball ball-2 :x 1 :y 1))))

      ((and (is-above-p (top-side ball-1) (bottom-side ball-2))
         (<= (left-side ball-2) (right-side ball-1)))
        (cond
          ((> (delta (left-side ball-2) (right-side ball-1))
             (delta (top-side ball-1) (bottom-side ball-2)))
            (invert-direction-ball ball-1 :y 1)
            (invert-direction-ball ball-2 :y 1))
          ((> (delta (top-side ball-1) (bottom-side ball-2))
             (delta (left-side ball-2) (right-side ball-1)))
            (invert-direction-ball ball-1 :x 1)
            (invert-direction-ball ball-2 :x 1))
          (t
            (invert-direction-ball ball-1 :x 1 :y 1)
            (invert-direction-ball ball-2 :x 1 :y 1)))))))

(defun overlap-balls (ball-1 ball-2)
  "Indicates if ball-2 overlaps ball-1"
  (let* ((point-1 (list (right-side ball-2) (top-side ball-2)))
          (point-2 (list (right-side ball-2) (bottom-side ball-2)))
          (point-3 (list (left-side ball-2) (bottom-side ball-2)))
          (point-4 (list (left-side ball-2) (top-side ball-2)))
          (points (list point-1 point-2 point-3 point-4)))
    (remove-if #'null
      (mapcar #'(lambda (point)
                  (let ((point-x (car point))
                         (point-y (cadr point)))
                    (and
                      ;; Check the values in X
                      (and (<= (left-side ball-1) point-x)
                        (<= point-x (right-side ball-1)))
                      ;; Check the values in Y
                      (and (<= (top-side ball-1) point-y)
                        (<= point-y (bottom-side ball-1))))))
        points))))

(defun create-ball-stand-still (x y r)
  "Creates a ball with color `COLOR' on coordinates (X Y), but with velocity 0."
  (make-ball :x x :y y :r r
    :direction-x 0 :direction-y 0
    :vel-x 0 :vel-y 0
    :color sdl:*green*))

(defun create-column-balls (column-number mid-x mid-y radius &key (separation 2))
  (let (;; The number of balls on this column
         (number-of-balls (+ column-number 3))
         ;; The columns whose number is positive are the ones further
         ;; away from the white ball
         (column-number-positive-p (> column-number 0))
         (column-number (abs column-number))
         (return-list '()))
    (if (zerop (mod column-number 2))
      ;; This is the algorithm for placing balls on a even column
      ;; (for example, the one that contains the middle-ball)
      (let ((value-x-of-column (if column-number-positive-p
                                 (- mid-x (+ (* 2 column-number radius) separation))
                                 (+ mid-x (+ (* 2 column-number radius) separation)))))
        ;; If the number-of-balls to draw is 1, then only draw the middle ball
        ;; (It should be the column closest to the white ball)
        (unless (= number-of-balls 1)
          ;; k indicates the distance between a ball and the column's middle ball
          (loop for k from 1 below number-of-balls by 2 do
            (let ((delta-y-of-ball (+
                                     (* 2 (ceiling (/ k 2)) radius)
                                     separation)))
              (push
                (create-ball-stand-still value-x-of-column (- mid-y delta-y-of-ball) radius)
                return-list)
              (push
                (create-ball-stand-still value-x-of-column (+ mid-y delta-y-of-ball) radius)
                return-list))))
        ;; Create the column's middle ball
        (push (create-ball-stand-still value-x-of-column (+ mid-y separation) radius)
          return-list))

      ;; This is the algorithm for placing balls on a odd column
      (let ((value-x-of-column (if column-number-positive-p
                                 (- mid-x (+ (* 2 radius) separation))
                                 (+ mid-x (+ (* 2 radius) separation)))))
        ;; k indicates the distance between a ball and the middle ball on an odd column
        (loop for k from 1 below number-of-balls by 2 do
          (let ((delta-y-of-ball (+ (* k radius) separation)))
            (push
              (create-ball-stand-still value-x-of-column (- mid-y delta-y-of-ball) radius)
              return-list)
            (push
              (create-ball-stand-still value-x-of-column (+ mid-y delta-y-of-ball) radius)
              return-list)))))
    return-list))
          
                             
(defun create-initial-balls (mid-x mid-y radius &key (separation 2))
  "Returns a list of balls, on the position they should occupy in the beginning of the game"
  (let* (
          ;; The order of the placement of the balls
          ;;
          ;;        ---                 
          ;;       /   \                
          ;;       | 6 |---             
          ;;       \   /   \            
          ;;        ---| 12|---         
          ;;       /   \   /   \        
          ;;       | 5 |---| 1 |---     
          ;;       \   /   \   /   \    
          ;;        ---| 11|---| 9 |--- 
          ;;       /   \   /   \   /   \
          ;;       | 4 |---| M |---| 3 |
          ;;       \   /   \   /   \   /
          ;;        ---| 13|---| 10|--- 
          ;;       /   \   /   \   /    
          ;;       | 7 |---| 2 |---     
          ;;       \   /   \   /        
          ;;        ---| 14|---         
          ;;       /   \   /            
          ;;       | 8 |---             
          ;;       \   /                
          ;;        ---
          ;;
          ;; The M ball represents the middle-ball

          ;; Create middle-column
          (middle-column (create-column-balls 0 mid-x mid-y radius))
          ;; The middle-ball is the black ball on the oficial game
          ;(middle-ball (create-ball-stand-still mid-x mid-y radius))
          ;; ball-1 and ball-2 are, on that order, above and below the middle-ball
          ;(ball-1 (create-ball-stand-still mid-x (- mid-y (+ (* 2 radius) separation))
          ;;          radius))
          ;(ball-2 (create-ball-stand-still mid-x (+ mid-y (+ (* 2 radius) separation)) radius))
          ;; ball-3 is the ball facing towards the white ball
          (column-facing-white-ball (create-column-balls -2 mid-x mid-y radius))
          ;;(ball-3 (create-ball-stand-still (+ mid-x (+ (* 4 radius) separation)) mid-y radius))
          ;; balls 4 through 8 are on the column futher away from the white ball
          ;; (ball-4 (create-ball-stand-still (- mid-x (+ (* 4 radius) separation)) mid-y radius))
          ;; (ball-5 (create-ball-stand-still (- mid-x (+ (* 4 radius) separation))
          ;;           (- mid-y (+ (* 2 radius) separation)) radius))
          ;; (ball-6 (create-ball-stand-still (- mid-x (+ (* 4 radius) separation))
          ;;           (- mid-y (+ (* 4 radius) separation)) radius))
          ;; (ball-7 (create-ball-stand-still (- mid-x (+ (* 4 radius) separation))
          ;;           (+ mid-y (+ (* 2 radius) separation)) radius))
          ;; (ball-8 (create-ball-stand-still (- mid-x (+ (* 4 radius) separation))
          ;;           (+ mid-y (+ (* 4 radius) separation)) radius))

          (end-column (create-column-balls 2 mid-x mid-y radius))
          ;; (ball-9 (create-ball-stand-still (+ mid-x (+ (* 2 radius) separation))
          ;;           (- mid-y (+ radius 2)) radius))
          ;; (ball-10 (create-ball-stand-still (+ mid-x (+ (* 2 radius) separation))
          ;;            (+ mid-y (+ radius 2)) radius))
          (column-1 (create-column-balls -1 mid-x mid-y radius))

          (column-2 (create-column-balls 1 mid-x mid-y radius)))
          ;; (ball-11 (create-ball-stand-still (- mid-x (+ (* 2 radius) separation))
          ;;            (- mid-y (+ radius 2)) radius))
          ;; (ball-12 (create-ball-stand-still (- mid-x (+ (* 2 radius) separation))
          ;;            (- mid-y (+ (* 3 radius) separation)) radius))
          ;; (ball-13 (create-ball-stand-still (- mid-x (+ (* 2 radius) separation))
          ;;            (+ mid-y (+ radius separation)) radius))
          ;; (ball-14 (create-ball-stand-still (- mid-x (+ (* 2 radius) separation))
          ;;            (+ mid-y (+ (* 3 radius) separation)) radius)))
    (append middle-column column-facing-white-ball end-column column-1 column-2)))
 

(defun billard ()
  (let* (;; (bola1 (make-ball :x (+ 100 (random 400)) :y (+ 100 (random 400)) :r 20
         ;;          :direction-x -1 :direction-y 1
         ;;          :vel-x 15 :vel-y 15
         ;;          :color sdl:*yellow*))
         ;;  (bola2 (make-ball :x 80 :y 280 :r 30
         ;;           :direction-x 1 :direction-y -1
         ;;           :vel-x 15 :vel-y 15
         ;;           :color sdl:*cyan*))
         ;; (bolas (list bola1 bola2))
          (bolas (create-initial-balls 400 300 20))
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
          ;(verify-collision-between-2-balls (car bolas) (cadr bolas))
          (move-balls bolas)
          (draw-balls bolas)
          (sdl:update-display))))))
