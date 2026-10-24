(in-package :raytracer)

(defgeneric add (a b))
(defgeneric sub (a b))
(defgeneric mul (a b))
(defgeneric div (a b))

(defclass vec3 ()
  ((x :type single-float :initarg :x :initform 0.0 :accessor vec3-x)
   (y :type single-float :initarg :y :initform 0.0 :accessor vec3-y)
   (z :type single-float :initarg :z :initform 0.0 :accessor vec3-z)))

(defun vec3 (x y z)
  (make-instance 'vec3 :x (float x)
                       :y (float y)
                       :z (float z)))

(defmethod print-object ((v vec3) out)
  (with-slots (x y z) v
    (format out "(~a, ~a, ~a)" x y z)))

(defmethod add ((a vec3) (b vec3))
  (with-slots ((ax x) (ay y) (az z)) a
    (with-slots ((bx x) (by y) (bz z)) b
      (make-instance 'vec3 :x (+ ax bx)
                           :y (+ ay by)
                           :z (+ az bz)))))

(defun add* (&rest vectors)
  (reduce #'add vectors :initial-value (vec3 0 0 0)))

(defmethod add ((a vec3) (b number))
  (with-slots ((ax x) (ay y) (az z)) a
    (make-instance 'vec3 :x (+ ax b)
                         :y (+ ay b)
                         :z (+ az b))))

(defmethod sub ((a vec3) (b vec3))
  (with-slots ((ax x) (ay y) (az z)) a
    (with-slots ((bx x) (by y) (bz z)) b
      (make-instance 'vec3 :x (- ax bx)
                           :y (- ay by)
                           :z (- az bz)))))

(defmethod sub ((a vec3) (b number))
  (with-slots ((ax x) (ay y) (az z)) a
    (make-instance 'vec3 :x (- ax b)
                         :y (- ay b)
                         :z (- az b))))

(defmethod mul ((a vec3) (b number))
  (with-slots ((ax x) (ay y) (az z)) a
    (make-instance 'vec3 :x (* ax b)
                         :y (* ay b)
                         :z (* az b))))

(defmethod mul ((b number) (a vec3))
  (mul a b))

(defmethod div ((a vec3) (b number))
  (with-slots ((ax x) (ay y) (az z)) a
    (make-instance 'vec3 :x (/ ax b)
                         :y (/ ay b)
                         :z (/ az b))))

(defmethod len ((v vec3))
  (with-slots (x y z) v
    (sqrt (+ (* x x)
             (* y y)
             (* z z)))))

(defmethod normalize ((v vec3))
  (let ((length (len v)))
    (with-slots (x y z) v
      (vec3 (/ x length)
            (/ y length)
            (/ z length)))))

(defmethod dot ((a vec3) (b vec3))
  (with-slots ((ax x) (ay y) (az z)) a
    (with-slots ((bx x) (by y) (bz z)) b
      (+ (* ax bx)
         (* ay by)
         (* az bz)))))

(defmethod cross ((a vec3) (b vec3))
  (with-slots ((ax x) (ay y) (az z)) a
    (with-slots ((bx x) (by y) (bz z)) b
      (vec3 (- (* ay bz) (* az by))
            (- (* az bx) (* ax bz))
            (- (* ax by) (* ay bx))))))

(defun deg2rad (deg)
  (float (* deg (/ pi 180.0)) 1.0))

(defun solve-quadractic (a b c)
  (let ((discriminant (- (* b b) (* 4 a c))))
    (when (< discriminant 0)
      (return-from solve-quadractic nil))
    (when (= discriminant 0)
      (return-from solve-quadractic (list (/ (* -0.5 b)
                                             a))))
    (let* ((q (if (> b 0)
                  (* -0.5 (+ b (sqrt discriminant)))
                  (* -0.5 (- b (sqrt discriminant)))))
           (x0 (/ q a))
           (x1 (/ c q)))
      (when (> x0 x1)
        (rotatef x0 x1))
      (list x0 x1))))
