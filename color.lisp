(in-package :raytracer)

(defclass color ()
  ((r :type single-float :initarg :r :initform 0.0 :accessor color-r)
   (g :type single-float :initarg :g :initform 0.0 :accessor color-g)
   (b :type single-float :initarg :b :initform 0.0 :accessor color-b)))

(defun color (r g b)
  (make-instance 'color :r (float r)
                        :g (float g)
                        :b (float b)))

(defun float-to-uint8 (x)
  (let ((value (floor (* 255.0 x))))
    (min (max value 0) 255)))

(defmethod add ((a color) (b color))
  (color (+ (color-r a) (color-r b))
         (+ (color-g a) (color-g b))
         (+ (color-b a) (color-b b))))

(defmethod mul ((a color) (b color))
  (color (* (color-r a) (color-r b))
         (* (color-g a) (color-g b))
         (* (color-b a) (color-b b))))

(defmethod mul ((color color) (scalar number))
  (color (* (color-r color) scalar)
         (* (color-g color) scalar)
         (* (color-b color) scalar)))

(defmethod mul ((scalar number) (color color))
  (mul color scalar))

(defmethod div ((color color) (scalar number))
  (color (/ (color-r color) scalar)
         (/ (color-g color) scalar)
         (/ (color-b color) scalar)))
