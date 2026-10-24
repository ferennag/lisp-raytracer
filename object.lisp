(in-package :raytracer)

(defclass object ()
  ((children :type list :initarg :children :initform nil :accessor children)))

(defgeneric intersect (object ray))

(defmethod intersect ((object object) (ray ray))
  (let ((results (loop for child in (children object)
                       for hit = (intersect child ray)
                       when hit
                         collect hit)))
    (first (sort results (lambda (a b)
                           (< (hit-record-t a)
                              (hit-record-t b)))))))

(defclass hittable ()
  ((material :type material :initarg :material :accessor hittable-material)))

(defclass sphere (hittable)
  ((position :type vec3 :initarg :position :initform (vec3 0 0 0) :accessor sphere-pos)
   (radius :type single-float :initarg :radius :initform 1.0 :accessor sphere-radius)))

(defmethod print-object ((s sphere) out)
  (with-slots (position radius) s
    (format out "Pos: ~a, Radius: ~a" position radius)))

(defun sphere (position radius material)
  (make-instance 'sphere :position position :radius radius :material material))

(defstruct hit-record
  object
  t
  normal)

(defmethod intersect ((object sphere) (ray ray))
  (with-slots (position radius) object
    (with-slots (origin direction) ray
      (let* ((L (sub origin position))
             (a (dot direction direction))
             (b (* 2 (dot direction L)))
             (c (- (dot L L) (* radius radius)))
             (ts (solve-quadractic a b c)))
        (when ts
          (let* ((closest (find-if (lambda (hit) (> hit 0)) ts))
                 (hitpoint (evaluate ray closest))
                 (normal (normalize (sub hitpoint (sphere-pos object)))))
            (make-hit-record :object object :t closest :normal normal)))))))
