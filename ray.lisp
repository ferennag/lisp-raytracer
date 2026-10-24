(in-package :raytracer)

(defstruct ray
  (origin (vec3 0 0 0) :type vec3)
  (direction (vec3 0 0 1) :type vec3))

(defun evaluate (ray t0)
  (with-slots (origin direction) ray
    (add origin (mul direction t0))))
