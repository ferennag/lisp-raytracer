(in-package :raytracer)

(defclass material () ())

(defgeneric material-evaluate (material l v n))

(defclass material-lambertian ()
  ((albedo :type color :initarg :albedo :accessor lambertian-albedo)))

(defun material-lambertian (color)
  (make-instance 'material-lambertian :albedo color))

(defmethod material-evaluate ((material material-lambertian) l v n)
  (lambertian-albedo material))

(defclass light () ())

(defgeneric illuminate (light eye ray hit))

(defclass point-light (light)
  ((position :type vec3 :initarg :position :initform (vec3 0 0 0) :accessor light-position)
   (color :type color :initarg :color :initform (vec3 1 1 1) :accessor light-color)))


(defmethod illuminate ((light point-light) (eye vec3) ray hit)
  (let* ((point (evaluate ray (hit-record-t hit)))
         (lightvector (sub (light-position light)
                           point))
         (L (normalize lightvector))
         (N (hit-record-normal hit))
         (V (normalize (sub point eye)))
         (E (div (mul (max 0 (dot N L))
                      (light-color light))
                 (len lightvector)))
         (k (material-evaluate (hittable-material (hit-record-object hit))
                               L
                               V
                               N)))
    (mul k E)))
