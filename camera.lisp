(in-package :raytracer)

(defstruct camera
  (position (vec3 0 0 0) :type vec3)
  (fov (deg2rad 60.0) :type single-float)
  (focal-length 1.0 :type single-float))

(defstruct camera-plane
  (aspect 1.0 :type single-float)
  (w 1.0 :type single-float)
  (h 1.0 :type single-float)
  (z 0.0 :type single-float)
  (vx (vec3 0 0 0))
  (vy (vec3 0 0 0))
  (px0 (vec3 0 0 0)))

(defun camera-plane (camera aspect w h)
  (let* ((plane-h (* 2.0 (camera-focal-length camera) (tan (/ (camera-fov camera) 2.0))))
         (plane-w (* plane-h aspect))
         (plane-z (+ (vec3-z (camera-position camera)) (camera-focal-length camera)))
         (px0 (vec3 (- (/ plane-w 2))
                    (/ plane-h 2)
                    plane-z))
         (vx (vec3 (/ plane-w w) 0 0))
         (vy (vec3 0 (/ (- plane-h) h) 0)))
    (make-camera-plane :aspect aspect
                :w plane-w
                :h plane-h
                :z plane-z
                :vx vx
                :vy vy
                :px0 px0)))

(defun pixel-vector (plane x y)
  (add* (camera-plane-px0 plane)
        (mul (camera-plane-vx plane) (+ x 0.5))
        (mul (camera-plane-vy plane) (+ y 0.5))))
