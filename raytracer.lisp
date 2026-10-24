;;;; raytracer.lisp

(in-package #:raytracer)

(defmacro check-sdl (&body body)
  "Check for SDL errors after executing an SDL function. Throws an error if an SDL error was detected"
  `(let ((result (progn ,@body)))
     (unless result
       (error (format nil "SDL Error: ~a" (sdl3:get-error))))
     result))

(defstruct canvas
  pixels
  width
  height)

(defstruct app
  window
  renderer
  texture
  canvas
  (width 800)
  (height 600)
  (running t))

(defun main ()
  (unwind-protect
       (progn
         (sdl3:init :video)
         (let* ((window (check-sdl (sdl3:create-window "Raytracer" 800 600 '(:resizable))))
                (renderer (check-sdl (sdl3:create-renderer window (cffi:null-pointer))))
                (app (make-app :window window
                               :renderer renderer)))
           (resize app 800 600)
           (loop while (app-running app)
                 do (process-events app)
                 do (update app)
                 do (render renderer app))))
    (sdl3:quit)))

(defun process-events (app)
  (loop for event = (sdl3:poll-event*)
        while event
        do (process-event event app)))

(defun process-event (event app)
  (typecase event
    (sdl3:quit-event
     (setf (app-running app) nil))
    (sdl3:keyboard-event
     (case (slot-value event 'sdl3::%key)
       (:escape
        (setf (app-running app) nil))))
    (sdl3:window-event
     (case (slot-value event 'sdl3::%type)
       (:window-resized
        (resize app
                (slot-value event 'sdl3::%data-1)
                (slot-value event 'sdl3::%data-2)))))))

(defun resize (app width height)
  (setf (app-width app) width)
  (setf (app-height app) height)
  (when (app-texture app)
    (sdl3:destroy-texture (app-texture app)))
  (setf (app-texture app) (sdl3:create-texture (app-renderer app)
                                               :rgba8888
                                               :streaming
                                               width
                                               height))
  (setf (app-canvas app) (make-canvas :pixels (make-array (list height width) :initial-element (color 0.0 0.0 0.0))
                                      :width width
                                      :height height))
  (run-raytracer (app-canvas app)))

(defun update (app)
  (cffi:with-foreign-objects ((pixels :pointer)
                              (pitch :int))
    (check-sdl (sdl3:lock-texture (app-texture app) nil pixels pitch))
    (unwind-protect
         (progn
           (dotimes (y (app-height app))
             (dotimes (x (app-width app))
               (let* ((stride (cffi:mem-aref pitch :int))
                      (data (cffi:mem-aref pixels :pointer))
                      (offset (+ (* y stride) (* x 4)))
                      (pixel (aref (canvas-pixels (app-canvas app)) y x)))
                                        ; alpha
                 (setf (cffi:mem-aref data :uint8 offset) 255)
                                        ; blue
                 (setf (cffi:mem-aref data :uint8 (+ offset 1)) (float-to-uint8 (color-b pixel)))
                                        ; green
                 (setf (cffi:mem-aref data :uint8 (+ offset 2)) (float-to-uint8 (color-g pixel)))
                                        ; red
                 (setf (cffi:mem-aref data :uint8 (+ offset 3)) (float-to-uint8 (color-r pixel)))))))
      (sdl3:unlock-texture (app-texture app)))))

(defun render (renderer app)
  (sdl3:set-render-draw-color-float renderer 0.0 0.0 0.0 1.0)
  (check-sdl (sdl3:render-clear renderer))
  (check-sdl (sdl3:render-texture renderer (app-texture app) nil nil))
  (check-sdl (sdl3:render-present renderer)))

(defun clear-canvas (canvas &optional (background (color 0.0 0.0 0.0)))
  (dotimes (y (canvas-height canvas))
    (dotimes (x (canvas-width canvas))
      (setf (aref (canvas-pixels canvas) y x) background))))

(defun run-raytracer (canvas)
  (clear-canvas canvas)
  (let* ((objects (make-instance 'object :children (list (sphere (vec3 -2 0 5) 1.0 (material-lambertian (color 1.0 0.0 0.0)))
                                                         (sphere (vec3 2 0 5) 0.5 (material-lambertian (color 0.0 1.0 0.0))))))
         (lights (list
                  (make-instance 'point-light :position (vec3 0 5 0) :color (color 3.0 3.0 3.0))
                  (make-instance 'point-light :position (vec3 0 -5 0) :color (color 1.0 1.0 1.0))))
         (camera (make-camera :position (vec3 0 0 -5)))
         (w (canvas-width canvas))
         (h (canvas-height canvas))
         (aspect (/ (float w) (float h)))
         (plane (camera-plane camera aspect w h)))
    (dotimes (y h)
      (dotimes (x w)
        (let* ((px (pixel-vector plane x y))
               (eye-vector (normalize (sub px (camera-position camera))))
               (ray (make-ray :origin (camera-position camera) :direction eye-vector))
               (hit (intersect objects ray)))
          (when hit
            (let ((color (shade-ray ray (camera-position camera) hit lights)))
              (setf (aref (canvas-pixels canvas) y x)
                    color))))))))

(defun shade-ray (ray eye hit lights)
  (let ((results (loop for light in lights
                       for color = (illuminate light eye ray hit)
                       collect color)))
    (reduce #'add results :initial-value (color 0 0 0))))
