;;;; raytracer.asd

(asdf:defsystem #:raytracer
  :description "Describe raytracer here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (:sdl3)
  :components ((:file "package")
               (:file "math")
               (:file "color")
               (:file "ray")
               (:file "light")
               (:file "object")
               (:file "camera")
               (:file "raytracer")))
