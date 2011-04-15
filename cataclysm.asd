;;;; cataclysm.asd

(asdf:defsystem #:cataclysm
  :serial t
  :depends-on (#:lispbuilder-sdl)
  :components ((:file "package")
               (:file "cataclysm")))

