(defpackage fitbit-system
  (:use #:cl #:asdf))

(in-package #:fitbit-system)

(defsystem fitbit
  :depends-on (drakma cl-oauth cl-json local-time)
  :serial t
  :components ((:file "package")
               (:file "fitbit")))
