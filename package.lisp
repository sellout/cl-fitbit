(defpackage fitbit
  (:use #:cl #:drakma #:oauth #:json)
  (:export #:activities-for)
  (:shadow #:request))
