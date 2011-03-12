(defpackage fitbit
  (:use #:cl #:drakma #:oauth #:json #:split-sequence #:puri)
  (:export #:activities-for)
  (:shadow #:request))
