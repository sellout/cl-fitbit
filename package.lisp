(defpackage fitbit
  (:use #:cl #:drakma #:oauth #:json #:split-sequence #:puri)
  (:shadow #:request)
  (:export #:make-fitbit-consumer
           #:get-authentication-url
           #:get-authorized-user
           
           #:activities-for
           #:recent-activities #:frequent-activities #:favorite-activities
           #:profile
           #:foods-for #:recent-foods #:frequent-foods #:favorite-foods
           
           #:calories-in

           #:calories-out
           #:steps
           #:distance
           #:time-sedentary
           #:time-lightly-active
           #:time-fairly-active
           #:time-very-active
           #:active-score

           #:time-asleep
           #:time-awake
           #:awakenings
           #:time-in-bed

           #:weight
           #:bmi
           #:fat

           #:activity
           #:search-foods
           #:food-units
           
           #:devices
           #:device-attributes))
