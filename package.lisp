(defpackage fitbit
  (:use #:cl #:drakma #:oauth #:json #:split-sequence #:puri #:closer-mop)
  (:shadowing-import-from #:cl #:standard-method #:standard-generic-function
                               #:defmethod #:defgeneric #:standard-class)
  (:shadow #:request)
  (:export #:make-fitbit-consumer
           #:get-authentication-url
           #:get-authorized-user
           #:make-user
           
           #:activities-for
           #:recent-activities #:frequent-activities #:favorite-activities
           #:foods-for #:recent-foods #:frequent-foods #:favorite-foods
           
           #:calories-in-time-series

           #:calories-out-time-series
           #:steps-time-series
           #:distance-time-series
           #:time-sedentary-time-series
           #:time-lightly-active-time-series
           #:time-fairly-active-time-series
           #:time-very-active-time-series
           #:active-score-time-series

           #:time-asleep-time-series
           #:time-awake-time-series
           #:awakenings-time-series
           #:time-in-bed-time-series

           #:weight-time-series
           #:bmi-time-series
           #:fat-time-series

           #:log #:add-favorite

           #:delete-log #:delete-favorite

           #:search-foods
           #:food-units
           
           #:devices))
