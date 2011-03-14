(defpackage fitbit
  (:use #:cl #:drakma #:oauth #:json #:split-sequence #:puri #:closer-mop)
  (:shadowing-import-from #:cl #:standard-method #:standard-generic-function
                               #:defmethod #:defgeneric #:standard-class)
  (:shadow #:log #:request)
  (:export #:make-fitbit-consumer
           #:get-authentication-url
           #:get-authorized-user
           #:make-user

           #:user
           #:about-me #:city #:country #:date-of-birth #:display-name
           #:full-name #:gender #:height #:nickname #:offset-from-utc #:state
           #:running-stride-length #:walking-stride-length #:timezone

           #:authorized-user
           #:unit-system
           
           #:activities-for
           #:recent-activities #:frequent-activities #:favorite-activities

           #:activity #:activity-level #:activity-instance #:activity-summary
           #:activity-levels #:has-speed-p #:name
           #:minimum-speed #:maximum-speed
           #:calories #:description #:distance #:duration
           #:active-score #:calories-out #:distances
           #:time-fairly-active #:time-lightly-active #:time-sedentary #:steps
           #:time-very-active
           
           #:foods-for #:recent-foods #:frequent-foods #:favorite-foods

           #:food #:food-instance #:food-log-entry #:nutritional-values
           #:food-summary #:unit
           #:brand #:name #:units
           #:amount #:calories #:meal-type
           #:favorite-p #:date #:nutritional-values
           #:calories #:carbs #:fat #:fiber #:protein #:sodium
           #:water
           #:plural
           
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

           #:devices

           #:device #:battery #:type))
