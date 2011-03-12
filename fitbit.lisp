(in-package #:fitbit)

;;; NOTE: The API is documented at http://wiki.fitbit.com/display/API

(defconstant +api-version+ 1)
(defvar +base-url+ (uri "http://api.fitbit.com/"))
(defvar +request-url+ (merge-uris "/oauth/request_token" +base-url+))
(defvar +auth-request-url+ (merge-uris "/oauth/authorize" +base-url+))
(defvar +access-url+ (merge-uris "/oauth/access_token" +base-url+))

(defclass user ()
  (about-me city country date-of-birth display-name encoded-id full-name gender
   height nickname offset-from-utc state stride-length-running
   stride-length-walking timezone
   (access-token :initarg :access-token :reader access-token)))

(defclass activity-level ()
  (id min-speed max-speed name))

(defclass activity ()
  (activity-levels has-speed-p id name))

(defclass device ()
  (battery id))

(defclass food ()
  (brand id name units))

(defclass food-unit ()
  (name plural id))

(defun make-fitbit-consumer (key secret)
  (make-consumer-token :key key :secret secret))

;; NOTE: setting up oauth
(defun get-authentication-url (consumer &optional callback-url)
  (let ((token (apply #'obtain-request-token +request-url+ consumer
                      (if callback-url (list :callback-uri callback-url)))))
    (make-authorization-uri +auth-request-url+ token)))

(defun get-access-token (consumer uri)
  (let* ((query (mapcar (lambda (param) (split-sequence #\= param))
                        (split-sequence #\& (uri-query uri))))
         (token (cadr (assoc "oauth_token" query :test #'string=)))
         (verifier (cadr (assoc "oauth_verifier" query :test #'string=)))
         (request-token (make-request-token :key token
                                            :verification-code verifier)))
    (setf (request-token-authorized-p request-token) t)
    (obtain-access-token +access-url+ request-token :consumer-token consumer)))

(defun get-authorized-user (consumer uri)
  (make-instance 'user :access-token (get-access-token consumer uri)))

(defun request (authorized-user resource-url &optional query)
  (let ((result (access-protected-resource
                 (merge-uris (format nil
                                     "/~a~a.json~@[?query=~a~]"
                                     +api-version+
                                     resource-url
                                     (when query
                                       (url-encode query)))
                             +base-url+)
                 (access-token authorized-user)
                 :request-method :auth)))
    (decode-json-from-string (if (stringp result)
                                 result
                                 (babel:octets-to-string result)))))

(defun activities-for (authorized-user date &optional user-id)
  (request authorized-user
           (format nil "/user/~a/activities/date/~a" (or user-id "-") date)))

(defun recent-activities (authorized-user &optional user-id)
  (request authorized-user
           (format nil "/user/~a/activities/recent" (or user-id "-"))))

(defun frequent-activities (authorized-user &optional user-id)
  (request authorized-user
           (format nil "/user/~a/activities/frequent" (or user-id "-"))))

(defun favorite-activities (authorized-user &optional user-id)
  (request authorized-user
           (format nil "/user/~a/activities/favorite" (or user-id "-"))))

(defun profile (authorized-user &optional user-id)
  (request authorized-user
           (format nil "/user/~a/profile" (or user-id "-"))))

(defun foods-for (authorized-user date &optional user-id)
  (request authorized-user
           (format nil "/user/~a/foods/log/date/~a" (or user-id "-") date)))

(defun recent-foods (authorized-user &optional user-id)
  (request authorized-user
           (format nil "/user/~a/foods/log/recent" (or user-id "-"))))

(defun frequent-foods (authorized-user &optional user-id)
  (request authorized-user
           (format nil "/user/~a/foods/log/frequent" (or user-id "-"))))

(defun favorite-foods (authorized-user &optional user-id)
  (request authorized-user
           (format nil "/user/~a/foods/log/favorite" (or user-id "-"))))

;; NOTE: this is internal and is wrapped to receive specific resources
(defun resource
    (authorized-user resource-path &key start-date end-date period user-id)
  ;; FIXME: must provide end-date and exactly one of start-date and period
  (request authorized-user
           (format nil "/user/~a~a/date/~a/~a"
                   (or user-id "-")
                   resource-path
                   (or start-date end-date)
                   (or period end-date))))

(defmacro defresource (name path)
  `(defun ,name
       (authorized-user &rest args &key start-date end-date period user-id)
     (declare (ignore start-date end-date period user-id))
     (apply #'resource authorized-user ,path args)))

(defresource calories-in "/foods/log/caloriesIn")

(defresource calories-out "/activities/log/calories")
(defresource steps "/activities/log/steps")
(defresource distance "/activities/log/distance")
(defresource time-sedentary "/activities/log/minutesSedentary")
(defresource time-lightly-active "/activities/log/minutesLightlyActive")
(defresource time-fairly-active "/activities/log/minutesFairlyActive")
(defresource time-very-active "/activities/log/minutesVeryActive")
(defresource active-score "/activities/log/activeScore")
;; (defresource calories-out "/activities/log/activityCalories")

(defresource time-asleep "/sleep/minutesAsleep")
(defresource time-awake "/sleep/minutesAwake")
(defresource awakenings "/sleep/awakeningsCount")
(defresource time-in-bed "/sleep/timeInBed")

(defresource weight "/body/weight")
(defresource bmi "/body/bmi")
(defresource fat "/body/fat")

(defun activity (authorized-user activity-id)
  (request authorized-user (format nil "/activities/~a" activity-id)))

(defun search-foods (authorized-user query)
  (request authorized-user "/foods/search" query))

(defun food-units (authorized-user)
  (request authorized-user "/foods/units"))

(defun devices (authorized-user &optional user-id)
  (request authorized-user (format nil "/user/~a/devices" (or user-id "-"))))

(defun device-attributes (authorized-user device-id &optional user-id)
  (request authorized-user
           (format nil "/user/~a/devices/~a" (or user-id "-") device-id)))
