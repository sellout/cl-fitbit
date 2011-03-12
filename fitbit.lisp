(in-package #:fitbit)

;;; NOTE: The API is documented at http://wiki.fitbit.com/display/API

(defconstant +api-version+ 1)
(defvar +base-url+ (uri "http://api.fitbit.com/"))
(defvar +request-url+ (merge-uris "/oauth/request_token" +base-url+))
(defvar +auth-request-url+ (merge-uris "/oauth/authorize" +base-url+))
(defvar +access-url+ (merge-uris "/oauth/access_token" +base-url+))

(defun parse-json (class json &optional (existing-object (make-instance class)))
  (mapcar (lambda (pair)
            (setf (slot-value existing-object
                              (find-symbol (symbol-name (car pair)) :fitbit))
                  (cdr pair)))
          json)
  existing-object)

(define-condition fitbit-error (error)
  ((error-type)
   (field-name)
   (message)))

(defclass user ()
  ((about-me :reader about-me)
   (city :reader city)
   (country :reader country)
   (date-of-birth :reader date-of-birth)
   (display-name :reader display-name)
   (encoded-id :initarg :encoded-id :reader encoded-id)
   (full-name :reader full-name)
   (gender :reader gender)
   (height :reader height)
   (nickname :reader nickname)
   (offset-from-+utc+-millis :reader offset-from-+utc+-millis)
   (state :reader state)
   (stride-length-running :reader stride-length-running)
   (stride-length-walking :reader stride-length-walking)
   (timezone :reader timezone)
   (access-token :initarg :access-token :reader access-token)))

(defmethod slot-unbound :around
    ((class (eql (find-class 'user))) instance slot-name)
  (profile instance)
  (if (slot-boundp instance slot-name)
      (slot-value instance slot-name)
      (call-next-method)))

(defun parse-user (json &optional (existing-user (make-instance 'user)))
  (let ((object (car json)))
    (assert (eq (car object) :user))
    (parse-json 'user (cdr object) existing-user)))

(defclass activity-level ()
  (id min-speed max-speed name))

(defclass activity ()
  (activity-levels has-speed-p id name))

(defclass activity-instance ()
  ((activity)
   (activity-id)
   (calories)
   (description)
   (distance)
   (duration)
   (name)))

(defclass device ()
  (battery id type))

(defclass food ()
  (brand id name units))

(defclass food-unit ()
  (name plural id))

(defun make-fitbit-consumer (key secret)
  (make-consumer-token :key key :secret secret))

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

(defun request
    (authorized-user resource-url &key (request-method :get) parameters)
  (let ((result (access-protected-resource
                 (merge-uris (format nil
                                     "/~a~a.json"
                                     +api-version+
                                     resource-url)
                             +base-url+)
                 (access-token authorized-user)
                 :request-method request-method
                 :user-parameters parameters)))
    (decode-json-from-string (if (stringp result)
                                 result
                                 (babel:octets-to-string result)))))

(defun user-id? (user)
  "This gets the user-id, or “-” if the user is NIL."
  (if user (encoded-id user) "-"))

(defun activities-for (authorized-user date &optional user)
  (request authorized-user
           (format nil "/user/~a/activities/date/~a" (user-id? user) date)))

(defun recent-activities (authorized-user &optional user)
  (mapcar (lambda (object) (parse-json 'activity-instance object))
          (request authorized-user
                   (format nil "/user/~a/activities/recent" (user-id? user)))))

(defun frequent-activities (authorized-user &optional user)
  (mapcar (lambda (object) (parse-json 'activity-instance object))
          (request authorized-user
                   (format nil "/user/~a/activities/frequent"
                           (user-id? user)))))

(defun favorite-activities (authorized-user &optional user)
  (mapcar (lambda (object) (parse-json 'activity-instance object))
          (request authorized-user
                   (format nil "/user/~a/activities/favorite"
                           (user-id? user)))))

(defun profile (authorized-user &optional user)
  (let ((json (request authorized-user
                       (format nil "/user/~a/profile" (user-id? user)))))
    (if user
        (parse-user json user)
        (parse-user json authorized-user))))

(defun foods-for (authorized-user date &optional user)
  (request authorized-user
           (format nil "/user/~a/foods/log/date/~a" (user-id? user) date)))

(defun recent-foods (authorized-user &optional user)
  (request authorized-user
           (format nil "/user/~a/foods/log/recent" (user-id? user))))

(defun frequent-foods (authorized-user &optional user)
  (request authorized-user
           (format nil "/user/~a/foods/log/frequent" (user-id? user))))

(defun favorite-foods (authorized-user &optional user)
  (request authorized-user
           (format nil "/user/~a/foods/log/favorite" (user-id? user))))

;; NOTE: this is internal and is wrapped to receive specific resources
(defun resource
    (authorized-user resource-path &key start-date end-date period user)
  ;; FIXME: must provide end-date and exactly one of start-date and period
  (request authorized-user
           (format nil "/user/~a~a/date/~a/~a"
                   (user-id? user)
                   resource-path
                   (or start-date end-date)
                   (or period end-date))))

(defmacro defresource (name path)
  `(defun ,name
       (authorized-user &rest args &key start-date end-date period user)
     (declare (ignore start-date end-date period user))
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

(defun %activity (authorized-user activity-id)
  (request authorized-user (format nil "/activities/~a" activity-id)))

(defun search-foods (authorized-user query)
  (request authorized-user "/foods/search" :parameters `(("query" . ,query))))

(defun food-units (authorized-user)
  (request authorized-user "/foods/units"))

(defun devices (authorized-user &optional user)
  (mapcar (lambda (object) (parse-json 'device object))
          (request authorized-user
                   (format nil "/user/~a/devices" (user-id? user)))))

(defun device-attributes (authorized-user device &optional user)
  (parse-json 'device
              (car (request authorized-user
                            (format nil "/user/~a/devices/~a"
                                    (user-id? user) (slot-value device 'id))))
              device))



;;; Logging User Data



(defmethod (setf weight) (weight authenticated-user date &optional user)
  (request authenticated-user
           (format nil "/user/~a/body/weight" (user-id? user))
           :request-method :post
           :parameters `(("weight" . ,(format nil "~a" weight))
                         ("date" . ,date)))
  weight)