(in-package #:fitbit)

;;; NOTE: The API is documented at http://wiki.fitbit.com/display/API

(defconstant +api-version+ 1)
(defvar +base-url+ (uri "http://api.fitbit.com/"))
(defvar +request-url+ (merge-uris "/oauth/request_token" +base-url+))
(defvar +auth-request-url+ (merge-uris "/oauth/authorize" +base-url+))
(defvar +access-url+ (merge-uris "/oauth/access_token" +base-url+))

(defun parse-json
    (class json &key parent (existing-object (make-instance class)))
  (mapcar (lambda (pair)
            (setf (slot-value existing-object
                              (find-symbol (symbol-name (car pair)) :fitbit))
                  (cdr pair)))
          json)
  (if parent
      (setf (slot-value existing-object 'parent) parent))
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
   (timezone :reader timezone)))

(defclass authorized-user (user)
  ((access-token :initarg :access-token :reader access-token)
   (unit-system :initform nil :initarg :unit-system :accessor unit-system)))

(defclass user-proxy ()
  ((parent :initarg :parent :accessor parent :type parent))
  (:documentation "All objects carry a reference to the object from which they
                   were created. This allows us to not have to pass around
                   multiple user objects everywhere."))

(defclass target-user (user-proxy user)
  ())

(defmethod slot-unbound :around (class (instance user) slot-name)
  (declare (ignorable class))
  (profile instance)
  (if (slot-boundp instance slot-name)
      (slot-value instance slot-name)
      (call-next-method)))

(defun parse-user (json existing-user)
  (let ((object (car json)))
    (assert (eq (car object) :user))
    (parse-json 'user (cdr object) :existing-object existing-user)))

(defclass activity-level (user-proxy)
  (id min-speed max-speed name))

(defclass activity (user-proxy)
  (activity-levels has-speed-p id name))

(defclass activity-instance (user-proxy)
  ((activity :accessor activity)
   (activity-id)
   (calories :reader calories)
   (description :reader description)
   (distance :initarg :distance :accessor distance)
   (duration :initarg :duration :accessor duration)
   (name :reader name)))

(defmethod slot-unbound :around
    (class (instance activity-instance) (slot-name (eql 'activity)))
  (declare (ignorable class))
  (setf (slot-value instance slot-name)
        (%activity instance (slot-value instance 'activity-id))))

(defmethod (setf activity) :before (activity (instance activity-instance))
  (setf (slot-value instance 'activity-id) (slot-value activity 'id)))

(defclass activity-summary (user-proxy)
  (active-score calories-out distances fairly-active-minutes
   lightly-active-minutes sedentary-minutes steps very-active-minutes))

(defclass device (user-proxy)
  (battery id type))

(defclass food (user-proxy)
  (brand id name units))

(defclass food-instance (user-proxy)
  ((amount :initarg amount :accessor amount)
   (brand :reader brand)
   (calories :reader calories)
   food-id
   meal-type-id
   (name :reader name)
   (unit :initarg unit :accessor unit)
   units))

(defclass food-unit (user-proxy)
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

(defun get-authorized-user (consumer uri &key unit-system)
  (make-instance 'authorized-user
                 :access-token (get-access-token consumer uri)
                 :unit-system unit-system))

(defgeneric request
    (user resource-url &key request-method parameters)
  (:method ((instance authorized-user) resource-url
            &key (request-method :get) parameters)
    (let ((result (access-protected-resource
                   (merge-uris (format nil
                                       "/~a~a.json"
                                       +api-version+
                                       resource-url)
                               +base-url+)
                   (access-token instance)
                   :request-method request-method
                   :user-parameters parameters
                   :additional-headers (case (unit-system instance)
                                         (:us '(("Accept-Language" . "en_US")))
                                         (:uk '(("Accept-Language" . "en_UK")))
                                         (otherwise '())))))
      (decode-json-from-string (if (stringp result)
                                   result
                                   (babel:octets-to-string result)))))
  (:method ((instance user-proxy) resource-url &rest args
            &key (request-method :get) parameters)
    (declare (ignore request-method parameters))
    (apply #'request (parent instance) resource-url args)))

(defgeneric user-id? (user)
  (:documentation "This gets the user-id of the “nearest” user to the object, or
                   “-” in the case of an authorized user.")
  (:method ((instance authorized-user)) "-")
  (:method ((instance target-user))     (encoded-id instance))
  (:method ((instance user-proxy))      (user-id? (parent instance))))

(defun activities-for (user date)
  (let ((json (request user
                       (format nil "/user/~a/activities/date/~a"
                               (user-id? user) date))))
    (values (mapcar (lambda (object) (parse-json 'activity-instance object
                                                 :parent user))
                    (cdr (first json)))
            (parse-json 'activity-summary (cdr (second json)) :parent user))))

(defun recent-activities (user)
  (mapcar (lambda (object) (parse-json 'activity-instance object :parent user))
          (request user
                   (format nil "/user/~a/activities/recent" (user-id? user)))))

(defun frequent-activities (user)
  (mapcar (lambda (object) (parse-json 'activity-instance object :parent user))
          (request user
                   (format nil "/user/~a/activities/frequent"
                           (user-id? user)))))

(defun favorite-activities (user)
  (mapcar (lambda (object) (parse-json 'activity-instance object :parent user))
          (request user
                   (format nil "/user/~a/activities/favorite"
                           (user-id? user)))))

(defun profile (user)
  (parse-user (request user (format nil "/user/~a/profile" (user-id? user)))
              user))

(defun foods-for (user date)
  (request user
           (format nil "/user/~a/foods/log/date/~a" (user-id? user) date)))

(defun recent-foods (user)
  (request user
           (format nil "/user/~a/foods/log/recent" (user-id? user))))

(defun frequent-foods (user)
  (request user (format nil "/user/~a/foods/log/frequent" (user-id? user))))

(defun favorite-foods (user)
  (request user (format nil "/user/~a/foods/log/favorite" (user-id? user))))

;; NOTE: this is internal and is wrapped to receive specific resources
(defun time-series
    (user resource-path &key start-date end-date period)
  ;; FIXME: must provide end-date and exactly one of start-date and period
  (request user
           (format nil "/user/~a~a/date/~a/~a"
                   (user-id? user)
                   resource-path
                   (or start-date end-date)
                   (or period end-date))))

(defmacro define-time-series (name path)
  `(defun ,(intern (format nil "~a-TIME-SERIES" (symbol-name name)))
       (user &rest args &key start-date end-date period)
     (declare (ignore start-date end-date period))
     (apply #'time-series user ,path args)))

(define-time-series calories-in "/foods/log/caloriesIn")

(define-time-series calories-out "/activities/log/calories")
(define-time-series steps "/activities/log/steps")
(define-time-series distance "/activities/log/distance")
(define-time-series time-sedentary "/activities/log/minutesSedentary")
(define-time-series time-lightly-active "/activities/log/minutesLightlyActive")
(define-time-series time-fairly-active "/activities/log/minutesFairlyActive")
(define-time-series time-very-active "/activities/log/minutesVeryActive")
(define-time-series active-score "/activities/log/activeScore")
;; (define-time-series calories-out "/activities/log/activityCalories")

(define-time-series time-asleep "/sleep/minutesAsleep")
(define-time-series time-awake "/sleep/minutesAwake")
(define-time-series awakenings "/sleep/awakeningsCount")
(define-time-series time-in-bed "/sleep/timeInBed")

(define-time-series weight "/body/weight")
(define-time-series bmi "/body/bmi")
(define-time-series fat "/body/fat")

;;; Logging User Data

(defmethod (setf weight) (weight user date)
  (request user
           (format nil "/user/~a/body/weight" (user-id? user))
           :request-method :post
           :parameters `(("weight" . ,(format nil "~a" weight))
                         ("date" . ,date)))
  weight)

(defgeneric log (instance)
  (:method ((instance activity-instance))
    (request instance
             (format nil "/user/~a/activities" (user-id? instance))
             :request-method :post
             :parameters `(("activityId" . ,(slot-value instance 'activity-id))
                           ("durationMillis" . ,(duration instance))
                           ("distance" . ,(distance instance)) 
                           ;;("date")
                           ;;("startTime")
                           )))
  (:method ((instance food-instance))
    (request instance
             (format nil "/user/~a/foods/log" (user-id? instance))
             :request-method :post
             :parameters `(("foodId" . ,(slot-value instance 'food-id))
                           ("mealTypeId" . ,(slot-value instance
                                                            'meal-type-id))
                           ("unitId" . ,(slot-value (unit instance) 'id))
                           ("amount" . ,(amount instance))
                           ;;("date")
                           ))))

(defgeneric add-favorite (instance)
  (:method ((instance activity))
    (request instance
             (format nil "/user/~a/activities/log/favorite/~a"
                     (user-id? instance) (slot-value instance 'id))
             :request-method :post))
  (:method ((instance food))
    (request instance
             (format nil "/user/~a/foods/log/favorite/~a"
                     (user-id? instance) (slot-value instance 'id))
             :request-method :post)))

;;; Deleting User Data

(defgeneric delete-log (instance)
  (:method ((instance activity-instance))
    (request instance
             (format nil "/user/~a/activities/~a"
                     (user-id? instance) (slot-value instance 'id))
             :method :delete))
  (:method ((instance food-instance))
    (request instance
             (format nil "/user/~a/foods/log/~a"
                     (user-id? instance) (slot-value instance 'id))
             :method :delete)))

(defgeneric delete-favorite (instance)
  (:method ((instance activity))
    (request instance
             (format nil "/user/~a/activities/log/favorite/~a"
                     (user-id? instance) (slot-value instance 'id))
             :method :delete))
  (:method ((instance food))
    (request instance
             (format nil "/user/~a/foods/favorite/~a"
                     (user-id? instance) (slot-value instance 'id))
             :method :delete)))

;;; General Activity Data

(defun %activity (proxy activity-id)
  (request proxy (format nil "/activities/~a" activity-id)))

;;; General Food Data

(defun search-foods (proxy query)
  (request proxy "/foods/search" :parameters `(("query" . ,query))))

(defun food-units (proxy)
  (request proxy "/foods/units"))

;;; Device Data

(defun devices (user)
  (mapcar (lambda (object) (parse-json 'device object :parent user))
          (request user (format nil "/user/~a/devices" (user-id? user)))))

(defun device-attributes (device)
  (parse-json 'device
              (car (request device
                            (format nil "/user/~a/devices/~a"
                                    (user-id? device) (slot-value device 'id))))
              :existing-object device))
