(in-package #:fitbit)

;;; NOTE: The API is documented at http://wiki.fitbit.com/display/API

(defconstant +api-version+ 1)
(defvar +base-url+ (uri "http://api.fitbit.com/"))
(defvar +request-url+ (merge-uris "/oauth/request_token" +base-url+))
(defvar +auth-request-url+ (merge-uris "/oauth/authorize" +base-url+))
(defvar +access-url+ (merge-uris "/oauth/access_token" +base-url+))

(defgeneric parse-value (existing-object slot-name slot-type value)
  (:method (existing-object slot-name slot-type value)
    (cond ((subtypep (find-class slot-type) 'user-proxy)
           (parse-json slot-type value :parent existing-object))
          ((eq slot-type 'list)
           (mapcar (lambda (v)
                     (parse-json (find-symbol (string-right-trim
                                               '(#\S)
                                               (symbol-name slot-name))
                                              :fitbit)
                                 v
                                 :parent existing-object))
                   value))
          (t value))))

(defun parse-json
    (class json &key parent (existing-object (make-instance class)))
  (mapcar (lambda (pair)
            (let ((field (car pair))
                  (value (cdr pair)))
              (handler-case
                  (let* ((slot-name (find-symbol (symbol-name field) :fitbit))
                         (type (slot-definition-type
                                (find slot-name
                                      (class-slots (find-class class))
                                      :key #'slot-definition-name))))
                    (setf (slot-value existing-object slot-name)
                          (parse-value existing-object slot-name type value)))
                (error (c)
                  (warn "Ignoring value ~a for unsupported field ~a: ~a"
                        value field c)))))
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
   (encoded-id :initarg :encoded-id)
   (full-name :reader full-name)
   (gender :reader gender)
   (height :reader height)
   (nickname :reader nickname)
   (offset-from-+utc+-millis :reader offset-from-utc)
   (state :reader state)
   (stride-length-running :reader running-stride-length)
   (stride-length-walking :reader walking-stride-length)
   (timezone :reader timezone)))

(defmethod print-object ((obj user) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "~a" (display-name obj))))

(defclass authorized-user (user)
  ((access-token :initarg access-token)
   (unit-system :initform nil :initarg :unit-system :accessor unit-system)))

(defclass user-proxy ()
  ((parent :initarg :parent :accessor parent))
  (:documentation "All objects carry a reference to the object from which they
                   were created. This allows us to not have to pass around
                   multiple user objects everywhere."))

(defclass target-user (user-proxy user)
  ())

(defun make-user (encoded-id authorized-user)
  (make-instance 'target-user :encoded-id encoded-id :parent authorized-user))

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
  (id
   (min-speed-+mph+ :reader minimum-speed)
   (max-speed-+mph+ :reader maximum-speed)
   (name :reader name)))

(defmethod print-object ((obj activity-level) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "~a" (name obj))))

(defclass activity (user-proxy)
  ((activity-levels :reader levels :type list)
   (has-speed :reader has-speed-p)
   id
   (name :reader name)))

(defmethod print-object ((obj activity) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "~a" (name obj))))

(defclass activity-instance (user-proxy)
  ((activity :accessor activity)
   (activity-id)
   (calories :reader calories)
   (description :reader description)
   (distance :initarg :distance :accessor distance)
   (duration :initarg :duration :accessor duration)
   (name :reader name)))

(defmethod print-object ((obj activity-instance) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "~a" (name obj))))

(defmethod slot-unbound :around
    (class (instance activity-instance) (slot-name (eql 'activity)))
  (declare (ignorable class))
  (setf (slot-value instance slot-name)
        (%activity instance (slot-value instance 'activity-id))))

(defmethod (setf activity) :before (activity (instance activity-instance))
  (setf (slot-value instance 'activity-id) (slot-value activity 'id)))

(defclass activity-summary (user-proxy)
  ((active-score :reader active-score)
   (calories-out :reader calories-out)
   (distances :reader distances)
   (fairly-active-minutes :reader time-fairly-active)
   (lightly-active-minutes :reader time-lightly-active)
   (sedentary-minutes :reader time-sedentary)
   (steps :reader steps)
   (very-active-minutes :reader time-very-active)))

(defclass device (user-proxy)
  ((battery :reader battery)
   id
   (type :reader type)))

(defclass unit (user-proxy)
  (id
   (name :reader name)
   (plural :reader plural)))

(defmethod print-object ((obj unit) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "~a" (name obj))))

(defclass food (user-proxy)
  ((brand :reader brand)
   food-id
   (name :reader name)
   (units :reader units)))

(defmethod print-object ((obj food) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "~a" (name obj))))

(defclass food-instance (user-proxy)
  ((amount :initarg amount :accessor amount)
   (brand :reader brand)
   (calories :reader calories)
   food-id
   (food :reader food)
   meal-type-id
   (meal-type :reader meal-type)
   (name :reader name)
   (unit :initarg unit :accessor unit)
   (units :reader units)))

(defmethod print-object ((obj food-instance) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "~a" (name obj))))

#| FIXME: no way to find food or meal-type object?
(defmethod slot-unbound :around
    (class (instance activity-instance) (slot-name (eql 'food)))
  (declare (ignorable class))
  (setf (slot-value instance slot-name)
        (%food instance (slot-value instance 'food-id))))

(defmethod slot-unbound :around
    (class (instance activity-instance) (slot-name (eql 'meal-type)))
  (declare (ignorable class))
  (setf (slot-value instance slot-name)
        (%meal-type instance (slot-value instance 'meal-type-id))))
|#

(defclass food-log-entry (user-proxy)
  ((is-favorite :reader favorite-p)
   (log-date :reader date)
   log-id
   (logged-food :reader food-instance :type food-instance)
   (nutritional-values :reader nutritional-values :type nutritional-values)))

(defmethod print-object ((obj food-log-entry) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "~a" (name (food-instance obj)))))

(defclass nutritional-values (user-proxy)
  ((calories :reader calories)
   (carbs :reader carbs)
   (fat :reader fat)
   (fiber :reader fiber)
   (protein :reader protein)
   (sodium :reader sodium)))

(defclass food-summary (nutritional-values)
  ((water :reader water)))

(defun make-fitbit-consumer (key secret)
  (make-consumer-token :key key :secret secret))

(defun get-authentication-url (consumer &optional callback-url)
  (let ((token (apply #'obtain-request-token +request-url+ consumer
                      (if callback-url (list :callback-uri callback-url)))))
    (make-authorization-uri +auth-request-url+ token)))

(defgeneric get-access-token (consumer auth-response)
  (:documentation "Auth response can be either a full URI or an alist of the
                   query parameters.")
  (:method (consumer (auth-response list))
    (let* ((token (cdr (assoc "oauth_token" auth-response :test #'string=)))
           (verifier (cdr (assoc "oauth_verifier" auth-response
                                 :test #'string=)))
           (request-token (make-request-token :key token
                                              :verification-code verifier)))
      (setf (request-token-authorized-p request-token) t)
      (obtain-access-token +access-url+ request-token
                           :consumer-token consumer)))
  (:method (consumer (auth-response uri))
    (get-access-token consumer
                      (mapcar (lambda (param)
                                (let ((list (split-sequence #\= param)))
                                  (cons (first list) (second list))))
                              (split-sequence #\& (uri-query auth-response))))))

(defun get-authorized-user (consumer auth-response &key unit-system)
  (make-instance 'authorized-user
                 'access-token (get-access-token consumer auth-response)
                 :unit-system unit-system))

(defgeneric request
    (instance resource-url &key method parameters additional-headers)
  (:method ((instance authorized-user) resource-url
            &key (method :get) parameters additional-headers)
    (let ((result (access-protected-resource
                   (merge-uris (format nil
                                       "/~a~a.json"
                                       +api-version+
                                       resource-url)
                               +base-url+)
                   (slot-value instance 'access-token)
                   :request-method method
                   :user-parameters parameters
                   :additional-headers (append (case (unit-system instance)
                                                 (:us '(("Accept-Language" . "en_US")))
                                                 (:uk '(("Accept-Language" . "en_UK")))
                                                 (otherwise '()))
                                               additional-headers))))
      (decode-json-from-string (if (stringp result)
                                   result
                                   (babel:octets-to-string result)))))
  (:method ((instance user-proxy) resource-url
            &rest args &key (method :get) parameters additional-headers)
    (declare (ignore method parameters additional-headers))
    (apply #'request (slot-value instance 'parent) resource-url args)))

(defgeneric user-id? (user)
  (:documentation "This gets the user-id of the “nearest” user to the object, or
                   “-” in the case of an authorized user.")
  (:method ((instance authorized-user)) "-")
  (:method ((instance target-user)) (slot-value instance 'encoded-id))
  (:method ((instance user-proxy)) (user-id? (slot-value instance 'parent))))

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
  (let ((json (request user
                       (format nil "/user/~a/foods/log/date/~a"
                               (user-id? user) date))))
    (values (mapcar (lambda (object)
                      (parse-json 'food-log-entry object :parent user))
                    (cdr (first json)))
            (parse-json 'food-summary (cdr (second json)) :parent user))))

(defun recent-foods (user)
  (mapcar (lambda (object) (parse-json 'food-instance object :parent user))
          (request user
                   (format nil "/user/~a/foods/log/recent" (user-id? user)))))

(defun frequent-foods (user)
  (mapcar (lambda (object) (parse-json 'food-instance object :parent user))
          (request user
                   (format nil "/user/~a/foods/log/frequent" (user-id? user)))))

(defun favorite-foods (user)
  (mapcar (lambda (object) (parse-json 'food-instance object :parent user))
          (request user
                   (format nil "/user/~a/foods/log/favorite" (user-id? user)))))

;; NOTE: this is internal and is wrapped to receive specific resources
(defun time-series
    (user resource-path &key start-date end-date period)
  "Returns an ordered alist of (timestamp . value). END-DATE must be provided,
   along with exactly one of START-DATE and PERIOD."
  ;; FIXME: ensure end-date and exactly one of start-date and period are
  ;;        provided
  (mapcar (lambda (pair) (cons (cdr (first pair)) (cdr (second pair))))
          (cdar (request user
                         (format nil "/user/~a~a/date/~a/~a"
                                 (user-id? user)
                                 resource-path
                                 (or start-date end-date)
                                 (or period end-date))))))

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
           :method :post
           :parameters `(("weight" . ,(format nil "~a" weight))
                         ("date" . ,date)))
  weight)

(defgeneric log (instance)
  (:method ((instance activity-instance))
    (request instance
             (format nil "/user/~a/activities" (user-id? instance))
             :method :post
             :parameters `(("activityId" . ,(slot-value instance 'activity-id))
                           ("durationMillis" . ,(duration instance))
                           ("distance" . ,(distance instance)) 
                           ;;("date")
                           ;;("startTime")
                           )))
  (:method ((instance food-instance))
    (request instance
             (format nil "/user/~a/foods/log" (user-id? instance))
             :method :post
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
             :method :post))
  (:method ((instance food))
    (request instance
             (format nil "/user/~a/foods/log/favorite/~a"
                     (user-id? instance) (slot-value instance 'food-id))
             :method :post)))

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
                     (user-id? instance) (slot-value instance 'food-id))
             :method :delete)))

;;; General Activity Data

(defun %activity (proxy activity-id)
  (parse-json 'activity
              (cdr (first (request proxy
                                   (format nil "/activities/~a" activity-id))))
              :parent proxy))

;;; General Food Data

(defun search-foods (proxy query)
  (mapcar (lambda (object) (parse-json 'food object :parent proxy))
          (cdar (request proxy "/foods/search"
                         :parameters `(("query" . ,query))))))

(defun food-units (proxy)
  (mapcar (lambda (object) (parse-json 'unit object :parent proxy))
          (request proxy "/foods/units")))

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
