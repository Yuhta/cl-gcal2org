(in-package #:cl-gcal2org)

#+ecl
(defun c-stat-mode (filename)
  (ffi:clines "#include <sys/stat.h>")
  (ffi:c-inline (filename) (:cstring) :unsigned-int
    "{
       struct stat s;
       stat (#0, &s);
       @(return) = s.st_mode;
     }"
    :side-effects nil))

#+(or sbcl ecl)
(defun permission (path)
  #+sbcl (sb-posix:stat-mode (sb-posix:stat path))
  #+ecl (c-stat-mode (namestring path)))

#+(or sbcl ecl)
(defun check-permission (path &optional (expected #o600))
  (when-let* ((path (probe-file path))
              (actual (logand (permission path) #o777)))
    (unless (= actual expected)
      (error "Permission of ~s is #o~o; should be #o~o"
             path actual expected))))

#+clisp
(defun check-permission/clisp (path &optional (expected '(:RUSR :WUSR)))
  (when-let* ((path (probe-file path))
              (actual (butlast (posix:file-stat-mode
                                (posix:file-stat path)))))
    (unless (equal actual expected)
      (error "Permission of ~s is ~a; should be ~a"
             path actual expected))))

(defun oauth-token-file ()
  (let ((path (or (getenv "CL_GCAL2ORG_OAUTH_TOKEN_FILE")
                  (format nil "/var/tmp/cl-gcal2org-~a-oauth-token"
                          (getenv "USER")))))
    #-clisp (check-permission path)
    #+clisp (check-permission/clisp path)
    path))

#+ecl
(defun c-umask (mask)
  (ffi:clines "#include <sys/stat.h>")
  (ffi:c-inline (mask) (:unsigned-int) :unsigned-int
    "umask (#0)" :one-liner t))

#+(or sbcl ecl clisp)
(defun umask (mask)
  #+sbcl (sb-posix:umask mask)
  #+ecl (c-umask mask)
  #+clisp (posix:umask mask))

(defmacro with-umask (mask &body forms)
  (with-gensyms (old-mask)
    `(let ((,old-mask (umask ,mask)))
       (unwind-protect (progn
                         ,@forms)
         (umask ,old-mask)))))

(defun save-oauth-token (token)
  (with-umask #o177
    (with-open-file (out (oauth-token-file) :direction :output)
      (format out "~s~%" token)))
  token)

(define-constant +oauth-client-id+
  "200701486136-td9daeuse8917ai8v4ojgpkf8ofblvv6.apps.googleusercontent.com"
  :test #'string=)

(define-constant +oauth-client-secret+
  "n45wv56dJvFOqW6eaDz9Td7g" :test #'string=)

(define-constant +oauth-client-params+
  `(("client_id" . ,+oauth-client-id+)
    ("client_secret" . ,+oauth-client-secret+))
  :test #'equal)

(define-constant +oauth-redirect-uri+
  "urn:ietf:wg:oauth:2.0:oob" :test #'string=)

(define-constant +google-oauth-token-url+
  "https://accounts.google.com/o/oauth2/token" :test #'string=)

(defun get-oauth-token ()
  (format t "~&Go to~%~a~%and enter the code you got: "
          (let ((*standard-output* (make-broadcast-stream)))
            (oauth2:request-code
             "https://accounts.google.com/o/oauth2/auth"
             +oauth-client-id+
             :scope "https://www.googleapis.com/auth/calendar.readonly"
             :redirect-uri +oauth-redirect-uri+)))
  (finish-output)
  (save-oauth-token (oauth2:request-token +google-oauth-token-url+
                                          (read-line)
                                          :redirect-uri +oauth-redirect-uri+
                                          :method :post
                                          :other +oauth-client-params+)))

(defun oauth-token ()
  (if-let ((path (probe-file (oauth-token-file))))
    (oauth2:refresh-token +google-oauth-token-url+
                          (with-open-file (in path)
                            (read in))
                          :method :post
                          :other +oauth-client-params+)
    (get-oauth-token)))

(defun around (timestamp before after)
  (let ((today (timestamp-minimize-part timestamp :hour)))
    (list (timestamp- today before :day)
          (timestamp+ today after :day))))

(defun list-events-around (timestamp calendar-id
                           &key
                             (external-format :utf-8)
                             (before 7) (after 7))
  (json:decode-json-from-string
   (flexi-streams:octets-to-string
    (oauth2:request-resource
     (format nil
             "https://www.googleapis.com/calendar/v3/calendars/~a/events"
             (drakma:url-encode calendar-id external-format))
     (oauth-token)
     :parameters (acons "singleEvents" "true"
                        (pairlis '("timeMin" "timeMax")
                                 (mapcar (curry #'format-timestring nil)
                                         (around timestamp
                                                 before after)))))
    :external-format external-format)))

(defun parse-datestring (datestring)
  (apply #'encode-timestamp 0 0 0 0
         (nreverse (map 'list
                        #'parse-integer
                        (second
                         (multiple-value-list
                          (ppcre:scan-to-strings "(\\d{4})-(\\d{2})-(\\d{2})"
                                                 datestring)))))))

(define-constant +date-format+
    '((:year 4) #\- (:month 2) #\- (:day 2) #\Space :short-weekday)
  :test #'equal)

(defun format-date (timestamp)
  (format-timestring nil timestamp :format +date-format+))

(defun format-date-time (timestamp)
  (format-timestring nil timestamp
                     :format (append +date-format+
                                     '(#\Space (:hour 2) #\: (:min 2)))))

(defun org-timestamp (start end)
  (assert (eq (car start) (car end)))
  (ecase (car start)
    (:date
     (let ((start-date (parse-datestring (cdr start)))
           (end-date (timestamp- (parse-datestring (cdr end)) 1 :day)))
       (if (timestamp= start-date end-date)
           (format nil "<~a>" (format-date start-date))
           (format nil "~{<~a>~^--~}"
                   (mapcar #'format-date (list start-date end-date))))))
    (:date-time
     (let ((start-time (parse-timestring (cdr start)))
           (end-time (parse-timestring (cdr end))))
       (if (timestamp= (timestamp-minimize-part start-time :hour)
                       (timestamp-minimize-part end-time :hour))
           (format nil "<~a-~2,'0d:~2,'0d>"
                   (format-date-time start-time)
                   (timestamp-hour end-time)
                   (timestamp-minute end-time))
           (format nil "~{<~a>~^--~}"
                   (mapcar #'format-date-time
                           (list start-time end-time))))))))

(defun parse-events (events)
  (mapcar (lambda (item)
            (nconc `(:headline ,(assoc-value item :summary)
                     :timestamp ,(org-timestamp (second (assoc :start item))
                                                (second (assoc :end item))))
                   (when-let ((description (assoc-value item :description)))
                     `(:description ,description))))
          (assoc-value events :items)))

(defun write-org (title items &optional (stream *standard-output*))
  (format stream "~&* ~a~%" title)
  (dolist (i items)
    (format stream "** ~a~%   ~a~%"
            (getf i :headline) (getf i :timestamp))
    (when-let ((d (getf i :description)))
      (format stream "~a~%" d))))

(defun calendars-to-org (calendar-ids)
  (let ((now (now)))
    (dolist (id calendar-ids)
      (write-org id (parse-events (list-events-around now id))))))

(defun all-calendar-ids ()
  (let ((response (json:decode-json-from-string
                   (flexi-streams:octets-to-string
                    (oauth2:request-resource
                     "https://www.googleapis.com/calendar/v3/users/me/calendarList"
                     (oauth-token))
                    :external-format :utf-8))))
    (mapcar (rcurry #'assoc-value :id)
            (assoc-value response :items))))

(defun main (&optional args)
  (let ((calendar-ids (rest args)))
    (if calendar-ids
        (calendars-to-org calendar-ids)
        (format t "~&~{~a~%~}" (all-calendar-ids)))))
