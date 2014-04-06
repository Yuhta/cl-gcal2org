(asdf:defsystem #:cl-gcal2org
  :serial t
  :author "Jimmy Lu <gongchuo.lu@gmail.com>"
  :depends-on (#:alexandria
               #:cl-json
               #:cl-ppcre
               #:flexi-streams
               #:local-time
               #:oauth2
               #+ccl #:osicat)
  :components ((:file "package")
               (:file "cl-gcal2org")))
