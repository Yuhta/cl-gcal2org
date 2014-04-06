(defpackage #:cl-gcal2org
  (:nicknames #:gcal2org)
  (:use #:cl
        #:alexandria
        #:local-time)
  (:import-from #+sbcl #:sb-posix
                #+(or ecl clisp) #:ext
                #+ccl #:ccl
                #:getenv)
  (:import-from #:oauth2
                #:token-string))
