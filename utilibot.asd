;;;; utilibot.asd

(asdf:defsystem #:utilibot
  :description "Describe utilibot here"
  :author "Birk Hirdman <lonjil@gmail.com>"
  :license  "MIT"
  :version "0.3.0"
  :serial t
  :depends-on (#:queues #:cl-harmony #:bordeaux-threads
               #:ubiquitous-concurrent)
  :components ((:file "package")
               (:file "utilibot")))
