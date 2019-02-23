;;;; utilibot.asd

(asdf:defsystem #:utilibot
  :description "Describe utilibot here"
  :author "Birk Hirdman <lonjil@gmail.com>"
  :license  "Give me your soul"
  :version "0.0.1"
  :serial t
  :depends-on (#:queues #:cl-harmony #:bordeaux-threads
               #:ubiquitous-concurrent)
  :components ((:file "package")
               (:file "utilibot")))
