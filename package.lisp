;;;; package.lisp

(defpackage #:utilibot
  (:use #:cl
        #:bt #:split-sequence
        #:alexandria)
  (:local-nicknames
                    (#:b #:xyz.lonjil.discord/base)
                    (#:l #:xyz.lonjil.discord/logger)
                    (#:unicode #:cl-unicode)
                    (#:ubi #:ubiquitous)
                    )
  (:shadow #:message))
