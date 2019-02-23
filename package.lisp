;;;; package.lisp

(defpackage #:utilibot
  (:use #:cl
        #:bt #:split-sequence
        #:alexandria #:metabang.bind)
  (:local-nicknames
                    (#:b #:xyz.lonjil.discord/base)
                    (#:l #:xyz.lonjil.discord/logger)
                    (#:unicode #:cl-unicode)
                    (#:ubi #:ubiquitous)
                    )
  (:shadow eval #:message))
