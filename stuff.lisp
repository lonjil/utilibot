(in-package #:utilibot)

(defun command-context (body)
  `(lambda (context)
     (with-accessors ((args args) (author author)
                                 (channel-id channel-id)
                                 (guild-id guild-id) (message-id message-id)
                                 (message message))
                    context
                  (flet ((reply (format-string &rest arguments)
                           (b:send-message
                            channel-id
                            (apply #'format nil format-string arguments))))
                    ,@body))))
(defmacro define-bot-command (name (&key documentation
                                      short-help
                                      long-help)
                              &body body)
  `(progn
     (let ((f ,(command-context body)))
       (setf (gethash ,name *bot-commands*)
             (make-instance 'command :name ,name
                                     :func f :docs ,documentation
                                     :short-help ,short-help
                                     :long-help ,long-help)))))

(defmacro private (&body b)
  `(when (equal (b:id author) (ubi:value 'owner))
     ,@b))
