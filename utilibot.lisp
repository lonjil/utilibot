;;;; utilibot.lisp

(in-package #:utilibot)

(ubi:restore (merge-pathnames "config/foo.config.lisp" (uiop:getcwd)))

(defclass command ()
  ((%name :accessor name :initarg :name)
   (%func :accessor func :initarg :func)
   (%docs :accessor docs :initarg :docs)
   (%short-help :accessor short-help :initarg :short-help)
   (%long-help :accessor long-help :initarg :long-help)))
(defclass context ()
  ((%args :accessor args :initarg :args :type string)
   (%author :accessor author :initarg :author :type b:user)
   (%channel_id :accessor channel-id :initarg :channel-id)
   (%guild_id :accessor guild-id :initarg :guild-id)
   (%message_id :accessor message-id :initarg :message-id)
   (%message :accessor message :initarg :message :type string)))

(defvar *bot-commands* (make-hash-table :test 'equal))
(defvar *helpers* (list))



(defparameter *prefix* "&")

(defparameter *help*
  (format nil "```~%~{&~a~%~}```"
          '("version: prints the bot version"
            "ping: measures the time the bot takes to send a message"
            "fortune [type]: prints a fortune of <type>, or any if none is specified"
            "fortune -list: lists fortune types"
            "help: prints this message"
            "roll [[<count>]d]<faces>[<+|-><modifier>]: roll <count> dice with <number> faces, plus <modifier>")))

(defvar *fortune-types* "art
ascii-art
computers
cookie
debian
definitions
drugs
education
ethnic
food
fortunes
goedel
humorists
kids
knghtbrd
law
linux
linuxcookie
literature
love
magic
medicine
men-women
miscellaneous
news
off
paradoxum
people
perl
pets
platitudes
politics
riddles
science
songs-poems
sports
startrek
theo
translate-me
void
wisdom
work
zippy")

(unless (boundp '+whitespace+)
  (defconstant +whitespace+
    (let ((w (unicode:list-all-characters "Whitespace")))
      (make-array (length w) :element-type 'character :adjustable nil
                             :fill-pointer nil :initial-contents w))))
(defun whitespace-p (char)
  "Returns true if CHAR is in +WHITESPACE+."
  (find char +whitespace+ :test 'char=))

(defun parse-dice (string)
  (let* ((dice (split-sequence #\d string))
         (count (if (cdr dice) (if (equal (car dice) "") "1" (car dice)) "1"))
         (faces (if (cdr dice) (cadr dice) (car dice)))
         (%modifier (let* ((+ (find #\+ faces))
                           (- (find #\- faces))
                           (m (cond (+ (split-sequence #\+ faces))
                                    (- (split-sequence #\- faces))
                                    (t (list faces nil))))
                           (n (cadr m)))
                     (if n
                         (let ((i (parse-integer n :junk-allowed t)))
                           (cond ((null i) (cons nil (car m)))
                                 ((and (not +) -) (cons (- i) (car m)))
                                 (t (cons i (car m)))))
                         (cons nil (car m)))))
         (modifier (car %modifier))
         (foo (cdr %modifier)))
    (setf faces foo)
    (list (cons :modifier modifier)
          (cons :count (parse-integer count))
          (cons :faces (parse-integer faces)))))


(define-bot-command "source" (:short-help "git link")
  (reply "http://github.com/lonjil/utilibot"))
(define-bot-command "test" ()
  (reply "maid!zap FV5 bunny girl"))
(define-bot-command "pronouns" ()
  (reply "They use they/them pronouns, friend."))
(define-bot-command "version" (:short-help "current version")
  (reply "utilibot 0.3 everlasting alpha (cl-harmony 0.2)"))
(define-bot-command "sh" ()
  (private (reply "```~a```"
                  (handler-case
                      (uiop:run-program args :output '(:string :stripped t))
                    (uiop:subprocess-error () (abort))))))
(define-bot-command "units" (:short-help "in value and unit [; out unit]"
                             :long-help "See units(1)")
  (let* ((o (handler-case
                (uiop:run-program
                 (cons "units" (cons "--" (split-sequence #\; args)))
                 :output '(:string :stripped t))
              (uiop:subprocess-error () (abort))))
         (o (format nil "~{~a~^~%~}"
                    (mapcar (lambda (x) (string-trim +whitespace+ x))
                            (split-sequence #\Newline o)))))
    (reply "```~a```" o)))
(define-bot-command "fortune" ()
  (if (equal (car (split-sequence #\Space args)) "-list")
      (reply "```~a```" *fortune-types*)
      (reply "```~a```"
             (handler-case
                 (uiop:run-program `("fortune" ,args)
                                   :output '(:string :stripped t))
               (uiop:subprocess-error () (abort))))))
(define-bot-command "help" ()
  (reply *help*))
(define-bot-command "bash" ()
  (reply "https://www.youtube.com/watch?v=f7mRG88KPbA"))
(define-bot-command "roll" ()
  (handler-case
      (let* ((x (parse-dice args))
             (count (cdr (assoc :count x)))
             (faces (cdr (assoc :faces x)))
             (modifier (cdr (assoc :modifier x)))
             (foo (cond
                    ((> count 1000000) nil)
                    ((> count 300)
                     (loop repeat count
                           for x = (1+ (random faces))
                           sum x into total
                           finally (return (cons nil total))))
                    (t (loop repeat count
                             for x = (1+ (random faces))
                             collect x into casts
                             sum x into total
                             finally (return (cons casts total))))))
             (total (cdr foo))
             (casts (car foo))
             (mod-string (format nil "~:[~;~:[~;+~]~a=~a~]"
                                 modifier
                                 (positive-integer-p modifier)
                                 modifier
                                 (when (numberp modifier)
                                   (+ total modifier))))
             (string (cond
                       ((> count 1000000) "Too many dice.")
                       ((> count 300)
                        (format nil "Total: ~a~a, mean: ~3$"
                                total
                                mod-string
                                (/ total count)))
                       ((> count 1)
                        (format nil "~{~a~^, ~}~% Total: ~a~a, mean: ~3$"
                                casts
                                total
                                mod-string
                                (/ total count)))
                       ((= count 1)
                        (format nil "~a~a"
                                total
                                mod-string)))))
        (unless (plusp faces)
          (error 'type-error :expected-type 'positive-integer :datum faces))
        (unless (plusp count)
          (error 'type-error :expected-type 'positive-integer :datum count))
        (format t "~ad~a~%" count faces)
        (reply "<@~a>: ~a" (getjso "id" author) string))
    ((or parse-error type-error) (error)
      (warn (format nil "~a" error))
      (reply "lmao are you stupid (note: if you're not actually stupid, please file a bug report)")
      (abort))))


(defun update-invite (old new)
  (let ((uses (b:uses old))
        (nuses (b:uses new))
        (gid (b:id (b:guild old)))
        (code (b:code old)))
    (setf (ubi:value 'guild gid 'invites code 'b::%uses) nuses)
    (values old
            (not (= uses nuses)))))

(defvar *invites* (make-hash-table :test 'equal))
(defun update-or-add-invite (invite)
  (with-accessors ((code b:code) (uses b:uses) (guild b:guild))
      invite
    (let ((it (ubi:value 'guild (b:id guild) 'invites code)))
     (if it
         (update-invite it invite)
         (values (setf (ubi:value 'guild (b:id guild) 'invites code)
                       invite)
                 (not (= 0 uses)))))))

(defun prep-invites (gid)
  (let ((invites (b:get-invites gid)))
    (loop :for i :in invites
          :do (update-or-add-invite i))))

(defun update-invites-and-return-changed (invites)
  (loop :for i :in invites
        :for x := (multiple-value-list (update-or-add-invite i))
        :if (cadr x)
          :collect (car x)))

(defun invite-string (x)
  (format nil "(~a to <#~a> by ~a#~a (~a))"
          (b:code x) (b:id (b:channel x))
          (b:username (b:inviter x))
          (b:discrim (b:inviter x))
          (b:id (b:inviter x))))
(defun comp-invite (member)
  (with-accessors ((gid b:guild-id))
      member
    (when (ubi:value 'guild gid 'invite-check-p)
      (let* ((invs (b:get-invites gid))
             (ui (update-invites-and-return-changed invs))
             (user (b:user member))
             (uid (b:id user))
             (name (b:username user))
             (discrim (b:discrim user))
             (c (format nil "<@~a> (~a#~a): ~{~a~^, ~}"
                        uid name discrim
                        (if ui
                            (loop :for x :in ui
                                  :collect (invite-string x))
                            (list "No invites seem to have changed.")))))
        (b:send-message (ubi:value 'guild gid 'invite-log) c)))))

(defun report-changed-invites (gid &optional reason)
  (when (ubi:value 'guild gid 'invite-check-p)
    (let* ((invs (b:get-invites gid))
           (ui (update-invites-and-return-changed invs)))
      (when ui
        (b:send-message
         (ubi:value 'guild gid 'invite-log)
         (format nil "Manual check of changed invites: ~{~a~^, ~}~
~@[~%~%Note from lonjil: ~a~]"
                 (loop :for x :in ui
                       :collect (invite-string x))
                 reason))))))

(defun globally-report-changed-invites (&optional reason)
  (maphash-keys
   (lambda (k)
     (report-changed-invites k reason))
   (ubi:value 'guild)))

(define-bot-command "update-invites" ()
  (private
    (globally-report-changed-invites
     (if (and args (> 3 (length args)))
         args))
    (reply "All done.")))


(defun event-handler (event data)
  (cond
    ((equal "READY" event) nil)
    ((equal "GUILD_MEMBER_ADD" event)
     (comp-invite (json-mop:json-to-clos data 'b:guild-member)))
    ((equal "MESSAGE_CREATE" event)
     (on-message (json-mop:json-to-clos data 'b:message)))
    ((equal "PRESENCE_UPDATE" event) nil)
    ((equal "TYPING_START" event) nil)
    ((equal "MESSAGE_UPDATE" event) nil)
    ((equal "MESSAGE_REACTION_ADD" event) nil)
    (t)))

(defun on-message (message)
  (with-accessors ((id b:id) (author b:author) (chid b:channel-id)
                   (guild-id b:guild-id) (content b:content)
                   (member b:member))
      message
    (let ((user-id (b:id author))
          (prefix nil))
      (when (>= (length content) 2)
        (setf prefix (subseq content 0 1)))
      (when (and (equal chid (ubi:value 'consent-channel))
                 (equal content "I have read the rules."))
        (b:add-member-role guild-id user-id (ubi:value 'user-role))
        (b:send-message chid "Noted."))
      (when (equal prefix "&")
        (unless (b:bot? author)
          (multiple-value-bind
                (cmd index)
              (split-sequence #\Space (subseq content 1) :count 1)
            (let* ((args (subseq content (1+ index)))
                   (cmd (car cmd))
                   (cmd (gethash cmd *bot-commands*)))
              (when (and cmd (func cmd))
                (funcall (func cmd) (make-instance 'context
                                            :message content
                                            :message-id id
                                            :guild-id guild-id
                                            :channel-id chid
                                            :author author
                                            :args args))))))))))

(b:new-discord
 (with-open-file (foo "./key.txt")
   (read-line foo))
 'event-handler)

(define-bot-command "yt" ()
  (let ((o (handler-case
               (concatenate
                'string
                "https://youtu.be/"
                (uiop:run-program
                 (list "youtube-dl" "--get-id" "--"
                       (concatenate 'string "ytsearch:" args))
                 :output '(:string :stripped t)))
             (uiop:subprocess-error () "There was an error."))))
    (reply o)))

(define-bot-command "eval" ()
  (private
    (let ((*package* (find-package :utilibot))
          (s (make-array 0 :fill-pointer 0 :adjustable t
                           :element-type 'character)))
      (with-output-to-string (*standard-output* s)
        (let* ((form (read-from-string args))
               (ret (handler-case
                       (funcall (compile nil (command-context (list form)))
                                context)
                     (t (c) c))))
         (reply "~@[~a~%~]~s" (if (> (length s) 0) s nil) ret))))))
