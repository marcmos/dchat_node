(defmodule dchat_sockserv_event
  (export all))

(defrecord state
  sockserv
  nick) ;; only for sync calls (like login)

(defun init (sockserv)
  (tuple 'ok (make-state sockserv sockserv)))

(defun handle_event
  (((tuple 'received msg) state)
   (let ((sockserv (state-sockserv state))
         (nick (state-nick state)))
     (case msg
       ((tuple 'login (nick))
        (handle_login nick state))
       ((tuple 'message (to body))
        (handle_message nick to body state))
       ((tuple 'logout ())
        (handle_logout state)))))
  (('accepted state)
   (tuple 'ok state)))

;; Internal
(defun handle_login (nick state)
  (let ((sockserv (state-sockserv state)))
    (case (dchat_user_serv:register nick sockserv)
      ('ok
       (dchat_sockserv:logged_in sockserv)
       (tuple 'ok (set-state-nick state nick))))))

(defun handle_message (from to body state)
  (case (dchat_user_serv:lookup to)
    ('not_exists
     ;; oops, unknown recipient
     ;; FIXME deal with it
     (tuple 'ok state))
    (pid ;; TODO add is_pid guard
     (dchat_sockserv:message pid from body)
     (tuple 'ok state))))

(defun handle_logout (state)
  (dchat_user:logout (state-nick state))
  (dchat_sockserv:logged_out (state-sockserv state))
  ;; do self-remove
  (tuple 'ok state)
  )