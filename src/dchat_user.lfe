(defmodule dchat_user
  (export all)) ;; FIXME add exports

(defrecord state
  sockserv
  nick)

;;; API
(defun start_link (sockserv)
  (gen_fsm:start_link (MODULE) (make-state sockserv sockserv) ()))

(defun handle (ref event)
  (gen_fsm:send_event ref event))

(defun send (ref sender msg)
  (gen_fsm:send_event ref (tuple 'send sender msg)))

;;; gen_fsm callbacks
(defun init (state)
  (tuple 'ok 'not_logged_in state))

(defun terminate (reason state data)
  'void)

;;; State functions
(defun not_logged_in
  (((tuple 'login (list nick)) state)
   (dchat_user_serv:register nick (self))
   (dchat_sockserv_serv:send (state-sockserv state) (tuple 'logged_in ()))
   (tuple 'next_state 'logged_in (set-state-nick state nick))))

(defun logged_in
  (((tuple 'message (list user msg)) state)
   (dchat_user:send (dchat_user_serv:lookup user)
                    (state-nick state)
                    msg)
   (tuple 'next_state 'logged_in state))
  (((tuple 'send sender msg) state)
   (dchat_sockserv_serv:send (state-sockserv state)
                       (tuple 'message (list sender msg)))
   (tuple 'next_state 'logged_in state)))