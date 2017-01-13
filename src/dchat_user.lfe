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

(defun login (ref nick)
  (gen_fsm:send_event ref (tuple 'login nick)))

(defun send (ref from msg)
  (gen_fsm:send_event ref (tuple 'send from msg)))

(defun message (ref to msg)
  (gen_fsm:send_event ref (tuple 'message to msg)))

(defun logout (ref)
  (gen_fsm:send_event ref 'logout))

;;; gen_fsm callbacks
(defun init (state)
  (tuple 'ok 'not_logged_in state))

(defun terminate (reason state data)
  'void)

;;; State functions
(defun not_logged_in
  (((tuple 'login nick) state)
   (case (dchat_user_serv:register nick)
     ('ok
      (dchat_sockserv:logged_in (state-sockserv state))
      (tuple 'next_state 'logged_in (set-state-nick state nick))))))

(defun logged_in
  ;; world → sockserv
  (((tuple 'send from msg) state)
   (dchat_sockserv:message (state-sockserv state) from msg)
   (tuple 'next_state 'logged_in state))
  ;; sockserv → world
  (((tuple 'message to msg) state)
   (dchat_node:send (state-nick state) to msg)
   (tuple 'next_state 'logged_in state))
  (((tuple 'logout) state)
   ;; TODO implement
   ))