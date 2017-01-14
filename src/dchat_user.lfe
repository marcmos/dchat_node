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
  (process_flag 'trap_exit 'true)
  (tuple 'ok 'not_logged_in state))

(defun terminate
  ((_ 'logged_in state)
   (dchat_user_serv:unregister (state-nick state)))
  ((reason state-name state-data)
  'void))

(defun handle_info
  ;; FIXME make this process less suicidal
  (((tuple 'EXIT _ _) state-name state-data)
   (tuple 'stop 'normal state-data))
  ((_ state-name state-data)
   (tuple 'next_state state-name state-data)))

;;; State functions
(defun not_logged_in
  (((tuple 'login nick) state)
   (case (dchat_user_serv:register nick)
     ('ok
      ;;(dchat_sockserv:logged_in (state-sockserv state))
      (add_servers (state-sockserv state))
      (tuple 'next_state 'logged_in (set-state-nick state nick)))
     ('nick_taken
      (dchat_sockserv:nick_taken (state-sockserv state) nick)
      (tuple 'next_state 'not_logged_in state)))))

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
   (dchat_user_serv:unregister (state-nick state))
   (tuple 'stop 'normal state)))

(defun add_servers (sockserv)
  (lists:map (lambda (host) (dchat_sockserv:add_server sockserv host))
             (dchat_directory_serv:get)))