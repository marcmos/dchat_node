;; sockserv socket events handler.
(defmodule dchat_sockserv_event
  (export all))

(defrecord state
  user)

(defun init (sockserv)
  (case (dchat_user_sup:start_user sockserv)
    ((tuple 'ok pid)
     (link pid)
     (tuple 'ok (make-state user pid)))))

(defun handle_event
  (('accepted state)
   (conn_pool_serv:accept 'dchat_conn_pool_serv)
   (tuple 'ok state))
  (((tuple 'received msg) state)
   (let ((user (state-user state)))
     (case msg
       ((tuple 'login (nick))
        (dchat_user:login (state-user state) nick))
       ((tuple 'message (to body))
        (dchat_user:message (state-user state) to body))
       ((tuple 'logout ())
        (dchat_user:logout (state-user state))))
     (tuple 'ok state)))
  ((_ state)
   (tuple 'ok state)))
