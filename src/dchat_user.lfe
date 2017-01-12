(defmodule dchat_user
  (export all)) ;; FIXME add exports

;;; API
(defun start_link (sockserv)
  (gen_fsm:start_link (MODULE) sockserv ()))

(defun handle (ref event)
  (gen_fsm:sync_send_event ref event))

;;; gen_fsm callbacks
(defun init (sockserv)
  (tuple 'ok 'not_logged_in sockserv))

(defun terminate (reason state data)
  'void)

;;; State functions
(defun not_logged_in
  (((tuple 'login (list nick)) _ sockserv)
   ;; TODO register self in users_serv
   (dchat_sockserv_serv:send sockserv (tuple 'logged_in ()))
   (tuple 'reply
          (tuple 'logged_in ())
          'logged_in
          sockserv)))