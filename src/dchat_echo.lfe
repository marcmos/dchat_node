;;; Example echo event handler
;;; Use as template for handler used by sockserv_serv
(defmodule dchat_echo
  (export (start_link 0)
          (init 1)
          (terminate 2)
          (handle_event 2)
          (handle_call 2)
          (handle_info 2)
          (code_change 3)))

;;; API
(defun start_link ()
  (let (((tuple 'ok pid) (gen_event:start_link (tuple 'local (MODULE)))))
    (gen_event:add_handler pid (MODULE) ())
    (tuple 'ok pid)))

;;; gen_event callback functions
(defun init
  ((()) (tuple 'ok ())))

(defun terminate (reason state)
  'ok)

(defun handle_event
  (((tuple 'msg msg) state)
   (broadcast-msg msg state)
   (tuple 'ok state))
  (((tuple 'accept pid) state)
   (tuple 'ok (cons pid state)))
  ((event state) (tuple 'ok state)))

(defun handle_call
  ((request state) (tuple 'ok 'ok state)))

(defun handle_info
  ((info state) (tuple 'ok state)))

(defun code_change (old-vsn state extra)
  (tuple 'ok state))

;;; Internal functions
(defun send-msg (dest msg)
  (gen_server:cast dest (tuple 'send msg)))

(defun broadcast-msg (msg users)
  (lists:map (lambda (user) (send-msg user msg)) users))
