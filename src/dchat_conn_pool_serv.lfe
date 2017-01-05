(defmodule dchat_conn_pool_serv
  (export (start_link 2)
          (accept 0)
          (init 1)
          (terminate 2)
          (handle_call 3)
          (handle_cast 2)
          (handle_info 2)
          (code_change 3)))

(defrecord state
  listen-socket
  worker-sup)

;;; API
(defun start_link (port handler)
  (gen_server:start_link (tuple 'local (MODULE))
                         (MODULE)
                         (tuple (self) port handler) ()))

(defun accept ()
  (gen_server:cast (MODULE) 'accept))

;;; gen-server callbacks
(defun init
  (((tuple parent-sup port handler))
   (case (listen port)
     ((tuple 'ok socket)
      (! (self) (tuple 'start_worker_sup parent-sup handler))
      (accept)
      (tuple 'ok (make-state listen-socket socket))))))

(defun terminate
  (('normal state)
   (gen_tcp:close (state-listen-socket state))
   'ok)
  ((reason state) (error_logger:error "~p (~p) terminated abnormally: ~p~n"
                                      (list (MODULE) (self) reason))))

;; No-op handle call
(defun handle_call (e from state)
  (tuple 'noreply state))

(defun handle_cast
  (('accept state)
   (handle_accept state))
  ;; Catch-all cast handler
  ((request state)
   (error_logger:warning_msg "Unhandled cast in ~p (~p): ~p~n"
                             (list (MODULE) (self) request))
   (tuple 'noreply state)))

(defun handle_info
  (((tuple 'start_worker_sup parent-sup handler) state)
   (start_worker_sup parent-sup handler state))
  ;; Catch-all info handle
  ((info state)
   (error_logger:warning_msg "Unhandled info in ~p (~p): ~p~n"
                             (list (MODULE) (self) info))
   (tuple 'noreply state)))

(defun code_change (old-vsn state extra)
  (tuple 'ok state))

;;; Internal functions
(defun handle_accept (state)
  (case (dchat_sockserv_sup:accept (state-worker-sup state))
    ((tuple 'ok pid)
     (monitor 'process pid)
     (tuple 'noreply state))))

;;; Called on connection accept by sockserv_serv
(defun accept_callback (handler)
  (lambda (pid)
    (gen_event:notify handler (tuple 'accepted pid))
    (accept)))

;;; Called on new message by sockserv_serv
(defun msg_callback (handler)
  (lambda (pid msg)
    (gen_event:notify handler (tuple 'msg pid msg))))

(defun start_worker_sup (parent-sup handler state)
  (let ((sockserv-sup-spec
         (map 'id 'sockserv_sup
              'start (tuple 'dchat_sockserv_sup
                            'start_link
                            (list (state-listen-socket state)
                                  (accept_callback handler)
                                  (msg_callback handler))))))
    (case (supervisor:start_child parent-sup sockserv-sup-spec)
      ((tuple 'ok pid)
       (tuple 'noreply (set-state-worker-sup state pid))))))

(defun listen (port)
  (gen_tcp:listen port ()))
