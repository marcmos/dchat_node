(defmodule dchat_conn_pool_serv
  (export (start_link 1)
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
(defun start_link (port)
  (gen_server:start_link (tuple 'local (MODULE))
                         (MODULE)
                         (tuple (self) port) ()))

(defun accept ()
  (gen_server:cast (MODULE) 'accept))

;;; gen_server callbacks
(defun init
  (((tuple parent-sup port))
   (case (listen port)
     ((tuple 'ok socket)
      (! (self) (tuple 'start_worker_sup parent-sup))
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
  (((tuple 'start_worker_sup parent-sup) state)
   (start_worker_sup parent-sup state))
  ;; Catch-all info handle
  ((info state)
   (error_logger:warning_msg "Unhandled info in ~p (~p): ~p~n"
                             (list (MODULE) (self) info))
   (tuple 'noreply state)))

(defun code_change (old-vsn state extra)
  (tuple 'ok state))

;;; Internal functions
(defun handle_accept (state)
  (case (dchat_sockserv_sup:accept (state-worker-sup state)
                                   (state-listen-socket state)
                                   (accept_callback))
    ((tuple 'ok pid)
     (monitor 'process pid)
     (tuple 'noreply state))))

;;; Called on connection accept by sockserv_serv
(defun accept_callback ()
  (lambda (pid)
    (accept)
    (case (dchat_user_sup:start_user pid)
      ((tuple 'ok user-pid)
       (lambda (msg) (dchat_user:handle user-pid msg))))))

(defun start_worker_sup (parent-sup state)
  (let ((sockserv-sup-spec
         (map 'id 'sockserv_sup
              'start (tuple 'dchat_sockserv_sup
                            'start_link
                            ()))))
    (case (supervisor:start_child parent-sup sockserv-sup-spec)
      ((tuple 'ok pid)
       (tuple 'noreply (set-state-worker-sup state pid))))))

(defun listen (port)
  (let* (((tuple 'ok socket) (gen_tcp:listen port (list 'binary #(packet 4))))
         ((tuple 'ok assigned-port) (inet:port socket)))
    (error_logger:info_msg "~p (~p) listening on port ~p~n"
                           (list (MODULE) (self) assigned-port))
    (tuple 'ok socket)))
