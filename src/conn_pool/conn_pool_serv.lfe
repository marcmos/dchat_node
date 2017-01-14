(defmodule conn_pool_serv
  (export all))

(defrecord state
  listen-socket
  conn-sup
  handlers)

;;; API
(defun start_link (port handlers)
  (gen_server:start_link (tuple 'local (MODULE))
                         (MODULE)
                         (tuple (self) port handlers) ()))

(defun accept ()
  (gen_server:cast (MODULE) 'accept))

;;; gen_server callbacks
(defun init
  (((tuple parent-sup port handlers))
   (case (listen port)
     ((tuple 'ok socket)
      (! (self) (tuple 'start_conn_sup parent-sup))
      (accept)
      (tuple 'ok (make-state listen-socket socket
                             handlers handlers))))))

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
  (((tuple 'start_conn_sup parent-sup) state)
   (start_conn_sup parent-sup state))
  ;; Catch-all info handle
  ((info state)
   (error_logger:warning_msg "Unhandled info in ~p (~p): ~p~n"
                             (list (MODULE) (self) info))
   (tuple 'noreply state)))

(defun code_change (old-vsn state extra)
  (tuple 'ok state))

;;; Internal functions
(defun handle_accept (state)
  (case (conn_sup:accept (state-conn-sup state)
                         (state-listen-socket state)
                         (cons 'conn_pool_event (state-handlers state)))
    ((tuple 'ok pid)
     (monitor 'process pid) ;; FIXME WTF monitors supervisor?
     (tuple 'noreply state))))

(defun start_conn_sup (parent-sup state)
  (let ((conn-sup-spec
         (map 'id 'conn_sup
              'start (tuple 'conn_sup 'start_link ()))))
    (case (supervisor:start_child parent-sup conn-sup-spec)
      ((tuple 'ok pid)
       (tuple 'noreply (set-state-conn-sup state pid))))))

(defun listen (port)
  (let* (((tuple 'ok socket) (gen_tcp:listen port (list 'binary #(packet 4))))
         ((tuple 'ok assigned-port) (inet:port socket)))
    (error_logger:info_msg "~p (~p) listening on port ~p~n"
                           (list (MODULE) (self) assigned-port))
    (tuple 'ok socket)))
