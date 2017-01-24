(defmodule conn_pool_serv
  (export all))

(defrecord state
  listen-socket
  conn-sup
  handlers)

;;; API
(defun start_link (server-name port handlers)
  (gen_server:start_link server-name (MODULE) (tuple (self) port handlers) ()))

(defun start_link (port handlers)
   (gen_server:start_link (MODULE) (tuple (self) port handlers) ()))

(defun accept (serv)
  (gen_server:cast serv 'accept))

(defun listen_port (serv)
  (gen_server:call serv 'listen_port))

;;; gen_server callbacks
(defun init
  (((tuple parent-sup port handlers))
   (case (listen port)
     ((tuple 'ok socket)
      (! (self) (tuple 'start_conn_sup parent-sup))
      (accept (self))
      (tuple 'ok (make-state listen-socket socket
                             handlers handlers))))))

(defun terminate
  (('normal state)
   (gen_tcp:close (state-listen-socket state))
   'ok)
  ((reason state) (error_logger:error "~p (~p) terminated abnormally: ~p~n"
                                      (list (MODULE) (self) reason))))

;; No-op handle call
(defun handle_call
  (('listen_port from state)
   (case (inet:port (state-listen-socket state))
     ((tuple 'ok assigned-port)
      (tuple 'reply assigned-port state))))
  ((_ _ state)
   (tuple 'noreply state)))

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
                         (state-handlers state))
    ((tuple 'ok pid)
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
