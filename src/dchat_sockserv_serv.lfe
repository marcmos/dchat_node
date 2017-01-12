(defmodule dchat_sockserv_serv
  (export (start_link 2)
          (init 1)
          (send 2)
          (handle_call 3)
          (handle_cast 2)
          (handle_info 2)
          (terminate 2)))

(defrecord state
  socket
  msg-callback)

;;; API
(defun start_link (listen-socket accept-callback)
  (gen_server:start_link (MODULE)
                         (tuple listen-socket accept-callback)
                         ()))

;;; gen-server callbacks
(defun init
  (((tuple listen-socket accept-callback))
   (gen_server:cast (self) (tuple 'accept listen-socket accept-callback))
   (tuple 'ok ())))

(defun send (ref payload)
  (gen_server:cast ref (tuple 'send payload)))

;; No-op handle call
(defun handle_call (e from state)
  (tuple 'noreply state))

(defun handle_cast
  (((tuple 'accept listen-socket accept-callback) state)
   (accept listen-socket accept-callback state))
  (((tuple 'send msg) state)
   (socket_send (term_to_binary msg) state))
  ;; Catch-all cast handler
  ((request state)
   (error_logger:warning_msg "Unhandled cast in ~p (~p): ~p~n"
                             (list (MODULE) (self) request))
   (tuple 'noreply state)))

(defun handle_info
  (((tuple 'tcp port msg) state)
   (handle_msg (state-msg-callback state) msg state))
  (((tuple 'tcp_closed socket) state)
   (handle_disconnect state))
  ;; Catch-all info handle
  ((info state)
   (error_logger:warning_msg "Unhandled info in ~p (~p): ~p~n"
                             (list (MODULE) (self) info))
   (tuple 'noreply state)))

(defun terminate
  (('normal state)
   (gen_tcp:close (state-socket state))
   'ok)
  ((reason state) (error_logger:error "~p (~p) terminated abnormally: ~p~n"
                                      (list (MODULE) (self) reason))))

;;; Internal functions
(defun accept (listen-socket accept-callback state)
  (error_logger:info_msg "~p (~p) blocks awaiting for connection~n"
                         (list (MODULE) (self)))
  (case (gen_tcp:accept listen-socket)
    ((tuple 'ok socket)
     (error_logger:info_msg "~p (~p) accepted new connection~n"
                            (list (MODULE) (self)))
     (case (apply accept-callback (list (self)))
       (msg-callback (tuple 'noreply (make-state socket socket
                                                 msg-callback msg-callback)))))))

(defun socket_send (msg state)
  (gen_tcp:send (state-socket state) msg)
  (tuple 'noreply state))

(defun handle_msg (msg-callback msg state)
  (error_logger:info_msg "~p (~p) received message: ~p~n"
                         (list (MODULE) (self) msg))
  (apply msg-callback (list (binary_to_term msg)))
  (tuple 'noreply state))

(defun handle_disconnect (state)
  (error_logger:info_msg "~p (~p) closed connection~n" (list (MODULE) (self)))
  (tuple 'stop 'normal state))
