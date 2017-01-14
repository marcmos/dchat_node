(defmodule sockserv_serv
  (export all))

(defrecord state
  conn-sup
  sup
  socket
  handler)

;;; API
(defun start_link (conn-sup listen-socket handlers)
  (gen_server:start_link (MODULE)
                         (tuple conn-sup (self) listen-socket handlers)
                         ()))

;;; gen-server callbacks
(defun init
  (((tuple conn-sup sup listen-socket handlers))
   (! (self) (tuple 'accept sup listen-socket handlers))
   (tuple 'ok (make-state conn-sup conn-sup sup sup))))

(defun send (ref payload)
  (gen_server:cast ref (tuple 'send payload)))

;; No-op handle call
(defun handle_call (e from state)
  (tuple 'noreply state))

(defun handle_cast
  (((tuple 'send msg) state)
   (socket_send msg state))
  ;; Catch-all cast handler
  ((request state)
   (error_logger:warning_msg "Unhandled cast in ~p (~p): ~p~n"
                             (list (MODULE) (self) request))
   (tuple 'noreply state)))

(defun handle_info
  (((tuple 'tcp port msg) state)
   (handle_msg (binary_to_term msg) state))
  (((tuple 'tcp_closed socket) state)
   (handle_disconnect state))
  (((tuple 'accept sup listen-socket handlers) state)
   (accept sup listen-socket handlers state))
  ;; Catch-all info handle
  ((info state)
   (error_logger:warning_msg "Unhandled info in ~p (~p): ~p~n"
                             (list (MODULE) (self) info))
   (tuple 'noreply state)))

(defun terminate
  (('normal state)
   (gen_tcp:close (state-socket state))
   (supervisor:terminate_child (state-conn-sup state) (state-sup state))
   'ok)
  ((reason state) (error_logger:error "~p (~p) terminated abnormally: ~p~n"
                                      (list (MODULE) (self) reason))))

;;; Internal functions
(defun accept (sup listen-socket handlers state)
  (error_logger:info_msg "~p (~p) blocks awaiting for connection~n"
                         (list (MODULE) (self)))
  (case (gen_tcp:accept listen-socket)
    ((tuple 'ok socket)
     (error_logger:info_msg "~p (~p) accepted new connection~n"
                            (list (MODULE) (self)))
     (case (start_handler sup handlers)
       ((tuple 'ok handler-pid)
        (gen_event:notify handler-pid 'accepted)
        (tuple 'noreply (set-state state
                                   socket socket
                                   handler handler-pid)))))))

(defun socket_send (msg state)
  (error_logger:info_msg "~p (~p) sending message: ~p~n"
                            (list (MODULE) (self) msg))
  (gen_tcp:send (state-socket state) (term_to_binary msg))
  (tuple 'noreply state))

(defun handle_msg (msg state)
  (error_logger:info_msg "~p (~p) received message: ~p~n"
                         (list (MODULE) (self) msg))
  (gen_event:notify (state-handler state) (tuple 'received msg))
  (tuple 'noreply state))

(defun handle_disconnect (state)
  (error_logger:info_msg "~p (~p) closed connection~n" (list (MODULE) (self)))
  (gen_event:notify (state-handler state) 'disconnected)
  (tuple 'stop 'normal state))

(defun start_handler (sup handlers)
  (let ((event-spec (map 'id 'event
                         'start #(gen_event start_link ())
                         'restart 'temporary)))
    (case (supervisor:start_child sup event-spec)
      ((tuple 'ok pid)
       (lists:map (lambda (handler) (gen_event:add_handler pid handler (self)))
                  handlers)
       (tuple 'ok pid)))))