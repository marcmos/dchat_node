(defmodule dchat_sockserv_serv
  (export (start_link 2)
          (init 1)
          (handle_call 3)
          (handle_cast 2)
          (handle_info 2)
          (terminate 2)))

(defrecord state
  socket
  handler)

;;; API
(defun start_link (handler listen-socket)
  (gen_server:start_link (MODULE) (tuple listen-socket handler) ()))

;;; gen-server callbacks
(defun init
  (((tuple listen-socket handler))
   (lfe_io:format "~p" (list handler))
   (gen_server:cast (self) (tuple 'accept listen-socket))
   (tuple 'ok (make-state handler handler))))

;; No-op handle call
(defun handle_call (e from state)
  (tuple 'noreply state))

(defun handle_cast
  (((tuple 'accept listen-socket) state)
   (accept listen-socket state))
  (((tuple 'send msg) state)
   (send msg state))
  ;; Catch-all cast handler
  ((request state)
   (error_logger:warning_msg "Unhandled cast in ~p (~p): ~p~n"
                             (list (MODULE) (self) request))
   (tuple 'noreply state)))

(defun handle_info
  (((tuple 'tcp port msg) state)
   (handle_msg msg state))
  (((tuple 'tcp_closed socket) state)
   (handle_disconnect state))
  ;; Catch-all info handle
  ((info state)
   (error_logger:warning_msg "Unhandled info in ~p (~p): ~p~n"
                             (list (MODULE) (self) info))
   (tuple 'noreply state)))

(defun terminate
  (('normal state) 'ok)
  ((reason state) (error_logger:error "~p (~p) terminated abnormally: ~p~n"
                                      (list (MODULE) (self) reason))))

;;; Internal functions
(defun accept (listen-socket state)
  (error_logger:info_msg "~p (~p) blocks awaiting for connection~n"
                         (list (MODULE) (self)))
  (case (gen_tcp:accept listen-socket)
    ((tuple 'ok socket)
     (error_logger:info_msg "~p (~p) accepted new connection~n"
                            (list (MODULE) (self)))
     (gen_event:notify (state-handler state) (tuple 'accept (self)))
     (tuple 'noreply (set-state-socket state socket)))))

(defun send (msg state)
  (gen_tcp:send (state-socket state) msg)
  (tuple 'noreply state))

(defun handle_msg (msg state)
  (error_logger:info_msg "~p (~p) received message: ~p~n"
                         (list (MODULE) (self) msg))
  (gen_event:notify (state-handler state) (tuple 'msg msg))
  (tuple 'noreply state))

(defun handle_disconnect (state)
  (error_logger:info_msg "~p (~p) closed connection~n" (list (MODULE) (self)))
  (tuple 'stop 'normal state))
