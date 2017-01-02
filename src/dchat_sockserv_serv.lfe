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
(defun start_link (listen-socket handler)
  (gen_server:start_link (MODULE)
                         (tuple listen-socket handler)
                         ()))

;;; gen_server callbacks
(defun init
  (((tuple listen-socket handler))
   (apply handler 'register ())
   (gen_server:cast (self) (tuple 'accept listen-socket))
   (tuple 'ok (make-state handler handler))))

;; No-op handle call
(defun handle_call (e from state)
  (tuple 'noreply state))

(defun handle_cast
  (((tuple 'accept listen-socket) state)
   (error_logger:info_msg "~p (~p) blocks awaiting for connection~n" (list (MODULE) (self)))
   (case (gen_tcp:accept listen-socket)
     ((tuple 'ok socket)
      (error_logger:info_msg "~p (~p) accepted new connection~n" (list (MODULE) (self)))
      (tuple 'noreply (set-state-socket state socket)))))
  (((tuple 'msg msg) state)
   (gen_tcp:send (state-socket state) msg)
   (tuple 'noreply state))
  ;; Catch-all cast handler
  ((request state)
   (error_logger:warning_msg "Unhandled cast in ~p (~p): ~p~n" (list (MODULE) (self) request))
   (tuple 'noreply state)))

(defun handle_info
  (((tuple 'tcp port msg) state)
   (error_logger:info_msg "~p (~p) received message: ~p~n" (list (MODULE) (self) msg))
   (handle_msg state msg)
   (tuple 'noreply state))
  (((tuple 'tcp_closed socket) state)
   (error_logger:info_msg "~p (~p) closed connection~n" (list (MODULE) (self)))
   (tuple 'stop 'normal state))
  ;; Catch-all info handle
  ((info state)
   (error_logger:warning_msg "Unhandled info in ~p (~p): ~p~n" (list (MODULE) (self) info))
   (tuple 'noreply state)))

(defun terminate
  (('normal state) 'ok)
  ((reason state) (error_logger:error "~p (~p) terminated abnormally: ~p~n"
                                      (list (MODULE) (self) reason))))

;;; Internal functions
(defun handle_msg (state msg)
  (apply (state-handler state) 'handle (list msg)))
