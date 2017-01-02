(defmodule sockserv_serv
  (export all))

;;; API
(defun start_link (listen-socket)
  (gen_server:start_link (MODULE) listen-socket ()))

;;; gen-server callbacks
(defun init (listen-socket)
  (gen_server:cast (self) 'accept)
  (tuple 'ok listen-socket))

;; No-op handle call
(defun handle_call (e from state)
  (tuple 'noreply state))

(defun handle_cast
  (('accept state)
   (error_logger:info_msg "~p (~p) blocks awaiting for connection~n" (list (MODULE) (self)))
   (case (gen_tcp:accept state)
     ((tuple 'ok socket)
      (error_logger:info_msg "~p (~p) accepted new connection~n" (list (MODULE) (self)))
      (tuple 'noreply socket))))
  ;; Catch-all cast handler
  ((request state)
   (error_logger:warning_msg "Unhandled cast in ~p (~p): ~p~n" (list (MODULE) (self) request))
   (tuple 'noreply state)))

(defun handle_info
  (((tuple 'tcp port msg) state)
   (error_logger:info_msg "~p (~p) received message: ~p~n" (list (MODULE) (self) msg))
   (tuple 'noreply (handle_msg state msg)))
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
  state)
