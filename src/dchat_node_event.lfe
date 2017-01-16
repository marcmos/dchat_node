;;; Global event handler
(defmodule dchat_node_event
  (export all)) ;; FIXME add exports

;;; API
(defun node_up (ref node addr event-pid)
  (gen_event:notify ref (tuple 'node (tuple 'up node addr event-pid))))

(defun node_down (ref node)
  (gen_event:notify ref (tuple 'node (tuple 'down node))))

(defun start_link ()
  (case (gen_event:start_link (tuple 'local (MODULE)))
    ((tuple 'ok pid)
     (case (gen_event:add_handler pid (MODULE) ())
       ('ok (tuple 'ok pid))))))

;;; gen_event callbacks
(defun init (args)
  (tuple 'ok ()))

(defun handle_event
  (((tuple 'node event) state)
   (handle_node_event event state))
  (((tuple 'user event) state)
   (handle_user_event event state)))

(defun handle_info
  (((tuple 'nodedown node) state)
   (node_down (self) node)
   (tuple 'ok state)))

;;; Internal functions
(defun handle_node_event
  (((tuple 'up node addr event-pid) state)
   (error_logger:info_msg "~p discovered new node UP ~p~n" (list (MODULE) node))
   (monitor_node node 'true)
   (user_announce_node addr)
   (tuple 'ok state))
  (((tuple 'down node) state)
   (error_logger:warning_msg "~p discovered new node DOWN ~p~n" (list (MODULE) node))
   ;; Unregister node globally
   (unregister_node node)
   ;; Notify connected users
   (user_unregister_node node)
   (tuple 'ok state)))

(defun handle_user_event (event state)
  ;; no-op
  (tuple 'ok state))

;; Announces node to local users
(defun user_announce_node
  (((tuple addr port))
   (lists:map (lambda (user)
                (dchat_user:add_server user addr port))
              (dchat_user_sup:users))))

(defun unregister_node (node)
  (error_logger:warning_msg "~p ran unimplemented unregister_node over node ~p~n"
                            (list (MODULE) node)))

(defun user_unregister_node (node)
  (error_logger:warning_msg
   "~p ran unimplemented user_unregister_node over node ~p~n"
   (list (MODULE) node)))
