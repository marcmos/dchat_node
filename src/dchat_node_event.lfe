;;; Global event handler
(defmodule dchat_node_event
  (export all)) ;; FIXME add exports

;; FIXME share this record with directory_serv
(defrecord node
  name
  addr
  event-pid)

;;; API
(defun node_up (ref node addr event-pid)
  (gen_event:notify ref (tuple 'node (tuple 'up node addr event-pid))))

(defun node_down (ref node)
  (gen_event:notify ref (tuple 'node (tuple 'down node))))

(defun node_unregistered (ref node)
  (gen_event:notify ref (tuple 'node (tuple 'unregistered node))))

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
   ;; Try unregistering dead node
   (case (unregister_node node)
     (()
      ;; Another node was faster, we're done
      (tuple 'ok state))
     (node-record
      ;; Remove orphaned users
      (user_unregister_node (node-name node-record))

      ;; Notify other nodes about removed node
      (lists:map (lambda (remote-handler)
                   (dchat_node_event:node_unregistered remote-handler node-record))
                 (dchat_directory_serv:pid_list))
      (tuple 'ok state))))
  (((tuple 'unregistered node) state)
   (let (((tuple addr port) (node-addr node)))
     (lists:map (lambda (user) (dchat_user:remove_server user addr port))
                (dchat_user_sup:users))
     (tuple 'ok state))))

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
  (dchat_directory_serv:unregister node))

(defun user_unregister_node (node)
  (dchat_user_serv:unregister_node node))
