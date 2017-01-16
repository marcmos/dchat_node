(defmodule dchat_directory_serv
  (export all))

(defrecord node
  name
  addr
  event-pid)

(defun start_link ()
  (gen_server:start_link (tuple 'local (MODULE)) (MODULE) () ()))

(defun node_list ()
  (gen_server:call (MODULE) 'list))

(defun init (args)
  (case (init_table)
    ('ok
     (let ((active-nodes (mnesia_read_all))
           (self-record (make_self_record)))
       ;; Register self in directory and announce to remote event handlers
       (register self-record)
       (announce active-nodes self-record)

       ;; Register discovered nodes in local event handler
       (lists:map (lambda (node)
                    (dchat_node_event:node_up 'dchat_node_event
                                              (node-name node)
                                              (node-addr node)
                                              (node-event-pid node)))
                  active-nodes))
     (tuple 'ok ()))))

(defun terminate (reason state)
  ;; TODO unregister self
  )

(defun handle_call
  (('list from state)
   (case (mnesia_read_all)
     (nodes (tuple 'reply nodes state)))))

;;; Internal functions
(defun register (node)
  (error_logger:info_msg "~p registering node ~p~n"
                         (list (MODULE) (node-name node)))
  (mnesia_write node))

(defun make_self_record ()
  (let (((tuple 'ok client-addr) (application:get_env 'listen_addr)))
    (make-node name (node)
               addr (tuple client-addr
                           (conn_pool_serv:listen_port 'dchat_conn_pool_serv))
               event-pid (whereis 'dchat_node_event))))

(defun announce (nodes node-record)
  (lists:map (lambda (node)
               (dchat_node_event:node_up (node-event-pid node)
                                         (node-name node-record)
                                         (node-addr node-record)
                                         (node-event-pid node-record)))
             nodes))

;;; Mnesia-related functions
(defun table_name () 'node)

(defun init_table ()
  (let* (((tuple 'ok nodes) (application:get_env 'nodes))
         (create-result (mnesia:create_table 'node
                                             (list `#(ram_copies ,nodes)
                                                   (tuple 'attributes (fields-node))
                                                   ))))
    (case create-result
      (#(atomic ok)
       (error_logger:info_msg "~p created node directory table~n" (list (MODULE))))
      (`#(aborted #(already_exists ,_))
       (error_logger:warning_msg "~p skipped node directory table creation~n"
                                 (list (MODULE))))))
  'ok)

(defun mnesia_write (node-record)
  (mnesia:activity 'transaction (lambda () (mnesia:write node-record))))

(defun mnesia_read_all ()
  (mnesia:activity 'transaction (lambda ()
                                  (mnesia:foldl (lambda (node node-list)
                                     (cons node node-list))
                                   ()
                                   'node))))