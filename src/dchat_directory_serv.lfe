(defmodule dchat_directory_serv
  (export all))

(defrecord node
  name
  addr
  port)

(defun start_link ()
  (gen_server:start_link (tuple 'local (MODULE)) (MODULE) () ()))

(defun list ()
  (gen_server:call (MODULE) 'list))

(defun init (args)
  (case (init_table)
    ('ok
     (register_self)
     (tuple 'ok ()))))

(defun handle_call
  (('list from state)
   (case (mnesia_read_all)
     (nodes (tuple 'reply nodes state)))))

;;; Internal functions
(defun register (name addr port)
  (error_logger:info_msg "~p registering node ~p~n"
                         (list (MODULE) name))
  (mnesia_write (make-node name name addr addr port port)))

(defun register_self ()
  (let ((name (node))
        ((tuple 'ok client-addr) (application:get_env 'listen_addr))
        (listen-port (conn_pool_serv:listen_port 'dchat_conn_pool_serv)))
    (register name client-addr listen-port)))

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
  (mnesia:activity 'transaction
                   (lambda ()
                     (mnesia:foldl (lambda (node node-list)
                                     (cons (tuple (node-addr node)
                                                  (node-port node))
                                           node-list))
                                   ()
                                   'node))))