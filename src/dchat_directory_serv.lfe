(defmodule dchat_directory_serv
  (export all))

(defrecord state
  nodes)

(defun start_link ()
  (gen_server:start_link (tuple 'local (MODULE)) (MODULE) () ()))

(defun register (addr port)
  (gen_server:call (MODULE) (tuple 'register (tuple addr port))))

(defun get ()
  (gen_server:call (MODULE) 'get))

(defun init (args)
  (tuple 'ok (make-state nodes ())))

(defun handle_call
  (((tuple 'register host) from state)
   (tuple 'reply 'ok (set-state-nodes state (cons host (state-nodes state)))))
  (('get from state)
   (tuple 'reply (state-nodes state) state)))