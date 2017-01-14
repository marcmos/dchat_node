(defmodule conn_pool_event
  (export all))

(defun init (serv)
  (tuple 'ok serv))

(defun handle_event
  (('accepted serv)
   (conn_pool_serv:accept)
   (tuple 'ok serv))
  (((tuple 'received _) serv)
   (tuple 'ok serv))
  ((_ serv)
   (tuple 'ok serv)))