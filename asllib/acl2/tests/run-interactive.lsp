

;; (assign :fname "ilog2-ast.lsp")

(in-package "ASL")

(include-book "toplevel" :dir :acl2asl)

(b* (((er (list (cons static-env ast))) (acl2::read-file (@ :fname) state))
     (state (f-put-global ':static-env static-env state))
     (state (f-put-global ':ast ast state)))
  (value :ok))


(defconst *magic-delimiter* "!@#$%^%$#@")

(time$
 (b* ((- (cw "~%~s0 begin ASL interpreter output~%" *magic-delimiter*))
      ((mv result orac) (run (@ :static-env) (@ :ast)))
      (status
       (eval_result-case result
         :ev_error (prog2$ (cw "Error: ~s0 -- ~x1~%" result.desc result.data)
                           1)
         :ev_throwing (prog2$ (cw "Uncaught exception: ~x0~%" result.throwdata)
                              1)
         :ev_normal (val-case result.res
                      :v_int result.res.val
                      :otherwise (prog2$ (cw "bad return value from main: ~x0" result.res)
                                         1))))
      (- (cw "~%~s0 end ASL interpreter output~%" *magic-delimiter*)
         status)
      (state (f-put-global ':status status state)))
   (mv status orac state))
 :msg "ASL run: ~st sec, ~sa bytes.~%")


