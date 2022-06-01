(coalton-library/utils:defstdlib-package #:coalton-library/resource
  (:use #:coalton
        #:coalton-library/classes
        #:coalton-library/result)
  (:local-nicknames (#:cell #:coalton-library/cell))
  (:export

   #:ResourceError
   #:OpenError
   #:RunError
   #:CloseError

   #:flatten-resource-error

   #:with-resource))
(cl:in-package #:coalton-library/resource)

;; a more robust version of `ResourceError' would encode various possibilityes when the `close!' step fails:
;; - if the `run!' step had a non-local exit, then neither a `:run-error' nor a `:result' exists
;; - if the `run!' step succeeded but the `close!' step returned an error, in addition to a `:close-error',
;;   there is also either a `:result' or a `:run-error'. the current definition discards the result of the
;;   `run!' step, if any, when the `close!' step fails.

(coalton-toplevel
  (define-type (ResourceError :open :run :close)
    "Encodes the three different occasions that an error might occur during a `with-resource' call."
    (OpenError :open)
    (RunError :run)
    (CloseError :close))

  (declare flatten-resource-error ((ResourceError :err :err :err) -> :err))
  (define (flatten-resource-error err)
    (match err
      ((OpenError e) e)
      ((RunError e) e)
      ((CloseError e) e)))

  (declare unwind-protect ((Unit -> :res)
                           -> (Unit -> Unit)
                           -> :res))
  (define (unwind-protect protected cleanup)
    (lisp :res (protected cleanup)
      (cl:unwind-protect (coalton-impl/codegen:a1 protected Unit)
        (coalton-impl/codegen:a1 cleanup Unit))))

  (declare do-run-step ((cell:Cell (Optional (Result (ResourceError :open-error
                                                                    :run-error
                                                                    :close-error)
                                                     :result)))
                        -> :resource
                        -> (:resource -> (Result :run-error :result))
                        -> Unit))
  (define (do-run-step result resource run!)
    (cell:write! result
                 (Some (match (run! resource)
                         ((Err run-error) (Err (RunError run-error)))
                         ((Ok result) (Ok result)))))
    Unit)

  (declare do-cleanup-step ((cell:Cell (Optional (Result (ResourceError :open-error
                                                                        :run-error
                                                                        :close-error)
                                                         :result)))
                            -> :resource
                            -> (:resource -> (Result :close-error Unit))
                            -> Unit))
  (define (do-cleanup-step result resource close!)
    (match (close! resource)
      ((Err close-error)
       ;; if `close!' returns an error, overwrite the result to return that error
       (cell:write! result (Some (Err (CloseError close-error))))
       Unit)
      ((Ok _)
       ;; if `close!' succeeds, there's no need to set the result
       Unit)))

  (declare with-resource ((Unit -> (Result :open-error :resource))
                          -> (:resource -> (Result :run-error :result))
                          -> (:resource -> (Result :close-error Unit))
                          -> (Result (ResourceError :open-error :run-error :close-error)
                                     :result)))
  (define (with-resource open! run! close!)
    "Use OPEN! to create a RESOURCE, pass it to RUN!, then CLOSE! the RESOURCE. Return the result of RUN! unless an error occurs.

If OPEN! returns an (Err open-error), `with-resource' returns (Result (OpenError open-error)). Neither RUN!
nor CLOSE! is invoked in this case, as no resource has been created.

If RUN! returns and CLOSE! returns an (Err close-error), `with-resource' returns (Result (CloseError
close-error)). Note that this clobbers the result of RUN!, even if RUN! returned an Err.

If RUN! returns an (Err run-error) and CLOSE! succeeds, `with-resource' returns (Result (RunError
run-error)). Note that, if CLOSE! fails, the error returned by RUN! will be silently discarded.

If RUN! returns (Ok result) and CLOSE! succeeds, `with-resource' returns (Ok result). Note that, if CLOSE!
fails, the result returned by RUN! will be silently discarded.

If RUN! takes a non-local exit, CLOSE! will be run. The non-local exit will then continue."
    (match (open!)
      ((Err open-error) (Err (OpenError open-error)))
      ((Ok resource)
       (let result = (cell:new None))
       (unwind-protect
            (fn () (do-run-step result resource run!))
         (fn () (do-cleanup-step result resource close!)))
       ;; if execution reaches this point, we know that `run!' did not take a non-local exit (or unwinding
       ;; would have continued after running `close!'). therefore, `result' contains `Some', whether it's an
       ;; `Ok' from `run!', a `RunError', or a `CloseError'.
       (unwrap (cell:read result))))))
