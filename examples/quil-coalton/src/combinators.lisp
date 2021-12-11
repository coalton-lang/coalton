(cl:in-package #:quil-coalton)

(coalton-toplevel
  ;;
  ;; Parser combinators
  ;;

  (declare many0 ((Parser :a) -> (Parser (List :a))))
  (define (many0 p_)
    (let ((p (get-parser p_))
          (f (fn (str)
               (match (p str)
                 ((Err _) (Tuple Nil str))
                 ((Ok (Tuple a str))
                  (match (f str)
                    ((Tuple b str) (Tuple (Cons a b) str))))))))
      (Parser
       (fn (str) (Ok (f str))))))

  (declare many1 ((Parser :a) -> (Parser (List :a))))
  (define (many1 p)
    (>>= p (fn (a) (map (Cons a) (many0 p)))))

  (declare option ((Parser :a) -> (Parser (Optional :a))))
  (define (option p_)
    (let ((p (get-parser p_)))
      (Parser
       (fn (str)
         (match (p str)
           ((Ok (Tuple a str)) (Ok (Tuple (Some a) str)))
           ((Err _) (Ok (Tuple None str))))))))

  (declare verify ((:a -> Boolean) -> ((Parser :a) -> (Parser :a))))
  (define (verify f p)
    (>>= p
         (fn (x)
           (match (f x)
             ((True) (const-value x))
             ((False) (fail "Validation failed"))))))

  (define-instance (Functor Parser)
    (define (map f p_)
      (let ((p (get-parser p_)))
        (Parser
         (fn (str)
           (match (p str)
             ((Err e) (Err e))
             ((Ok (Tuple a str))
              (Ok (Tuple (f a) str)))))))))

  (define-instance (Applicative Parser)
    (define pure const-value) 
    (define (liftA2 f a b)
      (do
       (a <- a)
       (b <- b)
        (pure (f a b)))))

  (define-instance (Monad Parser)
    (define (>>= p_ f)
      (let ((p (get-parser p_)))
        (Parser
         (fn (str)
           (match (p str)
             ((Err e) (Err e))
             ((Ok (Tuple a str))
              ((get-parser (f a)) str))))))))

  (define-instance (MonadFail Parser)
    (define (fail s)
      (Parser
       (fn (str)
         (Err (ParseError s))))))

  (define-instance (Alternative Parser)
    (define (alt p1_ p2_)
      (let ((p1 (get-parser p1_))
            (p2 (get-parser p2_)))
        (Parser
         (fn (str)
           (match (p1 str)
             ((Ok (Tuple a str))
              (Ok (Tuple a str)))
             ((Err _)
              (match (p2 str)
                ((Err e) (Err e))
                ((Ok (Tuple b str))
                 (Ok (Tuple b str))))))))))
    (define empty (fail "alt")))

  (declare map2 ((:a -> (:b -> :c)) -> ((Parser :a) -> ((Parser :b) -> (Parser :c)))))
  (define map2 liftA2)

  (declare map3 ((:a -> (:b -> (:c -> :d))) -> ((Parser :a) -> ((Parser :b) -> ((Parser :c) -> (Parser :d))))))
  (define (map3 f p1_ p2_ p3_)
    (let ((p1 (get-parser p1_))
          (p2 (get-parser p2_))
          (p3 (get-parser p3_)))
      (Parser
       (fn (str)
         (match (p1 str)
           ((Err e) (Err e))
           ((Ok (Tuple a str))
            (match (p2 str)
              ((Err e) (Err e))
              ((Ok (Tuple b str))
               (match (p3 str)
                 ((Err e) (Err e))
                 ((Ok (Tuple c str))
                  (Ok (Tuple (f a b c) str))))))))))))

  (declare map4 ((:a -> (:b -> (:c -> (:d -> :e)))) -> ((Parser :a) -> ((Parser :b) -> ((Parser :c) -> ((Parser :d) -> (Parser :e)))))))
  (define (map4 f p1_ p2_ p3_ p4_)
    (let ((p1 (get-parser p1_))
          (p2 (get-parser p2_))
          (p3 (get-parser p3_))
          (p4 (get-parser p4_)))
      (Parser
       (fn (str)
         (match (p1 str)
           ((Err e) (Err e))
           ((Ok (Tuple a str))
            (match (p2 str)
              ((Err e) (Err e))
              ((Ok (Tuple b str))
               (match (p3 str)
                 ((Err e) (Err e))
                 ((Ok (Tuple c str))
                  (match (p4 str)
                    ((Err e) (Err e))
                    ((Ok (Tuple d str))
                     (Ok (Tuple (f a b c d) str))))))))))))))

  (declare map5 ((:a -> (:b -> (:c -> (:d -> (:e -> :f))))) -> ((Parser :a) -> ((Parser :b) -> ((Parser :c) -> ((Parser :d) -> ((Parser :e) -> (Parser :f))))))))
  (define (map5 f p1_ p2_ p3_ p4_ p5_)
    (let ((p1 (get-parser p1_))
          (p2 (get-parser p2_))
          (p3 (get-parser p3_))
          (p4 (get-parser p4_))
          (p5 (get-parser p5_)))
      (Parser
       (fn (str)
         (match (p1 str)
           ((Err e) (Err e))
           ((Ok (Tuple a str))
            (match (p2 str)
              ((Err e) (Err e))
              ((Ok (Tuple b str))
               (match (p3 str)
                 ((Err e) (Err e))
                 ((Ok (Tuple c str))
                  (match (p4 str)
                    ((Err e) (Err e))
                    ((Ok (Tuple d str))
                     (match (p5 str)
                       ((Err e) (Err e))
                       ((Ok (Tuple e str))
                        (Ok (Tuple (f a b c d e) str)))))))))))))))))
