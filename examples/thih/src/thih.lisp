(cl:in-package #:thih-coalton)

(coalton-toplevel

  ;;
  ;; Preliminaries
  ;;

  (define-type Id
    (Id String))

  (define (id->string id)
    (match id
      ((Id s) s)))

  (define-instance (Eq Id)
    (define (== x y)
      (== (get-id x)
          (get-id y))))

  (declare get-id (Id -> String))
  (define (get-id x)
    (match x
      ((Id str) str)))

  (declare enumId (Integer -> Id))
  (define (enumId n)
    (Id (<> "v" (into n))))


  ;;
  ;; Kinds
  ;;

  (define-type Kind
    Star
    (Kfun Kind Kind))

  (define-instance (Eq Kind)
    (define (== k1 k2)
      (match (Tuple k1 k2)
        ((Tuple (Star) (Star)) True)
        ((Tuple (Kfun a1 a2)
                (Kfun b1 b2))
         (and (== a1 b1)
              (== a2 b2)))
        (_ False))))

  ;;
  ;; Types
  ;;

  (define-type Type
    (TVar Tyvar)
    (TCon Tycon)
    (TAp Type Type)
    (TGen UFix))

  (define-instance (Eq Type)
    (define (== t1 t2)
      (match (Tuple t1 t2)
        ((Tuple (TVar v1) (TVar v2)) (== v1 v2))
        ((Tuple (TCon c1) (TCon c2)) (== c1 c2))
        ((Tuple (TAp a1 a2) (TAp b1 b2))
         (and (== a1 b1)
              (== a2 b2)))
        ((Tuple (TGen g1) (TGen g2)) (== g1 g2))
        (_ False))))


  (define-type Tyvar
    (Tyvar Id Kind))

  (define-instance (Eq Tyvar)
    (define (== x y)
      (match (Tuple x y)
        ((Tuple (Tyvar i1 k1) (Tyvar i2 k2))
         (and (== i1 i2)
              (== k1 k2))))))


  (define-type Tycon
    (Tycon Id Kind))

  (define-instance (Eq Tycon)
    (define (== x y)
      (match (Tuple x y)
        ((Tuple (Tycon i1 k1) (Tycon i2 k2))
         (and (== i1 i2)
              (== k1 k2))))))


  (define tUnit (TCon (Tycon (Id "()") Star)))
  (define tChar (TCon (Tycon (Id "Char") Star)))
  (define tInt (TCon (Tycon (Id "Int") Star)))
  (define tInteger (TCon (Tycon (Id "Integer") Star)))
  (define tFloat (TCon (Tycon (Id "Float") Star)))
  (define tDouble (TCon (Tycon (Id "Double") Star)))

  (define tList (TCon (Tycon (Id "[]") (Kfun Star Star))))
  (define tArrow (TCon (Tycon (Id "->") (Kfun Star (Kfun Star Star)))))
  (define tTuple2 (TCon (Tycon (Id "(,)") (Kfun Star (Kfun Star Star)))))

  (define tString (mkList tChar))

  ;; Renamed from 'fn' due to keyword collision
  (declare mkFn (Type -> Type -> Type))
  (define (mkFn a b)
    (TAp (TAp tArrow a) b))

  (declare mkList (Type -> Type))
  (define (mkList a)
    (TAp tList a))

  (declare mkPair (Type -> Type -> Type))
  (define (mkPair a b)
    (TAp (TAp tTuple2 a) b))


  (define-class (HasKind :t)
    (kind (:t -> Kind)))

  (define-instance (HasKind Tyvar)
    (define (kind t)
      (match t
        ((Tyvar _ k) k))))

  (define-instance (HasKind Tycon)
    (define (kind t)
      (match t
        ((Tycon _ k) k))))

  (define-instance (HasKind Type)
    (define (kind t)
      (match t
        ((TCon tc) (kind tc))
        ((TVar u) (kind u))
        ((TAp t _)
         (match (kind t)
           ((Kfun _ k) k)
           (_ (error "unreachable"))))
        (_ (error "unreachable")))))


  ;;
  ;; Substitutions
  ;;

  (define-type Subst
    (Subst (List (Tuple Tyvar Type))))

  (define (get-subst s)
    (match s
      ((Subst l) l)))

  (declare nullSubst Subst)
  (define nullSubst (Subst Nil))

  (declare +-> (Tyvar -> Type -> Subst))
  (define (+-> u t)
    (Subst (make-list (Tuple u t))))


  (define-class (Types :t)
    (apply (Subst -> :t -> :t))
    (tv (:t -> (List Tyvar))))

  (define-instance (Types Type)
    (define (apply s t)
      (match t
        ((TVar u)
         (match (list:lookup u (get-subst s))
           ((Some t) t)
           ((None) (Tvar u))))
        ((TAp l r)
         (TAp (apply s l) (apply s r)))
        (x x)))
    (define (tv t)
      (match t
        ((TVar u) (make-list u))
        ((TAp l r) (list:union (tv l) (tv r)))
        (_ Nil))))


  (define-instance (Types :a => Types (List :a))
    (define (apply s t)
      (map (apply s) t))
    (define (tv t)
      (remove-duplicates (fold append Nil (map tv t)))))


  (declare @@ (Subst -> Subst -> Subst))
  (define (@@ s1 s2)
    (Subst (append
            (map
             (fn (s)
               (match s
                 ((Tuple u t)
                  (Tuple u (apply s1 t)))))
             (get-subst s2))
            (get-subst s1))))

  (declare merge (MonadFail :m => (Subst -> Subst -> (:m Subst))))
  (define (merge s1 s2)
    (let ((agree (all (fn (v)
                        (== (apply s1 (TVar v))
                            (apply s2 (TVar v))))
                      (list:intersection
                       (map fst (get-subst s1))
                       (map fst (get-subst s2))))))
      (if agree
          (pure (Subst (append (get-subst s1)
                               (get-subst s2))))
          (fail "Failed to merge substitution lists"))))


  ;;
  ;; Unification and Matching
  ;;

  (declare mgu (MonadFail :m => (Type -> Type -> (:m Subst))))
  (define (mgu t1 t2)
    (match (Tuple t1 t2)
      ((Tuple (TAp l r) (Tap l_ r_))
       (>>= (mgu l l_)
            (fn (s1)
              (>>= (mgu (apply s1 r)
                        (apply s1 r_))
                   (fn (s2)
                     (pure (@@ s2 s1)))))))
      ((Tuple (Tvar u) t2) (varBind u t2))
      ((Tuple t1 (Tvar u)) (varBind u t1))
      ((Tuple (TCon t1) (TCon t2))
       (if (== t1 t2)
           (pure nullSubst)
           (fail "Type constructors do not unify")))
      ((Tuple _ _)
       (fail "Types do not unify"))))

  (declare varBind (MonadFail :m => (Tyvar -> Type -> (:m Subst))))
  (define (varBind u t_)
    (if (== t_ (TVar u))
        (pure nullSubst)
        (if (list:member u (tv t_))
            (fail "Occurs check fails")
            (if (/= (kind u) (kind t_))
                (fail "Kinds do not match")
                (pure (+-> u t_))))))

  ;; Renamed from 'match' in thih
  (declare matchType (MonadFail :m => (Type -> Type -> (:m Subst))))
  (define (matchType t1 t2)
    (match (Tuple t1 t2)

      ((Tuple (TAp l r) (TAp l_ r_))
       (>>= (matchType l l_)
            (fn (sl)
              (>>= (matchType r r_)
                   (fn (sr) (merge sl sr))))))
      ((Tuple (TVar u) t)
       (if (== (kind u) (kind t))
           (pure (+-> u t))
           (fail "Kind mismatch in match")))
      ((Tuple (TCon tc1) (TCon tc2))
       (if (== tc1 tc2)
           (pure nullSubst)
           (fail "Type constructor mismatch")))
      ((Tuple _ _)
       (fail "Types do not match"))))


  ;;
  ;; Type Classes, Predicates and Qualified Types
  ;;

  ;; Basic Definitions

  (define-type (Qual :t)
    (Qual (List Pred) :t))

  (define-instance (Eq :t => Eq (Qual :t))
    (define (== x y)
      (match (Tuple x y)
        ((Tuple (Qual xs t1) (Qual ys t2))
         (and (== xs ys)
              (== t1 t2))))))

  (define-instance (Types :t => Types (Qual :t))
    (define (apply s q)
      (match q
        ((Qual ps t)
         (Qual (apply s ps)
               (apply s t)))))
    (define (tv q)
      (match q
        ((Qual ps t)
         (list:union (tv ps) (tv t))))))


  (define-type Pred
    (IsIn Id Type))

  (define-instance (Eq Pred)
    (define (== x y)
      (match (Tuple x y)
        ((Tuple (IsIn id1 t1) (IsIn id2 t2))
         (and (== id1 id2)
              (== t1 t2))))))

  (define-instance (Types Pred)
    (define (apply s p)
      (match p
        ((IsIn i t)
         (IsIn i (apply s t)))))
    (define (tv p)
      (match p
        ((IsIn _ t)
         (tv t)))))


  (declare mguPred (Pred -> Pred -> (Optional Subst)))
  (define (mguPred p q)
    ((lift mgu) p q))

  (declare matchPred (Pred -> Pred -> (Optional Subst)))
  (define matchPred (lift matchType))

  (declare lift (MonadFail :m => ((Type -> Type -> (:m Subst)) -> (Pred -> Pred -> (:m Subst)))))
  (define (lift m p1 p2)
    (match (Tuple p1 p2)
      ((Tuple (IsIn i _t) (IsIn i_ t_))
       (if (== i i_)
           (m _t t_)
           (fail "Classes differ")))))


  (define-type Class
    (Class (Tuple (List Id) (List Inst))))

  (declare get-class (Class -> (Tuple (List Id) (List Inst))))
  (define (get-class c)
    (match c
      ((Class x) x)))

  (define-type Inst
    (Instance (Qual Pred)))

  (declare get-inst (Inst -> (Qual Pred)))
  (define (get-inst i)
    (match i
      ((Instance x) x)))


  ;; Class Environments

  (define-type ClassEnv
    (ClassEnv (Id -> (Optional Class)) (List Type)))

  (declare classes (ClassEnv -> (Id -> (Optional Class))))
  (define (classes env)
    (match env
      ((ClassEnv cs _) cs)))

  (declare defaults (ClassEnv -> (List Type)))
  (define (defaults env)
    (match env
      ((ClassEnv _ ds) ds)))

  (declare super (ClassEnv -> Id -> (List Id)))
  (define (super ce i)
    (match (classes ce i)
      ((Some c)
       (fst (get-class c)))
      (_
       (error "unreachable"))))

  (declare insts (ClassEnv -> Id -> (List Inst)))
  (define (insts ce i)
    (match (classes ce i)
      ((Some c)
       (snd (get-class c)))
      (_
       (error "unreachable"))))

  (declare defined ((Optional :a) -> Boolean))
  (define (defined x)
    (match x
      ((Some _) True)
      ((None) False)))

  (declare modify (ClassEnv -> Id -> Class -> ClassEnv))
  (define (modify ce i c)
    (ClassEnv
     (fn (j)
       (if (== i j)
           (Some c)
           (classes ce j)))
     (defaults ce)))

  (declare initialEnv ClassEnv)
  (define initialEnv
    (ClassEnv (fn (i) (fail "Class not defined"))
              (make-list tInteger tDouble)))

  ;; (define-type-alias EnvTransformer (ClassEnv -> (Optional ClassEnv)))

  (declare compose ((ClassEnv -> (Optional ClassEnv)) -> (ClassEnv -> (Optional ClassEnv)) -> (ClassEnv -> (Optional ClassEnv))))
  (define (compose f g ce)
    (match (f ce)
      ((None) None)
      ((Some ce_) (g ce_))))


  (declare addClass (Id -> (List Id) -> (ClassEnv -> (Optional ClassEnv))))
  (define (addClass i is ce)
    (cond
      ((defined (classes ce i))
       (fail "class already defined"))
      ((any (fn (x) (not (defined (classes ce x)))) is)
       (fail "superclass not defined"))
      (True
       (Some (modify ce i (Class (Tuple is Nil)))))))

  (define addPreludeClasses
    (compose addCoreClasses addNumClasses))

  (define addCoreClasses
    (foldr compose
           Some
           (make-list
            (addClass (Id "Eq") Nil)
            (addClass (Id "Ord") (make-list (Id "Eq")))
            (addClass (Id "Show") Nil)
            (addClass (Id "Read") Nil)
            (addClass (Id "Bounded") Nil)
            (addClass (Id "Enum") Nil)
            (addClass (Id "Functor") Nil)
            (addClass (Id "Monad") Nil))))

  (define addNumClasses
    (foldr compose
           Some
           (make-list
            (addClass (Id "Num") (make-list (Id "Eq") (Id "Show")))
            (addClass (Id "Real") (make-list (Id "Num") (Id "Ord")))
            (addClass (Id "Fractional") (make-list (Id "Num")))
            (addClass (Id "Integral") (make-list (Id "Real") (Id "Enum")))
            (addClass (Id "RealFrac") (make-list (Id "Real") (Id "Fractional")))
            (addClass (Id "Floating") (make-list (Id "Fractional")))
            (addClass (Id "RealFloat") (make-list (Id "RealFrac") (Id "Floating"))))))


  (declare addInst ((List Pred) -> Pred -> (ClassEnv -> (Optional ClassEnv))))
  (define (addInst ps p ce)
    (match p
      ((IsIn i _)
       (let ((its (insts ce i))
             (qs (map
                  (fn (x)
                    (match x
                      ((Instance (Qual _ q)) q)))
                  its))
             (c (Class
                 (Tuple (super ce i)
                        (Cons (Instance (Qual ps p)) its)))))
         (cond
           ((not (defined (classes ce i)))
            (fail "No class for instance"))
           ((any (overlap p) qs)
            (fail "Overlapping instance"))
           (True
            (pure (modify ce i c))))))))

  (declare overlap (Pred -> Pred -> Boolean))
  (define (overlap p q)
    (defined (mguPred p q)))

  (declare exampleInsts (ClassEnv -> (Optional ClassEnv)))
  (define (exampleInsts ce)
    ((foldr compose
            Some
            (make-list
             (addInst Nil (IsIn (Id "Ord") tUnit))
             (addInst Nil (IsIn (Id "Ord") tChar))
             (addInst Nil (IsIn (Id "Ord") tInt))
             (addInst (make-list (IsIn (Id "Ord") (TVar (Tyvar (Id "a") Star)))
                                 (IsIn (Id "Ord") (TVar (Tyvar (Id "b") Star))))
                      (IsIn (Id "Ord") (mkPair (TVar (Tyvar (Id "a") Star))
                                               (TVar (Tyvar (Id "b") Star)))))))
     ce))


  ;;
  ;; Entailment
  ;;

  (declare bySuper (ClassEnv -> Pred -> (List Pred)))
  (define (bySuper ce p)
    (match p
      ((IsIn i t)
       (Cons p (fold append Nil
                     (map (fn (i_)
                            (bySuper ce (IsIn i_ t)))
                          (super ce i)))))))

  (declare byInst (ClassEnv -> Pred -> (Optional (List Pred))))
  (define (byInst ce p)
    (match p
      ((IsIn i _)
       (let ((tryInst
               (fn (x)
                 (match x
                   ((Instance (Qual ps h))
                    (>>= (matchPred h p)
                         (fn (u)
                           (Some (map (apply u) ps)))))))))
         (asum (map tryInst (insts ce i)))))))

  (declare entail (ClassEnv -> (List Pred) -> Pred -> Boolean))
  (define (entail ce ps p)
    (or (any
         (list:member p)
         (map (bySuper ce) ps))
        (match (byInst ce p)
          ((None)
           False)
          ((Some qs)
           (all (entail ce ps) qs)))))

  (declare inHnf (Pred -> Boolean))
  (define (inHnf pred)
    (let ((hnf (fn (t)
                 (match t
                   ((TVar _) True)
                   ((TCon _) False)
                   ((TAp t _) (hnf t))
                   (_ (error "unreachable"))))))
      (match pred
        ((IsIn _ t) (hnf t)))))

  (declare toHnfs (MonadFail :m => (ClassEnv -> (List Pred) -> (:m (List Pred)))))
  (define (toHnfs ce ps)
    (>>= (traverse (toHnf ce) ps)
         (fn (pss)
           (pure (foldr append Nil pss)))))

  (declare toHnf (MonadFail :m => (ClassEnv -> Pred -> (:m (List Pred)))))
  (define (toHnf ce p)
    (if (inHnf p)
        (pure (make-list p))
        (match (byInst ce p)
          ((None)
           (fail "Context reduction"))
          ((Some ps)
           (toHnfs ce ps)))))

  (declare simplify (ClassEnv -> (List Pred) -> (List Pred)))
  (define (simplify ce xs)
    (let ((loop (fn (rs xs)
                  (match xs
                    ((Nil)
                     rs)
                    ((Cons p ps)
                     (if (entail ce (append rs ps) p)
                         (loop rs ps)
                         (loop (Cons p rs) ps)))))))
      (loop Nil xs)))

  (declare reduce (MonadFail :m => (ClassEnv -> (List Pred) -> (:m (List Pred)))))
  (define (reduce ce ps)
    (>>= (toHnfs ce ps)
         (fn (qs)
           (pure (simplify ce qs)))))

  (declare scEntail (ClassEnv -> (List Pred) -> Pred -> Boolean))
  (define (scEntail ce ps p)
    (any (list:member p)
         (map (bySuper ce) ps)))


  ;;
  ;; Type Schemes
  ;;

  (define-type Scheme
    (Forall (List Kind) (Qual Type)))

  (define-instance (Eq Scheme)
    (define (== x y)
      (match (Tuple x y)
        ((Tuple (Forall ks1 q1)
                (Forall ks2 q2))
         (and (== ks1 ks2)
              (== q1 q2))))))

  (define-instance (Types Scheme)
    (define (apply s t)
      (match t
        ((Forall ks qt)
         (Forall ks (apply s qt)))))
    (define (tv t)
      (match t
        ((Forall _ qt)
         (tv qt)))))

  (declare quantify ((List Tyvar) -> (Qual Type) -> Scheme))
  (define (quantify vs qt)
    (let ((vs_ (filter
                (fn (e)
                  (list:member e vs))
                (tv qt)))
          (ks (map kind vs_))
          (gens (map TGen (range 0 (- (length vs_) 1))))
          (s (Subst (zip vs_ gens))))
      (Forall ks (apply s qt))))

  (declare toScheme (Type -> Scheme))
  (define (toScheme t)
    (Forall Nil (Qual Nil t)))


  ;;
  ;; Assumptions
  ;;

  (define-type Assump
    (Assump Id Scheme))

  (define-instance (Eq Assump)
    (define (== a b)
      (match (Tuple a b)
        ((Tuple (Assump id-a scheme-a) (Assump id-b scheme-b))
         (and (== id-a id-b)
              (== scheme-a scheme-b))))))

  (define-instance (Types Assump)
    (define (apply s a)
      (match a
        ((Assump i sc)
         (Assump i (apply s sc)))))
    (define (tv a)
      (match a
        ((Assump _ sc)
         (tv sc)))))

  (declare find (MonadFail :m => (Id -> (List Assump) -> (:m Scheme))))
  (define (find i xs)
    (match xs
      ((Nil)
       (fail (<> "Unbound identifier: " (id->string i))))
      ((Cons (Assump i_ sc) as)
       (if (== i i_)
           (pure sc)
           (find i as)))))


  ;;
  ;; Type Inference Monad
  ;;

  (define-type (TI :a)
    (TI (Subst -> Integer -> (Tuple3 Subst Integer :a))))

  (declare get-ti ((TI :a) -> (Subst -> Integer -> (Tuple3 Subst Integer :a))))
  (define (get-ti x)
    (match x
      ((TI y) y)))

  (define-instance (Functor TI)
    (define (map f x)
      (>>= x
           (fn (x)
             (pure (f x))))))

  (define-instance (Applicative TI)
    (define (pure x)
      (TI (fn (s n)
            (Tuple3 s n x))))
    (define (liftA2 f m1 m2)
      (>>= m1
           (fn (a)
             (>>= m2
                  (fn (b)
                    (pure (f a b))))))))

  (define-instance (Monad TI)
    (define (>>= f g)
      (TI (fn (s n)
            (match ((get-ti f) s n)
              ((Tuple3 s_ m x)
               (let ((gx (get-ti (g x))))
                 (gx s_ m))))))))

  (define-instance (MonadFail TI)
    (define fail error))

  (declare runTI ((TI :a) -> :a))
  (define (runTI t)
    (match ((get-ti t) nullSubst 0)
      ((Tuple3 _ _ x) x)))


  (declare getSubst (TI Subst))
  (define getSubst (TI (fn (s n)
                         (Tuple3 s n s))))

  (declare unify (Type -> Type -> (TI Unit)))
  (define (unify t1 t2)
    (do (s <- getSubst)
        (u <- (mgu (apply s t1) (apply s t2)))
      (extSubst u)))

  (declare extSubst (Subst -> (TI Unit)))
  (define (extSubst s_)
    (TI (fn (s n)
          (Tuple3 ( @@ s_ s) n Unit))))

  (declare newTVar (Kind -> (TI Type)))
  (define (newTVar k)
    (TI (fn (s n)
          (let ((v (Tyvar (enumId n) k)))
            (Tuple3 s (+ 1 n) (TVar v))))))

  (declare freshInst (Scheme -> (TI (Qual Type))))
  (define (freshInst s)
    (match s
      ((Forall ks qt)
       (do (ts <- (traverse newTVar ks))
           (pure (inst ts qt))))))


  (define-class (Instantiate :t)
    (inst ((List Type) -> :t -> :t)))

  (define-instance (Instantiate Type)
    (define (inst ts t)
      (match t
        ((TAp l r) (TAp (inst ts l) (inst ts r)))
        ((TGen n)  (from-some "Failed to find TGen type" (list:index n ts)))
        (_ t))))

  (define-instance (Instantiate :a => Instantiate (List :a))
    (define (inst ts)
      (map (inst ts))))

  (define-instance (Instantiate :t => Instantiate (Qual :t))
    (define (inst ts q)
      (match q
        ((Qual ps t) (Qual (inst ts ps)
                           (inst ts t))))))

  (define-instance (Instantiate Pred)
    (define (inst ts p)
      (match p
        ((IsIn c t) (IsIn c (inst ts t))))))


  ;;
  ;; Type Inference
  ;;

  ;; (define-type-alias (Infer :e :t) (ClassEnv -> (List Assump) -> :e -> (TI (Tuple (List Pred) :t))))

  ;; Literals

  (define-type Literal
    (LitInt Integer)
    (LitChar Char)
    ;; NOTE: Rationals are ignored because coalton does not have support
    (LitStr String))

  (declare tiLit (Literal -> (TI (Tuple (List Pred) Type))))
  (define (tiLit lit)
    (match lit
      ((LitChar _) (pure (Tuple Nil tChar)))
      ((LitInt _)  (do (v <- (newTVar Star))
                       (pure (Tuple (make-list (IsIn (Id "Num") v)) v))))
      ((LitStr _)  (pure (Tuple Nil tString)))))

  ;; Patterns

  (define-type Pat
    (PVar Id)
    PWildcard
    (PAs Id Pat)
    (PLit Literal)
    (PNpk Id Integer)
    (PCon Assump (List Pat)))

  (declare tiPat (Pat -> (TI (Tuple3 (List Pred) (List Assump) Type))))
  (define (tiPat pat)
    (match pat
      ((PVar i)
       (do (v <- (newTVar Star))
           (pure (Tuple3 Nil
                         (make-list (Assump i (toScheme v)))
                         v))))
      ((PWildcard)
       (do (v <- (newTVar Star))
           (pure (Tuple3 Nil
                         Nil
                         v))))
      ((PAs i pat)
       (do (x <- (tiPat pat))
           (match x
             ((Tuple3 ps as t)
              (pure (Tuple3 ps
                            (Cons (Assump i (toScheme t)) as)
                            t))))))
      ((PLit l)
       (do (x <- (tiLit l))
           (match x
             ((Tuple ps t)
              (pure (Tuple3 ps Nil t))))))
      ((PNpk i _)
       (do (t <- (newTVar Star))
           (pure (Tuple3 (make-list (IsIn (Id "Integral") t))
                         (make-list (Assump i (toScheme t)))
                         t))))
      ((PCon (Assump _ sc) pats)
       (do (x <- (tiPats pats))
           (t_ <- (newTVar Star))
         (a <- (freshInst sc))
         (match (Tuple x a)
           ((Tuple (Tuple3 ps as ts)
                   (Qual qs t))
            (do (unify t (foldr mkFn t_ ts))
                (pure (Tuple3 (append ps qs)
                              as
                              t_)))))))))

  (declare tiPats ((List Pat) -> (TI (Tuple3 (List Pred) (List Assump) (List Type)))))
  (define (tiPats pats)
    (do (psasts <- (traverse tiPat pats))
        (pure (foldr
               (fn (xyz acc)
                 (match (Tuple acc xyz)
                   ((Tuple (Tuple3 ps as ts)
                           (Tuple3 ps_ as_ t))
                    (Tuple3 (append ps_ ps)
                            (append as_ as)
                            (Cons t ts)))))
               (Tuple3 Nil Nil Nil)
               psasts))))

  ;; Expressions

  (define-type Expr
    (Var Id)
    (Lit Literal)
    (Const Assump)
    (Ap Expr Expr)
    (ELet BindGroup Expr))

  (declare tiExpr (ClassEnv -> (List Assump) -> Expr -> (TI (Tuple (List Pred) Type))))
  (define (tiExpr ce as e)
    (match e
      ((Var i)
       (do (sc <- (find i as))
           (x <- (freshInst sc))
         (match x
           ((Qual ps t)
            (pure (Tuple ps t))))))
      ((Const (Assump _ sc))
       (do (x <- (freshInst sc))
           (match x
             ((Qual ps t)
              (pure (Tuple ps t))))))
      ((Lit l)
       (tiLit l))
      ((Ap e f)
       (do (ee <- (tiExpr ce as e))
           (fe <- (tiExpr ce as f))
         (match (Tuple ee fe)
           ((Tuple (Tuple ps te)
                   (Tuple qs tf))
            (do (t <- (newTVar Star))
                (unify (mkFn tf t) te)
              (pure (Tuple (append ps qs) t)))))))
      ((ELet bg e)
       (do (tiBg <- (tiBindGroup ce as bg))
           (match tiBg
             ((Tuple ps as_)
              (do (tiEx <- (tiExpr ce (append as_ as) e))
                  (match tiEx
                    ((Tuple qs t)
                     (pure (Tuple (append ps qs) t)))))))))))

  ;; Alternatives

  (define-type Alt
    (Alt (List Pat) Expr))

  (declare tiAlt (ClassEnv -> (List Assump) -> Alt -> (TI (Tuple (List Pred) Type))))
  (define (tiAlt ce as alt_)
    (match alt_
      ((Alt pats e)
       (do (pats_ <- (tiPats pats))
           (match pats_
             ((Tuple3 ps as_ ts)
              (do (expr <- (tiExpr ce (append as_ as) e))
                  (match expr
                    ((Tuple qs t)
                     (pure (Tuple (append ps qs) (foldr mkFn t ts))))))))))))

  (declare tiAlts (ClassEnv -> (List Assump) -> (List Alt) -> Type -> (TI (List Pred))))
  (define (tiAlts ce as alts t)
    (do (psts <- (traverse (tiAlt ce as) alts))
        (traverse (unify t) (map snd psts))
      (pure (concat (map fst psts)))))

  ;; From Types to Type Schemes

  (declare split (MonadFail :m => (ClassEnv -> (List Tyvar) -> (List Tyvar) -> (List Pred) -> (:m (Tuple (List Pred) (List Pred))))))
  (define (split ce fs gs ps)
    (do (ps_ <- (reduce ce ps))
        (match (list:partition (fn (x) (all (flip list:member fs) (tv x)))
                               ps_)
          ((Tuple ds rs)
           (do (rs_ <- (defaultedPreds ce (append fs gs) rs))
               (pure (Tuple ds (list:difference rs rs_))))))))

  (define-type Ambiguity
    (Ambiguity Tyvar (List Pred)))

  (declare ambiguities (ClassEnv -> (List Tyvar) -> (List Pred) -> (List Ambiguity)))
  (define (ambiguities ce vs ps)
    (map (fn (v)
           (Ambiguity v (filter
                         (fn (x)
                           (list:member v (tv x)))
                         ps)))
         (list:difference (tv ps) vs)))

  (declare numClasses (List Id))
  (define numClasses
    (map Id
         (make-list "Num" "Integral" "Floating" "Fractional" "Real" "RealFloat" "RealFrac")))

  (declare stdClasses (List Id))
  (define stdClasses
    (append
     (map Id
          (make-list "Eq" "Ord" "Show" "Read" "Bounded" "Enum" "Ix" "Functor" "Monad" "MonadPlus"))
     numClasses))

  (declare candidates (ClassEnv -> Ambiguity -> (List Type)))
  (define (candidates ce am)
    (match am
      ((Ambiguity v qs)
       (let ((is (map
                  (fn (x)
                    (match x
                      ((IsIn i _)
                       i)))
                  qs))
             (ts (map
                  (fn (x)
                    (match x
                      ((IsIn _ t)
                       t)))
                  qs)))
         (if (and (all (== (TVar v)) ts)
                  (and (any (flip list:member numClasses) is)
                       (all (flip list:member stdClasses) is)))
             (let ((ts_ (defaults ce)))
               (if (all (fn (t_)
                          (all (entail ce Nil)
                               (map (fn (i) (IsIn i t_))
                                    is)))
                        ts_)
                   ts_
                   Nil))
             Nil)))))

  (declare withDefaults (MonadFail :m => (((List Ambiguity) -> (List Type) -> :a) -> ClassEnv -> (List Tyvar) -> (List Pred) -> (:m :a))))
  (define (withDefaults f ce vs ps)
    (let ((vps (ambiguities ce vs ps))
          (tss (map (candidates ce) vps)))
      (if (any list:null? tss)
          (fail "Cannot resolve ambiguity")
          (pure (f vps (map (fn (l) (from-some "" (head l))) tss))))))

  (declare defaultedPreds (MonadFail :m => (ClassEnv -> (List Tyvar) -> (List Pred) -> (:m (List Pred)))))
  (define defaultedPreds
    (withDefaults (fn (vps ts) (concat (map (fn (a)
                                              (match a
                                                ((Ambiguity _ x) x)))
                                            vps)))))

  (declare defaultSubst (MonadFail :m => (ClassEnv -> (List Tyvar) -> (List Pred) -> (:m Subst))))
  (define defaultSubst
    (withDefaults (fn (vps ts)
                    (Subst (zip (map (fn (a)
                                       (match a
                                         ((Ambiguity x _) x)))
                                     vps)
                                ts)))))

  ;; Binding Groups

  (define-type Expl
    (Expl Id Scheme (List Alt)))

  (declare tiExpl (ClassEnv -> (List Assump) -> Expl -> (TI (List Pred))))
  (define (tiExpl ce as e)
    (match e
      ((Expl _ sc alts)
       (do (qt <- (freshInst sc))
           (match qt
             ((Qual qs t)
              (do (ps <- (tiAlts ce as alts t))
                  (s <- getSubst)
                (let qs_ = (apply s qs))
                (let t_  = (apply s t))
                (let fs  = (tv (apply s as)))
                (let gs  = (list:difference (tv t_) fs))
                (let sc_ = (quantify gs (Qual qs_ t_)))
                (let ps_ = (filter (fn (p) (not (entail ce qs_ p)))
                                   (apply s ps)))
                (ps <- (split ce fs gs ps_))
                (match ps
                  ((Tuple ds rs)
                   (cond
                     ((/= sc sc_)
                      (fail "Signature too general"))
                     ((not (list:null? rs))
                      (fail "Context too weak"))
                     (True
                      (pure ds))))))))))))


  (define-type Impl
    (Impl Id (List Alt)))

  (declare restricted ((List Impl) -> Boolean))
  (define (restricted bs)
    (let ((simple (fn (x)
                    (match x
                      ((Impl _ alts)
                       (any (fn (x)
                              (match x
                                ((Alt pats _)
                                 (list:null? pats))))
                            alts))))))
      (any simple bs)))

  (declare tiImpls (ClassEnv -> (List Assump) -> (List Impl) -> (TI (Tuple (List Pred) (List Assump)))))
  (define (tiImpls ce as bs)
    (do (ts <- (traverse (fn (_) (newTVar Star)) bs))
        (let is    = (map (fn (b)
                            (match b
                              ((Impl i _) i)))
                          bs))
      (let scs   = (map toScheme ts))
      (let as_   = (append (zipWith Assump is scs) as))
      (let altss = (map (fn (b)
                          (match b
                            ((Impl _ a) a)))
                        bs))
      (pss <- (sequence (zipWith (tiAlts ce as_) altss ts)))
      (s <- getSubst)
      (let ps_ = (apply s (concat pss)))
      (let ts_ = (apply s ts))
      (let fs  = (tv (apply s as)))
      (let vss = (map tv ts_))
      (let gs  = (list:difference
                  (match vss
                    ((Cons vs vss)
                     (foldr list:union vs vss))
                    (_ (error "unreachable")))
                  fs))
      (ps <- (split ce fs
                    (match vss
                      ((Cons vs vss)
                       (foldr list:intersection vs vss))
                      (_ (error "unreachable")))
                    ps_))
      (match ps
        ((Tuple ds rs)
         (if (restricted bs)
             (let ((gs_ (list:difference gs (tv rs)))
                   (scs_ (map (fn (x)
                                (quantify gs_ (Qual Nil x)))
                              ts_)))
               (pure (Tuple (append ds rs)
                            (zipWith Assump is scs_))))
             (let ((scs_ (map (fn (x)
                                (quantify gs (Qual rs x)))
                              ts_)))
               (pure (Tuple ds
                            (zipWith Assump is scs_)))))))))

  (define-type BindGroup
    (BindGroup (List Expl) (List (List Impl))))

  (declare tiBindGroup (ClassEnv -> (List Assump) -> BindGroup -> (TI (Tuple (List Pred) (List Assump)))))
  (define (tiBindGroup ce as bg)
    (match bg
      ((BindGroup es iss)
       (do (let as_ = (map (fn (x)
                             (match x
                               ((Expl v sc _)
                                (Assump v sc))))
                           es))
           (tiI <- (tiSeq tiImpls ce (append as_ as) iss))
         (match tiI
           ((Tuple ps as__)
            (do (qss <- (traverse (tiExpl ce (append as__ (append as_ as))) es))
                (pure (Tuple (append ps (concat qss))
                             (append as__ as_))))))))))

  (declare tiSeq ((ClassEnv -> (List Assump) -> :bg -> (TI (Tuple (List Pred) (List Assump)))) ->
                  (ClassEnv -> (List Assump) -> (List :bg) -> (TI (Tuple (List Pred) (List Assump))))))
  (define (tiSeq ti ce as bg)
    (match bg
      ((Nil)
       (pure (Tuple Nil Nil)))
      ((Cons bs bss)
       (do (x <- (ti ce as bs))
           (match x
             ((Tuple ps as_)
              (do (x <- (tiSeq ti ce (append as_ as) bss))
                  (match x
                    ((Tuple qs as__)
                     (pure (Tuple (append ps qs)
                                  (append as__ as_))))))))))))


  (define-type Program
    (Program (List BindGroup)))

  (declare tiProgram (ClassEnv -> (List Assump) -> Program -> (List Assump)))
  (define (tiProgram ce as bgs)
    (match bgs
      ((Program bgs)
       (runTI
        (do (seq <- (tiSeq tiBindGroup ce as bgs))
            (match seq
              ((Tuple ps as_)
               (do (s  <- getSubst)
                   (rs <- (reduce ce (apply s ps)))
                 (s_ <- (defaultSubst ce Nil rs))
                 (pure (apply (@@ s_ s) as_))))))))))

  (declare assumption-list-equal ((List Assump) -> (List Assump) -> Boolean))
  (define (assumption-list-equal a b)
    (== a b)))
