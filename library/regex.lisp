(coalton-library/utils:defstdlib-package #:coalton-library/regex
  (:use
   #:coalton
   #:coalton-library/builtin
   #:coalton-library/classes
   #:coalton-library/tuple)
  (:local-nicknames (#:vec #:coalton-library/vector)
                    (#:cell #:coalton-library/cell)
                    (#:state #:coalton-library/monad/state)
                    (#:math #:coalton-library/math)
                    (#:str #:coalton-library/string))
  ;(:export)
  )

(in-package #:coalton-library/regex)

(named-readtables:in-readtable coalton:coalton)

#+coalton-release
(cl:declaim #.coalton-impl/settings:*coalton-optimize-library*)

;; first step is building the regex nfa
;; this should be a structure of nfa nodes which can be traversed to verify the matching

;; Thompson NFA
;; https://swtch.com/~rsc/regexp/regexp1.html

(coalton-toplevel


  (define-struct Node
    (Edges (vec:Vector UFix) ;; list of edge pointers
         ))

  (define-struct Edge
    (C Char)     ;; matching char
    (Target (cell:Cell UFix))  ;; pointer to end node
    )

  (define-struct NFA
    (nodestack (vec:Vector Node))
    (edgestack (vec:Vector Edge)))

  (declare init-env (Unit -> NFA))
  (define (init-env)
    "Initializes an environment with one empty node."
    (NFA (vec:with-initial-element 1 (Node (vec:new)))
         (vec:new))))

(coalton-toplevel

  (declare next-node (NFA -> UFix))
  (define (next-node (NFA nodestack _ ))
    (vec:length nodestack))

  (declare current-node (NFA -> UFix))
  (define (current-node (NFA nodestack _ ))
    (math:1- (vec:length nodestack)))

  (declare push-node (Node -> (state:ST NFA Unit)))
  (define (push-node node)
    "Pushes a node onto the nodestack."
    (do
     (env <- state:get)
     (pure (vec:push! node (.nodestack env)))
      (state:put env)))

  (declare new-node (Unit -> (state:ST NFA Unit)))
  (define (new-node)
    "Adds a new empty node to the nodestack."
    (push-node (Node (vec:new))))
  
  (declare add-node-edge (UFix -> UFix -> (state:ST NFA Unit)))
  (define (add-node-edge node edge)
    "Adds an edge pointer to an index node."
    (do
     (env <- state:get)
     (pure (match (vec:find-elem edge (.edges (vec:index-unsafe node (.nodestack env))))
             ((Some _)
              Unit)
             ((None)
              (vec:push! edge (.edges (vec:index-unsafe node (.nodestack env))))
              Unit)))
      (state:put env)))

  (declare add-edge (Edge -> (state:ST NFA Unit)))
  (define (add-edge edge)
    "Adds an edge."
    (do
     (env <- state:get)
     (pure (vec:push! edge (.edgestack env)))
      (state:put env)))

  (declare next-edge (NFA -> UFix))
  (define (next-edge (NFA _ edgestack))
    (vec:length edgestack))

  (declare current-edge (NFA -> UFix))
  (define (current-edge (NFA _ edgestack))
    (math:1- (vec:length edgestack)))

  (declare set-edge-target (UFix -> UFix -> (state:ST NFA Unit)))
  (define (set-edge-target edge target)
    "Sets the target node pointer for a given edge"
    (do
     (env <- state:get)
     (pure (cell:write! (.target (vec:index-unsafe edge (.edgestack env))) target))
      (state:put env)))
  

  )

(coalton-toplevel

  (define-type Regex
    (RChar Char)
    (RConcat Regex Regex)
    (RAlt Regex Regex)
    (R? Regex)
    (R* Regex)
    (R+ Regex)
    (R. Regex)
    (REps))

  (declare add-char (Char -> (state:ST NFA Unit)))
  (define (add-char char)
         (do
          (env <- state:get)
          (add-node-edge
           (current-node env)
           (next-edge env))
           (add-edge (Edge char
                           (cell:new (next-node env))))
           (new-node)))

  (declare add-concat (Regex -> Regex -> (state:ST NFA Unit)))
  (define (add-concat regex1 regex2)
    (do
     (NFAful regex1)
     (NFAful regex2)))

  (declare add-alt (Regex -> Regex -> (state:ST NFA Unit)))
  (define (add-alt regex1 regex2)
    (do
     (env <- state:get)
     (let current = (current-node env))
      (NFAful regex1)
      (let REnd = (current-edge env))
      (pure (vec:pop! (.nodestack env)))
      (add-node-edge current (next-edge env))
      (NFAful regex2)
      
      (set-edge-target REnd (current-node env))))

  (declare add-* (Regex -> (state:ST NFA Unit)))
  (define (add-* regex)
    (do
     (env <- state:get)
     (let current = (current-node env))
      (NFAful regex)
      (set-edge-target (current-edge env) current)
      (add-node-edge current (next-edge env))))

  (declare add-? (Regex -> (state:ST NFA Unit)))
  (define (add-? regex)
    (do
     (env <- state:get)
     (let current = (current-node env))
      (NFAful regex)
      (add-node-edge current (next-edge env))))

  (declare add-+ (Regex -> (state:ST NFA Unit)))
  (define (add-+ regex)
    (do
     (env <- state:get)
     (let current = (current-node env))
      (NFAful regex)
      (set-edge-target (current-edge env) current)
      (add-node-edge current (next-edge env))
      (NFAful regex)))

  ;; TODO
  #+inore(define (add.))
  
  (declare NFAful (Regex -> (state:ST NFA Unit)))
  (define (NFAful regex)
    "Builds an NFA."
    (match regex
      ((RChar c)
       (add-char c))
      ((RConcat regex1 regex2)
       (add-concat regex1 regex2))
      ((RAlt regex1 regex2)
       (add-alt regex1 regex2))
      ((R* regex)
       (add-* regex))
      ((R? regex)
       (add-? regex))
      ((R+ regex)
       (add-+ regex))
      (_ (error "not yet"))))

  (declare make-nfa (Regex -> NFA))
  (define (make-nfa regex)
    (fst (state:run (NFAful regex) (init-env)))))



;; break input into iterator of chars
#+i(coalton-toplevel

  (declare traverse-edge (Char -> UFix -> (state:ST NFA Boolean)))
  (define (traverse-edge char edge)
    (do
     (env <- state:get)
     (let e = (vec:index-unsafe edge (.edgestack env)))
      (if (== char (.c e))
          (traverse-edges (.target e))
            (pure False))))

  (declare traverse-edges (Char -> UFix -> (state:ST NFA Boolean)))
  (define (traverse-edges char node)
    "Traverses all edges leading from a node."
    (do
     (env <- state:get)
     (let n = (vec:index-unsafe node (.nodestack env)))
      (if (vec:empty? (.edges n))
          (pure True)
          (for e in (.edges n)
               (traverse-edge e)))))
  
  (define (rmatch regex string)
    (let nfa = (make-NFA regex))
    (let s = (str:chars string))
    
    ())
  )


#+sb-package-locks
(sb-ext:lock-package "COALTON-LIBRARY/REGEX")
