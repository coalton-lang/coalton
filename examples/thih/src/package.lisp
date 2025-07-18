(in-package #:cl-user)

(defpackage #:thih-coalton
  (:documentation "Public interface to THIH-COALTON.")
  (:use #:coalton
        #:coalton-prelude
        #:coalton-library/collections/immutable/list)
  (:shadow #:id #:find #:const
           #:type #:compose #:alt
           #:split #:reduce #:lift)
  (:export
   #:Id
   
   #:Kind
   #:Star
   #:KFun

   #:Type
   #:TVar #:Tyvar
   #:TCon #:Tycon
   #:TAp
   #:TGen

   #:tUnit
   #:tChar
   #:tInt
   #:tInteger
   #:tFloat
   #:tDouble
   #:tList
   #:tArrow
   #:tTuple2
   #:tString

   #:mkFn
   #:mkList
   #:mkPair

   #:Qual

   #:Pred
   #:IsIn

   #:initialEnv
   #:compose
   #:addPreludeClasses
   #:addCoreClasses
   #:addNumClasses
   #:exampleInsts

   #:Scheme
   #:Forall

   #:Assump

   #:Literal
   #:LitInt
   #:LitChar
   #:LitStr

   #:Pat
   #:PVar
   #:PWildcard
   #:PAs
   #:PLit
   #:PNpk
   #:PCon

   #:Expr
   #:Var
   #:Lit
   #:Const
   #:Ap
   #:ELet

   #:Alt
   
   #:Expl
   #:Impl

   #:BindGroup
   #:Program

   #:tiProgram))
