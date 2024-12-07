(ns cljcc.schema
  (:require [cljcc.token :as t]))

(declare Statement Exp Declaration Block Type)

(def StorageClass [:enum :static :extern])

(def IntType
  [:map
   [:type [:= :int]]])

(def LongType
  [:map
   [:type [:= :long]]])

(def FunType
  [:map
   [:type [:= :function]]
   [:return-type [:ref #'Type]]
   [:parameter-types [:vector [:ref #'Type]]]])

(def Type
  [:schema {:registry {::mtype-int #'IntType
                       ::mtype-long #'LongType
                       ::mtype-function #'FunType}}
   [:multi {:dispatch :type}
    [:int #'IntType]
    [:long #'LongType]
    [:function #'FunType]]])

(def Const
  [:map
   [:type [:enum :int :long]]
   [:value int?]])

(def ConstantExp
  [:map
   [:type [:= :exp]]
   [:exp-type [:= :constant-exp]]
   [:value #'Const]
   [:value-type {:optional true} #'Type]])

(def VariableExp
  [:map
   [:type [:= :exp]]
   [:exp-type [:= :variable-exp]]
   [:identifier string?]
   [:value-type {:optional true} #'Type]])

(def CastExp
  [:map
   [:type [:= :exp]]
   [:exp-type [:= :cast-exp]]
   [:target-type #'Type]
   [:typed-inner [:ref #'Exp]]
   [:value [:ref #'Exp]]
   [:children [:= [:value]]]
   [:value-type {:optional true} #'Type]])

(def UnaryExp
  [:map
   [:type [:= :exp]]
   [:exp-type [:= :unary-exp]]
   [:unary-operator `[:enum ~@t/unary-ops]]
   [:value [:ref #'Exp]]
   [:children [:= [:value]]]
   [:value-type {:optional true} #'Type]])

(def BinaryExp
  [:map
   [:type [:= :exp]]
   [:exp-type [:= :binary-exp]]
   [:binary-operator `[:enum ~@(set (keys t/bin-ops))]]
   [:left [:ref #'Exp]]
   [:right [:ref #'Exp]]
   [:children [:= [:left :right]]]
   [:value-type {:optional true} #'Type]])

(def AssignmentExp
  [:map
   [:type [:= :exp]]
   [:exp-type [:= :assignment-exp]]
   [:assignment-operator `[:enum ~@t/assignment-ops]]
   [:children [:= [:left :right]]]
   [:left [:ref #'Exp]]
   [:right [:ref #'Exp]]
   [:value-type {:optional true} #'Type]])

(def ConditionalExp
  [:map
   [:type [:= :exp]]
   [:exp-type [:= :conditional-exp]]
   [:children [:= [:left :right :middle]]]
   [:left [:ref #'Exp]]
   [:middle [:ref #'Exp]]
   [:right [:ref #'Exp]]
   [:value-type {:optional true} #'Type]])

(def FunctionCallExp
  [:map
   [:type [:= :exp]]
   [:exp-type [:= :function-call-exp]]
   [:identifier string?]
   [:arguments [:vector [:ref #'Exp]]]
   [:children [:= [:arguments]]]
   [:value-type {:optional true} #'Type]])

(def Exp
  [:schema {:registry {::mexp-constant #'ConstantExp
                       ::mexp-variable #'VariableExp
                       ::mexp-cast #'CastExp
                       ::mexp-unary #'UnaryExp
                       ::mexp-binary #'BinaryExp
                       ::mexp-assignment #'AssignmentExp

                       ::mexp-conditional #'ConditionalExp
                       ::mexp-function-call #'FunctionCallExp}}
   [:multi {:dispatch :exp-type}
    [:constant-exp #'ConstantExp]
    [:variable-exp #'VariableExp]
    [:cast-exp #'CastExp]
    [:unary-exp #'UnaryExp]
    [:binary-exp #'BinaryExp]
    [:assignment-exp #'AssignmentExp]
    [:conditional-exp #'ConditionalExp]
    [:function-call-exp #'FunctionCallExp]]])

(def VarDeclaration
  [:map
   [:type [:= :declaration]]
   [:declaration-type [:= :variable]]
   [:variable-type #'Type]
   [:storage-class [:maybe #'StorageClass]]
   [:identifier string?]
   [:initial [:maybe #'Exp]]])

(def FunDeclaration
  [:map
   [:type [:= :declaration]]
   [:declaration-type [:= :function]]
   [:function-type #'FunType]
   [:identifier string?]
   [:storage-class [:maybe #'StorageClass]]
   [:parameters [:vector string?]]
   [:body [:maybe [:ref #'Block]]]])

(def ReturnStatement
  [:map
   [:type [:= :statement]]
   [:statement-type [:= :return]]
   [:value #'Exp]])

(def ExpressionStatement
  [:map
   [:type [:= :statement]]
   [:statement-type [:= :expression]]
   [:value #'Exp]])

(def BreakStatement
  [:map
   [:type [:= :statement]]
   [:statement-type [:= :break]]
   [:label [:maybe string?]]])

(def ContinueStatement
  [:map
   [:type [:= :statement]]
   [:statement-type [:= :continue]]
   [:label [:maybe string?]]])

(def EmptyStatement
  [:map
   [:type [:= :statement]]
   [:statement-type [:= :empty]]])

(def WhileStatement
  [:map
   [:type [:= :statement]]
   [:statement-type [:= :while]]
   [:condition #'Exp]
   [:label {:optional true} string?]
   [:body [:ref #'Statement]]])

(def DoWhileStatement
  [:map
   [:type [:= :statement]]
   [:statement-type [:= :do-while]]
   [:condition #'Exp]
   [:label {:optional true} string?]
   [:body [:ref #'Statement]]])

(def ForStatement
  [:map
   [:type [:= :statement]]
   [:statement-type [:= :for]]
   [:init [:or
           [:ref #'VarDeclaration]
           [:maybe #'Exp]]]
   [:post [:maybe #'Exp]]
   [:condition [:maybe #'Exp]]
   [:label {:optional true} string?]
   [:body [:ref #'Statement]]])

(def IfStatement
  [:map
   [:type [:= :statement]]
   [:statement-type [:= :if]]
   [:condition #'Exp]
   [:then-statement [:ref #'Statement]]
   [:else-statement [:maybe [:ref #'Statement]]]])

(def CompoundStatement
  [:map
   [:type [:= :statement]]
   [:statement-type [:= :compound]]
   [:block [:ref #'Block]]])

(def Statement
  [:schema {:registry {::mstatement-return #'ReturnStatement
                       ::mstatement-expression #'ExpressionStatement
                       ::mstatement-break #'BreakStatement
                       ::mstatement-continue #'ContinueStatement
                       ::mstatement-empty #'EmptyStatement
                       ::mstatement-for #'ForStatement
                       ::mstatement-while #'WhileStatement
                       ::mstatement-do-while #'DoWhileStatement
                       ::mstatement-compound #'CompoundStatement
                       ::mstatement-if #'IfStatement}}
   [:multi {:dispatch :statement-type}
    [:return #'ReturnStatement]
    [:expression #'ExpressionStatement]
    [:break #'BreakStatement]
    [:continue #'ContinueStatement]
    [:empty #'EmptyStatement]
    [:compound #'CompoundStatement]
    [:while #'WhileStatement]
    [:do-while #'DoWhileStatement]
    [:if #'IfStatement]
    [:for #'ForStatement]]])

(def Declaration
  [:schema {:registry {::mdeclaration-function #'FunDeclaration
                       ::mdeclaration-variable #'VarDeclaration}}
   [:multi {:dispatch :declaration-type}
    [:function #'FunDeclaration]
    [:variable #'VarDeclaration]]])

(def BlockItem
  [:schema {:registry {::mblockitem-statement #'Statement
                       ::mblockitem-declaration #'Declaration}}
   [:multi {:dispatch :type}
    [:statement [:ref #'Statement]]
    [:declaration [:ref #'Declaration]]]])

(def Block
  [:schema {:registry {::mblock-blockitem #'BlockItem}}
   [:vector [:ref #'BlockItem]]])

(def Program
  [:schema {:registry {::mprogram-block #'Block}}
   [:vector [:ref #'Declaration]]])

(def FunAttribute
  [:map
   [:type [:= :fun]]
   [:defined? boolean?]
   [:global? boolean?]])

(def LocalAttribute
  [:map
   [:type [:= :local]]])

(def NoInitializer
  [:map
   [:type [:= :no-initializer]]])

(def Tentative
  [:map
   [:type [:= :tentative]]])

(def IntInit
  [:map
   [:type [:= :int-init]]
   [:value int?]])

(def LongInit
  [:map
   [:type [:= :long-init]]
   [:value int?]])

(def Initial
  [:map
   [:type [:= :initial]]
   [:static-init [:or IntInit LongInit]]])

(def InitialValue
  [:or
   NoInitializer
   Tentative
   Initial])

(def StaticAttribute
  [:map
   [:type [:= :static]]
   [:global? boolean?]
   [:initial-value #'InitialValue]])

(def Attribute
  [:multi {:dispatch :type}
   [:fun #'FunAttribute]
   [:static #'StaticAttribute]
   [:local #'LocalAttribute]])

(def Symbol
  [:map
   [:type #'Type]
   [:attribute #'Attribute]])

(def SymbolMap
  [:map-of string? #'Symbol])

(def TypecheckedOut
  [:map
   [:ident->symbol #'SymbolMap]
   [:program #'Program]])

(def TackyVar
  [:map
   [:type [:= :variable]]
   [:value string?]])

(def TackyConstant
  [:map
   [:type [:= :constant]]
   [:value #'Const]])

(def TackyVal
  [:schema {:registry {::mtacky-var #'TackyVar
                       ::mtacky-constant #'TackyConstant}}
   [:multi {:dispatch :type}
    [:variable #'TackyVar]
    [:constant #'TackyConstant]]])

(def TackyReturn
  [:map
   [:type [:= :return]]
   [:val #'TackyVal]])

(def TackySignExtend
  [:map
   [:type [:= :sign-extend]]
   [:src #'TackyVal]
   [:dst #'TackyVal]])

(def TackyTruncate
  [:map
   [:type [:= :truncate]]
   [:src #'TackyVal]
   [:dst #'TackyVal]])

(def TackyUnary
  [:map
   [:type [:= :unary]]
   [:unary-operator `[:enum ~@t/tacky-unary-ops]]
   [:src #'TackyVal]
   [:dst #'TackyVal]])

(def TackyBinary
  [:map
   [:type [:= :binary]]
   [:binary-operator `[:enum ~@t/tacky-binary-ops]]
   [:src1 #'TackyVal]
   [:src2 #'TackyVal]
   [:dst #'TackyVal]])

(def TackyCopy
  [:map
   [:type [:= :copy]]
   [:src #'TackyVal]
   [:dst #'TackyVal]])

(def TackyJump
  [:map
   [:type [:= :jump]]
   [:identifier string?]])

(def TackyJumpIfZero
  [:map
   [:type [:= :jump-if-zero]]
   [:val #'TackyVal]
   [:identifier string?]])

(def TackyJumpIfNotZero
  [:map
   [:type [:= :jump-if-not-zero]]
   [:val #'TackyVal]
   [:identifier string?]])

(def TackyLabel
  [:map
   [:type [:= :label]]
   [:identifier string?]])

(def TackyFunCall
  [:map
   [:type [:= :fun-call]]
   [:identifier string?]
   [:arguments [:vector #'TackyVal]]
   [:dst #'TackyVal]])

(def TackyInstruction
  [:multi {:dispatch :type}
   [:return #'TackyReturn]
   [:sign-extend #'TackySignExtend]
   [:truncate #'TackyTruncate]
   [:unary #'TackyUnary]
   [:binary #'TackyBinary]
   [:copy #'TackyCopy]
   [:jump #'TackyJump]
   [:jump-if-zero #'TackyJumpIfZero]
   [:jump-if-not-zero #'TackyJumpIfNotZero]
   [:label #'TackyLabel]
   [:fun-call #'TackyFunCall]])

(def TackyFunction
  [:map
   [:identifier string?]
   [:global? boolean?]
   [:type [:= :declaration]]
   [:declaration-type [:= :function]]
   [:parameters [:vector string?]]
   [:instructions [:vector #'TackyInstruction]]])

(def TackyStaticVariable
  [:map
   [:identifier string?]
   [:global? boolean?]
   [:variable-type #'Type]
   [:initial #'Initial]
   [:declaration-type [:= :static-variable]]
   [:type [:= :declaration]]])

(def TackyTopLevel
  [:multi {:dispatch :declaration-type}
   [:static-variable #'TackyStaticVariable]
   [:function #'TackyFunction]])

(def TackyProgram
  [:vector #'TackyTopLevel])
