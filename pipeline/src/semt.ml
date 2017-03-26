(* Semantic Tree *)

open Ast

module StringMap = Map.Make(String);;

type typed_id = typ * string

type sem_obj = (* lhs *)
    SId of string
  | SBrac of (* a[0] a[i] a[i+1] *)
    sem_obj * sem_expr *
    bool * (* indicate if it is insertion *) 
    typ
  | SBrac2 of sem_obj * sem_expr * sem_expr * typ (* a[0:2] *)
  | SAttr of (* a$b *)
    typ * (* type of the object (tuple or table of something) *)
    typ * (* type of the attribute *)
    sem_obj *
    string * (* name of the attribute *)
    int (* index of the attribute*)
  (* e.g. in a$b 1st typ is typ of a and 2nd is typ of attr b*)
  | SAttrInx of (* a$(int_expr) - TREAT AS STRING *)
    typ * (* type of the object (tuple or table of something) *)
    typ * (* type of the attribute (String or Array(String) ) *)
    sem_obj *
    sem_expr (* index expression *)
and
 sem_expr =
    SLiteral of int
  | SBoolLit of bool
  | SFloatLit of float
  | SStrLit of string
  | SObj of sem_obj
  | SBinop of typ * sem_expr * op * sem_expr(* as on lhs *)
  | SUnop of typ * uop * sem_expr
  | SAssign of typ * sem_obj * sem_expr
  | SCall of string * sem_expr list
  | STupInst of string * int (* tuple instantiation *)
  | STabInst of string (* table instantiation e.g. Foo[] *)
  | SArr of typ * sem_expr list (* arrays e.g. [1,2,3] *)
  | SNoexpr
  (* The elements below are internal use only *)
  (* SpecialCall is an unchecked call, used for hardcode only *)
  | SSpecialCall of typ * string * sem_expr list
  | SString of string (* For use inside codegen only *)
 

type sem_stmt =
    SBlock of sem_stmt list
  | SExpr of sem_expr
  | SReturn of sem_expr
  | SIf of sem_expr * sem_stmt * sem_stmt
  | SFor of string * typ * sem_expr * sem_stmt (* for i in a *)
  | SWhile of sem_expr * sem_stmt
  | SBreak
  | SContinue

type sem_func_decl = {
    rtyp: typ;
    semfname : string;
    originalname: string;
    semformals : typed_id list;
    semlocals : typ StringMap.t ref;
    sembody : sem_stmt list;
  }


type sem_program =
  typed_id list *
  sem_stmt list *
  sem_func_decl list * 
  tup list
