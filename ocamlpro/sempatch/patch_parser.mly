/***********************************************************************/
/*                                                                     */
/*                              TypeRex                                */
/*                                                                     */
/*  Copyright 2014, Fabrice Le Fessant, INRIA/OCamlPro.                */
/*                    All rights reserved.                             */
/*  All rights reserved.  This file is distributed under the terms     */
/*  of the Q Public License version 1.0.                               */
/*                                                                     */
/***********************************************************************/

/* The parser definition */

%{

(* The new syntax for .ocpp files:

patch add_method_arguments {
  arguments = [ "loc" ]
  methods = [];
}

*)


open Patch_types
%}

%token EOF
%token <string>STRING
%token <string> IDENT
%token PATCH
%token LBRACE
%token RBRACE
%token EQUAL
%token LBRACKET
%token RBRACKET
%token INCLUDE
%token SEMI
%token SHARP
%token <int>INT

%start main
%type <Patch_types.ast> main

%%
main:
 | patches EOF { $1 }
  ;

patches:
 | SHARP INT STRING patches { $4 }
 | patch patches { $1 :: $2 }
 |               { [] }
;

patch:
 | INCLUDE STRING { ASTInclude $2 }
 | PATCH IDENT LBRACE parameter_list RBRACE {
   ASTPatch($2, $4)
 }
;
parameter_list:
 | IDENT EQUAL value opt_semi parameter_list { ($1,$3) :: $5 }
 | { [] }
;

opt_semi:
 | SEMI { () }
 |      { () }
;

value:
 | STRING { ASTString $1 }
 | LBRACKET string_list RBRACKET { ASTList $2 }
;

string_list:
 | STRING opt_semi string_list { $1 :: $3 }
 |                             { [] }
;
