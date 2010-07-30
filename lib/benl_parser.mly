/**************************************************************************/
/*  Copyright © 2009 Stéphane Glondu <steph@glondu.net>                   */
/*                                                                        */
/*  This program is free software: you can redistribute it and/or modify  */
/*  it under the terms of the GNU Affero General Public License as        */
/*  published by the Free Software Foundation, either version 3 of the    */
/*  License, or (at your option) any later version, with the additional   */
/*  exemption that compiling, linking, and/or using OpenSSL is allowed.   */
/*                                                                        */
/*  This program is distributed in the hope that it will be useful, but   */
/*  WITHOUT ANY WARRANTY; without even the implied warranty of            */
/*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     */
/*  Affero General Public License for more details.                       */
/*                                                                        */
/*  You should have received a copy of the GNU Affero General Public      */
/*  License along with this program.  If not, see                         */
/*  <http://www.gnu.org/licenses/>.                                       */
/**************************************************************************/

%{
  open Benl_types
%}

%token <Benl_types.field> FIELD
%token <Benl_types.regexp> REGEXP
%token MATCH OR AND NOT LPAREN RPAREN EOF SOURCE
%token LBRACKET RBRACKET SEMICOLON
%token <string> STRING IDENT
%token LE LT GT GE EQ DEPMATCH

%left OR
%left AND
%nonassoc NOT

%start <Benl_types.expr> full_expr
%start <Benl_types.config> config_file

%%

full_expr:
| e = expr EOF { e }

expr:
| e1 = expr OR e2 = expr { EOr (e1, e2) }
| e1 = expr AND e2 = expr { EAnd (e1, e2) }
| c = comparison x = STRING { EVersion (c, x) }
| NOT e = expr { ENot e }
| n = FIELD MATCH v = REGEXP { EMatch (n, v) }
| f = FIELD DEPMATCH LPAREN p = STRING c = comparison v = STRING RPAREN { EDep (f, p, Some (c, v)) }
| f = FIELD DEPMATCH LPAREN p = STRING RPAREN { EDep (f, p, None) }
| LPAREN e = expr RPAREN { e }
| SOURCE { ESource }
| LBRACKET xs = separated_list(SEMICOLON, expr) RBRACKET { EList xs }
| x = STRING { EString x }

comparison:
| LE { Le }
| LT { Lt }
| EQ { Eq }
| GE { Ge }
| GT { Gt }

config_item:
| i = IDENT EQ e = expr SEMICOLON { (i, e) }

config_file:
| xs = list(config_item) EOF { xs }
