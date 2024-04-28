//////////////////////////////////////////////////////////////////////
//
//    Asl - Another simple language (grammar)
//
//    Copyright (C) 2020-2030  Universitat Politecnica de Catalunya
//
//    This library is free software; you can redistribute it and/or
//    modify it under the terms of the GNU General Public License
//    as published by the Free Software Foundation; either version 3
//    of the License, or (at your option) any later version.
//
//    This library is distributed in the hope that it will be useful,
//    but WITHOUT ANY WARRANTY; without even the implied warranty of
//    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
//    Affero General Public License for more details.
//
//    You should have received a copy of the GNU Affero General Public
//    License along with this library; if not, write to the Free Software
//    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
//
//    contact: José Miguel Rivero (rivero@cs.upc.edu)
//             Computer Science Department
//             Universitat Politecnica de Catalunya
//             despatx Omega.110 - Campus Nord UPC
//             08034 Barcelona.  SPAIN
//
//////////////////////////////////////////////////////////////////////

grammar Asl;

//////////////////////////////////////////////////
/// Parser Rules
//////////////////////////////////////////////////

// A program is a list of functions
program : function+ EOF
        ;

// A function has a name, a list of parameters and a list of statements
function
        : FUNC ID LPAR parameters RPAR (':' basic_type)? declarations statements ENDFUNC
        ;

parameters
        : (parameter_decl (COMMA parameter_decl)*)?
        ;

parameter_decl
        : ID ':' type
        ;

declarations
        : (variable_decl)*
        ;

variable_decl
        : VAR ID (COMMA ID)* ':' type
        ;

type    : basic_type                                    # basicType
        | ARRAY LCOR INTVAL RCOR OF basic_type          # arrayType
        ;

basic_type
        : INT
        | FLOAT
        | BOOL
        | CHAR
        ;

statements
        : (statement)*
        ;

// The different types of instructions
statement
          // Assignment
        : left_expr ASSIGN expr ';'                             # assignStmt
          // if-then-else statement (else is optional)
        | IF expr THEN statements (ELSE statements)? ENDIF      # ifStmt
          // while-do-endwhile statement
        | WHILE expr DO statements ENDWHILE                     # whileStmt
          // A function/procedure call has a list of arguments in parenthesis (possibly empty)
        | function_call ';'                                     # procCall
          // Read a variable
        | READ left_expr ';'                                    # readStmt
          // Write a char
        | WRITE CHARVAL ';'                                     # writeChar
          // Write a string
        | WRITE STRING ';'                                      # writeString
          // Write an expression
        | WRITE expr ';'                                        # writeExpr
          // Return an expression
        | RETURN (expr)? ';'                                    # returnStmt
        ;

// Grammar for left expressions (l-values in C++)
left_expr
        : ident                         # leftExprIdent
        | ident LCOR expr RCOR          # leftExprArray
        ;

// Grammar for function calls
function_call
        : ID LPAR (expr (COMMA expr)*)? RPAR
        ;

// Grammar for expressions with boolean, relational and aritmetic operators
expr    : '(' expr ')'                                      # parenthesis 
        | op=NOT expr                                       # binaryOperationUnary
        | op=(PLUS|MINUS) expr                              # arithmeticUnary
        | expr op=(MUL|DIV|MOD) expr                        # arithmetic
        | expr op=(PLUS|MINUS) expr                         # arithmetic
        | expr op=(EQUAL|NEQUAL|GT|GE|LT|LE) expr           # relational
        | expr op=AND expr                                  # binaryOperation
        | expr op=OR expr                                   # binaryOperation
        | (INTVAL|FLOATVAL|CHARVAL|BOOLVAL)                 # value
        | left_expr                                         # leftExprValue
        | function_call                                     # funcCallExpr
        ;

// Identifiers
ident   : ID
        ;

//////////////////////////////////////////////////
/// Lexer Rules
//////////////////////////////////////////////////

ASSIGN    : '=';
EQUAL     : '==';
NEQUAL    : '!=';
GT        : '>';
GE        : '>=';
LT        : '<';
LE        : '<=';
PLUS      : '+';
MINUS     : '-';
MUL       : '*';
DIV       : '/';
MOD       : '%';
NOT       : 'not';
AND       : 'and';
OR        : 'or';
VAR       : 'var';
INT       : 'int';
FLOAT     : 'float';
BOOL      : 'bool';
CHAR      : 'char';
ARRAY     : 'array';
OF        : 'of';
LPAR      : '(';
RPAR      : ')';
LCOR      : '[';
RCOR      : ']';
COMMA     : ',';
IF        : 'if';
THEN      : 'then';
ELSE      : 'else';
ENDIF     : 'endif';
WHILE     : 'while';
DO        : 'do';
ENDWHILE  : 'endwhile';
FUNC      : 'func';
RETURN    : 'return';
ENDFUNC   : 'endfunc';
READ      : 'read';
WRITE     : 'write';
BOOLVAL   : 'true' | 'false';
INTVAL    : DIGIT+ ;
FLOATVAL  : DIGIT+ '.' DIGIT+;
CHARVAL   : SINGLE_QUOTA (DIGIT | LETTER | '\\n' | '\\t' | '.') SINGLE_QUOTA;
ID        : LETTER (LETTER | '_' | DIGIT)* ;


// Strings (in quotes) with escape sequences
STRING    : '"' ( ESC_SEQ | ~('\\'|'"') )* '"' ;

fragment
ESC_SEQ   : '\\' ('b'|'t'|'n'|'f'|'r'|'"'|'\''|'\\') ;

fragment 
LETTER    : 'a'..'z'|'A'..'Z';

fragment
DIGIT     : '0'..'9';

fragment
SINGLE_QUOTA : '\'';


// Comments (inline C++-style)
COMMENT   : '//' ~('\n'|'\r')* '\r'? '\n' -> skip ;

// White spaces
WS        : (' '|'\t'|'\r'|'\n')+ -> skip ;
// Alternative description
// WS        : [ \t\r\n]+ -> skip ;
