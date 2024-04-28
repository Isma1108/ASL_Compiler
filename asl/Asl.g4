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
// Remeber that functions only return basic types following ASL description
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

type     
        : basic_type                                    #basicType
        | ARRAY LBRACK INTVAL RBRACK OF basic_type      #arrayType
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
          // Write an expression
        | WRITE expr ';'                                        # writeExpr
          // Write a string
        | WRITE STRING ';'                                      # writeString
          // Return an expression
        | RETURN (expr)? ';'                                    # returnStmt
        ;

// Grammar for left expressions (l-values in C++)
left_expr
        : ident                       # leftExprIdent
        | ident LBRACK expr RBRACK    # leftExprArray
        ;

function_call
        : ident LPAR exprs_call? RPAR 
        ;      

exprs_call
        : expr (COMMA expr)*
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
        | function_call                                     # functionValue
        | ident LBRACK expr RBRACK                          # arrayValue
        | ident                                             # exprIdent
        ;

// Identifiers
ident   : ID
        ;

//////////////////////////////////////////////////
/// Lexer Rules
//////////////////////////////////////////////////

//Assign
ASSIGN    : '=';

//Relational operators
EQUAL     : '==';
NEQUAL    : '!=';
GT        : '>';
GE        : '>=';
LT        : '<';
LE        : '<=';

//Arithmetic operators
PLUS      : '+';
MINUS     : '-';
MUL       : '*';
DIV       : '/';
MOD       : '%';

//Logical operators
NOT       : 'not';  //Unary operator
AND       : 'and';
OR        : 'or';

//Variable
VAR       : 'var';

//Basic types
INT       : 'int';
FLOAT     : 'float';
BOOL      : 'bool';
CHAR      : 'char';

//Array type
ARRAY     : 'array';
OF        : 'of';


//Some helpful  tokens
LPAR      : '(';
RPAR      : ')';
LBRACK    : '[';
RBRACK    : ']';
COMMA     : ',';

//CONTROL STRUCTURES

//Alternative composition
IF        : 'if';
THEN      : 'then';
ELSE      : 'else';
ENDIF     : 'endif';

//Iterative statements
WHILE     : 'while';
DO        : 'do';
ENDWHILE  : 'endwhile';

//Functions
FUNC      : 'func';
RETURN    : 'return';
ENDFUNC   : 'endfunc';

//Input/Output
READ      : 'read';
WRITE     : 'write';

//Primitive Values
BOOLVAL   : 'true' | 'false'; //It musk go before ID token to be recognized
INTVAL    : DIGIT+ ;
FLOATVAL  : DIGIT+ '.' DIGIT+;
CHARVAL   : SINGLE_QUOTA (DIGIT | LETTER | '\\n' | '\\t') SINGLE_QUOTA;

//Identifier (minimum priority)
ID        : LETTER (LETTER | '_' | DIGIT)* ;


// Strings (in quotes) with escape sequences
STRING    : '"' ( ESC_SEQ | ~('\\'|'"') )* '"' ;


// A fragment is a set of defined tokens that can be used to build more complex
// lexer rules. Fragments cannot be directly referenced in grammar rules. They are
// simply tools used internally by the lexer to construct larger and more complex tokens.

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
