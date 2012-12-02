grammar Cymbol;
options {
  output = AST;              // build trees
  ASTLabelType = CymbolAST;
}

tokens {
  METHOD_DECL; // function definition
  ARG_DECL;    // parameter
  BLOCK;
  MEMBERS;     // class body
  VAR_DECL;
  FIELD_DECL;
  CALL;
  ELIST;       // expression list
  EXPR; 	   // root of an expression
  ASSIGN='=';
  EXTENDS;
}

compilationUnit
    :   ( classDefinition | varDeclaration | methodDeclaration )+ EOF
    ;

// START: class
classDefinition
    :   'class' ID superClass? '{' classMember+ '}' ';'
        -> ^('class' ID superClass? ^(MEMBERS classMember+))
    ;
superClass
	:	':' 'public' ID -> ^(EXTENDS ID)
	;
// END: class

classMember
	:	type ID ('=' expression)? ';' -> ^(FIELD_DECL type ID expression?)
	|	methodDeclaration
	|	'public' ':' -> // throw away; just making input valid C++
	;
	
// START: method
methodDeclaration
    :   type ID '(' formalParameters? ')' block
        -> ^(METHOD_DECL type ID formalParameters? block)
    ;
// END: method

formalParameters
    :   type ID (',' type ID)* -> ^(ARG_DECL type ID)+
    ;

type:   'float'
    |   'int'
    |	'void'
    |	ID // class type name
    ;

// START: block
block
    :   '{' statement* '}' -> ^(BLOCK statement*)
    ;
// END: block

// START: var
varDeclaration
    :   type ID ('=' expression)? ';' -> ^(VAR_DECL type ID expression?)
    ;
// END: var

statement
    :   block
    |	varDeclaration
    |   'return' expression? ';' -> ^('return' expression?)
    |   postfixExpression // handles function calls like f(i);
        (   '=' expression -> ^('=' postfixExpression expression)
        |   -> ^(EXPR postfixExpression)
        )
        ';' 
    |	';' -> // empty statement      
    ;

expressionList
    :   expression (',' expression)* -> ^(ELIST expression+)
    |   -> ELIST
    ;

expression
    :   addExpression -> ^(EXPR addExpression)
    ;
    
addExpression
	:	postfixExpression ('+'^ postfixExpression)*
	;

// START: call
postfixExpression
    :   (primary->primary)
    	(	options {backtrack=true;}
		:	'.' ID '(' expressionList ')' -> ^(CALL ^('.' $postfixExpression ID))
		|	'.' ID						  -> ^('.' $postfixExpression ID)
		|	'(' expressionList ')'        -> ^(CALL $postfixExpression)
		)*
    ;
// END: call

suffix[CommonTree expr]
options {backtrack=true;}
	:	'.' ID '(' expressionList ')' -> ^(CALL ^('.' {$expr} ID))
	|	'.' ID						  -> ^('.' {$expr} ID)
	|	'(' expressionList ')'        -> ^(CALL {$expr})
	;
	
primary
    :   'this'
    |	'super'
    |	ID
    |   INT
    |   '(' expression ')' -> expression
    ;

// LEXER RULES

ID  :   LETTER (LETTER | '0'..'9')*
    ;

fragment
LETTER  :   ('a'..'z' | 'A'..'Z')
    ;

INT :   '0'..'9'+
    ;

WS  :   (' '|'\r'|'\t'|'\n') {$channel=HIDDEN;}
    ;

SL_COMMENT
    :   '//' ~('\r'|'\n')* '\r'? '\n' {$channel=HIDDEN;}
    ;
