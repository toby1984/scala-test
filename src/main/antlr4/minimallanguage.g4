grammar minimallanguage;

@header {
    package de.codesourcery.simplevm.grammar;
}

init : globalScope* ;

globalScope : varDef | valDef | functionDefinition | functionDeclaration;

varDef : 'var' IDENT '=' expr EOL;

valDef : 'val' IDENT '=' expr EOL;

expr:  expr '*' expr
       | expr '/' expr
       | expr '+' expr
       | expr '-' expr
       | value
       | '(' expr ')';

functionDeclaration: 'def' IDENT parameterList returnType EOL;

functionDefinition: 'def' IDENT parameterList returnType block EOL;

returnType : ':' typeName;

block :  '{' EOL* functionStatments* EOL* '}';

functionStatments : varDef | valDef;

parameterList : '(' ')' | '(' paramList ')';

paramList : parameterDecl | parameterDecl ',' parameterDecl;

parameterDecl : IDENT ':' typeName;
 
functionInvocation : IDENT argumentList;

typeName : simpleTypeName | arrayTypeName;

simpleTypeName : IDENT;

arrayTypeName : IDENT '[' ']';
       
argumentList : '(' ')' | '(' exprList ')';

exprList: ( expr | expr ',' expr );
               
value : NUMBER | STRING | IDENT | functionInvocation;

STRING : '"' ( '\\"' | . )*? '"' ;

IDENT: [a-zA-Z]+[_a-zA-Z0-9]* ;

NUMBER : [0-9]+ ;

EOL: '\r'?'\n';

WS : [ \t]+ -> skip;