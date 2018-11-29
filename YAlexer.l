/*
 *      - - LEXER FOR PYTHON TO C COMPILER - -
 */


/*
 * Declaration section
 */
%option noyywrap  yylineno
%{
     /* TODO: add inclusion of libraries */
    #include <stdlib.h>
    #include <stdio.h>
    #include <assert.h>
    #include "stack.h"
    #include "sym_ast_helper.h"
    
    #include "YAparser.tab.h"        /* TODO: change the header file of the new parser */
    #define MAX_DEPTH 72
    struct symbol symtab[NHASH];
    STACK indent;    
    
    
%}
/* Identifier and keywords */
digit           [0-9]
letter          [a-zA-Z]

/* Integer literals */     
nonzerodigit    [1-9]
bininteger      0(b|B)(0|1)+
hexinteger      0(x|X)([0-9]|[a-fA-F])+
octinteger      0(o|O)[0-7]+|0[0-7]+
decimalinteger  [1-9][0-9]*|0

/* Floating point literals */
exp             (e|E)(\+|-)?[0-9]+
pointfloat      ([0-9]+)?(\.[0-9]+)|([0-9]+)\.
expfloat        ([0-9]+|([0-9]+)?(\.[0-9]+)|([0-9]+)\.)(e|E)(\+|-)?[0-9]+
 

%%
  /*
   * Rules section
   */

    /* String literals */
\'[^\n]*\'|\"[^\n]*\"                           {yylval.str_literal = yytext; return STRING;}
 /*\n                                              {return EOL;}*/
    /* Single character operators */
"+"                                             {return TOKEN_PLUS;}
"-"                                             {return TOKEN_MINUS;}
"*"                                             {return TOKEN_MUL;}
"/"                                             {return TOKEN_DIV;}
"="                                             {return TOKEN_ASSIGN;}
":"                                             {return TOKEN_COLON;}
","                                             {return TOKEN_COMMA;}
"("                                             {return L_BRACKET;}
")"                                             {return R_BRACKET;}
    /* Comparison operators */
">"                                             {return GT;}
"<"                                             {return LT;}
"!="                                            {return NE;}
"=="                                            {return EQ;}
">="                                            {return GE;}
"<="                                            {return LE;}

    /* Keywords - cannot be used as identifiers */
and                                             {printf("Reserved word: %s\n", yytext);}
as                                              {printf("Reserved word: %s\n", yytext);}
assert                                          {printf("Reserved word: %s\n", yytext);}
break                                           {printf("Reserved word: %s\n", yytext);}
class                                           {printf("Reserved word: %s\n", yytext);}
continue                                        {printf("Reserved word: %s\n", yytext);}
def                                             {return DEFFN;}
del                                             {printf("Reserved word: %s\n", yytext);}
elif                                            {return ELIF;}
else                                            {return ELSE;}
except                                          {printf("Reserved word: %s\n", yytext);}
exec                                            {printf("Reserved word: %s\n", yytext);}
finally                                         {printf("Reserved word: %s\n", yytext);}
for                                             {printf("Reserved word: %s\n", yytext);}
from                                            {printf("Reserved word: %s\n", yytext);}
global                                          {printf("Reserved word: %s\n", yytext);}
if                                              {return IF;}
import                                          {printf("Reserved word: %s\n", yytext);}
in                                              {printf("Reserved word: %s\n", yytext);}
is                                              {printf("Reserved word: %s\n", yytext);}
lambda                                          {printf("Reserved word: %s\n", yytext);}
not                                             {printf("Reserved word: %s\n", yytext);}
or                                              {printf("Reserved word: %s\n", yytext);}
pass                                            {printf("Reserved word: %s\n", yytext);}
print                                           {return PRINT;}
raise                                           {printf("Reserved word: %s\n", yytext);}
raw_input                                       {return INPUT;}
return                                          {printf("Reserved word: %s\n", yytext);}
try                                             {printf("Reserved word: %s\n", yytext);}
while                                           {return WHILE;}
with                                            {printf("Reserved word: %s\n", yytext);}
yeld                                            {printf("Reserved word: %s\n", yytext);}
 
    /* Identifiers */
({letter}|\_)({letter}|{digit}|\_)*           {yylval.s = lookup(yytext);return NAME;}
    /* Numeric literals */
{decimalinteger}|{octinteger}|{hexinteger}|{bininteger}          {yylval.int_literal = atoi(yytext); return INTEGER;}

{pointfloat}|{expfloat}                         {yylval.float_literal = atof(yytext); return FLOAT;}

    /* Indentations */
^\n                                           {/* ignore blank line */} 
\n[ \t]*                                      { if(yyleng == 1){
                                                if(indent.size > 0){
                                                    pop(&indent);
                                                    if((peek(&indent) > yyleng) && (indent.size != 0))
                                                        yyless(0);
                                                    return(DEDENT);
                                                }
                                                return(END_STATEMENT);
                                              } else {
                                                  if((peek(&indent) < yyleng) || (indent.size == 0)){
                                                      push(&indent, yyleng);
                                                      return(INDENT);
                                                  } else if((peek(&indent) > yyleng) && (indent.size != 0)){
                                                      pop(&indent);
                                                      if((peek(&indent) > yyleng) && (indent.size != 0))
                                                            yyless(0);
                                                      return(DEDENT);
                                                  } else{
                                                      return(END_STATEMENT);
                                                  }
                                              }

                                            }


    /* Comments and other that are ignored    */
#[^\n]*\n                                       {/* ignore comment */}

[ ]                                       {/* ignore white spaces on the same line*/}

<<EOF>>                                   { if(indent.size > 0){
                                                pop(&indent);
                                                return(DEDENT);}
                                            yyterminate(); 
                                            } 
%%


/*
 * Subroutines Section
 */



