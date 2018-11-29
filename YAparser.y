/*
 *      - - PARSER FOR PYTHON TO C COMPILER - -
 */


/*
 * Declaration section
 */

%error-verbose

%{
    #include <stdio.h>
    #include <stdlib.h>
    #include <string.h>

    #include "sym_ast_helper.h" 
    #include "stack.h"
    
    #define bufSize 1024
    int mergefile(void);
    int writeSymbol(void);
    int type_check(struct expr* expr1, struct expr* expr2);
    extern int yyerror(char const* s);
    int yylex();
    struct symbol symtab[NHASH];
    struct symbol symtab_function[NHASH];
    struct stmt* parser_result = 0;
    extern FILE *fp1, *fp2, *fp3, *fp4;
%}

%union{
    struct decl* dec;
    struct stmt* stat;
    struct expr* exp;
    struct symbol* s;

    char* str_literal;
    int int_literal;
    float float_literal;

}

/* Declaration of tokens */
%token <s> NAME
%token <str_literal> STRING
%token <int_literal> INTEGER 
%token <float_literal> FLOAT
%token END_STATEMENT  INDENT L_BRACKET R_BRACKET TOKEN_COLON TOKEN_COMMA
%token DEFFN ELIF ELSE IF WHILE PRINT INPUT

%nonassoc IFX
%nonassoc ELSE
%nonassoc DEDENT
%left GT LT NE EQ GE LE
%right TOKEN_ASSIGN
%left TOKEN_PLUS TOKEN_MINUS
%left TOKEN_MUL TOKEN_DIV
%nonassoc UMINUS

%type <exp> expression term factor condExpression
%type <stat> statement statements codeBlock selectionStatement iterationStatement elifStatement printStatement inputStatement program function functionCall
%type <dec> functionDefinition


%%


program:
              function                        {fprintf(fp1,"return 0;\n");fclose(fp1);fclose(fp3);fclose(fp4);writeSymbol();mergefile();exit(0);}
            ;

function:   
              function statement              {eval_stmt($2); stmtFree($2);}
            | /* NULL */                      {;}
            ;

statements:
              statements statement            {$$ = stmt_create(STMT_BLOCK, NULL, NULL, NULL,
                                                 NULL, $1, NULL, $2);}
            | statement                       {$$ = $1;}
            ;

statement:    
              expression                      {$$ = stmt_create(STMT_EXPR, NULL, NULL, $1,
                                                 NULL, NULL, NULL, NULL);}
            | selectionStatement              {$$ = $1;}
            | iterationStatement              {$$ = $1;}
            | printStatement                  {$$ = $1;}
            | inputStatement                  {$$ = $1;}
            | END_STATEMENT                   {$$ = stmt_create(STMT_END, NULL, NULL, NULL,
                                                 NULL, NULL, NULL, NULL);}
            | functionDefinition              {$$ = stmt_create(STMT_DECL, $1, NULL, NULL,
                                                NULL, NULL, NULL, NULL);}
            | functionCall                    {$$ = $1;}
            ;

codeBlock:
             INDENT statements DEDENT         {$$ = stmt_create(STMT_BLOCK, NULL, NULL, NULL,
                                                 NULL, $2, NULL, NULL);}
            ;

expression:   
              expression TOKEN_PLUS term      {if(type_check($1,$3)==1) 
                                                $$ = expr_create(EXPR_ADD, $1, $3,$1->type);}
            | expression TOKEN_MINUS term     {if(type_check($1,$3)==1) 
                                                $$ = expr_create(EXPR_SUB, $1, $3,$1->type);}
            | term                            {$$ = $1;}
            | NAME TOKEN_ASSIGN expression    {$$ = expr_create(EXPR_ASSIGN, expr_create_name(lookup($1->name),$3->type, NULL), $3, $3->type);}       
            ;

term:         
              term TOKEN_MUL factor           {if(type_check($1,$3)==1) 
                                                $$ = expr_create(EXPR_MUL, $1, $3,$1->type);}
            | term TOKEN_DIV factor           {if(type_check($1,$3)==1) 
                                                $$ = expr_create(EXPR_DIV, $1, $3,$1->type);}
            | factor                          {$$ = $1;}
            ;

factor:       
              TOKEN_MINUS factor %prec UMINUS {$$ = expr_create(EXPR_SUB, NULL, $2, $2->type);}
            | L_BRACKET expression R_BRACKET  {$$ = expr_create(EXPR_BRACKET, $2, NULL, $2->type);}
            | INTEGER                         {$$ = expr_create_integer($1);}
            | FLOAT                           {$$ = expr_create_float($1);}
            | NAME                            {$$ = expr_create_name(lookup($1->name), $1->type, NULL);}
            ;
            
condExpression:
              expression GT expression        {if(type_check($1,$3)==1) 
                                                $$ = expr_create(EXPR_GT, $1, $3,$1->type);}
            | expression LT expression        {if(type_check($1,$3)==1) 
                                                $$ = expr_create(EXPR_LT, $1, $3,$1->type);}
            | expression GE expression        {if(type_check($1,$3)==1) 
                                                $$ = expr_create(EXPR_GE, $1, $3,$1->type);}
            | expression LE expression        {if(type_check($1,$3)==1) 
                                                $$ = expr_create(EXPR_LE, $1, $3,$1->type);}
            | expression EQ expression        {if(type_check($1,$3)==1) 
                                                $$ = expr_create(EXPR_EQ, $1, $3,$1->type);}
            | expression NE expression        {if(type_check($1,$3)==1) 
                                                $$ = expr_create(EXPR_NE, $1, $3,$1->type);}
            ;

selectionStatement:
              IF L_BRACKET condExpression R_BRACKET TOKEN_COLON codeBlock %prec IFX
                                              {$$ = stmt_create(STMT_IF_ELSE, NULL, NULL, $3,
                                                NULL, $6, NULL, NULL);}
            | IF L_BRACKET condExpression R_BRACKET TOKEN_COLON codeBlock ELSE TOKEN_COLON codeBlock
                                              {$$ = stmt_create(STMT_IF_ELSE, NULL, NULL, $3,
                                                NULL, $6, $9, NULL);}
            | IF L_BRACKET condExpression R_BRACKET TOKEN_COLON codeBlock elifStatement
                                              {$$ = stmt_create(STMT_IF_ELSE, NULL,NULL, $3,
                                                NULL, $6, NULL, $7);}
            ;

elifStatement:
              ELIF L_BRACKET condExpression R_BRACKET TOKEN_COLON codeBlock ELSE TOKEN_COLON codeBlock
                                              {$$ = stmt_create(STMT_IF_ELIF, NULL, NULL, $3,
                                                NULL, $6, $9, NULL);}
            | ELIF L_BRACKET condExpression R_BRACKET TOKEN_COLON codeBlock elifStatement
                                              {$$ = stmt_create(STMT_IF_ELIF, NULL, NULL, $3,
                                                NULL, $6, NULL, $7);}
            ;
iterationStatement:
              WHILE L_BRACKET condExpression R_BRACKET TOKEN_COLON codeBlock 
                                              {$$ = stmt_create(STMT_WHILE, NULL, NULL, $3,
                                                NULL, $6, NULL, NULL);}
            ;

printStatement:
              PRINT STRING                    {$$ = stmt_create(STMT_PRINT, NULL, NULL, expr_create_string($2),
                                                NULL, NULL, NULL, NULL);}                
            | PRINT NAME                      {$$ = stmt_create(STMT_PRINT, NULL,  NULL, expr_create_name($2, $2->type, NULL),
                                                NULL, NULL, NULL, NULL);}
            ;
inputStatement:
              NAME TOKEN_ASSIGN INPUT L_BRACKET STRING R_BRACKET    {$$ = stmt_create(STMT_INPUT, NULL, expr_create_name($1, SYM_STRING, NULL), expr_create_string($5),
                                                NULL, NULL, NULL, NULL);}
            | NAME TOKEN_ASSIGN INPUT L_BRACKET  R_BRACKET    {$$ = stmt_create(STMT_INPUT, NULL, expr_create_name($1, SYM_STRING, NULL), NULL,
                                                NULL, NULL, NULL, NULL);}
            ;
functionDefinition:
            DEFFN NAME L_BRACKET R_BRACKET TOKEN_COLON codeBlock  {$$ = decl_create($2, expr_create_name($2,SYM_VOID,$6), $6);}
            ;
functionCall:
             NAME L_BRACKET R_BRACKET        {if(($1->func))
                                                    $$ = stmt_create(STMT_CALL, NULL, expr_create_name($1,SYM_VOID,$1->func), NULL,
                                                                        NULL, NULL, NULL, NULL);
                                                else{
                                                    $$ = stmt_create(STMT_CALL, NULL, NULL, NULL,
                                                                        NULL, NULL, NULL, NULL);
                                                    yyerror("Function has no type.\n");}
                                            }
            ;
                                                            


%%

/*
 * Subroutines Section
 */

/* SYMBOL TABLE */
/* hash a symbol */

static unsigned symHash(char* sym){
    unsigned int hash = 0;
    unsigned c;

    while((c = *sym++))
        hash = hash*9 ^ c;
    
    return hash;
}

struct symbol* lookup(char* sym){
    struct symbol *sp = &symtab[symHash(sym)%NHASH];
    int scount = NHASH;             /* How many have we looked at */

    while((--scount >= 0)){
        if(sp->name && !strcmp(sp->name, sym))
            return sp;
        
        if(!sp->name){      /* new entry */
            sp->name = strdup(sym);
            sp->type = SYM_ND;
            sp->var_of_function = 0;
            sp->value = 0;
            sp->func = NULL;
            return sp;
        }

        if(++sp >= symtab+NHASH) 
            sp = symtab;        /* try the next entry */
    }
    yyerror("Symbol table overflow\n");
    abort();        /* tried them all, table is full */
}

struct symbol* lookup_function(struct symbol* sym){
    struct symbol *sp = &symtab_function[symHash(sym->name)%NHASH];
    int scount = NHASH;             /* How many have we looked at */

    while((--scount >= 0)){
        if(sp->name && !strcmp(sp->name, sym->name))
            return sp;
        
        if(!sp->name){      /* new entry */
            sp->name = strdup(sym->name);
            sp->type = sym->type;
            sp->var_of_function = 1;
            sp->value = 0;
            sp->func = NULL;
            return sp;
        }

        if(++sp >= symtab_function+NHASH) 
            sp = symtab_function;        /* try the next entry */
    }
    yyerror("Symbol table overflow\n");
    abort();        /* tried them all, table is full */
}

sym_t get_sym_type(struct expr* e){
    if(!e)
        return 0;
    switch(e->kind){
        case EXPR_INTEGER:
            return SYM_INT;
            break;
        case EXPR_FLOAT:
            return SYM_FLOAT;
            break;
        default:
            return SYM_ND;
            break;
    }
}


/* ABSTRACT SYNTAX TREE */

/*
 * DECLARATIONS 
 */
struct decl* decl_create(struct symbol* name, struct expr* value, struct stmt* code){
    struct decl* d = calloc(3, sizeof(struct decl));

    if(!d){
        printf("Out of space");
        exit(1);
    }

    d->name = name;
    d->value = value;
    d->code = code;

    return d;

}

/* 
 * STATEMENTS 
 */
struct stmt* stmt_create(stmt_t kind,
     struct decl *decl,
     struct expr *init_expr,
     struct expr *expr,
     struct expr *next_expr,
     struct stmt *body,
     struct stmt *else_body,
     struct stmt *next){
    
    struct stmt* s = calloc(8, sizeof(struct stmt));

    if(!s){
        printf("Out of space");
        exit(1);
    }
    s->kind = kind;
    s->decl = decl;
    s->init_expr = init_expr;
    s->expr = expr;
    s->next_expr = next_expr;
    s->body = body;
    s->else_body = else_body;
    s->next = next;

    return s;

}

/*
 * EXPRESSIONS 
 */

/* Binary operators */
struct expr* expr_create(expr_t kind,
                        struct expr* L, struct expr* R, sym_t type){
    
    struct expr* e = calloc(4, sizeof(struct expr));

    if(!e){
        printf("Out of space");
        exit(1);
    }

    e->kind = kind;
    e->left = L;
    e->right = R;
    e->type = type;
    return e;
}

/* Leaf types */ 
struct expr* expr_create_name(struct symbol* s, sym_t type, struct stmt* func){
    
    struct expr* e = calloc(4, sizeof(struct expr));

    if(!e){
        printf("Out of space");
        exit(1);
    }

    e->kind = EXPR_NAME;
    e->s = s;
    e->s->type = type;
    e->s->func = func;
    e->type = type;
    return e;
}

struct expr* expr_create_integer(int i){
    struct expr* e = calloc(3, sizeof(struct expr));

    if(!e){
        printf("Out of space");
        exit(1);
    }

    e->kind = EXPR_INTEGER;
    e->integer_value = i;
    e->type = SYM_INT;
    return e;

}

struct expr* expr_create_float(float f){
    struct expr* e = calloc(3, sizeof(struct expr));

    if(!e){
        printf("Out of space");
        exit(1);
    }

    e->kind = EXPR_FLOAT;
    e->float_value = f;
    e->type = SYM_FLOAT;
    return e;

}

struct expr* expr_create_string(const char* str){
    struct expr* e = calloc(3, sizeof(struct expr));

    if(!e){
        printf("Out of space");
        exit(1);
    }

    e->kind = EXPR_STRING;
    e->string = strdup(str);
    e->type = SYM_STRING;
    return e;

}

/* MERGING FILE TO PRODUCE OUTPUT OF COMPILER */
int mergefile(void)
{
  FILE* fp1, *fp2;
  char buf[bufSize];
  int line = 0;
  fp2 = fopen("code.c","a");
  fprintf(fp2,"#include <stdio.h>\n");
  fprintf(fp2,"#include <stdlib.h>\n\n\n");
  
  if ((fp1 = fopen("funcpart.txt", "r")) == NULL)
  { /* Open source file. */
    perror("fopen source-file");
    return 1;
  }
  while (fgets(buf, sizeof(buf), fp1) != NULL)
  {
    buf[strlen(buf) - 1] = '\0'; // eat the newline fgets() stores
    fprintf(fp2,"%s\n", buf);
    if(line==0){
        if ((fp4 = fopen("declpart_function.txt", "r")) == NULL)
        { /* Open source file. */
            perror("fopen source-file");
            return 1;
        }
        while (fgets(buf, sizeof(buf), fp4) != NULL)
        {
            buf[strlen(buf) - 1] = '\0'; // eat the newline fgets() stores
            fprintf(fp2,"%s\n", buf);
        } 
    }
    line++;
  }
  
  fprintf(fp2,"int main(void){\n\n");
  fprintf(fp2,"\\* DECLARATION PART *\\\n");
  if ((fp1 = fopen("declpart.txt", "r")) == NULL)
  { /* Open source file. */
    perror("fopen source-file");
    return 1;
  }
  while (fgets(buf, sizeof(buf), fp1) != NULL)
  {
    buf[strlen(buf) - 1] = '\0'; // eat the newline fgets() stores
    fprintf(fp2,"%s\n", buf);
  }
  fprintf(fp2,"\n\n\\* CODE PART *\\\n");
  if ((fp1 = fopen("codepart.txt", "r")) == NULL)
  { /* Open source file. */
    perror("fopen source-file");
    return 1;
  }
  while (fgets(buf, sizeof(buf), fp1) != NULL)
  {
    buf[strlen(buf) - 1] = '\0'; // eat the newline fgets() stores
    fprintf(fp2,"%s\n", buf);
  }
  fprintf(fp2,"}\n");
  fclose(fp1);
  fclose(fp2);
  fclose(fp4);
  return 0;
}

int writeSymbol(){
    int i;
    char *errorMsg = "";
    char *temp;
    fp2 = fopen("declpart.txt","a");
            
    for(i=0;i<NHASH;i++){
        if(symtab[i].name!=NULL && symtab[i].var_of_function==0){
            switch(symtab[i].type){
                case SYM_ND:
                    temp = strdup(symtab[i].name);
                    errorMsg = strcat(temp," must be declared.");
                    yyerror(errorMsg);
                    exit(1);
                    break;
                case SYM_VOID:
                    break;
                case SYM_INT:
                    fprintf(fp2,"int ");
                    fprintf(fp2,"%s;\n",symtab[i].name);
                    break;
                case SYM_FLOAT:
                    fprintf(fp2,"float ");
                    fprintf(fp2,"%s;\n",symtab[i].name);
                    break;
                case SYM_STRING:
                    fprintf(fp2,"char* ");
                    fprintf(fp2,"%s;\n",symtab[i].name);
                    break;
            }
        }
    }
    fclose(fp2);
    return 0;
}

int type_check(struct expr* expr1, struct expr* expr2){
    char *errorMsg = "";
    char *temp;
    if((expr1->type)!=SYM_ND && (expr2->type)!=SYM_ND){
        if((expr1->type)==(expr2->type))
            return 1;
        else{
            yyerror("Invalid expression: the terms of the expression have different types.");
            return 0;
        }
    }else if((expr1->type)==SYM_ND && (expr2->type)!=SYM_ND){
            temp = strdup(expr1->s->name);
            errorMsg = strcat(temp," has no type.");
            yyerror(errorMsg);
            return 0;
    }else if((expr1->type)!=SYM_ND && (expr2->type)==SYM_ND){
            temp = strdup(expr2->s->name);
            errorMsg = strcat(temp," has no type.");
            yyerror(errorMsg);
            return 0;
    }else   
        return 0;
}

int yyerror(char const *s) {
    fprintf(stderr, "<Line %d> %s\n", yylineno, s);
    return 0;
}



int main(int argc, char **argv){
    initStack(&indent, 9999);
    fp1 = fopen("codepart.txt","ab+");
    fp3 = fopen("funcpart.txt","ab+");
    fp4 = fopen("declpart_function.txt","ab+");
    if(yyparse()==0){
        free(indent.data);
        printf("Parse successful!\n");
    } else {
        free(indent.data);
        printf("Parse failed. \n");
    }

        
    return 0;
}


