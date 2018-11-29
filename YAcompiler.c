#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "sym_ast_helper.h"
#include "YAparser.tab.h"

FILE *fp1, *fp2, *fp3, *fp4;
struct symbol symtab[NHASH];
int yyerror(char const* s);

void eval_stmt(struct stmt* stmt){
    if(!stmt)
        return;
    
    
    switch(stmt->kind){
        case STMT_DECL:
            if((stmt->decl)!=NULL)
                eval_declaration(stmt->decl);
            break;
        case STMT_CALL:
            if((stmt->init_expr))
                fprintf(fp1,"%s();\n",stmt->init_expr->s->name);
            break;
        case STMT_EXPR:
            if(!(stmt->expr->left) || !(stmt->expr->right)){
                yyerror("Invalid statement.");
                return;
            }
            eval_expression(stmt->expr);
            fprintf(fp1,";\n");
            break;
        case STMT_IF_ELSE:
            fprintf(fp1,"if (");
            eval_expression(stmt->expr);
            fprintf(fp1,")");
            fprintf(fp1,"{\n");
            eval_stmt(stmt->body);
            fprintf(fp1,"}");
            if((stmt->else_body)!=NULL){
                fprintf(fp1,"else{\n");
                eval_stmt(stmt->else_body);
                fprintf(fp1,"}\n");
                break; 
            }
            if((stmt->next)!=NULL)
                eval_stmt(stmt->next);

            break;
        case STMT_IF_ELIF:
            fprintf(fp1,"else if (");
            eval_expression(stmt->expr);
            fprintf(fp1,")");
            fprintf(fp1,"{\n");
            eval_stmt(stmt->body);
            fprintf(fp1,"}");
            if((stmt->else_body)!=NULL){
                fprintf(fp1,"else{\n");
                eval_stmt(stmt->else_body);
                fprintf(fp1,"}\n");
                break; 
            }
            while((stmt->next)!=NULL){
                stmt=stmt->next;
                eval_stmt(stmt);
                break;
            }
            break;
        case STMT_WHILE:
            fprintf(fp1,"while (");
            eval_expression(stmt->expr);
            fprintf(fp1,")");
            fprintf(fp1,"{\n");
            eval_stmt(stmt->body);
            fprintf(fp1,"\n}");
            break;
        case STMT_PRINT:
            fprintf(fp1,"printf(");
                if((stmt->expr->kind)==EXPR_STRING)
                        eval_expression(stmt->expr);
                else if((stmt->expr->kind)==EXPR_NAME){
                        switch(stmt->expr->s->type){
                            case SYM_INT:
                                fprintf(fp1,"\"%%d\",%s",stmt->expr->s->name);
                                break;
                            case SYM_FLOAT:
                                fprintf(fp1,"\"%%f\",%s",stmt->expr->s->name);
                                break;
                            case SYM_ND:
                                fprintf(fp1,"\"%%g\",%s",stmt->expr->s->name);
                                break;
                            case SYM_VOID:
                                break;
                            case SYM_STRING:
                                fprintf(fp1,"\"%%s\",%s",stmt->expr->s->name);
                                break;
                        }
                }
            fprintf(fp1,");\n");
            break;
        case STMT_INPUT:
            
            if((stmt->init_expr)!=NULL){
                if((stmt->expr)!=NULL){
                    fprintf(fp1,"printf(");
                    eval_expression(stmt->expr);
                    fprintf(fp1,";\n");
                    fprintf(fp1,"scanf(\"%%s\",%s);\n",stmt->init_expr->s->name);
                    break;
                }else if((stmt->expr)==NULL){
                    fprintf(fp1,"scanf(\"%%s\",%s);\n",stmt->init_expr->s->name);
                    break;
                }
            }
        case STMT_END:
            break;
        case STMT_BLOCK:
            eval_stmt(stmt->body);
            if((stmt->next)){
                if((stmt->next->kind != STMT_END))
                    eval_stmt(stmt->next);
            }
            break;
            
    }
    
}

void eval_expression(struct expr* e){
    if(!e)
        return;
    if(e->kind == EXPR_BRACKET){
        fprintf(fp1,"(");
        eval_expression(e->left);
        fprintf(fp1,")");
        return;
    }

    eval_expression(e->left);

    switch(e->kind){
        case EXPR_ADD:
            fprintf(fp1,"+");
            break;
        case EXPR_SUB:
            fprintf(fp1,"-");
            break;
        case EXPR_MUL:
            fprintf(fp1,"*");
            break;
        case EXPR_DIV:
            fprintf(fp1,"/");
            break;
        case EXPR_GT:
            fprintf(fp1,">");
            break;
        case EXPR_LT:
            fprintf(fp1,"<");
            break;
        case EXPR_GE:
            fprintf(fp1,">=");
            break;
        case EXPR_LE:
            fprintf(fp1,"<=");
            break;
        case EXPR_EQ:
            fprintf(fp1,"==");
            break;
        case EXPR_NE:
            fprintf(fp1,"!=");
            break;
        case EXPR_ASSIGN:
            fprintf(fp1,"=");
            break;
        case EXPR_NAME:
            fprintf(fp1,"%s",e->s->name);

            break;
        case EXPR_INTEGER:
            fprintf(fp1,"%d",e->integer_value);
            break;
        case EXPR_FLOAT:
            fprintf(fp1,"%f",e->float_value);
            break;
        case EXPR_STRING:
            fprintf(fp1,"%s",e->string);
            break;
        case EXPR_BRACKET:
            break;
    }
    eval_expression(e->right);
}

void eval_declaration(struct decl* decl){
    extern struct symbol symtab_function[NHASH];
    fprintf(fp3,"void ");
    if((decl->name)!=NULL)
        fprintf(fp3,"%s", decl->name->name);
    fprintf(fp3,"()");
    if((decl->code)!=NULL){
        fprintf(fp3,"{\n");
        eval_stmt_function(decl->code);
        fprintf(fp3,"}\n\n");
    }
}

void eval_stmt_function(struct stmt* stmt){
    if(!stmt)
        return;
    
    
    switch(stmt->kind){
        case STMT_DECL:
            break;
        case STMT_CALL:
            fprintf(fp3,"%s();\n",stmt->init_expr->s->name);
            break;
        case STMT_EXPR:
            eval_expression_function(stmt->expr);
            fprintf(fp3,";\n");
            break;
        case STMT_IF_ELSE:
            fprintf(fp3,"if (");
            eval_expression_function(stmt->expr);
            fprintf(fp3,")");
            fprintf(fp3,"{\n");
            eval_stmt_function(stmt->body);
            fprintf(fp3,"}");
            if((stmt->else_body)!=NULL){
                fprintf(fp3,"else{\n");
                eval_stmt_function(stmt->else_body);
                fprintf(fp3,"}\n");
                break; 
            }
            if((stmt->next)!=NULL)
                eval_stmt_function(stmt->next);

            break;
        case STMT_IF_ELIF:
            fprintf(fp3,"else if (");
            eval_expression_function(stmt->expr);
            fprintf(fp3,")");
            fprintf(fp3,"{\n");
            eval_stmt_function(stmt->body);
            fprintf(fp3,"}");
            if((stmt->else_body)!=NULL){
                fprintf(fp3,"else{\n");
                eval_stmt_function(stmt->else_body);
                fprintf(fp3,"}\n");
                break; 
            }
            while((stmt->next)!=NULL){
                stmt=stmt->next;
                eval_stmt_function(stmt);
                break;
                /*if((stmt->next->next)!=NULL)
                    eval_stmt(stmt->next->next);*/
            }
            break;
        case STMT_WHILE:
            fprintf(fp3,"while (");
            eval_expression_function(stmt->expr);
            fprintf(fp3,")");
            fprintf(fp3,"{\n");
            eval_stmt_function(stmt->body);
            fprintf(fp3,"\n}");
            break;
        case STMT_PRINT:
            fprintf(fp3,"printf(");
                if((stmt->expr->kind)==EXPR_STRING)
                        eval_expression_function(stmt->expr);
                else if((stmt->expr->kind)==EXPR_NAME){
                        switch(stmt->expr->s->type){
                            case SYM_INT:
                                fprintf(fp3,"\"%%d\",%s",stmt->expr->s->name);
                                break;
                            case SYM_FLOAT:
                                fprintf(fp3,"\"%%f\",%s",stmt->expr->s->name);
                                break;
                            case SYM_ND:
                                fprintf(fp3,"\"%%g\",%s",stmt->expr->s->name);
                                break;
                            case SYM_VOID:
                                break;
                            case SYM_STRING:
                                fprintf(fp3,"\"%%s\",%s",stmt->expr->s->name);
                                break;
                        }
                }
            fprintf(fp3,");\n");
            break;
        case STMT_INPUT:
            fprintf(fp3,"scanf(");
            if((stmt->init_expr)!=NULL){
                if((stmt->expr)!=NULL){
                    eval_expression(stmt->expr);
                    fseek(fp3,-sizeof(char),SEEK_CUR);
                    fprintf(fp3,"%%s\",%s);\n",stmt->init_expr->s->name);
                    break;
                }else if((stmt->expr)==NULL){
                    fprintf(fp3,"\"%%s\",%s);\n",stmt->init_expr->s->name);
                    break;
                }
            }
        case STMT_END:
            break;
        case STMT_BLOCK:
            eval_stmt_function(stmt->body);
            if((stmt->next))
                eval_stmt_function(stmt->next);
            break;
            
    }
    
}

void eval_expression_function(struct expr* e){
    struct symbol* temp;
    if(!e)
        return;
    if(e->kind == EXPR_BRACKET){
        fprintf(fp3,"(");
        eval_expression_function(e->left);
        fprintf(fp3,")");
        return;
    }

    eval_expression_function(e->left);

    switch(e->kind){
        case EXPR_ADD:
            fprintf(fp3,"+");
            break;
        case EXPR_SUB:
            fprintf(fp3,"-");
            break;
        case EXPR_MUL:
            fprintf(fp3,"*");
            break;
        case EXPR_DIV:
            fprintf(fp3,"/");
            break;
        case EXPR_GT:
            fprintf(fp3,">");
            break;
        case EXPR_LT:
            fprintf(fp3,"<");
            break;
        case EXPR_GE:
            fprintf(fp3,">=");
            break;
        case EXPR_LE:
            fprintf(fp3,"<=");
            break;
        case EXPR_EQ:
            fprintf(fp3,"==");
            break;
        case EXPR_NE:
            fprintf(fp3,"!=");
            break;
        case EXPR_ASSIGN:
            fprintf(fp3,"=");
            break;
        case EXPR_NAME:
            fprintf(fp3,"%s",e->s->name);
            temp=lookup_function(e->s);
            e->s->var_of_function = 1;
            break;
        case EXPR_INTEGER:
            fprintf(fp3,"%d",e->integer_value);
            break;
        case EXPR_FLOAT:
            fprintf(fp3,"%f",e->float_value);
            break;
        case EXPR_STRING:
            fprintf(fp3,"%s",e->string);
            break;
        case EXPR_BRACKET:
            break;
    }
    eval_expression_function(e->right);
}

void stmtFree(struct stmt* stmt){
    int i;
    char *errorMsg = "";
    char *temp;
    switch(stmt->kind){
        case STMT_DECL:
            for(i=0;i<NHASH;i++){
                if(symtab_function[i].name!=NULL){
                    switch(symtab_function[i].type){
                        case SYM_ND:
                            temp = strdup(symtab_function[i].name);
                            errorMsg = strcat(temp," must be declared.");
                            yyerror(errorMsg);
                            exit(1);
                            break;
                        case SYM_VOID:
                            break;
                        case SYM_INT:
                            fprintf(fp4,"int ");
                            fprintf(fp4,"%s;\n",symtab_function[i].name);
                            break;
                        case SYM_FLOAT:
                            fprintf(fp4,"float ");
                            fprintf(fp4,"%s;\n",symtab_function[i].name);
                            break;
                        case SYM_STRING:
                            fprintf(fp4,"char* ");
                            fprintf(fp4,"%s;\n",symtab_function[i].name);
                            break;
                    }
                }
            }
            expFree(stmt->decl->value);
            free(stmt->decl);

            break;
        case STMT_CALL:
            if((stmt->init_expr))
                expFree(stmt->init_expr);
            break;
        case STMT_EXPR:
            expFree(stmt->expr);
            break;
            
        case STMT_IF_ELSE: case STMT_IF_ELIF: case STMT_WHILE:
            free(stmt->expr);
            if((stmt->body))
                stmtFree(stmt->body);
            if((stmt->else_body))
                stmtFree(stmt->else_body);
            if((stmt->next))
                stmtFree(stmt->next);
            break;
        
        case STMT_PRINT:
            /*expFree(stmt->expr);*/
        case STMT_INPUT:
            if((stmt->init_expr))
                expFree(stmt->init_expr);
            if((stmt->expr))
                expFree(stmt->expr);
        case STMT_END:
            break;
        
        case STMT_BLOCK:
            stmtFree(stmt->body);
            if((stmt->next))
                stmtFree(stmt->next);
            
    }
    free(stmt);    
}

void expFree(struct expr* expr){
    switch(expr->kind){
        /* Two subtrees */
        case EXPR_ADD:
        case EXPR_SUB:
        case EXPR_MUL:
        case EXPR_DIV:
        case EXPR_GT:
        case EXPR_LT:
        case EXPR_GE:
        case EXPR_LE:
        case EXPR_EQ:
        case EXPR_NE:
            if((expr->left)!=(expr->right)){
                if((expr->right))
                    expFree(expr->right);
            }
            break;
        case EXPR_ASSIGN:
            if((expr->left))
                expFree(expr->left);
            if((expr->right))
                expFree(expr->right);
            break;
           
        /* No subtree */
        case EXPR_NAME:
            break;
        case EXPR_INTEGER:
            break;
        case EXPR_FLOAT:
            break;
        case EXPR_STRING:
            break;
        
        case EXPR_BRACKET:
            expFree(expr->left);
            break;    
    }
    free(expr);     /* always free the node itself */
}

