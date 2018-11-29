/*
 * Support routines and data structures
 */

 /* interface to the lexer */
 extern int yylineno;           /* taken from lexer */
 /*void yyerror(char* s, ...);*/


/*
 * SYMBOL TABLE: since each symbol can potentially be both a variable and a user-defined function,
 * the "value" field holds the symbol's value as a variable, while the "func" field points to the 
 * specific AST for the user code for the function. Finally, the "syms" points to a linked list
 * of the parameters, which are themselves symbols
 */
typedef enum{
    SYM_ND,
    SYM_VOID,
    SYM_INT,
    SYM_FLOAT,
    SYM_STRING
 } sym_t;

typedef struct symbol {     /* identifiers */
    char *name;
    sym_t type;
    double value;
    int var_of_function;    /* Flag if a variable is of a function or not */
    struct stmt* func;       /* statement for the function */
}symbol;

/* Simple symbol table of fixed size */
#define  NHASH 9999
extern struct symbol symtab[NHASH];
extern struct symbol symtab_function[NHASH];
/* Function for lookup in the symtab */
struct symbol* lookup(char* );  
struct symbol* lookup_function(struct symbol* sym);    




/*
 * Abstract Syntax Tree
 */

/* Declarations */



 typedef struct decl{
     struct symbol *name;
     struct expr *value;
     struct stmt *code;
     struct decl *next;
 }decl;

struct decl* decl_create(struct symbol* name, struct expr* value, struct stmt* code);

/* Statement */
 typedef enum{
    STMT_DECL,
    STMT_CALL,
    STMT_EXPR,
    STMT_IF_ELSE,
    STMT_IF_ELIF,
    STMT_WHILE,
    STMT_PRINT,
    STMT_INPUT,
    STMT_END,
    STMT_BLOCK
 } stmt_t;

 typedef struct stmt{
     stmt_t kind;
     struct decl *decl;
     struct expr *init_expr;
     struct expr *expr;
     struct expr *next_expr;
     struct stmt *body;
     struct stmt *else_body;
     struct stmt *next;
 }stmt;
struct stmt* stmt_create(stmt_t kind, struct decl *decl,
                            struct expr *init_expr,struct expr *expr,
                            struct expr *next_expr, struct stmt *body,
                            struct stmt *else_body, struct stmt *next);


/* Expression */
typedef enum{
    EXPR_ADD,
    EXPR_SUB,
    EXPR_MUL,
    EXPR_DIV,
    EXPR_GT,
    EXPR_LT,
    EXPR_GE,
    EXPR_LE,
    EXPR_EQ,
    EXPR_NE,
    EXPR_ASSIGN,
    EXPR_BRACKET,
    EXPR_NAME,
    EXPR_INTEGER,
    EXPR_FLOAT,
    EXPR_STRING
} expr_t;

typedef struct expr{
    expr_t kind;
    struct expr *left;
    struct expr *right;
    struct symbol* s;
    int integer_value;
    float float_value;
    const char* string;
    sym_t type;

} expr;


struct expr* expr_create(expr_t kind, struct expr* L, struct expr* R, sym_t type);
struct expr* expr_create_name(struct symbol* s, sym_t type, struct stmt* func);
struct expr* expr_create_integer(int i);
struct expr* expr_create_float(float f);
struct expr* expr_create_string(const char* str);

sym_t get_sym_type(struct expr* e);

/* Evaluation of AST */
void eval_stmt(struct stmt* stmt);
void eval_expression(struct expr* e);
void eval_declaration(struct decl* decl);
void eval_stmt_function(struct stmt* stmt);
void eval_expression_function(struct expr* e);
void stmtFree(struct stmt* stmt);
void expFree(struct expr* expr);
void declFree(struct decl* decl);