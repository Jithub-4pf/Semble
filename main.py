import ply.lex as lex
from termcolor import colored


COLOR_ERROR = 'red'
COLOR_CODE = 'yellow'
files_structure = []

def highlight_error(message, token):
    lines = token.lexer.lexdata.split('\n')
    error_line = lines[token.lineno - 1]
    error_position = token.lexpos - error_line.rfind('\n', 0, token.lexpos)
    error_highlighted = error_line[:token.lexpos] + colored(error_line[token.lexpos], COLOR_ERROR) + error_line[token.lexpos + 1:]
    print(colored(message, COLOR_ERROR))
    print(error_highlighted)
    print(' ' * (error_position - 1) + colored('^', COLOR_ERROR))

keywords = ['i8', 'let16', 'let8', 'struct', 'def', 'return', "let", "if", "else", "and", "or", "true", "false", "NULL", "for", "while", "break", "as"]

# List of token names
tokens = [
    'NAME',
    'INTEGER',
    'FLOAT',
    'STRING',
    'CHARACTER',
    'ADD',
    "SUB",
    "MUL",
    "DIV",
    'LPAREN',
    'RPAREN',
    'LBRACE',
    'RBRACE',
    'LBRACKET',
    'RBRACKET',
    'SEMICOLON',
    'COLON',
    'COMMA',
    'DOT',
    'ARROW',
    'ASSIGN',
    'PLUS_ASSIGN',
    'MINUS_ASSIGN',
    'TIMES_ASSIGN',
    'DIVIDE_ASSIGN',
    'MODULO_ASSIGN',
    'EQUAL',
    'NOT_EQUAL',
    'LESS_THAN',
    'LESS_THAN_EQUAL',
    'GREATER_THAN',
    'GREATER_THAN_EQUAL',
    'LOGICAL_AND',
    'LOGICAL_OR',
    'LOGICAL_NOT',
    'BITWISE_AND',
    'BITWISE_OR',
    'BITWISE_XOR',
    'BITWISE_NOT',
    'LEFT_SHIFT',
    'RIGHT_SHIFT',
    'MEMBER_ACCESS',
    'TERNARY_IF',
    'TERNARY_ELSE',
    'ELLIPSIS',
    'STRING_LIT',
    'DPLUS',
    "DMINUS",
    "NEWLINE",
    "COMMENT"
] + [keyword.upper() for keyword in keywords]

# Regular expression rules for simple tokens
def t_NEWLINE(t):
    r'\n+'
    t.lexer.lineno += len(t.value)
    #t.lexer.lexpos = 0
   
def t_COMMENT(t):
    r'(/\\(.|\n)*?\\/)|(//.*)'
    pass
 
    
t_ADD = r'\+'
t_SUB = r'\-'
t_MUL = r'\/'
t_DIV = r'\*'
t_LPAREN = r'\('
t_RPAREN = r'\)'
t_LBRACE = r'\{'
t_RBRACE = r'\}'
t_LBRACKET = r'\['
t_RBRACKET = r'\]'
t_SEMICOLON = r';'
t_COLON = r':'
t_COMMA = r','
t_DOT = r'\.'
t_ARROW = r'\-\>'
t_ASSIGN = r'='
t_PLUS_ASSIGN = r'\+='
t_MINUS_ASSIGN = r'-='
t_TIMES_ASSIGN = r'\*='
t_DIVIDE_ASSIGN = r'/='
t_MODULO_ASSIGN = r'%='
t_DPLUS = r'\+\+'
t_DMINUS = r'--'
t_EQUAL = r'=='
t_NOT_EQUAL = r'!='
t_LESS_THAN_EQUAL = r'<='
t_GREATER_THAN_EQUAL = r'>='
t_LESS_THAN = r'<'
t_GREATER_THAN = r'>'

t_LOGICAL_NOT = r'!'
t_BITWISE_AND = r'&'
t_BITWISE_OR = r'\|'
t_BITWISE_XOR = r'\^'
t_BITWISE_NOT = r'~'
t_LEFT_SHIFT = r'<<'
t_RIGHT_SHIFT = r'>>'
t_MEMBER_ACCESS = r'\.'
t_TERNARY_IF = r'\?'
t_TERNARY_ELSE = r':'
t_ELLIPSIS = r'\.\.\.'

# Regular expression rules with action code
def t_FLOAT(t):
    r'\d+\.\d+'
    t.value = float(t.value)
    return t

def t_STRING(t):
    r'"(?:[^"\\]|\\.)*"'
    t.value = t.value[1:-1]  # Remove quotes
    return t

def t_STRING_LIT(t):
    r"'(?:[^'\\]|\\.)*'"
    t.value = t.value[1:-1]  # Remove quotes
    return t


def t_INTEGER(t):
    r'\d+'
    t.value = int(t.value)
    return t

def t_NAME(t):
    r'[a-zA-Z_][a-zA-Z0-9_]*'
    if t.value in keywords:
        t.type = t.value.upper()
    return t

# Ignored characters (whitespace and tabs)
t_ignore = ' \t'

# Error handling rule
def t_error(t):
    error_message = f"Lexer error at line {t.lineno}: Invalid character '{t.value[0]}'"
    highlight_error(error_message, t)
    exit(1)


# Build the lexer
lex = lex.lex()



library = True



# ------------------------------------------------------------------------------------------------#
#                                                                                                 #
#                       PARSER                                                                    #   
#                                                                                                 #
# ------------------------------------------------------------------------------------------------#







class Argument:
    def __init__(self, name, stype=None, eight=False):
        self.name = name
        self.type = stype
    def istyped(self):
        if self.type is None:
            return False
        else:
            return True





import ply.yacc as yacc
import os

file = os.argv[1]

lineno = 1

OUTPUT_FILE = "main"


def p_code(p):
    '''
    code : function
         | struct
         | code code
    '''
    runcode(p[1])

# ------------------------------------------------------------------------------------------------#
#                                                                                                 #
#                       STRUCTS                                                                   #   
#                                                                                                 #
# ------------------------------------------------------------------------------------------------#


    

def p_struct_one_member(p):
    '''
    struct : DEF NAME LBRACE NAME RBRACE
    ''' 

    p[0] = [("struct", p[2], [p[5]], [], ), p.lineno(1)]

def p_struct_members(p):
    '''
    struct : DEF NAME LBRACE name_list RBRACE
    ''' 

    p[0] = [("struct", p[2], p[4], []), p.lineno(1)]
    
def p_functions(p):
    '''
    functions : function function
              | functions function
    '''
    p[0] = [p[1], p[2]]

def p_struct_one_member_with_methods(p):
    '''
    struct : DEF NAME LBRACE NAME RBRACE LPAREN functions RPAREN
    ''' 

    p[0] = [("struct", p[2], [p[5]], p[7]), p.lineno(1)]

def p_struct_members_with_methods(p):
    '''
    struct : DEF NAME LBRACE name_list RBRACE LPAREN functions RPAREN
    ''' 

    p[0] = [("struct", p[2], p[4], p[7]), p.lineno(1)]

def p_struct_one_member_with_method(p):
    '''
    struct : DEF NAME LBRACE NAME RBRACE LPAREN function RPAREN
    ''' 

    p[0] = [("struct", p[2], [p[5]], [p[7]]), p.lineno(1)]

def p_struct_members_with_method(p):
    '''
    struct : DEF NAME LBRACE name_list RBRACE LPAREN function RPAREN
    ''' 

    p[0] = [("struct", p[2], p[4], [p[7]]), p.lineno(1)]
    

# ------------------------------------------------------------------------------------------------#
#                                                                                                 #
#                       FUNCTIONS                                                                 #   
#                                                                                                 #
# ------------------------------------------------------------------------------------------------#
def p_function_no_args(p):
    '''
    function : DEF NAME LPAREN RPAREN statementc
             | DEF NAME LPAREN RPAREN statement
    ''' 

    p[0] = [("function", p[2], p[5], []), p.lineno(1)]
    
def p_function_one_arg(p):
    '''
    function : DEF NAME LPAREN argument RPAREN statementc
             | DEF NAME LPAREN argument RPAREN statement
    ''' 

    p[0] = [("function", p[2], p[6], [p[4]]), p.lineno(1)]
    
def p_function_multi_args(p):
    '''
    function : DEF NAME LPAREN arg_list RPAREN statementc
             | DEF NAME LPAREN arg_list RPAREN statement
    ''' 

    p[0] = [("function", p[2], p[6], p[4]), p.lineno(1)]
    

def p_name_list(p):
    '''
    name_list : NAME COMMA NAME
              | NAME COMMA name_list
    '''
    p[0] = [p[1], p[3]]
    
def p_arg_list(p):
    '''
    arg_list : argument COMMA argument
             | argument COMMA arg_list
    '''
    p[0] = [p[1], p[3]]


    
# ------------------------------------------------------------------------------------------------#
#                                                                                                 #
#                       STATEMENTS                                                                #   
#                                                                                                 #
# ------------------------------------------------------------------------------------------------#
    
def p_statements(p):
    '''
    statements : statement statements
               | statement statement
    '''
    
    p[0] = [("statements", p[1], p[2]), p.lineno(1)]
    
def p_statementc(p):
    '''
    statementc : LBRACE statement RBRACE
               | LBRACE statements RBRACE
    '''
    p[0] = [("statementc", p[2]), p.lineno(1)]


# ------------------------------------------------------------------------------------------------#
#                                                                                                 #
#                       CODE                                                                      #   
#                                                                                                 #
# ------------------------------------------------------------------------------------------------#

def p_as(p):
    '''
    statement : NAME AS NAME SEMICOLON
    '''
    p[0] = [("as", p[1], p[3]), p.lineno(1)]

def p_define_variable(p):
    '''
    statement : LET NAME ASSIGN expression SEMICOLON
    '''
    p[0] = [("variableDefinition", p[2], p[4]), p.lineno(1)]
    
def p_define_variable_other_type(p):
    '''
    statement : NAME NAME ASSIGN expression SEMICOLON
    '''

    p[0] = [("variableDefinitionOtherType", p[2], p[4], p[1]), p.lineno(1)]
    
def p_define_variable_other_type_no_val(p):
    '''
    statement : NAME NAME SEMICOLON
    '''

    p[0] = [("variableDefinitionOtherType", p[2], None, p[1]), p.lineno(1)]

def p_define_variable_no_value(p):
    '''
    statement : LET NAME SEMICOLON
    '''

    p[0] = [("variableDefinition", p[2], None), p.lineno(1)]
    
def p_update_variable(p):
    '''
    statement : NAME ASSIGN expression SEMICOLON
    '''
    p[0] = [("variableUpdate", p[1], p[3]), p.lineno(1)]

def p_update_attribute(p):
    '''
    statement : NAME DOT NAME ASSIGN expression SEMICOLON
    '''
    p[0] = [("attrUpdate", p[1], p[3], p[5]), p.lineno(1)]
    
def p_update_indexed(p):
    '''
    statement : expression LBRACKET expression RBRACKET ASSIGN expression SEMICOLON
    '''

    p[0] = [("indexedUpdate", p[1], p[3], p[6]), p.lineno(1)]
    
def p_update_indexed_i8(p):
    '''
    statement : expression LBRACKET I8 COLON expression RBRACKET ASSIGN expression SEMICOLON
    '''

    p[0] = [("indexedUpdate8", p[1], p[5], p[8]), p.lineno(1)]
        

def p_return(p):
    '''
    statement : RETURN expression SEMICOLON
    '''
    p[0] = [("return", p[2]), p.lineno(1)]
    
    
def p_call_function(p):
    '''
    statement : NAME LPAREN expression_list RPAREN SEMICOLON
              | NAME LPAREN expression RPAREN SEMICOLON
    '''

    p[0] = [("fcall", p[1], p[3]), p.lineno(1)]
    
def p_call_function_None(p):
    '''
    statement : NAME LPAREN RPAREN SEMICOLON
    '''
    p[0] = [("fcall", p[1], []), p.lineno(1)]

def p_statement_member_method(p):
    '''
    statement  : NAME DOT NAME LPAREN expression RPAREN SEMICOLON
               | NAME DOT NAME LPAREN expression_list RPAREN SEMICOLON
    '''
    p[0] = [("member_call_statement", p[1], p[3], p[5]), p.lineno(1)]

def p_statement_member_method_none(p):
    '''
    statement  : NAME DOT NAME LPAREN RPAREN SEMICOLON
    '''
    p[0] = [("member_call_statement", p[1], p[3], []), p.lineno(1)]
    
    
def p_statement_if(p):
    '''
    statement : IF LPAREN expression RPAREN statementc
              | IF LPAREN expression RPAREN statement
    '''
    p[0] = [(["if"], p[3], p[5]), p.lineno(1)]

def p_statement_ifelse(p):
    '''
    statement : IF LPAREN expression RPAREN statementc ELSE statementc
                | IF LPAREN expression RPAREN statement ELSE statement
                | IF LPAREN expression RPAREN statementc ELSE statement
                | IF LPAREN expression RPAREN statement ELSE statementc
    '''
    p[0] = [("ifelse", p[3], p[5], p[7]), p.lineno(1)]
    
def p_statement_for(p):
    '''
    statement : FOR LPAREN statement expression SEMICOLON statement RPAREN statementc
              | FOR LPAREN statement expression SEMICOLON statement RPAREN statement
    '''
    p[0] = [("for", p[4], p[3], p[6], p[8]), p.lineno(1)]
  


def p_statement_while(p):
    '''
    statement : WHILE LPAREN expression RPAREN statementc
              | WHILE LPAREN expression RPAREN statement
    '''
    p[0] = [("while", p[3], p[5]), p.lineno(1)]

def p_statement_break(p):
    '''
    statement : BREAK SEMICOLON
    '''

    p[0] = [("break", p[1]), p.lineno(1)]
    
# ------------------------------------------------------------------------------------------------#
#                                                                                                 #
#                       ARGS                                                                      #   
#                                                                                                 #
# ------------------------------------------------------------------------------------------------#

def p_arg(p):
    '''
    argument : NAME
    '''   
    p[0] = Argument(p[1])

def p_arg_typed(p):
    '''
    argument : NAME COLON NAME
    '''
    p[0] = Argument(p[1], stype=p[3])
    
# ------------------------------------------------------------------------------------------------#
#                                                                                                 #
#                       EXPRESSIONS                                                               #   
#                                                                                                 #
# ------------------------------------------------------------------------------------------------#

def p_expression_list(p):
    '''
    expression_list : expression COMMA expression_list
                    | expression COMMA expression
    '''
    p[0] = [p[1], p[3]]
    
def p_expression_member(p):
    '''
    expression : NAME DOT NAME
    '''
    p[0] = [("member", p[1], p[3]), p.lineno(1)]

    
def p_expression_member_method(p):
    '''
    expression : NAME DOT NAME LPAREN expression RPAREN
               | NAME DOT NAME LPAREN expression_list RPAREN
    '''
    p[0] = [("member_call", p[1], p[3], p[5]), p.lineno(1)]

def p_expression_member_method_none(p):
    '''
    expression : NAME DOT NAME LPAREN RPAREN
    '''
    p[0] = [("member_call", p[1], p[3], []), p.lineno(1)]
    


def p_expression_bracketed_one(p):
    '''
    expression : LBRACE expression RBRACE
    '''
    p[0] = Semble_List([p[2]])
    
def p_expression_bracketed_multi(p):
    '''
    expression : LBRACE expression_list RBRACE
    '''
    p[0] = Semble_List(p[2])
     
def p_expression_indexed(p):
    '''
    expression : expression LBRACKET expression RBRACKET
    '''
    p[0] = [("index", p[1], p[3]), p.lineno(1)]
    
def p_expression_indexed_i8(p):
    '''
    expression : expression LBRACKET I8 COLON expression RBRACKET
    '''
    p[0] = [("index8", p[1], p[5]), p.lineno(1)]




def p_expression_basic(p):
    '''
    expression : INTEGER
               | NAME
    '''
    p[0] = p[1]

def p_expression_str(p):
    '''
    expression : STRING
    '''
    p[0] = STR(p[1])

class Str_lit():
    def __init__(self, v):
        self.v = v

def p_expression_str_lit(p):
    '''
    expression : STRING_LIT
    '''
    p[0] = Str_lit(p[1])


def p_expression_operation(p):
    '''
    expression : expression ADD expression
               | expression SUB expression
               | expression MUL expression
               | expression DIV expression
               | expression EQUAL expression
               | expression NOT_EQUAL expression
               | expression GREATER_THAN_EQUAL expression
               | expression LESS_THAN_EQUAL expression
               | expression GREATER_THAN expression
               | expression LESS_THAN expression
               | expression LEFT_SHIFT expression
               | expression RIGHT_SHIFT expression
               | expression BITWISE_OR expression
               | expression BITWISE_AND expression
               
    '''
    p[0] = [(p[2], p[1], p[3]), p.lineno(1)]
    
def p_expression_and(p):
    '''
    expression : expression AND expression
    '''
    p[0] = [("&&", p[1], p[3]), p.lineno(1)]
    
def p_expression_or(p):
    '''
    expression : expression OR expression
    '''
    p[0] = [("||", p[1], p[3]), p.lineno(1)]
    

def p_statement_dplus(p):
    '''
    statement : NAME DPLUS SEMICOLON
                | NAME DMINUS SEMICOLON
    '''
    #print(p)
    p[0] = [(p[2] + "s", p[1]), p.lineno(1)]


def p_statement_pdplus(p):
  '''
  statement : DPLUS NAME SEMICOLON
            | DMINUS NAME SEMICOLON
  '''
  p[0] = [(p[1] + "s", p[2]), p.lineno(1)]


def p_expression_dplus(p):
  '''
  expression : NAME DPLUS
             | NAME DMINUS
  '''
  p[0] = [(p[2], p[1]), p.lineno(1)]

def p_expression_dplusp(p):
  '''
  expression : DPLUS NAME
             | DMINUS NAME
  '''
  p[0] = [(p[1] + "p", p[2]), p.lineno(1)]
    
    
def p_call_function_exp(p):
    '''
    expression : NAME LPAREN expression_list RPAREN
              | NAME LPAREN expression RPAREN
    '''

    p[0] = [("efcall", p[1], p[3]), p.lineno(1)]
    
def p_call_function_exp_None(p):
    '''
    expression : NAME LPAREN RPAREN
    '''

    p[0] = [("efcall", p[1], []), p.lineno(1)]
    
def p_expression_with_paren(p):
    '''
    expression : LPAREN expression RPAREN
    '''
    p[0] = p[2]
    
def p_expression_false_and_NULL(p):
    '''
    expression : FALSE
               | NULL
    '''
    p[0] = 0

def p_expression_true(p):
    '''
    expression : TRUE
    '''
    p[0] = 1

def p_expression_unary(p):
    '''
    expression : LOGICAL_NOT expression
    '''
    p[0] = [(p[1], p[2]), p.lineno(1)]

def p_negative(p):
    '''
    expression : SUB expression
    '''
    p[0] = [("neg", p[2]), p.lineno(1)]

# ------------------------------------------------------------------------------------------------#
#                                                                                                 #
#                       COMPILE                                                                   #   
#                                                                                                 #
# ------------------------------------------------------------------------------------------------#

import ply.yacc as yacc
import os

def find_real_linecount(lineno, filepath, substring):
    global files_structure
    x = 0
    for i in range(lineno):
        if files_structure[i] != filepath: x += 1
    return lineno - x

import colorama
from colorama import Fore, Style

colorama.init(autoreset=True)

def p_error(p):
    global files_structure
    
    def print_error_line(filepath, lineno, error_part=None):
        try:
            with open(filepath, 'r') as f:
                lines = f.readlines()
                error_line = lines[lineno - 1].strip() if lineno <= len(lines) else "Line not found"
                print(Fore.YELLOW + "\n\tLine {}:".format(lineno) + Fore.WHITE + "\n\n\t{}".format(error_line))
                if error_part != None:
                    place = error_line.find(str(error_part))
                    print("\t" + (" " * place) + ("^" * len(str(error_part))))
        except Exception as e:
            print(Fore.RED + "Error reading file {}: {}".format(filepath, str(e)))
    
    if p is None:
        last_line_number = len(files_structure)
        last_filepath = files_structure[-1] if files_structure else "<unknown>"
        error_message = (
            Fore.RED + "SembleLang: Semantic error: The parser reached an unexpected end of input.\n"
            + Fore.WHITE + "This might be due to an incomplete expression or an unexpected token.\n"
            + Fore.WHITE + "Please check the syntax of your source files.\n"
            + Fore.RED + "Potential issue near the end of file: {} (Line: {})"
        ).format(last_filepath, last_line_number)
        print(error_message)
        if last_filepath != "<unknown>":
            print_error_line(last_filepath, last_line_number)
        exit(1)
    else:
        filepath = files_structure[p.lineno - 1]
        error_message = (
            Fore.RED + "SembleLang Parsing Error: (FILENAME: \"{}\"): Unexpected {} on line {}\n"
            + Fore.WHITE + "Please check the syntax near the indicated line for possible issues."
        ).format(filepath, p.value, find_real_linecount(p.lineno, filepath, str(p.value)))
        print(error_message)
        print_error_line(filepath, find_real_linecount(p.lineno, filepath, str(p.value)), error_part=p.value)
        exit(1)
        
class E_Error:
    def __init__(self, lineno, value, message):
        self.lineno = lineno
        self.value = value
        self.message = message

def e_error(p):
    global files_structure
    
    def print_error_line(filepath, lineno, error_part=None):
        try:
            with open(filepath, 'r') as f:
                lines = f.readlines()
                error_line = lines[lineno - 1].strip() if lineno <= len(lines) else "Line not found"
                print(Fore.YELLOW + "\n\tLine {}:".format(lineno) + Fore.WHITE + "\n\n\t{}".format(error_line))
                if error_part != None:
                    place = error_line.find(str(error_part))
                    print("\t" + (" " * place) + ("^" * len(str(error_part))))
        except Exception as e:
            print(Fore.RED + "Error reading file {}: {}".format(filepath, str(e)))
    
    if p is None:
        last_line_number = len(files_structure)
        last_filepath = files_structure[-1] if files_structure else "<unknown>"
        error_message = (
            Fore.RED + "SembleLang: Semantic error: The parser reached an unexpected end of input.\n"
            + Fore.WHITE + "This might be due to an incomplete expression or an unexpected token.\n"
            + Fore.WHITE + "Please check the syntax of your source files.\n"
            + Fore.RED + "Potential issue near the end of file: {} (Line: {})"
        ).format(last_filepath, last_line_number)
        print(error_message)
        if last_filepath != "<unknown>":
            print_error_line(last_filepath, last_line_number)
        exit(1)
    else:
        filepath = files_structure[p.lineno - 1]
        error_message = (
            Fore.RED + "SembleLang Parsing Error: (FILENAME: \"{}\"): \"{}\" near line {}\n"
            + Fore.WHITE + "Please check the syntax near the indicated line for possible issues."
        ).format(filepath, p.message, find_real_linecount(p.lineno, filepath, str(p.value)))
        print(error_message)
        print_error_line(filepath, find_real_linecount(p.lineno, filepath, str(p.value)), error_part=p.value)
        exit(1)



funcs = {'_start': ["call main", "movl %ebx, %eax", "movl $1, %eax", "int $0x80"]}
currentFuncVarVal = 0
data = {}
currentFuncVars= {}
currentFuncArgs= {}
MPN = 0
funcVarCount = {}
jumpStack = []
currentFunc = None
currentLineNo = 0
structs = {}
currentFuncStructs = {}
returnVals = {}
method_name = None

def asm(s):
    try:
        funcs[currentFunc].append(s)
    except:
        quit("Must be in a function to execute code!")
        
def extractFunctionNames(l):
    names = []
    for i in l:
        
        if type(i) == tuple:
            names.append(i[1])
    return names
    
def listify(l):
    
    if type(l) is not list:
        return [l]
    elif l == []:
        return l
    else:
        if len(l) == 1:
            return l
        elif type(l[0]) is not list:
            return [l[0]] + listify(l[1])
        else:
            return l[0] + listify(l[1])
    
def Reverse(lst):
    return [ele for ele in reversed(lst)]

class Semble_Struct:
    def __init__(self, name, members, methods):
        self.name = name
        if len(members) == 1:
            self.members = members
        
        else:
            self.members = listify(members)
        if len(methods) == 1:
            self.methods = methods
        
        else:
            self.methods = methods
            
            
class Semble_List:
    def __init__(self, vals):
        self.vals = vals
    def __str__(self):
        return "list {}".format(self.vals)
    def __repr__(self):
        return "list {}".format(self.vals)
    
class Variable:
    def __init__(self, name, location, size):
        self.size = size
        self.name = name
        self.location = location
        
def flatten_functions(nested_list):
    flattened_list = []

    def flatten(item):
        if isinstance(item, list):
            for subitem in item:
                flatten(subitem)
        elif isinstance(item, tuple) and len(item) > 0 and item[0] == 'function':
            flattened_list.append(item)

    flatten(nested_list)
    return flattened_list



def runcode(k):
    global funcs
    global currentFuncVarVal
    global currentFunc
    global currentFuncVars
    global MPN
    global funcVarCount
    global currentFuncArgs
    global jumpStack
    global structs
    global currentFuncStructs
    global returnVals
    global method_name
    global library
    global currentLineNo
    
    p = None

    if type(k) == list and type(k[0]) == tuple and type(k[1]) == int:
        p = k[0]
        currentLineNo = k[1]
        
    else:
        p = k
    
    library = True
    if type(p) != tuple:
        if type(p) == int:
            asm("movl ${}, %ecx\n".format(p))
            return p
        elif type(p) == str:
            if p in currentFuncVars.keys():
                asm("movl {}, %ecx".format(currentFuncVars[p]))
            else:
                e_error(E_Error(currentLineNo, p, "Unknown variable : {}!".format(p)))
        elif type(p) == STR:
            x = p.value
            data["semblestr{}".format(MPN)] = ".asciz \"{}\"".format(x)
            asm("movl ${}, %ecx".format("semblestr{}".format(MPN)))
        
            if library:
                asm("pushl %ecx")
                asm("call String")
                asm("popl %ebx")
                asm("movl %eax, %ecx")
            
            MPN += 1
            return 'str'
        elif type(p) == Str_lit:
            x = p.v

            data["semblestr{}".format(MPN)] = ".asciz \"{}\"".format(x)
            asm("movl ${}, %ecx".format("semblestr{}".format(MPN)))
            MPN += 1
            return 'str'
        elif type(p) == Semble_List:
            vals = None
            if len(p.vals) == 1:
                vals = p.vals
            else:
                vals = listify(p.vals)
            j = len(vals)
            asm("pushl ${}".format(j*4))
            asm("call malloc")
            asm("popl %ebx")
            asm("movl %eax, %edx")
            
            for i in range(len(vals)):
                asm("pushl %edx")
                runcode(vals[i])
                asm("popl %edx")
                if i == 0:
                    asm("movl %ecx,(%edx)")
                else:
                    asm("movl %ecx, {}(%edx)".format((i) * 4))
            asm("movl %edx, %ecx")

            
            
    else:
        if p[0] == "function":
            funcs[p[1]] = []
            currentFunc = p[1]
            currentFuncVarVal = 0
            currentFuncVars = {}
            a = 8
            ini = False
            args = []
            if p[3] != []:
                args = listify(p[3])
            elif len(p[3]) == 1:
                args = p[3]
            else:
                args = []
            for arg in args:
                arg_name = arg.name
                if arg.type != None:
                    if arg.type not in structs:
                        e_error(E_Error(currentLineNo, arg.type, "Unknown data type {}!".format(arg.type)))
                    else:
                        currentFuncStructs[arg_name] = structs[arg.type]
                ini = True
                currentFuncVars[arg_name] = "{}(%ebp)".format(a)
                a += 4
            if ini:
                currentFuncArgs[currentFunc] = a - 4
            else:
                currentFuncArgs[currentFunc] = 8
            if method_name != None:
                currentFuncStructs["self"] = structs[method_name]
                method_name = None
            runcode(p[2])
            funcVarCount[p[1]] = currentFuncVarVal
            
            
        elif p[0] == "struct":
            name = p[1]
            members = p[2]
            
            if p[3] != []:
                
                methods = p[3]
                print(methods)
                print()
                if len(methods) > 1:
                    methods = flatten_functions(methods)
                    
                structs[p[1]] = Semble_Struct(p[1], members, methods)
                for method in methods:
                    print(method)
                    name = method[1]
                    m = list(method)
                    m[3] = [Argument("self")] + [method[3]]
                    method = tuple(m) 
                    method_name = p[1]
                    runcode(method)
                
            else:
                structs[p[1]] = Semble_Struct(p[1], members, [])
                
                    
            
        elif p[0] == "as":
            name = p[1]
            struct = p[2]
            if struct not in structs:
                e_error(E_Error(currentLineNo, struct, "Unknown structure {}!".format(struct)))
            currentFuncStructs[name] = structs[struct]
            
            
        
               
        
        elif p[0] == "fcall":
            n = 0
            if p[2] != []:
                args = listify(p[2])
            else:
                args = []

            for i in Reverse(args):
                n += 1
                if type(i) == int:
                    asm("pushl ${}\n".format(i))
                else:
                    runcode(i)
                    asm("pushl %ecx\n")
            asm("call {}".format(p[1]))
            for i in range(n):
                asm("popl %ebx")
        elif p[0] == "if":
            runcode(p[1])
            asm("cmpl $1, %ecx")
            a = MPN
            MPN += 1
            asm("jne .ne{}".format(a))
            runcode(p[2])

            asm(".ne{}:".format(a))
        elif p[0] == "ifelse":
            runcode(p[1])
            asm("cmpl $1, %ecx")
            v = MPN
            MPN += 1
            asm("jne .ne{}".format(v))
            
            runcode(p[2])
            a = MPN
            MPN += 1
            asm("jmp .ne{}".format(a))


            asm(".ne{}:".format(v))
            runcode(p[3])
            asm(".ne{}:".format(a))
            
        
            

            runcode(p[1])
            
        elif p[0] == "neg":
            runcode(p[1])
            asm("neg %ecx")
            
        elif p[0] == "break":
            if jumpStack != []:
                x = jumpStack[-1]

                asm("jmp {} # break".format(x))

                del jumpStack[-1]
            else:
                e_error(E_Error(currentLineNo, "break", "Break condition in non-loop!"))
        elif p[0] == "variableUpdate":
            if p[1] in currentFuncVars.keys():
                runcode(p[2])
                asm("movl %ecx, {}".format(currentFuncVars[p[1]]))
            else:
                e_error(E_Error(currentLineNo, p[1], "Variable {} referenced before assignment!".format(p[1])))
        
        elif p[0] == "indexedUpdate":
            if p[1] in currentFuncVars.keys():
                runcode(p[1])
                asm("pushl %ecx")
                runcode(p[2])
                asm("popl %edx")
                asm("movl %ecx, %ebx")

                asm("imull $4, %ebx")
                #asm("neg %ebx")
                asm("pushl %edx")
                asm("pushl %ebx")
                runcode(p[3])
                asm("popl %ebx")
                asm("popl %edx")
                asm("movl %ecx, (%ebx, %edx)")
                

            else:
                e_error(E_Error(currentLineNo, p[1], "Variable {} referenced before assignment!".format(p[1])))
        
        elif p[0] == "indexedUpdate8":
            if p[1] in currentFuncVars.keys():
                runcode(p[1])
                asm("pushl %ecx")
                runcode(p[2])
                asm("popl %edx")
                asm("movl %ecx, %ebx")

                #asm("neg %ebx")
                asm("pushl %edx")
                asm("pushl %ebx")
                runcode(p[3])
                asm("popl %ebx")
                asm("popl %edx")
                asm("movl %ecx, (%ebx, %edx)")
                

            else:
                e_error(E_Error(currentLineNo, p[1], "Variable {} referenced before assignment!".format(p[1])))
        
        elif p[0] == "attrUpdate":
            
            if p[1] in currentFuncVars.keys():
                if p[1] not in currentFuncStructs:
                    e_error(E_Error(currentLineNo, p[1], "{} is not a data structure!".format(p[1])))
                else:
                    if p[2] not in currentFuncStructs[p[1]].members:
                        e_error(E_Error(currentLineNo, p[2], "{} is not a member of struct {}!".format(p[2], currentFuncStructs[p[2]].name)))
                    else:
                        
                        runcode(p[3])
                        asm("pushl %ecx")
                        runcode(p[1])
                        asm("popl %edx")
                        asm("movl %edx, {}(%ecx)".format(currentFuncStructs[p[1]].members.index(p[2]) * 4))
            else:
                e_error(E_Error(currentLineNo, p[1], "Variable {} referenced before assignment!".format(p[1])))
                
        elif p[0] == "member":
            initial = p[1]
            membername = p[2]
            if initial not in currentFuncStructs:
                e_error(E_Error(currentLineNo, initial, "Variable {} attempted member access before assignment as a data structure!".format(initial)))
            else:
                if membername not in currentFuncStructs[initial].members:
                    
                    e_error(E_Error(currentLineNo, p[2], "{} is not a member of struct {}!".format(p[2], currentFuncStructs[initial].name)))
                else:
                    memberindex = currentFuncStructs[initial].members.index(membername)
                    runcode(p[1])
                    asm("movl %ecx, %edx")
                    asm("movl ${}, %ebx".format((memberindex) * 4))
                    #asm("neg %ebx")
                    asm("movl (%ebx, %edx), %ecx")
            

        elif p[0] == "statementc":
            runcode(p[1])
            
        elif p[0] == "index":
            runcode(p[1])
            asm("pushl %ecx")
            runcode(p[2])
            asm("popl %edx")
            asm("movl %ecx, %ebx")

            asm("imull $4, %ebx")
            #asm("neg %ebx")
            asm("movl (%edx, %ebx), %ecx")
        
        elif p[0] == "index8":
            runcode(p[1])
            asm("pushl %ecx")
            runcode(p[2])
            asm("popl %edx")
            asm("movl %ecx, %ebx")

            #asm("neg %ebx")
            asm("movl (%edx, %ebx), %ecx")
        
        elif p[0] == "for":
            runcode(p[2])
            runcode(p[1])
            asm("cmpl $1, %ecx")
            a = MPN
            MPN += 1
            asm("jne .ne{}".format(a))

            d = MPN
            MPN += 1
            asm(".ne{}:".format(d))
            jumpStack.append(".ne{}".format(a))
            
            runcode(p[4])


            runcode(p[3])
            runcode(p[1])
            asm("cmpl $1, %ecx")
            asm("jne .ne{}".format(a))
            asm("jmp .ne{}".format(d))
            asm(".ne{}:".format(a))

        elif p[0] == "while":
            runcode(p[1])
            asm("cmpl $1, %ecx")
            a = MPN
            MPN += 1
            asm("jne .ne{}".format(a))

            d = MPN
            MPN += 1
            asm(".ne{}:".format(d))
            jumpStack.append(".ne{}".format(a))
            
            runcode(p[2])



            runcode(p[1])
            asm("cmpl $1, %ecx")
            asm("jne .ne{}".format(a))
            asm("jmp .ne{}".format(d))
            asm(".ne{}:".format(a))
        elif p[0] == "statements":
            runcode(p[1])
            runcode(p[2])
            
        elif p[0] == "return":
            runcode(p[1])
            asm("movl %ecx, %eax")
            asm("jmp .l{}".format(currentFunc))
        
        
        elif p[0] == "variableDefinition":
            currentFuncVarVal += 4
            currentFuncVars[p[1]] = "-{}(%ebp)".format(currentFuncVarVal)
            if p[2] != None:
                runcode(p[2])
                asm("movl %ecx, {}".format(currentFuncVars[p[1]]))
        
        elif p[0] == "variableDefinitionOtherType":
            currentFuncVarVal += 4
            currentFuncVars[p[1]] = "-{}(%ebp)".format(currentFuncVarVal)
            vtype = p[3]
            if p[2] != None:
                runcode(p[2])
                asm("movl %ecx, {}".format(currentFuncVars[p[1]]))
            if vtype in structs:
                currentFuncStructs[p[1]] = structs[vtype]
            else:
                e_error(E_Error(currentLineNo, vtype, "Unknown type {}".format(vtype)))
            
        elif p[0] == "+":
            runcode(p[1])
            asm("movl %ecx, %edx")
            asm("pushl %edx")
            runcode(p[2])
            asm("popl %edx")
            asm("addl %ecx, %edx")
            asm("movl %edx, %ecx")
        
        elif p[0] == ">>":
            runcode(p[1])
            asm("movl %ecx, %edx")
            asm("pushl %edx")
            runcode(p[2])
            asm("popl %edx")
            asm("shrl %cl, %edx")
            asm("movl %edx, %ecx")
        
        elif p[0] == "<<":
            runcode(p[1])
            asm("movl %ecx, %edx")
            asm("pushl %edx")
            runcode(p[2])
            asm("popl %edx")
            asm("sall %cl, %edx")
            asm("movl %edx, %ecx")

        elif p[0] == "-":
            runcode(p[1])
            asm("movl %ecx, %edx")
            asm("pushl %edx")
            runcode(p[2])
            asm("popl %edx")
            asm("subl %ecx, %edx")
            asm("movl %edx, %ecx")

        elif p[0] == "*":
            runcode(p[1])
            asm("pushl %ecx")
            runcode(p[2])
            asm("popl %edx")
            asm("imull %ecx, %edx")
            asm("movl %edx, %ecx")
            
        elif p[0] == "member_call_statement":
            n = 0
            if p[1] not in currentFuncStructs:
                e_error(E_Error(currentLineNo, p[1], "{} is not a struct".format(p[1])))
            structName = currentFuncStructs[p[1]]
            
            if p[2] not in extractFunctionNames(structName.methods):
                
                e_error(E_Error(currentLineNo, p[2], "{} is not a method of struct {}".format(p[2], structName.name)))
            
            if p[3] != []:
                args = listify(p[3])
            else:
                args = []
            
            args = [p[1]] + args
            for i in Reverse(args):
                n += 1
                if type(i) == int:
                    asm("pushl ${}\n".format(i))
                else:
                    runcode(i)
                    asm("pushl %ecx\n")
            asm("call {}".format(p[2]))
            for i in range(n):
                asm("popl %ebx")
            
        elif p[0] == "freturn":
            if p[2] not in structs.keys():
                e_error(E_Error(currentLineNo, p[2], "Unknown data type {}".format(p[2])))
            else:
                runcode(p[1])
                returnVals[p[1][1]] = p[2]
        
        elif p[0] == "efcall":
            n = 0
            if p[2] != []:
                args = listify(p[2])
            else:
                args = []
            #asm("pusha")
            for i in Reverse(args):
                n += 1
                if type(i) == int:
                    asm("pushl ${}\n".format(i))
                else:
                    runcode(i)
                    asm("pushl %ecx\n")
            asm("call {}".format(p[1]))
            for i in range(n):
                asm("popl %ebx")
            #asm("popa")
            asm("movl %eax, %ecx")
        
        
        elif p[0] == "member_call":
            n = 0
            if p[1] not in currentFuncStructs:
                e_error(E_Error(currentLineNo, p[1], "{} is not a struct".format(p[1])))
            structName = currentFuncStructs[p[1]]
            print(structName.methods)
            if p[2] not in extractFunctionNames(structName.methods):
                print(extractFunctionNames(structName.methods))
                e_error(E_Error(currentLineNo, p[2], "{} is not a method of struct {}".format(p[2], structName.name)))
            
            if p[3] != []:
                args = listify(p[3])
            else:
                args = []
            
            args = [p[1]] + args
            for i in Reverse(args):
                n += 1
                if type(i) == int:
                    asm("pushl ${}\n".format(i))
                else:
                    runcode(i)
                    asm("pushl %ecx\n")
            asm("call {}".format(p[2]))
            for i in range(n):
                asm("popl %ebx")
            asm("movl %eax, %ecx")
        elif p[0] == "&&":
            #print(p)
            runcode(p[1])
            asm("movl %ecx, %edx")
            asm("pushl %edx")
            runcode(p[2])
            asm("popl %edx")
            asm("cmpl $1, %edx")
            v = MPN
            MPN += 1
            asm("jne .ne{}".format(v))
            asm("cmpl $1, %ecx")
            asm("jne .ne{}".format(v))
            asm("jmp .ne{}".format(MPN))
            asm(".ne{}:".format(v))
            asm("movl $0, %ecx")
            asm(".ne{}:".format(MPN))
            MPN += 1
        elif p[0] == "||":
            #print(p)
            runcode(p[1])
            asm("movl %ecx, %edx")
            asm("pushl %edx")
            runcode(p[2])
            asm("popl %edx")
            asm("cmpl $1, %edx")
            v = MPN
            MPN += 1
            a = MPN
            MPN += 1
            asm("je .ne{}".format(a))
            asm("cmpl $1, %ecx")
            asm("je .ne{}".format(a))
            asm("movl $0, %ecx")
            asm("jmp .ne{}".format(v))
            asm(".ne{}:".format(a))
            asm("movl $1, %ecx")
            asm(".ne{}:".format(v))
            MPN += 1
        elif p[0] == "|":
            if type(p[1]) == int and type(p[2]) == int:
                asm("movl ${}, %ecx".format(p[1]))
                asm("orl ${}, %ecx".format(p[2]))
            else:
                runcode(p[1])
                asm("movl %ecx, %edx")
                asm("pushl %edx")
                runcode(p[2])
                asm("popl %edx")
                asm("orl %ecx, %edx")
                asm("movl %edx, %ecx")

        elif p[0] == "==":
            runcode(p[1])
            asm("movl %ecx, %edx")
            asm("pushl %edx")
            runcode(p[2])
            asm("popl %edx")
            asm("cmpl %ecx, %edx")
            asm("jne .ne{}".format(MPN))
            v = MPN
            MPN += 1
            asm("movl $1, %ecx")
            asm("jmp .ne{}".format(MPN))
            asm(".ne{}:".format(v))
            asm("movl $0, %ecx")
            asm(".ne{}:".format(MPN))
            MPN += 1
        elif p[0] == "!=":
            runcode(p[1])
            asm("movl %ecx, %edx")
            asm("pushl %edx")
            runcode (p[2])
            asm("popl %edx")
            asm("cmpl %ecx, %edx")
            asm("je .ne{}".format(MPN))
            v = MPN
            MPN += 1
            asm("movl $1, %ecx")
            asm("jmp .ne{}".format(MPN))
            asm(".ne{}:".format(v))
            asm("movl $0, %ecx")
            asm(".ne{}:".format(MPN))
            MPN += 1

        elif p[0] == ">":
            runcode(p[1])
            asm("movl %ecx, %edx")
            asm("pushl %edx")
            runcode(p[2])
            asm("popl %edx")
            asm("cmpl %ecx, %edx")
            asm("jle .ne{}".format(MPN))
            v = MPN
            MPN += 1
            asm("movl $1, %ecx")
            asm("jmp .ne{}".format(MPN))
            asm(".ne{}:".format(v))
            asm("movl $0, %ecx")
            asm(".ne{}:".format(MPN))
            MPN += 1

        elif p[0] == ">=" or p[0] == "=>": 
            runcode(p[1])
            asm("movl %ecx, %edx")
            asm("pushl %edx")
            runcode(p[2])
            asm("popl %edx")
            asm("cmpl %ecx, %edx")
            asm("jl .ne{}".format(MPN))
            v = MPN
            MPN += 1
            asm("movl $1, %ecx")
            asm("jmp .ne{}".format(MPN))
            asm(".ne{}:".format(v))
            asm("movl $0, %ecx")
            asm(".ne{}:".format(MPN))
            MPN += 1

        elif p[0] == "<":
            runcode(p[1])
            asm("movl %ecx, %edx")
            asm("pushl %edx")
            runcode(p[2])
            asm("popl %edx")
            asm("cmpl %ecx, %edx")
            asm("jge .ne{}".format(MPN))
            v = MPN
            MPN += 1
            asm("movl $1, %ecx")
            asm("jmp .ne{}".format(MPN))
            asm(".ne{}:".format(v))
            asm("movl $0, %ecx")
            asm(".ne{}:".format(MPN))
            MPN += 1


        elif p[0] == "<=" or p[0] == "=<": 
            runcode(p[1])
            asm("movl %ecx, %edx")
            asm("pushl %edx")
            runcode(p[2])
            asm("popl %edx")
            asm("cmpl %ecx, %edx")
            asm("jg .ne{}".format(MPN))
            v = MPN
            MPN += 1
            asm("movl $1, %ecx")
            asm("jmp .ne{}".format(MPN))
            asm(".ne{}:".format(v))
            asm("movl $0, %ecx")
            asm(".ne{}:".format(MPN))
            MPN += 1
            
        elif p[0] == "!":
            runcode(p[1])
            asm("notl %ecx")
            
        elif p[0] == "|":
            if type(p[1]) == int and type(p[2]) == int:
                asm("movl ${}, %ecx".format(p[1]))
                asm("orl ${}, %ecx".format(p[2]))
            else:
                runcode(p[1])
                asm("movl %ecx, %edx")
                asm("pushl %edx")
                runcode(p[2])
                asm("popl %edx")
                asm("orl %ecx, %edx")
                asm("movl %edx, %ecx")

        elif p[0] == "&":
            if type(p[1]) == int and type(p[2]) == int:
                asm("movl ${}, %ecx".format(p[1]))
                asm("andl ${}, %ecx".format(p[2]))
            else:
                runcode(p[1])
                asm("movl %ecx, %edx")
                asm("pushl %edx")
                runcode(p[2])
                asm("popl %edx")
                asm("andl %ecx, %edx")
                asm("movl %edx, %ecx")
        if p[0] == "++":
            if p[1] in currentFuncVars:
                asm("movl {}, %ecx".format(currentFuncVars[p[1]]))
                asm("incl {}".format(currentFuncVars[p[1]]))
            else:
                e_error(E_Error(currentLineNo, p[1], "Variable '{}' referenced before assignment!".format(p[1])))
        
        elif p[0] == "--":
            if p[1] in currentFuncVars:
                asm("movl {}, %ecx".format(currentFuncVars[p[1]]))
                asm("decl {}".format(currentFuncVars[p[1]]))
            else:
                e_error(E_Error(currentLineNo, p[1], "Variable '{}' referenced before assignment!".format(p[1])))

        elif p[0] == "++s":
            if p[1] in currentFuncVars:
                asm("incl {}".format(currentFuncVars[p[1]]))
            else:
                e_error(E_Error(currentLineNo, p[1], "Variable '{}' referenced before assignment!".format(p[1])))

        elif p[0] == "--s":
            if p[1] in currentFuncVars:
                asm("decl {}".format(currentFuncVars[p[1]]))
            else:
                e_error(E_Error(currentLineNo, p[1], "Variable '{}' referenced before assignment!".format(p[1])))

        elif p[0] == "++p":
            if p[1] in currentFuncVars:
                asm("incl {}".format(currentFuncVars[p[1]]))
                asm("movl {}, %ecx".format(currentFuncVars[p[1]]))
            else:
                e_error(E_Error(currentLineNo, p[1], "Variable '{}' referenced before assignment!".format(p[1])))

        elif p[0] == "--p":
            if p[1] in currentFuncVars:
                asm("decl {}".format(currentFuncVars[p[1]]))
                asm("movl {}, %ecx".format(currentFuncVars[p[1]]))
            else:
                e_error(E_Error(currentLineNo, p[1], "Variable '{}' referenced before assignment!".format(p[1])))

            
def line_wrap(str, width=25):
    strs = []
    nstr = ""
    for i in str:
        nstr = nstr + i
        if len(nstr) == width:
            strs.append(nstr)
            nstr = ""
    if nstr != "":
        strs.append(nstr)
    return strs               
imports = []
macros = {}



def add_file_entry(filepath):
    global files_structure
    lines = 0
    with open(filepath, 'r') as fp:
        lines = sum(1 for line in fp)
    for line in range(lines):
        files_structure.append(filepath)

home = os.getenv("HOME")

def preprocess(file_path, library_folder=home+"/.semble/Semble/lib"):
    global imports, macros, files_structure
    codeStr = ""
    with open(file_path, "r") as file:
        for line in file:
            files_structure.append(file_path)
            # Handle imports
            if line.startswith("#import"):
                
                line = line.replace("\n", "")
                l = line.replace("#import ", "")
                if l not in imports:
                    codeStr += "\n " + preprocess(os.path.join(library_folder, "{}.smb".format(l)))
                    imports.append(l)

            else:
                # Resolve macros
                for macro, value in macros.items():
                    line = line.replace(macro, value)
                line = line.strip().replace("\t", "") + "\n"
                codeStr += line + " "
    return codeStr

class STR:
    def __init__(self, value):
        self.value = value
    def __repr__(self):
        return "<Str = {}>".format(self.value)
    def __str__(self):
        return "<Str = {}>".format(self.value)


parser = yacc.yacc()



codeStr = preprocess(file)
parser.parse(codeStr)


def cmpf():
    global funcs, globs, data
    with open("main.s", "w") as file:
        file.write("\n.section .data\n\n\n")
        for i in data:
            file.write('\n{}:'.format(i))
            file.write("\n\t{}".format(data[i]))
        
        file.write("\n\n\n.section .text\n\n\n")
        for func in funcs:
            if funcs[func] == []:
                continue 
            if func not in ['_start']:
                file.write("\n.type {}, @function".format(func))
            file.write("\n" + func + ":")
            if func not in ['_start']:
                file.write("\n\tpushl %ebp")
                file.write("\n\tmovl %esp, %ebp")
                if funcVarCount[func] != 0:
                   file.write("\n\tsubl ${}, %esp".format(funcVarCount[func]))
            for line in funcs[func]:
                file.write("\n\t" + line)
            if func not in ['_start']:
                file.write("\n\t.l{}:".format(func))
                file.write("\n\tmovl %ebp, %esp\n\tpopl %ebp") 
                file.write("\n\tret")
            file.write("\n")
            
        file.write("\n\t.globl _start")
        file.write("\n")
    os.system("as --32 main.s -o main.o")
    os.system("ld -m elf_i386 -dynamic-linker /lib/ld-linux.so.2 main.o -o {} -lc && rm main.o".format(OUTPUT_FILE))

cmpf()
