import re
import ply.lex as lex
import ply.yacc as yacc

# Tokens para Smalltalk
tokens = [
    # Identificadores
    'IDENTIFIER', 'KEYWORD', 'BINARY_SELECTOR', 'UNARY_SELECTOR',
    
    # Literais
    'NUMBER', 'STRING', 'CHARACTER', 'SYMBOL',
    
    # Delimitadores
    'LPAREN', 'RPAREN', 'LBRACKET', 'RBRACKET', 'LBRACE', 'RBRACE',
    'PERIOD', 'CARET', 'PIPE', 'COLON', 'ASSIGN', 'SEMICOLON',
    
    # Comentários
    'COMMENT'
]

# Tokens simples
t_LPAREN = r'\('
t_RPAREN = r'\)'
t_LBRACKET = r'\['
t_RBRACKET = r'\]'
t_LBRACE = r'\{'
t_RBRACE = r'\}'
t_PERIOD = r'\.'
t_CARET = r'\^'
t_PIPE = r'\|'
t_COLON = r':'
t_ASSIGN = r':='
t_SEMICOLON = r';'

# Ignorar espaços em branco
t_ignore = ' \t\r'

def t_IDENTIFIER(t):
    r'[a-zA-Z_][a-zA-Z_0-9]*'
    return t

def t_KEYWORD(t):
    r'[a-zA-Z_][a-zA-Z_0-9]*:'
    return t

def t_BINARY_SELECTOR(t):
    r'[-+*/~=<>&!?@%|]+'
    return t

def t_NUMBER(t):
    r'-?\d+(\.\d+)?'
    if '.' in t.value:
        t.value = float(t.value)
    else:
        t.value = int(t.value)
    return t

def t_STRING(t):
    r"'([^'\\]|\\.)*'"
    t.value = t.value[1:-1]  # Remove as aspas simples
    return t

def t_CHARACTER(t):
    r'\$[^\\]'
    t.value = t.value[1]  # Remove o $
    return t

def t_SYMBOL(t):
    r'#([a-zA-Z_][a-zA-Z_0-9]*|[-+*/~=<>&!?@%|]+)'
    t.value = t.value[1:]  # Remove o #
    return t

def t_COMMENT(t):
    r'\"[^\"]*\"'
    t.value = t.value[1:-1]  # Remove as aspas duplas
    return t

def t_newline(t):
    r'\n+'
    t.lexer.lineno += len(t.value)

def t_error(t):
    print(f"Caractere ilegal '{t.value[0]}' na linha {t.lineno}")
    t.lexer.skip(1)

# ANÁLISE SINTÁTICA
def p_program(p):
    '''program : statements'''
    p[0] = ('program', p[1])
    print("✅ Análise sintática concluída com sucesso!")

def p_statements(p):
    '''statements : statement
                 | statement PERIOD statements'''
    if len(p) == 2:
        p[0] = [p[1]]
    else:
        p[0] = [p[1]] + p[3]

def p_statement(p):
    '''statement : expression
                | assignment
                | return_statement
                | block
                | declaration'''
    p[0] = p[1]

def p_declaration(p):
    '''declaration : PIPE temporaries PIPE'''
    p[0] = ('declaration', p[2])

def p_temporaries(p):
    '''temporaries : IDENTIFIER
                  | IDENTIFIER temporaries'''
    if len(p) == 2:
        p[0] = [p[1]]
    else:
        p[0] = [p[1]] + p[2]

def p_assignment(p):
    '''assignment : IDENTIFIER ASSIGN expression'''
    p[0] = ('assignment', p[1], p[3])

def p_return_statement(p):
    '''return_statement : CARET expression'''
    p[0] = ('return', p[2])

def p_expression(p):
    '''expression : primary
                 | message_expression
                 | cascade_message'''
    p[0] = p[1]

def p_primary(p):
    '''primary : literal
              | variable
              | block
              | parenthesized_expression'''
    p[0] = p[1]

def p_literal(p):
    '''literal : NUMBER
              | STRING
              | CHARACTER
              | SYMBOL
              | array
              | dictionary'''
    p[0] = ('literal', p[1])

def p_variable(p):
    '''variable : IDENTIFIER'''
    p[0] = ('variable', p[1])

def p_parenthesized_expression(p):
    '''parenthesized_expression : LPAREN expression RPAREN'''
    p[0] = p[2]

def p_block(p):
    '''block : LBRACKET block_parameters block_body RBRACKET'''
    p[0] = ('block', p[2], p[3])

def p_block_parameters(p):
    '''block_parameters : COLON IDENTIFIER block_parameters
                       | empty'''
    if len(p) == 4:
        p[0] = [p[2]] + p[3]
    else:
        p[0] = []

def p_block_body(p):
    '''block_body : statements
                  | CARET expression'''
    if len(p) == 2:
        p[0] = ('statements', p[1])
    else:
        p[0] = ('return', p[2])

def p_array(p):
    '''array : LBRACE expressions RBRACE'''
    p[0] = ('array', p[2])

def p_expressions(p):
    '''expressions : expression
                  | expression PERIOD expressions'''
    if len(p) == 2:
        p[0] = [p[1]]
    else:
        p[0] = [p[1]] + p[3]

def p_message_expression(p):
    '''message_expression : receiver unary_message
                         | receiver binary_message
                         | receiver keyword_message'''
    p[0] = ('message', p[1], p[2])

def p_cascade_message(p):
    '''cascade_message : message_expression SEMICOLON unary_message
                      | message_expression SEMICOLON binary_message
                      | message_expression SEMICOLON keyword_message'''
    p[0] = ('cascade', p[1], p[3])

def p_receiver(p):
    '''receiver : primary'''
    p[0] = p[1]

def p_unary_message(p):
    '''unary_message : IDENTIFIER'''
    p[0] = ('unary', p[1])

def p_binary_message(p):
    '''binary_message : BINARY_SELECTOR primary'''
    p[0] = ('binary', p[1], p[2])

def p_keyword_message(p):
    '''keyword_message : KEYWORD primary
                      | KEYWORD primary keyword_message'''
    if len(p) == 3:
        p[0] = [('keyword', p[1], p[2])]
    else:
        p[0] = [('keyword', p[1], p[2])] + p[3]

def p_empty(p):
    'empty :'
    p[0] = None

def p_error(p):
    if p:
        print(f"❌ Erro sintático na linha {p.lineno}: Token inesperado '{p.value}'")
    else:
        print("❌ Erro sintático: Fim inesperado do arquivo")

# Construindo os analisadores
lexer = lex.lex()
parser = yacc.yacc()

# Função para testar o analisador
def test_smalltalk_analyzer(code):
    print("=" * 60)
    print("📝 CÓDIGO SMALLTALK:")
    print(code)
    print("=" * 60)
    
    # Análise léxica
    lexer.input(code)
    print("🔍 TOKENS RECONHECIDOS:")
    tokens_list = []
    while True:
        tok = lexer.token()
        if not tok:
            break
        tokens_list.append(tok)
        print(f"  {tok}")
    
    # Análise sintática
    print("\n📊 ANÁLISE SINTÁTICA:")
    try:
        result = parser.parse(code)
        print("✅ Estrutura sintática válida!")
        print(f"Árvore sintática: {result}")
        return result
    except Exception as e:
        print(f"❌ Erro na análise: {e}")
        return None

def main():
    print("🔧 ANALISADOR LÉXICO/SINTÁTICO SMALLTALK")
    print("=" * 50)
    
    # Código de teste
    codigo_exemplo = """
    | x y resultado |
    x := 10.
    y := 20.
    resultado := x + y.
    ^ resultado
    """
    
    test_smalltalk_analyzer(codigo_exemplo)

if __name__ == "__main__":
    main()
