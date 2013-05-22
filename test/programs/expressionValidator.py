# Mathematical expression to validate
code = "[(((a+b)*c+d-e)/(f+g)-(r+j)*(k+e))]";

parentheses_open = ['(', '{', '[']
parentheses_close = [')', '}', ']']
    
def getParenthesesType(c):
    if c in parentheses_open:
        return parentheses_open.index(c)
    elif c in parentheses_close:
        return parentheses_close.index(c)
    else:
        return 0

def validateSyntax(x):
    size = len(x)
    s = []
    i = 0
    while i < size:
        if x[i] in parentheses_open:
            s.append(x[i])
        elif x[i] in parentheses_close:
            if len(s)==0:
                return 0
            if getParenthesesType(s.pop()) != getParenthesesType(x[i]):
                return 0
        i += 1
    if len(s)==0:
        return 1
    else:
        return 0

if validateSyntax(code):
    x = 1
else:
    x = 0