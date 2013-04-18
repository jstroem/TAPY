def greet(firstname = '<no firstname>', lastname = '<no lastname>'):
    print 'Hello', firstname, lastname
greet('John', 'Doe')
greet('John', lastname='Jack')
x = ['John', 'Doe']
greet(*x)
y = {'lastname': 'Doe'}
greet('John', **y)