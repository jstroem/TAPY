def greet(name):
    print 'Hello', name
greet('Jack')
greet(name='Jack')
x = ['Jack']
greet(*x)
y = {'name': 'Jack'}
greet(**y)