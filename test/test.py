class C(object):
	a = 10

def foo(x):
	def bar(y):
		return x+y
	return bar

x = foo(1)
y = x(2)