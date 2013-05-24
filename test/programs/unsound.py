class C():
	pass

def foo():
	return C()

x = foo()
y = foo()
z = x == y