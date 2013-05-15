x = 1

def foo():
	if (True):
		return x

def bar():
	if (True):
		return x
	else:
		return x

def baz():
	return x

z = foo()