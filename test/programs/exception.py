exc = Exception()

def foo(b):
	if (b):
		raise exc

foo(True)
foo(False)