exc = Exception()

def foo(b):
	if (b):
		raise exc
	return b

try:
	x = foo(False)
	y = foo(True)
	z = 10
	__Analysis_Dump_try__
except:
	err = "yes"