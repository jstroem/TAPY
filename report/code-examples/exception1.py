class C(object):
	if (__BooleanLattice_Abstract__):
		a = 42

x = C()
def foo():
	result = x.a
	return result

try:
	y = foo()
except:
	err = "An error occured"