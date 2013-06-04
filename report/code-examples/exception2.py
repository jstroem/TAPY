class C(object):
	if (__BooleanLattice_Abstract__):
		a = 42

x = C()
try:
	y = x.a
	z = "trickyComputation() was true"
	__Analysis_Dump_Test__
except:
	err = "An error occured"