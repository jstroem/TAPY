class C(object):
	x = 10

def __getattr__(self, name):
	return 20

if (__BooleanLattice_Abstract__):
	C.__getattr__ = __getattr__

ten = C().x