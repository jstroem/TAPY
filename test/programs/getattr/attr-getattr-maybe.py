class C(object):
	pass

def __getattr__(self, name):
	return 10.0

if (__BooleanLattice_Abstract__):
	C.__getattr__ = __getattr__

if (__BooleanLattice_Abstract__):
	C.x = 10

try:
	tens = C().x
except:
	tens = 10+0j