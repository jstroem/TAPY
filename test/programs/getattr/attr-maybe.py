class C(object):
	def __getattr__(self, name):
		return 10.0

if (__BooleanLattice_Abstract__):
	C.x = 10

try:
	tens = C().x
except:
	tens = 42l