class C(object):
	def __getattr__(self, name):
		return 20

if (__BooleanLattice_Abstract__):
	C.x = 10

c = C()
x = c.x