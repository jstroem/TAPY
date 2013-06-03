class C(object):
	def __getattr__(self, name):
		return 20.0

c = C()

#if (__BooleanLattice_Abstract__):
#	c.x = 10

x = c.x