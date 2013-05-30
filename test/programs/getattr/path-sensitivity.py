class C(object):
	def __getattr__(self, name):
		return 10.0

if (__BooleanLattice_Abstract__):
	C.x = 10

c = C()

try:
	tens = c.x
except:
	# C definately has __getattr__, so tens
	# should be undefined, 10, 10.0 at the exit node
	tens = 42l