class C(object):
	x = 10

	def __getattr__(self, name):
		return 42.0

try:
	ten = C().x
except:
	ten = 42l