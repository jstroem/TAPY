class C(object):
	def __getattr__(self, name):
		return 10
try:
	ten = C().x
except:
	ten = 42l