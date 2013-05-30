class C(object):
	def __getattr__(self, name):
		return 10

ten = C().x