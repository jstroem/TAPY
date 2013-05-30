class C(object):
	x = 10

	def __getattr__(self, name):
		return 20

ten1 = C.x
ten2 = C().x