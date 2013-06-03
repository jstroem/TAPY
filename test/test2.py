class C(object):
	def __init__(self):
		#self.b = self.a(1)
		pass

	def a(self, x):
		def b(y):
			return x+y
		return b

c = C()
d = c.a(10)