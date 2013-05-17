class C(object):
	x = 10
	y = None

	def foo(self, x):
		self.x = x

a = C()
y = a.x
y = 20