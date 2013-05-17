class C(object):
	x = 10
	y = None

	def foo(self, x):
		self.x = x

while (True):
	C()
	# a.foo(10.0)