class A():
	pass

class C(object):
	def __init__(self):
		self.a = A()

	def getA(self):
		return self.a

c = C()
a = c.a