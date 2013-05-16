x = object

class A(object):
	pass

class A1(x):
	pass

class B(A):
	x = 10

	def __init__(self):
		x = 20

	def setX(self, x):
		self.x = x

	def getX(self):
		return self.x

b = B()