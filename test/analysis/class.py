x = object

class A(object):
	pass

class B(A):
	x = 10

	def __init__(self):
		x = 20

	def setX(self, x):
		self.x = x

	def getX(self):
		return self.x

class C():
	pass

class D(C):
	x = 10

	def __init__(self):
		x = 20

	def setX(self, x):
		self.x = x

	def getX(self):
		return self.x

if (True):
	class E(B):
		pass
else:
	class E(D):
		pass

# c = C()