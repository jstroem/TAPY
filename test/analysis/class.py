class C(object):
	def __init__(self, x):
		self.x = x

	def setX(self, y):
		self.x = y

	def getX(self):
		return self.x

c = C(10)
x = c.getX()