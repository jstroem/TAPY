class C():
	x = 10

	def __init__(self):
		x = 20

	def setX(self, x):
		self.x = x

	def getX(self):
		return self.x

c = C()