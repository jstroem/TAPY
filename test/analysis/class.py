class C(object):
	x = 10

	def setX(self, x):
		self.x = x

	def getX(self):
		return self.x

__Analysis_Dump_Here__

c = C()
x = c.getX()