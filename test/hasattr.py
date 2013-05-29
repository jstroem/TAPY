class C(object):
	def __getattribute__(self, name):
		print "__getattribute__"
		raise AttributeError("Error")
	def __getattr__(self, name):
		print "__getattr__"
		return 10

c = C()
print c.x