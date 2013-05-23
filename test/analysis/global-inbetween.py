x = 10

def foo():
	x = 20
	def bar():
		global x
		x = 30
	bar()
	print x # 20

foo()
x # 30