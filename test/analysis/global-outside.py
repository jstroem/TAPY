x = 10

def foo():
	global x
	def bar():
		x = 20
	bar()

foo()
x # 10