x = 10

def foo():
	x = 20
	if (True):
		global x
	else:
		pass

foo()
x # 20