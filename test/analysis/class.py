class C(object):
	def __init__(self):
		pass

def f():
	if (True):
		local = 10
	else:
		local = 10.0
	return local

C.f = f