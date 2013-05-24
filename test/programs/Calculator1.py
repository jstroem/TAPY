def dunno(b):
	return b

dunno(True)
a = dunno(False)

try:
	x = 10
	if (a):
		b = 'b'
		raise Exception()
	else:
		c = 'c'
except:
	y = 10.0
else:
	z = 15

exc = __Analysis_Register_EXCEPTION__