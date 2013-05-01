try:
	1
	try:
		2
	except Foo:
		print "a"
	except Bar:
		print "b"
	else:
		print "c"
	finally:
		print "d"
except Bar:
	print "e"
else:
	print "f"
finally:
	print "g"

print "h"