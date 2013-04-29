try:
	pass
except Foo:
	print "b"
except Foo as Bar:
	print "c"
except:
	print "d"
else:
	print "e"