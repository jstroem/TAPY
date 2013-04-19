def echo(value=None):
	print "Execution starts when 'next()' is called for the first time."
	try:
		while True:
			try:
				value = (yield value)
				yield value
			except GeneratorExit:
				# never catch GeneratorExit
				raise
			except Exception, e:
				value = e
	finally:
		print "Don't forget to clean up when 'close()' is called."
gen = echo()
print gen.next()
print gen.send(2)
print gen.throw(Exception, "test")
print gen.close()