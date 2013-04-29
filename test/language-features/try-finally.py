try:
	10/0
except Foo, e: # equivalent to Foo as e
	print "a"
except Foo as e:
	print "b"
except ZeroDivisionError as (e1,e2):
	print "c"
except:
	print "d"
else: # Useful if the try clause does not raise an exception
	print "e"
print "f"
#finally:
#	print "f"