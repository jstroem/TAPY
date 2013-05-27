#class IllegalOperatorException(Exception):
#	def __init__(self, msg):
#		self.msg = msg

def calculator(a, op, b):
	if (op == "+"):
		result = a + b
	elif (op == "-"):
		result = a - b
	elif (op == "*"):
		result = a * b
	elif (op == "/"):
		result = a / b
	else:
		# Variant 1:
		# result = "error"
		#
		# Variant 2:
		#try:
		#	raise IllegalOperatorException("Illegal operator")
		#except:
		#	result = "error"
		#
		# Variant 3:
		raise Exception() # IllegalOperatorException("Illegal operator")
	return result

try:
	# amodb  = 10
	aplusb = calculator(10, "+", 20)
	__Analysis_Dump_try1__
	aminusb  = calculator(10, "-", 20)
	__Analysis_Dump_try2__
except:
	err = "An error occured"
	__Analysis_Dump_except__

#try:
#	result = calculator(a, op, b)
#except ZeroDivisionError:
#	err = "Division by zero"
#except IllegalOperatorException as e:
#	err = e.msg