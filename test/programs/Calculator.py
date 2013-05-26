class IllegalOperatorException(Exception):
	def __init__(self, msg):
		self.msg = msg

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

a = 10 # float(raw_input("First number: "))
b = 20 # float(raw_input("Second number: "))
op = "+" # raw_input("Operator: ")

#aplusb = calculator(a, "+", b)
#atimesb = calculator(a, "*", b)

try:
	# TODO: When setting amodb to e.g. 10 before calling
	# calculator results in amodb being undefined (but
	# it should have been 10)
	amodb = calculator(a, "%", b)
	# __Analysis_Dump_try__
except:
	err = "An error occured"
	__Analysis_Dump_except__


__Analysis_Dump_exit__
#try:
#	result = calculator(a, op, b)
#except ZeroDivisionError:
#	err = "Division by zero"
#except IllegalOperatorException as e:
#	err = e.msg