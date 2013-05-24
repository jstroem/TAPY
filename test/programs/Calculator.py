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
		try:
			raise IllegalOperatorException("Illegal operator")
		except:
			result = "error"
		#
		# Variant 3:
		# raise IllegalOperatorException("Illegal operator")
	return result

a = 10 # float(raw_input("First number: "))
b = 20 # float(raw_input("Second number: "))
op = "+" # raw_input("Operator: ")

#aplusb = calculator(a, "+", b)
#atimesb = calculator(a, "*", b)

amodb = calculator(a, "%", b)
x = __Analysis_Register_EXCEPTION__

#try:
#	result = calculator(a, op, b)
#except ZeroDivisionError:
#	err = "Division by zero"
#except IllegalOperatorException as e:
#	err = e.msg