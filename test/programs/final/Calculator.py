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
		raise IllegalOperatorException("Illegal operator")
	return result

a = 10
b = 20

try:
	aplusb = calculator(a, "+", b)
	aminusb  = calculator(a, "-", b)
	adiv0 = calculator(a, "/", 0)
	amodb = calculator(a, "%", b)
except ZeroDivisionError:
	err = "Division by zero"
except IllegalOperatorException as e:
	err = e.msg