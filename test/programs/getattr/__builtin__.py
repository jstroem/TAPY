## Primitive values
True = __BooleanLattice_Concrete_TRUE__
False = __BooleanLattice_Concrete_FALSE__
NotImplemented = __NotImplementedLattice_Concrete__
Ellipsis = __EllipsisLattice_Concrete__

#Built-in classes
class list(object):

	def append(self, x):
		self.__List_Elements__ = x

	def pop(i=0):
		return self.__List_Elements__

	def __getitem__(self, key):
		if (__BooleanLattice_Abstract__):
			raise IndexError("list index out of range")	
		else:
			return self.__List_Elements__

	def __setitem__(self, key, value):
		if (__BooleanLattice_Abstract__):
			self.__List_Elements__ = value
		else:
			raise TypeError("list indices must be integers")

class dict(object):
	def __getitem__(self, key):
		if (__BooleanLattice_Abstract__):
			raise KeyError("")
		else:
			return self.__Dict_Element_Values__

	def __setitem__(self, key, value):
		self.__Dict_Element_Values__ = value
		self.__Dict_Element_Keys__ = key

## Exceptions
class BaseException(object):
	def __init__(self, msg):
		self.message = msg


class Exception(BaseException): pass
class StopIteration(Exception): pass
class ArithmeticError(Exception): pass
class ZeroDivisionError(ArithmeticError): pass

class StandardError(Exception): pass
class LookupError(StandardError): pass
class AttributeError(StandardError): pass
class TypeError(StandardError): pass

class IndexError(LookupError):
	def __init__(self, msg):
		self.message = msg

class KeyError(LookupError):
	def __init__(self, msg):
		self.message = msg

## Built in functions
def raw_input():
	return __StringLattice_Abstract__

#Workround int should be a class
def int(el):
	return __IntegerLattice_Abstract__

def len(el):
	if (__BooleanLattice_Abstract__):
		return __IntegerLattice_Abstract__
	else:
		raise TypeError("object of type __ has no len()")