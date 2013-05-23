True = __BooleanLattice_Concrete_TRUE__
False = __BooleanLattice_Concrete_FALSE__

class BaseException(object): pass
class Exception(BaseException): pass
class ArithmeticError(Exception): pass
class ZeroDivisionError(ArithmeticError): pass

def raw_input():
	return __StringLattice_Abstract__
