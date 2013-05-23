True = __BooleanLattice_Concrete_TRUE__
False = __BooleanLattice_Concrete_FALSE__

None = __NoneLattice_Top__

class BaseException(object): pass
class Exception(BaseException): pass
class ArithmeticError(Exception): pass
class ZeroDivisionError(ArithmeticError): pass