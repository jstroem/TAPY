#class X(object): pass
#class Y(object): pass
#class A(X,Y): pass
#class B(Y,X): pass
#class C(A, B):
#	x = y # should return type error

class G(object):
	pass

class F(object):
	pass

if (True):
	GF = F
else:
	GF = G

class E(object):
	pass

class D(object):
	pass

class C(D,GF):
	pass

class B(D,E):
	pass

class A(B,C):
	x = y # should return A B C D E F O