class Eventify(object):

	def trigger(self, e, args):
		if (e in self.events):
			i = 0
			while i < len(self.events[e]):
				self.events[e][i](args)
				i += 1

	def on(self, e, func):
		if (e in self.events):
			self.events[e].append(func)
		else:
			self.events[e] = [func]

	def off(self, e, func = None):
		if (e in self.events):
			if (func is None):
				self.events[e] = []
			else:
				self.events[e].remove(func)

	def __init__(self, obj):
		self.events = {}
		obj.trigger = self.trigger
		obj.off = self.off
		obj.on = self.on

##Tester code##
class A(object):
	pass

x = A()
Eventify(x)

def onC(args):
	print "onC", args

def onB(args):
	print "onB", args

x.on("B", onB)
x.on("C", onC)

x.trigger("B", [1,2])
x.trigger("C", [1,2,3])

x.off("B", onB)
x.trigger("B", [1,2,3,4])

x.off("C")
x.trigger("C", [1,2,3,4,5])