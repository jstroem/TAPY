class Eventify(object):

	def trigger(self, obj, e, args):
		if (e in self.events):
			i = 0
			while i < len(self.events[e]):
				self.events[e][i](obj, args)
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

	def triggerWrapper(self, obj):
		def triggerInner(e,args):
			return self.trigger(obj,e,args)
		return triggerInner

	def __init__(self,obj):
		self.events = {}
		obj.trigger = self.triggerWrapper(obj)
		obj.off = self.off
		obj.on = self.on

##Tester code##
class A(object):
	def __init__(self):
		pass

x = A()
y = Eventify(x)

y = 0

def onC(self,args):
	y = 10
	print "onC", self, args

def onB(self,args):
	y = 20
	print "onB", self, args

x.on("B", onB)
x.on("C", onC)

x.trigger("B", 1)
x.trigger("C", 2)

x.off("B", onB)
x.trigger("B", 3)

x.off("C")
x.trigger("C", 4)