class Person(object):
	def __init__(self, name):
		self.setName(name)

def setName(self, name):
	self.name = name

Person.setName = setName

class Student(Person):
	def __init__(self, name, sid):
		super(Student, self).__init__(name)
		self.sid = sid

sid = 20130000
x = Student("Joe Average", sid)
y = Student("John Doe", sid + 1)

# does y have a setName method at this program point?
y.setName("John Q. Doe")