class Person(object):
    def __init__(self, name):
        self.setName(name)
    def getName(self):
        return self.name

def setName(self, name):
    self.name = name

Person.setName = setName

class Student(Person):
    def __init__(self, name, sid):
        super(Student, self).__init__(name)
        self.sid = sid
        self.setGrades({})
    def __getattr__(self, name):
        if name in self.grades:
            return self.grades[name]
        else:
            raise AttributeError("'Student' object has no attribute '"  + name + "'")
    def getGrades(self):
    	return self.grades
    def setGrades(self, grades):
        self.grades = grades

sid = 20130000
a = Student("John Doe", sid)

# does y have a setName method at this program point?
a.setName("John Q. Doe")
a.setGrades({'math': 'A'})

b = a.getName()
c = a.math