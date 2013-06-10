class Student(object):
    def __init__(self, name):
        self.name = name

    def __getattr__(self, name):
        if name in self.grades:
            return self.grades[name]
        else:
            raise AttributeError()

a = Student('John')
a.grades = { 'math': 'A' }
try:
	mathgrade = a.math
except:
	err = "An error occurred"