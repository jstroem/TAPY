class Student(object):
    def __getattr__(self, name):
        if name in self.grades:
            return self.grades[name]
        else:
            raise AttributeError("'Student' object has no attribute '"  + name + "'")

a = Student()
a.grades = { 'math': 'A' }
mathgrade = a.math