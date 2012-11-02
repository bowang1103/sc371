class student():
	counter = 0
	def __init__(self,n,a):
		"""docstring for fname"""
		self.name = n
		self.age = a
	def get_age(self):
		return self.age
		
f = student("Danny", "23")
print (f.__class__.counter)