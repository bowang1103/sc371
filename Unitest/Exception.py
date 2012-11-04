def test() :
	try :
		1
	except ZeroDivisinError as e :
		2
		raise TypeError("foo")
	else :
		3
	finally :
		4
