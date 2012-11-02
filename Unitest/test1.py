def reraise():
    try:
        raise TypeError("foo")
    except:
	    try:
	    	raise KeyError("caught")
	    finally:
	    	print("test")
reraise()
#___assertRaises(KeyError, reraise)


#def reraise():
#    try:
#        raise TypeError("foo")
#    except:
#        try:
#            raise KeyError("caught")
#        finally:
#            raise