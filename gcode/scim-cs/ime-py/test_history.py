import pickle, OrderedSet

file_ = open("/home/bhj/.sdim/history/wqvb", "rb")
dood = pickle.load(file_)
print(repr(dood))
