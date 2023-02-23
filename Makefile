kc: kc.cxx
	g++ $^ `llvm-config --cxxflags` -o $@ `llvm-config --link-shared --libs`
