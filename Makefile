kc: kc.cxx
	g++ $^ `llvm-config --cxxflags --ldflags --system-libs --libs core orcjit native` -Xlinker --export-dynamic -o $@
