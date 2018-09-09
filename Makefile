CXXFLAGS=-std=c++14 -W -Wall -Wundef -pedantic -g -MD -MP

run_tests: main_test.pass deepening_test.pass

main: main.o
	$(CXX) $(CXXFLAGS) -o $@ $^

deepening_test: deepening_test.o
	$(CXX) $(CXXFLAGS) -o $@ $^

main_test.pass: main
	./main test
	touch $@

deepening_test.pass: deepening_test
	./deepening_test
	touch $@

-include *.d
