CXXFLAGS=-std=c++14 -W -Wall -pedantic -g

run_tests: main
	./main test

main:
