CXXFLAGS=-std=c++11 -W -Wall -pedantic -g

run_tests: main
	./main test

main:
