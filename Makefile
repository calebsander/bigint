CXX = g++
CPPFLAGS = -std=c++11 -Wall -g -O3 -I./

test: test-main
	./test-main

test-main: bigint.o test-main.o
	$(CXX) $(CPPFLAGS) $^ -o $@

clean:
	rm -f *.o test-main
