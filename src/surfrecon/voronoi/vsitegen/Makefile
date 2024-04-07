SOURCES = vsitegen.cpp #list your source files here
TARGETS = vsitegen
OBJECTS = $(patsubst %.c,%.o,$(patsubst %.cpp,%.o,$(SOURCES)))

UNAME_S := $(shell uname -s)
ifeq ($(UNAME_S), Linux)
	CC = g++ -std=c++11
endif
ifeq ($(UNAME_S), Darwin)
	CC = /opt/homebrew/bin/g++-13 -std=c++11
endif

vsitegen: $(OBJECTS)
	$(CC) -Wall -O3 -I /opt/homebrew/include -L /opt/homebrew/lib -o vsitegen $(OBJECTS) -lteem -lm -fopenmp

# List header dependencies here in the following manner
# foo.o bar.o : foo.hh

vsitegen.o: vsitegen.cpp
	$(CC) -Wall -O3 -I /opt/homebrew/include -L /opt/homebrew/lib -c vsitegen.cpp -fopenmp

.PHONY: clean 
clean:
	rm -rf $(TARGETS) $(OBJECTS) $(TARGETS).dSYM
