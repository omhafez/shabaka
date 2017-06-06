SOURCES = ptcloudgen.cpp relerrorfunctional.cpp classes.cpp #list your source files here
TARGETS = ptcloudgen
OBJECTS = $(patsubst %.c,%.o,$(patsubst %.cpp,%.o,$(SOURCES)))
UNAME_S := $(shell uname -s)
ifeq ($(UNAME_S), Linux)
	CC = g++ -std=c++11
endif
ifeq ($(UNAME_S), Darwin)
	CC = g++-7 -std=c++11
endif

ptcloudgen: $(OBJECTS)
	$(CC) -Wall -O3 -o ptcloudgen $(OBJECTS) -lteem -lnlopt -lm -fopenmp

# List header dependencies here in the following manner
# foo.o bar.o : foo.hh

ptcloudgen.o: ptcloudgen.cpp relerrorfunctional.h classes.h
	$(CC) -Wall -O3 -c ptcloudgen.cpp

relerrorfunctional.o: relerrorfunctional.cpp relerrorfunctional.h classes.h
	$(CC) -Wall -O3 -c relerrorfunctional.cpp

classes.o: classes.cpp classes.h relerrorfunctional.h
	$(CC) -Wunused-result -O3 -c classes.cpp -fopenmp

.PHONY: clean 
clean:
	rm -rf $(TARGETS) $(OBJECTS) $(TARGETS).dSYM
