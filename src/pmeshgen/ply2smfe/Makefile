# omhafez; Hafez, Omar
SOURCES = kinds_m.f data_obj_m.f sort_i.f ply2smfe.f
#list your source files here
TARGETS = ply2smfe
OBJECTS = $(patsubst %.f,%.o,$(patsubst %.f,%.o,$(SOURCES)))

ply2smfe: $(OBJECTS)
	gfortran -g -O3 -fbounds-check -ffree-form -fopenmp -Wall -o ply2smfe $(OBJECTS)

# List header dependencies here in the following manner
# foo.o bar.o : foo.hh

kinds_m.o: kinds_m.f
	gfortran -g -O3 -fbounds-check -ffree-form -Wall -c kinds_m.f

data_obj_m.o: kinds_m.f data_obj_m.f
	gfortran -g -O3 -fbounds-check -ffree-form -fopenmp -c data_obj_m.f

sort_i.o: kinds_m.f data_obj_m.f sort_i.f
	gfortran -g -O3 -fbounds-check -fopenmp -Wall -c sort_i.f

ply2smfe.o: ply2smfe.f sort_i.f data_obj_m.f kinds_m.f
	gfortran -g -O3 -fbounds-check -ffree-form -Wall -c ply2smfe.f

.PHONY: clean 
clean:
	rm -f $(TARGETS) $(OBJECTS) *.mod
