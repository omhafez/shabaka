# omhafez; Hafez, Omar
SOURCES = kinds_m.f data_obj_m.f sort_i.f rmv_dupe_i.f matrix_m.f data_types_m.f qvor2vtk.f
#list your source files here
TARGETS = qvor2vtk
OBJECTS = $(patsubst %.f,%.o,$(patsubst %.f,%.o,$(SOURCES)))

qvor2vtk: $(OBJECTS)
	gfortran -g -O3 -fbounds-check -ffree-form -fopenmp -Wall -o qvor2vtk $(OBJECTS)

# List header dependencies here in the following manner
# foo.o bar.o : foo.hh

kinds_m.o: kinds_m.f
	gfortran -g -O3 -fbounds-check -ffree-form -fopenmp -Wall -c kinds_m.f

data_obj_m.o: kinds_m.f data_obj_m.f
	gfortran -g -O3 -fbounds-check -ffree-form -fopenmp -c data_obj_m.f

sort_i.o: kinds_m.f data_obj_m.f sort_i.f
	gfortran -g -O3 -fbounds-check -fopenmp -Wall -c sort_i.f

rmv_dupe_i.o: kinds_m.f data_obj_m.f rmv_dupe_i.f
	gfortran -g -O3 -fbounds-check -fopenmp -Wall -c rmv_dupe_i.f

matrix_m.o: kinds_m.f matrix_m.f
	gfortran -g -O3 -fbounds-check -ffree-form -fopenmp -Wall -c matrix_m.f

data_types_m.o: kinds_m.f data_types_m.f
	gfortran -g -O3 -fbounds-check -ffree-form -fopenmp -Wall -c data_types_m.f

qvor2vtk.o: qvor2vtk.f matrix_m.f sort_i.f data_obj_m.f rmv_dupe_i.f kinds_m.f data_types_m.f
	gfortran -g -O3 -fbounds-check -ffree-form -fopenmp -Wall -c qvor2vtk.f

.PHONY: clean 
clean:
	rm -f $(TARGETS) $(OBJECTS) *.mod
