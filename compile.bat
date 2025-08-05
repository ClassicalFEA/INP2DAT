gfortran -c INP2DAT.f90
gfortran -c OPNERR.f90
gfortran -c READERR.f90
gfortran -c GET_ELEM_CONNECT_DATA.f90
gfortran -c WRT_DATE_TIME_INFILE.f90
gfortran INP2DAT.o OPNERR.o WRT_DATE_TIME_INFILE.o READERR.o GET_ELEM_CONNECT_DATA.o -o inp2dat.exe