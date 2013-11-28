UGrid_netCDFio
==============

The purpose of this project is to develop a generic method to read and write computational grid information using UGRID compliant netCDF. The intended application is the file transfer between grid generation programs and model preprocessors. A generic interface would allow mixing and matching various generation programs and models, and thus allow more flexibility in using different models. Moreover, it would also provide the ability to maintain grid archives that could be accessed easily by any model. 

The code here is based on UGRID compliant netCDF, version 0.9.0. The web link is https://github.com/ugrid-conventions/ugrid-conventions. However, the convention is not sufficiently specific to aritrarily write and read data in files without further name standardizations as noted below.

The project is organized into a /src directory that contains the f90 code to build the read-write module and driver program, a /build directory that contains the scripts to compile and link the program using gfortran, and a /bin directory that contains the executable and test files. The program was built with linux (openSUSE 12.2) and gfortran 4.7.

The read-write module contains subroutines to create and write a netCDF file, and to read the data size and data from this file. The UGRID recommendations are to attach the grid attributes to an arbitrary scalar. However, this make it difficult to automatically interrogate the file. Hence the grid attributes are made global using nf90_global. In addition, the lack of standard variable names makes it tedious to automatically discover the data. Hence, we have adopted the (hopefully) standard variable names of node_x, node_y, and node_z for the 3 coordinates. The standard name attribute follows UGRID and CF recommendations.

The other place where modification is necessary is the use of face_node_connectivity. For coastal and ocean models, the horizontal face (element or cell) is stored and treated differently than the vertical faces at the edge of each element. Hence we discriminate between these by using the variable names element_node_connectivity for the horizontal area and face_node_connectivity for the vertical faces. The standard name attribute remains as recommended in the UGRID specifications.

Finally, the netCDF file contains integer codes assigned to nodes and to elements. These codes are used to define boundary conditions (or other attributes). Here node_code is defined as
 0=no code, an interior point,
 1=land boundary on the exterior boundary,
 2=island boundary,
 3=open boundary,
 4=radiation boundary
 5=discharge (flux) boundary such as river discharge,
 6=weir.
Weir is open ended so that by specifying larger integer codes, the individual weirs can be identified. Element codes are 0=inactive, 1=active, and 9=flux. These are all arbitrary but
should be defined with the code attributes so they can be automatically identified.

The driver program reads a test file, writes a netCDF file, then reads the netCDF file and generates an output file that should be identical to the input file. Some models require a list of boundary segments their type. For this purpose, subroutines are included to create a node adjacency list and then extract the boundaries. 



