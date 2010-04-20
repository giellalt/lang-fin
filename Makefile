# ******************************************************************** #
# This is a makefile for building the omorfi Finnish binaries, using   #
# the svn externals defined in this directory. The compiled binaries   #
# are installed in the bin/ subdirectory.                              #
#                                                                      #
# First-time usage:                                                    #
# run the target 'first-time', ie:                                     #
#                                                                      #
#  make first-time                                                     #
#                                                                      #
# Later it is enough to just run 'make'                                #
#                                                                      #
# ******************************************************************** #

all: omorfi

omorfi:
	@echo "***                              ***"
	@echo "*** Making and installing Omorfi ***"
	@echo "***                              ***"
	cd omorfi && make && make install

first-time:
	@echo "***                                           ***"
	@echo "*** Configuring, making and installing Omorfi ***"
	@echo "***                                           ***"
	cd omorfi && ./autogen.sh
	cd omorfi && ./configure --prefix=$(GTHOME)/kt/fin/bin/ && make && make install
