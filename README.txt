# README.txt - rout
#
# $Id: README.txt,v 1.1 2005/04/07 05:07:28 vicadmin Exp $
#

This version of the routing code requires the non-standard routine GETARG to read
command line arguments.  The makefile contains two sets of flags for two the systems
on which we have compiled the code.  Check your compiler documentation for the flags 
necessary to include this function on your system.

For running on plane, it was necessary to insert:  'integer IARGC' to typecast the function

NOTE:  the input stationlocation file has changed to allow the specification of the UH_S grid, which 
otherwise takes a long time to make.  A sample stationlocation file should now look like this:

1 CHARLESTON          67 35 -9999
/nfs/meter2/mnt/aww/ohio/route/run/CHARL.uh_s
1 ALDERSON            76 30 -9999
/nfs/meter2/mnt/aww/ohio/route/run/ALDER.uh_s
1 BLUESTONE           73 29 -9999
NONE


The change is that the path to the .uh_s array file is specified after each line that has the
runflag, basinname, row and col of the outletcell, and the -9999 for the area value which isn't used.  

IMPT:  if you don't have the .uh_s file, write  'NONE' as shown in the last line,
and a new file will be created.  Specify the file in subsequent runs to avoid 
the time-consuming step of making it. 

