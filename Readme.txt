                                                            April 24, 2000


               Zonebudget (ZONBUD) - Version: 2.0 2000/04/24
              Program for computing subregional water budgets
                    for MODFLOW ground-water flow models

NOTE: Any use of trade, product or firm names is for descriptive purposes 
      only and does not imply endorsement by the U.S. Government.

Instructions for installing, executing, and testing ZONBUD are 
provided below.

This version of Zonebudget is packaged for use on personal computers
using a Microsoft Windows operating system. For invoking Zonebudget,
the acronym ZONBUD is used. (For version 1.0, the acronym ZONEBDGT was
used.)


                            TABLE OF CONTENTS

                         A. DISTRIBUTION FILE
                         B. EXTRACTING FILES
                         C. COMPILING
                         D. INSTALLING
                         E. RUNNING THE SOFTWARE
                         F. TESTING


A. DISTRIBUTION FILE

The following self-extracting distribution file, containing the software,
test data sets, and information files, is currently available for
Microsoft Windows computer systems:

         znbd2_0.exe  - Compiled using Lahey Fortran 90 with source code

The program source code consists of the following file (found in the
ZONBUD.2_0\src directory):

    ZONBUD.for

Also included in the src directory are several files used by the Lahey
Automake utility for compiling the program.

File ZONBUD.2_0/doc/zonbud.txt describes the changes to version 1.0 of
the code, which result in version 2.0. Also included in directory
ZONBUD.2_0/doc is a Portable Document Format (PDF) version of the
ZONEBUDGET documentatoin (ofr90392.pdf).

The PDF file is readable and printable on various computer platforms using
Acrobat Reader from Adobe.  The Acrobat Reader is freely available from
the following World Wide Web sites:
      http://www.adobe.com/
      http://www.shareware.com/




B. EXTRACTING FILES

The following are the steps to extract the files from the distribution
file.  Note, replace <disk drive> with the drive letter where you want
to install ZONBUD and optionally replace [directory] with the name of a
directory on that drive:


   1. Extract the files by executing file:

        znbd2_0.exe

      When the extraction progrm runs, specify that the files should be
      restored to directory C:\WRDAPP
 

The following directory structure will be created in C:\WRDAPP (the
contents of each directory are shown to the right):

      ZONBUD.2_0      ; copy of this readme file
        `-----bin       ; compiled executable and Lahey error file
        `-----doc       ; documentation files
        `-----src       ; makefile and source code
        `-----test      ; batch files to run verification tests
        `-----data      ; standard data sets used in verification tests

Notes:
a) It is recommended that no user files are kept in the ZONBUD.2_0
   directory structure.  If you do plan to put files in the ZONBUD.2_0
   directory structure, do so only by creating subdirectories of
   ZONBUD.2_0.


C. COMPILING

An executable version of the code for personal computers is provided in
the bin directory; however, the source code is also provided in the src
directory so that users can generate the executable themselves. No
support can be provided for users generating their own versions of the
software.  In general, the requirements are a Fortran compiler and a
of using the compiler.



D: INSTALLING

To make the ZONBUD program accessible from any directory, the directory
containing the executable should be included in the PATH environment
variable.  For example, you could add a line similar to the following
to the AUTOEXEC.BAT file:

  PATH=%PATH%;C:\WRDAPP\ZONBUD.2_0\bin

Note, substitute the appropriate drive letter and pathname if not C:\
as shown above.

As an alternative, the ZONBUD executable can be moved to a directory
already included in the PATH environment variable.



E. RUNNING THE SOFTWARE

        **System Requirements**
        - 486-based or higher personal computer with math co-processor
          running a Microsoft Windows operating system.
        - 16 MB application RAM
        - 5 MB hard disk space

ZONBUD has been compiled using the Lahey Fortran 90 extended memory
compiler version 4.50i.  The file "LF90.EER" (from the Lahey compiler)
located in ZONBUD.2_0\bin contains error messages.  If an error occurs,
this file is used to print error messages if the ZONBUD.2_0\bin directory
is included in the PATH environment variable; if LF90.ERR cannot be found,
the error will only be identified by number.

The program arrays are dimensioned for models having up to 1,000,000 cells.

After ZONBUD is properly installed in a directory that is included in
your PATH, the program is initiated using the command:  ZONBUD.  The
program prompts for the following information:
     1) name of the LISTING FILE for results
     2) name of the CELL-BY-CELL BUDGET TERMS
     3) TITLE for use in LISTING FILE
     4) an option for specifying when budgets are calculated


F. TESTING

Test data sets are provided to verify that the program is correctly
installed and running on the system.  The tests may also be looked at
as examples of how to use the program.  The directory ZONBUD.2_0\test
contains batch files to run the tests.  The directory ZONBUD.2_0\data
contains the input data and expected results for each test.  Run the
tests in the ZONBUD.2_0\test directory using the command:

   test

After the tests are completed, the results can be compared to the
expected results.  To clean-up after the tests, type the command:

   clean

The tests are described in the table below.  Test is the test number,
program is the program used to run the test, and the usage column
indicates how a file is used, with i for input, o for output, and
i/o for both input and output.

IMPORTANT NOTE: use zbtest.nam and associated files to generate the cell-
by-cell flow data (zbtest.bud) using MODFLOW.  The zbtest.bud is a binary 
file that must be generated for each platform.  The one provided was 
generated on a Pentium personal computer.  The test.bat file does not run 
MODFLOW to generate the zbtest.bud, you must do this on your own.

test  program  description of test and files      file name & usage
----  -------  ---------------------------------  -----------------
  1   modflow  Runs the example problem in OFR 90-392

               Name File to designate files       zbtest.nam      i
               BAS5 Package                       zbtest.bas      i
               BCF5 Package                       zbtest.bcf      i
               SIP5 Package                       zbtest.sip      i
               Output Control                     zbtest.oc       i
               RCH5 Package                       zbtest.rch      i
               WEL5 Package                       zbtest.wel      i
               cell-by-cell flow data             zbtest.bud      o
               Listing of results                 zbtest_m.lst    o

      ZONBUD  Runs the example problem in OFR 90-392

               cell-by-cell flow data             zbtest.bud      i
               ZONE input file                    zbtest.zon      i
               response file for program prompts  go              i
               Listing of results                 zbtest.lst      o
