@echo off
cls
echo.
echo.
echo A test of ZONEBDGT will be performed using data from the
echo ..\data directory.  Iinformation will be sent to the screen
echo which you may ignore, unless the tests abort with an error
echo message.  You must be in DOS mode for the test to run.
echo.
echo Note: the data directory contains output files of the tests
echo that can be compared with the output files generated during
echo the test (DO NOT ALTER files in the ..\data directory).
echo.
pause
if exist go del go
echo zbtest.lst > go
echo ..\data\zbtest.bud >> go
echo Test data for running ZONEBUDGET >> go
echo ..\data\zbtest.zon >> go
echo A >> go

..\bin\zonebdgt.exe < go
