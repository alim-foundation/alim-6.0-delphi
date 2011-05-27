@echo off
rem Clobbers all non-essential and recreatable Delphi files in this 
rem subdirectory and below

erase *.~?? /q
erase *.dcu /q
erase *.map /q
erase *.dsk /q
erase *.dsm /q
erase derived /q
mkdir derived
