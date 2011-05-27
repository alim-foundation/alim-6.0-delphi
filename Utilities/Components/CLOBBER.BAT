@echo off
rem Clobbers all non-essential and recreatable Delphi files in this 
rem subdirectory and below

erase *.~??
erase *.dcu
erase *.map
erase *.dsk
erase *.dsm
erase derived
mkdir derived
