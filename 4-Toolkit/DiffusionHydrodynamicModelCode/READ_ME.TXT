DHM21.FOR	This is the "official" FORTRAN code from the USGS report.

DHM21.EXE	This is the "official" compiled executable code from the 
		USGS report.  This program will run input data files in 
		a wide variety of text formats on a wide variety of 
		computers.  This program will also run input data files 
		created with the input data editor EDHM21.EXE.

EDHM21.EXE	Input data editor developed and distributed at the 1999 
		FMA conference in Sacramento.

DOSXMSF.EXE	This program accompanies EDHM21.EXE and must reside in the 
                same directory. 

DHM21.DAT	Input data file for USGS report example, written in generic
		text format.  DHM21.EXE accepts and runs this file, but 
		EDHM21.EXE does not recognize it as a DHM file.

DHM_TEST.DHM	Edited DHM21.DAT file to make it consistent with EDHM21.EXE
		format.

READ_ME.TXT	Text file explaining program operation.


A minimum of only two files are needed to run DHM 21:  DHM21.EXE and an 
input data with standard 8-character filename and 3-character extension.
The DHM21 opening screen provides file naming suggestions.  As a general
guide, input and output file name extensions should not conflict with 
extensions used with other programs, such as .DOC, .XLS, etc.  If .DAT 
conflicts, try a .DHM extension.  Output filename extension .TXT will
allow direct output file access with programs such as NOTEPAD or other
ASCII editor.

To run DHM21.EXE, run the program from the DOS command screen, or highlight
the file and click [Enter], or double click on the file name.  After
the prompt "ENTER INPUT FILE NAME", type the input file name and hit 
[Enter].

The next prompt will be: 

PRINTOUT OPTIONS:
    1= RESULTS SENT DIRECTLY TO PRINTER
    2= RESULTS SENT TO A FILE ON DISK
SELECT DESIRED OPTION ->

Do not select 1 unless the output file will be very small, and a printer is 
connected directly to the computer.  It is recommended to select 2 and hit
[Enter].

A third prompt will be:

ENTER RESULTS FILE NAME.  Type the output file name and hit [Enter].

DHM 21 will begin.  There will be a sequential listing of model
output times such as:

MODEL TIME =   .51 HOURS
MODEL TIME =  1.00 HOURS
MODEL TIME =  1.50 HOURS
etc.

until calculations are completed.  The output file can be read with any 
ASCII text editor.


Input data files can be written directly with any ASCII text editor, or
with the prompted data input program EDHM21.EXE.  To run this program,
the accompanying program DOSXMSF.EXE must reside in the same directory.

To run EDHM21.EXE, run the program from the DOS command screen, or 
highlight the file and click [Enter], or double click on the file name.  

After some experience is gained using EDHM21, it is possible to directly
edit the input data file, using EDHM21 to provide a template.

