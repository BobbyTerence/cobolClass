      ******************************************************************
      * Author: Will Flowers
      * Date:
      * Purpose: New File Template with report and Date
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. DEFAULT-TEMPLATE.
       ENVIRONMENT DIVISION.
       Input-output section.
           file-control.
           select polFile assign to 'polfile.txt'
           organization is sequential.
           select polRep assign to printer.
       DATA DIVISION.
       FILE SECTION.
      **** polFile section ****
       fd polFile.
           01 polRec.
               03 polNum pic x(6).
               03 polDate pic 9(8).
               03 polAmt pic 9(8).
               03 polPrem pic 9(4)v99.
               03 polAg pic x(5).
               03 termDate pic 9(8).
               03 polType pic x(3).
               03 polField pic x(2) VALUE "NO".
      **********************
      *  temp-input files  *
      *  level: 03         *
      **********************

      **** polRep section ****
       fd polRep.
      **** printRec will store the next line going to the report ****
           01 printRec pic x(132).
       WORKING-STORAGE SECTION.
      **** Line/Page Count ****
       01 lnCnt pic 99.
       01 pg-in pic 9999.
      **** Date variables ****
       01 wsdate.
           04 yy pic 99.
           04 mm pic 99.
           04 dd pic 99.
      **** Perform Stop Value ****
       01 perfStop pic x.
      **** First line of report ****
       01 header1.
      **** HEADER DATE ****
           05 mm-out pic 99.
           05        pic x VALUE "/".
           05 dd-out pic 99.
           05        pic x VALUE "/".
           05 yy-out pic 99.
      **** EMPTY SPACE ****
           05        pic x(36).
      **** EMPTY SPACE ****
      **** STUDENT REPORT ****
           05        pic x(14)  VALUE "POLICY REPORT".
           05        pic x(19).
      **** PAGE NUBMBER ****
           05        pic x(5) VALUE "PAGE ".
           05 pg-out pic  Z,ZZ9.
           05          pic  x(19).
           05          pic  x(12) VALUE "Will Flowers".

      **** Second line of report ****
       01 header2.
           10 headNum pic x(13) VALUE "Policy Number".
           10      pic x(4).
           10 headDate pic x(11) VALUE "Policy Date".
           10      pic x(4).
           10 headAmt pic x(13) Value "Policy Amount".
           10      pic x(4).
           10 headPrem pic x(14) VALUE "Policy Premium".
           10      pic x(4).
           10 headAg pic x(5) VALUE "Agent".
           10      pic x(4).
           10 headTerm pic x(16) VALUE "Termination Date".
           10      pic x(4).
           10 headType pic x(11) VALUE "Policy Type".
           10      pic x(4).
           10 headField pic x(7) VALUE "Field x".

      **** Report Detail Line ****
       01 detailLine.
           03      pic x(7).
           03 num-out pic x(6).
           03      pic x(5).
           03 date-out pic 99/99/9(4).
           03      pic x(9).
           03 amt-out pic ZZZZZZZ9.
           03      pic x(11).
           03 prem-out pic ZZZ9.99.
           03      pic x(4).
           03 ag-out pic x(5).
           03      pic x(10).
           03 term-out pic 99/99/9(4).
           03      pic x(12).
           03 type-out pic x(3).
           03      pic x(9).
           03 field-out pic x(2).


      **** Procedure Division ****
           Procedure division.
           MAIN.
      **** 'Call' Procedure to open files ***
           PERFORM INITFILE.
           PERFORM READFILE.
           PERFORM CLOSEFILE.


       INITFILE.
           OPEN INPUT polFile
           OPEN OUTPUT polRep
           ACCEPT wsdate FROM DATE
           MOVE mm to mm-out
           MOVE yy to yy-out
           MOVE dd to dd-out
           ADD 1 to pg-in
           Move pg-in to pg-out
           WRITE printRec FROM header1 AFTER ADVANCING 3 LINES.
           WRITE printRec FROM header2 AFTER ADVANCING 3 LINES.

       MOVE-VAL.
           MOVE polNum to num-out
           MOVE polDate to date-out
           MOVE polAmt to amt-out
           MOVE polPrem to prem-out
           MOVE polAg to ag-out
           MOVE termDate to term-out
           MOVE polType to type-out
           MOVE polField to field-out
           WRITE printRec FROM detailLine AFTER ADVANCING 1 LINES
           ADD 1 to lnCnt
      **** Check to add a page if lnCnt > 50 ****
           IF lnCnt > 50
               PERFORM ADDHEAD
               END-IF.

      **** ADDHEAD Creates a new header with updated page count ****
       ADDHEAD.
           ADD 1 to pg-in
           MOVE pg-in to pg-out
           MOVE zeroes to lnCnt
           WRITE printRec FROM header1 AFTER ADVANCING PAGE
           WRITE printRec FROM header2 AFTER ADVANCING 2 LINES.

       READFILE.
           PERFORM UNTIL perfStop = "Y"
               READ polFile AT END MOVE "Y" to perfStop
               NOT AT END
                   DISPLAY polRec
                   PERFORM MOVE-VAL
               END-READ
           END-PERFORM.

       CLOSEFILE.
      *     WRITE printRec FROM total AFTER ADVANCING 2 LINES
           CLOSE polFile polRep
           STOP RUN.
