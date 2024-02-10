      ******************************************************************
      * Author: Will Flowers
      * Date:
      * Purpose: Read random polfile, and print a report
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. Proj8.
       Environment Division.
       Input-output section.
       file-control.

       select polName assign to 'polnamei.dat'
        organization is INDExED
        ACCESS MODE is SEQUENTIAL
        RECORD KEY is polnum-in.

       select polFile assign to 'polfile.dat'
        organization is INDExED
        ACCESS MODE is RANDOM
        RECORD KEY is polnum.

       select polReport assign to printer.
       DATA DIVISION.
       FILE SECTION.

      **** polnamei.dat ****
       fd polName.
           01 polNameRec.
               03 polNum-in pic x(6).
               03 fName-in  pic x(15).
               03 lName-in  pic x(15).
               03 addy      pic x(30).
               03 city      pic x(15).
               03 state     pic x(2).
               03 zip       pic x(9).
               03 phone     pic x(10).
               03 ssn       pic x(9).

      **** POLICY FILE ****
       fd polFile.
           01 polFileRec.
               03 polNum           pic x(6).
               03 polDate-mon-in   pic 99.
               03 polDate-day-in   pic xx.
               03 polDate-yr-in    pic xxxx.
               03 pol-amount-in    pic 9(8).
               03 prem-in          pic 9(4)v99.
               03 agent-in         pic x(5).
               03 term-date        pic 9(8).
               03 polType          pic x(3).
               03      pic xx.

      **** PRINTER File ****
       fd polReport.
           01 printRep pic x(132).

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
       01 endread pic x.
       01 grandTotal pic 9(5)v99.
      **** First line of report ****
       01 header1.
      **** HEADER DATE ****
           05 mm-out pic 99.
           05      pic x VALUE "/".
           05 dd-out pic 99.
           05      pic x VALUE "/".
           05 yy-out pic 99.
      **** EMPTY SPACE ****
           05      pic x(30).
      **** EMPTY SPACE ****
      **** STUDENT REPORT ****
           05      pic x(14)  VALUE "POLICY PRINTER".
           05      pic x(10).
      **** PAGE NUBMBER ****
           05      pic x(5) VALUE "PAGE ".
           05 pg-out pic Z,ZZ9.
           05      pic x(10).
           05      pic x(12) VALUE "Will Flowers".

      **** Second line of report ****
       01 header2.
           10      pic x(13) VALUE "Policy number".
           10      pic x(8).
           10      pic x(10) VALUE "First Name".
           10      pic x(8).
           10      pic x(9) VALUE "Last Name".
           10      pic x(8).
           10      pic x(11) VALUE "Policy Date".
           10      pic x(8).
           10      pic x(7) VALUE "Premium".

       01 detailLine.
           03      pic x(3).
           03 polNum-out pic x(6).
           03      pic x(15).
           03 fName-out pic x(15).
           03      pic x(1).
           03 lName-out pic x(15).
           03      pic x(1).
           03 polDate-mon pic xxx.
           03      pic x(1).
           03 polDate-day pic 99.
           03      pic x(1).
           03 polDate-year pic 9(4).
           03      pic x(6).
           03 prem-out pic ZZZ,ZZ9.99.

      **** Month Table ****
       01 MONTHS VALUE
        "JANFEBMARAPRMAYJUNJULAUGSEPOCTNOVDEC".
           05 MONTH OCCURS 12 TIMES PIC A(3).

      **** Total Line ****
       01 totalLine.
           03      pic x(17) VALUE "Total Amount: ".
           03 tot-out pic zz,zzz,zz9.99.

       PROCEDURE DIVISION.
       MAIN.
           PERFORM INITPROJ.
           PERFORM READFILE.
           PERFORM CLOSEPROJ.

       INITPROJ.
           OPEN INPUT polName
           OPEN INPUT polFile
           OPEN OUTPUT polReport
           ACCEPT wsdate FROM DATE
           MOVE mm to mm-out
           MOVE yy to yy-out
           MOVE dd to dd-out
           ADD 1 to pg-in
           Move pg-in to pg-out
           WRITE printRep FROM header1 AFTER ADVANCING 3 LINES.
           WRITE printRep FROM header2 AFTER ADVANCING 2 LINES.

       READFILE.
           PERFORM UNTIL endRead = "Y"
               READ polName AT END MOVE "Y" to endRead
                   NOT AT END PERFORM CONT
               END-READ
           END-PERFORM.

      **** Use Policy number from polnamei.dat as a key to read from *
      ****     polfile.dat ****
       CONT.
           MOVE polNum-in to polNum.
           READ polFile
               INVALID KEY
                   continue
               NOT INVALID KEY
                   PERFORM MOVE-VAL
           END-READ.

      **** Move all read values into an output stream ****
       MOVE-VAL.
           MOVE polNum-in to polNum
           MOVE polNum to polNum-out
           MOVE fName-in to fName-out
           MOVE lName-in to lName-out
           MOVE MONTH(polDate-mon-in) to polDate-mon
           MOVE polDate-yr-in to polDate-year
           MOVE polDate-day-in to polDate-day
           ADD prem-in to grandTotal
           MOVE prem-in to prem-out
           WRITE printRep FROM detailLine
           AFTER ADVANCING 1 LINE
           ADD 1 to lnCnt
      **** Check to add a page if lnCnt > 50 ****
           IF lnCnt > 50
               PERFORM ADDHEAD
           END-IF.

       ADDHEAD.
           ADD 1 to pg-in
           MOVE pg-in to pg-out
           MOVE zeroes to lnCnt
           WRITE printRep FROM header1 AFTER ADVANCING PAGE
           WRITE printRep FROM header2 AFTER ADVANCING 2 LINES.

       CLOSEPROJ.
           MOVE grandTotal to tot-out
           WRITE printRep FROM totalLine AFTER ADVANCING 2 LINES
           CLOSE polName polFile polReport
           STOP RUN.

       END PROGRAM Proj8.
