      ******************************************************************
      * Author: Will Flowers
      * Date:
      * Purpose: New File Template with report and Date
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CREAMERY.
       ENVIRONMENT DIVISION.
       Input-output section.
           file-control.
           select cremeFile assign to 'creme.txt'
           organization is sequential.
           select creamRep assign to printer.
       DATA DIVISION.
       FILE SECTION.
      **** tempFile section ****
       fd cremeFile.
           01 tempRec.
      **** Container choice ****
               03 cont-in pic x(4).
               03 flav-in pic xx.
               03 unPrice-in pic 9v99.
      **********************
      *  temp-input files  *
      *  level: 03         *
      **********************

      **** creamRep section ****
       fd creamRep.
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

      **** Stores total value of the order ****
       01 tot-in pic 9v99.

      **** First line of report ****
       01 header1.
      **** HEADER DATE ****
           05 mm-out pic 99.
           05        pic x VALUE "/".
           05 dd-out pic 99.
           05        pic x VALUE "/".
           05 yy-out pic 99.
      **** EMPTY SPACE ****
           05        pic x(30).
      **** EMPTY SPACE ****
      **** STUDENT REPORT ****
           05        pic x(14)  VALUE "ICE COLD CREAM".
           05        pic x(10).
      **** PAGE NUBMBER ****
           05        pic x(5) VALUE "PAGE ".
           05 pg-out pic  Z,ZZ9.
           05          pic  x(10).
           05          pic  x(12) VALUE "Will Flowers".

      **** Second line of report ****
       01 header2.
           10      pic x(25).
           10      pic x(6) VALUE "Flavor".
           10      pic x(10).
           10      pic x(9) VALUE "Container".
           10      pic x(10).
           10      pic x(11) VALUE "Amount Owed".

      **** Report Detail Line ****
       01 detailLine.
           03      pic x(23).
           03 flav-out pic x(10).
           03      pic x(13).
           03 cont-out pic x(4).
           03      pic x(17).
           03 unPrice-out pic 9.99.

      **** Total Values ****
       01 total.
           10      pic x(58).
           10      pic x(8) VALUE "Total: $".
           10 totOut pic Z9.99.
      ******** End of Report Values ********

      **** Flavor Menu Display ****
       01 flavDis.
           05      pic x(2).
           05 pic x(14) VALUE "Chocolate (CH)".
           05      pic x(2).
           05 pic x(14) VALUE "Strawberry (S)".
           05      pic x(2).
           05 pic x(11) VALUE "Vanilla (V)".
           05      pic x(2).
           05 pic x(13) VALUE "Sherbert (SH)".
           05      pic x(2).
           05 pic x(9) VALUE "Other (O)".

      **** Container Option Display ****
       01 contDis.
           05      pic x(10).
           05 pic x(8) VALUE "Cone (C)".
           05      pic x(5).
           05 pic x(8) VALUE "Bowl (B)".

      **** Procedure Division ****
       Procedure division.
           MAIN.
      **** 'Call' Procedure to open files ***
           PERFORM INITFILE

           PERFORM UNTIL perfStop = 'Y'
               DISPLAY "Select your Flavor"
                   " Order:"
               DISPLAY flavDis
               ACCEPT flav-in

               EVALUATE flav-in
                   WHEN "CH"
                       MOVE "Chocolate" to flav-out
                   WHEN "S"
                       MOVE "Strawberry" to flav-out
                   WHEN "V"
                       MOVE "Vanilla" to flav-out
                   WHEN "SH"
                       MOVE "Sherbert" to flav-out
                   WHEN OTHER
                       MOVE "Other" to flav-out
               END-EVALUATE

               PERFORM UNTIL perfStop = "n"
                   DISPLAY "Select your Container:"
                   DISPLAY contDis
                   ACCEPT cont-in

                   EVALUATE cont-in
                       WHEN "C"
                           MOVE "Cone" to cont-out
                           MOVE 2 to unPrice-in
                           MOVE "n" to perfStop
                       WHEN "B"
                           MOVE "Bowl" to cont-out
                           MOVE 1.5 to unPrice-in
                           MOVE "n" to perfStop
                       WHEN OTHER
                           DISPLAY "Please Enter a Valid Selection"
                           MOVE "k" to perfStop
                   END-EVALUATE
               END-PERFORM

               DISPLAY "Is this your Last Order(Y/N): "
               ACCEPT perfStop

               PERFORM MOVE-VAL

           END-PERFORM

      **** 'Call' Procedure to close opened files ****
           PERFORM CLOSEFILE.

       INITFILE.
           OPEN INPUT cremeFile
           OPEN OUTPUT creamRep
           ACCEPT wsdate FROM DATE
           MOVE mm to mm-out
           MOVE yy to yy-out
           MOVE dd to dd-out
           ADD 1 to pg-in
           Move pg-in to pg-out
           WRITE printRec FROM header1 AFTER ADVANCING 3 LINES
           WRITE printRec FROM header2 AFTER ADVANCING 3 LINES.

       MOVE-VAL.
           MOVE unPrice-in to unPrice-out
           ADD unPrice-in to tot-in
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

       CLOSEFILE.
           MOVE tot-in to totOut
           WRITE printRec FROM total AFTER ADVANCING 2 LINES
           CLOSE cremeFile creamRep
           STOP RUN.
