      ******************************************************************
      * Author: Will Flowers
      * Date:
      * Purpose: New File Template with report and Date
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. agentHeader.
       ENVIRONMENT DIVISION.
       Input-output section.
           file-control.
               select polFile assign to 'polfile.txt'
               organization is sequential.

               select agentFile assign to 'agent.dat'
               organization is INDEXED
               access mode is RANDOM
               record key is agent-code.

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

      **** agent file section ****
       fd agentFile.
           01 agentRec.
               03 agent-code pic x(5).
               03 agent-name pic x(20).

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
           05        pic x(17).
      **** EMPTY SPACE ****
      **** STUDENT REPORT ****
           05 ag-name-out pic x(15).
           05      pic x(5).
           05 ag-out pic x(5).
           05        pic x(17).
      **** PAGE NUBMBER ****
           05        pic x(5) VALUE "PAGE ".
           05 pg-out pic  Z,ZZ9.
           05          pic  x(19).
           05          pic  x(12) VALUE "Will Flowers".

      **** Second line of report ****
       01 header2.
           10      pic x(8).
           10 headDate pic x(11) VALUE "Policy Date".
           10      pic x(4).
           10 headNum pic x(13) VALUE "Policy Number".
           10      pic x(4).
           10 headAmt pic x(13) Value "Policy Amount".
           10      pic x(4).
           10 headPrem pic x(14) VALUE "Policy Premium".
           10      pic x(4).
           10 headTerm pic x(16) VALUE "Termination Date".
           10      pic x(4).
           10 headType pic x(11) VALUE "Policy Type".

      **** Report Detail Line ****
       01 detailLine.
           03      pic x(10).
           03 date-out pic 99/99/9(4).
           03      pic x(9).
           03 num-out pic x(6).
           03      pic x(9).
           03 amt-out pic ZZ,ZZZ,ZZ9.
           03      pic x(11).
           03 prem-out pic Z,ZZ9.99.
           03      pic x(4).

           03 term-out pic 99/99/9(4).
           03      pic x(12).
           03 type-out pic x(3).

       01 AgTotal.
           03      pic x(14) VALUE "Total Amount: ".
           03 ag-amt pic ZZZ,ZZZ,ZZ9.
           03      pic x(15).
           03      pic x(15) VALUE "Total Premium: ".
           03 ag-prem pic ZZ,ZZ9.99.

       01 agAmt pic 9(9).
       01 agPrem pic 9(5)v99.

      **** I am using agent-code itself as the agent-in ****
       01 agent-in pic x(5).

      **** Procedure Division ****
           Procedure division.
           MAIN.
      **** 'Call' Procedure to open files ***
           PERFORM INITPROJ.
           PERFORM READFILE.
           PERFORM CLOSEFILE.


       INITPROJ.
           OPEN INPUT polFile
           OPEN INPUT agentFile
           OPEN OUTPUT polRep
           ACCEPT wsdate FROM DATE
           MOVE mm to mm-out
           MOVE yy to yy-out
           MOVE dd to dd-out
           ADD 1 to pg-in
           Move pg-in to pg-out.

       MOVE-VAL.
      **** Add the Respective Policy Amount and Premium to Agent Val


           IF agent-in = ' '
               PERFORM HEADER
               MOVE agent-code to agent-in
           END-IF

      ****
           IF agent-in NOT = agent-code
               AND agent-code NOT = ' '
                   PERFORM ATot
                   IF agent-in not = ' '
                       PERFORM HEADER
                   END-IF
           END-IF

           ADD polAmt to agAmt
           ADD polPrem to agPrem

           MOVE polDate to date-out
           MOVE polAmt to amt-out
           MOVE polPrem to prem-out
           MOVE polNum to num-out
           MOVE termDate to term-out
           MOVE polType to type-out

           WRITE printRec FROM detailLine AFTER ADVANCING 1 LINES
           ADD 1 to lnCnt
      **** Check to add a page if lnCnt > 50 ****
           IF lnCnt > 50
               PERFORM ADDPAGE
           END-IF.

      **** ADDHEAD Creates a new header with updated page count ****
       ADDPAGE.
           ADD 1 to pg-in
           MOVE pg-in to pg-out
           MOVE zeroes to lnCnt
           WRITE printRec FROM header1 AFTER ADVANCING PAGE
           WRITE printRec FROM header2 AFTER ADVANCING 2 LINES.

       READFILE.
           PERFORM UNTIL perfStop = "Y"
               READ polFile AT END MOVE "Y" to perfStop
               NOT AT END
                   PERFORM READ-AG
               END-READ
           END-PERFORM.

       READ-AG.
           MOVE polAg to agent-code
           READ agentFile
               INVALID KEY
                   CONTINUE
               NOT INVALID KEY
                   PERFORM MOVE-VAL
           END-READ.

       ATot.
           MOVE agAmt to ag-amt
           MOVE agPrem to ag-prem

           WRITE printRec from AgTotal AFTER ADVANCING 2 LINES.

           MOVE agent-code to agent-in.

       HEADER.
      **** Write the Headers ****
           MOVE agent-name to ag-name-out
           MOVE agent-code to ag-out
           WRITE printRec from header1 AFTER ADVANCING 3 LINES.
           WRITE printRec FROM header2 AFTER ADVANCING 2 LINES.
           MOVE zeroes to agAmt, agPrem.

       CLOSEFILE.
           MOVE agAmt to ag-amt
           MOVE agPrem to ag-prem
           WRITE printRec from AgTotal AFTER ADVANCING 2 LINES.
           CLOSE polFile agentFile polRep
           STOP RUN.

           END PROGRAM agentHeader.
