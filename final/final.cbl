      ******************************************************************
      * Author: Will Flowers
      * Date: 12/04/23
      * Purpose: Be able to update, add, or delete entrires
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. FINAL-PROJ.
       ENVIRONMENT DIVISION.
       input-output section.
       file-control.
           select polFile assign to 'polfile.dat'
               organization is INDEXED
               ACCESS MODE is RANDOM
               RECORD KEY is polNum-in.

           select agentFile assign to 'agent.dat'
               organization is INDEXED
               access mode is RANDOM
               record key is agent-code.

           select polReport assign to printer.

       DATA DIVISION.
       file section.
           fd polFile.
           01 polFileRec.
               03 polNum-in pic x(6).
               03 polDate-in pic 9(8).
               03 polAmt-in pic 9(8).
               03 polPrem-in pic 9(4)v99.
               03 fill pic xx.
               03 polAgent-in pic x(5).
               03 polTerm-in pic x(8).
               03 poltype-in pic x(3).

           fd agentFile.
           01 agentRec.
               03 agent-code pic x(5).
               03 agent-name pic x(20).

           fd polReport.
               01 printRep pic x(132).
       working-storage section.
      **** Line/Page Count ****
       01 lnCnt pic 99.
       01 pg-in pic 9999.

       01 wsdate.
           04 yy pic 99.
           04 mm pic 99.
           04 dd pic 99.

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

       01 header2.
      **** Policy Number and Action Header ****
           03 pic x(13) VALUE "Policy Number".
           03 pic x(5).
           03 pic x(13) VALUE "Record Action".
           03 pic x(5).
           03 pic x(14) VALUE "Action Success".

      **** This will be used to display all of the field options ****
       01 updateDis.
           03 pic x(16) VALUE "Policy Number(1)".
           03      pic x(2).
           03 pic x(14) VALUE "Policy Date(2)".
           03      pic x(2).
           03 pic x(16) VALUE "Policy Amount(3)".
           03      pic x(2).
           03 pic x(17) VALUE "Policy Premium(4)".
           03      pic x(2).
           03 pic x(15) VALUE "Policy Agent(5)".
           03      pic x(2).
           03 pic x(26) VALUE "Policy Termination Date(6)".
           03      pic x(2).
           03 pic x(14) VALUE "Policy Type(7)".
           03      pic x(2).
           03 pic x(7) VALUE "Quit(8)".

      **** Creates variables for each field to be able to edit the record ****
       01 editLine.
           03 polNum-ed pic x(6).
           03 polDate-ed pic 9(8).
           03 polAmt-ed pic 9(8).
           03 polPrem-ed pic 9(4)v99.
           03 fill pic xx.
           03 polAgent-ed pic x(5).
           03 polTerm-ed pic x(8).
           03 polType-ed pic x(3).
      **** editLine will always beused to accept user input ****

      **** updateRec will be used to hold the information for the record ****
      *    that we are updating and change any old info with the new info   *
       01 updateRec.
           03 polNum-up pic x(6).
           03 polDate-up pic 9(8).
           03 polAmt-up pic 9(8).
           03 polPrem-up pic 9(4)v99.
           03 fill pic xx.
           03 polAgent-up pic x(5).
           03 polTerm-up pic x(8).
           03 polType-up pic x(3).

       01 recordHead.
           03 pic x(13) VALUE "Policy Number".
           03 pic x(4).
           03 pic x(11) VALUE "Policy Date".
           03 pic x(4).
           03 pic x(13) VALUE "Policy Amount".
           03 pic x(4).
           03 pic x(14) VALUE "Policy Premium".
           03 pic x(4).
           03 pic x(12) VALUE "Policy Agent".
           03 pic x(4).
           03 pic x(16) VALUE "Policy Term-Date".
           03 pic x(4).
           03 pic x(11) VALUE "Policy Type".

       01 recordDis.
           03 pic x(7).
           03 polNum pic x(6).
           03 pic x(5).
           03 polDate pic 99/99/9999.
           03 pic x(6).
           03 polAmt pic ZZZ,ZZZ,ZZZ.
           03 pic x(11).
           03 polPrem pic ZZZ9.99.
           03 pic x(11).
           03 polAgent pic x(5).
           03 pic x(10).
           03 polTerm pic XX/XX/XXXX.
           03 pic x(12).
           03 polType pic x(3).

       01 addPrint.
           03      pic x(4).
           03 addNum pic x(6).
           03      pic x(13).
           03      pic x(5) VALUE "ADDED".
           03 pic x(12).
           03 success pic x(5) VALUE "TRUE".

       01 upPrint.
           03      pic x(4).
           03 upNum pic x(6).
           03      pic x(11).
           03      pic x(7) VALUE "UPDATED".
           03 pic x(12).
           03 successUp pic x(5).

       01 delPrint.
           03 pic x(4).
           03 delNum pic x(6).
           03      pic x(12).
           03      pic x(6) VALUE "DELETE".
           03 pic x(12).
           03 successDel pic x(5).

      **** This will be used for holding the old policy number if the ****
      *    policy number is changing                                     *
       01 polNum-old pic x(6).

      **** Used to keep looping the project until you decide to quit ****
       01 EOProj pic x VALUE "N".
      **** Used to quit updating a record ****
       01 EOUpd pic x VALUE "N".

       01 validPolNum pic x VALUE "N".
      **** validNum is used for the Update and Delete paragraphs ****
       01 validNum pic x VALUE "N".
       01 validAg pic x VALUE "N".
       01 validType pic x VALUE "N".
       01 notNull pic x VALUE "N".

      **** Variable used to choose between add, update, delete, or quit ****
       01 option pic x(6).
      **** Variable use to choose which field to update ****
       01 updateChoice pic x.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           DISPLAY "All Commands will be taken in ALL CAPS"
           PERFORM INITPROJ

           PERFORM UNTIL EOProj = "Y"
               DISPLAY "Would you like to ADD, UPDATE, DELETE or QUIT"
               ACCEPT option
               IF option = "ADD" THEN
                   PERFORM ADD-REC
               ELSE IF option = "UPDATE" THEN
                   PERFORM UP-REC
               ELSE IF option = "DELETE" THEN
                   PERFORM DEL-REC
               ELSE IF option = "QUIT" THEN
                   DISPLAY "Program Quit"
                   MOVE "Y" to EOProj
               END-IF
           END-PERFORM

           PERFORM END-PROGRAM.

       INITPROJ.
           OPEN I-O polFile
           OPEN INPUT agentFile
           OPEN OUTPUT polReport
           ACCEPT wsdate FROM DATE
           MOVE mm to mm-out
           MOVE yy to yy-out
           MOVE dd to dd-out
           ADD 1 to pg-in
           Move pg-in to pg-out
           WRITE printRep FROM header1 AFTER ADVANCING 2 LINES
           WRITE printRep FROM header2 AFTER ADVANCING 2 LINES.

       ADD-REC.
           DISPLAY "Policy Number:"
           PERFORM UNTIL validPolNum = "Y"
               PERFORM Num-Change
               MOVE polNum-ed to polNum-in
               READ polFile
                   INVALID KEY
                       MOVE "Y" to validPolNum
                   NOT INVALID KEY
                       DISPLAY "Error, Record already exists."
                       continue
           END-PERFORM.
           ACCEPT polDate-ed FROM DATE
           DISPLAY "Policy Amount:"
           PERFORM Amt-Change
           DISPLAY "Policy Premium:"
           PERFORM Prem-Change

           DISPLAY "Agent Code"
           PERFORM Agent-Change

           MOVE 00000000 TO polTerm-ed
           DISPLAY "Policy Type:"
           PERFORM Type-Change

           PERFORM WRITE-REC

      **** Move an N to all validation values to add another record ****
           MOVE "N" TO validPolNum
           MOVE "N" TO validAg
           MOVE "N" TO validType.

      **** All Values are passed through the editLine block ****
       UP-REC.
           DISPLAY "What Record would you like to update"
           PERFORM GET-REC
           MOVE polFileRec to updateRec
           PERFORM UNTIL EOUpd = "Y"
               DISPLAY "What field would you like to Update"
               DISPLAY updateDis
               ACCEPT updateChoice
               IF updateChoice = "1" THEN
                   DISPLAY "Enter Policy Number Change:"
                   PERFORM UNTIL validPolNum = "Y"
                       PERFORM Num-Change
                       MOVE polNum-ed to polNum-in
                       READ polFile
                           INVALID KEY
                               MOVE "Y" to validPolNum
                           NOT INVALID KEY
                               DISPLAY "Error, Record already exists."
                               continue
                   END-PERFORM
                   MOVE polNum-ed to polNum-up
               ELSE IF updateChoice = "2" THEN
                   DISPLAY "Enter Date Change:"
                   PERFORM Date-Change
                   MOVE polDate-ed to polDate-up
               ELSE IF updateChoice = "3" THEN
                   DISPLAY "Enter Amount Change:"
                   PERFORM Amt-Change
                   MOVE polAmt-ed to polAmt-up
               ELSE IF updateChoice = "4" THEN
                   DISPLAY "Enter Premium Change:"
                   PERFORM Prem-Change
                   MOVE polPrem-ed to polPrem-up
               ELSE IF updateChoice = "5" THEN
                   DISPLAY "Enter Agent Change:"
                   PERFORM Agent-Change
                   MOVE polAgent-ed to polAgent-up
               ELSE IF updateChoice = "6" THEN
                   DISPLAY "Enter Termination Date Change:"
                   PERFORM Term-Change
                   MOVE polTerm-ed to polTerm-up
               ELSE IF updateChoice = "7" THEN
                   DISPLAY "Enter Type Change:"
                   PERFORM Type-Change
                   MOVE polType-ed to polType-up
               ELSE IF updateChoice = "8" THEN
                   MOVE "Y" to EOUpd
               END-IF
           END-PERFORM.

           PERFORM UPDATE-REC.

       DEL-REC.
           DISPLAY "What record would you like to delete"
           PERFORM GET-REC
           READ polFile
               INVALID KEY
                   DISPLAY "Record could not be Removed"
                   MOVE "FALSE" to successDel
                   continue
               NOT INVALID KEY
                   MOVE "TRUE" to successDel
                   DELETE polFile
                   DISPLAY "Record Successfully Removed"
           END-READ
           PERFORM DEL-PRINT.

       WRITE-REC.
           PERFORM ADD-PRINT
           WRITE polFileRec FROM editLine.

       UPDATE-REC.
           IF polNum-in NOT = polNum-up THEN
               READ polFile
                   INVALID KEY
                       continue
                   NOT INVALID KEY
                       DELETE polFile
               END-READ
               WRITE polFileRec FROM updateRec
           ELSE
               READ polFile
                   INVALID KEY
                       continue
                   NOT INVALID KEY
                   MOVE updateRec to polFileRec
                   REWRITE polFileRec
           END-IF

           READ polFile
           KEY IS polNum-up
               INVALID KEY
                   MOVE "FALSE" to successUp
                   DISPLAY "Error Updating"
                   continue
               NOT INVALID KEY
                   MOVE "TRUE" to successUp
                   DISPLAY "Update Successful"
           END-READ

           PERFORM UP-PRINT.

       GET-REC.
           PERFORM UNTIL validNum = "Y"
               ACCEPT polNum-in
               READ polFile
                   INVALID KEY
                       DISPLAY "Error, Record does not exist"
                       continue
                   NOT INVALID KEY
                       MOVE "Y" to validNum
           END-PERFORM

           PERFORM MOVE-REC
           DISPLAY recordHead
           DISPLAY recordDis

      **** Reset validNum for next time ****
           MOVE "N" to validNum.

      **** All -Change paragraphs are used to validate non-null entries ****
       Num-Change.
           PERFORM UNTIL notNull = "Y"
               ACCEPT polNum-ed
               IF polNum-ed = " "
                   DISPLAY "Error, field cannot be empty"
               ELSE
                   MOVE "Y" to notNull
               END-IF
           END-PERFORM

           MOVE "N" to notNull.

       Date-Change.
           PERFORM UNTIL notNull = "Y"
               ACCEPT polDate-ed
               IF polDate-ed < 1
                   DISPLAY "Error, field cannot be empty"
               ELSE
                   MOVE "Y" to notNull
               END-IF
           END-PERFORM

           MOVE "N" to notNull.

       Amt-Change.
           PERFORM UNTIL notNull = "Y"
               ACCEPT polAmt-ed
               IF polAmt-ed < 1
                   DISPLAY "Error, field cannot be empty"
               ELSE
                   MOVE "Y" to notNull
               END-IF
           END-PERFORM

           MOVE "N" to notNull.

       Prem-Change.
           PERFORM UNTIL notNull = "Y"
               ACCEPT polPrem-ed
               IF polPrem-ed < 1
                   DISPLAY "Error, field cannot be empty"
               ELSE
                   MOVE "Y" to notNull
               END-IF
           END-PERFORM

           MOVE "N" to notNull.

       Agent-Change.
           PERFORM UNTIL validAg = "Y"
               ACCEPT polAgent-ed
               MOVE polAgent-ed to agent-code
               READ agentFile
                   INVALID KEY
                       DISPLAY "Error, Agent does not exist."
                       continue
                   NOT INVALID KEY
                       MOVE "Y" to validAg
           END-PERFORM.

       Term-Change.
           PERFORM UNTIL notNull = "Y"
               ACCEPT polTerm-ed
               IF polTerm-ed = " "
                   DISPLAY "Error, field cannot be empty"
               ELSE
                   MOVE "Y" to notNull
               END-IF
           END-PERFORM

           MOVE "N" to notNull.

       Type-Change.
           PERFORM UNTIL validType = "Y"
               ACCEPT polType-ed
               IF polType-ed = "111" THEN
                   MOVE "Y" to validType
               ELSE IF polType-ed = "222" THEN
                   MOVE "Y" to validType
               ELSE IF polType-ed = "333" THEN
                   MOVE "Y" to validType
               ELSE
                   DISPLAY "Error, Policy Type must be 111, 222, or"
                       "333"
           END-PERFORM.

       ADD-PRINT.
           MOVE polNum-ed to addNum
           WRITE printRep FROM addPrint AFTER ADVANCING 2 LINES.

       UP-PRINT.
           MOVE polNum-up to upNum
           WRITE printRep FROM upPrint AFTER ADVANCING 2 LINES.

       DEL-PRINT.
           MOVE polNum-in to delNum
           WRITE printRep FROM delPrint AFTER ADVANCING 2 LINES.

      **** This is being used to display the record before updating and ****
      *    deleting                                                        *
       MOVE-REC.
           MOVE polNum-in to polNum
           MOVE polDate-in to polDate
           MOVE polAmt-in to polAmt
           MOVE polPrem-in to polPrem
           MOVE polAgent-in to polAgent
           MOVE polTerm-in to polTerm
           MOVE poltype-in to polType.

       END-PROGRAM.
           CLOSE polFile polReport agentFile
           STOP RUN.

       END PROGRAM FINAL-PROJ.
