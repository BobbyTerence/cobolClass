      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. call10.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       LINKAGE SECTION.
           01 Pol-Type pic x(3).
           01 new-prem-in pic 9(4)v99.
       PROCEDURE DIVISION USING Pol-Type new-prem-in.
       MAIN.
           IF Pol-Type = 111
               COMPUTE new-prem-in = (new-prem-in * 1.25)
           ELSE IF Pol-Type = 222
               COMPUTE new-prem-in = (new-prem-in * 1.1)
           ELSE IF Pol-Type = 333
               COMPUTE new-prem-in = (new-prem-in * 1.5)
       EXIT PROGRAM.
