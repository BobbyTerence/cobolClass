      ******************************************************************
      * Author: Will Flowers
      * Date: 08/29/23
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MAT-LAB.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01 num1 pic SZ(3)9.
       01 num2 pic SZ(3)9.
       01 operation pic 99.
       01 result pic SZ(5)9v99.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            DISPLAY "You will be asked to enter Three Numbers."
            DISPLAY "The first two numbers will be the values that are"
            DISPLAY "operated on."
            DISPLAY "Enter 99 to quit."
            PERFORM UNTIL operation = 99
                DISPLAY "Enter first number: "
                ACCEPT num1
                DISPLAY "Enter second number: "
                ACCEPT num2
                DISPLAY "Enter your Operation. 1(+), 2(-), 3(*),"
                DISPLAY "4(/), 99(quit): "
                ACCEPT operation

                EVALUATE operation
                    WHEN 1
                       DISPLAY "Addition"
                       PERFORM 200-add
                       DISPLAY num1 " + " num2 " = " result
                    WHEN 2
                    DISPLAY "Subtraction"
                       PERFORM 300-sub
                       DISPLAY num2 " - " num1 " = " result
                    WHEN 3
                    DISPLAY "Multiplication"
                       PERFORM 400-mul
                       DISPLAY num1 " * " num2 " = " result
                    WHEN 4
                    DISPLAY "Division"
                       PERFORM 500-div
                       EVALUATE num1
                           WHEN 0
                           DISPLAY "Cannot divide by 0"
                           WHEN OTHER
                           DISPLAY num2 " / " num1 " = " result
                       END-EVALUATE
                END-EVALUATE
            END-PERFORM
            STOP RUN.

       200-add.
               ADD num1 to num2 GIVING result.

       300-sub.
               SUBTRACT num1 FROM num2 GIVING result.

       400-mul.
               MULTIPLY num1 BY num2 GIVING result.

       500-div.
               EVALUATE num1
               WHEN 0
                   GOBACK
               WHEN OTHER
                   DIVIDE num1 INTO num2 GIVING result
               END-EVALUATE.
       END PROGRAM MAT-LAB.
