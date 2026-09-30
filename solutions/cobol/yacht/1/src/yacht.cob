       IDENTIFICATION DIVISION.
       PROGRAM-ID. YACHT.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-INDEX  PIC 9(1).
       01 WS-RESULT PIC 99 VALUE 0.
       01 WS-CATEGORY PIC X(15).
       01 WS-DICE PIC 9(5).
       01 WS-DICE-R REDEFINES WS-DICE.
           05 WS-DIE PIC 9 OCCURS 5 TIMES.
       01 WS-COUNTS.
           05 WS-COUNT PIC 9 OCCURS 6 TIMES VALUE 0.
       01 WS-HAS-THREE PIC 9 VALUE 0.
       01 WS-HAS-TWO   PIC 9 VALUE 0.
          
       PROCEDURE DIVISION.
       YACHT.
           EVALUATE WS-CATEGORY
              WHEN "ones"
                 PERFORM CALC-ONES
              WHEN "twos"
                 PERFORM CALC-TWOS
              WHEN "threes"
                 PERFORM CALC-THREES
              WHEN "fours"
                 PERFORM CALC-FOURS
              WHEN "fives"
                 PERFORM CALC-FIVES
              WHEN "sixes"
                 PERFORM CALC-SIXES
              WHEN "choice"
                 PERFORM CALC-CHOICES
              WHEN "yacht"
                 PERFORM CALC-YACHT
              WHEN "full house"
                 PERFORM CALC-FULL-HOUSE
              WHEN "four of a kind"
                 PERFORM CALC-FOUR-OF-A-KIND
              WHEN "little straight"
                 PERFORM CALC-LITTLE-STRAIGHT
              WHEN "big straight"
                 PERFORM CALC-BIG-STRAIGHT
           END-EVALUATE

           STOP RUN 
           .

       CALC-ONES.
           PERFORM VARYING WS-INDEX FROM 1 BY 1 UNTIL WS-INDEX > 5
              IF WS-DIE(WS-INDEX) = 1
                 ADD 1 TO WS-RESULT
              END-IF
           END-PERFORM
           .

       CALC-TWOS.
           PERFORM VARYING WS-INDEX FROM 1 BY 1 UNTIL WS-INDEX > 5
              IF WS-DIE(WS-INDEX) = 2
                 ADD 2 TO WS-RESULT
              END-IF
           END-PERFORM
           .

       CALC-THREES.
           PERFORM VARYING WS-INDEX FROM 1 BY 1 UNTIL WS-INDEX > 5
              IF WS-DIE(WS-INDEX) = 3
                 ADD 3 TO WS-RESULT
              END-IF
           END-PERFORM
           .

       CALC-FOURS.
           PERFORM VARYING WS-INDEX FROM 1 BY 1 UNTIL WS-INDEX > 5
              IF WS-DIE(WS-INDEX) = 4
                 ADD 4 TO WS-RESULT
              END-IF
           END-PERFORM
           .

       CALC-FIVES.
           PERFORM VARYING WS-INDEX FROM 1 BY 1 UNTIL WS-INDEX > 5
              IF WS-DIE(WS-INDEX) = 5
                 ADD 5 TO WS-RESULT
              END-IF
           END-PERFORM
           .

       CALC-SIXES.
           PERFORM VARYING WS-INDEX FROM 1 BY 1 UNTIL WS-INDEX > 5
              IF WS-DIE(WS-INDEX) = 6
                 ADD 6 TO WS-RESULT
              END-IF
           END-PERFORM
           .

       CALC-CHOICES.
           PERFORM VARYING WS-INDEX FROM 1 BY 1 UNTIL WS-INDEX > 5
              ADD WS-DIE(WS-INDEX) TO WS-RESULT
           END-PERFORM
           .

       CALC-YACHT.
           IF WS-DIE(1) = WS-DIE(2)
           AND WS-DIE(2) = WS-DIE(3)
           AND WS-DIE(3) = WS-DIE(4)
           AND WS-DIE(4) = WS-DIE(5)
           MOVE 50 TO WS-RESULT
           END-IF
           .

       CALC-FULL-HOUSE.
           PERFORM VARYING WS-INDEX FROM 1 BY 1 UNTIL WS-INDEX > 5
              ADD 1 TO WS-COUNT(WS-DIE(WS-INDEX))
           END-PERFORM
           PERFORM VARYING WS-INDEX FROM 1 BY 1 UNTIL WS-INDEX > 6
              IF WS-COUNT(WS-INDEX) = 3
                 MOVE 1 TO WS-HAS-THREE
              END-IF
              IF WS-COUNT(WS-INDEX) = 2
                 MOVE 1 TO WS-HAS-TWO
              END-IF
           END-PERFORM
           IF WS-HAS-THREE = 1 AND WS-HAS-TWO = 1
              PERFORM CALC-CHOICES
           END-IF
           .

       CALC-FOUR-OF-A-KIND.
           PERFORM VARYING WS-INDEX FROM 1 BY 1 UNTIL WS-INDEX > 5
              ADD 1 TO WS-COUNT(WS-DIE(WS-INDEX))
           END-PERFORM
           PERFORM VARYING WS-INDEX FROM 1 BY 1 UNTIL WS-INDEX > 6
              IF WS-COUNT(WS-INDEX) >= 4
                 COMPUTE WS-RESULT = WS-INDEX * 4
              END-IF
           END-PERFORM
           .

       CALC-LITTLE-STRAIGHT.
           PERFORM VARYING WS-INDEX FROM 1 BY 1 UNTIL WS-INDEX > 5
              ADD 1 TO WS-COUNT(WS-DIE(WS-INDEX))
           END-PERFORM
           IF WS-COUNT(1) = 1 AND WS-COUNT(2) = 1
           AND WS-COUNT(3) = 1 AND WS-COUNT(4) = 1
           AND WS-COUNT(5) = 1
              MOVE 30 TO WS-RESULT
           END-IF
           .

       CALC-BIG-STRAIGHT.
           PERFORM VARYING WS-INDEX FROM 1 BY 1 UNTIL WS-INDEX > 5
              ADD 1 TO WS-COUNT(WS-DIE(WS-INDEX))
           END-PERFORM
           IF WS-COUNT(2) = 1 AND WS-COUNT(3) = 1
           AND WS-COUNT(4) = 1 AND WS-COUNT(5) = 1
           AND WS-COUNT(6) = 1
              MOVE 30 TO WS-RESULT
           END-IF
           .