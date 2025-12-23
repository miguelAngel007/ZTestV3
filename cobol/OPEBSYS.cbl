      ****************************************************************
      * 200570 18/10/23 AEGM DETERMINA TIP COBOL (4.2 O 6.3)         *
      ****************************************************************
       IDENTIFICATION  DIVISION.
      *-------------------------*
       PROGRAM-ID.    OPEBSYS.
       AUTHOR.        ANGOMA.
      *---------------------*
       ENVIRONMENT DIVISION.
      *---------------------*
       CONFIGURATION SECTION.
       INPUT-OUTPUT  SECTION.
       FILE-CONTROL.

       DATA DIVISION.
      *--------------*
       FILE SECTION.


       WORKING-STORAGE SECTION.
      *------------------------*
       01  SW-FOUND              PIC X(01) VALUE SPACES.
       01  WS-APL                PIC X(03) VALUE SPACES.
       01  TBL-APLIC             PIC X(15) VALUE 'AAABBBCCCDDDEEE'.
       01  TABLA-APLIC.
           05 WS-APLIC OCCURS 5 TIMES INDEXED BY INDX-TBL.
              07 WS-APLIC-C42    PIC X(03).

       LINKAGE SECTION.
      *---------------*
       01  COM-DATA.
           02 COM-LENGTH         PIC X(02).
           02 COM-PARAMETR       PIC X(33).

      *---------------------------------*
       PROCEDURE DIVISION USING COM-DATA.
      *---------------------------------*
           MOVE TBL-APLIC TO TABLA-APLIC.
           UNSTRING COM-PARAMETR
           DELIMITED BY ','
           INTO WS-APL
           END-UNSTRING.

           SEARCH WS-APLIC
           AT END MOVE 'N' TO SW-FOUND
             WHEN WS-APL = WS-APLIC-C42 (INDX-TBL)
                  MOVE 'S' TO SW-FOUND
           END-SEARCH.

           IF SW-FOUND = 'S'
              MOVE 0 TO RETURN-CODE
           ELSE
              MOVE 4 TO RETURN-CODE
           END-IF.

           GOBACK.