*     * FO5238*12/05/11 JCTE PROYECTO UPGRADE SYSTEMAT
000100*----------------------------------------------------------------*
000200*    PLAN ACCOUNT RECERTIFICATION PROCESSING                     *
000300*----------------------------------------------------------------*
000400 PL-RECERT-CKUP.
000500*----------------------------------------------------------------*
000600*    GENERATE 'U2' EXCEPTION FOR NEXT YEAR RECERTIFICATION.      *
000700*----------------------------------------------------------------*
000800     IF  WMS-PLN-TRLR-RECERT-REQD EQUAL 'Y'
000900     AND WMS-PLN-TRLR-NXT-SNG-FAM EQUAL 'U'
001000         MOVE '17' TO IMEX-REC-NO
001100         MOVE 'U2' TO IMEX-CODE-1
001200         PERFORM WRITE-EX-MST THRU WRITE-EX-EXIT.
001300*----------------------------------------------------------------*
001400*    PERFORM YEAR END RECERTIFICATION PROCESSING.                *
001500*----------------------------------------------------------------*
001600     IF  WBC-YEAR-END NOT EQUAL '1'
001700         GO TO PL-RECERT-CONT.
001800     IF  WMS-PLN-TRLR-RECERT-REQD EQUAL 'N'
001900         GO TO PL-RECERT-OFF.
002000
002100 PL-RECERT-ON.
002200*----------------------------------------------------------------*
002300*    RECERTIFICATION PROCESSING REQUIRED.                        *
002400*----------------------------------------------------------------*
002500*    IF THE NEXT YEAR SINGLE/FAMILY FLAG IS 'U', MOVE 'U' TO THE *
002600*    SINGLE/FAMILY FLAG AND LEAVE THE NEXT YEAR SINGLE/FAMILY    *
002700*    FLAG SET TO 'U'.                                            *
002800*----------------------------------------------------------------*
002900     IF  WMS-PLN-TRLR-NXT-SNG-FAM EQUAL 'U'
003000         MOVE 'U' TO WMS-PLN-TRLR-SNG-FAM-FLG
003100         GO TO PL-RECERT-CONT.
003200*----------------------------------------------------------------*
003300*    IF THE NEXT YEAR SINGLE/FAMILY FLAG IS 'N', MOVE 'N' TO THE *
003400*    SINGLE/FAMILY FLAG AND MOVE 'U' TO THE NEXT YEAR            *
003500*    SINGLE/FAMILY FLAG.                                         *
003600*----------------------------------------------------------------*
003700     IF  WMS-PLN-TRLR-NXT-SNG-FAM EQUAL 'N'
003800         MOVE 'N' TO WMS-PLN-TRLR-SNG-FAM-FLG
003900         MOVE 'U' TO WMS-PLN-TRLR-NXT-SNG-FAM
004000         GO TO PL-RECERT-CONT.
004100*----------------------------------------------------------------*
004200*    IF THE NEXT YEAR SINGLE/FAMILY FLAG IS 'S', MOVE 'S' TO THE *
004300*    SINGLE/FAMILY FLAG AND MOVE 'U' TO THE NEXT YEAR            *
004400*    SINGLE/FAMILY FLAG.                                         *
004500*----------------------------------------------------------------*
004600     IF  WMS-PLN-TRLR-NXT-SNG-FAM EQUAL 'S'
004700         MOVE 'S' TO WMS-PLN-TRLR-SNG-FAM-FLG
004800         MOVE 'U' TO WMS-PLN-TRLR-NXT-SNG-FAM
004900         GO TO PL-RECERT-CONT.
005000*----------------------------------------------------------------*
005100*    IF THE NEXT YEAR SINGLE/FAMILY FLAG IS 'F', MOVE 'F' TO THE *
005200*    SINGLE/FAMILY FLAG AND MOVE 'U' TO THE NEXT YEAR            *
005300*    SINGLE/FAMILY FLAG.                                         *
005400*----------------------------------------------------------------*
005500     IF  WMS-PLN-TRLR-NXT-SNG-FAM EQUAL 'F'
005600         MOVE 'F' TO WMS-PLN-TRLR-SNG-FAM-FLG
005700         MOVE 'U' TO WMS-PLN-TRLR-NXT-SNG-FAM
005800         GO TO PL-RECERT-CONT.
005900     GO TO PL-RECERT-CONT.
006000
006100 PL-RECERT-OFF.
006200*----------------------------------------------------------------*
006300*    RECERTIFICATION PROCESSING NOT REQUIRED.                    *
006400*----------------------------------------------------------------*
006500*    IF THE NEXT YEAR SINGLE/FAMILY FLAG IS 'U', NO CHANGE IS    *
006600*    MADE TO THE PLAN; THE SINGLE/FAMILY FLAG REMAINS AT ITS     *
006700*    CURRENT VALUE, AND THE NEXT YEAR SINGLE/FAMILY FLAG IS LEFT *
006800*    EQUAL TO 'U'.                                               *
006900*----------------------------------------------------------------*
007000     IF  WMS-PLN-TRLR-NXT-SNG-FAM EQUAL 'U'
007100         GO TO PL-RECERT-CONT.
007200*----------------------------------------------------------------*
007300*    IF THE NEXT YEAR SINGLE/FAMILY FLAG IS 'N', MOVE 'N' TO THE *
007400*    SINGLE/FAMILY FLAG AND LEAVE THE NEXT YEAR SINGLE/FAMILY    *
007500*    FLAG SET TO 'N'.                                            *
007600*----------------------------------------------------------------*
007700     IF  WMS-PLN-TRLR-NXT-SNG-FAM EQUAL 'N'
007800         MOVE 'N' TO WMS-PLN-TRLR-SNG-FAM-FLG
007900         GO TO PL-RECERT-CONT.
008000*----------------------------------------------------------------*
008100*    IF THE NEXT YEAR SINGLE/FAMILY FLAG IS 'S', MOVE 'S' TO THE *
008200*    SINGLE/FAMILY FLAG AND LEAVE THE NEXT YEAR SINGLE/FAMILY    *
008300*    FLAG SET TO 'S'.                                            *
008400*----------------------------------------------------------------*
008500     IF  WMS-PLN-TRLR-NXT-SNG-FAM EQUAL 'S'
008600         MOVE 'S' TO WMS-PLN-TRLR-SNG-FAM-FLG
008700         GO TO PL-RECERT-CONT.
008800*----------------------------------------------------------------*
008900*    IF THE NEXT YEAR SINGLE/FAMILY FLAG IS 'F', MOVE 'F' TO THE *
009000*    SINGLE/FAMILY FLAG AND LEAVE THE NEXT YEAR SINGLE/FAMILY    *
009100*    FLAG SET TO 'F'.                                            *
009200*----------------------------------------------------------------*
009300     IF  WMS-PLN-TRLR-NXT-SNG-FAM EQUAL 'F'
009400         MOVE 'F' TO WMS-PLN-TRLR-SNG-FAM-FLG
009500         GO TO PL-RECERT-CONT.
009600
009700 PL-RECERT-CONT.
009800     EXIT.
009900
010000*----------------------------------------------------------------*
010100*    GENERATE 'U1' EXCEPTION FOR CURRENT YEAR RECERTIFICATION.   *
010200*----------------------------------------------------------------*
010300     IF  WMS-PLN-TRLR-RECERT-REQD EQUAL 'Y'
010400     AND WMS-PLN-TRLR-SNG-FAM-FLG EQUAL 'U'
010500         MOVE '17' TO IMEX-REC-NO
010600         MOVE 'U1' TO IMEX-CODE-1
010700         PERFORM WRITE-EX-MST THRU WRITE-EX-EXIT.
010800
010900 PL-RECERT-EXIT.
011000     EXIT.
011100
