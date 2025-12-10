*     * FO5238*12/05/11 JCTE PROYECTO UPGRADE SYSTEMAT
000100*
000200*
000300*--------------------------------------------------------------*
000400*    IMPACS EXTENDED FLOAT RECORD WRITE COPYBOOK               *
000500*--------------------------------------------------------------*
000600*
000700 WRITE-EXT-FLOAT.
000800*--------------------------------------------------------------*
000900*    BYPASS WRITING THE EXTENDED FLOAT FILE IF                 *
001000*    - ACCOUNT HEADER RECORD (WMS-EXC-CODE = 0)                *
001100*    - EXTENDED FLOAT OPTION IS NOT USED AT THE BCR LEVEL      *
001200*    - ACCOUNT DOES NOT HAVE AN EXTENDED FLOAT RECORD          *
001300*--------------------------------------------------------------*
001400     IF  WMS-EXC-CODE         EQUAL '0'
001500         GO TO WRITE-EXT-FLOAT-END.
001600
001700     IF  WBC-EXTEND-FLOAT-OPT EQUAL '0'
001800         IF  WMS-EXTEND-FLOAT EQUAL '0'
001900             GO TO WRITE-EXT-FLOAT-END
002000         ELSE
002100             MOVE '0'            TO WMS-EXTEND-FLOAT
002200             GO TO WRITE-EXT-FLOAT-END.
002300
002400     IF  WMS-EXTEND-FLOAT     EQUAL '0'
002500     AND AFF-FLAG             EQUAL 'F'
002600         GO TO WRITE-EXT-FLOAT-CLEAR.
002700
002800*--------------------------------------------------------------*
002900*    BYPASS WRITING THE EXTENDED FLOAT RECORD AND RESET THE    *
003000*    EXTEND FLOAT FLAG ON THE ACCOUNT MASTER IF                *
003100*    - ACCOUNT NO LONGER HAS UNAVAILABLE FUNDS GREATER THAN    *
003200*      12 DAYS (THESE CAN BE CARRIED SOLELY ON ACCOUNT MASTER) *
003300*--------------------------------------------------------------*
003400     IF  AFF-MAX-ENTRY    LESS THAN +13
003500     OR  WFF-TOTAL-13-99      EQUAL +0
003600         MOVE '0'                TO WMS-EXTEND-FLOAT
003700         GO TO WRITE-EXT-FLOAT-CLEAR.
003800
003900*--------------------------------------------------------------*
004000*    THE AFF-MAX-ENTRY FIELD CONTAINS THE HIGHEST OCCURRENCE   *
004100*    (DAY) WITH A NON-ZERO FLOAT AMOUNT.  IT WILL BE USED TO   *
004200*    DETERMINE THE LENGTH OF THE RECORD WRITTEN.               *
004300*    IF THE BCR DAYS ARE REDUCED, THE FLOAT ENTRIES WILL BE    *
004400*    REDUCED AS THE FLOAT AMOUNTS AGE OFF THE RECORD.          *
004500*--------------------------------------------------------------*
004600     COMPUTE WFF-LENGTH       EQUAL AFF-FIXED
004700                                  + AFF-MAX-ENTRY
004800                                  * AFF-ENTRY-SIZE.
004900
005000     MOVE AFF-MAX-ENTRY          TO WFF-ENTRIES.
005100
005200     MOVE WFF-EXTENDED-FLOAT-REC TO EXTENDED-FLOAT-REC.
005300
005400     PERFORM WRITE-EXT-FLOAT-REC THRU WRITE-EXT-FLOAT-EXIT.
005500
005600     IF  WMS-EXTEND-FLOAT     EQUAL '0'
005700         MOVE '1'                TO WMS-EXTEND-FLOAT.
005800
005900*--------------------------------------------------------------*
006000*    WRITE EXTENDED FLOAT RECORD FOR INTERBRANCH TRANSFER.     *
006100*--------------------------------------------------------------*
006200     IF  IBT-96-FLAG          EQUAL '1'
006300         MOVE HOLD-WORK-ACCOUNT  TO FF-KEY
006400         PERFORM WRITE-IBT-FLOAT-REC
006500                               THRU WRITE-IBT-FLOAT-EXIT.
006600
006700 WRITE-EXT-FLOAT-CLEAR.
006800*--------------------------------------------------------------*
006900*    CLEAR EXTENDED FLOAT WORK FIELDS                          *
007000*--------------------------------------------------------------*
007100     MOVE CLEAR-EXTENDED-FLOAT-REC
007200                                 TO WFF-EXTENDED-FLOAT-REC.
007300     MOVE 'F'                    TO AFF-FLAG.
007400     MOVE +0                     TO AFF-MAX-ENTRY.
007500     GO TO WRITE-EXT-FLOAT-END.
007600
007700 WRITE-EXT-FLOAT-REC.
007800     MOVE 'O'                    TO I-O-CONTROL-ACCESS.
007900     MOVE 'L'                    TO I-O-CONTROL-OPERATOR.
008000     CALL 'IMAFFMV'           USING I-O-CONTROL-AREA
008100                                    EXTENDED-FLOAT-REC.
008200
008300 WRITE-EXT-FLOAT-EXIT.
008400     EXIT.
008500
008600 WRITE-IBT-FLOAT-REC.
008700     MOVE 'O'                    TO I-O-CONTROL-ACCESS.
008800     MOVE 'L'                    TO I-O-CONTROL-OPERATOR.
008900     CALL 'IMIBTTS'           USING I-O-CONTROL-AREA
009000                                    EXTENDED-FLOAT-REC.
009100 WRITE-IBT-FLOAT-EXIT.
009200     EXIT.
009300
009400 WRITE-EXT-FLOAT-END.
009500     EXIT.
009600
