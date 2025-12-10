*     * FO5238*12/05/11 JCTE PROYECTO UPGRADE SYSTEMAT
000100 X0780-REJECT-TYPE.
000110****************************************************************  9916193
000120*   THE "GO TO" STATEMENT FOLLOWING THESE COMMENTS IS TO       *  9916193
000130*   BYPASS SAMPLE CODE (FOLLOWING THE "GO TO") WHICH MODIFIES  *  9916193
000140*   THE TRANSACTION BATCH NUMBER FOR VENDOR SPECIFIC NSF/OD    *  9916193
000150*   PROCESSING.  SIMILAR CODE FOR OTHER VENDORS MAY BE         *  9916193
000160*   ADDED HERE AND QUALIFIED BY BANK NUMBER IF NECESSARY.      *  9916193
000170****************************************************************  9916193
000190     GO TO X0780-REJECT-TYPE-EXIT.                                9916193
000200****************************************************************
000300*   INCLUDE EXCEPTION CODE ON TRANSACTION MAPPING FOLLOWS:     *
000400*   DESCRIPTION       IM EXEC PULL     V3000 CODE              *
000500*--------------------------------------------------------------*
000600*   ACCOUNT NOT FOUND      002          11  ACCOUNT NOT FOUND  *  0717565
000700*   AUDIT ACCOUNT          074          31  WATCH SIGNATURE    *  0717565
000800*   CAUTION ACCOUNT        076          31  WATCH SIGNATURE    *  0717565
000900*   CREDIT REJECTS        ANY LISTED    30  CREDIT EXCEPTIONS  *
001000*   ACCOUNT CLOSED         019/009      02  ACCOUNT CLOSED     *  0717565
001100*   DEBITS ONLY STATUS     005          13  BLOCKED ACCOUNT    *  0717565
001200*   NO ACCT ACTIVITY       010          13  BLOCKED ACCOUNT    *  0717565
001300*   NO CHECK ACTIVITY      011          13  BLOCKED ACCOUNT    *  0717565
001400*   CREDITS ONLY STATUS    051          13  BLOCKED ACCOUNT    *  0717565
001500*   HOLD ALL FUNDS         056          13  BLOCKED ACCOUNT    *  0717565
001600*   STOP HITS              012          04  STOP HITS          *  0717565
001700*   STOP SUSPECTS          SU           14  STOP SUSPECTS      *
001800*   DAU                    DU           03  DAU                *
001900*   OVERDRAWN   EXC-CODE = H2           12  OVERDRAWN          *
002000*   NSF (REJ-PROG = SPACE) 032          01  POSTED NSF         *  0717565
002100*   NSF POSTING PRI= 59    032          15  UNPOST NSF SECURITY*  0717565
002200*   NSF                    032          21  UNPOSTED NSF       *  0717565
002300*   WMS-STATUS IS  '06' OR '10'         32  DORMANT/ABANDONED  *
002400****************************************************************
002500     MOVE ZEROES TO EX13-BATCH.
002600     IF  EX13-TYPE EQUAL '1' OR '2' OR '5' OR '7'
002700         MOVE +20000 TO EX13-BATCH
002800*                       TR-BATCH                                  9915749
002900     ELSE
003000         MOVE +10000 TO EX13-BATCH.                               9915749
003100*                       TR-BATCH.                                 9915749
003200**********************
003300*  STOPS & SUSPECTS  *
003400**********************
003500     IF  (IMEX-CODE-1 EQUAL 'A0' OR 'A8')  AND
003600         IMEX-CODE-2 EQUAL 'A4'
003700         IF  EX13-TYPE EQUAL '1' OR '2' OR '5' OR '7'
003800             MOVE +20030 TO EX13-BATCH
003900         ELSE
004400             MOVE +10004 TO EX13-BATCH.                           0527333
004500*                           TR-BATCH.                             0527333
004505     IF  (IMEX-CODE-1 EQUAL 'AA')                                 0527333
004510         IF  EX13-TYPE EQUAL '1' OR '2' OR '5' OR '7'             0527333
004515             MOVE +20030 TO EX13-BATCH                            0527333
004520         ELSE                                                     0527333
004525             MOVE +10014 TO EX13-BATCH.                           0527333
004530*                           TR-BATCH.                             0527333
004600**********************
004700*  NSF               *
004800**********************
004900     IF  ((IMEX-CODE-1 EQUAL 'A0' OR 'A8')  AND
005000         IMEX-CODE-2 EQUAL 'A1') OR
005100         EX13-REJ-RESN EQUAL '032'                                0717565
005200         IF  EX13-TYPE EQUAL '1' OR '2' OR '5' OR '7'
005300             MOVE +20030 TO EX13-BATCH
005400         ELSE
005500             IF  EX13-REJ-PROG NOT EQUAL SPACES
005600                 IF  TR-POST-PRIORITY EQUAL '59'
005700                     MOVE +10015 TO EX13-BATCH
005800*                                   TR-BATCH                      9915749
005900                 ELSE
006000                     MOVE +10021 TO EX13-BATCH
006100*                                   TR-BATCH                      9915749
006200             ELSE
006300                 IF  WMS-OD-LIMIT EQUAL '9'
006400                     MOVE +10035 TO EX13-BATCH
006500*                                   TR-BATCH                      9915749
006600                 ELSE
006700                     MOVE +10001 TO EX13-BATCH.                   9915749
006800*                                   TR-BATCH.                     9915749
006900**********************
007000*  DAU               *
007100**********************
007200     IF  IMEX-CODE-1 EQUAL 'HJ'  OR
007300         EX13-REJ-RESN EQUAL 'DU '                                0717565
007400         MOVE '  ' TO EX13-REJ-PROG
007500         IF  EX13-TYPE EQUAL '1' OR '2' OR '5' OR '7'
007600             MOVE +20030         TO EX13-BATCH
007700*                                   TR-BATCH                      9915749
007800         ELSE
007900             MOVE +10003         TO EX13-BATCH.                   9915749
008000*                                   TR-BATCH.                     9915749
008100**********************
008200*  OVERDRAWN         *
008300**********************
008400     IF  IMEX-CODE-1 EQUAL 'H2'
008500         MOVE '  ' TO EX13-REJ-PROG
008600         IF  EX13-TYPE EQUAL '1' OR '2' OR '5' OR '7'
008700             MOVE +20030 TO EX13-BATCH
008800*                           TR-BATCH                              9915749
008900         ELSE
009000             MOVE +10012 TO EX13-BATCH.                           9915749
009100*                           TR-BATCH.                             9915749
009200**********************
009300*  DORMANT/ABANDONED *
009400**********************
009500     IF  (IMEX-CODE-1 EQUAL 'A0' OR 'A8')  AND
009600         (IMEX-CODE-2 EQUAL 'A2' OR 'A9')  AND
009700         EX13-REJ-RESN EQUAL '006'                                0717565
009800         IF EX13-TYPE EQUAL '1' OR '2' OR '5' OR '7'
009900             MOVE +20030 TO EX13-BATCH
010000*                           TR-BATCH                              9915749
010100         ELSE
010200             MOVE +10032 TO EX13-BATCH.                           9915749
010300*                           TR-BATCH.                             9915749
010400     IF  WMS-STATUS EQUAL '06' OR '10'
010500         IF EX13-TYPE EQUAL '1' OR '2' OR '5' OR '7'
010600             MOVE +20030 TO EX13-BATCH
010700*                           TR-BATCH                              9915749
010800         ELSE
010900             MOVE +10032 TO EX13-BATCH.                           9915749
011000*                           TR-BATCH.                             9915749
011100**********************
011200*  WATCH SIGNATURE   *
011300**********************
011400     IF  EX13-REJ-RESN EQUAL '074' OR '076'                       0717565
011500         IF  EX13-TYPE EQUAL '1' OR '2' OR '5' OR '7'
011600             MOVE +20030 TO EX13-BATCH
011700*                           TR-BATCH                              9915749
011800         ELSE
011900             MOVE +10031 TO EX13-BATCH.                           9915749
012000*                           TR-BATCH.                             9915749
012100**********************
012200*  BLOCKED ACCOUNT   *
012300**********************
012400     IF  EX13-REJ-RESN EQUAL
012500        '005' OR '010' OR '011' OR '051' OR '056'                 0717565
012600         IF  EX13-TYPE EQUAL '1' OR '2' OR '5' OR '7'
012700             MOVE +20030 TO EX13-BATCH
012800*                           TR-BATCH                              9915749
012900         ELSE
013000             MOVE +10013 TO EX13-BATCH.                           9915749
013100*                           TR-BATCH.                             9915749
013200**********************
013300*  ACCOUNT CLOSED    *
013400**********************
013500     IF  EX13-REJ-RESN EQUAL '019' OR '009' OR                    0717565
013600         WMS-STATUS EQUAL '04' OR '08'
013700         IF  EX13-TYPE EQUAL '1' OR '2' OR '5' OR '7'
013800             MOVE +20030 TO EX13-BATCH
013900*                           TR-BATCH                              9915749
014000         ELSE
014100             MOVE +10002 TO EX13-BATCH.                           9915749
014200*                           TR-BATCH.                             9915749
014300**********************
014400*  ACCOUNT NOT FOUND *
014500**********************
014600     IF  EX13-REJ-RESN EQUAL '002'                                0717565
014700         IF  EX13-TYPE EQUAL '1' OR '2' OR '5' OR '7'
014800             MOVE +20030 TO EX13-BATCH
014900*                           TR-BATCH                              9915749
015000         ELSE
015100             MOVE +10011 TO EX13-BATCH.                           9915749
015200*                           TR-BATCH.                             9915749
015300
015400 X0780-REJECT-TYPE-EXIT.
015500     EXIT.
