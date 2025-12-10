*     * FO5238*12/05/11 JCTE PROYECTO UPGRADE SYSTEMAT
000100*--------------------------------------------------------------*
000200*    IMPDREFF IS A PROCEDURE DIVISION COPYBOOK THAT DETERMINES *
000300*    WHETHER REGULATION E IS IN EFFECT FOR THE ACCOUNT BEING   *
000400*    PROCESSED.                                                *
000500*     - EFFECTIVE DATE FOR ACCOUNTS OPENED BEFORE JULY 1,2010  *
000600*       IS AUGUST 15, 2010 (EXISTING ACCOUNTS)                 *
000700*     - EFFECTIVE DATE FOR ACCOUNTS OPENED ON OR AFTER JULY 1, *
000800*       2010, IS JULY 1, 2010 (NEW ACCOUNTS)                   *
000900*                                                              *
001000*    THIS COPYBOOK REQUIRES THE PRESENCE OF THE FOLLOWING      *
001100*    IN THE USING PROGRAM:                                     *
001200*     - IMAWKMST COPYBOOK                                      *
001300*     - IMWRKBC1 COPYBOOK (MAY MOVE IMABCRB1 FIELDS TO IT)     *
001400*     - SIWSDTAR COPYBOOK                                      *
001500*     - WS-REGE-EFF WORK FIELD                                 *
001600*       INITIALIZE TO SPACE (ROUTINE HAS NOT BEEN CALLED)      *
001700*       VALUES IN USE BEFORE JULY 1 AND AFTER AUGUST 15, 2010  *
001800*       MUST ONLY BE CALCULATED ONE TIME                       *
001900*         X = REG E NOT IN EFFECT FOR ANY ACCOUNTS             *
002000*         A = REG E IN EFFECT FOR ALL ACCOUNTS                 *
002100*       VALUES IN USE ONLY BETWEEN JULY 1 AND AUGUST 15, 2010  *
002200*       MUST BE CALCULATED FOR EACH ACCOUNT                    *
002300*         Y = REG E IN EFFECT FOR THIS ACCOUNT                 *
002400*         N = REG E NOT IN EFFECT FOR THIS ACCOUNT             *
002500*                                                              *
002600*    THE IMPDREFF COPYBOOK MUST BE FOLLOWED IMMEDIATELY BY A   *
002700*    PARAGRAPH NAME, AS THE FINAL STATEMENT IS AN EXIT.        *
002800*--------------------------------------------------------------*
002900
003000 REGE-EFF-DATE.
003100     IF  WS-REGE-EFF          EQUAL 'A' OR 'X'
003200         GO TO REGE-EFF-EXIT.
003300
003400     IF  WS-REGE-EFF      NOT EQUAL ' '
003500         GO TO REGE-EFF-ACCT.
003600
003700*--------------------------------------------------------------*
003800*    EXCEPTION PROCESSING MODULES WILL GENERATE TRANSACTIONS   *
003900*    AND CHARGES FOR THE NEXT DAY, AND MUST USE THE NEXT       *
004000*    PROCESSING DATE TO DETERMINE WHETHER REG E IS IN EFFECT.  *
004100*    ALSO CHECK THE IM19 RUN COMPLETE FLAG TO SEE IF DATES     *
004200*    HAVE BEEN SET UP FOR THE NEXT NIGHT'S PROCESSING.         *
004300*--------------------------------------------------------------*
004400     IF  PROGRAM-NAME (1:6)   EQUAL 'IM41PX' OR 'IM41NY'
004500         IF  WBC-IM19-COMP    EQUAL '0'
004600             MOVE WBC-NEXT-CAPTURE-DATE
004700                                 TO DT-LOW-DATE
004800         ELSE
004900             MOVE WBC-CAPTURE-DATE
005000                                 TO DT-LOW-DATE
005100     ELSE
005200         IF  WBC-IM19-COMP    EQUAL '0'
005300             MOVE WBC-CAPTURE-DATE
005400                                 TO DT-LOW-DATE
005500         ELSE
005600             MOVE WBC-LAST-CAPTURE-DATE
005700                                 TO DT-LOW-DATE.
005800
005900*--------------------------------------------------------------*
006000*    REG E IN FULL EFFECT FOR ALL ACCOUNTS ON AUGUST 15, 2010  *
006100*--------------------------------------------------------------*
006200     MOVE WS-REGE-EFF-DT-EXIST   TO DT-HIGH-DATE.
006300
006400     CALL 'SIDIF1'            USING DATE-AREA.
006500     IF  RET-DAYS             EQUAL ZERO
006600         MOVE 'A'                TO WS-REGE-EFF
006700         GO TO REGE-EFF-EXIT.
006800
006900*--------------------------------------------------------------*
007000*    REG E NOT IN EFFECT FOR ANY ACCOUNT BEFORE JULY 1, 2010   *
007100*--------------------------------------------------------------*
007200     MOVE WS-REGE-EFF-DT-NEW     TO DT-HIGH-DATE.
007300
007400     CALL 'SIDIF1'            USING DATE-AREA.
007500     IF  RET-DAYS         NOT EQUAL ZERO
007600         MOVE 'X'                TO WS-REGE-EFF
007700         GO TO REGE-EFF-EXIT.
007800
007900*--------------------------------------------------------------*
008000*    DETERMINE IF IN EFFECT ON AN ACCOUNT BY ACCOUNT BASIS     *
008100*--------------------------------------------------------------*
008200
008300 REGE-EFF-ACCT.
008400     MOVE ' '                    TO WS-REGE-EFF.
008500
008600*--------------------------------------------------------------*
008700*    PROCESS DATE IS BETWEEN JULY 1 AND AUGUST 15, 2010        *
008800*    ONLY ACCOUNTS OPENED JULY 1 AND AFTER MUST COMPLY         *
008900*--------------------------------------------------------------*
009000     MOVE WMS-DATE-OPENED        TO DT-LOW-DATE.
009100     MOVE WS-REGE-EFF-DT-NEW     TO DT-HIGH-DATE.
009200
009300     CALL 'SIDIF1'            USING DATE-AREA.
009400     IF  RET-DAYS             EQUAL ZERO
009500         MOVE 'Y'                TO WS-REGE-EFF
009600     ELSE
009700         MOVE 'N'                TO WS-REGE-EFF.
009800
009900 REGE-EFF-EXIT.
010000     EXIT.
010100
