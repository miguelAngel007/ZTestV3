*     * FO5238*12/05/11 JCTE PROYECTO UPGRADE SYSTEMAT
000100 PLAN-EDITS-SETUP.
000200*
000300     MOVE SPACES                    TO WK-PLAN-EDITS.
000400     MOVE LOW-VALUES                TO WK-PE-ERR-CD-TBL.
000500     SET PE-IDX                     TO +1.
000600     MOVE 'Y'                       TO WK-PE-MASTER.
000700     MOVE 'N'                       TO WK-PE-TR-INFO.
000800     MOVE WBC-CAPTURE-YR            TO WK-PE-RUN-YY.
000900     IF  WK-PE-RUN-YY LESS THAN WBC-NEXT-CENT-YR
001000         MOVE '20'                  TO WK-PE-RUN-CC
001100     ELSE
001200         MOVE '19'                  TO WK-PE-RUN-CC.
001300     MOVE WMS-PLN-TRLR-TYPE         TO WK-PE-PL-TYPE.
001400     MOVE WMS-PLN-TRLR-PART-EXC-FLG TO WK-PE-PART-EXC-FLG.
001500     MOVE WMS-PLN-TRLR-BENE-FLAG    TO WK-PE-BENE-FLG.
001600     MOVE WMS-PLN-TRLR-VALID-CONTR  TO WK-PE-VALID-CONT.
001700     MOVE WMS-PLN-TRLR-SNG-FAM-FLG  TO WK-PE-SNG-FAM-FLG.
001800     MOVE WMS-PLN-TRLR-DEATH-DATE   TO WK-PE-DEATH-DT.
001900*
002000 PLAN-EDITS-SETUP-EXIT.
002100     EXIT.
002200*
002300 PLAN-EDITS-SETUP-TRAN.
002400*
002500     MOVE 'Y'                       TO WK-PE-TR-INFO.
002600     MOVE 'IM80'                    TO WK-PE-PGM.
002700     MOVE TR-TRAN-TYPE              TO WK-PE-TR-TYPE.
002800     MOVE TR-OPTION (2)             TO WK-PE-OPT-2.
002900     MOVE TR-OPTION (6)             TO WK-PE-OPT-6.
003000     MOVE TR-OPTION (9)             TO WK-PE-OPT-9.
003100     MOVE TR-OPTION (11)            TO WK-PE-OPT-11.
003200     MOVE TR-OPTION (12)            TO WK-PE-OPT-12.
003300     MOVE TR-OPTION (13)            TO WK-PE-OPT-13.
003400     MOVE TR-OPTION (16)            TO WK-PE-OPT-16.
003500     MOVE TR-OPTION (18)            TO WK-PE-OPT-18.
003600*
003700 PLAN-EDITS-SETUP-TRAN-EXIT.
003800     EXIT.
003900*
004000 PL-ERR-CD-PROC.
004100*
004200     GO TO PL-ERR-CD-1    PL-ERR-CD-2    PL-ERR-CD-3
004300           PL-ERR-CD-4    PL-ERR-CD-5    PL-ERR-CD-6
004400           PL-ERR-CD-7    PL-ERR-CD-8    PL-ERR-CD-9
004500           PL-ERR-CD-10   PL-ERR-CD-11   PL-ERR-CD-12
004600           PL-ERR-CD-13   PL-ERR-CD-14   PL-ERR-CD-15
004700           PL-ERR-CD-16   PL-ERR-CD-17   PL-ERR-CD-18
004800           PL-ERR-CD-19   PL-ERR-CD-20
004900*
005000           DEPENDING ON WK-PE-ERR-CD (PE-IDX).
005100*
005200     GO TO PL-ERR-CD-PROC-EXIT.
005300*
005400 PL-ERR-CD-1.
005500*    *-----------------------------*
005600*    *    CUSTOMER NOT DISABLED    *
005700*    *-----------------------------*
005800     MOVE '038' TO REJRESN.
005900     GO             TO PL-ERR-CD-PROC-EXIT.
006000*
006100*
006200 PL-ERR-CD-2.
006300*    *-----------------------------*
006400*    *     CUSTOMER IS DISABLED    *
006500*    *-----------------------------*
006600     MOVE '038' TO REJRESN.
006700     GO             TO PL-ERR-CD-PROC-EXIT.
006800*
006900*
007000 PL-ERR-CD-3.
007100*    *-----------------------------*
007200*    *     DEATH DATE REQUIRED     *
007300*    *-----------------------------*
007400     MOVE '038' TO REJRESN.
007500     GO             TO PL-ERR-CD-PROC-EXIT.
007600*
007700*
007800 PL-ERR-CD-4.
007900*    *-----------------------------*
008000*    *   PL CD INVALID ON BENE     *
008100*    *-----------------------------*
008200     MOVE '038' TO REJRESN.
008300     GO             TO PL-ERR-CD-PROC-EXIT.
008400*
008500*
008600 PL-ERR-CD-5.
008700*    *-----------------------------*
008800*    *  PL CD/DEATH YEAR INVALID   *
008900*    *-----------------------------*
009000     MOVE '038' TO REJRESN.
009100     GO             TO PL-ERR-CD-PROC-EXIT.
009200*
009300*
009400 PL-ERR-CD-6.
009500*    *-----------------------------*
009600*    *  SECONDARY PL CODE INVALID  *
009700*    *-----------------------------*
009800     MOVE '038' TO REJRESN.
009900     GO             TO PL-ERR-CD-PROC-EXIT.
010000*
010100*
010200 PL-ERR-CD-7.
010300*    *-----------------------------*
010400*    *  PL CD/BENE INCONSISTENT    *
010500*    *-----------------------------*
010600     GO             TO PL-ERR-CD-PROC-EXIT.
010700*
010800*
010900 PL-ERR-CD-8.
011000*    *-----------------------------*
011100*    *  INVALID FOR CONTR          *
011200*    *-----------------------------*
011300     MOVE '038' TO REJRESN.
011400     GO             TO PL-ERR-CD-PROC-EXIT.
011500*
011600*
011700 PL-ERR-CD-9.
011800*    *-----------------------------*
011900*    *  INV PL CD FOR TAX ADJ      *
012000*    *-----------------------------*
012100     MOVE '038' TO REJRESN.
012200     GO             TO PL-ERR-CD-PROC-EXIT.
012300*
012400*
012500 PL-ERR-CD-10.
012600*    *-----------------------------*
012700*    *  INVALID PLAN CODE          *
012800*    *-----------------------------*
012900     MOVE '038' TO REJRESN.
013000     GO             TO PL-ERR-CD-PROC-EXIT.
013100*
013200 PL-ERR-CD-11.
013300*    *-----------------------------*
013400*    * PL CD NOT VALID WITH TR OPT *
013500*    *-----------------------------*
013600     MOVE '038' TO REJRESN.
013700     GO             TO PL-ERR-CD-PROC-EXIT.
013800*
013900 PL-ERR-CD-12.
014000*    *-------------------------------------------*
014100*    * SECONDARY PL CODE INVALID ON CONTRIBUTION *
014200*    *-------------------------------------------*
014300     MOVE '038' TO REJRESN.
014400     GO             TO PL-ERR-CD-PROC-EXIT.
014500*
014600 PL-ERR-CD-13.
014700*    *-----------------------------------*
014800*    * PL CODE INVALID FOR HSA           *
014900*    *-----------------------------------*
015000     MOVE '038' TO REJRESN.
015100     GO             TO PL-ERR-CD-PROC-EXIT.
015200*
015300*
015400 PL-ERR-CD-14.
015500*    *------------------------------*
015600*    * INT DIST TYP INVALID HSA     *
015700*    *------------------------------*
015800     MOVE '038' TO REJRESN.
015900     GO             TO PL-ERR-CD-PROC-EXIT.
016000*
016100*
016200 PL-ERR-CD-15.
016300*    *---------------------------------*
016400*    * PLAN ACCOUNT OWNER IS DECEASED  *
016500*    *---------------------------------*
016600     MOVE '038' TO REJRESN.
016700     GO             TO PL-ERR-CD-PROC-EXIT.
016800*
016900*
017000 PL-ERR-CD-16.
017100*    *-------------------------------------*
017200*    * IF PL DIST CODE EQ 04/35/36/45 PART *
017300*    * EXC FLAG MUST EQ 2 OR A BENE ACCT   *
017400*    *-------------------------------------*
017500     MOVE '038' TO REJRESN.
017600     GO             TO PL-ERR-CD-PROC-EXIT.
017700*
017800*
017900 PL-ERR-CD-17.
018000*    *-----------------------------------*
018100*    * INVALID SECONDARY CODE AND        *
018200*    * TYPE COMBINATIONS                 *
018300*    *-----------------------------------*
018400     MOVE '038' TO REJRESN.
018500     GO             TO PL-ERR-CD-PROC-EXIT.
018600*
018700*
018800 PL-ERR-CD-18.
018900*    *-----------------------------*
019000*    *  INV PL CD SECONDARY PL CD  *
019100*    *  COMBINATION                *
019200*    *-----------------------------*
019300     MOVE '038' TO REJRESN.
019400     GO             TO PL-ERR-CD-PROC-EXIT.
019500*
019600 PL-ERR-CD-19.
019700*    *------------------------------*
019800*    * INVALID COMBINATION CREDIT   *
019900*    *------------------------------*
020000     MOVE '038' TO REJRESN.
020100     GO             TO PL-ERR-CD-PROC-EXIT.
020200*
020300 PL-ERR-CD-20.
020400*    *------------------------------*
020500*    * INVALID COMBINATION DEBIT    *
020600*    *------------------------------*
020700     MOVE '038' TO REJRESN.
020800     GO             TO PL-ERR-CD-PROC-EXIT.
020900*
050000*
050100 PL-ERR-CD-PROC-EXIT.
050200     EXIT.
