***************************(COPY IMPDMEM1)******************************00000010
***   * 407115 16/01/24 AMG  CUENTA EXCLUSIVA PAGO DE REMUNERACIONES   *
***   * 901179 22/04/19 JCSC MEJORAS AL PROCESO BATCH DE IMPACS       **
***   * 705602 26/12/17 JCTE CTAS.CTES BPE. CONTROL CAS-BATCH         **
*     * 105967*29/04/15 JEFV NUEVO PRODUCTO DINERO ELECTRONICO
***   * 105411 09/01/15 JCTE NUEVO PRODUCTO DEPOSITO CTACTES ESCROW    *00000020
***   * FO4104 21/03/12 JCSC USAR MSG IM0126 EN VEZ DE IM8251 & IM8252 *00000020
*     * FO5238*12/05/11 JCTE PROYECTO UPGRADE SYSTEMAT
************************************************************************00000010
000001*----------------------------------------------------------------*0266725
000002*                **  HISTORY OF REVISION **                      *0266725
000003*                                                                *0266725
000004* DATE     DESCRIPTION                                   CHANGID *0266725
000005* -------- -------------------------------------------   ------- *0266725
000006* 15/09/10 REAPPLY CODE: SYSTEM TYPE VS PRODUCT VALIDATE IMIBXX  *IMIBXX
000007* 15/09/10 REAPPLY CODE: ACCOUNT STATUS HANDLING         IMIB004 *IMIB004
000008* 15/09/10 REAPPLY CODE: NEW FIELD RETENCION MEGA        IMIB78  *IMIB78
000009* 15/09/10 REAPPLY CODE:PROPAGATE MISC MEMO CUSTOM FIELDS IMIBXXX*IMIBXXX
000010* 19/04/11 MOVE 12 BYTE CUSTOMER NUMBER TO MEMO          IMUP021 *IMUP021
000011* 30/05/11 DO NOT INCLUDE DAY 1 FLOAT IN THE AVAILABLE   IMIB85  *IMIB85
000012*          BALANCE BUT INCLUDE IN TOTAL UNAVAILABLE AMT          *IMIB85
FO4104* 21/03/12 JCSC    MSG IM0126 INSTEAD OF IM8251 & IM8252 U21MAR  *IMIBXXX
000099*                                                                *
000100*----------------------------------------------------------------*1003624
000200*                                                                *1003624
000300*    THIS COPYBOOK LOADS INTIAL VALUES INTO THE MEMO RECORD.     *1003624
000400*                                                                *1003624
000500*    THIS COPYBOOK IS USED IN THE FOLLOWING PROGRAMS:            *0416925
000600*                                                                *1003624
000700*    FIIMLOAD    IM31                                            *0416925
000800*                                                                *1003624
000900*----------------------------------------------------------------*1003624
001000                                                                  1003624
001100     MOVE SPACES            TO IM-MEMO-OL-AREA.
001110     MOVE ZEROES            TO IM-MEMO-OL-KEY                     IMIB004
001120                               IM-MEMO-OL-STAT-CHG-SEQ.           IMIB004
001200     MOVE WMS-CONTROL-KEY   TO IM-MEMO-OL-KEY.
001300     MOVE WMS-EXC-CODE      TO IM-MEMO-OL-RECORD-ID.              1003624
001320     MOVE WMS-RED-ALERT-FLAG                                      0837826
001340                            TO IM-MEMO-OL-RED-ALERT-FLAG.         0837826
001400     MOVE HIGH-VALUES       TO IM-MEMO-OL-CUR-LOG-RBA             IM002
001500                               IM-MEMO-OL-TS-CUR-LOG-RBA          IM002
001600                               IM-MEMO-OL-MIR-RBA-X.              IM002
001700*                                                                 9915749
001800     MOVE ZEROS             TO IM-MEMO-OL-LTT-AMT
001850                               IM-MEMO-OL-NAME-ADDR-FLAG          9915868
001900                               IM-MEMO-OL-DR-NUM
002000                               IM-MEMO-OL-DR-AMT
002100                               IM-MEMO-OL-CR-NUM
002200                               IM-MEMO-OL-CR-AMT
002300                               IM-MEMO-OL-SV-DR-NUM
002400                               IM-MEMO-OL-SV-DR-AMT
002500                               IM-MEMO-OL-SV-CR-NUM
002600                               IM-MEMO-OL-SV-CR-AMT
002700                               IM-MEMO-OL-LN-DR-NUM
002800                               IM-MEMO-OL-LN-DR-AMT
002900                               IM-MEMO-OL-LN-CR-NUM
003000                               IM-MEMO-OL-LN-CR-AMT.
003010     MOVE +90000            TO IM-MEMO-OL-NEXT-ACCUM-SEQ.         0447264
003020     MOVE +9000             TO IM-MEMO-OL-NEXT-STOP-SEQ.          0447264
003100*                                                                 9915749
003200     MOVE ZEROES            TO IM-MEMO-OL-AVAIL-BAL
003300                               IM-MEMO-OL-CURRENT-BAL             IM002
003310                               IM-MEMO-OL-REGDD-BAL               0930023
003400                               IM-MEMO-OL-LEDGER-BAL.
003500     MOVE ZEROES            TO IM-MEMO-OL-BEGIN-HOLD-AMT          IM004
003600                               IM-MEMO-OL-HOLD-AMT-TODAY
003700                               IM-MEMO-OL-UNAVAILABLE-AMT         IM007
003800                               IM-MEMO-OL-OD-AMT                  IM004
003900                               IM-MEMO-OL-MIN-BAL                 IM004
003910                               IM-MEMO-OL-XINV-BAL                2016639
004000                               IM-MEMO-OL-SV-AVAIL-BAL            IM002
004010                               IM-MEMO-OL-WL-LOAN-AVAIL           0327005
004015                               IM-MEMO-OL-XC-CARD-AVAIL           0617492
004100                               IM-MEMO-OL-LN-AVAIL-BAL            IM002
004200                               IM-MEMO-OL-PRIN-BAL                IM002
004300                               IM-MEMO-OL-LST-CUST-DEP            IM004
004400                               IM-MEMO-OL-OTHER-CHGS              IM002
004500                               IM-MEMO-OL-INTRST-BAL              IM002
004510                               IM-MEMO-OL-STAT-CHG-SEQ            IMIB004
004520                               IM-MEMO-OL-OD-CYC-ACR              IMIBXX
004600                               IM-MEMO-OL-IOD-YTD-INT-PD          IM002
004700                               IM-MEMO-OL-IOD-MTD-INT-PD          IM002
004800                               IM-MEMO-OL-IOD-CYC-INT-PD          IM002
004900                               IM-MEMO-OL-MTD-AGGR-BAL            IM004
005000                               IM-MEMO-OL-INTEREST-RATE           IM004
005100                               IM-MEMO-OL-INT-ACCRUED             IM002
005200                               IM-MEMO-OL-AMT-LST-MON-TRAN        IM004
005300                               IM-MEMO-OL-DDA-BAL                 IM006
005400                               IM-MEMO-OL-CASH-AVAIL-AMT          IM007
005500                               IM-MEMO-OL-PM-CASH-AVAIL           IM007
005510                               IM-MEMO-OL-RETENCION-MEGA          IMIB74
005520                               IM-MEMO-OL-RETENC-TRAMITE          IMIBXX
005530                               IM-MEMO-OL-TARGET-AMOUNT           IMIBXX
005540                               IM-MEMO-OL-STP-NUM                 IMIBXXX
005600                               IM-MEMO-OL-SV-INT-ACCRUED          0902834
005700                               IM-MEMO-OL-SV-YTD-INT-PD.          0902834
005800     MOVE ZEROES            TO IM-MEMO-OL-GL-CODE                 IM004
005900                               IM-MEMO-OL-IOD-RATE-PTR            IM004
006000                               IM-MEMO-OL-HIFI-INDICATOR.         IM007
006100     MOVE ZEROES            TO IM-MEMO-OL-STOP-PAYS               IM007
006200                               IM-MEMO-OL-MTD-AGGR-DAYS
006300                               IM-MEMO-OL-YTD-TIMES-NSF           IM003
006400                               IM-MEMO-OL-YTD-TIMES-OD.           IM003
006500     MOVE +5000             TO IM-MEMO-EFA-LARGE-DEP-AMT.         IM007
006600     MOVE ZEROES            TO IM-MEMO-NB-MAIL-RTN-CNT.           IM007
006700     MOVE ZEROES            TO IM-MEMO-NB-TOT-DMT-SC.             9715504
006800     MOVE ZEROES            TO IM-MEMO-NB-SC-OD-TIMES             9715504
006900                               IM-MEMO-NB-MTD-NSF-TIMES           9715504
007000                               IM-MEMO-NB-DAYS-OD-MO-1            9715504
007100                               IM-MEMO-NB-TIMES-NSF-MO-1          9715504
007200                               IM-MEMO-NB-TIMES-CB-MO-1           9715504
007300                               IM-MEMO-NB-REGD-NTC-NBR            9715504
007400                               IM-MEMO-NB-REGD-TR-VIOL            9715504
007500                               IM-MEMO-NB-REGD-CK-VIOL.           9715504
007600     MOVE ZEROES            TO IM-MEMO-NB-MIN-FND-BAL             9715504
007700                               IM-MEMO-NB-MIN-SWP-AMT.            9715504
007800*                                                                 IM003
007900     MOVE WMS-IT-TRAN-FLAGS TO IM-MEMO-IT-TRAN-FLAGS.             9915749
008000*                                                                 IM005
008100     MOVE WMS-CURR-BAL      TO IM-MEMO-OL-AVAIL-BAL
008200                               IM-MEMO-OL-CURRENT-BAL             IM002
008202                               IM-MEMO-OL-REGDD-BAL               0930023
008210                               IM-MEMO-OL-LEDGER-BAL.             9916157
008220     SUBTRACT WMS-IOD-PEND-TAX FROM IM-MEMO-OL-AVAIL-BAL.         9916157
008222     SUBTRACT WMS-IOD-PEND-TAX FROM IM-MEMO-OL-REGDD-BAL.         0930023
008224     IF  WMS-SAVINGS-TRLR EQUAL '1'                               0930023
008226         SUBTRACT WMS-SAV-PEND-TAX FROM IM-MEMO-OL-REGDD-BAL.     0930023
008230     MOVE WMS-DDA-BAL       TO IM-MEMO-OL-DDA-BAL.                9916157
008240     MOVE WMS-IOD-PROJ-ACCR TO IM-MEMO-OL-INT-ACCRUED.            9916157
008242     MOVE WMS-PROV-HOLD-AMT TO IM-MEMO-OL-PROV-HOLD-AMT.          0927850
008243     SUBTRACT WMS-PROV-HOLD-AMT FROM IM-MEMO-OL-AVAIL-BAL.        0927850
008244     SUBTRACT WMS-PROV-HOLD-AMT FROM IM-MEMO-OL-REGDD-BAL.        0930023
008245*--------------------------------------------------------------*  2016639
008250*    EXTERNAL INVESTMENTS ARE INCLUDED IN ONLINE AVAILABLE BAL *  2016639
008255*--------------------------------------------------------------*  2016639
008260     IF  (WMS-XINV-LINK-IND EQUAL 'I' OR 'L' OR 'P')              2016639
008265     AND (WMS-XINV-RESTRICT NOT EQUAL 'A' AND 'R')                2016639
008270         MOVE WMS-XINV-BALANCE  TO IM-MEMO-OL-XINV-BAL            2016639
008275         ADD  WMS-XINV-BALANCE  TO IM-MEMO-OL-AVAIL-BAL.          2016639
008300*                                                                 9916157
008305***************************************************************** 9916157
008310*  A NEW FIELD (WMS-OL-CALC) HAS BEEN ADDED TO THE IMACTM FILE    9916157
008315*  TO ALLOW THE ONLINE AVAILABLE BALANCE CALCULATION TO BE        9916157
008320*  SET AT THE ACCOUNT LEVEL. BEFORE THIS CHANGE THE CALCULATION   9916157
008325*  WAS DETERMINED AT THE BCR LEVEL.                               9916157
008330*                                                                 9916157
008335*  IF THE NEW FIELD IS BEING USED (WMS-OL-CALC1 NOT EQUAL 0)      9916157
008340*  THEN THE VALUE IN WMS-OL-CALC1 IS USED TO DETERMINE            9916157
008345*  WHAT CALCULATIONS WILL AFFECT THE ONLINE AVAILABLE             9916157
008350*  BALANCE. BCR OVERRIDES WILL NOT BE CHECKED.                    9916157
008355*                                                                 9916157
008360*  IF THE NEW FIELD IS TURNED OFF (WMS-OL-CALC1 EQUAL 0)          9916157
008365*  THEN EACH BCR OVERRIDE IS CHECKED TO DETERMINE                 9916157
008370*  WHAT CALCULATIONS WILL AFFECT THE ONLINE AVAILABLE             9916157
008375*  BALANCE.                                                       9916157
008380***************************************************************** 9916157
008385*                                                                 9916157
008390     MOVE WMS-SAVINGS-TRLR  TO IM-MEMO-OL-SAVINGS-TRLR.           9916157
008395     IF  WMS-SAVINGS-TRLR EQUAL '1'                               9916157
008400         MOVE WMS-ACCR-INT         TO   IM-MEMO-OL-SV-INT-ACCRUED 9916157
008405         MOVE WMS-YTD-INT-PAID     TO   IM-MEMO-OL-SV-YTD-INT-PD  9916157
008410         MOVE WMS-INT-CUR-BAL      TO   IM-MEMO-OL-SV-AVAIL-BAL   9916157
008415         SUBTRACT WMS-SAV-PEND-TAX FROM IM-MEMO-OL-SV-AVAIL-BAL   9916157
008420         MOVE WMS-INT-STATUS       TO   IM-MEMO-OL-INT-STATUS.    9916157
008421*                                                                 0416925
008422*----------------------------------------------------------------*0416925
008423*    MOVE ZEROS TO MEMO AVAILABLE BALANCE IF HOLD ALL FLAG IS ON *0416925
008424*                                         OR ACCOUNT IS CLOSED   *0416925
008425*----------------------------------------------------------------*0416925
008426     IF  WMS-HOLD-ALL-FUNDS-FLAG EQUAL '1'                        0416925
008427     OR (WMS-HOLD-ALL-FUNDS-FLAG EQUAL '2' AND                    0416925
008428         WBC-ONLINE-HOLD-FLAG EQUAL '0')                          0416925
008429     OR  WMS-STATUS EQUAL '04'                                    0416925
008430         MOVE ZEROS              TO IM-MEMO-OL-AVAIL-BAL          0416925
008431         GO TO IMPDMEM1-CONT.                                     0416925
008432*                                                                 0416925
008433     IF  WMS-OL-CALC1 EQUAL '0'                                   0416925
008435         GO TO IMPDMEM1-BCR-OVERRIDES.                            9916157
008440*                                                                 9916157
008445     IF  (WMS-OL-CALC1 EQUAL '1' OR '3' OR '5' OR '7' OR          9916157
008450                             '9' OR 'B' OR 'D' OR 'F')            9916157
008455         IF  WMS-OD-LIMIT-FILE EQUAL '1'                          9916157
008460             ADD WMS-OD-LIMIT-AMT-ONLINE TO IM-MEMO-OL-AVAIL-BAL  9916157
008465                                                                  9916157
008470         ELSE                                                     9916157
008475         IF  WMS-OD-LIMIT EQUAL '0' OR 'L' OR 'S'                 9916157
008480             ADD WMS-OD-LIMIT-AMT        TO IM-MEMO-OL-AVAIL-BAL  9916157
008485                                                                  9916157
008490         ELSE                                                     9916157
008495         IF  WMS-OD-LIMIT EQUAL  'A'                              9916157
008500             MOVE +9999999999999.99      TO IM-MEMO-OL-AVAIL-BAL  9916157
008505                                                                  9916157
008510         ELSE                                                     9916157
008515         IF  WMS-OD-LIMIT IS NUMERIC                              9916157
008520             ADD WBC-OD-LIMIT (WMS-OD-LIMIT-9)                    0266741
008525                                         TO IM-MEMO-OL-AVAIL-BAL. 9916157
008530*                                                                 9916157
008535     IF  WMS-LOAN-TRLR EQUAL '1'                                  9916157
008537     AND WMS-LOAN-STATUS NOT EQUAL '5'                            0325985
008538       IF  WMS-OD-LIMIT EQUAL 'A'                                 0326952
008539       AND (WMS-OL-CALC1        EQUAL '1' OR '3' OR '5' OR '7' OR 0737706
008540                                      '9' OR 'B' OR 'D' OR 'F')   0326952
008541         NEXT SENTENCE                                            0326952
008542       ELSE                                                       0326952
008543         IF  (WMS-OL-CALC1 EQUAL '2' OR '3' OR '6' OR '7' OR      0326952
008545                                 'A' OR 'B' OR 'E' OR 'F')        9916157
008550             ADD WMS-AVAIL-BAL           TO IM-MEMO-OL-AVAIL-BAL. 9916157
008555*                                                                 9916157
008560     IF  WMS-SAVINGS-TRLR EQUAL '1'                               9916157
008561       IF  WMS-OD-LIMIT EQUAL 'A'                                 0326952
008562       AND (WMS-OL-CALC1        EQUAL '1' OR '3' OR '5' OR '7' OR 0737706
008563                                      '9' OR 'B' OR 'D' OR 'F')   0326952
008564         NEXT SENTENCE                                            0326952
008565       ELSE                                                       0326952
008567         IF  (WMS-OL-CALC1 EQUAL '8' OR '9' OR 'A' OR 'B' OR      0326952
008570                                 'C' OR 'D' OR 'E' OR 'F')        9916157
008575             SUBTRACT WMS-INT-CUR-BAL  FROM IM-MEMO-OL-AVAIL-BAL  9916157
008580         ELSE                                                     9916157
008585             SUBTRACT WMS-SAV-PEND-TAX FROM IM-MEMO-OL-AVAIL-BAL. 9916157
008590*                                                                 9916157
008595     IF  WMS-OL-CALC2 EQUAL '2' OR '5' OR '6'                     9916157
008600         NEXT SENTENCE                                            9916157
008605     ELSE                                                         9916157
008610         GO TO IMPDMEM1-NEXT.                                     9916157
008615     MOVE WMS-HOLD-AMT      TO IM-MEMO-OL-BEGIN-HOLD-AMT.         9916157
008620     IF  (WMS-OL-CALC1 EQUAL '4' OR '5' OR '6' OR '7' OR          9916157
008625                             'C' OR 'D' OR 'E' OR 'F')            9916157
008630         SUBTRACT WMS-DROP-HOLD-AMOUNT                            9916157
008635                          FROM IM-MEMO-OL-BEGIN-HOLD-AMT.         9916157
008640     IF  IM-MEMO-OL-BEGIN-HOLD-AMT GREATER THAN ZERO              9916157
008641       IF  WMS-OD-LIMIT EQUAL 'A'                                 0326952
008642       AND (WMS-OL-CALC1        EQUAL '1' OR '3' OR '5' OR '7' OR 0737706
008643                                      '9' OR 'B' OR 'D' OR 'F')   0326952
008644           GO TO IMPDMEM1-NEXT                                    0326952
008645       ELSE                                                       0326952
008647           SUBTRACT IM-MEMO-OL-BEGIN-HOLD-AMT                     0326952
008655                          FROM IM-MEMO-OL-AVAIL-BAL.              9916157
008660     GO TO IMPDMEM1-NEXT.                                         9916157
008665*                                                                 9916157
008680 IMPDMEM1-BCR-OVERRIDES.                                          9916157
008700     IF  WBC-ONLINE-OD-FLAG EQUAL '1'                             0266741
008710         IF  WMS-OD-LIMIT-FILE EQUAL '1'                          9915855
008720             ADD WMS-OD-LIMIT-AMT-ONLINE                          9915855
008730                            TO IM-MEMO-OL-AVAIL-BAL               9915855
008740         ELSE                                                     9915855
008800         IF  WMS-OD-LIMIT EQUAL '0' OR 'L' OR 'S'
008900             ADD WMS-OD-LIMIT-AMT
009000                            TO IM-MEMO-OL-AVAIL-BAL
009100         ELSE
009200         IF  WMS-OD-LIMIT EQUAL  'A'
009300             MOVE +9999999999999.99                               9915845
009400                            TO IM-MEMO-OL-AVAIL-BAL
009500         ELSE
009600         IF  WMS-OD-LIMIT IS NUMERIC
009700             ADD WBC-OD-LIMIT (WMS-OD-LIMIT-9)                    0266741
009800                            TO IM-MEMO-OL-AVAIL-BAL.
009900*
010000     IF  WBC-ONLINE-LOC-FLAG EQUAL '1'                            0266741
010100     AND WMS-LOAN-TRLR EQUAL '1'
010120     AND WMS-LOAN-STATUS NOT EQUAL '5'                            0325985
010122       IF  WMS-OD-LIMIT EQUAL 'A'                                 0326952
010124       AND (WMS-OL-CALC1        EQUAL '1' OR '3' OR '5' OR '7' OR 0737706
010126                                      '9' OR 'B' OR 'D' OR 'F')   0326952
010128         NEXT SENTENCE                                            0326952
010130       ELSE                                                       0326952
010200         ADD WMS-AVAIL-BAL  TO IM-MEMO-OL-AVAIL-BAL.
010300*----------------------------------------------------------------*IM005
010400*      IF  SAV-FLAG EQUAL '1'  SUBTRACT SAVINGS BALANCE FROM     *IM006
010500*                                     IM-MEMO-OL-AVAIL-BAL       *IM005
010600*----------------------------------------------------------------*IM005
010800     IF  WMS-SAVINGS-TRLR EQUAL '1'                               IM007
010822       IF  WMS-OD-LIMIT EQUAL 'A'                                 0326952
010824       AND (WMS-OL-CALC1        EQUAL '1' OR '3' OR '5' OR '7' OR 0737706
010826                                      '9' OR 'B' OR 'D' OR 'F')   0326952
010828         NEXT SENTENCE                                            0326952
010830       ELSE                                                       0326952
011400         IF  WBC-ONLINE-SAV-FLAG EQUAL '1'                        0266741
011500             SUBTRACT WMS-INT-CUR-BAL FROM IM-MEMO-OL-AVAIL-BAL   IM007
011600         ELSE                                                     IM007
011700             SUBTRACT WMS-SAV-PEND-TAX FROM IM-MEMO-OL-AVAIL-BAL. IM007
011800*                                                                 IM007
011900     IF  WMS-OL-CALC2 EQUAL '2' OR '5' OR '6'                     9916157
012000         NEXT SENTENCE                                            IM004
012100     ELSE                                                         IM004
012200         GO TO IMPDMEM1-NEXT.                                     IM004
012300     MOVE WMS-HOLD-AMT      TO IM-MEMO-OL-BEGIN-HOLD-AMT.         IM004
012400     IF  WBC-ONLINE-HOLD-FLAG EQUAL '1'                           0266741
012500         SUBTRACT WMS-DROP-HOLD-AMOUNT                            IM004
012600                          FROM IM-MEMO-OL-BEGIN-HOLD-AMT.         IM004
012700     IF  IM-MEMO-OL-BEGIN-HOLD-AMT GREATER THAN ZERO              IM004
012722       IF  WMS-OD-LIMIT EQUAL 'A'                                 0326952
012724       AND (WMS-OL-CALC1        EQUAL '1' OR '3' OR '5' OR '7' OR 0737706
012726                                      '9' OR 'B' OR 'D' OR 'F')   0326952
012728         NEXT SENTENCE                                            0326952
012730       ELSE                                                       0326952
012800         SUBTRACT IM-MEMO-OL-BEGIN-HOLD-AMT                       IM004
012900                          FROM IM-MEMO-OL-AVAIL-BAL.              IM004
013000*
013100 IMPDMEM1-NEXT.                                                   IM004
013200     IF  (WMS-OL-CALC2 EQUAL '3' OR '5')                          0326952
013222       IF  WMS-OD-LIMIT EQUAL 'A'                                 0326952
013224       AND (WMS-OL-CALC1        EQUAL '1' OR '3' OR '5' OR '7' OR 0737706
013226                                      '9' OR 'B' OR 'D' OR 'F')   0326952
013228         NEXT SENTENCE                                            0326952
013230       ELSE                                                       0326952
013300         SUBTRACT WMS-BANK-AVAIL-AMT FROM IM-MEMO-OL-AVAIL-BAL.   IM007
013400     IF  (WMS-OL-CALC2 EQUAL '4' OR '6')                          0326952
013422       IF  WMS-OD-LIMIT EQUAL 'A'                                 0326952
013424       AND (WMS-OL-CALC1        EQUAL '1' OR '3' OR '5' OR '7' OR 0737706
013426                                      '9' OR 'B' OR 'D' OR 'F')   0326952
013428         NEXT SENTENCE                                            0326952
013430       ELSE                                                       0326952
013500         SUBTRACT WMS-CUST-AVAIL-AMT FROM IM-MEMO-OL-AVAIL-BAL.   IM007
013600*    IF  WMS-CUST-AVAIL-TRLR EQUAL '1'                            IMIB85
013622*      IF  WMS-OD-LIMIT EQUAL 'A'                                 IMIB85
013624*      AND (WMS-OL-CALC1        EQUAL '1' OR '3' OR '5' OR '7' OR IMIB85
013626*                                     '9' OR 'B' OR 'D' OR 'F')   IMIB85
013628*        NEXT SENTENCE                                            IMIB85
013630*      ELSE                                                       IMIB85
013700*        IF  WMS-OL-CALC2 EQUAL '4' OR '6'                        IMIB85
013800*            ADD WMS-CUST-AVAIL-FUNDS (1)                         IMIB85
013900*                           TO IM-MEMO-OL-AVAIL-BAL.              IMIB85
014000     IF  WMS-BANK-AVAIL-TRLR EQUAL '1'                            IM007
014022       IF  WMS-OD-LIMIT EQUAL 'A'                                 0326952
014024       AND (WMS-OL-CALC1        EQUAL '1' OR '3' OR '5' OR '7' OR 0737706
014026                                      '9' OR 'B' OR 'D' OR 'F')   0326952
014028         NEXT SENTENCE                                            0326952
014030       ELSE                                                       0326952
014100         IF  WMS-OL-CALC2 EQUAL '3' OR '5'                        9916157
014200             ADD WMS-BANK-AVAIL-TR-AMT (1)                        IM007
014300                            TO IM-MEMO-OL-AVAIL-BAL.              IM007
014400*
014410 IMPDMEM1-CONT.                                                   0416925
014500     MOVE WMS-LOAN-TRLR     TO IM-MEMO-OL-LOAN-TRLR.              0903097
014600     IF  WMS-LOAN-TRLR NOT EQUAL ZERO                             IM006
014700     AND WMS-LOAN-TRLR NOT EQUAL SPACE                            IM006
014710         MOVE WMS-LOAN-STATUS TO IM-MEMO-OL-LOAN-STATUS           1020116
014800         MOVE WMS-AVAIL-BAL TO IM-MEMO-OL-LN-AVAIL-BAL            0903097
014900         MOVE WMS-PRIN-BAL  TO IM-MEMO-OL-PRIN-BAL                0903097
015000         MOVE WMS-INTRST-BAL TO IM-MEMO-OL-INTRST-BAL             0903097
015100         MOVE WMS-OTHER-CHGS TO IM-MEMO-OL-OTHER-CHGS.            0903097
015200*                                                                 IM006
015300     MOVE WMS-OD-LIMIT      TO IM-MEMO-OL-OD-LIMIT.               0903097
015310     IF  WMS-OD-LIMIT-FILE EQUAL '1'                              9915845
015320         MOVE WMS-OD-LIMIT-AMT-ONLINE TO IM-MEMO-OL-OD-AMT        9915845
015330     ELSE                                                         9915845
015400     IF  WMS-OD-LIMIT EQUAL '0' OR 'L' OR 'S'                     IM006
015500         MOVE WMS-OD-LIMIT-AMT TO IM-MEMO-OL-OD-AMT               IM006
015600     ELSE                                                         IM006
015700     IF  WMS-OD-LIMIT EQUAL 'A'                                   IM006
015800         MOVE +9999999999999.99  TO IM-MEMO-OL-OD-AMT             9915845
015900     ELSE                                                         IM006
016000     IF  WMS-OD-LIMIT IS NUMERIC                                  IM006
016100         MOVE WBC-OD-LIMIT (WMS-OD-LIMIT-9) TO IM-MEMO-OL-OD-AMT. 0266741
016200*                                                                 0902008
016205     MOVE WMS-CUSTOMER-NUMBER    TO WS-CUSTNO-14.                 IMUP021
016207     MOVE WS-CUST-NO-12          TO IM-MEMO-OL-CUSTOMER-NUMBER.   IMUP021
016210*    MOVE WMS-CUSTOMER-NUMBER    TO IM-MEMO-OL-CUSTOMER-NUMBER.   IMIB79
016212     MOVE WMS-CONTROL-KEY        TO IM-MEMO-OL-CONTROL-KEY-ALT.   IMIBXXX
016214     IF WMS-ACCT-TYPE = '103' OR '104' OR '107' OR '108' OR       IMIBXXX
016216                        '117' OR          '121' OR '122' OR       IMIBXXX
016218                        '129' OR '130' OR '131' OR                IMIBXXX
016220                        '133' OR '134' OR '135' OR                105411
407115                        '136' OR '143'                            105967
016230        MOVE  '99'               TO IM-MEMO-OL-CTL-1-ALT          IMIBXXX
016232     END-IF.                                                      IMIBXXX
FO4104*    IF WMS-TARGET-AMT-TRLR = '1'                                 U21MAR
FO4104*       IF WMS-CONTROL-KEY NOT = WMS-TARGET-CONTROL               U21MAR
FO4104*          MOVE  '99'            TO IM-MEMO-OL-CTL-1-ALT          U21MAR
FO4104*       END-IF                                                    U21MAR
FO4104*    END-IF.                                                      U21MAR
016244                                                                  IMIBXXX
016246     MOVE WMS-USER-CODE-AREA     TO IM-MEMO-OL-USER-CODE-AREA.    IMIBXXX
016290                                                                  IMIBXXX
016300     MOVE WMS-HOLD-ALL-FUNDS-FLAG                                 0902008
016400                            TO IM-MEMO-OL-ALL-FUNDS-FLAG.         0902008
016500     MOVE WMS-SHORT-NAME    TO IM-MEMO-OL-SHORT-NAME.
016600*    MOVE WMS-HOLD-AMT      TO IM-MEMO-OL-BEGIN-HOLD-AMT.         0417121
016700     MOVE WMS-EXC-CODE      TO IM-MEMO-OL-EXC-CODE.
016800     MOVE WMS-STATUS        TO IM-MEMO-OL-STATUS.
016900     MOVE WMS-LOST-CONTACT  TO IM-MEMO-OL-LOST-CONTACT.
017000     MOVE WMS-OFF-EMP       TO IM-MEMO-OL-OFF-EMP.
017100     MOVE WMS-SYS-TYPE      TO IM-MEMO-OL-SYSTEM-TYPE.
017110     MOVE WMS-KITING-SUSP-TRLR                                    0417148
017120                            TO IM-MEMO-OL-KITING-TRLR.            0417148
017122     MOVE WMS-FORCE-POST    TO IM-MEMO-OL-FORCE-POST-FLAG.        0827769
017200     MOVE WMS-SPEC-INST-TODAY                                     IM007
017300                            TO IM-MEMO-OL-SPEC-INSTR-FLAG.        IM007
017400     MOVE WMS-ACCT-TYPE     TO IM-MEMO-OL-ACCT-TYPE.
017500     MOVE WMS-ACH-FLAG      TO IM-MEMO-OL-ACH-FLAG.
017600     MOVE WMS-BKUP-WTHLD-FLAG                                     IM007
017700                            TO IM-MEMO-OL-BKUP-WTHLD-FLAG.        IM007
017705     MOVE WMS-STATE-LOCAL-TX-CD                                   2012254
017710                            TO IM-MEMO-OL-STATE-LOCAL-TX-CD.      2012254
017800     MOVE WMS-TIN-CERTIFICATION                                   IM008
017900                            TO IM-MEMO-OL-TIN-CERT.               IM008
018000     MOVE WMS-TAX-EXEMPT-RSN                                      IM008
018100                            TO IM-MEMO-OL-EXEMPT-RSN.             IM008
018200     MOVE WMS-STOP-PAYS     TO IM-MEMO-OL-STOP-PAYS.
018205     MOVE WMS-OFFICER       TO IM-MEMO-OL-OFFICER.                IMIB79
018210     MOVE WMS-TODAYS-CREDITS TO IM-MEMO-TODAYS-CREDITS.           IMUP001
018215     MOVE WMS-TODAYS-CREDITS-NO                                   IMUP001
018220                            TO IM-MEMO-TODAYS-CREDITS-NO.         IMUP001
018225     MOVE WMS-TODAYS-DEBITS TO IM-MEMO-TODAYS-DEBITS.             IMUP001
018230     MOVE WMS-TODAYS-DEBITS-NO                                    IMUP001
018235                            TO IM-MEMO-TODAYS-DEBITS-NO.          IMUP001
018240     IF WMS-TARGET-AMT-TRLR = '1'                                 IMUP001
018245        MOVE WMS-TARGET-AMOUNT TO IM-MEMO-OL-TARGET-AMOUNT        IMUP001
018250     ELSE                                                         IMUP001
018255        MOVE +0.00          TO IM-MEMO-OL-TARGET-AMOUNT           IMUP001
018260     END-IF.                                                      IMUP001
018300     MOVE WMS-AMT-LAST-CUST-DEPOSIT
018400                            TO IM-MEMO-OL-LST-CUST-DEP.
018500     MOVE WMS-DATE-LAST-CUST-DEPOSIT
018600                            TO IM-MEMO-OL-DT-LST-CUST-DEP.
018700     MOVE WMS-DATE-LAST-MONETARY-ACT
018800                            TO IM-MEMO-OL-DT-LST-MON-TRAN.
018900     MOVE WMS-DATE-OPENED   TO IM-MEMO-OL-OPEN-DATE.
019000     MOVE WMS-MTD-AGGR-BAL  TO IM-MEMO-OL-MTD-AGGR-BAL.
019100     MOVE WMS-MTD-AGGR-DAYS TO IM-MEMO-OL-MTD-AGGR-DAYS.
019200     MOVE WMS-TAX-CODE      TO IM-MEMO-OL-TAX-CODE.               IM008
019300     MOVE WMS-TAX-NUMBER    TO IM-MEMO-OL-TAX-NUMBER.             IM008
019400     MOVE WMS-AMT-LAST-MONETARY-ACTIVITY
019500                            TO IM-MEMO-OL-AMT-LST-MON-TRAN.
019505     MOVE WMS-DATE-LAST-CUST-ACTIVITY                             IMIBXXX
019510                            TO IM-MEMO-OL-DT-LST-CUS-ACT.         IMIBXXX
019515     MOVE WMS-DATE-LAST-CUST-WITHDRAWAL                           IMUP001
019520                            TO IM-MEMO-OL-D-L-CUST-WITHDRAWAL.    IMUP001
019525     MOVE WMS-DAYS-OD      TO IM-MEMO-OL-DAYS-OD.                 IMIBXXX
019530     IF WMS-OD-ACCRUAL-TRLR = '1'                                 IMIBXX
019535        IF WBC-SYS-PREV-PROC-MO = WBC-SYS-CURR-PROC-MO            IMIBXX
019540           MOVE WMS-OD-CYC-ACR  TO IM-MEMO-OL-OD-CYC-ACR          IMIBXX
019545        ELSE                                                      IMIBXX
019550           MOVE WMS-OD-CYC-CHGD TO IM-MEMO-OL-OD-CYC-ACR          IMIBXX
019555        END-IF                                                    IMIBXX
019560     END-IF.                                                      IMIBXX
019600*                                                                 0903097
019700     MOVE WMS-GL-CODE       TO IM-MEMO-OL-GL-CODE.                0903097
019800     MOVE WMS-SECURED       TO IM-MEMO-OL-SECURED.                0903097
019900     MOVE WMS-IOD-CYC-PAID-INT  TO IM-MEMO-OL-IOD-CYC-INT-PD.     0903097
020000     MOVE WMS-IOD-MTD-PAID-INT  TO IM-MEMO-OL-IOD-MTD-INT-PD.     0903097
020100     MOVE WMS-IOD-YTD-INT-PAID  TO IM-MEMO-OL-IOD-YTD-INT-PD.     0903097
020200     MOVE WMS-TRANSFER-PRIORITY TO IM-MEMO-OL-TRANSFER-PRIORITY.  0903097
020300*                                                                 0903097
020400     MOVE WMS-NSF-CALC      TO IM-MEMO-OL-NSF-CALC.               IM003
020410     MOVE WMS-OL-CALC       TO IM-MEMO-OL-OL-CALC.                9916157
020500     MOVE WMS-CUST-COLL-CALC                                      0902834
020600                            TO IM-MEMO-OL-CUST-CALC-CODE.         0902834
020700     MOVE WMS-IOD-RATE-PTR  TO IM-MEMO-OL-IOD-RATE-PTR.           IM003
020800     MOVE WMS-HIFI-INDICATOR TO IM-MEMO-OL-HIFI-INDICATOR.        IM007
020900     MOVE WMS-MMDA-INDICATOR TO IM-MEMO-OL-MMDA-INDICATOR.        1003929
020910     MOVE WMS-OD-REGE-OPT-CODE TO IM-MEMO-OL-OD-REGE-OPT-CODE.    1020116
020920     MOVE WMS-SC-TYPE       TO IM-MEMO-OL-SC-TYPE.                IMIBXXX
021000     MOVE WMS-SC-MIN-BAL    TO IM-MEMO-OL-MIN-BAL.                IM003
021100     MOVE WMS-CHG-CARD-FLG  TO IM-MEMO-OL-CHG-CARD-FLAG.          1003929
021102     MOVE WMS-DDA-BAL       TO WS-ACCRUAL-BAL.                    9915845
021104*                                                                 9915845
021106     IF  WMS-IOD-CALC EQUAL '1'                                   9915845
021108         GO TO IMPDMEM1-NEXT-A.                                   9915845
021110*                                                                 9915845
021112     IF  WMS-IOD-CALC EQUAL '2'                                   9915845
021114         SUBTRACT WMS-HOLD-AMT FROM WS-ACCRUAL-BAL                9915845
021116         GO TO IMPDMEM1-NEXT-A.                                   9915845
021118*                                                                 9915845
021120     IF  WMS-IOD-CALC EQUAL '3'                                   9915845
021122         SUBTRACT WMS-BANK-AVAIL-AMT FROM WS-ACCRUAL-BAL          9915845
021124         GO TO IMPDMEM1-NEXT-A.                                   9915845
021126*                                                                 9915845
021128     IF  WMS-IOD-CALC EQUAL '4'                                   9915845
021130         SUBTRACT WMS-CUST-AVAIL-AMT FROM WS-ACCRUAL-BAL          9915845
021132         GO TO IMPDMEM1-NEXT-A.                                   9915845
021134*                                                                 9915845
021136     IF  WMS-IOD-CALC EQUAL '5'                                   9915845
021138         SUBTRACT WMS-HOLD-AMT FROM WS-ACCRUAL-BAL                9915845
021140         SUBTRACT WMS-BANK-AVAIL-AMT FROM WS-ACCRUAL-BAL          9915845
021142         GO TO IMPDMEM1-NEXT-A.                                   9915845
021144*                                                                 9915845
021146     IF  WMS-IOD-CALC EQUAL '6'                                   9915845
021148         SUBTRACT WMS-HOLD-AMT FROM WS-ACCRUAL-BAL                9915845
021150         SUBTRACT WMS-CUST-AVAIL-AMT FROM WS-ACCRUAL-BAL          9915845
021152         GO TO IMPDMEM1-NEXT-A.                                   9915845
021154*                                                                 9915845
021156 IMPDMEM1-NEXT-A.                                                 9915845
021158     IF  WMS-DAILY-RATE-CODE EQUAL '1'                            0817657
021160         MOVE +360 TO WS-FACTOR                                   0627539
021162     ELSE                                                         9915845
021164     IF  WMS-DAILY-RATE-CODE EQUAL '3'                            0817657
021166         MOVE +365 TO WS-FACTOR                                   0627539
021168     ELSE                                                         0627539
021170         IF  WBC-LEAP-YEAR EQUAL '1'                              0627539
021172             IF  WMS-LEAP-YEAR-FLG EQUAL '1'                      0627539
021174                 MOVE +366 TO WS-FACTOR                           0627539
021176             ELSE                                                 0627539
021178                 MOVE +365 TO WS-FACTOR                           0627539
021180         ELSE                                                     0627539
021182             MOVE +365     TO WS-FACTOR.                          0627539
021184*                                                                 0627539
021200     IF  WS-ACCRUAL-BAL GREATER THAN ZERO                         9915845
021300         COMPUTE IM-MEMO-OL-INTEREST-RATE =                       IM003
021400             (WMS-IOD-DAILY-ACC * WS-FACTOR) / WS-ACCRUAL-BAL.    9915845
021410*                                                                 9915845
021500     IF  WMS-EFA-TRLR EQUAL '1'                                   IM007
021600         MOVE WMS-EFA-NEW-ACCT-FLAG TO IM-MEMO-EFA-NEW-ACCT-FLAG  IM007
021700         MOVE WMS-EFA-COLLECTN-FLAG TO IM-MEMO-EFA-COLLECTN-FLAG  IM007
021800         MOVE WMS-EFA-STATUS-FLAG   TO IM-MEMO-EFA-STATUS-FLAG    IM007
021900         MOVE WMS-EFA-LARGE-DEPOSIT-AMT                           IM007
022000                            TO IM-MEMO-EFA-LARGE-DEP-AMT.         IM007
022100     MOVE IM-MEMO-OL-AVAIL-BAL TO IM-MEMO-OL-CASH-AVAIL-AMT.      IM007
022200     IF  WMS-CASH-AVAIL-TRLR EQUAL '0'                            IM007
022300     OR  IM-MEMO-OL-CASH-AVAIL-AMT EQUAL +9999999999999.99        9915845
022310     OR (IM-MEMO-OL-CASH-AVAIL-AMT EQUAL +0.00                    0416925
022315     AND WMS-STATUS EQUAL '04')                                   0416925
022400         GO TO IMPDMEM1-MEMO-UNAVAIL.                             1003856
022500     MOVE WMS-CASH2-AVAIL (1) TO IM-MEMO-OL-PM-CASH-AVAIL.        IM007
022600     ADD WMS-CASH1-AVAIL (1) TO IM-MEMO-OL-CASH-AVAIL-AMT.        IM007
022700     SUBTRACT WMS-CASH-AVAIL-AMT FROM IM-MEMO-OL-CASH-AVAIL-AMT.  IM007
022800     IF  (WMS-OL-CALC2 EQUAL '3' OR '5')                          9916157
022900     AND WMS-BANK-AVAIL-TRLR EQUAL '1'                            IM007
023000         ADD WMS-BANK-AVAIL-AMT TO IM-MEMO-OL-CASH-AVAIL-AMT      IM007
023100         SUBTRACT WMS-BANK-AVAIL-TR-AMT (1)                       IM007
023200             FROM IM-MEMO-OL-CASH-AVAIL-AMT.                      IM007
023300     IF  (WMS-OL-CALC2 EQUAL '4' OR '6')                          9916157
023400     AND WMS-CUST-AVAIL-TRLR EQUAL '1'                            IM007
023500*        ADD WMS-CUST-AVAIL-AMT TO IM-MEMO-OL-CASH-AVAIL-AMT      IM007
023510         ADD WMS-CUST-AVAIL-AMT TO IM-MEMO-OL-CASH-AVAIL-AMT.     IMIB85
023600*        SUBTRACT WMS-CUST-AVAIL-FUNDS (1)                        IMIB85
023700*                         FROM IM-MEMO-OL-CASH-AVAIL-AMT.         IMIB85
023800 IMPDMEM1-MEMO-UNAVAIL.                                           1003856
023900     IF (WMS-CUST-COLL-CALC EQUAL '3' OR '5')                     0827801
024000     AND (WMS-BANK-AVAIL-TRLR EQUAL '1')                          0827801
024100         MOVE WMS-BANK-AVAIL-AMT TO IM-MEMO-OL-UNAVAILABLE-AMT    0827801
024102         SUBTRACT WMS-BANK-AVAIL-TR-AMT (1)                       0827801
024104                               FROM IM-MEMO-OL-UNAVAILABLE-AMT.   0827801
024200     IF (WMS-CUST-COLL-CALC EQUAL '4' OR '6')                     0827801
024300     AND (WMS-CUST-AVAIL-TRLR EQUAL '1')                          0827801
024400*        MOVE WMS-CUST-AVAIL-AMT TO IM-MEMO-OL-UNAVAILABLE-AMT    0827801
024401         MOVE WMS-CUST-AVAIL-AMT TO IM-MEMO-OL-UNAVAILABLE-AMT.   IMIB85
024402*        SUBTRACT WMS-CUST-AVAIL-FUNDS (1)                        IMIB85
024404*                              FROM IM-MEMO-OL-UNAVAILABLE-AMT.   IMIB85
024500     MOVE WMS-STMT-SPECL-HANDL TO IM-MEMO-NB-STMT-SPEC-HANDL.     9715504
024600     MOVE WMS-NB-STMT-SPECL-HANDL-2                               9715504
024700                            TO IM-MEMO-NB-STMT-SPEC-HANDL-2.      9715504
024800     MOVE WMS-NB-MAIL-RTN-CNT TO IM-MEMO-NB-MAIL-RTN-CNT.         9715504
024900     MOVE WMS-ESCHEAT-FLAG  TO IM-MEMO-NB-ESCHEAT-FLAG.           9715504
025000     MOVE WMS-NB-TOT-DMT-SC TO IM-MEMO-NB-TOT-DMT-SC.             9715504
025100     MOVE WMS-REASON-CLOSED TO IM-MEMO-NB-REASON-CLOSED.          9715504
025200     MOVE WMS-HOME-PHONE    TO IM-MEMO-NB-HOME-PHONE.             9715504
025300     MOVE WMS-BUSINESS-PHONE TO IM-MEMO-NB-BUS-PHONE-NUMBER.      9715504
025400     MOVE WMS-SC-OD-TIMES   TO IM-MEMO-NB-SC-OD-TIMES.            9715504
025500     MOVE WMS-MTD-NSF-TIMES TO IM-MEMO-NB-MTD-NSF-TIMES.          9715504
025600     MOVE WMS-NB-UNLIM-TEL-HLD   TO IM-MEMO-NB-UNLIM-TEL-HLD.     9715504
025700     MOVE WMS-AUDIT         TO IM-MEMO-NB-AUDIT.                  9715504
025800     MOVE WMS-MTD-ANALYSIS  TO IM-MEMO-NB-MTD-ANALYSIS.           9715504
025900     MOVE WMS-NB-SWP-MIN    TO IM-MEMO-NB-MIN-SWP-AMT.            9715504
026000     MOVE WMS-TIS-CONSUMER-FLAG  TO IM-MEMO-NB-TIS-CONSUMER.      9715504
026100     MOVE WMS-LOCATOR-NUMBER TO IM-MEMO-NB-LOC-NBR.               9715504
026200     MOVE WMS-CLS-OVRRIDE   TO IM-MEMO-NB-CLS-OVRRIDE.            9715504
026300     MOVE WMS-NB-REGD-NTC-NBR TO IM-MEMO-NB-REGD-NTC-NBR.         9715504
026400     MOVE WMS-NB-REGD-TR-VIOL TO IM-MEMO-NB-REGD-TR-VIOL.         9715504
026500     MOVE WMS-NB-REGD-CK-VIOL TO IM-MEMO-NB-REGD-CK-VIOL.         9715504
026600     MOVE WMS-SIGNATURE-CARD TO IM-MEMO-NB-SIGNATURE-CARD.        9715504
026700     MOVE WMS-NON-TAXABLE   TO IM-MEMO-NB-NON-TAXABLE.            9715504
026800     MOVE WMS-TAX-EXEMPT-RSN TO IM-MEMO-NB-TAX-EXEMPT-RSN.        9715504
026900     MOVE WMS-LIST-POST     TO IM-MEMO-NB-LIST-POST.              9715504
027000     MOVE WMS-FUNDING-FLAG  TO IM-MEMO-NB-FUNDING-FLAG.           9715504
027100     MOVE WMS-NB-MIN-FND-BAL TO IM-MEMO-NB-MIN-FND-BAL.           9715504
027200     MOVE WMS-ARP-CODE      TO IM-MEMO-NB-ARP-CODE.               9715504
027300     MOVE WMS-DATE-LAST-CONTACT  TO IM-MEMO-NB-DATE-LAST-CONTACT. 9715504
027400     MOVE WMS-USER-CD (6)   TO IM-MEMO-NB-GEO-CODE.               9715504
027410     MOVE WMS-TRNSFR-AFFL-TRLR   TO IM-MEMO-OL-AFXF-TRLR-IN.      0917821
027500     IF  WMS-DC-TRLR NOT EQUAL ZERO                               9715504
027600         MOVE WMS-NB-DC-REGD-DT1 TO IM-MEMO-NB-REGD-DT1           9715504
027700         MOVE WMS-NB-DC-REGD-DT2 TO IM-MEMO-NB-REGD-DT2           9715504
027800         MOVE WMS-NB-DC-REGD-DT3 TO IM-MEMO-NB-REGD-DT3           9715504
027900         MOVE WMS-NB-DC-REGD-CVT-DT                               9715504
028000                            TO IM-MEMO-NB-REGD-CVT-DT             9715504
028100         MOVE WMS-NB-DC-CDA TO IM-MEMO-NB-DC-CDA.                 9715504
028200     IF  WMS-INFO-TRLR NOT EQUAL ZERO                             9715504
028300         MOVE WMS-NRA-CERT-NAME1 TO IM-MEMO-NB-NRA-CERT-NAME1     9715504
028400         MOVE WMS-NRA-CERT-NAME2 TO IM-MEMO-NB-NRA-CERT-NAME2     9715504
028500         MOVE WMS-NRA-TAX-COUNTRY                                 9715504
028600                            TO IM-MEMO-NB-NRA-TAX-CNTRY           9715504
028700         MOVE WMS-INFO-COUNTRY   TO IM-MEMO-NB-INFO-COUNTRY.      9715504
028800     IF  WMS-TARGET-AMT-TRLR NOT EQUAL ZERO                       9715504
028900         MOVE WMS-NB-ZB-LVL-NBR  TO IM-MEMO-NB-ZB-LVL-NBR         9715504
029000         MOVE WMS-NB-ZB-REL-NBR  TO IM-MEMO-NB-ZB-REL-NBR.        9715504
029100     IF  WMS-NME-ADDR-TLRS GREATER THAN ZERO                      9715504
029200         MOVE WMS-NA-LINE (1)    TO IM-MEMO-NB-NA-LINE (1)        9715504
029300         MOVE WMS-NA-LINE (2)    TO IM-MEMO-NB-NA-LINE (2).       9715504
029400     IF  WMS-OD-NSF-TRLR NOT EQUAL ZERO                           9715504
029500         MOVE WMS-DAYS-OD-MO (1) TO IM-MEMO-NB-DAYS-OD-MO-1       9715504
029600         MOVE WMS-TIMES-NSF-MO (1)                                9715504
029700                            TO IM-MEMO-NB-TIMES-NSF-MO-1          9715504
029800         MOVE WMS-NB-TIMES-CB-MO(1)                               9715504
029900                            TO IM-MEMO-NB-TIMES-CB-MO-1.          9715504
030000     MOVE WMS-CITIZEN-COUNTRY TO IM-MEMO-OL-CITIZEN-COUNTRY.      9915845
030100     MOVE WMS-MULTI-LANGUAGE-INFO TO IM-MEMO-OL-LANGUAGE-INFO.    9915845
030200     MOVE WMS-CURR-INFO TO IM-MEMO-OL-CURRENCY-INFO.              9915845
030300     MOVE WMS-PASSBOOK-INFO TO IM-MEMO-PASSBOOK-INFO.             9915845
030400     MOVE WMS-XINV-LINK-IND      TO IM-MEMO-OL-XINV-LINK-IND.     2016639
030500     MOVE WMS-XINV-PROCESS-IND   TO IM-MEMO-OL-XINV-PROC-IND.     2016639
030600     MOVE WMS-GL-COST-CENTER     TO IM-MEMO-GL-COST-CENTER.       0517228
030700*                                                                 0617360
030800     IF  WMS-PLAN-TRLR EQUAL '1'                                  0617360
030900         MOVE WMS-PLN-TRLR-STATUS-CD                              0617360
031000                                 TO IM-MEMO-OL-PLN-STATUS-CD      0617360
031100         MOVE WMS-PLN-TRLR-PART-EXC-FLG                           0617360
031200                                 TO IM-MEMO-OL-PLN-PART-EXC-FLG   0617360
031300         MOVE WMS-PLN-TRLR-VALID-CONTR                            0617360
031400                                 TO IM-MEMO-OL-PLN-VALID-CONTR    0617360
031500         MOVE WMS-PLN-TRLR-TYPE                                   0617360
031600                                 TO IM-MEMO-OL-PLN-TYPE           0617360
031700         MOVE WMS-PLN-TRLR-PLAN-KEY                               0617360
031800                                 TO IM-MEMO-OL-PLN-PLAN-KEY       0617360
031900         MOVE WMS-PLN-TRLR-ACT-CNT-LIM                            0617360
032000                                 TO IM-MEMO-OL-PLN-MAX-CNT-LIM    0617360
032100         MOVE WMS-PLN-TRLR-PY-ACT-CNT-LIM                         0617360
032200                                 TO IM-MEMO-OL-PLN-PY-MAX-CNT     0617360
032300         MOVE WMS-PLN-TRLR-DEATH-YEAR                             0617360
032400                                 TO IM-MEMO-OL-PLN-TRLR-DEATH-YR. 0617360
032500*                                                                 0617360
037200     IF  WMS-PLAN-TRLR NOT EQUAL '1'                              0617360
037300         MOVE SPACES             TO IM-MEMO-OL-PLN-STATUS-CD      0617360
037400                                    IM-MEMO-OL-PLN-PART-EXC-FLG   0617360
037500                                    IM-MEMO-OL-PLN-TYPE           0617360
037600                                    IM-MEMO-OL-PLN-PLAN-KEY       0617360
037700                                    IM-MEMO-OL-PLN-TRLR-DEATH-YR  0617360
037800         MOVE ZERO               TO IM-MEMO-OL-PLN-VALID-CONTR    0617360
037900                                    IM-MEMO-OL-PLN-MAX-CNT-LIM    0617360
038000                                    IM-MEMO-OL-PLN-PY-MAX-CNT     0617360
038100                                    IM-MEMO-OL-XRF-P02-SEQ.       0617360
038200*----------------------------------------------------------------*0930023
038300*    THE FOLLOWING CODE ADJUST THE REG DD BALANCE BASED ON       *0930023
038400*    HOLD ALL FUNDS, ACCOUNT IS CLOSED AND NSF CALC CODE         *0930023
038500*----------------------------------------------------------------*0930023
038600     IF  WMS-HOLD-ALL-FUNDS-FLAG EQUAL '1'                        0930023
038700     OR (WMS-HOLD-ALL-FUNDS-FLAG EQUAL '2' AND                    0930023
038800         WBC-ONLINE-HOLD-FLAG EQUAL '0')                          0930023
038900     OR  WMS-STATUS EQUAL '04'                                    0930023
039000         MOVE ZEROS              TO IM-MEMO-OL-REGDD-BAL          0930023
039100         GO TO IMPDMEM1-MEMO-EXIT.                                0930023
039200*                                                                 0930023
039300     IF  WMS-NSF-CALC EQUAL TO '1',                               0930023
039400         GO TO IMPDMEM1-MEMO-EXIT.                                0930023
039500     IF  WMS-NSF-CALC EQUAL TO '2'                                0930023
039600         SUBTRACT WMS-HOLD-AMT FROM IM-MEMO-OL-REGDD-BAL          0930023
039610         IF  WBC-ONLINE-HOLD-FLAG EQUAL '1'                       1020116
039620             ADD WMS-DROP-HOLD-AMOUNT  TO IM-MEMO-OL-REGDD-BAL    1020116
039630         END-IF                                                   1020116
039700         GO TO IMPDMEM1-MEMO-EXIT.                                0930023
039800     IF  WMS-NSF-CALC EQUAL TO '3'                                0930023
039900         SUBTRACT WMS-BANK-AVAIL-AMT FROM IM-MEMO-OL-REGDD-BAL    0930023
040010         IF  WMS-BANK-AVAIL-TRLR    EQUAL '1'                     1020116
040020             ADD WMS-BANK-AVAIL-TR-AMT (1) TO IM-MEMO-OL-REGDD-BAL1020116
040030         END-IF                                                   1020116
040100         GO TO IMPDMEM1-MEMO-EXIT.                                0930023
040200     IF  WMS-NSF-CALC EQUAL TO '4'                                0930023
040300         SUBTRACT WMS-CUST-AVAIL-AMT FROM IM-MEMO-OL-REGDD-BAL    0930023
040310         IF  WMS-CUST-AVAIL-TRLR    EQUAL '1'                     1020116
040320             ADD WMS-CUST-AVAIL-FUNDS (1) TO IM-MEMO-OL-REGDD-BAL 1020116
040330         END-IF                                                   1020116
040400         GO TO IMPDMEM1-MEMO-EXIT.                                0930023
040500     IF  WMS-NSF-CALC EQUAL TO '5'                                0930023
040600         SUBTRACT WMS-HOLD-AMT FROM IM-MEMO-OL-REGDD-BAL          0930023
040610         IF  WBC-ONLINE-HOLD-FLAG EQUAL '1'                       1020116
040620             ADD WMS-DROP-HOLD-AMOUNT  TO IM-MEMO-OL-REGDD-BAL    1020116
040630         END-IF                                                   1020116
040700         SUBTRACT WMS-BANK-AVAIL-AMT FROM IM-MEMO-OL-REGDD-BAL    0930023
040710         IF  WMS-BANK-AVAIL-TRLR    EQUAL '1'                     1020116
040720             ADD WMS-BANK-AVAIL-TR-AMT (1) TO IM-MEMO-OL-REGDD-BAL1020116
040730         END-IF                                                   1020116
040800         GO TO IMPDMEM1-MEMO-EXIT.                                0930023
040900     IF  WMS-NSF-CALC EQUAL TO '6'                                0930023
041000         SUBTRACT WMS-HOLD-AMT FROM IM-MEMO-OL-REGDD-BAL          0930023
041010         IF  WBC-ONLINE-HOLD-FLAG EQUAL '1'                       1020116
041020             ADD WMS-DROP-HOLD-AMOUNT  TO IM-MEMO-OL-REGDD-BAL    1020116
041030         END-IF                                                   1020116
041200         SUBTRACT WMS-CUST-AVAIL-AMT FROM IM-MEMO-OL-REGDD-BAL    0930023
041210         IF  WMS-CUST-AVAIL-TRLR    EQUAL '1'                     1020116
041220             ADD WMS-CUST-AVAIL-FUNDS (1) TO IM-MEMO-OL-REGDD-BAL 1020116
041230         END-IF                                                   1020116
041300         GO TO IMPDMEM1-MEMO-EXIT.                                0930023
041400     MOVE WMS-NB-RSRVE-RT TO WMS-NB-RSRVE-RT.                     0930023
041500     COMPUTE RESERVE-AMT EQUAL (IM-MEMO-OL-REGDD-BAL              0930023
041600                                * WMS-NB-RSRVE-RT).               0930023
041700     IF  WMS-NSF-CALC EQUAL TO '7'                                0930023
041800         SUBTRACT RESERVE-AMT FROM IM-MEMO-OL-REGDD-BAL           0930023
041900         GO TO IMPDMEM1-MEMO-EXIT.                                0930023
042000     IF  WMS-NSF-CALC EQUAL TO '8'                                0930023
042100         SUBTRACT WMS-HOLD-AMT FROM IM-MEMO-OL-REGDD-BAL          0930023
042110         IF  WBC-ONLINE-HOLD-FLAG EQUAL '1'                       1020116
042120             ADD WMS-DROP-HOLD-AMOUNT TO IM-MEMO-OL-REGDD-BAL     1020116
042130         END-IF                                                   1020116
042200         COMPUTE RESERVE-AMT EQUAL (IM-MEMO-OL-REGDD-BAL          0930023
042300                                    * WMS-NB-RSRVE-RT)            0930023
042400         SUBTRACT RESERVE-AMT FROM IM-MEMO-OL-REGDD-BAL           0930023
042500         GO TO IMPDMEM1-MEMO-EXIT.                                0930023
042600     IF  WMS-NSF-CALC EQUAL TO '9'                                0930023
042700         SUBTRACT WMS-BANK-AVAIL-AMT FROM IM-MEMO-OL-REGDD-BAL    0930023
042710         IF  WMS-BANK-AVAIL-TRLR    EQUAL '1'                     1020116
042720             ADD WMS-BANK-AVAIL-TR-AMT (1) TO IM-MEMO-OL-REGDD-BAL1020116
042730         END-IF                                                   1020116
042800         SUBTRACT WMS-HOLD-AMT FROM IM-MEMO-OL-REGDD-BAL          0930023
042810         IF  WBC-ONLINE-HOLD-FLAG EQUAL '1'                       1020116
042820             ADD WMS-DROP-HOLD-AMOUNT TO IM-MEMO-OL-REGDD-BAL     1020116
042830         END-IF                                                   1020116
042900         COMPUTE RESERVE-AMT EQUAL (IM-MEMO-OL-REGDD-BAL          0930023
043000                                    * WMS-NB-RSRVE-RT)            0930023
043100         SUBTRACT RESERVE-AMT FROM IM-MEMO-OL-REGDD-BAL           0930023
043200         GO TO IMPDMEM1-MEMO-EXIT.                                0930023
043300     IF  WMS-NSF-CALC EQUAL TO 'A'                                0930023
043400         SUBTRACT WMS-CUST-AVAIL-AMT FROM IM-MEMO-OL-REGDD-BAL    0930023
043410         IF  WMS-CUST-AVAIL-TRLR    EQUAL '1'                     1020116
043420             ADD WMS-CUST-AVAIL-FUNDS (1) TO IM-MEMO-OL-REGDD-BAL 1020116
043430         END-IF                                                   1020116
043500         SUBTRACT WMS-HOLD-AMT FROM IM-MEMO-OL-REGDD-BAL          0930023
043510         IF  WBC-ONLINE-HOLD-FLAG EQUAL '1'                       1020116
043520             ADD WMS-DROP-HOLD-AMOUNT TO IM-MEMO-OL-REGDD-BAL     1020116
043530         END-IF                                                   1020116
043600         COMPUTE RESERVE-AMT EQUAL (IM-MEMO-OL-REGDD-BAL          0930023
043700                                    * WMS-NB-RSRVE-RT)            0930023
043800         SUBTRACT RESERVE-AMT FROM IM-MEMO-OL-REGDD-BAL           0930023
043900         GO TO IMPDMEM1-MEMO-EXIT.                                0930023
044000     IF  WMS-NSF-CALC EQUAL TO 'B'                                0930023
044100         SUBTRACT WMS-BANK-AVAIL-AMT FROM IM-MEMO-OL-REGDD-BAL    0930023
044110         IF  WMS-BANK-AVAIL-TRLR    EQUAL '1'                     1020116
044120             ADD WMS-BANK-AVAIL-TR-AMT (1) TO IM-MEMO-OL-REGDD-BAL1020116
044130         END-IF                                                   1020116
044200         COMPUTE RESERVE-AMT EQUAL (IM-MEMO-OL-REGDD-BAL          0930023
044300                                    * WMS-NB-RSRVE-RT)            0930023
044400         SUBTRACT RESERVE-AMT FROM IM-MEMO-OL-REGDD-BAL           0930023
044500         GO TO IMPDMEM1-MEMO-EXIT.                                0930023
044600     IF WMS-NSF-CALC NOT EQUAL TO 'C'                             0930023
044700         GO TO IMPDMEM1-MEMO-EXIT.                                0930023
044800     SUBTRACT WMS-CUST-AVAIL-AMT FROM IM-MEMO-OL-REGDD-BAL.       0930023
044810     IF  WMS-CUST-AVAIL-TRLR    EQUAL '1'                         1020116
044820         ADD WMS-CUST-AVAIL-FUNDS (1) TO IM-MEMO-OL-REGDD-BAL.    1020116
044900     COMPUTE RESERVE-AMT EQUAL (IM-MEMO-OL-REGDD-BAL              0930023
045000                                * WMS-NB-RSRVE-RT).               0930023
045100     SUBTRACT RESERVE-AMT FROM IM-MEMO-OL-REGDD-BAL.              0930023
045200                                                                  0930023
045300 IMPDMEM1-MEMO-EXIT.                                              0930023
045400     EXIT.                                                        0930023
045500                                                                  0930023
