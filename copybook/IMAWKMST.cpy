*-----*----------------------------------------------------------------*
*     * 512468 28/04/25 JCTE IMPLEMENTAR SPLIT FORMA NATURAL SDO.COMP. *
*     * 409766 27/05/24 NYZM CUENTA GARANTIA TRANSACCIONAL             *
*     * 407681 21/02/24 NYZM MOSTRAR CANAL APERTURA                    *
*     * 000426 06/02/20 JCTE NUEVA GLOSA ABONO REMESAS. TRX. 0409.     *
*     * 903136 10/09/19 JCTE CAMBIO DE PACK PARA CUENTAS NEGOCIO       *
*     * 101613 30/04/13 JCTE IMPRIMIR EECC PN.                         *
*     * 100731 25/01/13 JCTE TRANSPARENCIA SBS:NO IMPRIMIR EST.CTAS PN *
*     * FO5238*12/05/11 JCTE PROYECTO UPGRADE SYSTEMAT                 *
000100*----------------------------------------------------------------*
000200*         IMPACS WORK MASTER COPYBOOK                            *
000300*----------------------------------------------------------------*
000301*                                                                 9916304
000302*----------------------------------------------------------------*9916304
000303*                **  HISTORY OF REVISION **                      *9916304
000304*                                                                *9916304
000305*  DESCRIPTION                                           CHNGID  *9916304
000306*  ----------------------------------------------------  --------*9916304
000307*                                                                *9916304
000308*  1998 - ADDED HISTORY.                                   6304  *9916304
000309*         ADDED REDEFINE OF WMS-NA-COUNTRY FOR TX.               *9916304
000310* 05/99 - ADDED FIELDS FOR BONUS INTEREST RATES.           6374  *2016374
000311* 11/99 - ADDED FIELD FOR ELECTRONIC ORIGIN REG.           5816  *2015816
000312* 05/2000 - ADDED STATE AND LOCAL TAX FIELDS.              2254  *2012254
000313* -------------------------------------------------------------- *9916304
000314* **** INTERBANK UPGRADE REVISION HISTORY ****                   *9916304
000315* 13/08/10 REAPPLY CUSTOM CODE NEW DC FIELDS             IMIB01A *IMIB01A
000316* 13/08/10 REAPPLY CUSTOM CODE NEW DC FIELD FINALIDAD    IMIB76  *IMIB76
000317* 13/08/10 REAPPLY CUSTOM CODE NEW DC FIELDS             IMIB58  *IMIB58
000318* 22/03/05 JCSC TIPOS DE BLOQUEO DE DEPOSITOS            FO1416  *FO1416
000319* 22/12/03 JCSC ITF                                      FO0836  *FO0836
000320* 13/08/10 REAPPLY CUSTOM CODE FOR USER CODE DESCRIPTIONSIMUP002 *IMUP002
000322*                                                                *9916304
000323*                                                                *9916304
000324*                                                                *9916304
000325*                                                                *9916304
000326*                                                                *9916304
000327*                                                                *9916304
000328*                                                                *9916304
000329*                                                                *9916304
000330*                                                                *9916304
000331*                                                                *9916304
000332*                                                                *9916304
000333*                                                                *9916304
000334*                                                                *9916304
000335*                                                                *9916304
000336*                                                                *9916304
000337*                                                                *9916304
000338*                                                                *9916304
000339*                                                                *9916304
000340*                                                                *9916304
000341*                                                                *9916304
000342*                                                                *9916304
000343*                                                                *9916304
000344*                                                                *9916304
000345*                                                                *9916304
000346*                                                                *9916304
000347*                                                                *9916304
000348*                                                                *9916304
000349*                                                                *9916304
000350*                                                                *9916304
000351*                                                                *9916304
000352*                                                                *9916304
000353*                                                                *9916304
000354*                                                                *2015816
000397*                                                                *9916304
000398*----------------------------------------------------------------*9916304
000399*                                                                 9916304
000400 01  MASTER-AREA.
000500   02  WMS-LENGTH                    PIC S9999       COMP.
000600   02  WMS-BIN0                      PIC XX.
000700   02  DDA-ACCT-MASTER.
000800     03  WMS-CONTROL-KEY.
000900         05  WMS-CONTROL-1           PIC XX.
001000         05  WMS-CONTROL-2           PIC XXX.
001100         05  WMS-CONTROL-3           PIC XXX.
001110         05  WMS-CTL4-ACCT.                                       0417078
001200             07  WMS-CONTROL-4       PIC XXXX.                    0417078
001300             07  WMS-ACCT-NO         PIC X(10).                   0417078
001310         05  FILLER REDEFINES WMS-CTL4-ACCT.                      0417078
001320             07  FILLER                  PIC X(02).               0417078
001330             07  WMS-ACCT-12             PIC X(12).               0417078
001400     03  WMS-EXC-CODE                PIC X.
001450     03  WMS-RED-ALERT-FLAG          PIC X.                       0837826
001500     03  FILLER                      PIC X(13).                   0837826
001600     03  WMS-REGION                  PIC XXX.
001700     03  WMS-BRANCH5.                                             0427044
001701         05  WMS-BRANCH              PIC XXX.                     0427044
001702         05  FILLER                  PIC XX.                      0427044
001710     03  WMS-FORCE-POST              PIC X.                       0827769
001800     03  WMS-LEDGER                  PIC XXX.
001900     03  WMS-OFFICER                 PIC X(5).                    9715504
001910     03  WMS-STATE-LOCAL-TX-CD       PIC X.                       2012254
001915     03  WMS-FDIC-LOSS-IND           PIC X.                       1020089
001920     03  FILLER                      PIC X.                       1020089
001922     03  WMS-CUSTOMER-KEY.                                        0827767
001924         05  WMS-CUSTOMER-CONTROLS.                               0827767
001926             07  WMS-CUSTOMER-CTL1   PIC X(04).                   0827767
001928             07  WMS-CUSTOMER-CTL2   PIC X(04).                   0827767
001930             07  WMS-CUSTOMER-CTL3   PIC X(04).                   0827767
001932             07  WMS-CUSTOMER-CTL4   PIC X(04).                   0827767
002000         05  WMS-CUSTOMER-NUMBER     PIC X(14).                   0827767
002100     03  WMS-CIF-DATA.
002200         05  WMS-CIF-CODE            PIC X.
002300         05  WMS-CIF-KEY             PIC X(35).
002400     03  WMS-GL-KEY.
002500         05  WMS-GL-CODE             PIC XX.
002600         05  WMS-GL-USER-DEF         PIC X(10).
002700         05  WMS-GL-COST-CENTER      PIC X(30).
002800         05  FILLER   REDEFINES  WMS-GL-COST-CENTER.
002900             07  WMS-GL-COST-CTR     PIC X(05)   OCCURS 6 TIMES.
002910     03  FILLER                      PIC X(17).                   9915845
003000     03  WMS-USER-FIELDS.
003100         05  WMS-USER-FLD1           PIC X(05).
003200         05  WMS-USER-FLD2           PIC X(05).
003300     03  WMS-USER-CODE-AREA.
003310* 1 - NO SUJETA A ENCAJE                                          IMUP002
003400         05  WMS-USER-CD-1           PIC X.
003410* 5 - BANCA PERSONAL / 7 - REFINANCIADA / 9 - NO EMITE ESTADOS    IMUP002
003500         05  WMS-USER-CD-2           PIC X.
003510*     TIPO DE VENCIDO                                             IMUP002
003600         05  WMS-USER-CD-3           PIC X.
003610*     CUENTAS DE CARLOS VERAU                                     IMUP002
003700         05  WMS-USER-CD-4           PIC X.
003710* L - CUENTAS CON LINEA DE CREDITO ESPECIAL                       IMUP002
003800         05  WMS-USER-CD-5           PIC X.
003810* 1 - CUENTAS TARGET : PARA LINEA DE CREDITO AUTOMATICA           IMUP002
003900         05  WMS-USER-CD-6           PIC X.
003910* 1 - CUENTAS CONVENIO : PARA NO INGRESAR A VENCIDOS              IMUP002
004000         05  WMS-USER-CD-7           PIC X.
004010* 1 - EN EL DEPOSITO : PIDA RUC AL DEPOSITANTE                    IMUP002
004100         05  WMS-USER-CD-8           PIC X.
004110* TIPO CLIENTE ITF : PARCIAL / TOTAL / EXCLUSIVA / ' '            FO0836
004200         05  WMS-USER-CD-9           PIC X.
004210* BLOQUEO DEPOSITOS: 1-NO DEP. , 2-NO DEP.EFEC , 3-NO DEP.CHEQUES FO1416
004300         05  WMS-USER-CD-10          PIC X.
903136* PACK DE CUENTAS NEGOCIO: 0-DIGITAL , 1=TRADICIONAL              FO1416
004400         05  WMS-USER-CD-11          PIC X.
903136* INDICADOR CTA.CTE. OPEN BANKING      B=BOGOTA                   FO1416
004500         05  WMS-USER-CD-12          PIC X.
407681* CANAL DE APERTURA: 1=WEB, 2=TIENDA, 3=SOPORTE
004600         05  WMS-USER-CD-13          PIC X.
409766* INDICADOR AFECTO A RETENCIONES JUDICIALES BLANCO=SI, N=NO
004700         05  WMS-USER-CD-14          PIC X.
409766* INDICADOR LIBRE DISPONIBILIDAD  BLANCO=SI, N=NO
004800         05  WMS-USER-CD-15          PIC X.
004810* INDICADOR DE SALDO COMPROMISO CON SPLIT '1'=SI, ESPACIOS=NO     512468
004900         05  WMS-USER-CD-16          PIC X.
005000         05  WMS-USER-CD-17          PIC X.
005100         05  WMS-USER-CD-18          PIC X.
005200     03  WMS-USER-CODES REDEFINES WMS-USER-CODE-AREA
005300                                                 OCCURS 6 TIMES.
005400         05  WMS-USER-CD             PIC XXX.
005500     03  WMS-ACCOUNT-FLAGS.
005600         05  WMS-STATUS              PIC XX.
005700         05  WMS-LARGE-ITEMS         PIC S9          COMP-3.
005800         05  WMS-CLS-OVRRIDE         PIC X.
005900         05  WMS-BAL-CHNG            PIC S9          COMP-3.
006000         05  WMS-LIST-POST           PIC X.
006100         05  WMS-NSF-CALC            PIC X.
006200         05  WMS-COLL-CALC           PIC X.
006300         05  WMS-CUST-COLL-CALC      PIC X.
006310         05  WMS-OD-CALC             PIC X.                       9915845
006400         05  WMS-CAUTION             PIC X.
006500         05  WMS-NSF-WAIVE           PIC X.
006600         05  WMS-OD-WAIVE            PIC X.
006700         05  WMS-LOST-CONTACT        PIC X.
006800         05  WMS-AUDIT               PIC X.
006900         05  WMS-INTANG-TAX          PIC X.
007000         05  WMS-ACCT-TYPE           PIC XXX.
007100         05  WMS-SYS-TYPE.
007200             07  WMS-SYS-TYP-1-2.
007300                 09  WMS-SYS-TYP-1   PIC X.
007400                 09  WMS-SYS-TYP-2   PIC X.
007500             07  WMS-SYS-TYP-3       PIC X.
007600         05  WMS-SECURED             PIC X.
007700         05  WMS-OFF-EMP             PIC X.
007800         05  WMS-BANK-AVAIL-FLAG     PIC X.
007900         05  WMS-CUST-AVAIL-FLAG     PIC X.
008000         05  WMS-NSF-OD-TRLR-FLAG    PIC X.
008010         05  WMS-DUAL-YEAR-FLAG      PIC X.                       9915858
008100         05  WMS-EXTRNL-DEP-FLAG     PIC X.
008200         05  WMS-ARP-AREA.
008300             07  WMS-ARP-CODE        PIC X.
008400             07  WMS-ARP-OTHER       PIC X.
008500             07  WMS-ARP-GEN         PIC X.
008600             07  WMS-ARP-STMT        PIC X.
008700             07  WMS-ARP-N-A         PIC X.
008800             07  WMS-ARP-ISSUE       PIC X.
008900         05  WMS-SPECL-HANDL         PIC X.
009000         05  WMS-ACH-FLAG            PIC X.
009100         05  WMS-CHG-CARD-FLG        PIC X.
009200         05  WMS-BANK-AVAIL-EXCP     PIC X.
009300         05  WMS-SPEC-INST-TODAY     PIC X.
009400         05  WMS-BALANCE-HISTORY     PIC X.
009500         05  WMS-COMB-STMT-ONLY      PIC X.
009600         05  WMS-SEP-COMB-FILE       PIC X.
009700         05  WMS-BULK-FILE           PIC X.
009800         05  WMS-STOP-WAIVE          PIC X.
009900         05  WMS-DAU-WAIVE           PIC X.
010000         05  WMS-NON-TAXABLE         PIC X.
010100         05  WMS-CHK-TRUNC-FLAG      PIC X.
010200         05  WMS-NSF-SUPPRESS-NTC    PIC X.
010300         05  WMS-TIS-CONSUMER-FLAG   PIC X.
010302         05  WMS-MIN-BAL-CALC        PIC X.                       9916072
010304         05  WMS-OL-CALC.                                         9916157
010306             07  WMS-OL-CALC1        PIC X.                       9916157
010308             07  WMS-OL-CALC2        PIC X.                       9916157
010310         05  WMS-NEW-ACCT-RPT-FLAG   PIC X.                       0266887
010320     03  WMS-BRT-TERM-OPT            PIC X.                       0417066
010322     03  WMS-BRT-TERM-LMT            PIC S999        COMP-3.      0417066
010324     03  WMS-BRT-TERM-BEG-DT.                                     0417066
010326         05  WMS-BRT-TERM-BEG-CENT   PIC XX.                      0417066
010328         05  WMS-BRT-TERM-BEG-YR     PIC XX.                      0417066
010330         05  WMS-BRT-TERM-BEG-MO     PIC XX.                      0417066
010332         05  WMS-BRT-TERM-BEG-DA     PIC XX.                      0417066
010334     03  WMS-BRT-TERM-CNV            PIC X.                       0417066
010340     03  WMS-DAILY-RATE-CODE         PIC X.                       0817657
010342     03  WMS-LOAN-360-DAY-FACTOR     PIC X.                       0817657
010344     03  WMS-CHARGE-OFF-CODE         PIC X.                       0817662
010350     03  WMS-EXTEND-FLOAT            PIC X.                       0927817
010400     03  WMS-RATE-REGION             PIC X(10).
010500     03  WMS-SVC-CHRG-REGION         PIC X(10).
010501     03  WMS-ML-SOURCE-OF-FUNDS      PIC X(3).                    0727682
010502     03  WMS-ML-PURPOSE-ACTIVITY-OTHER PIC X(2).                  0727682
010503     03  WMS-ML-LEVEL-OF-ACTIVITY    PIC X.                       0727682
010504     03  WMS-ML-TYPE-OF-ACTIVITY1    PIC X.                       0727682
010505     03  WMS-ML-TYPE-OF-ACTIVITY2    PIC X.                       0727682
010506     03  WMS-ML-PURPOSE-OF-ACTIVITY PIC X(2).                     0727682
010600     03  WMS-TISA-AGGR-BAL-CD        PIC X.
010700     03  WMS-NB-THRESHOLD            PIC S9(13)V99   COMP-3.      9915845
010800     03  WMS-EXTERNAL-CMA-IND        PIC X(3).
010900     03  WMS-DAU-MIN-AMT             PIC S9(13)V99   COMP-3.      9915845
011000     03  WMS-DAU-MAX-CHG             PIC S9(13)V99   COMP-3.      9915845
011100     03  WMS-SHORT-NAME              PIC X(13).
011102     03  WMS-MIN-CR-INT              PIC S9(9)V99    COMP-3.      0827727
011104     03  WMS-ARCH-HIST-RET-DAYS      PIC S9(3)       COMP-3.      0927050
011110     03  FILLER                      PIC X(9).                    0927050
011200     03  WMS-CURR-BAL                PIC S9(13)V99   COMP-3.
011300     03  WMS-DDA-BAL                 PIC S9(13)V99   COMP-3.
011400     03  WMS-HOLD-AMT                PIC S9(13)V99   COMP-3.
011500     03  WMS-BANK-AVAIL-ADJ          PIC S9(13)V99   COMP-3.      9915845
011700     03  WMS-CUST-AVAIL-ADJ          PIC S9(13)V99   COMP-3.      9915845
011900     03  WMS-BANK-AVAIL-AMT          PIC S9(13)V99   COMP-3.      9915845
012100     03  WMS-CUST-AVAIL-AMT          PIC S9(13)V99   COMP-3.      9915845
012200     03  WMS-LEAP-YEAR-FLG           PIC X.                       CIB5519
012300     03  WMS-STOP-PAYS               PIC S9(7)       COMP-3.
012400     03  WMS-BAL-HIST-RET            PIC S9(3)       COMP-3.
012500     03  WMS-PURGE                   PIC X.
012600     03  WMS-PURGE-DAYS              PIC S999        COMP-3.
012700     03  WMS-DORMANT                 PIC X.
012800     03  WMS-DORMANT-DAYS            PIC S999        COMP-3.
012900     03  WMS-HIFI-DORM               PIC X.
013000     03  WMS-HIFI-DORM-DAYS          PIC S999        COMP-3.
013100     03  WMS-INACTIVE                PIC X.
013200     03  WMS-INACTIVE-DAYS           PIC S999        COMP-3.
013300     03  WMS-NSF-CODE                PIC X.
013400     03  WMS-NSF-CHG                 PIC S9(13)V99   COMP-3.      9915845
013500     03  WMS-NSF-MAX-CHG             PIC S9(13)V99   COMP-3.      9915845
013600     03  WMS-DAYS-NSF-TO-DATE        PIC S9(5)       COMP-3.
013705     03  WMS-IT-TRAN-FLAGS.                                       9715504
013710         05  WMS-IT-TRAN-FLAG1       PIC X.                       9715504
013715         05  WMS-IT-TRAN-FLAG2       PIC X.                       9715504
013720         05  WMS-IT-TRAN-FLAG3       PIC X.                       9715504
013725         05  WMS-IT-TRAN-FLAG4       PIC X.                       9715504
013730         05  WMS-IT-TRAN-FLAG5       PIC X.                       9715504
013735         05  WMS-IT-TRAN-FLAG6       PIC X.                       9715504
013740         05  WMS-IT-TRAN-FLAG7       PIC X.                       9715504
013745         05  WMS-IT-TRAN-FLAG8       PIC X.                       9715504
013750         05  WMS-IT-TRAN-FLAG9       PIC X.                       9715504
013755         05  WMS-IT-TRAN-FLAG10      PIC X.                       9715504
013800     03  WMS-OD-OVERDRAFT            PIC X.
013900     03  WMS-OD-CHG                  PIC S9(13)V99   COMP-3.      9915845
014000     03  WMS-OD-SEC-NOTICE           PIC X.
014100     03  WMS-OD-CHG-CODE             PIC X.
014200     03  WMS-OD-MIN-AMT              PIC S9(13)V99   COMP-3.      9915845
014300     03  WMS-OD-MAX-CHG              PIC S9(13)V99   COMP-3.      9915845
014400     03  WMS-OD-LIMIT                PIC X.
014500     03  WMS-OD-LIMIT-9 REDEFINES WMS-OD-LIMIT
014600                                     PIC 9.
014700     03  WMS-OD-LIMIT-AMT            PIC S9(13)V99   COMP-3.      9915845
014800     03  WMS-DAYS-OD                 PIC S9(3)       COMP-3.
014900     03  WMS-DATE-INTO-OD.
015000         05  WMS-INTO-OD-MO          PIC XX.
015100         05  WMS-INTO-OD-DA          PIC XX.
015200         05  WMS-INTO-OD-YR.
015300             07  WMS-INTO-OD-YR-1    PIC X.
015400             07  WMS-INTO-OD-YR-2    PIC X.
015410     03  WMS-OD-CONSEC-DAYS          PIC S9(3)       COMP-3.      0627540
015500     03  WMS-DAYS-OD-TO-DATE         PIC S9(5)       COMP-3.
015600     03  WMS-DDA-CHG-OFF-AMT         PIC S9(13)V99   COMP-3.      9915845
015700     03  WMS-DAYS-ZERO-BAL           PIC S9(3)       COMP-3.
015800     03  WMS-CUST-AVAIL-CODE         PIC X.
015900     03  WMS-CUST-AVAIL-CHG          PIC S9(13)V99   COMP-3.      9915845
016000     03  WMS-CHK-LIMIT               PIC S999        COMP-3.
016100     03  WMS-CHK-CHG                 PIC S9(12)V999  COMP-3.      9915845
016200     03  WMS-NSF-TODAY               PIC X.
016300     03  WMS-OD-TODAY                PIC X.
016400     03  WMS-UNAVAIL-TODAY           PIC X.
016500     03  WMS-OD-FOLLOWUP             PIC S999        COMP-3.
016600     03  WMS-HOLD-NO                 PIC S9(7)       COMP-3.
016700     03  WMS-OD-SUPPRESS-NTC         PIC X.
016800     03  WMS-TODAYS-CREDITS          PIC S9(13)V99   COMP-3.      9915845
016900     03  WMS-TODAYS-CREDITS-NO       PIC S9(5)       COMP-3.
017000     03  WMS-PREV-DAY-CR             PIC S9(13)V99   COMP-3.      9915845
017100     03  WMS-PREV-DAY-CR-NO          PIC S9(5)       COMP-3.
017200     03  WMS-TODAYS-DEBITS           PIC S9(13)V99   COMP-3.      9915845
017300     03  WMS-TODAYS-DEBITS-NO        PIC S9(5)       COMP-3.
017400     03  WMS-DROP-HOLD-AMOUNT        PIC S9(13)V99   COMP-3.      9915845
017500     03  WMS-KITING-INFO.
017600         05  WMS-KITING-FLAG         PIC X.
017700         05  WMS-KITE-CONSEC-DAYS    PIC S9(3)       COMP-3.
017800     03  WMS-FUNDING-INFO.
017900         05  WMS-FUNDING-FLAG        PIC X.
018000         05  WMS-DAYS-BELOW-MIN-BAL  PIC S9(3)       COMP-3.
018100     03  WMS-MMDA-INDICATOR          PIC X.
018200     03  WMS-ACCT-OPENING-INFO.
018300         05  WMS-BRANCH-OPENED       PIC X(5).                    0427044
018310         05  FILLER                  PIC X.                       0427044
018400         05  WMS-OFFICER-OPENED      PIC X(5).                    9715504
018410         05  WMS-OPENING-METHOD      PIC X.                       2015816
018500         05  WMS-OPENING-DEPOSIT     PIC S9(13)V99   COMP-3.      9915845
018600         05  WMS-REOPENED-ACCT       PIC X.
018700     03  WMS-AMT-LAST-CUST-DEPOSIT   PIC S9(13)V99   COMP-3.      9915845
018800     03  WMS-AMT-LAST-CUST-WITHDRAWAL PIC S9(13)V99  COMP-3.      9915845
018900     03  WMS-AMT-LAST-DEPOSIT        PIC S9(13)V99   COMP-3.      9915845
019000     03  WMS-AMT-LAST-MONETARY-ACTIVITY  PIC S9(13)V99    COMP-3. 9915845
019100     03  WMS-SOURCE-OF-FUNDS         PIC X(3).
019200     03  WMS-ESCHEAT-FLAG            PIC X.
019300     03  WMS-HOLD-ALL-FUNDS-FLAG     PIC X.
019400     03  WMS-REASON-CLOSED           PIC XX.
019500     03  WMS-0BAL-GRACE-DAYS         PIC S999        COMP-3.
019502     03  WMS-OD-MIN-CHG              PIC S9(9)V99    COMP-3.      0827728
019600     03  FILLER                      PIC X(6).                    0827728
019700     03  WMS-ACCOUNT-DATES.
019800         05  WMS-DATE-LAST-CUST-DEPOSIT.
019900             07  WMS-MO-LAST-CUST-DEP  PIC XX.
020000             07  WMS-DA-LAST-CUST-DEP  PIC XX.
020100             07  WMS-YR-LAST-CUST-DEP.
020200                 09  WMS-YR1-LCD       PIC X.
020300                 09  WMS-YR2-LCD       PIC X.
020400         05  WMS-DATE-LAST-CUST-WITHDRAWAL.
020500             07  WMS-MO-LAST-CUST-WDRL PIC XX.
020600             07  WMS-DA-LAST-CUST-WDRL PIC XX.
020700             07  WMS-YR-LAST-CUST-WDRL PIC XX.
020800         05  WMS-DATE-LAST-CUST-ACTIVITY.
020900             07  WMS-LAST-CUST-ACT-MO  PIC XX.
021000             07  WMS-LAST-CUST-ACT-DA  PIC XX.
021100             07  WMS-LAST-CUST-ACT-YR  PIC XX.
021200         05  WMS-DATE-LAST-MONETARY-ACT.
021300             07  WMS-MO-LAST-MNTY-ACT  PIC XX.
021400             07  WMS-DA-LAST-MNTY-ACT  PIC XX.
021500             07  WMS-YR-LAST-MNTY-ACT  PIC XX.
021600         05  WMS-DATE-LAST-MAINT.
021700             07  WMS-LAST-MAINT-MO   PIC XX.
021800             07  WMS-LAST-MAINT-DA   PIC XX.
021900             07  WMS-LAST-MAINT-YR   PIC XX.
022000         05  WMS-DATE-OPENED.
022100             07  WMS-OPENED-MO       PIC XX.
022200             07  WMS-OPENED-DA       PIC XX.
022300             07  WMS-OPENED-YR       PIC XX.
022400         05  WMS-DATE-INACT-DORM.
022500             07  WMS-INACT-DORM-MO   PIC XX.
022600             07  WMS-INACT-DORM-DA   PIC XX.
022700             07  WMS-INACT-DORM-YR   PIC XX.
022705         05  WMS-DATE-INACT-DORM-ABNDN REDEFINES                  9715504
022710             WMS-DATE-INACT-DORM     PIC X(06).                   9715504
022800         05  WMS-DATE-LAST-CONTACT.
022900             07  WMS-CONTACT-MO      PIC XX.
023000             07  WMS-CONTACT-DA      PIC XX.
023100             07  WMS-CONTACT-YR      PIC XX.
023200         05  WMS-DATE-LAST-WITHDRAWAL.
023300             07  WMS-WITHDRW-MO      PIC XX.
023400             07  WMS-WITHDRW-DA      PIC XX.
023500             07  WMS-WITHDRW-YR      PIC XX.
023600         05  WMS-DATE-LAST-DEPOSIT.
023700             07  WMS-MO-LAST-DEP     PIC XX.
023800             07  WMS-DA-LAST-DEP     PIC XX.
023900             07  WMS-YR-LAST-DEP.
024000                 09  WMS-YR1-LD      PIC X.
024100                 09  WMS-YR2-LD      PIC X.
024200         05  WMS-DATE-MIN.
024300             07  WMS-MIN-MO          PIC XX.
024400             07  WMS-MIN-DA          PIC XX.
024500             07  WMS-MIN-YR          PIC XX.
024600         05  WMS-DATE-CLOSED.
024700             07  WMS-CLOSED-CENT     PIC XX.
024800             07  WMS-CLOSED-YR       PIC XX.
024900             07  WMS-CLOSED-MO       PIC XX.
025000             07  WMS-CLOSED-DA       PIC XX.
025002     03  WMS-CLOSE-DAYS              PIC S9(3)       COMP-3.      0527322
025004     03  WMS-CLOSE-AMT               PIC S9(13)V99   COMP-3.      0527322
025006     03  WMS-CLOSE-WVE               PIC X.                       0527322
025008     03  WMS-CLOSE-EXP-DTE.                                       0527322
025010         05  WMS-CLOSE-EXP-DTE-CENT  PIC XX.                      0527322
025012         05  WMS-CLOSE-EXP-DTE-YR    PIC XX.                      0527322
025014         05  WMS-CLOSE-EXP-DTE-MO    PIC XX.                      0527322
025016         05  WMS-CLOSE-EXP-DTE-DA    PIC XX.                      0527322
025017     03  WMS-DATE-LAST-NA-MAINT.                                  0837840
025018         05  WMS-DATE-LAST-NA-MAINT-CENT PIC XX.                  0837840
025019         05  WMS-DATE-LAST-NA-MAINT-YR   PIC XX.                  0837840
025020         05  WMS-DATE-LAST-NA-MAINT-MO   PIC XX.                  0837840
025021         05  WMS-DATE-LAST-NA-MAINT-DA   PIC XX.                  0837840
025022     03  WMS-TRUST-CODE              PIC X(1).                    1010086
025023     03  WMS-REGD-TR-VIOL2           PIC S999        COMP-3.      0837840
025024     03  WMS-REGD-TR-VIOL3           PIC S999        COMP-3.      0437111
025026     03  WMS-REGD-TR-VIOL4           PIC S999        COMP-3.      0437111
025028     03  WMS-REGD-TR-VIOL5           PIC S999        COMP-3.      0437111
025030     03  WMS-REGD-TR-VIOL6           PIC S999        COMP-3.      0437111
025032     03  WMS-REGD-TR-VIOL7           PIC S999        COMP-3.      0437111
025034     03  WMS-REGD-TR-VIOL8           PIC S999        COMP-3.      0437111
025036     03  WMS-REGD-TR-VIOL9           PIC S999        COMP-3.      0437111
025038     03  WMS-REGD-TR-VIOL10          PIC S999        COMP-3.      0437111
025040     03  WMS-REGD-TR-VIOL11          PIC S999        COMP-3.      0437111
025042     03  WMS-REGD-TR-VIOL12          PIC S999        COMP-3.      0437111
025044     03  WMS-REGD-CK-VIOL2           PIC S999        COMP-3.      0437111
025046     03  WMS-REGD-CK-VIOL3           PIC S999        COMP-3.      0437111
025048     03  WMS-REGD-CK-VIOL4           PIC S999        COMP-3.      0437111
025050     03  WMS-REGD-CK-VIOL5           PIC S999        COMP-3.      0437111
025052     03  WMS-REGD-CK-VIOL6           PIC S999        COMP-3.      0437111
025054     03  WMS-REGD-CK-VIOL7           PIC S999        COMP-3.      0437111
025056     03  WMS-REGD-CK-VIOL8           PIC S999        COMP-3.      0437111
025058     03  WMS-REGD-CK-VIOL9           PIC S999        COMP-3.      0437111
025060     03  WMS-REGD-CK-VIOL10          PIC S999        COMP-3.      0437111
025062     03  WMS-REGD-CK-VIOL11          PIC S999        COMP-3.      0437111
025064     03  WMS-REGD-CK-VIOL12          PIC S999        COMP-3.      0437111
025110     03  WMS-OD-LIMIT-AMT-ONLINE     PIC S9(13)V99   COMP-3.      9915855
025200     03  WMS-TAX-NUMBER-DATA.
025300         05  WMS-HX-ACCT-KEY         PIC S9(17)  COMP-3.
025400         05  WMS-TAX-CODE            PIC X.
025500         05  WMS-TAX-NUMBER.
025600             07  WMS-TAX-NO-PREFIX   PIC X.
025700             07  WMS-TAX-NO          PIC X(9).
025800             07  WMS-TAX-NO-SUFFIX   PIC X.
025810         05  FILLER                  PIC X(4).                    9915845
025900         05  WMS-BKUP-WTHLD-FLAG     PIC X.
026000         05  WMS-TAX-EXEMPT-RSN      PIC X.
026100         05  WMS-TIN-CERTIFICATION   PIC X.
026110         05  WMS-CITIZEN-COUNTRY     PIC XX.                      9915845
026112         05  WMS-TAX-FED-PTR         PIC S999   COMP-3.           2012254
026114         05  WMS-TAX-STATE-PTR       PIC S999   COMP-3.           2012254
026116         05  WMS-TAX-LOCAL-PTR       PIC S999   COMP-3.           2012254
026120         05  FILLER                  PIC X(10).                   2012254
026130     03  FILLER                      PIC X(20).                   9915845
026200     03  WMS-BIRTHDATE.
026300         05  WMS-BIRTH-MO            PIC XX.
026400         05  WMS-BIRTH-DA            PIC XX.
026500         05  WMS-BIRTH-YEAR.
026600             07  WMS-BIRTH-CENT      PIC XX.
026700             07  WMS-BIRTH-YR        PIC XX.
026800     03  WMS-HOME-PHONE.
026900         05  WMS-HOME-AREA-CODE      PIC XXX.
027000         05  WMS-HOME-PHONE-NO       PIC X(7).
027010         05  WMS-HOME-EXTENSION      PIC X(4).                    9915845
027020     03  FILLER                      PIC X(8).                    9915845
027100     03  WMS-BUSINESS-PHONE.
027200         05  WMS-BUS-AREA-CODE       PIC XXX.
027300         05  WMS-BUS-PHONE-NO        PIC X(7).
027400         05  WMS-BUS-EXTENSION       PIC XXXX.
027410     03  FILLER                      PIC X(8).                    9915845
027500     03  WMS-ZIP-SV-AREA.
027600         05  WMS-ZIP-SAVE.
027700             07  WMS-ZIP-CODE        PIC X(5).
027800             07  WMS-ZIP-HYPHEN      PIC X.
027900             07  WMS-ZIP-SUFFIX      PIC X(4).
028000         05  WMS-SV-WALK-SEQ-CD      PIC X(2).
028010     03  FILLER                      PIC X(4).                    9915845
028100     03  INTEREST-ON-DEMAND-DATA.
028200         05  WMS-HIFI-INDICATOR      PIC S999        COMP-3.
028300         05  WMS-IOD-RATE-PTR        PIC S999        COMP-3.
028310         05  WMS-IOD-MIN-RATE        PIC S9V9(8)     COMP-3.      9915845
028320         05  WMS-IOD-MAX-RATE        PIC S9V9(8)     COMP-3.      9915845
028400         05  WMS-IOD-CALC            PIC X.
028500         05  WMS-IOD-INT-CALC        PIC S9          COMP-3.
028600         05  WMS-IOD-DAILY-ACC       PIC S9(11)V9(6) COMP-3.      9915845
028700         05  WMS-IOD-ACCRD-TODAY     PIC S9(11)V9(6) COMP-3.      9915845
028800         05  WMS-IOD-MTD-ACC-INT     PIC S9(13)V99   COMP-3.      9915845
028900         05  WMS-IOD-MTD-PAID-INT    PIC S9(13)V99   COMP-3.      9915845
029000         05  WMS-IOD-MTD-TAX         PIC S9(13)V99   COMP-3.      9915845
029010         05  WMS-IOD-MTD-STATE-TAX   PIC S9(13)V99   COMP-3.      9915845
029020         05  WMS-IOD-MTD-LOCAL-TAX   PIC S9(13)V99   COMP-3.      9915845
029100         05  WMS-IOD-CYC-ACC-INT     PIC S9(11)V9(6) COMP-3.      9915845
029200         05  WMS-IOD-CYC-PAID-INT    PIC S9(13)V99   COMP-3.      9915845
029300         05  WMS-IOD-CYC-TAX         PIC S9(13)V99   COMP-3.      9915845
029310         05  WMS-IOD-CYC-STATE-TAX   PIC S9(13)V99   COMP-3.      9915845
029320         05  WMS-IOD-CYC-LOCAL-TAX   PIC S9(13)V99   COMP-3.      9915845
029400         05  WMS-IOD-YTD-INT-PAID    PIC S9(13)V99   COMP-3.      9915845
029500         05  WMS-IOD-PRV-YTD-INT-PD  PIC S9(13)V99   COMP-3.      9915845
029600         05  WMS-IOD-YTD-TAX         PIC S9(13)V99   COMP-3.      9915845
029610         05  WMS-IOD-YTD-STATE-TAX   PIC S9(13)V99   COMP-3.      9915845
029620         05  WMS-IOD-YTD-LOCAL-TAX   PIC S9(13)V99   COMP-3.      9915845
029700         05  WMS-IOD-PREV-YTD-TAX    PIC S9(13)V99   COMP-3.      9915845
029710         05  WMS-IOD-PREV-YTD-STATE-TAX PIC  S9(13)V99   COMP-3.  9915845
029720         05  WMS-IOD-PREV-YTD-LOCAL-TAX PIC  S9(13)V99   COMP-3.  9915845
029800         05  WMS-IOD-ACCRUAL-BAL     PIC S9(13)V99   COMP-3.      9915845
030000         05  WMS-IOD-DIST-CD         PIC X.
030100         05  WMS-IOD-PROJ-ACCR       PIC S9(13)V99   COMP-3.      9915845
030200         05  WMS-MIN-AMT-FOR-HIFI    PIC S9(13)V99   COMP-3.      9915845
030300         05  WMS-MAX-TRAN-FOR-HIFI   PIC S999        COMP-3.
030500         05  WMS-CHECK-NA-PTR        PIC X.
030600         05  WMS-IOD-DIST-DR-TC      PIC X(4).
030700         05  WMS-IOD-DIST-CR-TC      PIC X(4).
030800         05  WMS-IOD-XFER-INT-PTR    PIC S9          COMP-3.
030900         05  WMS-IOD-ACCRUAL-TYPE    PIC X.
031000         05  WMS-IOD-RATE-USE        PIC X.
031010         05  WMS-NB-IOD-PRV-CTD-PD   PIC S9(13)V99   COMP-3.      9915845
031020         05  WMS-IOD-YTD-INT-PAID-1042   PIC S9(13)V99 COMP-3.    9915845
031030         05  WMS-IOD-PRV-YTD-INT-PD-1042 PIC S9(13)V99 COMP-3.    9915845
031035         05  WMS-IOD-DIST-PL-CD      PIC XX.                      0617360
031100         05  FILLER                  PIC X(06).                   0617360
031110     03  WMS-IOD-INT-CYCLE.                                       9915845
031115         05  WMS-IOD-INT-PAY-CYCLE   PIC X.                       9915845
031120         05  WMS-IOD-INT-INCR        PIC X.                       9915845
031130         05  WMS-IOD-INT-MONTH1      PIC X(02).                   9915845
031140         05  WMS-IOD-INT-DAY         PIC X(02).                   9915845
031145         05  WMS-IOD-INT-DAY2        PIC X(02).                   9915845
031150         05  WMS-IOD-INT-CLEAR       PIC X.                       9915845
031160         05  WMS-IOD-INT-DATE-LAST-PMT.                           9915845
031170             07  WMS-IOD-INT-DL-PMT-MO   PIC X(02).               9915845
031180             07  WMS-IOD-INT-DL-PMT-DA   PIC X(02).               9915845
031190             07  WMS-IOD-INT-DL-PMT-YR   PIC X(02).               9915845
031195         05  FILLER                  PIC X(02).                   9915845
031200     03  WMS-SIGNATURE-CARD          PIC X.
031300     03  WMS-LOCATOR-NUMBER          PIC X(10).
031400     03  FILLER                      PIC X(10).                   9915845
031500     03  WMS-CHECKS-RETURNED         PIC S9(7)       COMP-3.
031600     03  WMS-AUTO-DEP-DA             PIC XX.
031700     03  WMS-MIN-BAL                 PIC S9(13)V99   COMP-3.      9915845
031800     03  WMS-TRANSFER-PRIORITY       PIC X.
031900     03  FILLER                      PIC X(15).                   9915845
032000     03  WMS-STATEMENT-DATA.
032100         05  WMS-STMT-SCHEDULE.
032200             07  WMS-STMT-PULL       PIC X.
032300             07  WMS-STMT-SUPP       PIC X.
032400             07  WMS-STMT-FIELDS.
032500                 09  WMS-STMT-FLD1   PIC 99.
032600                 09  WMS-STMT-FLD2   PIC 99.
032700                 09  WMS-STMT-FLD3   PIC 99.
032800                 09  WMS-STMT-FLD4   PIC 99.
032900                 09  WMS-STMT-FLD5   PIC 99.
033000                 09  WMS-STMT-FLD6   PIC 99.
033100                 09  WMS-STMT-FLD7   PIC 99.
033200                 09  WMS-STMT-FLD8   PIC 99.
033300                 09  WMS-STMT-FLD9   PIC 99.
033400                 09  WMS-STMT-FLD10  PIC 99.
033500                 09  WMS-STMT-FLD11  PIC 99.
033600                 09  WMS-STMT-FLD12  PIC 99.
033700             07  FILLER REDEFINES WMS-STMT-FIELDS.
033800                 09  WMS-STMT-FLD    PIC XX      OCCURS 12 TIMES.
033900         05  WMS-STMT-MSG            PIC X.
034000         05  WMS-STMT-FULL-SHEET     PIC X.
034100         05  WMS-STMT-FORMAT         PIC S9          COMP-3.
034200         05  WMS-STMT-CLEAR          PIC X.
034300         05  WMS-STMTS-EACH-MO       PIC 9       OCCURS 12 TIMES.
034400         05  WMS-STMT-BUMP-DAYS      PIC S999        COMP-3.
034500         05  WMS-STMT-CYCL-CLSD      PIC S9          COMP-3.
034600         05  WMS-DATE-LAST-STMT.
034700             07  WMS-LAST-STMT-MO    PIC XX.
034800             07  WMS-LAST-STMT-DA    PIC XX.
034900             07  WMS-LAST-STMT-DA9 REDEFINES WMS-LAST-STMT-DA
035000                                     PIC 99.
035100             07  WMS-LAST-STMT-YR    PIC XX.
035110         05  FILLER                  PIC X(2).                    9915845
035118         05  WMS-STMT-EARN-CRED      PIC S9(13)V99   COMP-3.      0337109
035120         05  WMS-IMAGES-PER-PAGE     PIC S9(3)       COMP-3.      0417110
035122         05  WMS-SIMPLEX-DUPLEX      PIC X(1).                    0417110
035124         05  FILLER                  PIC X(1).                    0417110
035130         05  WMS-STMT-WAIVED-SC-RP   PIC S9(13)V99   COMP-3.      0347045
035200         05  WMS-STMT-COMP-DATA                      COMP-3.
035300             07  WMS-STMT-BEGIN-BAL  PIC S9(13)V99.               9915845
035400             07  WMS-STMT-NO-CREDITS PIC S9(7).
035500             07  WMS-STMT-CR-AMT     PIC S9(13)V99.               9915845
035600             07  WMS-STMT-NO-DEBITS  PIC S9(7).
035700             07  WMS-STMT-DR-AMT     PIC S9(13)V99.               9915845
035800             07  WMS-STMT-SERV-CHG   PIC S9(13)V99.               9915845
035900             07  WMS-STMT-ITEMS      PIC S9(7).
036000             07  WMS-STMT-LINES      PIC S9(3).
036100             07  WMS-STMT-PAGE       PIC S9(5).
036200             07  WMS-STMT-DUPL       PIC S9.
036300         05  WMS-STMT-SPECL-HANDL    PIC X.
036400         05  WMS-STMT-TRAN-CHG       PIC S9(13)V99   COMP-3.      9915845
036500         05  WMS-NEXT-STMT-DATA.
036600             07  WMS-NEXT-STMT-FORMAT PIC S9         COMP-3.
036700             07  WMS-NEXT-STMT-PULL  PIC X.
036800             07  WMS-NEXT-STMT-SUPP  PIC X.
036900             07  WMS-NEXT-STMT-CYCLE PIC XX.
037000             07  WMS-NEXT-BULK-FILE  PIC X.
037100         05  WMS-LAST-STMT-CY-DAYX   PIC X.
037200         05  WMS-LAST-STMT-CY-DAY9 REDEFINES WMS-LAST-STMT-CY-DAYX
037300                                     PIC 9.
037320         05  WMS-NB-NXT-VEC-SEG-CD   PIC X(03).                   9715504
037327     03  WMS-NB-UNLIM-TEL-HLD        PIC X(01).                   9715504
037328     03  WMS-NB-INACT-NOT            PIC 9(01).                   9715504
037330     03  WMS-CUST-STMT-PRICE         PIC X.                       9715504
037340     03  WMS-NEXT-CHK-TRUNC-FLAG     PIC X.                       0527261
037350     03  WMS-PROV-HOLD-AMT           PIC S9(13)V99   COMP-3.      0927850
037400     03  FILLER                      PIC X(5).                    0927850
037500     03  WMS-ESCHEAT-DATE.
037600         05  WMS-ESCHEAT-CENT        PIC XX.
037700         05  WMS-ESCHEAT-YR          PIC XX.
037800         05  WMS-ESCHEAT-MO          PIC XX.
037900         05  WMS-ESCHEAT-DA          PIC XX.
038000     03  WMS-SERVICE-CHARGE-DATA.
038100         05  WMS-DATE-LAST-SERV-CHG.
038200             07  WMS-DL-SERV-CHG-MO  PIC XX.
038300             07  WMS-DL-SERV-CHG-DA  PIC XX.
038400             07  WMS-DL-SERV-CHG-YR  PIC XX.
038410         05  FILLER                  PIC X(2).                    9915845
038500         05  WMS-NEW-THIS-CYCLE      PIC X.
038600         05  WMS-SC-CHECK-CHG        PIC X.
038700         05  WMS-SC-CLEAR            PIC X.
038800         05  WMS-SC-CYCLE            PIC X.
038900         05  WMS-SC-FREQ.
039000             07  WMS-SC-INCR         PIC 9.
039100             07  WMS-SC-MONTH1       PIC 9.
039200         05  WMS-SC-DAY.
039300             07  WMS-SC-FLD1         PIC X.
039400             07  WMS-SC-FLD2         PIC X.
039500         05  WMS-SC-TYPE             PIC XXX.
039600         05  WMS-SC-ROUTINE-PTR      PIC S9      COMP-3.
039700         05  WMS-SC-CHARGE           PIC X.
039800         05  WMS-SC-WAIVE-REASON     PIC XX.
039900         05  WMS-SC-RELATED-ACCT.
040000             07  WMS-SC-REL-CODE     PIC X.
040100             07  WMS-SC-REL-NO.
040200                 09  WMS-REL-CTL1    PIC XX.
040300                 09  WMS-REL-CTL2    PIC XXX.
040400                 09  WMS-REL-CTL3    PIC XXX.
040410                 09  WMS-REL-CTL4-ACCT.                           0417078
040500                     11  WMS-REL-CTL4    PIC XXXX.                0417078
040600                     11  WMS-REL-ACCT    PIC X(10).               0417078
040602                 09  FILLER REDEFINES WMS-REL-CTL4-ACCT.          0417078
040604                     11  FILLER          PIC X(02).               0417078
040606                     11  WMS-REL-ACCT-12 PIC X(12).               0417078
040610         05  FILLER                  PIC X(14).                   9915845
040700         05  WMS-SERVICE-CHG-COMP-DATA               COMP-3.
040800             07  WMS-SC-AGGR-BAL     PIC S9(15)V99.               9915845
040900             07  WMS-SC-AGGR-COLL    PIC S9(15)V99.               9915845
041000             07  WMS-SC-AGGR-CUST-COLL   PIC S9(15)V99.           9915845
041100             07  WMS-SC-AGGR-OD-BAL  PIC S9(15)V99.               9915845
041200             07  WMS-SC-AGGR-DAYS    PIC S999.
041300             07  WMS-SC-MIN-BAL      PIC S9(13)V99.               9915845
041400             07  WMS-SC-NO-CHECKS    PIC S9(7).
041500             07  WMS-SC-DEBITS       PIC S9(7).
041600             07  WMS-SC-CREDITS      PIC S9(7).
041700             07  WMS-SC-MISC-COSTS   PIC S9(13)V99.               9915845
041800         05  WMS-SC-NSF-DATA                         COMP-3.
041900             07  WMS-SC-NSF-TIMES    PIC S999.
042000             07  WMS-SC-NSF-ITEMS    PIC S9(7).
042100         05  WMS-SC-UNAVAIL-DATA                     COMP-3.
042200             07  WMS-SC-UNAVAIL-TIMES    PIC S999.
042300             07  WMS-SC-UNAVAIL-ITEMS    PIC S9(7).
042400         05  WMS-SC-OD-DATA                          COMP-3.
042500             07  WMS-SC-OD-TIMES     PIC S999.
042600             07  WMS-SC-OD-ITEMS     PIC S9(7).
042700         05  WMS-SC-AMTTRNSFR-NO     PIC S9(7)       COMP-3.
042800         05  WMS-SC-CHECK-ITEMS      PIC S9(7)       COMP-3.
042900         05  WMS-SC-NUM-ACH          PIC S9(7)       COMP-3.
043000         05  WMS-SC-NUM-ATM          PIC S9(7)       COMP-3.
043100         05  WMS-SERV-CHARGE-CALCD   PIC S9(13)V99   COMP-3.      9915845
043200         05  WMS-SERV-CHARGE         PIC S9(13)V99   COMP-3.      9915845
043300         05  WMS-SC-COMP-BAL         PIC S9(13)V99   COMP-3.      9915845
043400         05  WMS-SC-PAY-PERIOD-CTR   PIC S999        COMP-3.
043500         05  WMS-SAV-BAL-FLAG        PIC X.
043600         05  WMS-SC-FIRST-CHECK      PIC X.
043700         05  WMS-SC-TRANSFER-DATA                    COMP-3.
043800             07  WMS-SC-TRAN-CHG     PIC S9(13)V99.               9915845
043900             07  WMS-SC-TRAN-NO      PIC S9(5).
044000             07  WMS-SC-TRAN-AMT     PIC S9(13)V99.               9915845
044100         05  WMS-SC-DDA-MIN          PIC S9(13)V99   COMP-3.      9915845
044200         05  WMS-SC-AGG-STOPS        PIC S9(7)       COMP-3.
044300         05  WMS-SC-AGG-HOLDS        PIC S9(7)       COMP-3.
044310         05  WMS-NB-CHRG-OFF-SC      PIC S9(13)V99   COMP-3.      9915845
044320         05  WMS-NB-CHRG-OFF-NSF     PIC S9(13)V99   COMP-3.      9915845
044330         05  WMS-SC-AGGR-DDA         PIC S9(15)V99   COMP-3.      9916070
044400     03  FILLER                      PIC X(11).                   9916070
044500     03  WMS-IOD-TRUTH-IN-SAVINGS.
044510         05  WMS-IOD-TIS-CYC-BEGIN-ACCR                           9715232
044520                                     PIC S9(13)V99   COMP-3.      9915845
044600         05  WMS-IOD-TIS-AGGR-BAL    PIC S9(15)V99   COMP-3.
044700         05  WMS-IOD-TIS-AGGR-DAYS   PIC S999        COMP-3.
044800         05  WMS-IOD-TIS-CYC-FEES    PIC S9(13)V99   COMP-3.      9915845
044900         05  WMS-IOD-TIS-EARN-INT    PIC S9(11)V9(6) COMP-3.      9915845
045000         05  WMS-IOD-TIS-AVG-BAL     PIC S9(13)V99   COMP-3.
045100         05  WMS-IOD-TIS-AVG-DAYS    PIC S999        COMP-3.
045200         05  WMS-IOD-TIS-REPT-INT    PIC S9(13)V99   COMP-3.      9915845
045300         05  WMS-IOD-TIS-INT-DATE    PIC X(6).
045310         05  FILLER                  PIC X(2).                    9915845
045400         05  WMS-IOD-TIS-INT-RECALC  PIC X.
045500     03  WMS-IOD-PEND-TAX            PIC S9(13)V99   COMP-3.      9915845
045600     03  WMS-MONTH-TO-DATE-DATA.
045700         05  WMS-NEW-THIS-MONTH      PIC X.
045800         05  WMS-MTD-AGGREGATES                      COMP-3.
045900             07  WMS-MTD-AGGR-BAL    PIC S9(15)V99.               9915845
046000             07  WMS-MTD-AGGR-COLL   PIC S9(15)V99.               9915845
046100             07  WMS-MTD-NEG-AGGR-CUST-COLL   PIC S9(15)V99.      2016654
046200             07  WMS-MTD-AGGR-CUST-COLL  PIC S9(15)V99.           9915845
046300             07  WMS-MTD-AGGR-OD     PIC S9(15)V99.               9915845
046400             07  WMS-MTD-AGGR-DAYS   PIC S999.
046500             07  WMS-MTD-NEG-AGGR-COLL   PIC S9(15)V99.           2016654
046600         05  WMS-MTD-MIN-BAL         PIC S9(13)V99   COMP-3.      9915845
046700         05  WMS-MTD-HIGH-BAL        PIC S9(13)V99   COMP-3.      9915845
046800         05  WMS-MTD-SVC-CHG-DR      PIC S9(7)       COMP-3.
046900         05  WMS-MTD-SVC-CHG-CR      PIC S9(7)       COMP-3.
047000         05  WMS-MTD-MISC-COSTS      PIC S9(13)V99   COMP-3.      9915845
047100         05  WMS-MTD-NSF-DATA                        COMP-3.
047200             07  WMS-MTD-NSF-TIMES   PIC S999.
047300             07  WMS-MTD-NSF-ITEMS   PIC S9(7).
047400         05  WMS-MTD-UNAVAIL-DATA                    COMP-3.
047500             07  WMS-MTD-UNAVAIL-TIMES   PIC S999.
047600             07  WMS-MTD-UNAVAIL-ITEMS   PIC S9(7).
047700         05  WMS-MTD-OD-DATA                         COMP-3.
047800             07  WMS-MTD-OD-DAYS     PIC S999.
047900             07  WMS-MTD-OD-TIMES    PIC S999.
048000             07  WMS-MTD-OD-ITEMS    PIC S9(7).
048100         05  WMS-MTD-CALC-SERV-CHG   PIC S9(13)V99   COMP-3.      9915845
048200         05  WMS-MTD-KITING-SUSP     PIC S9(3)       COMP-3.
048300         05  WMS-MTD-ANALYSIS        PIC X.
048400         05  WMS-MTD-ACCUM-SERV-CHG  PIC S9(13)V99   COMP-3.      9915845
048500         05  WMS-MTD-ACCUM-TRAN-CHG  PIC S9(13)V99   COMP-3.      9915845
048600         05  WMS-MTD-TRN-NO          PIC S999        COMP-3.
048700         05  WMS-MTD-TRAN-AMT        PIC S9(15)V99   COMP-3.      9915845
048800         05  WMS-MTD-DDA-MIN         PIC S9(13)V99   COMP-3.      9915845
048900         05  WMS-MTD-NUM-ACH         PIC S9(7)       COMP-3.
049000         05  WMS-MTD-NUM-ATM         PIC S9(7)       COMP-3.
049100         05  WMS-MTD-ACCRUAL-DAYS    PIC S999        COMP-3.
049110     03  WMS-NEW-BASE-FIELDS2.                                    9715504
049120         05  WMS-NB-TODAYS-BEGIN-BAL PIC S9(13)V99   COMP-3.      9715504
049130         05  WMS-NB-TODAYS-COLL-BAL  PIC S9(13)V99   COMP-3.      9715504
049132         05  WMS-YTD-ACCUM-SERV-CHG  PIC S9(13)V99   COMP-3.      0617360
049134         05  WMS-PYTD-ACCUM-SERV-CHG PIC S9(13)V99   COMP-3.      0617360
049136         05  WMS-QTD-AGGR-BAL        PIC S9(15)V99   COMP-3.      0727675
049138         05  WMS-QTD-AGGR-DAYS       PIC S999        COMP-3.      0727675
049140         05  WMS-INT-QTD-AGGR-BAL    PIC S9(15)V99   COMP-3.      0727675
049142         05  WMS-INT-QTD-AGGR-DAYS   PIC S999        COMP-3.      0727675
049144         05  WMS-OD-REGE-OPT-CODE    PIC X.                       1020116
049146         05  WMS-OD-REGE-OPT-DATE.                                1020116
049148             07  WMS-OD-REGE-OPT-YEAR.                            1020116
049150                 09  WMS-OD-REGE-OPT-CENT PIC XX.                 1020116
049152                 09  WMS-OD-REGE-OPT-YR   PIC XX.                 1020116
049154             07  WMS-OD-REGE-OPT-MO       PIC XX.                 1020116
049156             07  WMS-OD-REGE-OPT-DA       PIC XX.                 1020116
049158         05  FILLER REDEFINES WMS-OD-REGE-OPT-DATE.               1020116
049160             07  FILLER              PIC XX.                      1020116
049162             07  WMS-OD-REGE-DATE    PIC X(6).                    1020116
049164         05  WMS-OD-REGE-OPT-REASON  PIC XX.                      1020116
049166         05  WMS-OD-REGE-OPT-YTD     PIC S999        COMP-3.      1020116
049168         05  WMS-OD-REGE-OPT-PYTD    PIC S999        COMP-3.      1020116
049170         05  WMS-OD-REGE-SUST-CTR    PIC S999        COMP-3.      1020116
049200         05  FILLER                  PIC X(09).                   1020116
049300     03  WMS-YEAR-TO-DATE-AGGREGATES                 COMP-3.
049400         05  WMS-YTD-AGGR-BAL        PIC S9(15)V99.               9915845
049500         05  WMS-YTD-AGGR-DAYS       PIC S999.
049600     03  WMS-IOD-YTD-ACC-INT         PIC S9(13)V99   COMP-3.      9915845
049700     03  WMS-AVG-BAL-LAST-YEAR       PIC S9(13)V99   COMP-3.      9915845
049800     03  WMS-CLUB-ACCOUNT-AREA.
049900         05  WMS-CLUB-ACCT-CONTROL   PIC X.
050000         05  WMS-CLUB-ACCT-STATUS    PIC X.
050100         05  WMS-CLUB-TIMES-CHANGED  PIC S9          COMP-3.
050200         05  WMS-CLUB-CURRENT-TYPE   PIC XX.
050300         05  WMS-CLUB-LAST-TYPE      PIC XX.
050310     03  WMS-OD-LIMIT-FILE           PIC X.                       9915845
050320     03  WMS-MULTI-LANGUAGE-INFO.                                 9915845
050330         05  WMS-CUST-LANGUAGE       PIC XX.                      9915845
050340         05  WMS-CUST-LANG-GROUP     PIC X.                       9915845
050350     03  WMS-CURR-INFO.                                           9915845
050360         05  WMS-CURR-CODE           PIC XXX.                     9915845
050370         05  WMS-CURR-DEC            PIC X.                       9915845
050380         05  FILLER                  PIC X(4).                    9915845
050400     03  WMS-PASSBOOK-INFO.                                       9915845
050410         05  WMS-PASSBOOK-ACCT       PIC X.                       9915845
050420         05  WMS-PASSBOOK-BALANCE    PIC S9(13)V99   COMP-3.      9915845
050430         05  WMS-PASSBOOK-DATE.                                   9915845
050440             07  WMS-PBD-YEAR.                                    9915845
050450                 09  WMS-PBD-CENT    PIC XX.                      9915845
050460                 09  WMS-PBD-YR      PIC XX.                      9915845
050470             07  WMS-PBD-MO          PIC XX.                      9915845
050480             07  WMS-PBD-DA          PIC XX.                      9915845
050490         05  FILLER REDEFINES WMS-PASSBOOK-DATE.                  9915845
050500             07  FILLER              PIC XX.                      9915845
050510             07  WMS-PB-DATE         PIC X(6).                    9915845
050520         05  FILLER                  PIC X(8).                    9915845
050530     03  WMS-IBT-INFO.                                            9915845
050540         05  WMS-IBT-FLAG            PIC X.                       9915845
050550         05  WMS-IBT-REROUTE-FLAG    PIC X.                       9915845
050560         05  WMS-IBT-RETENTION-DAYS  PIC S999        COMP-3.      9915845
050570         05  WMS-IBT-OLD-NEW-ACCOUNT PIC X(20).                   9915845
050580     03  WMS-OD-PER-CHRG-FLAG        PIC X.                       9916073
050590     03  WMS-OD-PER-GRACE-DAYS       PIC S9(03)      COMP-3.      9916073
050600     03  WMS-OD-PER-CHRG-DAYS        PIC S9(03)      COMP-3.      9916073
050610     03  WMS-OD-PER-MIN-AMT          PIC S9(13)V99   COMP-3.      9916073
050620     03  WMS-OD-PER-CHRG-AMT         PIC S9(13)V99   COMP-3.      9916073
050630     03  WMS-EXTERNAL-INVESTMENT.                                 2016639
050640         05  WMS-XINV-LINK-IND       PIC X.                       2016639
050650         05  WMS-XINV-PROCESS-IND    PIC X.                       2016639
050660         05  WMS-XINV-BAL-CALC       PIC X.                       2016639
050670         05  WMS-XINV-RESTRICT       PIC X.                       2016639
050680         05  WMS-XINV-FUND.                                       2016639
050682             07  WMS-XINV-FUND-GROUP PIC X.                       2016639
050684             07  WMS-XINV-FUND-NBR   PIC X(3).                    2016639
050690         05  WMS-XINV-LINK-DATE.                                  2016639
050691             07  WMS-XINV-LINK-YEAR.                              2016639
050692                 09  WMS-XINV-LINK-CC    PIC XX.                  2016639
050693                 09  WMS-XINV-LINK-YR    PIC XX.                  2016639
050694             07  WMS-XINV-LINK-MO        PIC XX.                  2016639
050695             07  WMS-XINV-LINK-DA        PIC XX.                  2016639
050696         05  FILLER REDEFINES WMS-XINV-LINK-DATE.                 2016639
050697             07  FILLER                  PIC XX.                  2016639
050698             07  WMS-XINV-LINK-DATE6     PIC X(6).                2016639
050700         05  WMS-XINV-DELINK-DATE.                                2016639
050701             07  WMS-XINV-DELINK-YEAR.                            2016639
050702                 09  WMS-XINV-DELINK-CC  PIC XX.                  2016639
050703                 09  WMS-XINV-DELINK-YR  PIC XX.                  2016639
050704             07  WMS-XINV-DELINK-MO      PIC XX.                  2016639
050705             07  WMS-XINV-DELINK-DA      PIC XX.                  2016639
050706         05  FILLER REDEFINES WMS-XINV-DELINK-DATE.               2016639
050707             07  FILLER                  PIC XX.                  2016639
050708             07  WMS-XINV-DELINK-DATE6   PIC X(6).                2016639
050710         05  WMS-XINV-BALANCE        PIC S9(13)V99   COMP-3.      2016639
050720         05  WMS-XINV-ACCR-DIV       PIC S9(13)V99   COMP-3.      2016639
050730         05  FILLER                  PIC X(8).                    2016639
050740         05  WMS-XINV-AGGREGATES.                                 2016639
050742             07  WMS-XINV-CTD-AGGR   PIC S9(15)V99   COMP-3.      2016639
050744             07  WMS-XINV-CTD-DAYS   PIC S9(03)      COMP-3.      2016639
050746             07  WMS-XINV-MTD-AGGR   PIC S9(15)V99   COMP-3.      2016639
050748             07  WMS-XINV-MTD-DAYS   PIC S9(03)      COMP-3.      2016639
050750             07  WMS-XINV-YTD-AGGR   PIC S9(15)V99   COMP-3.      2016639
050752             07  WMS-XINV-YTD-DAYS   PIC S9(03)      COMP-3.      2016639
050754         05  WMS-XINV-ACCT           PIC X(15).                   2016639
050756         05  WMS-XINV-FILLER         PIC X(24).                   2016639
050760     03  WMS-EDITED-ACCOUNT.                                      0417078
050770         05  WMS-EDITED-ACCT         PIC X(12).                   0417078
050780         05  FILLER                  PIC X(12).                   0417078
051000     03  WMS-OVER-OD-LIMIT-DATE.                                  0547276
051010         05  WMS-OVER-OD-LIMIT-YEAR.                              0547276
051012             07  WMS-OVER-OD-LIMIT-CC    PIC XX.                  0547276
051014             07  WMS-OVER-OD-LIMIT-YR    PIC XX.                  0547276
051016         05  WMS-OVER-OD-LIMIT-MO        PIC XX.                  0547276
051018         05  WMS-OVER-OD-LIMIT-DA        PIC XX.                  0547276
051020     03  FILLER REDEFINES WMS-OVER-OD-LIMIT-DATE.                 0547276
051022         05  FILLER                      PIC XX.                  0547276
051024         05  WMS-OVER-OD-LIMIT-DATE6     PIC X(6).                0547276
051026     03  WMS-OVER-OD-DAYS                PIC S999 COMP-3.         0547276
051028     03  WMS-STMT-OVERDRAFT-CHGD-CTD     PIC S9(13)V99  COMP-3.   0627451
051030     03  WMS-STMT-OVERDRAFT-CHGD-YTD     PIC S9(13)V99  COMP-3.   0627451
051032     03  WMS-STMT-NSF-CHGD-CTD           PIC S9(13)V99  COMP-3.   0627451
051034     03  WMS-STMT-NSF-CHGD-YTD           PIC S9(13)V99  COMP-3.   0627451
051036     03  WMS-STMT-OVERDRAFT-CHGD-PYTD    PIC S9(13)V99  COMP-3.   0930023
051038     03  WMS-STMT-NSF-CHGD-PYTD          PIC S9(13)V99  COMP-3.   0930023
051040     03  WMS-NB-SS-RT-START-DT           PIC X(8).                0930013
051042     03  WMS-NB-SS-RT-STOP-DT            PIC X(8).                0930013
051060     03  FILLER                          PIC X(22).               0930013
051195     03  WMS-BONUS-RATE-AREA.                                     2016374
051200         05  WMS-BIR-FLAG            PIC X.                       2016374
051205         05  WMS-BIR-INIT-WD-NBR     PIC S9(7)       COMP-3.      2016374
051210         05  WMS-BIR-WD-NBR          PIC S9(7)       COMP-3.      2016374
051215         05  WMS-BIR-WD-AMT          PIC S9(13)V99   COMP-3.      2016374
051220         05  WMS-BIR-DP-NBR          PIC S9(7)       COMP-3.      2016374
051225         05  WMS-BIR-DP-AMT          PIC S9(13)V99   COMP-3.      2016374
051230         05  WMS-BIR-PYMT-METH       PIC X.                       2016374
051235         05  WMS-BIR-GRACE-MOS       PIC S9          COMP-3.      2016374
051240         05  WMS-BIR-MTD-WD-NBR      PIC S9(7)       COMP-3.      2016374
051245         05  WMS-BIR-MTD-WD-AMT      PIC S9(13)V99   COMP-3.      2016374
051250         05  WMS-BIR-MTD-DP-NBR      PIC S9(7)       COMP-3.      2016374
051255         05  WMS-BIR-MTD-DP-AMT      PIC S9(13)V99   COMP-3.      2016374
051260         05  WMS-BIR-CONSEC-MOS      PIC S9(7)       COMP-3.      2016374
051265         05  WMS-BIR-AMT-PD          PIC S9(13)V99   COMP-3.      2016374
051400     03  WMS-CASH-AVAIL-FLAG         PIC X.                       9915845
051500     03  WMS-CASH-AVAIL-AMT          PIC S9(13)V99   COMP-3.      9915845
051600     03  WMS-CASH-AVAIL-MTD-AGGR     PIC S9(15)V99   COMP-3.      9915845
051700     03  WMS-CASH-AVAIL-CTD-AGGR     PIC S9(15)V99   COMP-3.      9915845
051800     03  WMS-CASH-AVAIL-YTD-AGGR     PIC S9(15)V99   COMP-3.      9915845
051900     03  WMS-NAME-CTL                PIC X(4).
051910     03  WMS-SUSPECT-LIMIT-RPT       PIC X.                       0266778
051920     03  WMS-SUSPECT-LIMIT           PIC S9(13)V99   COMP-3.      0266778
052000     03  FILLER                      PIC X.                       0266778
052001     03  WMS-NEW-BASE-FIELDS.                                     9715504
052002         05  WMS-NB-TMI-AREA.                                     9715504
052003             07  WMS-NB-DC-ABM           PIC X.                   9715504
052004             07  WMS-NB-DC-ACH-ORIG      PIC X.                   9715504
052005             07  WMS-NB-DC-CDA           PIC X.                   9715504
052006             07  WMS-NB-DC-EDI           PIC X.                   9715504
052007             07  WMS-NB-DC-LOCKBOX       PIC X.                   9715504
052008             07  WMS-NB-DC-NBW-BAL-CURR  PIC X.                   9715504
052009             07  WMS-NB-DC-NBW-BAL-PRV   PIC X.                   9715504
052010             07  WMS-NB-DC-NBW-INQ       PIC X.                   9715504
052011             07  WMS-NB-DC-NBW-ST-PAY    PIC X.                   9715504
052012             07  WMS-NB-DC-WIRE          PIC X.                   9715504
052013             07  WMS-NB-DC-BUS-EXPR      PIC X.                   9715504
052014             07  WMS-NB-DC-VLT-SVCS      PIC X.                   9715504
052015             07  WMS-NB-DC-DEP-PLUS      PIC X.                   9715504
052016             07  WMS-NB-DC-RSVE4         PIC X.                   9715504
052017             07  WMS-NB-DC-RSVE5         PIC X.                   9715504
052020         05  WMS-NB-SC-AREA.                                      9715504
052021             07  WMS-NB-WVE-EXP-DT.                               9715504
052022                 09  WMS-NB-WVE-EXP-MM   PIC X(02).               9715504
052023                 09  WMS-NB-WVE-EXP-DD   PIC X(02).               9715504
052024                 09  WMS-NB-WVE-EXP-CC   PIC X(02).               9715504
052025                 09  WMS-NB-WVE-EXP-YY   PIC X(02).               9715504
052026             07  WMS-NB-ANC-TYPE         PIC X(03).               9715504
052027             07  WMS-NB-ANC-WV-EX-DT.                             9715504
052028                 09  WMS-NB-ANC-WV-EX-MM PIC X(02).               9715504
052029                 09  WMS-NB-ANC-WV-EX-DD PIC X(02).               9715504
052030                 09  WMS-NB-ANC-WV-EX-CC PIC X(02).               9715504
052031                 09  WMS-NB-ANC-WV-EX-YY PIC X(02).               9715504
052032             07  WMS-NB-ANC-WVE-RSN      PIC X(02).               9715504
052033             07  WMS-NB-ANC-CHARGE       PIC X(01).               9715504
052034             07  WMS-NB-FEE-WV-EX-DT.                             9715504
052035                 09  WMS-NB-FEE-WV-EX-MM PIC X(02).               9715504
052036                 09  WMS-NB-FEE-WV-EX-DD PIC X(02).               9715504
052037                 09  WMS-NB-FEE-WV-EX-CC PIC X(02).               9715504
052038                 09  WMS-NB-FEE-WV-EX-YY PIC X(02).               9715504
052039             07  WMS-NB-FEE-WVE-RSN      PIC X(02).               9715504
052040             07  WMS-NB-DISC-AREA        PIC X(20).               9715504
052041             07  WMS-NB-DISC-FEE REDEFINES WMS-NB-DISC-AREA       9715504
052042                                         OCCURS 5 TIMES.          9715504
052043                 09  WMS-NB-DISC-FEE-TYP PIC X(01).               9715504
052044                 09  WMS-NB-DISC-FEE-NBR PIC X(03).               9715504
052045             07  WMS-NB-SEAS-WVE-DT.                              9715504
052046                 09  WMS-NB-SEAS-WVE-MM  PIC X(02).               9715504
052047                 09  WMS-NB-SEAS-WVE-DD  PIC X(02).               9715504
052048                 09  WMS-NB-SEAS-WVE-CC  PIC X(02).               9715504
052049                 09  WMS-NB-SEAS-WVE-YY  PIC X(02).               9715504
052050             07  WMS-NB-REL-IND          PIC X(01).               9715504
052051             07  WMS-NB-MTD-MIN-WVE      PIC S9(13)V99   COMP-3.  9915845
052052             07  WMS-NB-MTD-WVE-AMT      PIC S9(13)V99   COMP-3.  9915845
052053             07  WMS-NB-MTD-SC-AMT       PIC S9(13)V99   COMP-3.  9915845
052054             07  WMS-NB-YTD-MIN-WVE      PIC S9(13)V99   COMP-3.  9915845
052055             07  WMS-NB-YTD-WVE-AMT      PIC S9(13)V99   COMP-3.  9915845
052056             07  WMS-NB-YTD-SC-AMT       PIC S9(13)V99   COMP-3.  9915845
052057             07  WMS-NB-REGD-NTC-NBR     PIC X(01).               9715504
052058             07  WMS-NB-REGD-TR-VIOL     PIC S9(03)      COMP-3.  9715504
052059             07  WMS-NB-REGD-CK-VIOL     PIC S9(03)      COMP-3.  9715504
052060             07  WMS-NB-STU-ANN-DT.                               9715504
052061                 09  WMS-NB-STU-ANN-MM   PIC X(02).               9715504
052062                 09  WMS-NB-STU-ANN-DD   PIC X(02).               9715504
052063                 09  WMS-NB-STU-ANN-YY   PIC X(02).               9715504
052064             07  FILLER                  PIC X(02).               9915845
052065         05  WMS-NB-ACCOUNT-FLAGS.                                9715504
052066             07  WMS-NB-OD-LIMIT-SCOR-TYPE PIC X(01).             9715504
052067             07  WMS-NB-FUNDS-OWNER  PIC X(02).                   9715504
052068             07  WMS-NB-VEC-SEG-CD   PIC X(03).                   9715504
052069             07  WMS-NB-REL-PRCNG-WVE-IND  PIC X(01).             9715504
052070             07  WMS-NB-STMT-SPECL-HANDL-2 PIC X(01).             9715504
052071             07  FILLER              PIC X(01).                   9715504
052072             07  WMS-NB-ODP-TYP-IND  PIC X(01).                   9715504
052073             07  WMS-NB-LOB-IND      PIC 9(02).                   9715504
052074             07  WMS-NB-PST-RST-CD   PIC X(01).                   9715504
052075             07  WMS-NB-DDA-SAV-TYPE PIC X(01).                   9715504
052076             07  WMS-NB-TFR-IND      PIC X(01).                   9715504
052077             07  WMS-NB-ALT-NA-IND   PIC X(01).                   9715504
052085         05  WMS-NB-SPEC-SALES.                                   9715504
052086             07  WMS-NB-SS-IND       PIC X(01).                   9715504
052087             07  WMS-NB-SS-MNT-DISC  PIC S9V9(08)    COMP-3.      9915845
052088             07  WMS-NB-SS-RT-IND    PIC X(01).                   9715504
052089             07  WMS-NB-SS-RT-ADJ    PIC S9V9(08)    COMP-3.      9915845
052090         05  WMS-NB-DUNS-NBR         PIC X(09).                   9715504
052091         05  WMS-NB-MAIL-RTN-CNT     PIC S9(05)      COMP-3.      9715504
052092         05  WMS-NB-TOT-DMT-SC       PIC S9(13)V99   COMP-3.      9915845
052093         05  WMS-NB-MIN-FND-BAL      PIC S9(13)V99   COMP-3.      9915845
052094         05  WMS-NB-PTD-IND          PIC X(01).                   9715504
052095         05  WMS-NB-HOLD-AREA.                                    9715504
052096             07  WMS-NB-DEP-HLD-IND  PIC X(01).                   9715504
052097             07  WMS-NB-DEP-HLD-PTS  PIC S9(03)      COMP-3.      9715504
052098             07  WMS-NB-CST-UNAV-HLD PIC S9(13)V99   COMP-3.      9715504
052099             07  WMS-NB-OTH-HLD      PIC S9(13)V99   COMP-3.      9715504
052100         05  WMS-NB-STMT-AREA.                                    9715504
052101             07  WMS-NB-SEAS-MSG-IND PIC X(01).                   9715504
052102             07  WMS-NB-DMT-CNT      PIC S9(03)      COMP-3.      9715504
052103             07  WMS-NB-SPEC-SMT-IND PIC X(01).                   9715504
052105         05  WMS-NB-DATE-AREA.                                    9715504
052106             07  WMS-NB-ODP-TRNS-DT.                              9715504
052107                 09  WMS-NB-ODP-TRNS-MM   PIC X(02).              9715504
052108                 09  WMS-NB-ODP-TRNS-DD   PIC X(02).              9715504
052109                 09  WMS-NB-ODP-TRNS-YY   PIC X(02).              9715504
052110             07  WMS-NB-OD-EXP-DT.                                9715504
052111                 09  WMS-NB-OD-EXP-MM     PIC X(02).              9715504
052112                 09  WMS-NB-OD-EXP-DD     PIC X(02).              9715504
052113                 09  WMS-NB-OD-EXP-YY     PIC X(02).              9715504
052114             07  WMS-NB-DRCRD-ISS-DT.                             9715504
052115                 09  WMS-NB-DRCRD-ISS-MM  PIC X(02).              9715504
052116                 09  WMS-NB-DRCRD-ISS-DD  PIC X(02).              9715504
052117                 09  WMS-NB-DRCRD-ISS-YY  PIC X(02).              9715504
052118             07  WMS-NB-DRCRD-FEE-EFFDT.                          9715504
052119                 09  WMS-NB-DRCRD-FEE-MM  PIC X(02).              9715504
052120                 09  WMS-NB-DRCRD-FEE-DD  PIC X(02).              9715504
052121                 09  WMS-NB-DRCRD-FEE-YY  PIC X(02).              9715504
052125             07  FILLER                   PIC X(08).              9915845
052130         05  WMS-NB-IOD.                                          9715504
052131             07  WMS-NB-INVEST-BAL   PIC S9(13)V99   COMP-3.      9915845
052132             07  WMS-NB-RSRVE-RT     PIC S9V9(8)     COMP-3.      9915845
052135         05  WMS-NB-SWEEPS-AREA.                                  9715504
052136             07  WMS-NB-INT-MTD-TSSG PIC S9(13)V99   COMP-3.      9915845
052137             07  WMS-NB-SWP-MIN      PIC S9(13)V99   COMP-3.      9915845
052140         05  FILLER                  PIC X(19).                   9915845
052145         05  WMS-NB-DC-GOAL          PIC X(40).                   9715504
052146         05  WMS-NB-DC-GOAL-AMT      PIC S9(13)V99   COMP-3.      9915845
052147         05  WMS-NB-DC-OFFICER2      PIC X(05).                   9715504
052148         05  FILLER                  PIC X.                       9915845
052149         05  WMS-NB-DC-OFFICER3      PIC X(05).                   9915845
052150         05  FILLER                  PIC X.                       9915845
052151         05  WMS-NB-DC-MINOR-CONV-DT.                             9915845
052152             07  WMS-NB-DC-MINOR-CONV-MM      PIC X(02).          9915845
052153             07  WMS-NB-DC-MINOR-CONV-DD      PIC X(02).          9915845
052154             07  WMS-NB-DC-MINOR-CONV-YY      PIC X(02).          9915845
052155         05  WMS-NB-DC-STUDENT-AREA.                              9715504
052156             07  WMS-NB-DC-GRAD-DT.                               9715504
052157                 09  WMS-NB-DC-GRAD-CC        PIC X(02).          9715504
052158                 09  WMS-NB-DC-GRAD-YY        PIC X(02).          9715504
052159                 09  WMS-NB-DC-GRAD-MM        PIC X(02).          9715504
052160             07  WMS-NB-DC-GRAD-EXP-IND       PIC X(01).          9715504
052161             07  WMS-NB-DC-MAINT-GEN          PIC X(01).          9715504
052162             07  WMS-NB-DC-STU-TYPE           PIC X(01).          9715504
052163             07  WMS-NB-DC-UNIV               PIC X(40).          9715504
052164             07  FILLER                       PIC X(01).          9715504
052165         05  FILLER                           PIC X(4).           9915845
052170         05  WMS-NB-DC-REGD-AREA.                                 9715504
052171             07  WMS-NB-DC-REGD-PRD-CNV       PIC X(01).          9715504
052172             07  WMS-NB-DC-REGD-DT1.                              9715504
052173                 09  WMS-NB-DC-REGD-MM1       PIC X(02).          9715504
052174                 09  WMS-NB-DC-REGD-DD1       PIC X(02).          9715504
052175                 09  WMS-NB-DC-REGD-YY1       PIC X(02).          9715504
052176             07  WMS-NB-DC-REGD-DT2.                              9715504
052177                 09  WMS-NB-DC-REGD-MM2       PIC X(02).          9715504
052178                 09  WMS-NB-DC-REGD-DD2       PIC X(02).          9715504
052179                 09  WMS-NB-DC-REGD-YY2       PIC X(02).          9715504
052180             07  WMS-NB-DC-REGD-DT3.                              9715504
052181                 09  WMS-NB-DC-REGD-MM3       PIC X(02).          9715504
052182                 09  WMS-NB-DC-REGD-DD3       PIC X(02).          9715504
052183                 09  WMS-NB-DC-REGD-YY3       PIC X(02).          9715504
052184             07  WMS-NB-DC-REGD-CVT-DT.                           9715504
052185                 09  WMS-NB-DC-REGD-CVT-MM    PIC X(02).          9715504
052186                 09  WMS-NB-DC-REGD-CVT-DD    PIC X(02).          9715504
052187                 09  WMS-NB-DC-REGD-CVT-YY    PIC X(02).          9715504
052188             07  WMS-NB-DC-REGD-MAX-CKS  PIC S9(03)    COMP-3.    9715504
052190             07  FILLER                       PIC X(8).           9915845
052195     03  WMS-TRAILER-CNTL.                                        9715504
052200         05  WMS-NME-ADDR-TLRS       PIC S9          COMP-3.
052300         05  WMS-SOC-SEC-NA          PIC X.
052500         05  WMS-LOAN-TRLR           PIC X.
052600         05  WMS-MARKET-TRLR         PIC X.
052700         05  WMS-TRNSFR-AFFL-TRLR    PIC S999        COMP-3.      9915845
052800         05  WMS-OD-NSF-TRLR         PIC X.
052900         05  WMS-SAVINGS-TRLR        PIC X.
053000         05  WMS-BANK-AVAIL-TRLR     PIC X.
053100         05  WMS-CUST-AVAIL-TRLR     PIC X.
053200         05  WMS-COMBINED-STAT       PIC S999        COMP-3.
053300         05  WMS-TARGET-AMT-TRLR     PIC X.
053400         05  WMS-LIMIT-TRANSFER      PIC X.
053500         05  WMS-DCD-TAX-DATA-TRLR   PIC X.
053600         05  WMS-OD-ACCRUAL-TRLR     PIC X.
053700         05  WMS-INFO-TRLR           PIC X.
053900         05  WMS-EXTRNL-DEPOSIT-TRLR PIC X.
054000         05  WMS-KITING-SUSP-TRLR    PIC X.
054100         05  WMS-EFA-TRLR            PIC X.
054200         05  WMS-CASH-AVAIL-TRLR     PIC X.
054300         05  WMS-INVESTMENT-TRLR     PIC X.
054400         05  WMS-RATE-TRLR           PIC X.
054405         05  WMS-EXT-SC-DATA-TRLR    PIC X.                       9915845
054410         05  WMS-DUAL-YEAR-TRLR      PIC X.                       9915858
054415         05  WMS-PLAN-TRLR           PIC X.                       0617360
054500         05  FILLER                  PIC X(11).                   0617360
054550         05  WMS-DC-TRLR             PIC X.                       9915845
054600         05  WMS-SAV-PTR             PIC S9          COMP-3.
054700         05  WMS-EXPANSION-IND       PIC X.
054800*
054900     03  WMS-NAME-ADDRESS-TRAILER-1.
055000       04  WMS-NA-NO-LINES-1         PIC S9      COMP-3.
055010       04  WMS-NA-COUNTRY            PIC XX.                      9915845
055015       04  FILLER REDEFINES WMS-NA-COUNTRY.                       9916304
055020           05  WMS-NA-COUNTRY-1042   PIC X(02).                   9916304
055100       04  WMS-NA-LINE                           OCCURS 8 TIMES.  9715504
055200         05  WMS-NAME-ADDR-TYPE      PIC X.
055300         05  WMS-LINE-NO             PIC X.
055400         05  WMS-NAME-ADDRESS        PIC X(40).
055410       04  FILLER                    PIC X(50).                   9915845
055500     03  FILLER REDEFINES WMS-NAME-ADDRESS-TRAILER-1.
055600       04  FILLER                    PIC S9          COMP-3.
055610       04  FILLER                    PIC XX.                      9915845
055700       04  FILLER                                OCCURS 8 TIMES.  9715504
055800         05  FILLER                  PIC XX.
055900         05  WMS-CITY-STATE          PIC X(30).
056000         05  WMS-NA-ZIP-CODE         PIC X(10).
056010       04  FILLER                    PIC X(50).                   9915845
056100     03  FILLER REDEFINES WMS-NAME-ADDRESS-TRAILER-1.
056200       04  FILLER                    PIC S9          COMP-3.
056210       04  FILLER                    PIC XX.                      9915845
056300       04  FILLER                                OCCURS 8 TIMES.  9715504
056400         05  FILLER                  PIC XX.
056500         05  WMS-SPEC-INSTR          PIC X(40).
056510       04  FILLER                    PIC X(50).                   9915845
056600     03  FILLER REDEFINES WMS-NAME-ADDRESS-TRAILER-1.
056700       04  FILLER                    PIC S9          COMP-3.
056710       04  FILLER                    PIC XX.                      9915845
056800       04  FILLER                                OCCURS 8 TIMES.  9715504
056900         05  FILLER                  PIC XX.
057000         05  FILLER.
057100             07  WMS-NA-EFFECTIVE-DATES-1.
057200                 09  WMS-NA-START-DATE-1.
057300                     11  WMS-NA-START-MO-DA-1.
057400                         13  WMS-NA-START-MO-1   PIC XX.
057500                         13  WMS-NA-START-DA-1   PIC XX.
057600                     11  WMS-NA-START-YR-1       PIC XX.
057700                 09  WMS-NA-REMAIN-DAYS-1        PIC S999  COMP-3.
057900             07  WMS-NA-NO-STMTS-1   PIC S999        COMP-3.
058000             07  WMS-NA-USE-CODE-1   PIC X.
058100             07  WMS-NA-SSN-CODE-1   PIC X.
058200             07  WMS-NA-SOC-SEC-1.
058300                 09  WMS-NA-SSN-PRE-1    PIC X.
058400                 09  WMS-NA-SSN-NO-1     PIC X(9).
058500                 09  WMS-NA-SSN-SUF-1    PIC X.
058600             07  WMS-NA-PARTNER-PCT-1    PIC SV999   COMP-3.
058700             07  WMS-NA-USER-1           PIC X.
058800             07  WMS-NA-PHONE-1.
058900                 09  WMS-NA-AREA-CODE-1  PIC XXX.
059000                 09  WMS-NA-PHONE-NO-1   PIC X(7).
059110                 09  WMS-NA-EXTEN-NO-1   PIC X(4).                9915845
059120       04  FILLER                    PIC X(50).                   9915845
059190*                                                                 9915845
059200     03  WMS-NAME-ADDRESS-TRAILER-2.
059300         05  WMS-NA-NO-LINES-2       PIC S9          COMP-3.
059310         05  WMS-NA-COUNTRY-2        PIC XX.                      9915845
059400         05  WMS-NA-LINE-2                       OCCURS 8 TIMES.  9715504
059500             07  WMS-NAME-ADDR-TYPE-2    PIC X.
059600             07  WMS-LINE-NO-2           PIC X.
059700             07  WMS-NAME-ADDRESS-2.
059800                 09  WMS-NA-CITY-STATE-2 PIC X(30).
059900                 09  WMS-NA-ZIP-CODE-2   PIC X(10).
060000             07  FILLER REDEFINES WMS-NAME-ADDRESS-2.
060100                 09  WMS-NA-START-DATE-2.
060200                     11  WMS-NA-START-MO-DA-2.
060300                         13  WMS-NA-START-MO-2   PIC XX.
060400                         13  WMS-NA-START-DA-2   PIC XX.
060500                     11  WMS-NA-START-YR-2       PIC XX.
060600                 09  WMS-NA-REMAIN-DAYS-2        PIC S999 COMP-3.
060800                 09  WMS-NA-NO-STMTS-2           PIC S999 COMP-3.
060900                 09  WMS-NA-USE-CODE-2           PIC X.
061000                 09  WMS-NA-SSN-CODE-2           PIC X.
061100                 09  WMS-NA-SOC-SEC-2.
061200                     11  WMS-NA-SSN-PRE-2        PIC X.
061300                     11  WMS-NA-SSN-NO-2         PIC X(9).
061400                     11  WMS-NA-SSN-SUF-2        PIC X.
061500                 09  WMS-NA-PARTNER-PCT-2        PIC SV999 COMP-3.
061600                 09  WMS-NA-USER-2               PIC X.
061700                 09  WMS-NA-PHONE-2.
061800                     11  WMS-NA-AREA-CODE-2      PIC XXX.
061900                     11  WMS-NA-PHONE-NO-2       PIC X(7).
061910                     11  WMS-NA-EXTEN-NO-2       PIC X(4).        9915845
061920         05  FILLER                  PIC X(50).                   9915845
062000*
062100     03  WMS-SOC-SEC-NA-TRAILER.
062200         05  WMS-SSN-DELETE          PIC X.
062300         05  WMS-SSN-NAME            PIC X(40).
062400         05  WMS-SSN-ST-ADDR         PIC X(40).
062500         05  WMS-SSN-CITY-ST-ZIP.
062600             07  WMS-SSN-CITY-ST     PIC X(30).
062700             07  WMS-SSN-ZIP         PIC X(10).
062800         05  WMS-SSN-FOREIGN-ADDRESS PIC X.
062810         05  WMS-SSN-COUNTRY         PIC XX.                      9915845
062900         05  FILLER                  PIC X(11).
064500*
064600     03  WMS-LOAN-TRAILER.
064700         05  WMS-LOAN-STATUS         PIC X.
064800         05  WMS-AUTO-REACTIVATE     PIC X.
064900         05  WMS-PAST-DUE            PIC X.
065000         05  WMS-OVERLIMIT           PIC X.
065100         05  WMS-LOAN-CAUTION        PIC X.
065200         05  WMS-LOAN-BAD-ACCT       PIC X.
065300         05  WMS-OVERLIM-INCR        PIC X.
065400         05  WMS-WAIVE-FEE           PIC X.
065500         05  WMS-AUTO-PYMT           PIC X.
065600         05  WMS-WAIVE-PYMT          PIC X.
065700         05  WMS-SPLIT-RATE-IND      PIC X.
065800         05  WMS-CAPITALIZATION      PIC X.
065900         05  WMS-LOAN-NEW-THIS-CYCLE PIC X.
066000         05  WMS-LOAN-TO-MAKE-PYMT   PIC X.
066100         05  WMS-OVER-THIS-CYCLE     PIC X.
066200         05  WMS-PAYMENT-SUSPECT     PIC X.
066300         05  WMS-LOAN-TYPE           PIC XX.
066400         05  WMS-LOAN-OFFICER        PIC X(5).                    9715504
066410         05  WMS-LOAN-STAT-CHG       PIC X.                       0637636
066500         05  WMS-COMPT-CALL-CODE     PIC XXXX.
066600         05  WMS-HOME-EQUITY-CODE    PIC X.
066700         05  FILLER                  PIC XXX.                     9715504
066900         05  WMS-CONSEC-PAY-LN       PIC S999        COMP-3.
067000         05  WMS-DATE-LAST-RATE-CHANGE.
067100             07  WMS-RATE-CHANGE-MO  PIC XX.
067200             07  WMS-RATE-CHANGE-DA  PIC XX.
067300             07  WMS-RATE-CHANGE-YR  PIC XX.
067310         05  FILLER                  PIC X(2).                    9915845
067400         05  WMS-CR-LIMIT-PTR        PIC S9          COMP-3.
067500         05  WMS-LOAN-INCR-PTR       PIC S9          COMP-3.
067600         05  WMS-INT-RATE-PTR        PIC S999        COMP-3.
067700         05  WMS-SPLIT-RATE-SCHD     PIC S999        COMP-3.
067800         05  WMS-CREDIT-LIMIT        PIC S9(13)V99   COMP-3.      9915845
067900         05  WMS-LOAN-LINE-PCT       PIC S9V99       COMP-3.
068000         05  WMS-LOAN-INTEREST-RATE                  COMP-3.
068100             07  WMS-ANNUAL-RATE     PIC S9V9(8).                 9915845
068200             07  WMS-DAILY-RATE      PIC SVP9(15).                9915845
068300         05  WMS-LOAN-CALC-CODE      PIC X.
068400         05  WMS-LOAN-ACCR-AMT       PIC S9(11)V9(6) COMP-3.      9915845
068500         05  WMS-REVIEW-FREQ         PIC X.
068600         05  WMS-CREDIT-RATING.
068700             07  WMS-CREDIT-LAST-DATE.
068800                 09  WMS-CR-LAST-MO  PIC XX.
068900                 09  WMS-CR-LAST-DA  PIC XX.
069000                 09  WMS-CR-LAST-YR  PIC XX.
069100             07  WMS-CR-RATING-DATE.
069200                 09  WMS-CR-RATING-MO    PIC XX.
069300                 09  WMS-CR-RATING-DA    PIC XX.
069400                 09  WMS-CR-RATING-YR    PIC XX.
069500             07  WMS-CR-RATING           PIC XXX.
069510             07  FILLER              PIC X(4).                    9915845
069600         05  WMS-APPROVAL-DATE.
069700             07  WMS-APPROVAL-MO     PIC XX.
069800             07  WMS-APPROVAL-DA     PIC XX.
069900             07  WMS-APPROVAL-YR     PIC XX.
070000         05  WMS-PAST-DUE-DATE.
070100             07  WMS-PAST-DUE-MO     PIC XX.
070200             07  WMS-PAST-DUE-DA     PIC XX.
070300             07  WMS-PAST-DUE-YR     PIC XX.
070400         05  WMS-LOAN-CLOSED-DATE.
070500             07  WMS-LOAN-CLOSED-MO  PIC XX.
070600             07  WMS-LOAN-CLOSED-DA  PIC XX.
070700             07  WMS-LOAN-CLOSED-YR  PIC XX.
070800         05  WMS-DATE-LAST-LOAN-ACTIVITY.
070900             07  WMS-LOAN-DL-ACT-MO  PIC XX.
071000             07  WMS-LOAN-DL-ACT-DA  PIC XX.
071100             07  WMS-LOAN-DL-ACT-YR  PIC XX.
071200         05  WMS-DATE-LAST-ADVANCE.
071300             07  WMS-LAST-ADV-MO     PIC XX.
071400             07  WMS-LAST-ADV-DA     PIC XX.
071500             07  WMS-LAST-ADV-YR     PIC XX.
071600         05  WMS-DATE-LAST-LATE-CHG.
071700             07  WMS-LAST-LATE-MO    PIC XX.
071800             07  WMS-LAST-LATE-DA    PIC XX.
071900             07  WMS-LAST-LATE-YR    PIC XX.
072000         05  WMS-DATE-LAST-BAL-CHANGE.
072100             07  WMS-BAL-CHANGE-MO   PIC XX.
072200             07  WMS-BAL-CHANGE-DA   PIC XX.
072300             07  WMS-BAL-CHANGE-YR   PIC XX.
072400         05  WMS-DATE-LAST-BILLING.
072500             07  WMS-LAST-BILL-MO    PIC XX.
072600             07  WMS-LAST-BILL-DA    PIC XX.
072700             07  WMS-LAST-BILL-YR    PIC XX.
072800         05  FILLER                  PIC X(9).                    2016412
072810         05  WMS-LOAN-CHG-OFF-STATUS PIC X.                       2016412
072820         05  WMS-LOAN-CHG-OFF-DATE.                               2016412
072822             07  WMS-LOAN-CHG-OFF-MO PIC XX.                      2016412
072824             07  WMS-LOAN-CHG-OFF-DA PIC XX.                      2016412
072826             07  WMS-LOAN-CHG-OFF-YR PIC XX.                      2016412
072900         05  WMS-LOAN-BALANCE-AMOUNTS                COMP-3.
073000             07  WMS-AVAIL-BAL       PIC S9(13)V99.               9915845
073100             07  WMS-PRIN-BAL        PIC S9(13)V99.               9915845
073200             07  WMS-INTRST-BAL      PIC S9(13)V99.               9915845
073300             07  WMS-OTHER-CHGS      PIC S9(13)V99.               9915845
073400             07  WMS-OTHER-CHGS-BREAKDOWN.
073500                 09  WMS-INS1        PIC S9(13)V99.               9915845
073600                 09  WMS-INS2        PIC S9(13)V99.               9915845
073700                 09  WMS-FEES        PIC S9(13)V99.               9915845
073800         05  WMS-LOAN-AGGREGATE-DATA                 COMP-3.
073900             07  WMS-LOAN-AGGR-DAYS  PIC S9(5).
074000             07  WMS-CURR-AGGREGATES.
074100                 09  WMS-CURR-AGGR1  PIC S9(15)V99.               9915845
074200                 09  WMS-CURR-AGGR2  PIC S9(15)V99.               9915845
074300                 09  WMS-CURR-AGGR3  PIC S9(15)V99.               9915845
074400             07  WMS-PREV-AGGREGATES.
074500                 09  WMS-PREV-AGGR1  PIC S9(15)V99.               9915845
074600                 09  WMS-PREV-AGGR2  PIC S9(15)V99.               9915845
074700                 09  WMS-PREV-AGGR3  PIC S9(15)V99.               9915845
074800         05  WMS-LOAN-ACCR-INT       PIC S9(11)V9(6) COMP-3.      9915845
074900         05  WMS-LOAN-ACCR-TODAY     PIC S9(11)V9(6) COMP-3.      9915845
075000         05  WMS-LOAN-DLAY-INT       PIC S9(13)V99   COMP-3.      9915845
075100         05  WMS-PAYOFF-AMT          PIC S9(13)V99   COMP-3.      9915845
075200         05  WMS-BEGIN-INT           PIC S9(13)V99   COMP-3.      9915845
075300         05  WMS-AMT-LAST-ADVANCE    PIC S9(13)V99   COMP-3.      9915845
075400         05  WMS-LOAN-HIGH-BAL       PIC S9(13)V99   COMP-3.      9915845
075500         05  WMS-HIGH-BAL-THIS-CYCLE PIC S9(13)V99   COMP-3.      9915845
075600         05  WMS-DDA-DEPOSITS        PIC S9(13)V99   COMP-3.      9915845
075700         05  WMS-CALCED-NOT-BILLED.
075800             07  WMS-CALC-BILL-FLG   PIC X.
075900             07  WMS-CALC-INS1       PIC S9(13)V99   COMP-3.      9915845
076000             07  WMS-CALC-INS2       PIC S9(13)V99   COMP-3.      9915845
076100             07  WMS-CALC-FEES       PIC S9(13)V99   COMP-3.      9915845
076200             07  WMS-CALC-INTEREST   PIC S9(13)V99   COMP-3.      9915845
076300         05  WMS-LOAN-STMT-CLEAR     PIC X.
076400         05  WMS-LOAN-STATEMENT-INFO                 COMP-3.
076500             07  WMS-BEGIN-BAL       PIC S9(13)V99.               9915845
076600             07  WMS-LN-STMT-DB-NO   PIC S9(5).
076700             07  WMS-LN-STMT-DB-AMT  PIC S9(13)V99.               9915845
076800             07  WMS-LN-STMT-CR-NO   PIC S9(5).
076900             07  WMS-LN-STMT-CR-AMT  PIC S9(13)V99.               9915845
077000             07  WMS-THIS-CYCLE-AMTS.
077100                 09  WMS-FINCHG-THIS-CY  PIC S9(13)V99.           9915845
077200                 09  WMS-INSUR1-THIS-CY  PIC S9(13)V99.           9915845
077300                 09  WMS-INSUR2-THIS-CY  PIC S9(13)V99.           9915845
077400                 09  WMS-FEE-THIS-CY     PIC S9(13)V99.           9915845
077500             07  WMS-LN-STMT-AVG-BAL PIC S9(13)V99.               9915845
077600             07  WMS-LN-STMT-CY-DAYS PIC S999.
077700             07  WMS-LN-BILL-CY-DAYS PIC S999.
077800         05  WMS-LOAN-YTD-AGGR       PIC S9(15)V99   COMP-3.      9915845
077900         05  WMS-LOAN-YTD-AGGR-DAYS  PIC S9(3)       COMP-3.
078000         05  WMS-PRIN-LAST-ADVANCE   PIC S9(13)V99   COMP-3.      9915845
078100         05  WMS-INT-LAST-ADVANCE    PIC S9(13)V99   COMP-3.      9915845
078200         05  WMS-CHGS-LAST-ADVANCE   PIC S9(13)V99   COMP-3.      9915845
078300         05  WMS-PAYMENT-DATA.
078400             07  WMS-LAST-PMT-MADE                   COMP-3.
078500                 09  WMS-LAST-PMT-PRIN   PIC S9(13)V99.           9915845
078600                 09  WMS-LAST-PMT-INT    PIC S9(13)V99.           9915845
078700                 09  WMS-LAST-PMT-OTHER  PIC S9(13)V99.           9915845
078800             07  WMS-LAST-PMT-DATE.
078900                 09  WMS-LAST-PMT-MO PIC XX.
079000                 09  WMS-LAST-PMT-DA PIC XX.
079100                 09  WMS-LAST-PMT-YR PIC XX.
079110             07  FILLER                  PIC X(2).                9915845
079200             07  FILLER                  PIC X(21).               9915845
080000             07  WMS-PAYMENT-CALC.
080100                 09  WMS-PYMT-CALC   PIC X.
080200                 09  WMS-PYMT-PHASE  PIC S9          COMP-3.
080300                 09  WMS-PYMT-CALC-CODE  PIC X.
080400                 09  WMS-PYMT-CALC-DAY   PIC XX.
080500             07  WMS-PYMT-SCHEDULE.
080600                 09  WMS-PYMT-SCHED-CODE PIC X.
080700                 09  WMS-PYMT-SCHED-DAY  PIC XX.
080800             07  WMS-PYMT-CLEAR      PIC X.
080900             07  WMS-PYMT-DUE-AMT    PIC S9(13)V99   COMP-3.      9915845
081000             07  WMS-LAST-PYMT-AMT   PIC S9(13)V99   COMP-3.      9915845
081100             07  WMS-FIXED-PYMT      PIC X.
081200             07  WMS-FIXED-PYMT-AMT  PIC S9(13)V99   COMP-3.      9915845
081300             07  WMS-LOAN-PART-PMT   PIC X.
081400             07  WMS-LOAN-MIN-PART-PMT PIC S9(13)V99 COMP-3.      9915845
081500         05  WMS-LOAN-SPLIT-RATE-SCHEDULE            COMP-3.
081600             07  WMS-SPLIT-RATE1.
081700                 09  WMS-RATE1-ANNUAL    PIC S9V9(8).             9915845
081800                 09  WMS-RATE1-DAILY     PIC SVP9(15).            9915845
081900             07  WMS-SPLIT-RATE2.
082000                 09  WMS-RATE2-ANNUAL    PIC S9V9(8).             9915845
082100                 09  WMS-RATE2-DAILY     PIC SVP9(15).            9915845
082200             07  WMS-SPLIT-RATE3.
082300                 09  WMS-RATE3-ANNUAL    PIC S9V9(8).             9915845
082400                 09  WMS-RATE3-DAILY     PIC SVP9(15).            9915845
082500             07  WMS-SPLIT-LIMITS.
082600                 09  WMS-RATE1-LIMIT     PIC S9(13)V99.           9915845
082700                 09  WMS-RATE2-LIMIT     PIC S9(13)V99.           9915845
082800         05  WMS-LOAN-NEW-THIS-MONTH PIC X.
082900         05  WMS-LOAN-MONTH-TO-DATE                  COMP-3.
083000             07  WMS-LOAN-MTD-AGGR       PIC S9(15)V99.           9915845
083100             07  WMS-LOAN-MTD-AGGR-DAYS  PIC S999.
083200             07  WMS-EARNED-INT          PIC S9(13)V99.           9915845
083300             07  WMS-COLLECT-INT         PIC S9(13)V99.           9915845
083400             07  WMS-LOAN-MTD-INS1-ACCR  PIC S9(13)V99.           9915845
083500             07  WMS-LOAN-MTD-INS2-ACCR  PIC S9(13)V99.           9915845
083600             07  WMS-BILLED-INS1         PIC S9(13)V99.           9915845
083700             07  WMS-BILLED-INS2         PIC S9(13)V99.           9915845
083800             07  WMS-BILLED-FEES         PIC S9(13)V99.           9915845
083900             07  WMS-COLL-OTHER          PIC S9(13)V99.           9915845
084000             07  WMS-MTD-CURR-AGGREGATES.
084100                 09  WMS-MTD-AGGR1   PIC S9(15)V99.               9915845
084200                 09  WMS-MTD-AGGR2   PIC S9(15)V99.               9915845
084300                 09  WMS-MTD-AGGR3   PIC S9(15)V99.               9915845
084400             07  WMS-COSTS-AMORT-MTD PIC S9(13)V99.               9915845
084500         05  WMS-MAX-LOAN-INT        PIC S9V9(8)     COMP-3.      9915845
084600         05  WMS-ADDITIONAL-CREDIT   PIC S9(13)V99   COMP-3.      9915845
084700         05  WMS-ADDL-PYMT-CODE      PIC X.
084800         05  WMS-ADDL-PYMT-AMT       PIC S9(13)V99   COMP-3.      9915845
084900         05  WMS-ADDL-LOAN-TYPE      PIC XX.
085000         05  WMS-OLD-MONEY.
085100             07  WMS-PREV-BAL        PIC S9(13)V99   COMP-3.      9915845
085200             07  WMS-PREV-INT-PTR    PIC S999        COMP-3.
085300             07  WMS-PREV-SPLIT-RATE-PTR PIC S999    COMP-3.
085400             07  WMS-PREV-ANNUAL     PIC S9V9(8)     COMP-3.      9915845
085500             07  WMS-PREV-DAILY      PIC SVP9(15)   COMP-3.       9915845
085600         05  WMS-PAST-DUE-CTS.
085700             07  WMS-A-CT            PIC S999        COMP-3.
085800             07  WMS-B-CT            PIC S999        COMP-3.
085900             07  WMS-C-CT            PIC S999        COMP-3.
086000             07  WMS-D-CT            PIC S999        COMP-3.
086100             07  WMS-E-CT            PIC S999        COMP-3.
086200             07  WMS-F-CT            PIC S999        COMP-3.
086300         05  FILLER              REDEFINES WMS-PAST-DUE-CTS
086400                                                 OCCURS 6 TIMES.
086500             07  WMS-PD-CNT          PIC S999        COMP-3.
086600         05  WMS-PAST-DUE-AMT        PIC S9(13)V99   COMP-3.      9915845
086700         05  WMS-PAST-DUE-DAYS       PIC S999        COMP-3.
086800         05  WMS-BILLING-TRAILERS                OCCURS 7 TIMES.
086900             07  WMS-BILL-DUE-DATE.
087000                 09  WMS-BILL-DUE-CENT-YR.
087100                     11  WMS-BILL-DUE-CENT  PIC XX.
087200                     11  WMS-BILL-DUE-YR    PIC XX.
087300                 09  WMS-BILL-DUE-MO        PIC XX.
087400                 09  WMS-BILL-DUE-DA        PIC XX.
087500             07  WMS-BILL-DUE-AMT           PIC S9(13)V99  COMP-3.9915845
087600             07  WMS-BILL-DUE-STAT          PIC X.
087700         05  WMS-LOAN-PENALTY.
087800             07  WMS-PNLTY-CODE      PIC X.
087900             07  WMS-PNLTY-AMT       PIC S9(13)V99   COMP-3.      9915845
088000             07  WMS-PNLTY-DAYS      PIC S999        COMP-3.
088100         05  WMS-PAST-DUE-POST-PRI   PIC XX.
088200         05  WMS-DAYS-SINCE-PNLTY    PIC S999        COMP-3.
088300         05  WMS-PDUE-NTC-DAYS       PIC S999        COMP-3.
088400         05  WMS-PDUE-DAYS-AFTER-NTC PIC S999        COMP-3.
088500         05  WMS-EXTENSION-STATUS    PIC X.
088600         05  WMS-LAST-EXTENSION.
088700             07  WMS-LAST-EXT-DATE.
088800                 09  WMS-LAST-EXT-MO PIC XX.
088900                 09  WMS-LAST-EXT-DA PIC XX.
089000                 09  WMS-LAST-EXT-YR PIC XX.
089010             07  FILLER              PIC X(2).                    9915845
089100             07  WMS-REMAIN-COUNT    PIC S999        COMP-3.
089200         05  WMS-YTD-EXTENSIONS      PIC S999        COMP-3.
089300         05  WMS-YTD-EXT-MONTHS      PIC S999        COMP-3.
089400         05  WMS-PREV-YR-EXTENSIONS                  COMP-3.
089500             07  WMS-PREV-NO-EXT     PIC S999.
089600             07  WMS-PREV-MONTHS-EXT PIC S999.
089700         05  WMS-LOAN-PREV-YTD-INT   PIC S9(13)V99   COMP-3.      9915845
089800         05  WMS-LOAN-YTD-INT        PIC S9(13)V99   COMP-3.      9915845
089900         05  WMS-LOAN-YTD-INS1       PIC S9(13)V99   COMP-3.      9915845
090000         05  WMS-LOAN-YTD-INS2       PIC S9(13)V99   COMP-3.      9915845
090100         05  WMS-LOAN-YTD-FEES       PIC S9(13)V99   COMP-3.      9915845
090200         05  WMS-LOAN-PREV-YTD-INS1  PIC S9(13)V99   COMP-3.      9915845
090300         05  WMS-LOAN-PREV-YTD-INS2  PIC S9(13)V99   COMP-3.      9915845
090400         05  WMS-LOAN-PREV-YTD-FEES  PIC S9(13)V99   COMP-3.      9915845
090500         05  WMS-PREV-CYC-HISTORY.
090600           06  WMS-PREV-CYC-HIST                 OCCURS 12 TIMES.
090700             07  WMS-PREV-AVG-BAL    PIC S9(13)V99   COMP-3.      9915845
090800             07  WMS-PREV-PAST-DUE   PIC X.
090900             07  WMS-PYMT-LOAN       PIC X.
091000             07  WMS-OVER-FLAG       PIC X.
091100             07  WMS-SUSPECT-FLAG    PIC X.
091200         05  FILLER REDEFINES WMS-PREV-CYC-HISTORY.
091300             07  WMS-PREV-CYC1       PIC X(12).                   9915845
091400             07  WMS-PREV-CYC-2-12   PIC X(132).                  9915845
091500         05  WMS-PRIME-ADJ-CODE      PIC X.
091600         05  WMS-PRIME-ADJ           PIC S9V9(8)     COMP-3.      9915845
091700         05  WMS-PRIME-BAL-CALC      PIC X.
091800         05  WMS-PRIME-CHNG-RTE      PIC X.
091900         05  WMS-PRIME-RATE          PIC S9V9(8)     COMP-3.      9915845
092000         05  WMS-LOAN-INSURANCE.
092100             07  WMS-LOAN-INSUR1.
092200                 09  WMS-INSUR1          PIC X.
092300                 09  WMS-INSUR1-CO       PIC XXX.
092400                 09  WMS-INSUR1-RATE     PIC S9V9(8)     COMP-3.  9915845
092500                 09  WMS-INSUR1-DAILY    PIC SVP9(15)    COMP-3.  9915845
092600                 09  FILLER              PIC X(12).               9715504
093400             07  WMS-LOAN-INSUR2.
093500                 09  WMS-INSUR2          PIC X.
093600                 09  WMS-INSUR2-CO       PIC XXX.
093700                 09  WMS-INSUR2-RATE     PIC S9V9(8)     COMP-3.  9915845
093800                 09  WMS-INSUR2-DAILY    PIC SVP9(15)    COMP-3.  9915845
093900                 09  FILLER              PIC X(12).               9715504
094700         05  WMS-ORIG-COSTS-DATA.
094800             07  WMS-COSTS-CODE          PIC X.
094900             07  WMS-COSTS-DAY           PIC XX.
095000             07  WMS-COSTS-RECALC        PIC X.
095100             07  WMS-COSTS-ORIG-TERM     PIC S999    COMP-3.
095200             07  WMS-COSTS-REMN-TERM     PIC S999    COMP-3.
095300             07  WMS-COSTS-ORIG-DATE.
095400                 09  WMS-COSTS-ORIG-MO   PIC XX.
095500                 09  WMS-COSTS-ORIG-DA   PIC XX.
095600                 09  WMS-COSTS-ORIG-YR   PIC XX.
095700             07  WMS-COSTS-EXPR-DATE.
095800                 09  WMS-COSTS-EXPR-MO   PIC XX.
095900                 09  WMS-COSTS-EXPR-DA   PIC XX.
096000                 09  WMS-COSTS-EXPR-YR   PIC XX.
096010             07  FILLER                  PIC X(4).                9915845
096100             07  WMS-COSTS-AMOUNTS                   COMP-3.
096200                 09  WMS-COSTS-ORIG-AMT  PIC S9(13)V99.           9915845
096300                 09  WMS-COSTS-UNEARNED  PIC S9(13)V99.           9915845
096400                 09  WMS-COSTS-AMORTIZED PIC S9(13)V99.           9915845
096500         05  WMS-LOAN-TFR-INFO.
096600             07  WMS-LOAN-REG-CODE       PIC 9(3).
096700             07  WMS-LOAN-DEBT-RESTRUCT  PIC 9.
096800             07  WMS-LOAN-RISK-CODE      PIC 9.
096900             07  WMS-LOAN-CHG-OFF-AMT    PIC S9(13)V99  COMP-3.   9915845
097000             07  WMS-LOAN-ORIG-AVAIL     PIC S9(13)V99  COMP-3.   9915845
097100             07  WMS-LOAN-AMORT-YTD      PIC S9(13)V99  COMP-3.   9915845
097115         05  WMS-NB-ABM-AREA.                                     9715504
097120             07  WMS-NB-ABM-BRW-TARG     PIC S9(13)V99  COMP-3.   9915845
097125             07  WMS-NB-ABM-BRW-INC      PIC S9(13)V99  COMP-3.   9915845
097130             07  WMS-NB-ABM-REP-TARG     PIC S9(13)V99  COMP-3.   9915845
097135             07  WMS-NB-ABM-REP-INC      PIC S9(13)V99  COMP-3.   9915845
097150         05  WMS-LOAN-YTD-ACC-INT        PIC S9(13)V99  COMP-3.   9915845
097155         05  WMS-YTD-INT-CAP             PIC S9(13)V99  COMP-3.   9915845
097160         05  WMS-INT-CAP                 PIC S9(13)V99  COMP-3.   9915845
097162         05  WMS-FINCHG-YTD              PIC S9(13)V99  COMP-3.   1020057
097164         05  WMS-FINCHG-PYTD             PIC S9(13)V99  COMP-3.   1020057
097166         05  WMS-LOAN-CONSENT            PIC X.                   1020057
097168         05  WMS-PREV-PYMT-DATE          PIC X(8).                1020057
097170         05  WMS-CRED-CARD-IND           PIC X.                   1020057
097200         05  FILLER                      PIC X(77).               1020057
097300     03  WMS-MARKETING-TRAILER.
097400         05  WMS-MARKET-DELETE       PIC X.
097500         05  WMS-KEY-ACCT            PIC X.
097600         05  WMS-MKT-LARGE-ITEM      PIC X.
097700         05  WMS-MKT-BAL-CHANGE      PIC X.
097800         05  WMS-SIC-CODE            PIC X(5).
097900         05  WMS-MKT-ANALYSIS-CODE   PIC XX.
098000         05  FILLER                  PIC X(2).
098100         05  WMS-CALL-SCHEDULE.
098200             07  WMS-CALL-CODE       PIC X.
098300             07  WMS-MAX-PERIOD      PIC S999        COMP-3.
098400             07  WMS-SCHED-NO        PIC S999        COMP-3.
098500             07  WMS-DATE-LAST-CALL.
098600                 09  WMS-LAST-CALL-MO        PIC XX.
098700                 09  WMS-LAST-CALL-DA        PIC XX.
098800                 09  WMS-LAST-CALL-YR        PIC XX.
098810             07  FILLER                      PIC X(2).            9915845
098900             07  WMS-MRKT-DATE-NEXT-CALL.
099000                 09  WMS-MRKT-NEXT-CALL-MO   PIC XX.
099100                 09  WMS-MRKT-NEXT-CALL-DA   PIC XX.
099200                 09  WMS-MRKT-NEXT-CALL-YR   PIC XX.
099210             07  FILLER                      PIC X(2).            9915845
099300             07  WMS-MKT-CONTACT     PIC X.
099400             07  WMS-CALL-TYPE       PIC X.
099500             07  WMS-CURR-CALLS      PIC S999        COMP-3.
099600         05  FILLER                  PIC X(4).                    9915845
099700         05  WMS-REPORT-FLUCTUATION                  COMP-3.
099800             07  WMS-PCT-DOWN        PIC S9V99.
099900             07  WMS-AMT-DOWN        PIC S9(13)V99.               9915845
100000             07  WMS-PCT-UP          PIC S9V99.
100100             07  WMS-AMT-UP          PIC S9(13)V99.               9915845
100200             07  WMS-MKT-LOW-LIMIT   PIC S9(13)V99.               9915845
100300             07  WMS-MKT-HIGH-LIMIT  PIC S9(13)V99.               9915845
100400             07  WMS-MRKT-LARGE-ITEM PIC S9(13)V99.               9915845
100500             07  WMS-MRKT-BAL-COUNT  PIC S999.
100600             07  WMS-MRKT-ITEM-COUNT PIC S9(11).
100700         05  FILLER                  PIC X(8).                    9715504
100800         05  WMS-MRKT-OFFICER        PIC X(5).                    9715504
100810         05  FILLER                  PIC X.                       9915845
100900         05  WMS-MRKT-BRANCH         PIC X(5).                    0427044
100910         05  FILLER                  PIC X.                       0427044
101000         05  WMS-MRKT-GEOG-LOC       PIC XXX.
101100         05  WMS-NATL-STATE-ACCT     PIC X.
101200         05  WMS-PROF-CODE           PIC X.
101300         05  WMS-HISTORY-OPT-BAL     PIC X.
101400         05  WMS-HISTORY-BAL-CALC    PIC X.
101500         05  WMS-HST-AVG-BALANCES                    COMP-3.
101600             07  WMS-HST-13-MO-HIST              OCCURS 13 TIMES.
101700                 09  WMS-HST-AGGR-DAYS   PIC S999.
101800                 09  WMS-HST-AVG-BAL     PIC S9(13)V99.           9915845
101900         05  FILLER REDEFINES WMS-HST-AVG-BALANCES   COMP-3.
102000             07  FILLER              PIC S999.
102100             07  FILLER              PIC S9(13)V99.               9915845
102200             07  WMS-HST-PREV-MO-HIST.
102300                 09  FILLER                      OCCURS 12 TIMES.
102400                     11  FILLER      PIC S999.
102500                     11  FILLER      PIC S9(13)V99.               9915845
102600         05  WMS-HST-AGGR-BAL        PIC S9(15)V99   COMP-3.      9915845
102610         05  WMS-NAICS-CODE          PIC X(6).                    0266822
102700         05  FILLER                  PIC X(94).                   0266822
102800*
102900     03  WMS-TRANSFER-AFFILIATE-TRAILER          OCCURS 9 TIMES.
103000         05  WMS-TRNSFR-AFFL-FLAG    PIC X.
103100         05  WMS-LINK-SYS-ID         PIC XX.
103200         05  WMS-LINKAGE-CNTL.
103300             07  WMS-LINK-DELETE     PIC X.
103400             07  FILLER              PIC X(34).
103500         05  WMS-LINKAGE-SCHEDULE.
103600             07  WMS-LINK-SCHD-CODE  PIC X.
103700             07  WMS-LINK-SCHD-DAY1  PIC XX.
103800             07  WMS-LINK-SCHD-DAY2  PIC XX.
103900             07  WMS-LINK-SCHD-MON   PIC XX.
104000             07  WMS-WEEK-CNTR       PIC S9          COMP-3.
104100         05  WMS-NB-ODP-PRD-CD       PIC X(3).                    9715504
104200         05  WMS-TRANSFER-INFO.
104300             07  WMS-EFFECTIVE-DATE.
104400                 09  WMS-EFFECTIVE-MO    PIC XX.
104500                 09  WMS-EFFECTIVE-DA    PIC XX.
104600                 09  WMS-EFFECTIVE-CENT  PIC XX.
104700                 09  WMS-EFFECTIVE-YR    PIC XX.
104800             07  WMS-TRANSFER-AMT    PIC S9(13)V99   COMP-3.      9915845
104900             07  WMS-EXPIRATION-DATE.
105000                 09  WMS-EXPIRATION-MO   PIC XX.
105100                 09  WMS-EXPIRATION-DA   PIC XX.
105200                 09  WMS-EXPIRATION-CENT PIC XX.
105300                 09  WMS-EXPIRATION-YR   PIC XX.
105400             07  WMS-ORIGINAL-NO     PIC S999        COMP-3.
105500             07  WMS-REMAINING-NO    PIC S999        COMP-3.
105600             07  FILLER              PIC X(8).                    9915845
105700*
105800     03  WMS-OD-NSF-TRAILER.
105900         05  WMS-OD-NSF-HISTORY                      COMP-3.
106000             07  WMS-DAYS-OD-MO      PIC S999    OCCURS 12 TIMES.
106100             07  WMS-TIMES-OD-MO     PIC S999    OCCURS 12 TIMES.
106200             07  WMS-TIMES-NSF-MO    PIC S999    OCCURS 12 TIMES.
106300             07  WMS-TIMES-UNAVAIL-MO PIC S999   OCCURS 12 TIMES.
106400         05  FILLER REDEFINES WMS-OD-NSF-HISTORY.
106500             07  WMS-OD-DAYS-MO                      COMP-3.
106600                 09  FILLER          PIC S999.
106700                 09  WMS-OD-DAYS-PREV-MO.
106800                     11  FILLER      PIC S999    OCCURS 11 TIMES.
106900             07  WMS-OD-TIMES-MO                     COMP-3.
107000                 09  FILLER          PIC S999.
107100                 09  WMS-OD-TIMES-PREV-MO.
107200                     11  FILLER      PIC S999    OCCURS 11 TIMES.
107300             07  WMS-NSF-MONTHS                      COMP-3.
107400                 09  FILLER          PIC S999.
107500                 09  WMS-NSF-PREV-MO.
107600                     11  FILLER      PIC S999    OCCURS 11 TIMES.
107700             07  WMS-UNAVAIL-MONTHS                  COMP-3.
107800                 09  FILLER          PIC S999.
107900                 09  WMS-UNAVAIL-PREV-MO.
108000                     11  FILLER      PIC S999    OCCURS 11 TIMES.
108010         05  WMS-NB-CB-MO-TOTALS                     COMP-3.      9715504
108020             07  WMS-NB-TIMES-CB-MO  PIC S999                     9715504
108030                                     OCCURS 12 TIMES.             9715504
108040         05  FILLER REDEFINES WMS-NB-CB-MO-TOTALS.                9715504
108050             07  FILLER                              COMP-3.      9715504
108060                 09  FILLER          PIC S999.                    9715504
108070                 09  WMS-NB-CB-PREV-MO.                           9715504
108080                     11  FILLER      PIC S999                     9715504
108090                                     OCCURS 11 TIMES.             9715504
108100         05  FILLER                  PIC X(10).                   9915845
108200*
108300     03  WMS-SAVINGS-TRAILER.
108400         05  WMS-INT-STATUS          PIC X.
108500         05  WMS-INT-CUR-BAL         PIC S9(13)V99   COMP-3.
108600         05  WMS-INT-USER-CODE       PIC X.
108700         05  WMS-INTRST-CALC         PIC S9          COMP-3.
108800         05  WMS-INT-RATE            PIC S999        COMP-3.
108810         05  WMS-INT-TAX-STATE       PIC S9(13)V99   COMP-3.      2012254
108820         05  WMS-SAV-TIER-PTR        PIC S999        COMP-3.      0316967
108900         05  WMS-INT-INCREMNT        PIC S9          COMP-3.
109000         05  WMS-SAV-DIST-CD         PIC X.
109100         05  WMS-INT-DORM            PIC X.
109200         05  WMS-INT-DORM-DAYS       PIC S999        COMP-3.
109300         05  WMS-SAV-XFER-INT-PTR    PIC S9          COMP-3.
109400         05  WMS-MIN-AMT-FOR-SUPER   PIC S9(13)V99   COMP-3.      9915845
109500         05  WMS-MAX-TRAN-FOR-SUPER  PIC S999        COMP-3.
109600         05  WMS-INT-DATE-LAST-CUST-ACT.
109700             07  WMS-INT-LST-CUST-MO PIC XX.
109800             07  WMS-INT-LST-CUST-DA PIC XX.
109900             07  WMS-INT-LST-CUST-YR PIC XX.
109910         05  FILLER                  PIC X(2).                    9915845
110000         05  WMS-INT-DATE-DORMANT.
110100             07  WMS-INT-DORM-MO     PIC XX.
110200             07  WMS-INT-DORM-DA     PIC XX.
110300             07  WMS-INT-DORM-YR     PIC XX.
110310         05  FILLER                  PIC X(2).                    9915845
110400         05  WMS-SAV-BAL-HIST        PIC X.
110500         05  WMS-SAV-BAL-HIST-RET    PIC S9(3)       COMP-3.
110700         05  WMS-CMA-DATA.
110800             07  WMS-CMA-INCR-PTR    PIC S9          COMP-3.
110900             07  WMS-CMA-BALANCE-USE PIC X.
111000             07  WMS-CMA-INDICATOR   PIC X.
111100             07  WMS-CMA-FREQ-CODE   PIC X.
111200             07  WMS-CMA-FREQ-INCR   PIC S999        COMP-3.
111300             07  WMS-CMA-BAL-CALC    PIC X.
111400             07  WMS-CMA-NEXT-SWEEP-DATE.
111500                 09  WMS-CMA-NXT-SWP-MO    PIC XX.
111600                 09  WMS-CMA-NXT-SWP-DA    PIC XX.
111700                 09  WMS-CMA-NXT-SWP-YR    PIC XX.
111710             07  WMS-CMA-SWEEP-IND         PIC X.                 9916072
111720             07  WMS-CMA-FEE-WAIVE-IND     PIC X.                 9916072
111800             07  WMS-CMA-CTD-MGMT-FEES     PIC S9(13)V99  COMP-3. 9915845
111900             07  WMS-CMA-YTD-MGMT-FEES     PIC S9(13)V99  COMP-3. 9915845
112000             07  WMS-CMA-PREV-YTD-MN-FEES  PIC S9(13)V99  COMP-3. 9915845
112100         05  WMS-INT-AGGREGATE-DATA                  COMP-3.
112200             07  WMS-INT-AGGR-DAYS   PIC S999.
112300             07  WMS-INT-AGGR-BAL    PIC S9(15)V99.               9915845
112400             07  WMS-ACCR-INT        PIC S9(11)V9(6).             9915845
112500         05  WMS-ACCRUAL-AMT         PIC S9(11)V9(6) COMP-3.      9915845
112600         05  WMS-INT-ACCRUAL-BAL     PIC S9(13)V99   COMP-3.
112700         05  WMS-INT-ACCRD-TODAY     PIC S9(11)V9(6) COMP-3.      9915845
112800         05  WMS-PROJ-ACCR           PIC S9(13)V99   COMP-3.      9915845
112900         05  WMS-PREV-INT-BAL        PIC S9(13)V99   COMP-3.
113000         05  WMS-INT-STMT-CLEAR      PIC X.
113100         05  WMS-INT-STATEMENT-DATA                  COMP-3.
113200             07  WMS-INT-BEG-BAL     PIC S9(13)V99.
113300             07  WMS-INT-DR-NUMBER   PIC S9(7).
113400             07  WMS-INT-DR-AMT      PIC S9(13)V99.
113500             07  WMS-INT-CR-NUMBER   PIC S9(7).
113600             07  WMS-INT-CR-AMT      PIC S9(13)V99.
113700             07  WMS-INT-ACCUM-INT   PIC S9(13)V99.               9915845
113800             07  WMS-INT-CALC-INT    PIC S9(13)V99.               9915845
113900             07  WMS-INT-EARN-INT    PIC S9(13)V99.               9915845
114000             07  WMS-INT-TAX         PIC S9(13)V99.               9915845
114100             07  WMS-INT-AVG-BAL     PIC S9(13)V99.               9915845
114200             07  WMS-INT-MIN-BALANCE PIC S9(13)V99.               9915845
114300             07  WMS-INT-TR-CHG      PIC S9(13)V99.               9915845
114400             07  WMS-INT-TRAN-NO     PIC S9(5).
114500             07  WMS-INT-TRAN-AMT    PIC S9(13)V99.
114600         05  WMS-SAV-DIST-DR-TC      PIC X(4).
114700         05  WMS-SAV-DIST-CR-TC      PIC X(4).
114800         05  WMS-INT-INV-IND         PIC X.
114900         05  WMS-INT-INV-TYPE        PIC X(3).
115000         05  WMS-INT-INV-SVC-RT      PIC S9V9(8)     COMP-3.      9915845
115100         05  WMS-INT-INV-CONFIRM     PIC X.
115300         05  WMS-INT-NEW-THIS-MONTH  PIC X.
115400         05  WMS-INT-MTD-DATA                        COMP-3.
115500             07  WMS-MTD-INT-PAID    PIC S9(13)V99.               9915845
115600             07  WMS-MTD-CALC-INT    PIC S9(13)V99.               9915845
115700             07  WMS-MTD-ACCR-INT    PIC S9(13)V99.               9915845
115800             07  WMS-INT-MTD-TAX     PIC S9(13)V99.               9915845
115810             07  WMS-INT-MTD-STATE-TAX   PIC S9(13)V99.           9915845
115820             07  WMS-INT-MTD-LOCAL-TAX   PIC S9(13)V99.           9915845
115900             07  WMS-INT-MTD-AGGR    PIC S9(15)V99.               9915845
116000             07  WMS-INT-MTD-MIN     PIC S9(13)V99.               9915845
116100             07  WMS-INT-MTD-TRCHG   PIC S9(13)V99.               9915845
116200             07  WMS-INT-MTD-TRANNO  PIC S9(5).
116300             07  WMS-INT-MTD-TRNAMT  PIC S9(13)V99.               9915845
116400             07  WMS-INT-MTD-ACCR-DAYS   PIC S999    COMP-3.
116500         05  WMS-INT-LEAP-YEAR-FLG   PIC X.                       CIB5519
116600         05  WMS-INT-TIS-AGGR-BAL    PIC S9(15)V99   COMP-3.
116700         05  WMS-INT-TIS-AGGR-DAYS   PIC S999        COMP-3.
116800         05  WMS-SAV-PEND-TAX        PIC S9(13)V99   COMP-3.      9915845
116900         05  WMS-INT-YTD-AGGR-DAYS   PIC S999        COMP-3.
117000         05  WMS-INT-YTD-AGGR-BALS   PIC S9(15)V99   COMP-3.      9915845
117100         05  WMS-YTD-INT-PAID        PIC S9(13)V99   COMP-3.      9915845
117200         05  WMS-YTD-SAV-TAX         PIC S9(13)V99   COMP-3.      9915845
117210         05  WMS-YTD-SAV-STATE-TAX   PIC S9(13)V99   COMP-3.      9915845
117220         05  WMS-YTD-SAV-LOCAL-TAX   PIC S9(13)V99   COMP-3.      9915845
117300         05  WMS-PREV-YTD-AVG-BAL    PIC S9(13)V99   COMP-3.      9915845
117400         05  WMS-PREV-YTD-INT        PIC S9(13)V99   COMP-3.      9915845
117500         05  WMS-PREV-YTD-SAV-TAX    PIC S9(13)V99   COMP-3.      9915845
117510         05  WMS-PREV-YTD-SAV-STATE-TAX  PIC S9(13)V99   COMP-3.  9915845
117520         05  WMS-PREV-YTD-SAV-LOCAL-TAX  PIC S9(13)V99   COMP-3.  9915845
117600         05  WMS-INT-TAX-LOCAL       PIC S9(13)V99   COMP-3.      2012254
117700         05  WMS-DDA-MAX-FLAG        PIC X.
117800         05  WMS-DDA-MAX-DATA                        COMP-3.
117900             07  WMS-DDA-MAX-BAL         PIC S9(13)V99.           9915845
118000             07  WMS-DDA-MAX-CYCLE-NO    PIC S9(5).
118100             07  WMS-DDA-MAX-CYCLE-AMT   PIC S9(13)V99.           9915845
118200             07  WMS-DDA-MAX-MTD-NO      PIC S9(5).
118300             07  WMS-DDA-MAX-MTD-AMT     PIC S9(13)V99.           9915845
118400         05  WMS-INT-TRUTH-IN-SAVINGS    COMP-3.
118500             07  WMS-INT-TIS-CYC-FEES    PIC S9(13)V99.           9915845
118600             07  WMS-INT-TIS-EARN-INT    PIC S9(11)V9(6).         9915845
118700             07  WMS-INT-TIS-AVG-BAL     PIC S9(13)V99.           9915845
118800             07  WMS-INT-TIS-REPT-INT    PIC S9(13)V99.           9915845
118805             07  WMS-INT-TIS-AVG-DAYS    PIC S9(3).               9714895
118810         05  WMS-INT-DATE-OPENED.                                 CIB5519
118820             07  WMS-INT-OPENED-MO       PIC XX.                  CIB5519
118830             07  WMS-INT-OPENED-DA       PIC XX.                  CIB5519
118840             07  WMS-INT-OPENED-YR       PIC XX.                  CIB5519
118850         05  FILLER                      PIC X(2).                9915845
118860         05  WMS-INT-TIS-CYC-BEGIN-ACCR  PIC S9(13)V99 COMP-3.    9915845
118870         05  WMS-YTD-ACCR-INT            PIC S9(13)V99 COMP-3.    9915845
118872         05  WMS-SAV-INT-CYCLE.                                   9915845
118874             07  WMS-SAV-INT-PAY-CYCLE   PIC X.                   9915845
118876             07  WMS-SAV-INT-INCR        PIC X.                   9915845
118878             07  WMS-SAV-INT-MONTH1      PIC X(02).               9915845
118880             07  WMS-SAV-INT-DAY         PIC X(02).               9915845
118882             07  WMS-SAV-INT-DAY2        PIC X(02).               9915845
118884             07  WMS-SAV-INT-CLEAR       PIC X.                   9915845
118886             07  WMS-SAV-INT-DATE-LAST-PMT.                       9915845
118888                 09  WMS-SAV-INT-DL-PMT-MO   PIC X(02).           9915845
118890                 09  WMS-SAV-INT-DL-PMT-DA   PIC X(02).           9915845
118892                 09  WMS-SAV-INT-DL-PMT-YR   PIC X(02).           9915845
118894             07  FILLER                      PIC X(02).           9915845
118896         05  WMS-YTD-INT-PAID-1042       PIC S9(13)V99 COMP-3.    9915845
118898         05  WMS-PREV-YTD-INT-1042       PIC S9(13)V99 COMP-3.    9915845
118900         05  WMS-CMA-FEE-MIN-AMT         PIC S9(13)V99 COMP-3.    9916072
118902         05  WMS-CMA-CTD-MGMT-FEES-WV    PIC S9(13)V99 COMP-3.    9916072
118904         05  WMS-CMA-YTD-MGMT-FEES-WV    PIC S9(13)V99 COMP-3.    9916072
118906         05  WMS-DDA-MAX-BAL-BEG         PIC S9(13)V99 COMP-3.    9916072
118908         05  WMS-SAV-RATE-USE            PIC X.                   0316967
118910         05  WMS-INT-SC-AGGR-BAL         PIC S9(15)V99 COMP-3.    9916070
118912         05  WMS-INT-SC-AGGR-COLL        PIC S9(15)V99 COMP-3.    9916070
118914         05  WMS-INT-SC-AGGR-DAYS        PIC S999      COMP-3.    9916070
118916         05  WMS-INT-SC-MIN-BAL          PIC S9(13)V99 COMP-3.    9916070
118918         05  WMS-INT-COLL-CALC           PIC X.                   9916070
119000*
119100     03  WMS-BANK-AVAIL-TRAILER                      COMP-3.
119200         05  WMS-BANK-AVAIL-TR-AMT PIC S9(13)V99 OCCURS 7 TIMES.  9915845
119300     03  FILLER REDEFINES WMS-BANK-AVAIL-TRAILER     COMP-3.
119400         05  FILLER                PIC S9(13)V99.                 9915845
119500         05  WMS-BANK-AVAIL-2-7.
119600             07  FILLER            PIC S9(13)V99 OCCURS 6 TIMES.  9915845
119800*
119900     03  WMS-CUST-AVAIL-TRAILER                     COMP-3.
120000         05  WMS-CUST-AVAIL-FUNDS  PIC S9(13)V99 OCCURS 12 TIMES. 9915845
120100     03  FILLER REDEFINES WMS-CUST-AVAIL-TRAILER    COMP-3.
120200         05  FILLER                PIC S9(13)V99.                 9915845
120300         05  WMS-CUST-AVAIL-2-12.
120400             07  FILLER            PIC S9(13)V99 OCCURS 11 TIMES. 9915845
120600*
120700     03  WMS-COMBINED-STATEMENT-TRAILER          OCCURS 15 TIMES.
120800         05  WMS-COMB-STMT-SYS-ID    PIC XX.
120900         05  WMS-COMB-STMT-CONTROL.
121000             07  WMS-COMB-STMT-DELETE    PIC X.
121100             07  FILLER                  PIC X(34).
121200         05  FILLER                  PIC X(7).
121300         05  WMS-COMB-CUST-NO        PIC X.
121400         05  WMS-COMB-STMT-TRAN-FLAG PIC X.
121500*
121600     03  WMS-TARGET-AMOUNT-TRAILER.
121700         05  WMS-TARGET-DELETE       PIC X.
121800         05  WMS-TARGET-CONTROL      PIC X(22).
121900         05  WMS-TARGET-FL-TO-PRNT   PIC X.
122000         05  WMS-NB-ZBA-OPTIONS.                                  9715504
122010             07  WMS-NB-ZB-FRCD-REF-OPT  PIC X(01).               9715504
122020             07  WMS-NB-ZB-SUMM-OPT      PIC X(01).               9715504
122030             07  WMS-NB-ZB-TRN-NTFY-OPT  PIC X(01).               9715504
122040             07  WMS-NB-ZB-ZBA-TBA-OPT   PIC X(01).               9715504
122050             07  WMS-NB-ZB-PAR-CHILD     PIC X(01).               9715504
122060             07  WMS-NB-ZB-LVL-NBR       PIC X(01).               9715504
122100         05  WMS-TARGET-AMOUNT       PIC S9(13)V99   COMP-3.      9915845
122200         05  WMS-TARGET-AMT-FLAG     PIC X.
122210         05  WMS-NB-ZBA-AREA.                                     9715504
122220             07  WMS-NB-ZB-FRCD-REF-NO   PIC X(10).               9715504
122230             07  WMS-NB-ZB-REL-NBR       PIC X(06).               9715504
122240         05  WMS-NB-ULTIMATE-PARENT.                              9715504
122250             07  WMS-NB-ZB-ULTMT-PAR PIC X(22).                   9715504
122260         05  WMS-TARGET-XFER-MINIMUM     PIC S9(13)V99 COMP-3.    9915845
122270         05  WMS-TARGET-XFER-INCREMENT   PIC S9(13)V99 COMP-3.    9915845
122280         05  FILLER                      PIC X(32).               9915845
122400*
122500     03  WMS-LIMIT-TRANSFER-TRAILER.
122600         05  WMS-LIMIT-TRAN-DELETE   PIC X.
122700         05  WMS-LIM-TRAN-CODE       PIC XX.
122800         05  WMS-LIM-TRAN-CONTROL    PIC X(35).
122900         05  WMS-LIM-TRAN-AMT        PIC S9(13)V99   COMP-3.      9915845
122910         05  WMS-LIM-TRAN-MIN        PIC S9(13)V99   COMP-3.      9915845
122920         05  WMS-LIM-TRAN-INCR       PIC S9(13)V99   COMP-3.      9915845
122930         05  WMS-LIM-TRAN-CONSEC-DAY PIC S999        COMP-3.      9915845
123000         05  FILLER                  PIC X(11).                   9915845
123100*
123200     03  WMS-DECEDENT-TAX-DATA-TRAILER.
123300         05  WMS-DCD-NA-PTR          PIC S9          COMP-3.
123400         05  WMS-DCD-STATUS          PIC X.
123500         05  WMS-DCD-TAX-ID.
123600             07  WMS-DCD-TAX-CODE    PIC X.
123700             07  WMS-DCD-TAX-PRE     PIC X.
123800             07  WMS-DCD-TAX-NO      PIC X(9).
123900             07  WMS-DCD-TAX-SUF     PIC X.
123910         05  FILLER                  PIC X(4).                    9915845
124000         05  WMS-DCD-DATE            PIC X(6).
124010         05  FILLER                  PIC X(2).                    9915845
124100         05  WMS-DCD-YTD-INT-IOD     PIC S9(13)V99   COMP-3.      9915845
124200         05  WMS-DCD-YTD-INT-SAV     PIC S9(13)V99   COMP-3.      9915845
124300         05  WMS-DCD-YTD-INT-LOAN    PIC S9(13)V99   COMP-3.      9915845
124400         05  WMS-DCD-YTD-TAX-IOD     PIC S9(13)V99   COMP-3.      9915845
124410         05  WMS-DCD-YTD-TAX-IOD-STATE   PIC S9(13)V99   COMP-3.  9915845
124420         05  WMS-DCD-YTD-TAX-IOD-LOCAL   PIC S9(13)V99   COMP-3.  9915845
124500         05  WMS-DCD-YTD-TAX-SAV     PIC S9(13)V99   COMP-3.      9915845
124510         05  WMS-DCD-YTD-TAX-SAV-STATE   PIC S9(13)V99   COMP-3.  9915845
124520         05  WMS-DCD-YTD-TAX-SAV-LOCAL   PIC S9(13)V99   COMP-3.  9915845
124600         05  WMS-DCD-USER            PIC X.
124610         05  WMS-DCD-NRA-CERT-NAME   PIC X.                       9915845
124620         05  WMS-DCD-YTD-INT-IOD-1042    PIC S9(13)V99   COMP-3.  9915845
124630         05  WMS-DCD-YTD-INT-SAV-1042    PIC S9(13)V99   COMP-3.  9915845
124700         05  FILLER                  PIC X(04).                   9915845
124800*
124900     03  WMS-OD-ACCRUAL-TRAILER.
125000         05  WMS-OD-CALC-FLAG        PIC X.
125200         05  WMS-OD-INT-WAIVE        PIC X.
125210         05  WMS-OD-INT-RATE-USE     PIC X.                       9915845
125300         05  WMS-OD-ACCRL-AMT        PIC S9(11)V9(6)  COMP-3.     9915845
125400         05  WMS-OD-ACCRD-TDY        PIC S9(11)V9(6)  COMP-3.     9915845
125500         05  WMS-OD-CYC-ACR          PIC S9(11)V9(6)  COMP-3.     9915845
125600         05  WMS-OD-CYC-CHGD         PIC S9(13)V99    COMP-3.     9915845
125700         05  WMS-OD-MTD-ACR          PIC S9(13)V99    COMP-3.     9915845
125800         05  WMS-OD-MTD-CHGD         PIC S9(13)V99    COMP-3.     9915845
125900         05  WMS-OD-YTD-CHGD         PIC S9(13)V99    COMP-3.     9915845
126000         05  WMS-OD-PREV-YTD-CHGD    PIC S9(13)V99    COMP-3.     9915845
126100         05  WMS-OD-ACCRL-BAL        PIC S9(13)V99    COMP-3.     9915845
126110         05  WMS-OD-MIN-RATE         PIC S9V9(8)      COMP-3.     9915845
126120         05  WMS-OD-MAX-RATE         PIC S9V9(8)      COMP-3.     9915845
126200         05  WMS-OD-INT-PTR-PRIME-ADJ.                            9915845
126210             07  WMS-OD-INT-PTR1     PIC S999         COMP-3.     9915845
126220             07  WMS-OD-PRIME-ADJ1   PIC S9V9(8)      COMP-3.     9915845
126230             07  WMS-OD-INT-PTR2     PIC S999         COMP-3.     9915845
126240             07  WMS-OD-PRIME-ADJ2   PIC S9V9(8)      COMP-3.     9915845
126250             07  WMS-OD-INT-PTR3     PIC S999         COMP-3.     9915845
126260             07  WMS-OD-PRIME-ADJ3   PIC S9V9(8)      COMP-3.     9915845
126270             07  WMS-OD-INT-PTR4     PIC S999         COMP-3.     9915845
126280             07  WMS-OD-PRIME-ADJ4   PIC S9V9(8)      COMP-3.     9915845
126290             07  WMS-OD-INT-PTR5     PIC S999         COMP-3.     9915845
126300             07  WMS-OD-PRIME-ADJ5   PIC S9V9(8)      COMP-3.     9915845
126310             07  WMS-OD-INT-PTR6     PIC S999         COMP-3.     9915845
126320             07  WMS-OD-PRIME-ADJ6   PIC S9V9(8)      COMP-3.     9915845
126330             07  WMS-OD-INT-PTR7     PIC S999         COMP-3.     9915845
126340             07  WMS-OD-PRIME-ADJ7   PIC S9V9(8)      COMP-3.     9915845
126350             07  WMS-OD-INT-PTR8     PIC S999         COMP-3.     9915845
126360             07  WMS-OD-PRIME-ADJ8   PIC S9V9(8)      COMP-3.     9915845
126370             07  WMS-OD-INT-PTR9     PIC S999         COMP-3.     9915845
126380             07  WMS-OD-PRIME-ADJ9   PIC S9V9(8)      COMP-3.     9915845
126390         05  FILLER REDEFINES WMS-OD-INT-PTR-PRIME-ADJ            9915845
126400                                     OCCURS 9 TIMES.              9915845
126410             07  WMS-OD-INT-PTR      PIC S999         COMP-3.     9915845
126420             07  WMS-OD-PRIME-ADJ    PIC S9V9(8)      COMP-3.     9915845
126500         05  WMS-OD-LIMIT-DATE-EIGHT.                             9915845
126510             07  WMS-OD-LIMIT1           PIC X.                   9915845
126520             07  WMS-OD-LIMIT1-9 REDEFINES WMS-OD-LIMIT1          9915845
126530                                         PIC 9.                   9915845
126540             07  WMS-OD-LIMIT-AMT1       PIC S9(13)V99 COMP-3.    9915845
126550             07  WMS-DATE-INTO-OD2.                               9915845
126560                 09  WMS-INTO-OD-MO2     PIC XX.                  9915845
126570                 09  WMS-INTO-OD-DA2     PIC XX.                  9915845
126580                 09  WMS-INTO-OD-YR2.                             9915845
126590                     11  WMS-INTO-OD-YR1-2   PIC X.               9915845
126600                     11  WMS-INTO-OD-YR2-2   PIC X.               9915845
126610             07  WMS-OD-LIMIT2           PIC X.                   9915845
126620             07  WMS-OD-LIMIT2-9  REDEFINES  WMS-OD-LIMIT2        9915845
126630                                         PIC 9.                   9915845
126640             07  WMS-OD-LIMIT-AMT2       PIC S9(13)V99 COMP-3.    9915845
126650             07  WMS-DATE-INTO-OD3.                               9915845
126660                 09  WMS-INTO-OD-MO3     PIC XX.                  9915845
126670                 09  WMS-INTO-OD-DA3     PIC XX.                  9915845
126680                 09  WMS-INTO-OD-YR3.                             9915845
126690                     11  WMS-INTO-OD-YR1-3   PIC X.               9915845
126700                     11  WMS-INTO-OD-YR2-3   PIC X.               9915845
126710             07  WMS-OD-LIMIT3           PIC X.                   9915845
126720             07  WMS-OD-LIMIT3-9  REDEFINES  WMS-OD-LIMIT3        9915845
126730                                         PIC 9.                   9915845
126740             07  WMS-OD-LIMIT-AMT3       PIC S9(13)V99 COMP-3.    9915845
126750             07  WMS-DATE-INTO-OD4.                               9915845
126760                 09  WMS-INTO-OD-MO4     PIC XX.                  9915845
126770                 09  WMS-INTO-OD-DA4     PIC XX.                  9915845
126780                 09  WMS-INTO-OD-YR4.                             9915845
126790                     11  WMS-INTO-OD-YR1-4   PIC X.               9915845
126800                     11  WMS-INTO-OD-YR2-4   PIC X.               9915845
126810             07  WMS-OD-LIMIT4           PIC X.                   9915845
126820             07  WMS-OD-LIMIT4-9  REDEFINES  WMS-OD-LIMIT4        9915845
126830                                         PIC 9.                   9915845
126840             07  WMS-OD-LIMIT-AMT4       PIC S9(13)V99 COMP-3.    9915845
126850             07  WMS-DATE-INTO-OD5.                               9915845
126860                 09  WMS-INTO-OD-MO5     PIC XX.                  9915845
126870                 09  WMS-INTO-OD-DA5     PIC XX.                  9915845
126880                 09  WMS-INTO-OD-YR5.                             9915845
126890                     11  WMS-INTO-OD-YR1-5   PIC X.               9915845
126900                     11  WMS-INTO-OD-YR2-5   PIC X.               9915845
126910             07  WMS-OD-LIMIT5           PIC X.                   9915845
126920             07  WMS-OD-LIMIT5-9  REDEFINES  WMS-OD-LIMIT5        9915845
126930                                         PIC 9.                   9915845
126940             07  WMS-OD-LIMIT-AMT5       PIC S9(13)V99 COMP-3.    9915845
126950             07  WMS-DATE-INTO-OD6.                               9915845
126960                 09  WMS-INTO-OD-MO6     PIC XX.                  9915845
126970                 09  WMS-INTO-OD-DA6     PIC XX.                  9915845
126980                 09  WMS-INTO-OD-YR6.                             9915845
126990                     11  WMS-INTO-OD-YR1-6   PIC X.               9915845
127000                     11  WMS-INTO-OD-YR2-6   PIC X.               9915845
127010             07  WMS-OD-LIMIT6           PIC X.                   9915845
127020             07  WMS-OD-LIMIT6-9  REDEFINES  WMS-OD-LIMIT6        9915845
127030                                         PIC 9.                   9915845
127040             07  WMS-OD-LIMIT-AMT6       PIC S9(13)V99 COMP-3.    9915845
127050             07  WMS-DATE-INTO-OD7.                               9915845
127060                 09  WMS-INTO-OD-MO7     PIC XX.                  9915845
127070                 09  WMS-INTO-OD-DA7     PIC XX.                  9915845
127080                 09  WMS-INTO-OD-YR7.                             9915845
127090                     11  WMS-INTO-OD-YR1-7   PIC X.               9915845
127100                     11  WMS-INTO-OD-YR2-7   PIC X.               9915845
127110             07  WMS-OD-LIMIT7           PIC X.                   9915845
127120             07  WMS-OD-LIMIT7-9  REDEFINES  WMS-OD-LIMIT7        9915845
127130                                        PIC 9.                    9915845
127140             07  WMS-OD-LIMIT-AMT7       PIC S9(13)V99 COMP-3.    9915845
127150             07  WMS-DATE-INTO-OD8.                               9915845
127160                 09  WMS-INTO-OD-MO8     PIC XX.                  9915845
127170                 09  WMS-INTO-OD-DA8     PIC XX.                  9915845
127180                 09  WMS-INTO-OD-YR8.                             9915845
127190                     11  WMS-INTO-OD-YR1-8   PIC X.               9915845
127200                     11  WMS-INTO-OD-YR2-8   PIC X.               9915845
127210             07  WMS-OD-LIMIT8           PIC X.                   9915845
127220             07  WMS-OD-LIMIT8-9  REDEFINES  WMS-OD-LIMIT8        9915845
127230                                         PIC 9.                   9915845
127240             07  WMS-OD-LIMIT-AMT8       PIC S9(13)V99 COMP-3.    9915845
127250             07  WMS-DATE-INTO-OD9.                               9915845
127260                 09  WMS-INTO-OD-MO9     PIC XX.                  9915845
127270                 09  WMS-INTO-OD-DA9     PIC XX.                  9915845
127280                 09  WMS-INTO-OD-YR9.                             9915845
127290                     11  WMS-INTO-OD-YR1-9   PIC X.               9915845
127300                     11  WMS-INTO-OD-YR2-9   PIC X.               9915845
127310         05  FILLER REDEFINES WMS-OD-LIMIT-DATE-EIGHT             9915845
127320                                         OCCURS 8 TIMES.          9915845
127330             07  WMS-OD-LIMIT-1-TO-8     PIC X.                   9915845
127340             07  WMS-OD-LIMIT-1-TO-8N REDEFINES                   9915845
127350                                         WMS-OD-LIMIT-1-TO-8      9915845
127360                                         PIC 9.                   9915845
127370             07  WMS-OD-LIMIT-AMT-1-TO-8 PIC S9(13)V99 COMP-3.    9915845
127380             07  WMS-DATE-INTO-OD-2-TO-9 PIC X(6).                9915845
127800         05  WMS-OD-ACR-AGGR             COMP-3.                  9915845
127900             07  WMS-OD-ACR-AGGR-CTD         PIC S9(15)V99.       9915845
127910             07  WMS-OD-ACR-AGGR-MTD         PIC S9(15)V99.       9915845
127920             07  WMS-OD-ACR-AGGR-DAYS-CTD    PIC S999.            9915845
127930             07  WMS-OD-ACR-AGGR-DAYS-MTD    PIC S999.            9915845
129000         05  WMS-OD-PRIME-ADJ-FLAGS-1-9.                          9915845
129010             07  WMS-OD-PRIME-ADJ-FLAG1  PIC X.                   9915845
129020             07  WMS-OD-PRIME-ADJ-FLAG2  PIC X.                   9915845
129030             07  WMS-OD-PRIME-ADJ-FLAG3  PIC X.                   9915845
129040             07  WMS-OD-PRIME-ADJ-FLAG4  PIC X.                   9915845
129050             07  WMS-OD-PRIME-ADJ-FLAG5  PIC X.                   9915845
129060             07  WMS-OD-PRIME-ADJ-FLAG6  PIC X.                   9915845
129070             07  WMS-OD-PRIME-ADJ-FLAG7  PIC X.                   9915845
129080             07  WMS-OD-PRIME-ADJ-FLAG8  PIC X.                   9915845
129090             07  WMS-OD-PRIME-ADJ-FLAG9  PIC X.                   9915845
129100         05  FILLER REDEFINES WMS-OD-PRIME-ADJ-FLAGS-1-9          9915845
129110                                         OCCURS 9 TIMES.          9915845
129120             07  WMS-OD-PRIME-ADJ-FLAG   PIC X.                   9915845
129500         05  WMS-OD-STATUS           PIC X.                       9915845
129600         05  WMS-DATE-INTO-OD1.                                   9915845
129610             07  WMS-INTO-OD-MO1     PIC XX.                      9915845
129620             07  WMS-INTO-OD-DA1     PIC XX.                      9915845
129630             07  WMS-INTO-OD-YR1.                                 9915845
129640                 09  WMS-INTO-OD-YR1-1   PIC X.                   9915845
129650                 09  WMS-INTO-OD-YR2-1   PIC X.                   9915845
129700         05  WMS-ODAC-BAL-HIST       PIC X.                       9915845
129800         05  WMS-ODAC-BAL-HIST-RET   PIC S9(3)       COMP-3.      9915845
129900         05  WMS-OD-YTD-ACR          PIC S9(13)V99   COMP-3.      9915845
130000         05  WMS-OD-INT-CYCLE.                                    9915845
130010             07  WMS-OD-INT-SCHED    PIC X.                       9915845
130020             07  WMS-OD-INT-INCR     PIC X.                       9915845
130030             07  WMS-OD-INT-MONTH1   PIC X(02).                   9915845
130040             07  WMS-OD-INT-DAY      PIC X(02).                   9915845
130050             07  WMS-OD-INT-DAY2     PIC X(02).                   9915845
130060             07  WMS-OD-INT-CLEAR    PIC X.                       9915845
130070             07  WMS-OD-INT-DATE-LAST-PMT.                        9915845
130080                 09  WMS-OD-INT-DL-PMT-MO PIC X(02).              9915845
130090                 09  WMS-OD-INT-DL-PMT-DA PIC X(02).              9915845
130100                 09  WMS-OD-INT-DL-PMT-YR PIC X(02).              9915845
130110             07  FILLER              PIC X(02).                   9915845
130120         05  WMS-OD-INT-XFER-PTR     PIC S9          COMP-3.      9915845
130130         05  WMS-OD-STMT-CYC-CHRG    PIC S9(13)V99   COMP-3.      9915845
130132         05  WMS-OD-PROJ-ACCR        PIC S9(13)V99   COMP-3.      0335614
130134         05  WMS-OD-PRIME-ADJ-START-DT PIC X(8).                  0930013
130136         05  WMS-OD-PRIME-ADJ-STOP-DT PIC X(8).                   0930013
130500         05  FILLER                  PIC X(55).                   0930013
130600*
130700     03  WMS-INFORMATION-TRAILER.
130800         05  WMS-INFO-MAR-STAT       PIC X.
130900         05  WMS-INFO-SEX            PIC X.
131000         05  WMS-INFO-RACE           PIC X.
131100         05  WMS-INFO-ALIEN-CODE     PIC X.
131200         05  WMS-INFO-COUNTRY.
131300             07  WMS-INFO-CONTINENT  PIC X.
131400             07  WMS-INFO-CNTRY      PIC XXXX.
131500         05  WMS-INFO-SMSA           PIC X(6).
131600         05  WMS-INFO-CENSUS-CODE    PIC X.
131700         05  WMS-INFO-CENSUS-TRACT   PIC X(10).
131800         05  WMS-INFO-STATE          PIC XX.
131900         05  WMS-INFO-COUNTY         PIC XXX.
132000         05  WMS-NRA-CERT-NAME1      PIC X.
132100         05  WMS-NRA-CERT-NAME2      PIC X.
132200         05  WMS-NRA-TAX-COUNTRY     PIC XX.
132300         05  WMS-INFO-DELETE         PIC X.
132310         05  WMS-NRA-TAX-PROVINCE    PIC XX.                      9915845
132400         05  FILLER                  PIC X(22).                   9915845
132500*
135900     03  WMS-EXTERNAL-DEPOSITS-TRAILER.
136000         05  WMS-EXTERNAL-SAV                        COMP-3.
136100             07  WMS-SAVINGS-BAL     PIC S9(13)V99.               9915845
136200             07  WMS-AGGR-SAV-BAL    PIC S9(15)V99.               9915845
136300             07  WMS-MIN-SAV-BAL     PIC S9(13)V99.               9915845
136400         05  FILLER                  PIC X(100).                  9915845
137600         05  WMS-EXT-DEP-DELETE      PIC X.
137700         05  FILLER                  PIC X(4).                    9915845
137800*
137900     03  WMS-KITING-SUSPECT-TRAILER.
138000         05  WMS-KITE-TRLR-DELETE    PIC X.
138100         05  WMS-DEP-TURN-RATIO      PIC S9V99       COMP-3.
138200         05  WMS-CR-DR-NO-RATIO      PIC S9V99       COMP-3.
138300         05  WMS-CR-DR-AMT-RATIO     PIC S9V99       COMP-3.
138400         05  WMS-DR-UNAVAIL-AMT-RATIO    PIC S9V99   COMP-3.
138500         05  WMS-KITE-BUS-DAYS       PIC S9(3)       COMP-3.
138600         05  WMS-KITING-SUSP-DAYS    PIC S9(3)       COMP-3.
138700         05  WMS-KITE-YTD-SUSP-DAYS  PIC S9(3)       COMP-3.
138800         05  FILLER                  PIC X(12).
138900         05  WMS-KITE-LGE-ITEM       PIC S9(13)V99   COMP-3.      9915845
139000         05  WMS-KITE-AGGR-BAL       PIC S9(15)V99   COMP-3.      9915845
139100         05  WMS-KITE-TOT-CR-NO      PIC S9(7)       COMP-3.
139200         05  WMS-KITE-TOT-CR-AMT     PIC S9(13)V99   COMP-3.
139300         05  WMS-KITE-TOT-DR-NO      PIC S9(7)       COMP-3.
139400         05  WMS-KITE-TOT-DR-AMT     PIC S9(13)V99   COMP-3.
139500         05  FILLER                  PIC X(14).
139600         05  WMS-KITE-TODAY-CREDIT-NO                COMP-3.
139700             07  WMS-KITE-TODAY-CR-NO    PIC S9(5)
139800                                                 OCCURS 7 TIMES.
139900         05  FILLER REDEFINES WMS-KITE-TODAY-CREDIT-NO    COMP-3.
140000             07  FILLER              PIC S9(5).
140100             07  WMS-KITE-TODAY-CR-NO-2-7.
140200                 09  FILLER          PIC S9(5)
140300                                                 OCCURS 6 TIMES.
140400         05  WMS-KITE-TODAY-CREDIT-AMT               COMP-3.
140500             07  WMS-KITE-TODAY-CR-AMT   PIC S9(13)V99            9915845
140600                                                 OCCURS 7 TIMES.
140700         05  FILLER REDEFINES WMS-KITE-TODAY-CREDIT-AMT   COMP-3.
140800             07  FILLER              PIC S9(13)V99.               9915845
140900             07  WMS-KITE-TODAY-CR-AMT-2-7.
141000                 09  FILLER          PIC S9(13)V99                9915845
141100                                                 OCCURS 6 TIMES.
141200         05  WMS-KITE-TODAY-DEBIT-NO                 COMP-3.
141300             07  WMS-KITE-TODAY-DR-NO    PIC S9(5)
141400                                                 OCCURS 7 TIMES.
141500         05  FILLER REDEFINES WMS-KITE-TODAY-DEBIT-NO     COMP-3.
141600             07  FILLER              PIC S9(5).
141700             07  WMS-KITE-TODAY-DR-NO-2-7.
141800                 09  FILLER          PIC S9(5)
141900                                                 OCCURS 6 TIMES.
142000         05  WMS-KITE-TODAY-DEBIT-AMT                COMP-3.
142100             07  WMS-KITE-TODAY-DR-AMT   PIC S9(13)V99            9915845
142200                                                 OCCURS 7 TIMES.
142300         05  FILLER REDEFINES WMS-KITE-TODAY-DEBIT-AMT    COMP-3.
142400             07  FILLER              PIC S9(13)V99.               9915845
142500             07  WMS-KITE-TODAY-DR-AMT-2-7.
142600                 09  FILLER          PIC S9(13)V99                9915845
142700                                                 OCCURS 6 TIMES.
142800         05  FILLER                  PIC X(39).                   9915845
142900*
143000     03  WMS-FUNDS-AVAIL-TRAILER.
143100         05  WMS-EFA-DELETE          PIC X.
143200         05  WMS-EFA-CALC-FLAG       PIC X.
143300         05  WMS-EFA-NEW-ACCT-FLAG   PIC X.
143400         05  WMS-EFA-COLLECTN-FLAG   PIC X.
143500         05  WMS-EFA-STATUS-FLAG     PIC X.
143600         05  WMS-EFA-LARGE-DEPOSIT-AMT PIC S9(13)V99 COMP-3.      9915845
143700         05  WMS-EFA-OD-NSF-TIMES    PIC S999        COMP-3.
143800         05  WMS-EFA-OD-ALL-AMT      PIC S9(13)V99   COMP-3.      9915845
143900         05  FILLER                  PIC X(3).
144000         05  WMS-EFA-CASH-SCHEDULE.
144100             07  WMS-EFA-NEXT-PTR-1  PIC X.
144200             07  WMS-EFA-NEXT-PTR9-1 REDEFINES WMS-EFA-NEXT-PTR-1
144300                                     PIC 9.
144400             07  WMS-EFA-NEXT-AMT-1  PIC S9(13)V99   COMP-3.      9915845
144500             07  WMS-EFA-SPLT-PTR-1  PIC X.
144600             07  WMS-EFA-SPLT-PTR9-1 REDEFINES WMS-EFA-SPLT-PTR-1
144700                                     PIC 9.
144800             07  WMS-EFA-SPLT-AMT-1  PIC S9(13)V99   COMP-3.      9915845
144900             07  WMS-EFA-PERCENT-1   PIC S9V99       COMP-3.
145000         05  FILLER                  PIC X(16).
145100         05  WMS-EFA-NEW-ACCT-DATE.
145200             07  WMS-EFA-NEW-ACCT-MO PIC XX.
145300             07  WMS-EFA-NEW-ACCT-DA PIC XX.
145400             07  WMS-EFA-NEW-ACCT-YR PIC XX.
145500         05  WMS-EFA-EXP-OD-NSF-DATE.
145600             07  WMS-EFA-EXP-OD-NSF-MO   PIC XX.
145700             07  WMS-EFA-EXP-OD-NSF-DA   PIC XX.
145800             07  WMS-EFA-EXP-OD-NSF-YR   PIC XX.
145900         05  WMS-EFA-CUR-OD-NSF-DATES            OCCURS 6 TIMES.
146000             07  WMS-EFA-OD-NSF-DATE PIC X(6).
146100             07  WMS-EFA-OD-NSF-TYPE PIC X.
146200         05  FILLER                  PIC X(17).                   9915845
146300*
146400     03  WMS-CASH-AVAIL-TRAILER.                                  9715504
146500         05  WMS-SPLIT-DAY-CASH                  OCCURS 12 TIMES.
146600             07  WMS-CASH1-AVAIL     PIC S9(13)V99   COMP-3.      9915845
146700             07  WMS-CASH2-AVAIL     PIC S9(13)V99   COMP-3.      9915845
146750         05  FILLER                  PIC X(12).                   9715504
146800     03  FILLER REDEFINES WMS-CASH-AVAIL-TRAILER.                 9915845
146900         05  FILLER                  PIC S9(13)V99   COMP-3.      9915845
147000         05  FILLER                  PIC S9(13)V99   COMP-3.      9915845
147100         05  WMS-CASH-2-12.
147200             07  FILLER                          OCCURS 11 TIMES.
147300                 09  FILLER          PIC S9(13)V99   COMP-3.      9915845
147400                 09  FILLER          PIC S9(13)V99   COMP-3.      9915845
147500         05  FILLER                  PIC X(12).                   9915845
147600*
147700     03  WMS-INVESTMENT-TRAILER.
147800         05  WMS-INV-SVC-ACCRD-TDY   PIC S9(11)V9(6) COMP-3.      9915845
147900         05  WMS-INV-SVC-ACCRD-CYC   PIC S9(11)V9(6) COMP-3.      9915845
148000         05  WMS-INV-SVC-ACCRD-MTD   PIC S9(13)V99   COMP-3.      9915845
148100         05  WMS-INV-SVC-COLL-TDY    PIC S9(13)V99   COMP-3.      9915845
148200         05  WMS-INV-SVC-COLL-CYC    PIC S9(13)V99   COMP-3.      9915845
148300         05  WMS-INV-SVC-COLL-MTD    PIC S9(13)V99   COMP-3.      9915845
148400         05  WMS-INV-SVC-COLL-YTD    PIC S9(13)V99   COMP-3.      9915845
148500         05  WMS-INV-SVC-COLL-PYTD   PIC S9(13)V99   COMP-3.      9915845
148600         05  FILLER                  PIC X(54).                   9915845
148700*
148800     03  WMS-RATE-TRAILER.
148900         05  WMS-RATE-TRAILER-FLAGS.
149000             10  WMS-DDA-RATE-FLAG   PIC X.
149100             10  WMS-SAV-RATE-FLAG   PIC X.
149200             10  WMS-OD-RATE-FLAG    PIC X.
149300             10  WMS-LN-RATE-FLAG    PIC X.
149310             10  WMS-FED-TAX-RATE-FLAG    PIC X.                  2012254
149320             10  WMS-ST-TAX-RATE-FLAG     PIC X.                  2012254
149330             10  WMS-LOC-TAX-RATE-FLAG    PIC X.                  2012254
149400         05  WMS-IOD-RATES.
149500             10  WMS-IOD-RATE-KEY-LEVEL    PIC X.
149600             10  WMS-IOD-CUR-DATA.
149700                 15  WMS-IOD-CUR-DATE      PIC X(8).
149800                 15  WMS-IOD-CUR-RATES     COMP-3.
149900                     20  WMS-IOD-CUR-ANN   PIC S9V9(8).           9915845
150100                     20  WMS-IOD-CUR-DAF   PIC SVP9(15).          9915845
150200             10  WMS-IOD-PREV-DATA.
150300                 15  WMS-IOD-PREV-DATE     PIC X(8).
150400                 15  WMS-IOD-PREV-RATES    COMP-3.
150500                     20  WMS-IOD-PREV-ANN  PIC S9V9(8).           9915845
150700                     20  WMS-IOD-PREV-DAF  PIC SVP9(15).          9915845
150800             10  FILLER                    PIC X(378).            9915845
150900         05  WMS-TIER-RATES REDEFINES WMS-IOD-RATES.
151000             10  WMS-TIER-RATE-KEY-LEVEL   PIC X.
151100             10  WMS-TIER-CUR-DATA.
151200                 15  WMS-TIER-CUR-DATE     PIC X(8).
151300                 15  WMS-TIER-CUR-RATES    OCCURS 9 TIMES COMP-3.
151400                     20  WMS-TIER-CUR-ANN  PIC S9V9(8).           9915845
151600                     20  WMS-TIER-CUR-DAF  PIC SVP9(15).          9915845
151700                 15  WMS-TIER-CUR-LMT      PIC S9(13)V99  COMP-3  9915845
151800                                               OCCURS 8 TIMES.
151900             10  WMS-DFLT-IOD-CUR-DATA.
152000                 15  WMS-DFLT-IOD-CUR-DATE     PIC X(8).
152100                 15  WMS-DFLT-IOD-CUR-RATES    COMP-3.
152200                     20  WMS-DFLT-IOD-CUR-ANN  PIC S9V9(8).       9915845
152400                     20  WMS-DFLT-IOD-CUR-DAF  PIC SVP9(15).      9915845
152500             10  WMS-TIER-PREV-DATA.
152600                 15  WMS-TIER-PREV-DATE    PIC X(8).
152700                 15  WMS-TIER-PREV-RATES   OCCURS 9 TIMES COMP-3.
152800                     20  WMS-TIER-PREV-ANN PIC S9V9(8).           9915845
153000                     20  WMS-TIER-PREV-DAF PIC SVP9(15).          9915845
153100                 15  WMS-TIER-PREV-LMT     PIC S9(13)V99  COMP-3  9915845
153200                                               OCCURS 8 TIMES.
153300             10  WMS-DFLT-IOD-PREV-DATA.
153400                 15  WMS-DFLT-IOD-PREV-DATE    PIC X(8).
153500                 15  WMS-DFLT-IOD-PREV-RATES   COMP-3.
153600                     20  WMS-DFLT-IOD-PREV-ANN PIC S9V9(8).       9915845
153800                     20  WMS-DFLT-IOD-PREV-DAF PIC SVP9(15).      9915845
153900         05  WMS-SAV-RATES.
154000             10  WMS-SAV-RATE-KEY-LEVEL    PIC X.
154100             10  WMS-SAV-CUR-DATA.
154200                 15  WMS-SAV-CUR-DATE      PIC X(8).
154300                 15  WMS-SAV-CUR-RATES     COMP-3.
154400                     20  WMS-SAV-CUR-ANN   PIC S9V9(8).           9915845
154600                     20  WMS-SAV-CUR-DAF   PIC SVP9(15).          9915845
154700             10  WMS-SAV-PREV-DATA.
154800                 15  WMS-SAV-PREV-DATE     PIC X(8).
154900                 15  WMS-SAV-PREV-RATES    COMP-3.
155000                     20  WMS-SAV-PREV-ANN  PIC S9V9(8).           9915845
155200                     20  WMS-SAV-PREV-DAF  PIC SVP9(15).          9915845
155300         05  WMS-OD-RATES.
155400             10  WMS-OD-RATE-KEY-LEVEL        PIC X.
155500             10  WMS-OD-CUR-DATA              OCCURS 9 TIMES.     9915845
155600                 15  WMS-OD-CUR-DATE          PIC X(8).
155700                 15  WMS-OD-CUR-RATES         COMP-3.
155800                     20  WMS-OD-CUR-ANN       PIC S9V9(8).        9915845
156000                     20  WMS-OD-CUR-DAF       PIC SVP9(15).       9915845
156100             10  WMS-OD-PRM-CUR-DATA.
156200                 15  WMS-OD-PRM-CUR-DATE      PIC X(8).
156300                 15  WMS-OD-CUR-PRIME         COMP-3.
156400                     20  WMS-OD-PRM-CUR-ANN   PIC S9V9(8).        9915845
156600                     20  WMS-OD-PRM-CUR-DAF   PIC SVP9(15).       9915845
156700             10  WMS-OD-PREV-DATA             OCCURS 9 TIMES.     9915845
156800                 15  WMS-OD-PREV-DATE         PIC X(8).
156900                 15  WMS-OD-PREV-RATES        COMP-3.
157000                     20  WMS-OD-PREV-ANN      PIC S9V9(8).        9915845
157200                     20  WMS-OD-PREV-DAF      PIC SVP9(15).       9915845
157300             10  WMS-OD-PRM-PREV-DATA.
157400                 15  WMS-OD-PRM-PREV-DATE     PIC X(8).
157500                 15  WMS-OD-PREV-PRIME        COMP-3.
157600                     20  WMS-OD-PRM-PREV-ANN  PIC S9V9(8).        9915845
157800                     20  WMS-OD-PRM-PREV-DAF  PIC SVP9(15).       9915845
157900         05  WMS-PRM-RATE.
158000             10  WMS-PRM-RATE-KEY-LEVEL   PIC X.
158100             10  WMS-PRM-CUR-DATA.
158200                 15  WMS-PRM-CUR-DATE     PIC X(8).
158300                 15  WMS-PRM-CUR-RATES.
158400                     20  WMS-PRM-CUR-ANN  PIC S9V9(8)   COMP-3.   9915845
158600                     20  WMS-PRM-CUR-DAF  PIC SVP9(15) COMP-3.    9915845
158700                     20  WMS-PRM-CUR-ADC  PIC X.
158800                     20  WMS-PRM-CUR-BCL  PIC X.
158900                     20  WMS-PRM-CUR-ADJ  PIC S9V9(8)   COMP-3    9915845
159000                                                 OCCURS 5 TIMES.
159100                 15  WMS-PRM-CUR-LMT      PIC S9(13)V99 COMP-3    9915845
159200                                                 OCCURS 4 TIMES.
159300             10  WMS-PRM-PREV-DATA.
159400                 15  WMS-PRM-PREV-DATE    PIC X(8).
159500                 15  WMS-PRM-PREV-RATES.
159600                     20  WMS-PRM-PREV-ANN PIC S9V9(8)   COMP-3.   9915845
159800                     20  WMS-PRM-PREV-DAF PIC SVP9(15)  COMP-3.   9915845
159900                     20  WMS-PRM-PREV-ADC PIC X.
160000                     20  WMS-PRM-PREV-BCL PIC X.
160100                     20  WMS-PRM-PREV-ADJ PIC S9V9(8)   COMP-3    9915845
160200                                                 OCCURS 5 TIMES.
160300                 15  WMS-PRM-PREV-LMT     PIC S9(13)V99 COMP-3    9915845
160400                                                 OCCURS 4 TIMES.
160500         05  WMS-SPL-RATES REDEFINES WMS-PRM-RATE.
160600             10  WMS-SPL-RATE-KEY-LEVEL   PIC X.
160700             10  WMS-SPL-CUR-DATA.
160800                 15  WMS-SPL-CUR-DATE     PIC X(8).
160900                 15  WMS-SPL-CUR-RATES    COMP-3 OCCURS 3 TIMES.
161000                     20  WMS-SPL-CUR-ANN  PIC S9V9(8).            9915845
161200                     20  WMS-SPL-CUR-DAF  PIC SVP9(15).           9915845
161300                 15  WMS-SPL-CUR-LMT      PIC S9(13)V99  COMP-3   9915845
161400                                                 OCCURS 2 TIMES.
161500             10  WMS-SPL-PREV-DATA.
161600                 15  WMS-SPL-PREV-DATE    PIC X(8).
161700                 15  WMS-SPL-PREV-RATES   COMP-3 OCCURS 3 TIMES.
161800                     20  WMS-SPL-PREV-ANN PIC S9V9(8).            9915845
162000                     20  WMS-SPL-PREV-DAF PIC SVP9(15).           9915845
162100                 15  WMS-SPL-PREV-LMT     PIC S9(13)V99  COMP-3   9915845
162200                                                 OCCURS 2 TIMES.
162300             10  FILLER                   PIC X(34).              9915845
162400         05  WMS-LN-RATES REDEFINES WMS-PRM-RATE.
162500             10  WMS-LN-RATE-KEY-LEVEL    PIC X.
162600             10  WMS-LN-CUR-DATA.
162700                 15  WMS-LN-CUR-DATE      PIC X(8).
162800                 15  WMS-LN-CUR-RATES     COMP-3.
162900                     20  WMS-LN-CUR-ANN   PIC S9V9(8).            9915845
163100                     20  WMS-LN-CUR-DAF   PIC SVP9(15).           9915845
163200             10  WMS-LN-PREV-DATA.
163300                 15  WMS-LN-PREV-DATE     PIC X(8).
163400                 15  WMS-LN-PREV-RATES    COMP-3.
163500                     20  WMS-LN-PREV-ANN  PIC S9V9(8).            9915845
163700                     20  WMS-LN-PREV-DAF  PIC SVP9(15).           9915845
163800             10  FILLER                   PIC X(118).             9915845
163900*  THE 'OLD-RATES' ARE USED FOR LOAN OLD MONEY CALCULATIONS.
164000         05  WMS-SPL-OLD-RATES.
164100             10  WMS-SPL-OLD-RATE-KEY-LEVEL  PIC X.
164200             10  WMS-SPL-OLD-CUR-DATA.
164300                 15  WMS-SPL-OLD-CUR-DATE    PIC X(8).
164400                 15  WMS-SPL-OLD-CUR-RATES   COMP-3
164500                                             OCCURS 3 TIMES.
164600                     20  WMS-SPL-OLD-CUR-ANN PIC S9V9(8).         9915845
164800                     20  WMS-SPL-OLD-CUR-DAF PIC SVP9(15).        9915845
164900                 15  WMS-SPL-OLD-CUR-LMT     PIC S9(13)V99 COMP-3 9915845
165000                                             OCCURS 2 TIMES.
165100             10  WMS-SPL-OLD-PREV-DATA.
165200                 15  WMS-SPL-OLD-PREV-DATE   PIC X(8).
165300                 15  WMS-SPL-OLD-PREV-RATES  COMP-3
165400                                             OCCURS 3 TIMES.
165500                     20  WMS-SPL-OLD-PREV-ANN PIC S9V9(8).        9915845
165700                     20  WMS-SPL-OLD-PREV-DAF PIC SVP9(15).       9915845
165800                 15  WMS-SPL-OLD-PREV-LMT    PIC S9(13)V99 COMP-3 9915845
165900                                             OCCURS 2 TIMES.
166000         05  WMS-LN-OLD-RATES REDEFINES WMS-SPL-OLD-RATES.
166100             10  WMS-LN-OLD-RATE-KEY-LEVEL   PIC X.
166200             10  WMS-LN-OLD-CUR-DATA.
166300                 15  WMS-LN-OLD-CUR-DATE     PIC X(8).
166400                 15  WMS-LN-OLD-CUR-RATES    COMP-3.
166500                     20  WMS-LN-OLD-CUR-ANN  PIC S9V9(8).         9915845
166700                     20  WMS-LN-OLD-CUR-DAF  PIC SVP9(15).        9915845
166800             10  WMS-LN-OLD-PREV-DATA.
166900                 15  WMS-LN-OLD-PREV-DATE    PIC X(8).
167000                 15  WMS-LN-OLD-PREV-RATES   COMP-3.
167100                     20  WMS-LN-OLD-PREV-ANN PIC S9V9(8).         9915845
167300                     20  WMS-LN-OLD-PREV-DAF PIC SVP9(15).        9915845
167400             10  FILLER                      PIC X(84).           9915845
167405         05  WMS-FED-TAX-RATES.                                   2012254
167410             10  WMS-FED-TAX-RATE-KEY-LEVEL  PIC X.               2012254
167415             10  WMS-FED-TAX-CUR-DATA.                            2012254
167420                 15  WMS-FED-TAX-CUR-DATE    PIC X(8).            2012254
167425                 15  WMS-FED-TAX-CUR-ANN     PIC S9V9(8)   COMP-3 2012254
167430                                               OCCURS 9 TIMES.    2012254
167435                 15  WMS-FED-TAX-CUR-LMT     PIC S9(13)V99 COMP-3 2012254
167440                                               OCCURS 8 TIMES.    2012254
167445         05  WMS-ST-TAX-RATES.                                    2012254
167450             10  WMS-ST-TAX-RATE-KEY-LEVEL   PIC X.               2012254
167455             10  WMS-ST-TAX-CUR-DATA.                             2012254
167460                 15  WMS-ST-TAX-CUR-DATE     PIC X(8).            2012254
167465                 15  WMS-ST-TAX-CUR-ANN      PIC S9V9(8)   COMP-3 2012254
167470                                               OCCURS 9 TIMES.    2012254
167475                 15  WMS-ST-TAX-CUR-LMT      PIC S9(13)V99 COMP-3 2012254
167480                                               OCCURS 8 TIMES.    2012254
167485         05  WMS-LOC-TAX-RATES.                                   2012254
167490             10  WMS-LOC-TAX-RATE-KEY-LEVEL  PIC X.               2012254
167495             10  WMS-LOC-TAX-CUR-DATA.                            2012254
167500                 15  WMS-LOC-TAX-CUR-DATE    PIC X(8).            2012254
167505                 15  WMS-LOC-TAX-CUR-ANN     PIC S9V9(8)   COMP-3 2012254
167510                                               OCCURS 9 TIMES.    2012254
167515                 15  WMS-LOC-TAX-CUR-LMT     PIC S9(13)V99 COMP-3 2012254
167520                                               OCCURS 8 TIMES.    2012254
167600*
167700     03  WMS-EXTENDED-SERV-CHG-TRLR.                              9715504
167800         05  WMS-DEPOSITED-ITEMS                     COMP-3.      9715504
167900             07  WMS-LOCAL-ITEMS         PIC S9(7).               9715504
168000             07  WMS-FOREIGN-ITEMS       PIC S9(7).               9715504
168100             07  WMS-LOCKBOX-ITEMS       PIC S9(7).               9715504
168200             07  WMS-NONPAR-ITEMS        PIC S9(7).               9715504
168300         05  WMS-DEPOSITED-AMTS                      COMP-3.      9715504
168400             07  WMS-CASH-DEPOSITS       PIC S9(13)V99.           9915845
168500             07  WMS-NON-PAR-DEP         PIC S9(13)V99.           9915845
168600             07  WMS-CARD-ENTRY          PIC S9(13)V99.           9915845
168700         05  WMS-CAPTURE-COUNT                       COMP-3.      9715504
168800             07  WMS-HI-SPEED-ITEMS      PIC S9(7).               9715504
168900             07  WMS-LOW-SPEED-ITEMS     PIC S9(7).               9715504
169000         05  WMS-MTD-DEPOSITED-ITEMS                 COMP-3.      9715504
169100             07  WMS-MTD-LOCAL-ITEMS     PIC S9(7).               9715504
169200             07  WMS-MTD-FOREIGN-ITEMS   PIC S9(7).               9715504
169300             07  WMS-MTD-LOCKBOX-ITEMS   PIC S9(7).               9715504
169400             07  WMS-MTD-NONPAR-ITEMS    PIC S9(7).               9715504
169500         05  WMS-MTD-DEPOSITED-AMTS                  COMP-3.      9715504
169600             07  WMS-MTD-CASH-DEPOSITS   PIC S9(13)V99.           9915845
169700             07  WMS-MTD-NONPAR-AMT      PIC S9(13)V99.           9915845
169800         05  WMS-MTD-CAPTURE-COUNT                   COMP-3.      9715504
169900             07  WMS-MTD-HI-SPEED-ITEMS  PIC S9(7).               9715504
170000             07  WMS-MTD-LOW-SPEED-ITEMS PIC S9(7).               9715504
170100         05  WMS-SC-DELETE               PIC X.                   9715504
170200         05  FILLER                      PIC X(111).              9915845
170300         05  WMS-SC-MISC-AMOUNTS     OCCURS 200 TIMES.            9715504
170400             07  WMS-SC-MISC-CNTR-FLG    PIC X.                   9715504
170500             07  WMS-SC-MISC-AMTS        PIC S9(13)V99 COMP-3.    9915845
170600             07  WMS-SC-MISC-NOS  REDEFINES WMS-SC-MISC-AMTS      9715504
170700                                         PIC S9(15)    COMP-3.    9915845
170800*                                                                 9915845
170900     03  WMS-DUAL-YEAR-TRAILER.                                   9915858
171000         05  WMS-DUAL-YR-CUR-IOD-AMOUNTS.                         9915858
171100             07  WMS-DUAL-YR-CUR-IOD-INT    PIC S9(13)V99 COMP-3. 9915858
171200             07  WMS-DUAL-YR-CUR-IOD-FED    PIC S9(13)V99 COMP-3. 9915858
171300             07  WMS-DUAL-YR-CUR-IOD-STATE  PIC S9(13)V99 COMP-3. 9915858
171400             07  WMS-DUAL-YR-CUR-IOD-LOCAL  PIC S9(13)V99 COMP-3. 9915858
171500             07  FILLER                     PIC S9(13)V99 COMP-3. 9915858
171600             07  FILLER                     PIC S9(13)V99 COMP-3. 9915858
171700             07  FILLER                     PIC S9(13)V99 COMP-3. 9915858
171800         05  WMS-DUAL-YR-PREV-IOD-AMOUNTS.                        9915858
171900             07  WMS-DUAL-YR-PREV-IOD-INT   PIC S9(13)V99 COMP-3. 9915858
172000             07  WMS-DUAL-YR-PREV-IOD-FED   PIC S9(13)V99 COMP-3. 9915858
172100             07  WMS-DUAL-YR-PREV-IOD-STATE PIC S9(13)V99 COMP-3. 9915858
172200             07  WMS-DUAL-YR-PREV-IOD-LOCAL PIC S9(13)V99 COMP-3. 9915858
172300             07  FILLER                     PIC S9(13)V99 COMP-3. 9915858
172400             07  FILLER                     PIC S9(13)V99 COMP-3. 9915858
172500             07  FILLER                     PIC S9(13)V99 COMP-3. 9915858
172600         05  WMS-DUAL-YR-CUR-SAV-AMOUNTS.                         9915858
172700             07  WMS-DUAL-YR-CUR-SAV-INT    PIC S9(13)V99 COMP-3. 9915858
172800             07  WMS-DUAL-YR-CUR-SAV-FED    PIC S9(13)V99 COMP-3. 9915858
172900             07  WMS-DUAL-YR-CUR-SAV-STATE  PIC S9(13)V99 COMP-3. 9915858
173000             07  WMS-DUAL-YR-CUR-SAV-LOCAL  PIC S9(13)V99 COMP-3. 9915858
173100             07  FILLER                     PIC S9(13)V99 COMP-3. 9915858
173200             07  FILLER                     PIC S9(13)V99 COMP-3. 9915858
173300             07  FILLER                     PIC S9(13)V99 COMP-3. 9915858
173400         05  WMS-DUAL-YR-PREV-SAV-AMOUNTS.                        9915858
173500             07  WMS-DUAL-YR-PREV-SAV-INT   PIC S9(13)V99 COMP-3. 9915858
173600             07  WMS-DUAL-YR-PREV-SAV-FED   PIC S9(13)V99 COMP-3. 9915858
173700             07  WMS-DUAL-YR-PREV-SAV-STATE PIC S9(13)V99 COMP-3. 9915858
173800             07  WMS-DUAL-YR-PREV-SAV-LOCAL PIC S9(13)V99 COMP-3. 9915858
173900             07  FILLER                     PIC S9(13)V99 COMP-3. 9915858
174000             07  FILLER                     PIC S9(13)V99 COMP-3. 9915858
174100             07  FILLER                     PIC S9(13)V99 COMP-3. 9915858
174200         05  FILLER                         PIC X(16).            9915858
175000*                                                                 9915845
500000     03  FILLER                            PIC X(143).            2012254
520000     03  WMS-PLN-TRLR-INFO.                                       0617360
520100         05  WMS-PLN-TRLR-TYPE               PIC X(02).           0617360
520200         05  WMS-PLN-TRLR-PLAN-KEY.                               0617360
520300             07  WMS-PLN-TRLR-PK-PLAN-ID     PIC X(12).           0617360
520400             07  WMS-PLN-TRLR-PK-SEQ-NO      PIC XXX.             0617360
520500             07  WMS-PLN-TRLR-PK-EMP-PLAN-ID PIC X(12).           0617360
520600         05  WMS-PLN-TRLR-REG-MIN-DATE.                           0617360
520700             07  WMS-PLN-TRLR-RMND-YEAR      PIC 9(4).            0617360
520800             07  FILLER  REDEFINES WMS-PLN-TRLR-RMND-YEAR.        0617360
520900                 09  WMS-PLN-TRLR-RMND-CENT.                      0617360
521000                     11  WMS-PLN-TRLR-RMND-C1 PIC X.              0617360
521100                     11  WMS-PLN-TRLR-RMND-C2 PIC X.              0617360
521200                 09  WMS-PLN-TRLR-RMND-YR    PIC XX.              0617360
521300             07  WMS-PLN-TRLR-RMND-MO        PIC XX.              0617360
521400             07  WMS-PLN-TRLR-RMND-DA        PIC XX.              0617360
521500         05  FILLER      REDEFINES WMS-PLN-TRLR-REG-MIN-DATE.     0617360
521600             07  FILLER                      PIC XX.              0617360
521700             07  WMS-PLN-TRLR-RMND-DATE      PIC X(6).            0617360
521800         05  WMS-PLN-TRLR-REG-MAX-DATE.                           0617360
521900             07  WMS-PLN-TRLR-RMXD-YEAR      PIC 9(4).            0617360
522000             07  FILLER  REDEFINES WMS-PLN-TRLR-RMXD-YEAR.        0617360
522100                 09  WMS-PLN-TRLR-RMXD-CENT.                      0617360
522200                     11  WMS-PLN-TRLR-RMXD-C1 PIC X.              0617360
522300                     11  WMS-PLN-TRLR-RMXD-C2 PIC X.              0617360
522400                 09  WMS-PLN-TRLR-RMXD-YR    PIC XX.              0617360
522500             07  WMS-PLN-TRLR-RMXD-MO        PIC XX.              0617360
522600             07  WMS-PLN-TRLR-RMXD-DA        PIC XX.              0617360
522700         05  FILLER      REDEFINES WMS-PLN-TRLR-REG-MAX-DATE.     0617360
522800             07  FILLER                      PIC XX.              0617360
522900             07  WMS-PLN-TRLR-RMXD-DATE      PIC X(6).            0617360
523000         05  WMS-PLN-TRLR-SPOUSE-SOC-SEC.                         0617360
523100             07  WMS-PLN-TRLR-SP-SS-CD       PIC X.               0617360
523200             07  WMS-PLN-TRLR-SP-SS-PREFIX   PIC X.               0617360
523300             07  WMS-PLN-TRLR-SP-SS-NO.                           0617360
523400                 09  WMS-PLN-TRLR-SP-SSN1    PIC XXX.             0617360
523500                 09  WMS-PLN-TRLR-SP-SSN2    PIC XX.              0617360
523600                 09  WMS-PLN-TRLR-SP-SSN3    PIC X(4).            0617360
523700             07  WMS-PLN-TRLR-SP-SS-SUFFIX   PIC X.               0617360
523800         05  WMS-PLN-TRLR-SPOUSE-BIRTH-DATE.                      0617360
523900             07  WMS-PLN-TRLR-SPBD-YEAR.                          0617360
524000                 09  WMS-PLN-TRLR-SPBD-CENT  PIC XX.              0617360
524100                 09  WMS-PLN-TRLR-SPBD-YR    PIC XX.              0617360
524200             07  WMS-PLN-TRLR-SPBD-MO        PIC XX.              0617360
524300             07  WMS-PLN-TRLR-SPBD-DA        PIC XX.              0617360
524400         05  FILLER REDEFINES WMS-PLN-TRLR-SPOUSE-BIRTH-DATE.     0617360
524500             07  FILLER                      PIC XX.              0617360
524600             07  WMS-PLN-TRLR-SPBD-DATE      PIC X(6).            0617360
524700         05  WMS-PLN-TRLR-YTD-BEG-BAL        PIC S9(13)V99 COMP-3.0617360
524800         05  WMS-PLN-TRLR-PY-BEG-BAL         PIC S9(13)V99 COMP-3.0617360
524900         05  WMS-PLN-TRLR-PY-MAX-CNT         PIC S9(7)V99 COMP-3. 0617360
525000         05  WMS-PLN-TRLR-FED-TAX-INFO.                           0617360
525100             07  WMS-PLN-TRLR-FED-TAX-RATE   PIC S9V9999  COMP-3. 0617360
525200             07  WMS-PLN-TRLR-FED-TAX-RATER                       0617360
525300                            REDEFINES WMS-PLN-TRLR-FED-TAX-RATE   0617360
525400                                             PIC S999V99  COMP-3. 0617360
525500             07  FILLER                      PIC XX.              0617360
525600             07  WMS-PLN-TRLR-FED-TAX-METH   PIC X.               0617360
525700             07  WMS-PLN-TRLR-FED-TAX-AMT    PIC S9(13)V99 COMP-3.0617360
525800             07  WMS-PLN-TRLR-FED-EXEMPTS    PIC S999     COMP-3. 0617360
525900             07  WMS-PLN-TRLR-FED-SNGL-JNT-OPT PIC X.             0617360
526000             07  FILLER                      PIC X(10).           0617360
526100         05  WMS-PLN-TRLR-STATE-TAX-INFO.                         0617360
526200             07  WMS-PLN-TRLR-STATE-TAX-RATE PIC S9V9999  COMP-3. 0617360
526300             07  WMS-PLN-TRLR-STATE-TAX-RATER                     0617360
526400                            REDEFINES WMS-PLN-TRLR-STATE-TAX-RATE 0617360
526500                                             PIC S999V99  COMP-3. 0617360
526600             07  FILLER                      PIC XX.              0617360
526700             07  WMS-PLN-TRLR-STATE-TAX-METH PIC X.               0617360
526800             07  WMS-PLN-TRLR-STATE-TAX-AMT  PIC S9(13)V99 COMP-3.0617360
526900             07  WMS-PLN-TRLR-STATE-EXEMPTS  PIC S999     COMP-3. 0617360
527000             07  WMS-PLN-TRL-STATE-SNGL-JNT-OPT PIC X.            0617360
527100             07  FILLER                      PIC XX.              0617360
527105         05  WMS-PLN-TRLR-DATE-ELIGIBLE.                          0617360
527110             07  WMS-PLN-TRLR-ELIG-YEAR.                          0617360
527115                 09  WMS-PLN-TRLR-ELIG-CENT  PIC XX.              0617360
527120                 09  WMS-PLN-TRLR-ELIG-YR    PIC XX.              0617360
527125             07  WMS-PLN-TRLR-ELIG-MO        PIC XX.              0617360
527130             07  WMS-PLN-TRLR-ELIG-DA        PIC XX.              0617360
527135         05  FILLER REDEFINES WMS-PLN-TRLR-DATE-ELIGIBLE.         0617360
527140             07  FILLER                      PIC XX.              0617360
527145             07  WMS-PLN-TRLR-ELIG-DATE      PIC X(6).            0617360
527200         05  WMS-PLN-TRLR-SUBTYPE            PIC XXX.             0617360
527300         05  WMS-PLN-TRLR-FAIR-MKT-VAL-CUR   PIC S9(13)V99 COMP-3.0617360
527400         05  WMS-PLN-TRLR-FAIR-MKT-VAL-PRV   PIC S9(13)V99 COMP-3.0617360
527500         05  WMS-PLN-TRLR-YR-END-ACCR-CUR    PIC S9(13)V99 COMP-3.0617360
527600         05  WMS-PLN-TRLR-YR-END-ACCR-PRV    PIC S9(13)V99 COMP-3.0617360
527700         05  WMS-PLN-TRLR-LAST-DIST-INFO.                         0617360
527800             07  WMS-PLN-TRLR-DATE-LAST-DIST.                     0617360
527900                 09  WMS-PLN-TRLR-LST-DIST-CN    PIC XX.          0617360
528000                 09  WMS-PLN-TRLR-LAST-DIST-DATE.                 0617360
528100                     11  WMS-PLN-TRLR-LST-DIST-YY PIC XX.         0617360
528200                     11  WMS-PLN-TRLR-LST-DIST-MO PIC XX.         0617360
528300                     11  WMS-PLN-TRLR-LST-DIST-DA PIC XX.         0617360
528400             07  WMS-PLN-TRLR-LAST-DIST-CODE PIC XX.              0617360
528500         05  WMS-PLN-TRLR-UPGRADED           PIC X.               0617360
528600         05  WMS-PLN-TRLR-EARLY-DIST         PIC X.               0617360
528700         05  WMS-PLN-TRLR-MIN-DIST-AMT-PY    PIC S9(13)V99 COMP-3.0617360
528800*        THE NEXT FIELD IS USED WHEN BUILDING THE P01 AND P02     0617360
528900*        IMXRF RECORDS TO HOUSE THE YTD REPORTABLE CONTRIBUTIONS  0617360
529000*        AND DISTRIBUTIONS CALCULATED BY THE FIIMLOAD PROGRAM     0617360
529100         05  WMS-PLN-TRLR-XRF-YTD-REM        PIC X(32).           0617360
529200         05  WMS-PLN-TRLR-PY-MAX-CNT-PNT     PIC S9(4) COMP.      0617360
529300         05  FILLER      REDEFINES WMS-PLN-TRLR-PY-MAX-CNT-PNT.   0617360
529400             07  FILLER                      PIC X.               0617360
529500             07  WMS-PLN-TRLR-PY-MAX-CNT-PNT-B  PIC X.            0617360
529600         05  WMS-PLN-TRLR-PY-STATUS-CD       PIC X.               0617360
529700         05  WMS-PLN-TRLR-FAIR-MKT-VAL-DTH   PIC S9(13)V99 COMP-3.0617360
529800         05  WMS-PLN-TRLR-SNG-FAM-FLG        PIC X.               0617360
529810         05  WMS-PLN-TRLR-PL-WITHHOLDING     PIC X.               0617360
529900         05  FILLER                          PIC X.               0617360
530000         05  WMS-PLN-TRLR-PRIMARY-IND.                            0617360
530100             07  WMS-PLN-TRLR-PRIMARY-IND-N  PIC 9.               0617360
530200         05  WMS-PLN-TRLR-STATUS-CD          PIC X.               0617360
530300         05  WMS-PLN-TRLR-PART-EXC-FLG       PIC X.               0617360
530400         05  WMS-PLN-TRLR-EARLY-DIST-RSN     PIC X.               0617360
530500         05  WMS-PLN-TRLR-VALID-CONTR        PIC X.               0617360
530600         05  WMS-PLN-TRLR-MAX-CNT-PNT        PIC S9(4)    COMP.   0617360
530700         05  FILLER      REDEFINES WMS-PLN-TRLR-MAX-CNT-PNT.      0617360
530800             07  FILLER                      PIC X.               0617360
530900             07  WMS-PLN-TRLR-MAX-CNT-PNT-B  PIC X.               0617360
531000         05  WMS-PLN-TRLR-MAX-CNT-LIM        PIC S9(7)V99 COMP-3. 0617360
531100         05  WMS-PLN-TRLR-RECERT-REQD        PIC X.               0827778
531102         05  WMS-PLN-TRLR-NXT-SNG-FAM        PIC X.               0827778
531200         05  WMS-PLN-TRLR-MIN-DIST-AMT       PIC S9(13)V99 COMP-3.0617360
531300         05  WMS-PLN-TRLR-EMPR-ID            PIC X(12).           0617360
531400         05  WMS-PLN-TRLR-SCHED-DISTRIB.                          0617360
531500             07  WMS-PLN-TRLR-SC-DIST-SCHED-CD PIC X(02).         0617360
531600             07  WMS-PLN-TRLR-SC-DIST-PAY-AMT                     0617360
531700                                             PIC S9(13)V99 COMP-3.0617360
531800             07  WMS-PLN-TRLR-SC-DIST-CD     PIC X.               0617360
531900             07  WMS-PLN-TRLR-SC-DIST-FREQ   PIC X.               0617360
532000             07  WMS-PLN-TRLR-DIST-FQ-N                           0617360
532100                         REDEFINES WMS-PLN-TRLR-SC-DIST-FREQ      0617360
532200                                             PIC 9.               0617360
532300             07  WMS-PLN-TRLR-SC-C.                               0617360
532400                 09  WMS-PLN-TRLR-SC-C-DAY1  PIC XX.              0617360
532500                 09  WMS-PLN-TRLR-SC-C-DAY2  PIC XX.              0617360
532600                 09  WMS-PLN-TRLR-SC-C-MNTH  PIC XX.              0617360
532700             07  WMS-PLN-TRLR-SC-DATE-NXT-PAY.                    0617360
532800                 09  WMS-PLN-TRLR-SC-DNP-YEAR PIC 9(4).           0617360
532900                 09  FILLER REDEFINES WMS-PLN-TRLR-SC-DNP-YEAR.   0617360
533000                     11  WMS-PLN-TRLR-SC-DNP-CENT.                0617360
533100                         13  WMS-PLN-TRLR-SC-DNP-C1  PIC X.       0617360
533200                         13  WMS-PLN-TRLR-SC-DNP-C2  PIC X.       0617360
533300                     11  WMS-PLN-TRLR-SC-DNP-YR      PIC XX.      0617360
533400                 09  WMS-PLN-TRLR-SC-DNP-MO          PIC XX.      0617360
533500                 09  WMS-PLN-TRLR-SC-DNP-DA          PIC XX.      0617360
533600             07  FILLER  REDEFINES WMS-PLN-TRLR-SC-DATE-NXT-PAY.  0617360
533700                 09  FILLER                          PIC XX.      0617360
533800                 09  WMS-PLN-TRLR-SC-DNP-DATE        PIC X(6).    0617360
533900             07  WMS-PLN-TRLR-SC-CHK-SP-HNDL         PIC X.       0617360
534000             07  WMS-PLN-TRLR-SC-FIXED-EXP-CD        PIC X.       0617360
534100             07  WMS-PLN-TRLR-SC-ORIG-CNT        PIC S9(5) COMP-3.0617360
534200             07  WMS-PLN-TRLR-SC-REM-CNT         PIC S9(5) COMP-3.0617360
534300             07  WMS-PLN-TRLR-SC-EXPIRE-DATE.                     0617360
534400                 09  WMS-PLN-TRLR-SC-EXP-YEAR PIC 9(4).           0617360
534500                 09  FILLER REDEFINES WMS-PLN-TRLR-SC-EXP-YEAR.   0617360
534600                     11  WMS-PLN-TRLR-SC-EXP-CENT.                0617360
534700                         13  WMS-PLN-TRLR-SC-EXP-C1  PIC X.       0617360
534800                         13  WMS-PLN-TRLR-SC-EXP-C2  PIC X.       0617360
534900                     11  WMS-PLN-TRLR-SC-EXP-YR      PIC XX.      0617360
535000                 09  WMS-PLN-TRLR-SC-EXP-MO          PIC XX.      0617360
535100                 09  WMS-PLN-TRLR-SC-EXP-DA          PIC XX.      0617360
535200             07  FILLER  REDEFINES WMS-PLN-TRLR-SC-EXPIRE-DATE.   0617360
535300                 09  FILLER                          PIC XX.      0617360
535400                 09  WMS-PLN-TRLR-SC-EXP-DATE        PIC X(6).    0617360
535500         05  FILLER                               PIC X.          0617360
535600         05  WMS-PLN-TRLR-PR-AVERAGING            PIC X.          0617360
535700         05  WMS-PLN-TRLR-PR-DEATH-EXCL           PIC X.          0617360
535800         05  WMS-PLN-TRLR-PLAN-RPTG-INFO   COMP-3.                0617360
535900             07  WMS-PLN-TRLR-PR-LTD-D-CT-VOL     PIC S9(13)V99.  0617360
536000             07  WMS-PLN-TRLR-PR-LTD-D-CT-EMPR    PIC S9(13)V99.  0617360
536100             07  WMS-PLN-TRLR-PR-LTD-D-CT-RO      PIC S9(13)V99.  0617360
536200             07  WMS-PLN-TRLR-PR-LTD-D-CT-TRNF    PIC S9(13)V99.  0617360
536300             07  WMS-PLN-TRLR-PR-LTD-D-CT-ITRF    PIC S9(13)V99.  0617360
536400             07  WMS-PLN-TRLR-PR-LTD-D-CT-ETRF    PIC S9(13)V99.  0617360
536500             07  WMS-PLN-TRLR-PR-LTD-ND-CT-VOL    PIC S9(13)V99.  0617360
536600             07  WMS-PLN-TRLR-PR-LTD-ND-CT-EMPR   PIC S9(13)V99.  0617360
536700             07  WMS-PLN-TRLR-PR-LTD-ND-CT-RO     PIC S9(13)V99.  0617360
536800             07  WMS-PLN-TRLR-PR-LTD-ND-CT-TRNF   PIC S9(13)V99.  0617360
536900             07  WMS-PLN-TRLR-PR-LTD-ND-CT-ITRF   PIC S9(13)V99.  0617360
537000             07  WMS-PLN-TRLR-PR-LTD-ND-CT-ETRF   PIC S9(13)V99.  0617360
537100             07  WMS-PLN-TRLR-PR-LTD-IRA-DIST     PIC S9(13)V99.  0617360
537200             07  WMS-PLN-TRLR-PR-LTD-SEP-DIST     PIC S9(13)V99.  0617360
537300             07  WMS-PLN-TRLR-PR-LTD-D-EM-CT-K    PIC S9(13)V99.  0617360
537400             07  WMS-PLN-TRLR-PR-LTD-ND-EM-CT-K   PIC S9(13)V99.  0617360
537500             07  WMS-PLN-TRLR-PR-LTD-EMPR-CT-K    PIC S9(13)V99.  0617360
537600             07  WMS-PLN-TRLR-PR-LTD-INT-PD       PIC S9(13)V99.  0617360
537700         05  WMS-PLN-TRLR-DEATH-DATE.                             0617360
537800             09  WMS-PLN-TRLR-DEATH-YEAR          PIC 9(4).       0617360
537900             09  FILLER REDEFINES WMS-PLN-TRLR-DEATH-YEAR.        0617360
538000                 11  WMS-PLN-TRLR-DEATH-CENT      PIC XX.         0617360
538100                 11  WMS-PLN-TRLR-DEATH-YR        PIC XX.         0617360
538200             09  WMS-PLN-TRLR-DEATH-MO            PIC XX.         0617360
538300             09  WMS-PLN-TRLR-DEATH-DA            PIC XX.         0617360
538400         05  WMS-PLN-TRLR-RETIRE-DATE.                            0617360
538500             09  WMS-PLN-TRLR-RETIRE-YEAR         PIC 9(4).       0617360
538600             09  FILLER REDEFINES WMS-PLN-TRLR-RETIRE-YEAR.       0617360
538700                 11  WMS-PLN-TRLR-RETIRE-CENT     PIC XX.         0617360
538800                 11  WMS-PLN-TRLR-RETIRE-YR       PIC XX.         0617360
538900             09  WMS-PLN-TRLR-RETIRE-MO           PIC XX.         0617360
539000             09  WMS-PLN-TRLR-RETIRE-DA           PIC XX.         0617360
539100         05  FILLER                               PIC X(2).       0617360
539200         05  WMS-PLN-TRLR-FEE-INFO.                               0617360
539300             07  WMS-PLN-TRLR-FEE-BILL-CODE       PIC X.          0617360
539400             07  WMS-PLN-TRLR-DATE-FEE-PAID.                      0617360
539500                 09  WMS-PLN-TRLR-FPD-CENT        PIC XX.         0617360
539600                 09  WMS-PLN-TRLR-FEE-PAID-DATE.                  0617360
539700                     11  WMS-PLN-TRLR-FPD-YR      PIC XX.         0617360
539800                     11  WMS-PLN-TRLR-FPD-MO      PIC XX.         0617360
539900                     11  WMS-PLN-TRLR-FPD-DA      PIC XX.         0617360
540000             07  WMS-PLN-TRLR-BILLING-DUE-DATE.                   0617360
540100                 09  WMS-PLN-TRLR-FBDD-YEAR.                      0617360
540200                     11  WMS-PLN-TRLR-FBDD-CENT   PIC XX.         0617360
540300                     11  WMS-PLN-TRLR-FBDD-YR     PIC XX.         0617360
540400                 09  WMS-PLN-TRLR-FBDD-MO         PIC XX.         0617360
540500                 09  WMS-PLN-TRLR-FBDD-DA         PIC XX.         0617360
540600             07  FILLER  REDEFINES WMS-PLN-TRLR-BILLING-DUE-DATE. 0617360
540700                 09  FILLER                       PIC XX.         0617360
540800                 09  WMS-PLN-TRLR-FEE-BILL-DUE-DATE PIC X(6).     0617360
540900             07 WMS-PLN-TRLR-FEES-DUE        PIC S9(13)V99 COMP-3.0617360
541000             07 WMS-PLN-TRLR-FEE-COLL-MTD    PIC S9(13)V99 COMP-3.0617360
541100             07 WMS-PLN-TRLR-FEE-COLL-LST-PYMT                    0617360
541200                                             PIC S9(13)V99 COMP-3.0617360
541300             07 WMS-PLN-TRLR-FEE-COLL-YTD    PIC S9(13)V99 COMP-3.0617360
541400             07 WMS-PLN-TRLR-FEE-COLL-PREV-YTD                    0617360
541500                                             PIC S9(13)V99 COMP-3.0617360
541600             07 WMS-PLN-TRLR-FEE-COLL-LTD    PIC S9(13)V99 COMP-3.0617360
541700         05  WMS-PLN-TRLR-SDA-FLAG            PIC X.              0617360
541800         05  WMS-PLN-TRLR-AUTO-DIST-ORDER     PIC X.              0617360
541900         05  WMS-PLN-TRLR-AUTO-DIST-SEQ       PIC S999     COMP-3.0617360
542000         05  WMS-PLN-TRLR-BENE-FLAG           PIC X.              0617360
542100         05  WMS-PLN-TRLR-PLAN-SCHED-FLAG     PIC X.              0617360
542200         05  WMS-PLN-TRLR-BENE-PCT            PIC S999V99  COMP-3.0617360
542300         05  WMS-PLN-TRLR-PCT-R  REDEFINES WMS-PLN-TRLR-BENE-PCT  0617360
542400                                              PIC S9V9999  COMP-3.0617360
542500         05  WMS-PLN-TRLR-XRF-CALCD-FLDS      PIC X(16).          0617360
542600         05  WMS-PLN-TRLR-ACT-CNT-LIM         PIC S9(7)V99 COMP-3.0617360
542700         05  WMS-PLN-TRLR-ACC-CNT-LIM         PIC S9(7)V99 COMP-3.0617360
542800         05  WMS-PLN-TRLR-PY-ACT-CNT-LIM      PIC S9(7)V99 COMP-3.0617360
542900         05  FILLER                           PIC X(2).           0617360
543000         05  WMS-PLN-TRLR-EDITED-PLAN-KEY.                        0617360
543100             07  WMS-PLN-TRLR-EDITED-PLAN-ID  PIC X(15).          0617360
543200             07  FILLER                       PIC X.              0617360
543300             07  WMS-PLN-TRLR-EDITED-SEQ      PIC XXX.            0617360
543400             07  FILLER                       PIC X.              0617360
543500             07  WMS-PLN-TRLR-EDITED-EMP-ID   PIC X(15).          0617360
543600         05  WMS-PLN-TRLR-CTDT-TOTALS.                            0617360
543700             07  WMS-PLN-TRLR-CTDT-NO        PIC S999     COMP-3. 0617360
543800             07  WMS-PLN-TRLR-CTDT-TRLR       OCCURS 100 TIMES    0617360
543900                                              INDEXED BY          0617360
544000                                              WMS-PLN-TRLR-IND    0617360
544100                                              WMS-PLN-TRLR-IND2.  0617360
544200                 09  WMS-PLN-TRLR-CTDT-KEY.                       0617360
544300                     11  WMS-PLN-TRLR-CAL-YR  PIC X.              0617360
544400                     11  WMS-PLN-TRLR-TAX-YR  PIC X.              0617360
544500                     11  WMS-PLN-TRLR-COMB-PL-CODES.              0617360
544600                         13  WMS-PLN-TRLR-PL-CODE PIC XX.         0617360
544700                         13  WMS-PLN-TRLR-PL-CODE2 PIC XX.        0617360
544800                     11  WMS-PLN-TRLR-TAX-CODE PIC X.             0617360
544900                 09  WMS-PLN-TRLR-CTDT-AMT   PIC S9(13)V99 COMP-3.0617360
600000*                                                                 9915845
600100     03  WMS-DC-TRAILER.                                          9915845
600200         05  WMS-DC-TRLR-LENGTH      PIC S999        COMP-3.      9915845
600300         05  WMS-DC-DELETE           PIC X.                       9915845
600400         05  WMS-DC-FILLER           PIC X(747).                  0930011
600500*****    PLACE DESCRIPTION OF DC FIELDS HERE - SEE EXAMPLE BELOW. 9915845
600600*        05  SALT-LAKE-DC-FILLER REDEFINES WMS-DC-FILLER.         9915845
600700*            07  WMS-DC-PIN-NUMBER   PIC XXXX.                    9915845
600800*            07  WMS-DC-NO-CARDS     PIC 9.                       9915845
600900*            07  WMS-DC-EXP-DATE.                                 9915845
601000*                09  WMS-DC-EXP-MO   PIC XX.                      9915845
601100*                09  WMS-DC-EXP-DA   PIC XX.                      9915845
601200*                09  WMS-DC-EXP-YR   PIC XX.                      9915845
601300*            07  WMS-DC-CARD-NUMBER  PIC X(14).                   9915845
601400*            07  WMS-DC-TYPE-CNT     PIC X.                       9915845
700000*                                                                 9915845
700100         05  WMS-INTBK-DC-FILLER REDEFINES WMS-DC-FILLER.         IMIB004
700200             07  WMS-DC-DATE-STATUS-CHANGE.                       IMIB004
700300                 09  WMS-DC-DSC-MO   PIC XX.                      IMIB004
700400                 09  WMS-DC-DSC-DA   PIC XX.                      IMIB004
700500                 09  WMS-DC-DSC-CC   PIC XX.                      IMIB004
700600                 09  WMS-DC-DSC-YR   PIC XX.                      IMIB004
700700             07  WMS-DC-DEPARTMENT           PIC X(20).           IMIB01A
700800             07  WMS-DC-ECONOMIC-GROUP       PIC XXXX.            IMIB01A
700900             07  WMS-DC-CIIU-CODE            PIC XXXX.            IMIB01A
701000             07  WMS-DC-QUALIFICATION        PIC 9(03).           IMIB01A
701100             07  WMS-DC-QUALIFICATION-YEAR   PIC XXXX.            IMIB01A
701200             07  WMS-DC-QUALIFICATION-PERIOD PIC XX.              IMIB01A
701300             07  WMS-DC-QUALIFICATION-CHECKS PIC X.               IMIB01A
701400             07  WMS-DC-FINALIDAD            PIC X(25).           IMIB76
701600             07  WMS-DC-TIPO-DE-SOCIEDAD     PIC X(10).           IMIB58
701700             07  WMS-DC-REFRENDO             PIC X.               IMIB58
701800             07  WMS-DC-NOMBRE-COMERCIAL     PIC X(25).           IMIB58
701900             07  FILLER                      PIC X(640).          IMIB58
