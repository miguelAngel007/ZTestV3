*     * FO5238*12/05/11 JCTE PROYECTO UPGRADE SYSTEMAT
000001*--------------------------------------------------------------*  IMIB86
000002*               ** HISTORY OF REVISIONS **                     *  IMIB86
000003* DESCRIPTION                                           CHNGID *  IMIB86
000004* ____________________________________________________  _______*  IMIB86
000005*                                                              *  IMIB86
000006* 27/06/06 JCSC RETENCION DE TODOS LOS FONDOS - DEBITOS FO2407 *  FO2407
000007* 10/09/09 PAMH NUEVOS TIPOS DE CREDITO (BASILEA II)    GG0374 *  GG0374
000008* 13/08/10 REAPPLY: ADD USER CODE, OFFICER SIMB011      IMIB86 *  IMIB86
000009* 13/08/10 REAPPLY: CREATE ALTERNATE KEY IN MEMO        IMIB79 *  IMIB79
000010* 13/08/10 REAPPLY CUSTOM CODE AND REPLACED THE FF             *  IMUP001
000011*          CHANGE ID: IMIBXX, IMIBXXX, MLNI038          IMUP001*  IMUP001
000012* 13/08/10 REAPPLY: DEFINE RETENCION MEGA FIELD IN MEMO IMIB74 *  IMIB74
000013* 13/08/10 REAPPLY CUSTOM CODE FOR HANDLING ACCT STATUS IMIB004*  IMIB74
000099*--------------------------------------------------------------*  IMIB86
000100 01  IM-MEMO-OL-AREA.
000300   02  IM-MEMO-OL-RECORD.
000500     03  IM-MEMO-OL-KEY.
000700         05  IM-MEMO-OL-CTL-1              PIC XX.
000800         05  IM-MEMO-OL-CTL-2              PIC XXX.
000900         05  IM-MEMO-OL-CTL-3              PIC XXX.
000910         05  IM-MEMO-OL-CTL4-ACCT.                                0417078
001000             07  IM-MEMO-OL-CTL-4          PIC X(4).              0417078
001100             07  IM-MEMO-OL-ACCT-NO        PIC X(10).             0417078
001110         05  FILLER REDEFINES IM-MEMO-OL-CTL4-ACCT.               0417078
001120             07  FILLER                        PIC X(02).         0417078
001130             07  IM-MEMO-OL-ACCT-12            PIC X(12).         0417078
001140     03  IM-MEMO-OL-RECORD-ID              PIC X.                 0417078
001200     03  FILLER                            PIC X(14).             9915845
001300     03  IM-MEMO-OL-TODAY-RBA.
001410         05  IM-MEMO-OL-CUR-LOG-RBA-CMP    PIC S9(8) COMP.        IM008
001500         05  IM-MEMO-OL-CUR-LOG-RBA REDEFINES                     IM008
001510               IM-MEMO-OL-CUR-LOG-RBA-CMP  PIC X(4).              IM008
001520         05  IM-MEMO-OL-TS-CUR-LOG-RBA-CMP PIC S9(8) COMP.        IM008
001600         05  IM-MEMO-OL-TS-CUR-LOG-RBA REDEFINES                  IM008
001610             IM-MEMO-OL-TS-CUR-LOG-RBA-CMP PIC X(4).              IM008
001700         05  IM-MEMO-OL-MIR-RBA            PIC S9(8) COMP.
001800         05  IM-MEMO-OL-MIR-RBA-X REDEFINES
001900                IM-MEMO-OL-MIR-RBA         PIC X(4).
002000         05  FILLER                        PIC X(4).
002200     03  IM-MEMO-OL-FLAGS.
002400         05  IM-MEMO-OL-NEW-DEL-FLAG       PIC X.
002500             88  IM-MEMO-OL-NEW-ACCT                   VALUE 'N'.
002600             88  IM-MEMO-OL-DEL-ACCT                   VALUE 'D'.
002700         05  IM-MEMO-OL-FM-FLAG            PIC X.
002800             88  IM-MEMO-OL-FM-TODAY                   VALUE 'Y'.
002900         05  IM-MEMO-OL-NAME-ADDR-FLAG     PIC X.
003000             88  IM-MEMO-OL-NAME-ADDR-CHG         VALUES '1' '2'. 9915868
003100         05  IM-MEMO-OL-MEMO-FLAG          PIC X.
003200             88  IM-MEMO-OL-MEMO-TRAN                  VALUE 'Y'.
003300         05  IM-MEMO-OL-STOP-FLAG          PIC X.
003400             88  IM-MEMO-OL-STOP-TRAN                  VALUE 'Y'.
003500         05  IM-MEMO-OL-HOLD-FLAG          PIC X.
003600             88  IM-MEMO-OL-HOLD-TRAN                  VALUE 'Y'.
003700         05  IM-MEMO-OL-REC-RESTART-FLAG   PIC X.
003800             88  IM-MEMO-OL-REC-RESTART                VALUE 'Y'.
003900         05  IM-MEMO-OL-BACKDATE-FLAG      PIC X.
004000             88  IM-MEMO-OL-BACKDATE                   VALUE 'Y'.
004010         05  IM-MEMO-OL-STOP-HOLD-DEL-FLAG PIC X.                 0902008
004020             88  IM-MEMO-OL-STOP-HOLD-DEL              VALUE 'Y'. 0902008
004030         05  IM-MEMO-OL-SV-BKDT-FLAG       PIC X.                 0902834
004040             88  IM-MEMO-OL-SV-BKDT                    VALUE 'Y'. 0902834
004050         05  IM-MEMO-OL-LN-BKDT-FLAG       PIC X.                 0902834
004060             88  IM-MEMO-OL-LN-BKDT                    VALUE 'Y'. 0902834
004070         05  IM-MEMO-OL-STMT-MAINT-TODAY   PIC X.                 9915876
004080             88  IM-MEMO-OL-STMT-MAINT                 VALUE 'Y'. 9915876
004082         05  IM-MEMO-OL-FORCE-POST-FLAG    PIC X.                 0827769
004084             88  IM-MEMO-OL-FORCE-POST                 VALUE 'Y'. 0827769
004086         05  IM-MEMO-OL-RED-ALERT-FLAG     PIC X.                 0837826
004100         05  FILLER                        PIC X(10).             0837826
004300     03  IM-MEMO-OL-TODAY-ACTIVITY.
004500         05  IM-MEMO-OL-LTT-TELLER         PIC X(4).
004600         05  IM-MEMO-OL-LTT-TRAN-ID        PIC X(4).
004700         05  IM-MEMO-OL-LTT-AMT            PIC S9(13)V99  COMP-3.
004800         05  IM-MEMO-OL-DR-NUM             PIC S9(5)      COMP-3.
004900         05  IM-MEMO-OL-DR-AMT             PIC S9(13)V99  COMP-3.
005000         05  IM-MEMO-OL-CR-NUM             PIC S9(5)      COMP-3.
005100         05  IM-MEMO-OL-CR-AMT             PIC S9(13)V99  COMP-3.
005200         05  IM-MEMO-OL-SV-DR-NUM          PIC S9(3)      COMP-3.
005300         05  IM-MEMO-OL-SV-DR-AMT          PIC S9(13)V99  COMP-3. 9915845
005400         05  IM-MEMO-OL-SV-CR-NUM          PIC S9(3)      COMP-3.
005500         05  IM-MEMO-OL-SV-CR-AMT          PIC S9(13)V99  COMP-3. 9915845
005600         05  IM-MEMO-OL-LN-DR-NUM          PIC S9(3)      COMP-3.
005700         05  IM-MEMO-OL-LN-DR-AMT          PIC S9(13)V99  COMP-3. 9915845
005800         05  IM-MEMO-OL-LN-CR-NUM          PIC S9(3)      COMP-3.
005900         05  IM-MEMO-OL-LN-CR-AMT          PIC S9(13)V99  COMP-3. 9915845
005910         05  IM-MEMO-OL-LAST-LOG-GRPN      PIC XX.                IM008
005920         05  IM-MEMO-OL-LAST-LOG-NXT-DAY   PIC X.                 IM008
005922         05  IM-MEMO-OL-LOAN-TRLR-FLAG     PIC X.                 9915876
005924             88  IM-MEMO-OL-LOAN-TRLR-ADD              VALUE 'Y'. 9915876
005926         05  IM-MEMO-OL-SAV-TRLR-FLAG      PIC X.                 9915876
005928             88  IM-MEMO-OL-SAV-TRLR-ADD               VALUE 'Y'. 9915876
005930         05  IM-MEMO-OL-MRKT-TRLR-FLAG     PIC X.                 9915876
005932             88  IM-MEMO-OL-MRKT-TRLR-ADD              VALUE 'Y'. 9915876
005940         05  IM-MEMO-OL-AFXF-TRLR-IN       PIC S999    COMP-3.    0917821
005942         05  IM-MEMO-OL-STMT-TRLR-FLAG     PIC X.                 9915876
005944             88  IM-MEMO-OL-STMT-TRLR-ADD              VALUE 'Y'. 9915876
005946         05  IM-MEMO-OL-DC-TRLR-FLAG       PIC X.                 9915876
005948             88  IM-MEMO-OL-DC-TRLR-ADD                VALUE 'Y'. 9915876
005950         05  IM-MEMO-OL-DECD-TRLR-FLAG     PIC X.                 9915876
005952             88  IM-MEMO-OL-DECD-TRLR-ADD              VALUE 'Y'. 9915876
005954         05  IM-MEMO-OL-XFND-TRLR-FLAG     PIC X.                 9915876
005956             88  IM-MEMO-OL-XFND-TRLR-ADD              VALUE 'Y'. 9915876
005958         05  IM-MEMO-OL-EXSC-TRLR-FLAG     PIC X.                 9915876
005960             88  IM-MEMO-OL-EXSC-TRLR-ADD              VALUE 'Y'. 9915876
005962         05  IM-MEMO-OL-INFO-TRLR-FLAG     PIC X.                 9915876
005964             88  IM-MEMO-OL-INFO-TRLR-ADD              VALUE 'Y'. 9915876
005966         05  IM-MEMO-OL-LMTT-TRLR-FLAG     PIC X.                 9915876
005968             88  IM-MEMO-OL-LMTT-TRLR-ADD              VALUE 'Y'. 9915876
005970         05  IM-MEMO-OL-OD-TRLR-FLAG       PIC X.                 9915876
005972             88  IM-MEMO-OL-OD-TRLR-ADD                VALUE 'Y'. 9915876
005974         05  IM-MEMO-OL-NA2-TRLR-FLAG      PIC X.                 9915876
005976             88  IM-MEMO-OL-NA2-TRLR-ADD               VALUE 'Y'. 9915876
005978         05  IM-MEMO-OL-SSNA-TRLR-FLAG     PIC X.                 9915876
005980             88  IM-MEMO-OL-SSNA-TRLR-ADD              VALUE 'Y'. 9915876
005982         05  IM-MEMO-OL-TRGT-TRLR-FLAG     PIC X.                 9915876
005984             88  IM-MEMO-OL-TRGT-TRLR-ADD              VALUE 'Y'. 9915876
005986         05  IM-MEMO-OL-PLAN-TRLR-FLAG     PIC X.                 0617360
005988             88  IM-MEMO-OL-PLAN-TRLR-ADD              VALUE 'Y'. 0617360
006000         05  FILLER                        PIC X(10).             0917821
006040         05  IM-MEMO-OL-AFXF-NXTDAY-FLAGS.                        0917821
006045             07  IM-MEMO-OL-AFXF-NXTDAY-1  PIC X.                 0917821
006050             07  IM-MEMO-OL-AFXF-NXTDAY-2  PIC X.                 0917821
006055             07  IM-MEMO-OL-AFXF-NXTDAY-3  PIC X.                 0917821
006060             07  IM-MEMO-OL-AFXF-NXTDAY-4  PIC X.                 0917821
006065             07  IM-MEMO-OL-AFXF-NXTDAY-5  PIC X.                 0917821
006070             07  IM-MEMO-OL-AFXF-NXTDAY-6  PIC X.                 0917821
006075             07  IM-MEMO-OL-AFXF-NXTDAY-7  PIC X.                 0917821
006080             07  IM-MEMO-OL-AFXF-NXTDAY-8  PIC X.                 0917821
006085             07  IM-MEMO-OL-AFXF-NXTDAY-9  PIC X.                 0917821
006090         05  FILLER REDEFINES IM-MEMO-OL-AFXF-NXTDAY-FLAGS.       0917821
006095             07  IM-MEMO-OL-AFXF-NXTDAY    PIC X OCCURS 9 TIMES.  0917821
006100         05  IM-MEMO-OL-NEXT-ACCUM-SEQ     PIC S9(7)      COMP-3. 9915845
006200         05  IM-MEMO-OL-NEXT-STOP-SEQ      PIC S9(7)      COMP-3. 9915845
006300     03  IM-MEMO-OL-AMOUNTS.                                      9915845
006400         05  IM-MEMO-OL-AVAIL-BAL          PIC S9(13)V99  COMP-3.
006500         05  IM-MEMO-OL-CURRENT-BAL        PIC S9(13)V99  COMP-3.
006600         05  IM-MEMO-OL-LEDGER-BAL         PIC S9(13)V99  COMP-3.
006700         05  IM-MEMO-OL-BEGIN-HOLD-AMT     PIC S9(13)V99  COMP-3. 9915845
006800         05  IM-MEMO-OL-HOLD-AMT-TODAY     PIC S9(13)V99  COMP-3. 9915845
006900         05  IM-MEMO-OL-UNAVAILABLE-AMT    PIC S9(13)V99  COMP-3. 9915845
007000         05  IM-MEMO-OL-OD-AMT             PIC S9(13)V99  COMP-3. 9915845
007100         05  IM-MEMO-OL-MIN-BAL            PIC S9(13)V99  COMP-3. 9915845
007200         05  IM-MEMO-OL-SV-AVAIL-BAL       PIC S9(13)V99  COMP-3.
007300         05  IM-MEMO-OL-LN-AVAIL-BAL       PIC S9(13)V99  COMP-3. 9915845
007400         05  IM-MEMO-OL-PRIN-BAL           PIC S9(13)V99  COMP-3. 9915845
007500         05  IM-MEMO-OL-LST-CUST-DEP       PIC S9(13)V99  COMP-3.
007600         05  IM-MEMO-OL-OTHER-CHGS         PIC S9(13)V99  COMP-3. 9915845
007700         05  IM-MEMO-OL-INTRST-BAL         PIC S9(13)V99  COMP-3. 9915845
007800         05  IM-MEMO-OL-IOD-YTD-INT-PD     PIC S9(13)V99  COMP-3. 9915845
007900         05  IM-MEMO-OL-IOD-MTD-INT-PD     PIC S9(13)V99  COMP-3. 9915845
008000         05  IM-MEMO-OL-IOD-CYC-INT-PD     PIC S9(13)V99  COMP-3. 9915845
008100         05  IM-MEMO-OL-MTD-AGGR-BAL       PIC S9(15)V99  COMP-3. 9915845
008200         05  IM-MEMO-OL-INTEREST-RATE      PIC S9V9(8)    COMP-3. 9915845
008300         05  IM-MEMO-OL-INT-ACCRUED       PIC S9(11)V9(6) COMP-3. 9915845
008400         05  IM-MEMO-OL-AMT-LST-MON-TRAN   PIC S9(13)V99  COMP-3. 9915845
008420         05  IM-MEMO-OL-DDA-BAL            PIC S9(13)V99  COMP-3. 9915845
008440         05  IM-MEMO-OL-CASH-AVAIL-AMT     PIC S9(13)V99  COMP-3. 9915845
008460         05  IM-MEMO-OL-PM-CASH-AVAIL      PIC S9(13)V99  COMP-3. 9915845
008470         05  IM-MEMO-OL-SV-INT-ACCRUED    PIC S9(11)V9(6) COMP-3. 9915845
008480         05  IM-MEMO-OL-SV-YTD-INT-PD      PIC S9(13)V99  COMP-3. 9915845
008485         05  IM-MEMO-OL-XINV-BAL           PIC S9(13)V99  COMP-3. 2016639
008490         05  IM-MEMO-OL-WL-LOAN-AVAIL      PIC S9(13)V99  COMP-3. 0327005
008492         05  IM-MEMO-OL-XC-CARD-AVAIL      PIC S9(13)V99  COMP-3. 0617492
008494         05  IM-MEMO-OL-PROV-HOLD-AMT      PIC S9(13)V99  COMP-3. 0927850
008496         05  IM-MEMO-OL-REGDD-BAL          PIC S9(13)V99  COMP-3. 0930023
008700     03  IM-MEMO-OL-OTHER-INFO.
008900         05  IM-MEMO-OL-LOAN-TRLR          PIC X.
009000         05  IM-MEMO-OL-SAVINGS-TRLR       PIC X.
009100         05  IM-MEMO-OL-INT-STATUS         PIC X.
009200         05  IM-MEMO-OL-TRANSFER-PRIORITY  PIC X.
009300         05  IM-MEMO-OL-OFF-EMP            PIC X.
009400         05  IM-MEMO-OL-SECURED            PIC X.
009500         05  IM-MEMO-OL-LOST-CONTACT       PIC X.
009600         05  IM-MEMO-OL-STATUS             PIC XX.
009700         05  IM-MEMO-OL-OPEN-DATE          PIC X(6).
009710         05  IM-MEMO-OL-KITING-TRLR        PIC X.                 0417148
009720         05  IM-MEMO-OL-LOAN-STATUS        PIC X.                 1020116
009800         05  IM-MEMO-OL-ACCT-TYPE          PIC X(3).
009900         05  IM-MEMO-OL-SYSTEM-TYPE        PIC X(3).
009910         05  FILLER  REDEFINES IM-MEMO-OL-SYSTEM-TYPE.            IM007
009920             10  IM-MEMO-OL-SYS-TYPE-XX    PIC XX.                IM007
009930             10  FILLER                    PIC X.                 IM007
010000         05  IM-MEMO-OL-DATE-LAST-TRAN     PIC X(6).
010010         05  FILLER                        PIC X(2).              9915845
010100         05  IM-MEMO-OL-DT-LST-MON-TRAN    PIC X(6).
010105         05  IM-MEMO-OL-OD-REGE-OPT-CODE   PIC X.                 1020116
010110         05  FILLER                        PIC X.                 1020116
010200         05  IM-MEMO-OL-DT-LST-CUST-DEP    PIC X(6).
010210         05  FILLER                        PIC X(2).              9915845
010300         05  IM-MEMO-OL-GL-CODE            PIC 99.
010400         05  IM-MEMO-OL-IOD-RATE-PTR       PIC S999  COMP-3.      1003625
010500         05  IM-MEMO-OL-HIFI-INDICATOR     PIC S999  COMP-3.      1003625
010600         05  IM-MEMO-OL-ACH-FLAG           PIC X.
010700         05  IM-MEMO-OL-CHG-CARD-FLAG      PIC X.
010800         05  IM-MEMO-OL-BKUP-WTHLD-FLAG    PIC X.
010900         05  IM-MEMO-OL-SPEC-INSTR-FLAG    PIC X.
011000         05  IM-MEMO-OL-REQD-SIGNS         PIC X.
011100         05  IM-MEMO-OL-EXC-CODE           PIC X.
011200         05  IM-MEMO-OL-SHORT-NAME         PIC X(13).
011202         05  IM-MEMO-OL-STATE-LOCAL-TX-CD  PIC X.                 2012254
011210*        05  FILLER                        PIC X(16).             2012254
011212         05  IM-MEMO-OL-STAT-CHG-SEQ       PIC 99.                IMIB004
011214         05  IM-MEMO-OL-SC-TYPE            PIC X(3).              IMUP001
011216         05  IM-MEMO-TODAYS-CREDITS        PIC S9(13)V99 COMP-3.  IMUP001
011218         05  IM-MEMO-TODAYS-CREDITS-NO     PIC S9(5)     COMP-3.  IMUP001
011300         05  IM-MEMO-OL-TAX-NUMBER-DATA    PIC X(12).
011310         05  FILLER REDEFINES IM-MEMO-OL-TAX-NUMBER-DATA.         IM007
011320             07  IM-MEMO-OL-TAX-CODE       PIC X.                 IM007
011323             07  IM-MEMO-OL-TAX-NUMBER.                           IM008
011330                 09  IM-MEMO-OL-TAX-PREFIX PIC X.                 IM008
011340                 09  IM-MEMO-OL-TAX-NO     PIC X(9).              IM008
011350                 09  IM-MEMO-OL-TAX-SUFFIX PIC X.                 IM008
011355         05  IM-MEMO-OL-DAYS-OD            PIC S9(05)     COMP-3. IMUP001
011360*        05  FILLER                        PIC X(4).              9915845
011365         05  FILLER                        PIC X.                 IMUP001
011370         05  IM-MEMO-OL-CITIZEN-COUNTRY    PIC XX.                9915845
011380         05  IM-MEMO-OL-OL-CALC.                                  9916157
011382             07  IM-MEMO-OL-OL-CALC1       PIC X.                 9916157
011384             07  IM-MEMO-OL-OL-CALC2       PIC X.                 9916157
011400         05  IM-MEMO-OL-STOP-PAYS          PIC S9(7)      COMP-3.
011500         05  IM-MEMO-OL-MTD-AGGR-DAYS      PIC S9(3)      COMP-3.
011600         05  IM-MEMO-OL-YTD-TIMES-NSF      PIC S9(5)      COMP-3.
011700         05  IM-MEMO-OL-YTD-TIMES-OD       PIC S9(5)      COMP-3.
011800         05  IM-MEMO-OL-OD-LIMIT           PIC X.
011900         05  IM-MEMO-OL-NSF-CALC           PIC X.
012000         05  IM-MEMO-OL-BCR-CTL-LEV        PIC X.
012100         05  IM-MEMO-OL-DC-FILLER          PIC X(50).             9915845
012120         05  IM-MEMO-EFA-NEW-ACCT-FLAG     PIC X.                 IM007
012140         05  IM-MEMO-EFA-COLLECTN-FLAG     PIC X.                 IM007
012160         05  IM-MEMO-EFA-STATUS-FLAG       PIC X.                 IM007
012180         05  IM-MEMO-EFA-LARGE-DEP-AMT     PIC S9(13)V99  COMP-3. 9915845
012183         05  IM-MEMO-OL-EXEMPT-RSN         PIC X.                 IM008
012187         05  IM-MEMO-OL-TIN-CERT           PIC X.                 IM008
012188         05  IM-MEMO-OL-ALL-FUNDS-FLAG     PIC X.                 0902008
012189         05  IM-MEMO-OL-CUST-CALC-CODE     PIC X.                 0902008
012200         05  IM-MEMO-OL-MMDA-INDICATOR     PIC X.                 1003929
012210         05  IM-MEMO-OL-WL-LOAN-EXP-DATE.                         0327005
012220             07  IM-MEMO-OL-WL-EXP-CC      PIC XX.                0327005
012230             07  IM-MEMO-OL-WL-EXP-DATE-6.                        0327005
012240                 09  IM-MEMO-OL-WL-EXP-YY  PIC XX.                0327005
012250                 09  IM-MEMO-OL-WL-EXP-MM  PIC XX.                0327005
012260                 09  IM-MEMO-OL-WL-EXP-DD  PIC XX.                0327005
012500*        05  FILLER                        PIC X(10).             0327005
012502         05  IM-MEMO-OL-TARGET-AMOUNT      PIC S9(13)V99  COMP-3. IMUP001
012504         05  IM-MEMO-OL-TIP-EXP            PIC X(02).             GG0374
012510     03  IM-MEMO-OL-EXTERNAL-INVEST.                              2016639
012520         05  IM-MEMO-OL-XINV-LINK-IND      PIC X.                 2016639
012530         05  IM-MEMO-OL-XINV-PROC-IND      PIC X.                 2016639
013400     03  IM-MEMO-OL-LANGUAGE-INFO.                                9915845
013500         05  IM-MEMO-OL-CUST-LANGUAGE      PIC XX.                9915845
013600         05  IM-MEMO-OL-CUST-LANG-GROUP    PIC X.                 9915845
013700     03  IM-MEMO-OL-CURRENCY-INFO.                                9915845
013800         05  IM-MEMO-OL-CURR-CODE          PIC XXX.               9915845
013900         05  IM-MEMO-OL-CURR-DEC           PIC X.                 9915845
013910         05  FILLER                        PIC X(4).              9915845
014000*    03  FILLER                            PIC X(12).             9915845
014010     03  IM-MEMO-OL-DT-LST-CUS-ACT.                               IMUP001
014020         05  IM-MEMO-OL-LST-CUS-ACT-MO     PIC XX.                IMUP001
014030         05  IM-MEMO-OL-LST-CUS-ACT-DA     PIC XX.                IMUP001
014040         05  IM-MEMO-OL-LST-CUS-ACT-YR     PIC XX.                IMUP001
014050     03  IM-MEMO-OL-D-L-CUST-WITHDRAWAL.                          IMUP001
014060         05  IM-MEMO-OL-MO-L-CUST-WDRL     PIC XX.                IMUP001
014070         05  IM-MEMO-OL-DA-L-CUST-WDRL     PIC XX.                IMUP001
014080         05  IM-MEMO-OL-YR-L-CUST-WDRL     PIC XX.                IMUP001
014100     03  IM-MEMO-OL-NB-FLAGS.                                     9715504
014200         05  IM-MEMO-NB-SPEC-HANDL-FLAG    PIC X(1).              9715504
014300             88  IM-MEMO-NB-SPEC-HANDL-TRAN            VALUE 'N'. 9715504
014400         05  IM-MEMO-NB-SPEC-HANDL2-FLAG   PIC X(1).              9715504
014500             88  IM-MEMO-NB-SPEC-HANDL2-TRAN           VALUE 'N'. 9715504
014600         05  IM-MEMO-NB-MAIL-RTN-CNT-FLAG  PIC X(1).              9715504
014700             88  IM-MEMO-NB-MAIL-RTN-TRAN              VALUE 'N'. 9715504
014800     03  IM-MEMO-NB-INFO.                                         9715504
014900         05  IM-MEMO-NB-STMT-SPEC-HANDL    PIC X(1).              9715504
015000         05  IM-MEMO-NB-MAIL-RTN-CNT       PIC S9(5)      COMP-3. 9715504
015100         05  IM-MEMO-NB-DATE-LAST-CONTACT.                        9715504
015200             07  IM-MEMO-NB-CONTACT-MO     PIC X(2).              9715504
015300             07  IM-MEMO-NB-CONTACT-DA     PIC X(2).              9715504
015400             07  IM-MEMO-NB-CONTACT-YR     PIC X(2).              9715504
015410         05  FILLER                        PIC X(2).              9915845
015500         05  IM-MEMO-NB-ESCHEAT-FLAG       PIC X(1).              9715504
015600         05  IM-MEMO-NB-TOT-DMT-SC         PIC S9(13)V99  COMP-3. 9915845
015700         05  IM-MEMO-NB-REASON-CLOSED      PIC X(2).              9715504
015800         05  IM-MEMO-NB-HOME-PHONE.                               9715504
015900             07  IM-MEMO-NB-HOME-AREA-CODE PIC X(3).              9715504
016000             07  IM-MEMO-NB-PHONE-NBR      PIC X(7).              9715504
016010             07  IM-MEMO-NB-HOME-EXT       PIC X(4).              9915845
016020         05  FILLER                        PIC X(8).              9915845
016100         05  IM-MEMO-NB-BUS-PHONE-NUMBER.                         9715504
016200             07  IM-MEMO-NB-BUS-AREA-CODE  PIC X(3).              9715504
016300             07  IM-MEMO-NB-BUS-PHONE-NBR  PIC X(7).              9715504
016400             07  IM-MEMO-NB-BUS-PHONE-EXT  PIC X(4).              9715504
016410         05  FILLER                        PIC X(8).              9915845
016500         05  IM-MEMO-NB-SC-OD-TIMES        PIC S9(3)      COMP-3. 9715504
016600         05  IM-MEMO-NB-MTD-NSF-TIMES      PIC S9(3)      COMP-3. 9715504
016700         05  IM-MEMO-NB-DAYS-OD-MO-1       PIC S9(3)      COMP-3. 9715504
016800         05  IM-MEMO-NB-TIMES-NSF-MO-1     PIC S9(3)      COMP-3. 9715504
016900         05  IM-MEMO-NB-TIMES-CB-MO-1      PIC S9(5)      COMP-3. 9715504
017000         05  IM-MEMO-NB-UNLIM-TEL-HLD      PIC X(1).              9715504
017100         05  IM-MEMO-NB-AUDIT              PIC X(1).              9715504
017200         05  IM-MEMO-NB-REGD-NTC-NBR       PIC 9(01).             9715504
017300         05  IM-MEMO-NB-REGD-TR-VIOL       PIC S9(3)      COMP-3. 9715504
017400         05  IM-MEMO-NB-REGD-CK-VIOL       PIC S9(3)      COMP-3. 9715504
017500         05  IM-MEMO-NB-REGD-DT1.                                 9715504
017600             07  IM-MEMO-NB-REGD-MM1       PIC X(2).              9715504
017700             07  IM-MEMO-NB-REGD-DD1       PIC X(2).              9715504
017800             07  IM-MEMO-NB-REGD-YY1       PIC X(2).              9715504
017810         05  FILLER                        PIC X(2).              9915845
017900         05  IM-MEMO-NB-REGD-DT2.                                 9715504
018000             07  IM-MEMO-NB-REGD-MM2       PIC X(2).              9715504
018100             07  IM-MEMO-NB-REGD-DD2       PIC X(2).              9715504
018200             07  IM-MEMO-NB-REGD-YY2       PIC X(2).              9715504
018210         05  FILLER                        PIC X(2).              9915845
018300         05  IM-MEMO-NB-REGD-DT3.                                 9715504
018400             07  IM-MEMO-NB-REGD-MM3       PIC X(2).              9715504
018500             07  IM-MEMO-NB-REGD-DD3       PIC X(2).              9715504
018600             07  IM-MEMO-NB-REGD-YY3       PIC X(2).              9715504
018610         05  FILLER                        PIC X(2).              9915845
018700         05  IM-MEMO-NB-REGD-CVT-DT.                              9715504
018800             07  IM-MEMO-NB-REGD-CVT-MM    PIC X(2).              9715504
018900             07  IM-MEMO-NB-REGD-CVT-DD    PIC X(2).              9715504
019000             07  IM-MEMO-NB-REGD-CVT-YY    PIC X(2).              9715504
019010         05  FILLER                        PIC X(2).              9915845
019100         05  IM-MEMO-NB-SIGNATURE-CARD     PIC X(1).              9715504
019200         05  IM-MEMO-NB-NON-TAXABLE        PIC X(1).              9715504
019300         05  IM-MEMO-NB-TAX-EXEMPT-RSN     PIC X(1).              9715504
019400         05  IM-MEMO-NB-NRA-CERT-NAME1     PIC X(1).              9715504
019500         05  IM-MEMO-NB-NRA-CERT-NAME2     PIC X(1).              9715504
019600         05  IM-MEMO-NB-NRA-TAX-CNTRY      PIC X(2).              9715504
019700         05  IM-MEMO-NB-LIST-POST          PIC X(1).              9715504
019800         05  IM-MEMO-NB-FUNDING-FLAG       PIC X(1).              9715504
019900         05  IM-MEMO-NB-MIN-FND-BAL        PIC S9(13)V99  COMP-3. 9915845
020000         05  IM-MEMO-NB-MTD-ANALYSIS       PIC X(1).              9715504
020100         05  IM-MEMO-NB-MIN-SWP-AMT        PIC S9(13)V99  COMP-3. 9915845
020200         05  IM-MEMO-NB-TIS-CONSUMER       PIC X(1).              9715504
020300         05  IM-MEMO-NB-LOC-NBR            PIC X(10).             9715504
020400         05  IM-MEMO-NB-ZB-LVL-NBR         PIC X(1).              9715504
020500         05  IM-MEMO-NB-ZB-REL-NBR         PIC X(6).              9715504
020600         05  IM-MEMO-NB-CLS-OVRRIDE        PIC X(1).              9715504
020700         05  IM-MEMO-NB-NA-LINE            PIC X(42)              9715504
020800                                                 OCCURS 2 TIMES.  9715504
020900         05  IM-MEMO-NB-INFO-COUNTRY       PIC X(5).              9715504
021000         05  IM-MEMO-NB-ARP-CODE           PIC X(1).              9715504
021100         05  IM-MEMO-NB-DC-CDA             PIC X(1).              9715504
021200         05  IM-MEMO-NB-STMT-SPEC-HANDL-2  PIC X(1).              9715504
021300         05  IM-MEMO-NB-GEO-CODE           PIC X(3).              9715504
021305         05  IM-MEMO-IT-TRAN-FLAGS.                               9715504
021310             07  IM-MEMO-IT-TRAN-FLAG1     PIC X.                 9715504
021315             07  IM-MEMO-IT-TRAN-FLAG2     PIC X.                 9715504
021320             07  IM-MEMO-IT-TRAN-FLAG3     PIC X.                 9715504
021325             07  IM-MEMO-IT-TRAN-FLAG4     PIC X.                 9715504
021330             07  IM-MEMO-IT-TRAN-FLAG5     PIC X.                 9715504
021335             07  IM-MEMO-IT-TRAN-FLAG6     PIC X.                 9715504
021340             07  IM-MEMO-IT-TRAN-FLAG7     PIC X.                 9715504
021345             07  IM-MEMO-IT-TRAN-FLAG8     PIC X.                 9715504
021350             07  IM-MEMO-IT-TRAN-FLAG9     PIC X.                 9715504
021355             07  IM-MEMO-IT-TRAN-FLAG10    PIC X.                 9715504
021360         05  FILLER                        PIC X(30).             9915845
021400     03  IM-MEMO-PASSBOOK-INFO.                                   9915845
021410         05  IM-MEMO-PASSBOOK-ACCT         PIC X.                 9915845
021420         05  IM-MEMO-PASSBOOK-BALANCE      PIC S9(13)V99 COMP-3.  9915845
021430         05  IM-MEMO-PASSBOOK-DATE.                               9915845
021440             07  IM-MEMO-PBD-YEAR.                                9915845
021450                 09  IM-MEMO-PBD-CENT      PIC XX.                9915845
021460                 09  IM-MEMO-PBD-YR        PIC XX.                9915845
021470             07  IM-MEMO-PBD-MO            PIC XX.                9915845
021480             07  IM-MEMO-PBD-DA            PIC XX.                9915845
021490         05  FILLER REDEFINES IM-MEMO-PASSBOOK-DATE.              9915845
021500             07  FILLER                    PIC XX.                9915845
021510             07  IM-MEMO-PB-DATE           PIC X(6).              9915845
021520     03  IM-MEMO-GL-COST-CENTER            PIC X(30).             0517228
021550     03  IM-MEMO-PLAN-INFO.                                       0617360
021552         05  IM-MEMO-OL-PLN-STATUS-CD      PIC X.                 0617360
021554         05  IM-MEMO-OL-PLN-PART-EXC-FLG   PIC X.                 0617360
021556         05  IM-MEMO-OL-PLN-VALID-CONTR    PIC X.                 0617360
021558         05  IM-MEMO-OL-PLN-TYPE           PIC XX.                0617360
021560         05  IM-MEMO-OL-PLN-PLAN-KEY       PIC X(27).             0617360
021562         05  IM-MEMO-OL-PLN-MAX-CNT-LIM    PIC S9(7)V99 COMP-3.   0617360
021564         05  IM-MEMO-OL-PLN-PY-MAX-CNT     PIC S9(7)V99 COMP-3.   0617360
021566         05  IM-MEMO-OL-PLN-TRLR-DEATH-YR  PIC X(4).              0617360
021568         05  IM-MEMO-OL-XRF-P02-SEQ        PIC S9(5) COMP-3.      0617360
021600     03  FILLER                            PIC X(21).             0617360
040000     03  IM-MEMO-OL-INTL-FILLER            PIC X(100).            9915845
040010     03  IM-MEMO-OL-INTBK-FIELDS REDEFINES IM-MEMO-OL-INTL-FILLER.IMIB86
040020         05  IM-MEMO-OL-USER-CODE-AREA.                           IMIB86
040030             07  IM-MEMO-OL-USER-CD-01     PIC X.                 IMIB86
040040             07  IM-MEMO-OL-USER-CD-02     PIC X.                 IMIB86
040050             07  IM-MEMO-OL-USER-CD-03     PIC X.                 IMIB86
040060             07  IM-MEMO-OL-USER-CD-04     PIC X.                 IMIB86
040070             07  IM-MEMO-OL-USER-CD-05     PIC X.                 IMIB86
040080             07  IM-MEMO-OL-USER-CD-06     PIC X.                 IMIB86
040090             07  IM-MEMO-OL-USER-CD-07     PIC X.                 IMIB86
040100             07  IM-MEMO-OL-USER-CD-08     PIC X.                 IMIB86
040110             07  IM-MEMO-OL-USER-CD-09     PIC X.                 IMIB86
040120             07  IM-MEMO-OL-USER-CD-10     PIC X.                 IMIB86
040130             07  IM-MEMO-OL-USER-CD-11     PIC X.                 IMIB86
040140             07  IM-MEMO-OL-USER-CD-12     PIC X.                 IMIB86
040150             07  IM-MEMO-OL-USER-CD-13     PIC X.                 IMIB86
040160             07  IM-MEMO-OL-USER-CD-14     PIC X.                 IMIB86
040170             07  IM-MEMO-OL-USER-CD-15     PIC X.                 IMIB86
040180             07  IM-MEMO-OL-USER-CD-16     PIC X.                 IMIB86
040190             07  IM-MEMO-OL-USER-CD-17     PIC X.                 IMIB86
040200             07  IM-MEMO-OL-USER-CD-18     PIC X.                 IMIB86
040210         05  IM-MEMO-OL-KEY-ALT.                                  IMIB79
040220             07  IM-MEMO-OL-CUSTOMER-NUMBER  PIC X(12).           IMIB79
040230             07  IM-MEMO-OL-CONTROL-KEY-ALT.                      IMUP001
040240                 09  IM-MEMO-OL-CTL-1-ALT  PIC XX.                IMUP001
040250                 09  IM-MEMO-OL-CTL-2-ALT  PIC XXX.               IMUP001
040260                 09  IM-MEMO-OL-CTL-3-ALT  PIC XXX.               IMUP001
040270                 09  IM-MEMO-OL-CTL-4-ALT  PIC X(4).              IMUP001
040280                 09  IM-MEMO-OL-ACCT-NO-ALT PIC X(10).            IMUP001
040290         05  IM-MEMO-OL-OFFICER            PIC X(05).             IMIB86
040300         05  IM-MEMO-OL-SIMB010            PIC X.                 IMIB86
040310         05  IM-MEMO-OL-IND-DPE            PIC X(01).             IMUP001
040320         05  IM-MEMO-OL-OD-CYC-ACR         PIC S9(11)V9(6) COMP-3.IMUP001
040330         05  IM-MEMO-OL-RETENCION-MEGA     PIC S9(13)V99   COMP-3.IMIB74
040340         05  IM-MEMO-OL-RETENC-TRAMITE     PIC S9(13)V99   COMP-3.FO2407
040350         05  IM-MEMO-TODAYS-DEBITS         PIC S9(13)V99   COMP-3.IMUP001
040360         05  IM-MEMO-TODAYS-DEBITS-NO      PIC S9(5)       COMP-3.IMUP001
040370         05  IM-MEMO-OL-STP-NUM            PIC S9(3)       COMP-3.IMUP001
040380         05  FILLER                        PIC X(3).              IMUP001
060000   02  FILLER REDEFINES IM-MEMO-OL-RECORD.                        9915845
070000     03  IM-MEMO-OL-BYTES                 PIC X OCCURS 1200 TIMES.9915845
