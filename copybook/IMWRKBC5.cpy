*     * FO5238*12/05/11 JCTE PROYECTO UPGRADE SYSTEMAT
000100*----------------------------------------------------------------*0902970
000200*              IMPACS WORK BCR RECORD 5                          *0902970
000300*----------------------------------------------------------------*0902970
003100 01  DDA-WRKBCR-5.                                                IM006
003200     03  WBC5-CONTROL-KEY.                                        IM006
003300         05  WBC5-CONTROL-1  PIC XX.                              IM006
003400         05  WBC5-CONTROL-2  PIC XXX.                             IM006
003500         05  WBC5-CONTROL-3  PIC XXX.                             IM006
003600     03  WBC5-RECORD-ID      PIC XX.                              IM006
003700     03  FILLER              PIC X(10).                           9915845
003800     03  WBC-SORT-DEFAULT.                                        IM006
003900         05  WBC-DEF-PRIORITY    PIC X.                           IM006
004000         05  WBC-DEF-FORM        PIC X.                           IM006
004100         05  WBC-DEF-SORT1       PIC X.                           IM006
004200         05  WBC-DEF-SORT2       PIC X.                           IM006
004300         05  WBC-DEF-SORT3       PIC X.                           IM006
004400     03  WBC5-REPT-TABLE OCCURS 99 TIMES                          IM006
004500             INDEXED BY WBC5-REPT-INDEX.                          IM006
004600         05  WBC-RPT-PRIORITY    PIC X.                           IM006
004700         05  WBC-RPT-FORM        PIC X.                           IM006
004800         05  WBC-RPT-SORT1       PIC X.                           IM006
004900         05  WBC-RPT-SORT2       PIC X.                           IM006
005000         05  WBC-RPT-SORT3   PIC X.                               IM006
005010     03  FILLER              PIC X(500).                          9915845
005100     03  WBC-MAJOR-SORT-TABLE    OCCURS 5 TIMES.                  IM006
005200         05  WBC-DIST-CODE       PIC X.                           IM006
005300         05  WBC-MAJOR-FLD-NOS.                                   IM006
005400           06  FILLER OCCURS 4 TIMES.                             IM006
005500             07  WBC-MJR-FLD-NO  PIC S999  COMP-3.                IM006
005600             07  WBC-MJR-A-D     PIC X.                           IM006
005700     03  WBC-INTER-SORT-TABLE    OCCURS 9 TIMES.                  IM006
005800         05  WBC-INTER-FIELD-NOS OCCURS 3 TIMES.                  IM006
005900             07  WBC-INTER-FLD-NO PIC S999 COMP-3.                IM006
006000             07  WBC-INTER-A-D   PIC X.                           IM006
006100     03  WBC-MINOR-SORT-TABLE    OCCURS 9 TIMES.                  IM006
006200         05  WBC-MINOR-FLD-NOS   OCCURS 3 TIMES.                  IM006
006300             07  WBC-MNR-FLD-NO  PIC S999  COMP-3.                IM006
006400             07  WBC-MNR-A-D     PIC X.                           IM006
006500     03  WBC-USER-SORT-PHASE     PIC XXXX.                        IM006
006600     03  WBC-ZEROBAL-DR          PIC XXXX.                        IM006
006700     03  WBC-ZEROBAL-CR          PIC XXXX.                        IM006
006800     03  WBC-TRANSFER-DR         PIC XXXX.                        IM006
006900     03  WBC-TRANSFER-CR         PIC XXXX.                        IM006
007000     03  FILLER                  PIC X(16).                       IM006
007400     03  WBC-TRAN-MERGE-PRINT.                                    IM006
007500         05  FILLER                  PIC X.                       IM008
007600         05  WBC-TRAN-MERGE-PHASE    PIC XXXX.                    IM006
007700         05  FILLER                  PIC X.                       IM008
007800     03  WBC-LIST-POST.                                           IM006
007900         05  WBC-LIST-REPT-NO        PIC XX.                      IM008
008000         05  FILLER                  PIC X.                       IM008
008100         05  WBC-LIST-POST-FLAG      PIC X.                       IM006
008200         05  WBC-LIST-SORT-FLDS  OCCURS 9 TIMES  COMP-3.          IM006
008300             07  WBC-LIST-FIELD  PIC S9.                          IM006
008400     03  WBC-SYSTEM-TOTAL-FLAG   PIC X.                           IM006
008500     03  FILLER                  PIC XX.                          IM008
008700     03  WBC-ZERO-BAL-ACCT       PIC X.                           IM006
008800     03  FILLER                  PIC X(4).                        0902970
008900     03  WBC-STOP-HOLD-CHG-FLAG  PIC X.                           IM006
009000     03  WBC-IM28-STP-HLD-PHASE  PIC X(4).                        IM006
009100     03  WBC-STOP-HOLD-DR        PIC X(4).                        IM006
009200     03  WBC-TC31-CHG            PIC S9(13)V99   COMP-3.          9915845
009300     03  WBC-TC32-CHG            PIC S9(13)V99   COMP-3.          9915845
009400     03  WBC-TC33-CHG            PIC S9(13)V99   COMP-3.          9915845
009500     03  WBC-TC34-CHG            PIC S9(13)V99   COMP-3.          9915845
009600     03  WBC-TC35-CHG            PIC S9(13)V99   COMP-3.          9915845
009700     03  WBC-SH-HIT-CHG          PIC S9(13)V99   COMP-3.          9915845
009800     03  WBC-SH-MAX-CHG          PIC S9(13)V99   COMP-3.          9915845
009900     03  WBC-SH-CHARGE           PIC S9(13)V99   COMP-3.          9915845
010000     03  WBC-KITING-SUSP         PIC X.                           IM006
010100     03  FILLER                  PIC XX.                          IM008
010200     03  WBC-KITING-RATIOS.                                       IM008
010300         05  WBC-KITE-CR-OVR-DBS     PIC S9V99       COMP-3.      IM006
010400         05  WBC-KITE-DEP-TURN       PIC S9V99       COMP-3.      IM006
010500         05  WBC-KITE-DR-TO-UNCOLL   PIC S9V99       COMP-3.      IM006
010600     03  WBC-KITE-SUSP-PHASE         PIC X(4).                    IM006
010700     03  WBC-FUNDS-AVAILABILTY-DATA.                              IM007
010800         05  WBC-EFA-EMERGENCY-FLAG          PIC X.               IM007
010900         05  WBC-EFA-HISTORY-FLAG            PIC X.               IM007
011000         05  WBC-EFA-NEXT-CASH-TABLE.                             IM007
011100             07  WBC-EFA-NEXT-CASH-LIMITS    PIC S9(13)V99 COMP-3 9915845
011200                 OCCURS 9 TIMES.                                  IM007
011300         05  WBC-EFA-SPLT-CASH-TABLE.                             IM007
011400             07  WBC-EFA-SPLT-CASH-LIMITS    PIC S9(13)V99 COMP-3 9915845
011500                 OCCURS 9 TIMES.                                  IM007
011600         05  WBC-EFA-EXCEPTION-DAYS          COMP-3.              IM007
011700             07  WBC-EFA-NEW-ACCT-DAYS       PIC S9.              IM007
011800             07  WBC-EFA-NA-LARGE-DAYS       PIC S9.              IM007
011900             07  WBC-EFA-LARGE-DP-DAYS       PIC S9.              IM007
012000             07  WBC-EFA-REDEP-CK-DAYS       PIC S9.              IM007
012100             07  WBC-EFA-RPEAT-OD-DAYS       PIC S9.              IM007
012200             07  WBC-EFA-COLLECTN-DAYS       PIC S9.              IM007
012300             07  WBC-EFA-EMERGNCY-DAYS       PIC S9.              IM007
012400             07  WBC-EFA-LOW-RISK-DAYS       PIC S9.              IM007
012500         05  WBC-EFA-PHASE                   PIC X(4).            IM007
012600         05  WBC-EFA-TIME-OF-DAY.                                 IM007
012700             07  WBC-EFA-TIME-HH             PIC XX.              IM007
012800             07  WBC-EFA-TIME-MM             PIC XX.              IM007
012900             07  WBC-EFA-TIME-SS             PIC XX.              IM007
012910     03  WBC-TC36-CHG            PIC S9(13)V99   COMP-3.          2012992
012920     03  WBC-TC38-CHG            PIC S9(13)V99   COMP-3.          0266778
012930     03  WBC-TC38-CHG-OPT1       PIC X.                           0266778
012932     03  WBC-TC38-CHG-OPT2       PIC X.                           0266778
012934     03  WBC-TC38-CHG-OPT3       PIC X.                           0266778
012936     03  WBC-TC38-CHG-OPT4       PIC X.                           0266778
012938     03  WBC-TC38-CHG-OPT5       PIC X.                           0266778
012940     03  WBC-TC38-CHG-OPT6       PIC X.                           0266778
013000     03  FILLER                              PIC X(118).          0266778
