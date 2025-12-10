*     * FO5238*12/05/11 JCTE PROYECTO UPGRADE SYSTEMAT
000100*
000200*---------------------------------------------------------------*
000300*     COPYBOOK FOR TAX CALC FIELDS                              *
000400*---------------------------------------------------------------*
000500*
000600 01  TAX-CALC-WORK-FIELDS.
000700     03  COMM-AREA-TXCALC-PHS-LEN PIC 9(5)    COMP-3 VALUE 47.
000800     03  TAX-WORK-FIELDS.
000900         05  TAX-CALC-REASON      PIC X.
001000         05  TAX-CALC-IOD-FLAG    PIC X.
001100         05  TAX-CALC-DAYS        PIC S999          COMP-3.
001200         05  TAX-CALC-PAYMENT     PIC S9(13)V99     COMP-3.
001300         05  TAX-CALC-AMOUNT      PIC S9(13)V99     COMP-3.
001400         05  TAX-CALC-AMOUNT-ST   PIC S9(13)V99     COMP-3.
001500         05  TAX-CALC-AMOUNT-LOC  PIC S9(13)V99     COMP-3.
001600     03  IMACT-ADDR               PIC S9(8)   COMP.
001700     03  TSBCR-ADDR               PIC S9(8)   COMP.
001800     SKIP3
001900 02  WORK-TAX-FIELDS-OTHER.
002000     03  BIN-9                    PIC S9999   COMP  VALUE +9.
002100     03  Y                        PIC S9999   COMP  VALUE +0.
002200     03  WORK-TAX-RATES.
002300         05  WORK-TAX-RATE-KEY-LEVEL  PIC X.
002400         05  WORK-TAX-CUR-DATA.
002500             07  WORK-TAX-CUR-DATE    PIC X(8).
002600             07  WORK-TAX-CUR-ANN     PIC S9V9(8)   COMP-3
002700                                      OCCURS 9 TIMES.
002800             07  WORK-TAX-CUR-LMT     PIC S9(13)V99 COMP-3
002900                                      OCCURS 8 TIMES.
003000     03  WORK-TAX-RATE            PIC S9V9(8) COMP-3  VALUE ZEROS.
