*     * FO5238*12/05/11 JCTE PROYECTO UPGRADE SYSTEMAT
000100*----------------------------------------------------------------*
000200*   GENERAL LEDGER EXTRACT RECORD                                *
000300*   WRITTEN BY IM31 TO INTERFACE WITH SA GENERAL LEDGER SYSTEM   *
000400*   INPUT TO SA71.                                               *
000500*----------------------------------------------------------------*
000600*
000700  01  GL-EXTRACT-RECORD.
000800      03  GL-REC-LENGTH            PIC S9(4)         COMP.
000900      03  GL-BIN-0                 PIC XX.
001000      03  GL-REC-LNGTH             PIC S9(4)           COMP.
001100      03  GL-APPL-KEY.
001200          05  GL-APPL-ID           PIC XX.
001300          05  GL-APPL-CTL1         PIC XX.
001400          05  GL-APPL-GL-KEY       PIC X(30).
001500          05  GL-APPL-EFF-DATE.                                   0266688
001510              07  GL-APPL-EFF-CENT PIC XX.                        0266688
001512              07  GL-APPL-EFF-YR   PIC XX.                        0266688
001514              07  GL-APPL-EFF-MO   PIC XX.                        0266688
001516              07  GL-APPL-EFF-DA   PIC XX.                        0266688
001600      03  GL-REC-KEY               PIC X.
001700      03  GL-ORIG-ACCT             PIC X(31).
001800      03  GL-CURR-NO               PIC S999            COMP-3.
001810      03  GL-CURRENCY.                                            9915845
001820          05  GL-CURR-CD           PIC X(3).                      9915845
001830          05  GL-CURR-DEC          PIC X.                         9915845
001900      03  FILLER                   PIC X(18).                     9915845
002000      03  GL-APPL-DATA             PIC X(1150).                   9915845
002100      03  GL-APPL-RECAP-ITEM   REDEFINES GL-APPL-DATA.
002200          05  GP-RECAP-ITEM    OCCURS 50 TIMES
002300                               INDEXED BY RECAP-IND.
002400              07  GL-RECAP-CODE    PIC X(8).
002500              07  GL-RECAP-ACCT    PIC X(5).
002600              07  GL-RECAP-AMT     PIC S9(15)V99       COMP-3.    9915845
002700              07  FILLER       REDEFINES GL-RECAP-AMT.
002800                  09  FILLER       PIC X(4).                      9915845
002900                  09  GL-RECAP-CNT PIC S9(9)           COMP-3.    9915845
003000*----------------------------------------------------------------*
