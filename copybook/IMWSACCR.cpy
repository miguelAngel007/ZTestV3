*     * FO5238*12/05/11 JCTE PROYECTO UPGRADE SYSTEMAT
000100*----------------------------------------------------------------*
000200*    INTEREST ACCRUAL PARAMETERS                                 *
000300*----------------------------------------------------------------*
000400 01  INTEREST-ACCRUAL-PARAMETERS.
000500     03  FILLER                      COMP-3.
000600         05  ACCRUAL-ANNUAL-RATE     PIC S9V9(8).                 9915845
000700         05  ACCRUAL-DAILY-RATE      PIC SVP9(15).                9915845
000800         05  ACCRUAL-DAYS            PIC S999.
000900         05  ACCRUAL-BALANCE         PIC S9(13)V99.
001000         05  ACCRUAL-AMOUNT          PIC S9(11)V9(6) VALUE +0.    9915845
001100         05  ACCRUED-TO-DATE         PIC S9(11)V9(6).             9915845
001200*----------------------------------------------------------------*
001300*    INTEREST-ADJUST-CODE -                                      *
001400*        1 = NORMAL DAILY ACCRUAL                                *
001500*        2 = ADJUSTING INTEREST FOR A BACK-DATED TRANSACTION     *
001600*        3 = ADJUSTING INTEREST FOR PAST ACCRUALS USING PREVIOUS *
001700*            RATE                                                *
001800*        4 = ADJUSTING INTEREST FOR BACKDATING AN ENTIRE RATE    *
001900*            CLASSIFICATION (BACK OUT PREVIOUS RATE AND REACCRUE *
002000*            AT CURRENT RATE)                                    *
002100*        5 = ADJUSTING INTEREST FOR CORRECTING AND DELETING RATES*
002200*            WHEN THE CURRENT RATE IS OLDER THAN THE DELETED RATE*
002300*            (BACK OUT DELETED RATE AND PREVIOUS RATE AND        *
002400*            REACCRUE AT THE CURRENT RATE)                       *
002500*        6 = ADJUSTING INTEREST FOR DELETING AND CORRECTING RATES*
002600*            WHEN THE DELETED RATE IS OLDER THAN THE CURRENT RATE*
002700*            (BACK OUT DELETED RATE AND REACCRUE AT PREVIOUS RATE*
002800*            AND CURRENT RATE)                                   *
002900*        7 = DETERMINES ELIGIBILITY FOR HIFI AND SUPER HIFI      *
003000*            ACCOUNTS WITH ACCRUAL BALANCES LESS THAN OR EQUAL   *
003100*            TO ZERO                                             *
003200*----------------------------------------------------------------*
003300     03  INTEREST-ADJUST-CODE        PIC X.
003400     03  HIFI-INEL-FLAG              PIC X.
003500     03  LEAP-YEAR-FLAG              PIC X.
003600     03  PAST-ACCR                   PIC X.
003700     03  OD-ACR-PAY-FLAG             PIC X           VALUE '0'.
003800     03  YEAR-CHANGE                 PIC X           VALUE '0'.
003900     03  NEXT-DAYS                   PIC S9(3) VALUE +0 COMP-3.
004000     03  ACCR-NO-HOLD                PIC S999        COMP-3.
004100     03  ACCR-INT-TRAN-NO            PIC S9 VALUE +0 COMP-3.
004200*----------------------------------------------------------------*
004300*  INTEREST-ADJ-SUBCODE VALUES:                                  *
004400*    N = NORMAL TRANSACTION                                      *
004500*    P = PROJECTION ACCRUALS                                     *
004600*    T = TRANSACTION BACKDATE WITH BALANCE HISTORY FILE          *
004700*    I = INITIALIZE BALANCE HISTORY FOR BACKDATED NEW ACCOUNT    *
004800*    F = BANK UNAVAILABLE ADJUSTMENT WITH BALANCE HISTORY (FLOAT)*
004900*    U = CUSTOMER UNAVAILABLE ADJUSTMENT WITH BALANCE HISTORY    *
005000*----------------------------------------------------------------*
005100     03  INTEREST-ADJ-SUBCODE        PIC X.
005200     03  ACCRUAL-DATE.
005300         05  ACCR-CC                 PIC XX.
005400         05  ACCR-YR                 PIC XX.
005500         05  ACCR-MO                 PIC XX.
005600         05  ACCR-DA                 PIC XX.
005700*----------------------------------------------------------------*
005800*    INVESTMENT ACCRUAL PARAMETERS                               *
005900*----------------------------------------------------------------*
006000     03  FILLER                      COMP-3.
006100         05  ACCRUAL-SVC-RT          PIC S9V9(8)     VALUE +0.    9915845
006200         05  ACCRUAL-SVC-DAF         PIC SVP9(15)    VALUE +0.    9915845
006300         05  ACCRUAL-SVC-AMT         PIC S9(11)V9(6) VALUE +0.    9915845
006400         05  ACCRUED-SVC-ACCR        PIC S9(11)V9(6) VALUE +0.    9915845
006500         05  HOLD-SVC-FEE1           PIC S9(13)V99   VALUE +0.    9915845
006600         05  HOLD-SVC-FEE2           PIC S9(13)V99   VALUE +0.    9915845
006800     03  WK-INVEST-FUNCTION          PIC X.
007000     03  WK-INVEST-DDA-BAL           PIC S9(13)V99   COMP-3.      9915845
