*     * FO5238*12/05/11 JCTE PROYECTO UPGRADE SYSTEMAT
000100*----------------------------------------------------------------*
000200*    DAILY FILE MAINTENANCE COPYBOOK.                            *
000300*----------------------------------------------------------------*
000400*    INTERFACE FILES FOR AUTOBORROW AND COMMERCIAL SWEEP         *
000500*    SYSTEMS.                                                    *
000600*----------------------------------------------------------------*
000700 01  IM-ACM-ABM-TRAN.
000800     03  IM-ACF-BANK-ID          PIC 9(04)       COMP.
000900     03  IM-ACF-ACCOUNT.
001000         05  IM-ACF-ACCT-FIL     PIC X(01).
001100         05  IM-ACF-ACCT-NO      PIC X(10).
001200     03  IM-ACF-BRCH-NO          PIC S9(05)      COMP-3.
001300     03  IM-ACF-ACCT-STATUS      PIC X(02).
001400     03  IM-ACF-OFFICER          PIC X(05).
001500     03  IM-ACF-SUB-PRD-CD       PIC X(02).
001600     03  IM-ACF-STMT-CYCLE       PIC X(02).
001700     03  IM-ACF-TAX-ID-IND       PIC X(01).
001800     03  IM-ACF-TAX-ID           PIC 9(09).
001900     03  IM-ACF-NAME-ADDR-A.
002000         05  IM-ACF-NAME-1       PIC X(40).
002100         05  IM-ACF-NAME-2       PIC X(40).
002200         05  IM-ACF-ADDR-1       PIC X(40).
002300         05  IM-ACF-ADDR-2       PIC X(40).
002400         05  IM-ACF-CITY-ST      PIC X(40).
002500     03  IM-ACF-NAME-ADDR REDEFINES IM-ACF-NAME-ADDR-A
002600                                 PIC X(40)   OCCURS 5 TIMES.
002700     03  IM-ACF-ZIP-CD.
002800         05  IM-ACF-ZIP-CD-5     PIC X(05).
002900         05  IM-ACF-ZIP-CD-4     PIC X(04).
003000     03  IM-ACF-COST-CENT        PIC S9(05)      COMP-3.
003100     03  IM-ACF-SWEEP-IND        PIC X(01).
003200     03  IM-ACF-ABM-IND          PIC X(01).
003300     03  IM-ACF-END-BAL          PIC S9(13)V99   COMP-3.          0726713
003400     03  IM-ACF-AVAIL-CR         PIC S9(13)V99   COMP-3.
003500     03  IM-ACF-LOAN-BAL         PIC S9(13)V99   COMP-3.
003600     03  FILLER                  PIC X(125).                      0726713
