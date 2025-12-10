*     * FO5238*12/05/11 JCTE PROYECTO UPGRADE SYSTEMAT
000100*----------------------------------------------------------------*
000200*    FEE NUMBER ASSIGNMENT TABLE                                 *
000300*----------------------------------------------------------------*
000400 01  FTC-RECORD.
000500     05  FTC-CTRL-SRCE-TRAN-KEY.
000600         10  FTC-CONTROL-1        PIC X(2).
000700         10  FTC-CONTROL-2        PIC X(3).
000800         10  FTC-CONTROL-3        PIC X(3).
000900         10  FTC-SOURCE-NUMBER    PIC X(3).
001000         10  FTC-TRAN-CODE        PIC X(4).
001100     05  FTC-TRAN-TYPE            PIC 9.
001200     05  FTC-FEE-TYPE             PIC X.
001300     05  FTC-FEE-NUMBER           PIC 9(3).
001400     05  FTC-FEE-GEN-TC           PIC X(4).
001500     05  FTC-SRV-CHG-ACC-1        PIC X(3).
001600     05  FTC-SRV-CHG-ACC-2        PIC X(3).
001700     05  FTC-OVERRIDE-FLAG        PIC X.
001800     05  FTC-HARD-POST-ALL        PIC X(1).
001900     05  FTC-REV-IND              PIC X(1).
002000     05  FTC-IM-TXN-CODE          PIC X(4).
002100     05  FTC-CAPS-TXN-CODE        PIC X(4).
002200     05  FTC-SOFT-DOLLAR-FLAG     PIC X(1).
002300     05  FTC-REV-TXN-CODE         PIC X(4).
002400     05  FILLER                   PIC X(2).
002500     05  FTC-GL-AREA.
002600         10  FTC-COM-GL-INC.
002700             15  FTC-COM-GL-INC-ACCT PIC X(10).
002800             15  FTC-COM-GL-INC-CC PIC X(7).
002900         10  FTC-PRV-GL-INC.
003000             15  FTC-PRV-GL-INC-ACCT PIC X(10).
003100             15  FTC-PRV-GL-INC-CC PIC X(7).
003200         10  FTC-PUB-GL-INC.
003300             15  FTC-PUB-GL-INC-ACCT PIC X(10).
003400             15  FTC-PUB-GL-INC-CC PIC X(7).
003500         10  FTC-COM-GL-WVE.
003600             15  FTC-COM-GL-WVE-ACCT PIC X(10).
003700             15  FTC-COM-GL-WVE-CC PIC X(7).
003800         10  FTC-PRV-GL-WVE.
003900             15  FTC-PRV-GL-WVE-ACCT PIC X(10).
004000             15  FTC-PRV-GL-WVE-CC PIC X(7).
004100         10  FTC-PUB-GL-WVE.
004200             15  FTC-PUB-GL-WVE-ACCT PIC X(10).
004300             15  FTC-PUB-GL-WVE-CC PIC X(7).
004400     05  FILLER                   REDEFINES FTC-GL-AREA.
004500         10  FTC-GL-INC           OCCURS 0003 INDEXED BY
004600             FTC-INC-INDX.
004700             15  FTC-GL-INC-ACCT  PIC X(10).
004800             15  FTC-GL-INC-CC    PIC X(7).
004900         10  FTC-GL-WVE           OCCURS 0003 INDEXED BY
005000             FTC-WVE-INDX.
005100             15  FTC-GL-WVE-ACCT  PIC X(10).
005200             15  FTC-GL-WVE-CC    PIC X(7).
