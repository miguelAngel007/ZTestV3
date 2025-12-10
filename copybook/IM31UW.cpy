*     * FO5238*12/05/11 JCTE PROYECTO UPGRADE SYSTEMAT
000100*****************************************************************
000110*   BELOW IS THE INDIVIDUAL USER WORKING STORAGE AREA           *
000120*   FOR THIS PROGRAM.                                           *
000130*                                                               *
000140*****************************************************************
000150*   PLEASE ADD CHANGES TO THE WORKING STORAGE COPYBOOK BELOW    *
000160*****************************************************************
000170* PROGRAMMER  USER OPTION #    DATE      CHANGE ID              *
000180*          DESCRIPTION                                          *
000190* BUTCH JONES   9999         8/ 6/ 997    REU0001A              *
000200*          INITIAL EXAMPLE OF OPT     PARAGRAPH                 *
000800*****************************************************************
000805       EJECT
000810*****************************************************************
000815*   COMMON AREA TO REFORMAT CONTROLS                            *
000820*    ADJUST THE CONTROLS TO THE APPLICATION MAY BE NEEDED.      *
000825*****************************************************************
000826 01  WK-UO-APPL                  PIC XX.
000830 01  WK-UO-CONTROLS.
000835     05  WK-UO-CTL1              PIC 9(4).
000840     05  WK-UO-CTL2              PIC 9(4).
000845     05  WK-UO-CTL3              PIC 9(4).
000850     05  WK-UO-CTL4              PIC 9(4).
000855****************************************************************
000860*   THE FOLLOWING SECTION IS RESERVED FOR WORKING STORAGE      *
000865*   FIELDS THAT ARE REQUIRED BY THE TABLE LOAD PROCESS.        *
000870****************************************************************
000875  01  UB-WORKING-FIELDS.
000880      05  UO-INIT-CNTR               PIC 9(04) VALUE ZEROS COMP.
000885      05  UO-INIT-CNTR-MAX           PIC 9(04) VALUE 1000  COMP.
000890      05  UO-TABLE-OPEN              PIC X(01) VALUE 'N'.
000895       EJECT
000900*****************************************************************
000905*   PLEASE TRY TO KEEP OPTIONAL WORK AREAS FOR A GIVEN          *
000910*   OPTION TOGETHER AND COMMENTED SO IT CAN BE EASILY           *
000915*   IDENTIFIED.                                                 *
000920*****************************************************************
000925*    WORKING STORAGE FOR OPTION XXXX                            *
000930*****************************************************************
001000*---------------------------------------------------------------* 0417235
001001*    WORKING STORAGE FOR OPTION 0018                            * 0417235
001002*---------------------------------------------------------------* 0417235
001009*                                                                 0417235
001010 01  WS-USER-0018-FIELDS.                                         0417235
001015     05  WS-U0018-FEE-ENTRY.                                      0417235
001020         10  WS-U0018-FEE-AP         PIC X.                       0417235
001025         10  WS-U0018-FEE-NO         PIC XXX.                     0417235
001030         10  WS-U0018-FEE-NO9        REDEFINES WS-U0018-FEE-NO    0417235
001035                                     PIC 999.                     0417235
001040     05  WS-U0018-GT-NO              PIC XX.                      0417235
001045     05  WS-U0018-GT-NO9   REDEFINES WS-U0018-GT-NO               0417235
001050                                     PIC 99.                      0417235
001055     05  WS-U0018-FEE-AMOUNT         PIC S9(13)V99 COMP-3.        0417235
001057     05  WS-U0018-FEE-AMOUNT-X  REDEFINES WS-U0018-FEE-AMOUNT     0417235
001058                                     PIC X(8).                    0417235
001060     05  WS-U0018-COUNT              PIC S9(3)     VALUE ZERO.    0417235
001065     05  WS-U0018-ORIG-01LEN         PIC S9(4) COMP VALUE ZERO.   0417235
001070     05  WS-U0018-BEGIN-POS          PIC S9(4) COMP VALUE ZERO.   0417235
001075     05  WS-U0018-FEE-CHARGED        PIC X         VALUE 'N'.     0417235
001080     05  WS-U0018-FEE-TABLE.                                      0417235
001085         10  WS-U0018-FEE-KEY        PIC X(19).                   0417235
001090         10  WS-U0018-FEE-ENTRIES    OCCURS 200 TIMES.            0417235
001095             15  WS-U0018-FEE-TBL-NO PIC XXX.                     0417235
001100             15  FILLER              PIC X(31).                   0417235
001105             15  WS-U0018-FEE-TBL-AMT1 PIC S9(13)V99 COMP-3.      0417235
001110             15  FILLER              PIC X(62).                   0417235
001115     05  WS-U0018-HOLD-AMT   PIC S9(13)V99  COMP-3.               0417235
001200                                                                  0527332
001300 01  WS-U0019-AREA.                                               0527332
001400     05  WS-U0019-EXEC-DATA.                                      0527332
001500         10  WS-U0019-PGM-NAME       PIC X(8).                    0527332
001600         10  FILLER                  PIC X(2).                    0527332
001700     05  WS-U0019-CAP-AMT            PIC S9(13)V99.               0527332
001800     05  WS-U0019-CAP-ERROR          PIC X.                       0527332
001900                                                                  0527332
002000 01  WS-U0024-AREA.                                               1020095
002100     05  WS-U0024-EXEC-DATA.                                      1020095
002200         10  WS-U0024-PGM-NAME       PIC X(8).                    1020095
002300         10  FILLER                  PIC X(2).                    1020095
002400     05  WS-U0024-FLAG               PIC X.                       1020095
002500     05  WS-U0024-ERROR              PIC X.                       1020095
