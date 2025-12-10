*     * 802262 12/11/20 PROYECTO UPGRADE GN172 EDVR
000001****************************************************************  0324492
000002*                                                                 0324492
000004*             TRANSACTION SYSTEM USER OPTION DETAIL RECORD        0324492
000006*                                                                 0324492
000007*               ** HISTORY OF REVISIONS **                     *  0324492
000008* DESCRIPTION                                           CHNGID *  0324492
000009* ____________________________________________________  _______*  0324492
000098* 07/23/02 ADD ADDITIONAL COMMENTS                     ~~~4492 *  0324492
000099*                                                              *  0324492
000100****************************************************************
000200*               USER OPTION DETAIL RECORD IS A RECORD OF THE   *
000300*               VARIABLE PROCESSING OPTIONS FOR A GIVEN        *
000400*               CLIENT.  THIS RECORD CONTAINS ONLY THE INFO.   *  0324492
000500*               NEEDED TO CODE VARIABLE OPTIONS WITHIN A PROG. *
000600*               AND TURN THOSE OPTIONS ON BY CLIENT USING THIS *
000700*               FILE.  THE DESCRIPTIONS FOR THESE OPTIONS ARE  *
000800*               HOUSED ON THE TSUOP FILE.                      *  0324492
000802*               THIS COPYBOOK IS USED BY BOTH BATCH & ONLINE.  *  0324492
000804*               PROGRAMS WILL USE THIS COPYBOOK WHEN INVOKING  *  0324492
000806*               TSUOADDR TO ACCESS THE USER OPTION ONLINE      *  0324492
000808*               TABLE IN THE EXPANDED FORMAT.                  *  0324492
000810*               TSUOB IS THE CORRESPONDING DAG FOR USE BY API  *  0324492
000812*               MODULES.                                       *  0324492
000900****************************************************************
001000  01  USER-OPTION-DETAIL.
001100      05  USER-OPTION-KEY.
001110          10  UB-APPL             PIC X(02).
001200          10  UB-CONTROLS.
001300              15  UB-CTL1         PIC 9(04).
001400              15  UB-CTL2         PIC 9(04).
001500              15  UB-CTL3         PIC 9(04).
001600              15  UB-CTL4         PIC 9(04).
001700      05  UB-TABLE.
001800****************************************************************
001900*   UB-OPTION-DET IS THE DETAIL RECORD THAT CONTAINS THE FLAGS *
002000*   REQUIRED BY THE SYSTEM TO PROCESS THE VARIABLE USER OPTIONS*
002100*   THAT ARE CODED IN ORDER TO REPLACE 'IF BANK' CODE IN THE   *
002200*   MAINLINE MODULES OF OUR SYSTEMS.                           *
002300****************************************************************
002400          10  UB-OPTION-DET OCCURS 1000 TIMES.
002500              15  UB-OPTION-FLAG  PIC X(01).
002600****************************************************************
002700*   UB-EXECUTION-OPT IS THE FIELD THAT CONTAINS THE VALUES     *
002800*   WHICH WILL BE MAINTAINED AS VARIABLE DECISION POINTS IN    *
002900*   THE PROGRAM.  (IE.ACCOUNT TYPE, INVESTOR TYPE, PRODUCT,    *
003000*   ..,ETC).  THE REDEFINED FIELDS BELOW WILL BE USED BY THE   *
003100*   PROGRAMMER TO COMPARE WITHIN THE USER OPTION PARAGRAPH.    *
003200****************************************************************
003300              15  UB-EXECUTION-OPT   PIC X(10).
003400              15  UB-FIELD1-1 REDEFINES UB-EXECUTION-OPT.
003500                  20  UB-FLD1-1      PIC X(1) OCCURS 10 TIMES.
003600              15  UB-FIELD1-2 REDEFINES UB-EXECUTION-OPT.
003700                  20  UB-FLD1-2      PIC X(2) OCCURS 5 TIMES.
003800              15  UB-FIELD1-3 REDEFINES UB-EXECUTION-OPT.
003900                  20  UB-FLD1-3      PIC X(3) OCCURS 3 TIMES.
004000                  20  FILLER         PIC X(1).
004100              15  UB-FIELD1-4 REDEFINES UB-EXECUTION-OPT.
004200                  20  UB-FLD1-4      PIC X(4) OCCURS 2 TIMES.
004300                  20  FILLER         PIC X(2).
004400              15  UB-FIELD1-5 REDEFINES UB-EXECUTION-OPT.
004500                  20  UB-FLD1-5      PIC X(5) OCCURS 2 TIMES.
