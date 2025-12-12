*ASM XOPTS(NOEPILOG)                                                    00000100
*     * 802262 12/11/20 PROYECTO UPGRADE GN172 EDVR
         SIRENT                                               2024448   00000102
**********************************************************************  00000120
*                      HISTORY OF REVISIONS                          *  00000121
*DESCRIPTION                                                  CHG ID *  00000122
*--------------------------------------------------------------------*  00000123
* 04/29/02 LE ENABLED, REENTRANT                              2024448*  00000182
* 03/08/00 ENABLE FOR 31-BIT PROCESSING                       2024183*  00000184
* 08/11/97 SET RC FOR COBOL/VSE.                              9913437 * 00000185
*05/21/96  ADDED CAPABILITY FOR 31-BIT ADDRESSING             9913165*  00000187
*                                                                    *  00000188
**********************************************************************  00000189
         LCLB  &CICS                                                    00000200
         SIEQREG                                              2024448   00000205
         AIF   ('&SYSPARM' NE 'CICS').BTCHDTA                 2024448   00000235
* THE FOLLOWING STATEMENT(S) MODIFIED BY                      2024448   00000239
DFHEISTG DSECT                                                          00000240
         AGO   .DATAMRG                                       2024448   00000246
* THE FOLLOWING STATEMENT(S) MODIFIED BY                      2024448   00000247
.BTCHDTA ANOP                                                           00000248
         PRINT ON,GEN                                         2024448   00000250
* THE FOLLOWING STATEMENT(S) MODIFIED BY                      2024448   00000251
         CEECAA                                                         00000252
         EJECT                                                2024448   00000254
* THE FOLLOWING STATEMENT(S) MODIFIED BY                      2024448   00000255
         CEEDSA                                                         00000256
         EJECT                                                2024448   00000258
* THE FOLLOWING STATEMENT(S) MODIFIED BY                      2024448   00000259
.DATAMRG ANOP                                                           00000260
UDSABEG  DS    0D                  BEGIN OF USER DSA          2024448   00000262
PGMPLIST DS    F                   ORIGINAL R1 SAVE AREA      2024448   00000264
P0       DS    P                                              2024448   00000266
#UDSALEN EQU   *-UDSABEG           LENGTH OF DSA              2024448   00000270
         AIF   ('&SYSPARM' NE 'CICS').GEN1                              00000300
&CICS    SETB  1                                                        00000400
         TITLE 'FIVARUNP - VARIABLE-LENGTH UNPACK ROUTINE'              00000500
FIVARUNP DFHEIENT CODEREG=(12),DATAREG=(13),EIBREG=(11)                 00000600
FIVARUNP AMODE 31                                             9913165 / 00000635
FIVARUNP RMODE ANY                                            9913165 / 00000640
         L     R1,DFHEICAP         LOAD ADDR OF COMMON AREA             00000800
         B     *+24                                                     00000900
         DC    CL20'FIVARUNP(SIVARUNP)'                                 00001000
         AGO   .GEN2                                                    00001100
.GEN1    ANOP                                                           00001200
#DSALEN  EQU   *-CEEDSA            LENGTH OF DSA              2024448   00001205
         TITLE 'SIVARUNP - VARIABLE-LENGTH UNPACK ROUTINE'              00001300
* THE FOLLOWING STATEMENT(S) MODIFIED BY                      2024448   00001399
SIVARUNP CEEENTRY ,                                                    X00001400
               PPA=PPA,                                                X00001505
               AUTO=#DSALEN,                                           X00001510
               MAIN=NO,                                                X00001515
               NAB=NO,                                                 X00001520
               PARMREG=1,                                              X00001525
               BASE=9                                                   00001530
         XR    R15,R15                                        2024448   00001535
         B     INIT000                                        2024448   00001540
         USING CEECAA,R12                                     2024448   00001545
* ---------------------------------------------------------- *2024448   00001550
*                  PROLOG AREA                               *2024448   00001555
* ---------------------------------------------------------- *2024448   00001560
* THE FOLLOWING STATEMENT(S) MODIFIED BY                      2024448   00001564
PPA      CEEPPA ,                                                      X00001565
               LIBRARY=NO,                                             X00001570
               PPA2=YES,                                               X00001575
               EXTPROC=YES,                                            X00001580
               TSTAMP=YES,                                             X00001585
               PEP=YES,                                                X00001590
               INSTOP=YES,                                             X00001595
               EPNAME=SIVARUNP,                                        X00001600
               VER=01,                                                 X00001605
               REL=01,                                                 X00001610
               MOD=00,                                                 X00001615
               DSA=YES                                                  00001620
         SPACE 2                                              2024448   00001625
         LTORG                                                2024448   00001630
INIT000  EQU   *                                              2024448   00001635
         ST    R1,PGMPLIST                                    2024448   00001640
* THE FOLLOWING STATEMENT(S) MODIFIED BY                      2024448   00001644
.GEN2    ANOP                                                           00001645
         LM    R2,R4,0(R1)         GET PARAMETER ADDRESSES              00001700
         LH    R4,0(0,R4)          LOAD REG 4 WITH PACKED LENGTH        00001800
         BCTR  R4,0                SUBTRACT 1 FOR EX                    00001900
         LA    R6,0(R4,R4)         CALC LENGTH OF UNPACKED FIELD        00002000
         MVI   0(R2),C' '          CLEAR SIGN                           00002100
         LA    R7,0(R4,R3)         GET ADDR OF LAST BYTE OF FROM FLD    00002200
         MVC   P0,0(R7)            PUT SIGN BYTE INTO WORK BYTE         00002300
         OI    P0,X'F0'            TURN ON HI-ORDER 4-BITS              00002400
         CLI   P0,X'FD'            IS IT A NEGATIVE FIELD?              00002500
         BNE   *+8                 NO...POSITIVE                        00002600
         MVI   0(R2),C'-'          MOVE MINUS SIGN                      00002700
         LA    R2,1(0,R2)          BUMP TO 2ND OUTPUT BYTE              00002800
         OI    0(R7),X'0F'         MAKE SIGN POSITIVE                   00002900
         CH    R6,H14              IS TO-FLD GREATER THAN 15 BYTES      00003000
         BH    CHKLONG             YES - PROCESS LONG OUTPUT FIELD      00003100
DOUNPK   EQU   *                                                        00003200
         SLA   R6,4                SHIFT LEFT 4 BITS                    00003300
         OR    R4,R6               OR LENGTHS INTO ONE BYTE             00003400
         EX    R4,UNPKINST         EXECUTE UNPACK INSTRUCTION           00003500
         AIF   (&CICS).CICSRET                                          00003600
         CEETERM RC=(15)           RETURN                     2024448   00003700
         AGO   .DATA                                                    00003800
.CICSRET ANOP                                                           00003900
         EXEC  CICS RETURN                                              00004000
         LTORG                                                          00004100
         DS    0H                                                       00004200
.DATA    ANOP                                                           00004300
CHKLONG  EQU   *                                                        00004400
         CH    R4,H7               IS FROM-FLD GREATER THAN 8 BYTES     00004500
         BH    UNPKLONG            YES - PROCESS LONG FROM/TO-FIELDS    00004600
*        THIS IS A LONG TO-FLD AND A SHORT FROM-FLD                     00004700
         EX    R6,CLRTOFLD         CLEAR ALL OF TO-FIELD                00004800
         LA    R1,1(R4,R4)         CALC NEEDED TO-FLD LNG               00004900
         LA    R2,0(R6,R2)         POINT TO LAST BYTE IN TO-FLD         00005000
         SR    R2,R1               BACK UP TO CORRECT BYTE              00005100
         LR    R6,R1               GET NEW TO LNG IN R6                 00005200
         B     DOUNPK              UNPACK RIGHTMOST INPUT               00005300
UNPKLONG EQU   *                                                        00005400
         SH    R4,H7               SET FIRST UNPK FROM LNG              00005500
         SH    R6,H14              SET FIRST UNPK TO LNG                00005600
         LR    R7,R4               SAVE FROM-LENGTH                     00005700
         LR    R8,R6               SAVE TO-LENGTH                       00005800
         SLA   R6,4                SHIFT LEFT 4 BITS                    00005900
         OR    R4,R6               OR LENGTHS INTO ONE BYTE             00006000
         EX    R4,UNPKINST         EXECUTE UNPK INSTRUCTION             00006100
         AR    R2,R8               SET NEW TO-FLD ADDR                  00006200
         AR    R3,R7               SET NEW FROM-FLD ADDR                00006300
         LA    R4,7                SET NEW FROM-FLD LNG                 00006400
         LA    R6,14               SET NEW TO-FLD LNG                   00006500
         B     DOUNPK              UNPACK THE REST OF IT                00006600
UNPKINST UNPK  0(0,R2),0(0,R3)                                          00006700
CLRTOFLD MVC   0(0,R2),ZEROS                                            00006800
H7       DC    H'7'                                                     00006900
H14      DC    H'14'                                                    00007000
ZEROS    DC    31CL1'0'                                                 00007100
         AIF   ('&SYSPARM' NE 'CICS').DTSBAT                            00007585
FIVARUNP CSECT                                                          00007586
         AGO   .DTSMRG                                                  00007587
.DTSBAT  ANOP                                                           00007588
SIVARUNP CSECT                                                          00007589
.DTSMRG  ANOP                                                           00007590
         LTORG                                                          00007591
         DS    0D                                                       00007592
SITMSTMP DC    CL64'SIVARUNP  -----TSD-             06/24/02  12.26.55' 00007593
*        THIS PROGRAM CONTAINS TRADE SECRETS THAT BELONG                00007594
*        TO ALLTEL INFORMATION SERVICES, INC. AND IS                    00007595
*        LICENSED BY AN AGREEMENT.  ANY UNAUTHORIZED ACCESS,            00007596
*        USE, DUPLICATION, OR DISCLOSURE IS UNLAWFUL.                   00007597
*        COPYRIGHT ALLTEL INFORMATION SERVICES, INC.                    00007598
*        2002, ALL RIGHTS RESERVED.                                     00007599
         END                                                            00007600
