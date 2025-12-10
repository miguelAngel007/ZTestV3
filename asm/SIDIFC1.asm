*ASM XOPTS(NOEPILOG)                                                    00000100
*     * 802262 12/11/20 PROYECTO UPGRADE GN172 EDVR
         SIRENT                                               2024448   00000102
**********************************************************************  00000120
*                      HISTORY OF REVISIONS                          *  00000121
*DESCRIPTION                                                  CHG ID *  00000122
*--------------------------------------------------------------------*  00000123
* 04/10/02 LE ENABLE, REENTRANT                               2024448*  00000182
* 01/10/00 ENABLE FOR 31-BIT                                  2024183*  00000184
* 08/08/97 SET RC FOR COBOL/VSE.                              9913437 * 00000185
*05/13/96  ADDED CAPABILITY FOR 31-BIT ADDRESSING             9913165*  00000187
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
DBLWORD  DS    D                                              2024448   00000266
PARMADDR DS    3F                                             2024448   00000268
SAVELYR  DS    H                                              2024448   00000270
SAVEHYR  DS    H                                              2024448   00000272
SAVEMO   DS    XL1                                            2024448   00000274
LOWDAYS  DS    PL2                                            2024448   00000276
HIGHDAYS DS    PL2                                            2024448   00000278
         DS    0D                                             2024448   00000280
DATEAREA DS    0CL19                                          2024448   00000282
LOWDATE  DS    0CL8                                           2024448   00000284
LOYR     DS    CL4                                            2024448   00000286
LOMO     DS    CL2                                            2024448   00000288
LODA     DS    CL2                                            2024448   00000290
HIGHDATE DS    0CL8                                           2024448   00000292
HIYR     DS    CL4                                            2024448   00000294
HIMO     DS    CL2                                            2024448   00000296
HIDA     DS    CL2                                            2024448   00000298
NODAYS   DS    PL3                                            2024448   00000300
#UDSALEN EQU   *-UDSABEG           LENGTH OF DSA              2024448   00000302
         AIF   ('&SYSPARM' NE 'CICS').GEN1                    2024448   00000304
&CICS    SETB  1                                                        00000400
         TITLE 'FIDIFC1 - ACTUAL DAYS DATE DIFFERENCE ROUTINE'          00000500
FIDIFC1  DFHEIENT CODEREG=(12),DATAREG=(13),EIBREG=(11)                 00000600
FIDIFC1  AMODE 31                                             9913165 / 00000635
FIDIFC1  RMODE ANY                                            9913165 / 00000640
         L     R1,DFHEICAP         LOAD ADDR OF COMMON AREA             00000800
         B     *+24                                                     00000900
         DC    CL20'FIDIFC1 (SIDIFC1 )'                                 00001000
         AGO   .GEN2                                                    00001100
.GEN1    ANOP                                                           00001200
#DSALEN  EQU   *-CEEDSA            LENGTH OF DSA              2024448   00001205
         TITLE 'SIDIFC1 - ACTUAL DAYS DATE DIFFERENCE ROUTINE'          00001300
* THE FOLLOWING STATEMENT(S) MODIFIED BY                      2024448   00001399
SIDIFC1 CEEENTRY ,                                                     X00001400
               PPA=PPA,                                                X00001505
               AUTO=#DSALEN,                                           X00001510
               MAIN=NO,                                                X00001515
               NAB=NO,                                                 X00001520
               PARMREG=1,                                              X00001525
               BASE=10                                                  00001530
         XR    R15,R15                                        2024448   00001535
         B     INIT000                                        2024448   00001540
         USING CEECAA,R12                                     2024448   00001545
* -----------------------------------------------------------*2024448   00001550
*                  PROLOG AREA                               *2024448   00001555
* -----------------------------------------------------------*2024448   00001560
* THE FOLLOWING STATEMENT(S) MODIFIED BY                      2024448   00001564
PPA      CEEPPA ,                                                      X00001565
               LIBRARY=NO,                                             X00001570
               PPA2=YES,                                               X00001575
               EXTPROC=YES,                                            X00001580
               TSTAMP=YES,                                             X00001585
               PEP=YES,                                                X00001590
               INSTOP=YES,                                             X00001595
               EPNAME=SIDIFC1,                                         X00001600
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
         LM    R2,R4,0(R1)              LOAD PARAMETER ADDRESSES        00001700
         STM   R2,R4,PARMADDR           SAVE PARAMETER ADDRESSES        00001800
         LTR   R2,R2                    IS THE 1ST PARM THE LAST PARM   00001900
         BM    A0010                    YES - MOVE ONE PARM             00002000
         MVC   LOWDATE,0(R2)            MOVE LOW DATE TO WORK AREA      00002100
         MVC   HIGHDATE,0(R3)           MOVE HIGH DATE                  00002200
         B     A0020                                                    00002300
A0010    EQU   *                                                        00002400
         MVC   DATEAREA,0(R2)           MOVE PARMS TO WORK AREA         00002500
A0020    EQU   *                                                        00002600
         USING DATE,R7                                                  00002700
         ZAP   NODAYS,P0                ZERO NO-OF-DAYS                 00002800
         CLC   LOWDATE,HIGHDATE         CHK FOR LOW-DATE GT HIGH DATE   00002900
         BH    EXIT                                                     00003000
         TRT   LOWDATE(16),NUMTBL                                       00003100
         BNZ   EXIT                                                     00003200
STARTLOW EQU   *                                                        00003300
         LA    R6,SAVELYR               SET UP LOW-DATE PARAMETERS FOR  00003400
         LA    R7,LOWDATE                    CALCDAYS ROUTINE           00003500
         LA    R8,LOWDAYS                                               00003600
         LA    R9,STARTHI                                               00003700
CALCDAYS EQU   *                                                        00003800
         PACK  DBLWORD,MO                                               00003900
         CVB   R4,DBLWORD                                               00004000
         STC   R4,SAVEMO                CHK MONTH LT ONE                00004100
         CLI   SAVEMO,X'01'                                             00004200
         BL    EXIT                                                     00004300
         CLI   SAVEMO,X'0C'             CHK MONTH GT 12                 00004400
         BH    EXIT                                                     00004500
         SLL   R4,1                     MULTIPLY BY 2                   00004600
         LA    R2,DAYTBL-2(R4)                                          00004700
         MVC   0(2,R8),0(R2)            MOVE JULIAN DAY TO WORK-DAYS    00004800
         PACK  DBLWORD(2),DA                                            00004900
         CP    DBLWORD(2),P1            CHK DAY LT ONE                  00005000
         BL    EXIT                                                     00005100
         CP    DBLWORD(2),DAYTBL+2(2)   CHK DAY GT 31                   00005200
         BH    EXIT                                                     00005300
         AP    0(2,R8),DBLWORD(2)       ADD DAYS TO WORK-DAYS           00005400
         PACK  DBLWORD,YR               CHK FOR LEAP YEAR               00005500
         CVB   R5,DBLWORD                                               00005600
         STH   R5,0(R6)                                                 00005700
         TM    1(R6),X'03'                                              00005800
         BNZR  R9                                                       00005900
         CLI   SAVEMO,X'02'                                             00006000
         BNHR  R9                                                       00006100
         AP    0(2,R8),P1               ADD 1 TO WORK DAYS IF LEAP YEAR 00006200
         BR    R9                            AND NOT JAN OR FEB         00006300
STARTHI  EQU   *                                                        00006400
         LA    R6,SAVEHYR               SET UP HIGH-DATE PARAMETERS FOR 00006500
         LA    R7,HIGHDATE                   CALCDAYS ROUTINE           00006600
         LA    R8,HIGHDAYS                                              00006700
         BAS   R9,CALCDAYS                                    2024183   00006800
         ZAP   NODAYS,HIGHDAYS          MOVE HIGHDAYS TO RETURN NODAYS  00006900
         SP    NODAYS,LOWDAYS           SUBTRACT LOWDAYS                00007000
         LH    R4,SAVELYR                                               00007100
         LH    R5,SAVEHYR                                               00007200
CKLEAPYR EQU   *                                                        00007300
         CR    R4,R5                    CHK LOW YEAR NOT LT HIGH YEAR   00007400
         BNL   EXIT                                                     00007500
         AP    NODAYS,P365              ADD 365 DAYS                    00007600
         STC   R4,DBLWORD               CHK FOR LEAP YEAR               00007700
         TM    DBLWORD,X'03'                                            00007800
         BNZ   ADDYR                                                    00007900
         AP    NODAYS,P1                ADD 1 DAY IF LEAP YEAR          00008000
ADDYR    EQU   *                                                        00008100
         LA    R4,1(0,R4)               ADD 1 TO LOW YEAR               00008200
         B     CKLEAPYR                                                 00008300
EXIT     EQU   *                                                        00008400
         TM    PARMADDR,X'80'           IS 1ST PARM THE ONLY PARM       00008500
         BO    EXIT10                   YES                             00008600
         L     R4,PARMADDR+8            GET LAST PARM ADDR (NO-DAYS)    00008700
         B     EXIT20                                                   00008800
EXIT10   EQU   *                                                        00008900
         L     R4,PARMADDR              GET PARAMETER ADDRESS           00009000
         LA    R4,16(0,R4)              POINT TO NO-DAYS AREA           00009100
EXIT20   EQU   *                                                        00009200
         ZAP   0(3,R4),NODAYS           MOVE NO-DAYS TO PARM AREA       00009300
         AIF   (&CICS).CICSRET                                          00009400
         CEETERM RC=(15)           RETURN                     2024448   00009500
         AGO   .DATA                                                    00009600
.CICSRET ANOP                                                           00009700
         EXEC  CICS RETURN                                              00009800
         LTORG                                                          00009900
.DATA    ANOP                                                           00010000
         DS    0D                                                       00010100
P0       DC    PL1'0'                                                   00010200
P1       DC    PL1'1'                                                   00010300
P365     DC    PL2'365'                                                 00010400
DAYTBL   DC    PL2'000,031,059,090,120,151,181,212,243,273,304,334'     00010500
         DS    0D                                                       00010600
NUMTBL   DC    256X'FF'                                                 00010700
         ORG   NUMTBL+X'F0'                                             00010800
         DC    10X'00'                                                  00010900
         ORG                                                            00011000
         SPACE 3                                                        00013200
         EJECT                                                          00013300
DATE     DSECT                                                          00013400
YR       DS    CL4                                                      00013500
MO       DS    CL2                                                      00013600
DA       DS    CL2                                                      00013700
         AIF   ('&SYSPARM' NE 'CICS').DTSBAT                            00013785
FIDIFC1  CSECT                                                          00013786
         AGO   .DTSMRG                                                  00013787
.DTSBAT  ANOP                                                           00013788
SIDIFC1  CSECT                                                          00013789
.DTSMRG  ANOP                                                           00013790
         LTORG                                                          00013791
         DS    0D                                                       00013792
SITMSTMP DC    CL64'SIDIFC1   -----TSD-             06/24/02  11.36.51' 00013793
*        THIS PROGRAM CONTAINS TRADE SECRETS THAT BELONG                00013794
*        TO ALLTEL INFORMATION SERVICES, INC. AND IS                    00013795
*        LICENSED BY AN AGREEMENT.  ANY UNAUTHORIZED ACCESS,            00013796
*        USE, DUPLICATION, OR DISCLOSURE IS UNLAWFUL.                   00013797
*        COPYRIGHT ALLTEL INFORMATION SERVICES, INC.                    00013798
*        2002, ALL RIGHTS RESERVED.                                     00013799
         END                                                            00013800
