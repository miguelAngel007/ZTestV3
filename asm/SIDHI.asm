*ASM XOPTS(NOEPILOG)                                                    00000100
*     * 802262 12/11/20 PROYECTO UPGRADE GN172 EDVR
         SIRENT                                               2024448   00000102
**********************************************************************  00000120
*                      HISTORY OF REVISIONS                          *  00000121
*DESCRIPTION                                                  CHG ID *  00000122
*--------------------------------------------------------------------*  00000123
* 04/11/02 LE ENABLED, REENTRANT                              2024448*  00000182
* 01/07/00 ENABLE FOR 31-BIT                                  2024183*  00000184
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
DBLWORD  DS    D                                              2024448   00000264
PGMPLIST DS    F                   ORIGINAL R1 SAVE AREA      2024448   00000266
HOLDDAYS DS    PL3                                            2024448   00000268
#UDSALEN EQU   *-UDSABEG           LENGTH OF DSA              2024448   00000270
         AIF   ('&SYSPARM' NE 'CICS').GEN1                              00000300
&CICS    SETB  1                                                        00000400
         TITLE 'FIDHI - CALCULATE HIGH DATE'                            00000500
FIDHI    DFHEIENT CODEREG=(12),DATAREG=(13),EIBREG=(11)                 00000600
FIDHI    AMODE 31                                             9913165 / 00000635
FIDHI    RMODE ANY                                            9913165 / 00000640
         L     R1,DFHEICAP         LOAD ADDR OF COMMON AREA             00000800
         B     *+24                                                     00000900
         DC    CL20'FIDHI   (SIDHI   )'                                 00001000
         AGO   .GEN2                                                    00001100
.GEN1    ANOP                                                           00001200
#DSALEN  EQU   *-CEEDSA            LENGTH OF DSA              2024448   00001205
         TITLE 'SIDHI - CALCULATE HIGH DATE'                            00001300
* THE FOLLOWING STATEMENT(S) MODIFIED BY                      2024448   00001399
SIDHI CEEENTRY ,                                                       X00001400
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
               EPNAME=SIDHI,                                           X00001600
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
*                                                                       00001700
*********************************************************************** 00001800
*        THIS ROUTINE DETERMINES A HIGH DATE GIVEN A LOW STARTING     * 00001900
*              DATE AND THE NO. OF DAYS BETWEEN THE DATES.            * 00002000
*        THE DATES MUST ZONED DECIMAL MONTH, DAY, YEAR.               * 00002100
*        THE DATA AREA BEING USED MUST BE IN THE ORDER:               * 00002200
*              LOW-DATE, HIGH-DATE, NO-OF-DAYS.                       * 00002300
*        THE NO-OF-DAYS MUST BE A 3 BYTE PACKED FIELD.                * 00002400
*********************************************************************** 00002700
*                                                                       00002800
         L     R1,0(0,R1)            LOAD ADDR OF DATE AREA INTO R1     00002900
         MVC   6(6,R1),0(R1)         MOVE INPUT DATE TO OUTPUT DATE     00002910
         TRT   0(6,R1),NUMTBL        IS DATE NUMERIC                    00002920
         BNZ   EXIT                  NO                                 00002930
         CLC   0(6,R1),ZEROS         IS DATE ALL ZEROS                  00002940
         BE    EXIT                  YES                                00002950
         PACK  6(2,R1),0(2,R1)       PACK LOW MONTH INTO HIGH MONTH     00003000
         PACK  8(2,R1),2(2,R1)       PACK LOW DAY INTO HIGH DAY         00003100
         PACK  10(2,R1),4(2,R1)      PACK LOW YEAR INTO HIGH YEAR       00003200
         ZAP   HOLDDAYS,12(3,R1)     MOVE NO. OF DAYS TO HOLDDAYS       00003300
         AP    HOLDDAYS,8(2,R1)      ADD HIGH DAYS TO HOLDDAYS          00003400
GETDAYS  EQU   *                                                        00003500
         ZAP   DBLWORD,6(2,R1)       MOVE HIGH MONTH TO DBLWORD         00003600
         CVB   R3,DBLWORD            CONVERT MONTH TO BINARY            00003700
         SLL   R3,1                  MULTIPLY MONTH BY 2                00003800
         LA    R3,DAYTBL-2(R3)       ADD DAYTBL BASE                    00003900
         ZAP   8(2,R1),0(2,R3)       MOVE DAYTBL DAYS TO HIGH DAYS      00004000
         CP    6(2,R1),P2            CHK HIGH MONTH = 2                 00004100
         BNE   CHKDAYS               NOT FEB                            00004200
         ZAP   DBLWORD,10(2,R1)      MOVE HIGH YEAR TO DBLWORD          00004300
         CVB   R3,DBLWORD            CONVERT HIGH YEAR TO BINARY        00004400
         STC   R3,DBLWORD            STORE HIGH YEAR IN DBLWORD         00004500
         TM    DBLWORD,X'03'         CHK FOR LEAP YEAR                  00004600
         BNZ   CHKDAYS               GO TO CHKDAYS IF NOT LEAP YEAR     00004700
         AP    8(2,R1),P1            ADD 1 TO HIGH DAYS                 00004800
CHKDAYS  EQU   *                                                        00004900
         CP    HOLDDAYS,8(2,R1)      CHK FOR HOLDDAYS LE HIGH DAYS      00005000
         BH    SUBDAYS               GO TO SUBDAYS IF GT                00005100
         ZAP   8(2,R1),HOLDDAYS      MOVE HOLDDAYS TO HIGH DAYS         00005200
         B     FINISH                GO TO FINISH                       00005300
SUBDAYS  SP    HOLDDAYS,8(2,R1)      SUBTRACT HIGH DAYS FROM HOLDDAYS   00005400
         AP    6(2,R1),P1            ADD 1 TO HIGH MONTH                00005500
         CP    6(2,R1),P13           CHK FOR MONTH = 13                 00005600
         BNE   GETDAYS               GO TO GETDAYS IF NOT EQUAL         00005700
         ZAP   6(2,R1),P1            MOVE 1 TO HIGH MONTH               00005800
         AP    10(2,R1),P1           ADD 1 TO HIGH YEAR                 00005900
         B     GETDAYS               GO TO GETDAYS                      00006000
FINISH   UNPK  6(2,R1),6(2,R1)       UNPACK HIGH MONTH                  00006100
         UNPK  8(2,R1),8(2,R1)       UNPACK HIGH DAY                    00006200
         UNPK  10(2,R1),10(2,R1)     UNPACK HIGH YEAR                   00006300
         OI    7(R1),X'F0'           CHANGE HIGH MONTH SIGN             00006400
         OI    9(R1),X'F0'           CHANGE HIGH DAY SIGN               00006500
         OI    11(R1),X'F0'          CHANGE HIGH YEAR SIGN              00006600
EXIT     EQU   *                                                        00006610
         AIF   (&CICS).CICSRET                                          00006700
         CEETERM RC=(15)           RETURN                     2024448   00006800
         AGO   .DATA                                                    00006900
.CICSRET ANOP                                                           00007000
         EXEC  CICS RETURN                                              00007100
         LTORG                                                          00007200
.DATA    ANOP                                                           00007300
P1       DC    PL1'1'                                                   00007400
P2       DC    PL1'2'                                                   00007500
P13      DC    PL2'013'                                                 00007600
DAYTBL   DC    PL2'031,028,031,030,031,030,031,031,030,031,030,031'     00007700
ZEROS    DC    CL6'000000'                                              00007710
NUMTBL   DC    256X'FF'                                                 00007720
         ORG   NUMTBL+X'F0'                                             00007730
         DC    10X'00'                                                  00007740
         ORG                                                            00007750
         AIF   ('&SYSPARM' NE 'CICS').DTSBAT                            00008285
FIDHI    CSECT                                                          00008286
         AGO   .DTSMRG                                                  00008287
.DTSBAT  ANOP                                                           00008288
SIDHI    CSECT                                                          00008289
.DTSMRG  ANOP                                                           00008290
         LTORG                                                          00008291
         DS    0D                                                       00008292
SITMSTMP DC    CL64'SIDHI     -----TSD-             08/08/02  19.51.01' 00008293
*        THIS PROGRAM CONTAINS TRADE SECRETS THAT BELONG                00008294
*        TO ALLTEL INFORMATION SERVICES, INC. AND IS                    00008295
*        LICENSED BY AN AGREEMENT.  ANY UNAUTHORIZED ACCESS,            00008296
*        USE, DUPLICATION, OR DISCLOSURE IS UNLAWFUL.                   00008297
*        COPYRIGHT ALLTEL INFORMATION SERVICES, INC.                    00008298
*        2002, ALL RIGHTS RESERVED.                                     00008299
         END                                                            00008300
