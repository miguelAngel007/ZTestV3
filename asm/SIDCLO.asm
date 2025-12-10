*ASM XOPTS(NOEPILOG)                                                    00000100
*     * 802262 12/11/20 PROYECTO UPGRADE GN172 EDVR
         SIRENT                                               2024448   00000105
**********************************************************************  00000120
*                      HISTORY OF REVISIONS                          *  00000121
*DESCRIPTION                                                  CHG ID *  00000122
*--------------------------------------------------------------------*  00000123
* 04/10/02 LE ENABLED, REENTRANT                              2024448*  00000182
* 01/07/00 ENBALE FOR 31-BIT                                  2024183*  00000184
* 08/08/97 SET RC FOR COBOL/VSE                               9913437 * 00000185
*05/10/96  ADDED CAPABILITY FOR 31-BIT ADDRESSING             9913165*  00000187
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
WORKDAYS DS    PL3                                            2024448   00000270
         DS    0D                                             2024448   00000272
DATEAREA DS    0CL19                                          2024448   00000274
LOWDATE  DS    0CL8                                           2024448   00000276
LOYR     DS    CL4                                            2024448   00000278
LOMO     DS    CL2                                            2024448   00000280
LODA     DS    CL2                                            2024448   00000282
HIGHDATE DS    0CL8                                           2024448   00000284
HIYR     DS    CL4                                            2024448   00000286
HIMO     DS    CL2                                            2024448   00000288
HIDA     DS    CL2                                            2024448   00000290
NODAYS   DS    PL3                                            2024448   00000292
#UDSALEN EQU   *-UDSABEG           LENGTH OF DSA              2024448   00000300
         AIF   ('&SYSPARM' NE 'CICS').GEN1                    2024448   00000302
&CICS    SETB  1                                                        00000400
         TITLE 'FIDCLO - DETERMINE LOW DATE ROUTINE'                    00000500
FIDCLO   DFHEIENT CODEREG=(12),DATAREG=(13),EIBREG=(11)                 00000600
FIDCLO   AMODE 31                                             9913165 / 00000635
FIDCLO   RMODE ANY                                            9913165 / 00000640
         L     R1,DFHEICAP         LOAD ADDR OF COMMON AREA             00000800
         B     *+24                                                     00000900
         DC    CL20'FIDCLO  (SIDCLO  )'                                 00001000
         AGO   .GEN2                                                    00001100
.GEN1    ANOP                                                           00001200
#DSALEN  EQU   *-CEEDSA            LENGTH OF DSA              2024448   00001205
         TITLE 'SIDCLO  - DETERMINE LOW DATE ROUTINE'                   00001300
* THE FOLLOWING STATEMENT(S) MODIFIED BY                      2024448   00001399
SIDCLO CEEENTRY ,                                                      X00001400
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
               EPNAME=SIDCLO,                                          X00001600
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
         MVC   HIGHDATE,0(R3)           MOVE LOW DATE TO WORK AREA      00002100
         ZAP   NODAYS,0(3,R4)           MOVE NO. OF DAYS                00002200
         B     A0020                                                    00002300
A0010    EQU   *                                                        00002400
         MVC   DATEAREA,0(R2)           MOVE PARMS TO WORK AREA         00002500
A0020    EQU   *                                                        00002600
         TRT   HIGHDATE,NUMTBL          CHK FOR VALID HIGH DATE         00002700
         BNZ   ERREXIT                  SET LOW DATE ALL ZEROS IF BAD   00002800
         PACK  LOYR,HIYR                                                00002900
         PACK  LOMO,HIMO                                                00003000
         PACK  LODA,HIDA                                                00003100
         ZAP   WORKDAYS,NODAYS                                          00003200
COMPDAY  EQU   *                                                        00003300
         CP    WORKDAYS,LODA            CHK WORKDAYS LT DAYS IN MONTH   00003400
         BNL   SUBDAYS                                                  00003500
         SP    LODA,WORKDAYS                                            00003600
         B     FINISH                                                   00003700
SUBDAYS  EQU   *                                                        00003800
         SP    WORKDAYS,LODA                                            00003900
         CP    LOMO,P1                  CHK FOR JANUARY                 00004000
         BNE   SUBMO                                                    00004100
         ZAP   LOMO,P13                 MOVE 13 TO MONTH                00004200
         SP    LOYR,P1                  SUBTRACT 1 FROM YEAR            00004300
SUBMO    EQU   *                                                        00004400
         SP    LOMO,P1                  SUBTRACT 1 FROM MONTH           00004500
         ZAP   DBLWORD,LOMO             FIND MONTH DAYS IN TABLE        00004600
         CVB   R3,DBLWORD                                               00004700
         SLL   R3,1                     MULTIPLY BY 2                   00004800
         LA    R2,DAYTBL-2(R3)                                          00004900
         ZAP   LODA,0(2,R2)             MOVE MONTH DAYS TO LOW DAYS     00005000
         CP    LOMO,P2                  CHK FOR FEB                     00005100
         BNE   COMPDAY                                                  00005200
         ZAP   DBLWORD,LOYR             CHK FOR LEAP YEAR               00005300
         CVB   R3,DBLWORD                                               00005400
         STC   R3,DBLWORD                                               00005500
         TM    DBLWORD,X'03'                                            00005600
         BNZ   COMPDAY                                                  00005700
         AP    LODA,P1                  ADD 1 TO DAYS                   00005800
         B     COMPDAY                                                  00005900
ERREXIT  EQU   *                                                        00006000
         MVC   LOWDATE,ZEROS                                            00006100
         B     EXIT                                                     00006200
FINISH   EQU   *                                                        00006300
         ZAP   WORKDAYS,LOYR                                            00006400
         UNPK  LOYR,WORKDAYS                                            00006500
         UNPK  LOMO,LOMO                                                00006600
         UNPK  LODA,LODA                                                00006700
         OI    LOYR+3,X'F0'                                             00006800
         OI    LOMO+1,X'F0'                                             00006900
         OI    LODA+1,X'F0'                                             00007000
EXIT     EQU   *                                                        00007100
         L     R4,PARMADDR              GET PARAMETER ADDRESS           00007200
         MVC   0(8,R4),LOWDATE          MOVE NO-DAYS TO PARM AREA       00007300
         AIF   (&CICS).CICSRET                                          00007400
         CEETERM RC=(15)           RETURN                     2024448   00007500
         AGO   .DATA                                                    00007600
.CICSRET ANOP                                                           00007700
         EXEC  CICS RETURN                                              00007800
         LTORG                                                          00007900
.DATA    ANOP                                                           00008000
         EJECT                                                          00008100
         DS    0D                                                       00008200
ZEROS    DC    CL8'00000000'                                            00008300
P1       DC    PL1'1'                                                   00008400
P2       DC    PL1'2'                                                   00008500
P13      DC    PL2'013'                                                 00008600
DAYTBL   DC    PL2'031,028,031,030,031,030,031,031,030,031,030,031'     00008700
         DS    0D                                                       00008800
NUMTBL   DC    256X'FF'                                                 00008900
         ORG   NUMTBL+X'F0'                                             00009000
         DC    10X'00'                                                  00009100
         ORG                                                            00009200
         EJECT                                                          00009300
         AIF   ('&SYSPARM' NE 'CICS').DTSBAT                            00011085
FIDCLO   CSECT                                                          00011086
         AGO   .DTSMRG                                                  00011087
.DTSBAT  ANOP                                                           00011088
SIDCLO   CSECT                                                          00011089
.DTSMRG  ANOP                                                           00011090
         LTORG                                                          00011091
         DS    0D                                                       00011092
SITMSTMP DC    CL64'SIDCLO    -----TSD-             06/24/02  10.43.59' 00011093
*        THIS PROGRAM CONTAINS TRADE SECRETS THAT BELONG                00011094
*        TO ALLTEL INFORMATION SERVICES, INC. AND IS                    00011095
*        LICENSED BY AN AGREEMENT.  ANY UNAUTHORIZED ACCESS,            00011096
*        USE, DUPLICATION, OR DISCLOSURE IS UNLAWFUL.                   00011097
*        COPYRIGHT ALLTEL INFORMATION SERVICES, INC.                    00011098
*        2002, ALL RIGHTS RESERVED.                                     00011099
         END                                                            00011100
