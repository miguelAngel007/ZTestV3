*ASM XOPTS(NOEPILOG)                                                    00000100
*     * 802262 12/11/20 PROYECTO UPGRADE GN172 EDVR
         SIRENT                                               2024448   00000105
**********************************************************************  00000120
*                      HISTORY OF REVISIONS                          *  00000121
*DESCRIPTION                                                  CHG ID *  00000122
*--------------------------------------------------------------------*  00000123
* 04/10/02 LE ENABLED, REENTRANT                              2024448*  00000182
* 01/07/00 ENABLE FOR 31-BIT                                  2024183*  00000184
* 08/08/97 SET RC FOR COBOL/VSE                               9913437 * 00000185
*05/09/96  ADDED CAPABILITY FOR 31-BIT ADDRESSING             9913165*  00000187
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
#UDSALEN EQU   *-UDSABEG           LENGTH OF DSA              2024448   00000294
         AIF   ('&SYSPARM' NE 'CICS').GEN1                              00000300
&CICS    SETB  1                                                        00000400
         TITLE 'FIDCHI  - DETERMINE HIGH DATE ROUTINE'                  00000500
FIDCHI   DFHEIENT CODEREG=(12),DATAREG=(13),EIBREG=(11)                 00000600
FIDCHI   AMODE 31                                             9913165 / 00000635
FIDCHI   RMODE ANY                                            9913165 / 00000640
         L     R1,DFHEICAP         LOAD ADDR OF COMMON AREA             00000800
         B     *+24                                                     00000900
         DC    CL20'FIDCHI  (SIDCHI  )'                                 00001000
         AGO   .GEN2                                                    00001100
.GEN1    ANOP                                                           00001200
#DSALEN  EQU   *-CEEDSA            LENGTH OF DSA              2024448   00001205
         TITLE 'SIDCHI  - DETERMINE HIGH DATE ROUTINE'                  00001300
* THE FOLLOWING STATEMENT(S) MODIFIED BY                      2024448   00001399
SIDCHI   CEEENTRY ,                                                    X00001400
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
               EPNAME=SIDCHI,                                          X00001600
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
         ZAP   NODAYS,0(3,R4)           MOVE NO. OF DAYS                00002200
         B     A0020                                                    00002300
A0010    EQU   *                                                        00002400
         MVC   DATEAREA,0(R2)           MOVE PARMS TO WORK AREA         00002500
A0020    EQU   *                                                        00002600
         TRT   LOWDATE,NUMTBL           CHK FOR VALID LOW DATE          00002700
         BNZ   ERREXIT                  SET HIGH DATE ALL ZEROS IF BAD  00002800
         PACK  HIYR,LOYR                                                00002900
         PACK  HIMO,LOMO                                                00003000
         PACK  HIDA,LODA                                                00003100
         ZAP   WORKDAYS,NODAYS                                          00003200
         AP    WORKDAYS,HIDA                                            00003300
CALCSUB  EQU   *                                                        00003400
         ZAP   DBLWORD,HIMO             GET MONTH DAYS FROM TABLE       00003500
         CVB   R3,DBLWORD                                               00003600
         SLL   R3,1                     MULTIPLY BY 2                   00003700
         LA    R2,DAYTBL-2(R3)                                          00003800
         ZAP   HIDA,0(2,R2)             MOVE TABLE DAYS TO HIDA         00003900
CHKLPYR  EQU   *                                                        00004000
         CP    HIMO,P2                  CHK MONTH FOR FEB               00004100
         BNE   NOLPYR                                                   00004200
         ZAP   DBLWORD,HIYR             CHK FOR LEAP YEAR               00004300
         CVB   R3,DBLWORD                                               00004400
         STC   R3,DBLWORD                                               00004500
         TM    DBLWORD,X'03'                                            00004600
         BNZ   NOLPYR                                                   00004700
         AP    HIDA,P1                  ADD 1 TO DAYS                   00004800
NOLPYR   EQU   *                                                        00004900
         CP    WORKDAYS,HIDA            CHK WORKDAYS LT HIGH DAYS       00005000
         BH    SUBDAYS                                                  00005100
         ZAP   HIDA,WORKDAYS                                            00005200
         B     FINISH                                                   00005300
SUBDAYS  EQU   *                                                        00005400
         SP    WORKDAYS,HIDA            SUBTRACT MON DAYS FROM WORKDAYS 00005500
         CP    HIMO,P12                 CHK FOR DECEMBER                00005600
         BL    ADDMO                                                    00005700
         ZAP   HIMO,P1                  MOVE 1 TO MONTH                 00005800
         AP    HIYR,P1                  ADD 1 TO YEAR                   00005900
         B     CALCSUB                                                  00006000
ADDMO    EQU   *                                                        00006100
         AP    HIMO,P1                                                  00006200
         B     CALCSUB                                                  00006300
ERREXIT  EQU   *                                                        00006400
         MVC   HIGHDATE,ZEROS                                           00006500
         B     EXIT                                                     00006600
FINISH   EQU   *                                                        00006700
         ZAP   WORKDAYS,HIYR            FORMAT OUTPUT HIGH DATE         00006800
         UNPK  HIYR,WORKDAYS                                            00006900
         UNPK  HIMO,HIMO                                                00007000
         UNPK  HIDA,HIDA                                                00007100
         OI    HIYR+3,X'F0'                                             00007200
         OI    HIMO+1,X'F0'                                             00007300
         OI    HIDA+1,X'F0'                                             00007400
EXIT     EQU   *                                                        00007500
         TM    PARMADDR,X'80'           IS 1ST PARM THE ONLY PARM       00007600
         BO    EXIT10                   YES                             00007700
         L     R4,PARMADDR+4            GET LAST PARM ADDR (NO-DAYS)    00007800
         B     EXIT20                                                   00007900
EXIT10   EQU   *                                                        00008000
         L     R4,PARMADDR              GET PARAMETER ADDRESS           00008100
         LA    R4,8(0,R4)               POINT TO NO-DAYS AREA           00008200
EXIT20   EQU   *                                                        00008300
         MVC   0(8,R4),HIGHDATE         MOVE NO-DAYS TO PARM AREA       00008400
         AIF   (&CICS).CICSRET                                          00008500
         CEETERM RC=(15)           RETURN                     2024448   00008600
         AGO   .DATA                                                    00008700
.CICSRET ANOP                                                           00008800
         EXEC  CICS RETURN                                              00008900
         LTORG                                                          00009000
.DATA    ANOP                                                           00009100
         EJECT                                                          00009200
         DS    0D                                                       00009300
ZEROS    DC    CL8'00000000'                                            00009400
P1       DC    P'1'                                                     00009500
P2       DC    P'2'                                                     00009600
P12      DC    P'012'                                                   00009700
DAYTBL   DC    PL2'031,028,031,030,031,030,031,031,030,031,030,031'     00009800
         DS    0D                                                       00009900
NUMTBL   DC    256X'FF'                                                 00010000
         ORG   NUMTBL+X'F0'                                             00010100
         DC    10X'00'                                                  00010200
         ORG                                                            00010300
         AIF   ('&SYSPARM' NE 'CICS').DTSBAT                            00012085
FIDCHI   CSECT                                                          00012086
         AGO   .DTSMRG                                                  00012087
.DTSBAT  ANOP                                                           00012088
SIDCHI   CSECT                                                          00012089
.DTSMRG  ANOP                                                           00012090
         LTORG                                                          00012091
         DS    0D                                                       00012092
SITMSTMP DC    CL64'SIDCHI    -----TSD-             06/24/02  10.25.47' 00012093
*        THIS PROGRAM CONTAINS TRADE SECRETS THAT BELONG                00012094
*        TO ALLTEL INFORMATION SERVICES, INC. AND IS                    00012095
*        LICENSED BY AN AGREEMENT.  ANY UNAUTHORIZED ACCESS,            00012096
*        USE, DUPLICATION, OR DISCLOSURE IS UNLAWFUL.                   00012097
*        COPYRIGHT ALLTEL INFORMATION SERVICES, INC.                    00012098
*        2002, ALL RIGHTS RESERVED.                                     00012099
         END                                                            00012100
