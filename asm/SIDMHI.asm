*ASM XOPTS(NOEPILOG)                                                    00000100
*     * 802262 12/11/20 PROYECTO UPGRADE GN172 EDVR
         SIRENT                                               2024448   00000102
**********************************************************************  00000120
*                      HISTORY OF REVISIONS                          *  00000121
*DESCRIPTION                                                  CHG ID *  00000122
*--------------------------------------------------------------------*  00000123
* 04/22/02 LE ENABLED, REENTRANT                              2024448*  00000182
* 01/13/00 ENABLE FOR 31-BIT                                  2024183*  00000184
* 08/08/97 SET RC FOR COBOL/VSE.                              9913437 * 00000185
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
DWORD    DS    D                                              2024448   00000264
PGMPLIST DS    F                   ORIGINAL R1 SAVE AREA      2024448   00000266
WKMO     DS    PL3                                            2024448   00000268
WKDAYS   DS    CL2                                            2024448   00000270
SAVEYR   DS    PL2                                            2024448   00000272
#UDSALEN EQU   *-UDSABEG           LENGTH OF DSA              2024448   00000274
         AIF   ('&SYSPARM' NE 'CICS').GEN1                              00000300
&CICS    SETB  1                                                        00000400
         TITLE 'FIDMHI - CALCULATE HIGH DATE BY MONTHS'                 00000500
FIDMHI   DFHEIENT CODEREG=(12),DATAREG=(13),EIBREG=(11)                 00000600
FIDMHI   AMODE 31                                             9913165 / 00000635
FIDMHI   RMODE ANY                                            9913165 / 00000640
         L     R1,DFHEICAP         LOAD ADDR OF COMMON AREA             00000800
         B     *+24                                                     00000900
         DC    CL20'FIDMHI  (SIDMHI  )'                                 00001000
         AGO   .GEN2                                                    00001100
.GEN1    ANOP                                                           00001200
#DSALEN  EQU   *-CEEDSA            LENGTH OF DSA              2024448   00001205
         TITLE 'SIDMHI - CALCULATE HIGH DATE BY MONTHS'                 00001300
* THE FOLLOWING STATEMENT(S) MODIFIED BY                      2024448   00001399
SIDMHI CEEENTRY ,                                                      X00001400
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
               EPNAME=SIDMHI,                                          X00001600
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
         L     R1,0(0,R1)              GET DATE AREA ADDR               00001700
         USING DATEAREA,R1                                              00001800
         MVC   6(6,R1),0(R1)           MOVE INPUT DATE TO OUTPUT DATE   00001810
         TRT   0(6,R1),NUMTBL          IS DATE NUMERIC                  00001820
         BNZ   EXIT                    NO                               00001830
         CLC   0(6,R1),ZEROS           IS DATE ALL ZEROS                00001840
         BE    EXIT                    YES                              00001850
         MVC   HIDA,LODA               MOVE LOW DAY TO HIGH DAY         00001900
         PACK  WKMO,LOMO                                                00002000
         AP    WKMO,MOINCRM            ADD INCREMENT TO LOW MONTH       00002100
         PACK  DWORD,LOYR                                               00002200
ADJYR    EQU   *                                                        00002300
         CP    WKMO,P12                CHK MONTH GREATER THAN 12        00002400
         BNH   CHKDAY                                                   00002500
         AP    DWORD,P1                ADD 1 TO YEAR                    00002600
         SP    WKMO,P12                SUBTRACT 12 FROM MONTHS          00002700
         B     ADJYR                                                    00002800
CHKDAY   EQU   *                                                        00002900
         ZAP   SAVEYR,DWORD                                             00003000
         UNPK  HIYR,DWORD+6(2)          PUT YEAR IN HIGH YEAR           00003100
         UNPK  HIMO,WKMO                PUT MONTH IN HIGH MONTH         00003200
         OI    HIYR+1,X'F0'             FIX ZONE                        00003300
         OI    HIMO+1,X'F0'             FIX ZONE                        00003400
         ZAP   DWORD,WKMO               FIND MONTH-DAYS IN TABLE        00003500
         CVB   R2,DWORD                                                 00003600
         SLL   R2,1                                                     00003700
         LA    R3,MOTABLE-2(R2)                                         00003800
         MVC   WKDAYS,0(R3)                                             00003900
         CP    WKMO,P2                                                  00004000
         BNE   TESTDAY                                                  00004100
         ZAP   DWORD,SAVEYR                                             00004200
         CVB   R2,DWORD                  CHK FOR LEAP YEAR              00004300
         STC   R2,DWORD                                                 00004400
         TM    DWORD,X'03'                                              00004500
         BNZ   TESTDAY                                                  00004600
         MVI   WKDAYS+1,C'9'                                            00004700
TESTDAY  EQU   *                                                        00004800
         CLC   HIDA,WKDAYS               CHK FOR INVALID HIGH DAYS      00004900
         BNH   EXIT                                                     00005000
         MVC   HIDA,WKDAYS                                              00005100
EXIT     EQU   *                                                        00005200
         AIF   (&CICS).CICSRET                                          00005300
         CEETERM RC=(15)           RETURN                     2024448   00005400
         AGO   .DATA                                                    00005500
.CICSRET ANOP                                                           00005600
         EXEC  CICS RETURN                                              00005700
         LTORG                                                          00005800
.DATA    ANOP                                                           00005900
         EJECT                                                          00006000
P1       DC    P'1'                                                     00006100
P2       DC    P'2'                                                     00006200
P12      DC    P'12'                                                    00006300
MOTABLE  EQU   *                                                        00006400
JAN      DC    C'31'                                                    00006500
FEB      DC    C'28'                                                    00006600
MAR      DC    C'31'                                                    00006700
APR      DC    C'30'                                                    00006800
MAY      DC    C'31'                                                    00006900
JUN      DC    C'30'                                                    00007000
JUL      DC    C'31'                                                    00007100
AUG      DC    C'31'                                                    00007200
SEP      DC    C'30'                                                    00007300
OCT      DC    C'31'                                                    00007400
NOV      DC    C'30'                                                    00007500
DEC      DC    C'31'                                                    00007600
ZEROS    DC    CL6'000000'                                              00007610
NUMTBL   DC    256X'FF'                                                 00007620
         ORG   NUMTBL+X'F0'                                             00007630
         DC    10X'00'                                                  00007640
         ORG                                                            00007650
         SPACE 3                                                        00008400
DATEAREA DSECT                                                          00008500
LODATE   DS    0CL6                                                     00008600
LOMO     DS    CL2                                                      00008700
LODA     DS    CL2                                                      00008800
LOYR     DS    CL2                                                      00008900
HIDATE   DS    0CL6                                                     00009000
HIMO     DS    CL2                                                      00009100
HIDA     DS    CL2                                                      00009200
HIYR     DS    CL2                                                      00009300
MOINCRM  DS    PL3                                                      00009400
         AIF   ('&SYSPARM' NE 'CICS').DTSBAT                            00009485
FIDMHI   CSECT                                                          00009486
         AGO   .DTSMRG                                                  00009487
.DTSBAT  ANOP                                                           00009488
SIDMHI   CSECT                                                          00009489
.DTSMRG  ANOP                                                           00009490
         LTORG                                                          00009491
         DS    0D                                                       00009492
SITMSTMP DC    CL64'SIDMHI    -----TSD-             06/24/02  11.52.18' 00009493
*        THIS PROGRAM CONTAINS TRADE SECRETS THAT BELONG                00009494
*        TO ALLTEL INFORMATION SERVICES, INC. AND IS                    00009495
*        LICENSED BY AN AGREEMENT.  ANY UNAUTHORIZED ACCESS,            00009496
*        USE, DUPLICATION, OR DISCLOSURE IS UNLAWFUL.                   00009497
*        COPYRIGHT ALLTEL INFORMATION SERVICES, INC.                    00009498
*        2002, ALL RIGHTS RESERVED.                                     00009499
         END                                                            00009500
