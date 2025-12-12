*ASM XOPTS(NOEPILOG)                                                    00000100
*     * 802262 12/11/20 PROYECTO UPGRADE GN172 EDVR
         SIRENT                                               2024448   00000102
**********************************************************************  00000120
*                      HISTORY OF REVISIONS                          *  00000121
*DESCRIPTION                                                  CHG ID *  00000122
*--------------------------------------------------------------------*  00000123
* 04/10/02 LE ENABLED, REENTRANT                              2024448*  00000182
* 01/07/00 ENABLE FOR 31-BIT                                  2024183*  00000184
* 08/08/97 SET RC FOR COBOL/VSE                               9913437 * 00000185
*05/13/96  ADDED CAPABILITY FOR 31-BIT ADDRESSING             9913165*  00000187
*                                                                    *  00000188
**********************************************************************  00000189
         COPY  SIOPTNS                                        9913165   00000190
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
DWORD    DS    D                                              2024448   00000266
PARMADDR DS    3F                                             2024448   00000268
WKMO     DS    PL3                                            2024448   00000270
DAYSINMO DS    CL2                                            2024448   00000272
         DS    0D                                             2024448   00000274
DATEAREA DS    0CL19                                          2024448   00000276
LODATE   DS    0CL8                                           2024448   00000278
LOYR     DS    CL4                                            2024448   00000280
LOMO     DS    CL2                                            2024448   00000282
LODA     DS    CL2                                            2024448   00000284
HIDATE   DS    0CL8                                           2024448   00000286
HIYR     DS    CL4                                            2024448   00000288
HIMO     DS    CL2                                            2024448   00000290
HIDA     DS    CL2                                            2024448   00000292
MOINCRM  DS    PL3                                            2024448   00000294
#UDSALEN EQU   *-UDSABEG           LENGTH OF DSA              2024448   00000296
         AIF   ('&SYSPARM' NE 'CICS').GEN1                              00000300
&CICS    SETB  1                                                        00000400
         TITLE 'FIDCMLO - ROUTINE TO CALCULATE LOW DATE BY MONTHS'      00000500
FIDCMLO  DFHEIENT CODEREG=(12),DATAREG=(13),EIBREG=(11)                 00000600
FIDCMLO  AMODE 31                                             9913165 / 00000635
FIDCMLO  RMODE ANY                                            9913165 / 00000640
         L     R1,DFHEICAP         LOAD ADDR OF COMMON AREA             00000800
         B     *+24                                                     00000900
         DC    CL20'FIDCMLO (SIDCMLO )'                                 00001000
         AGO   .GEN2                                                    00001100
.GEN1    ANOP                                                           00001200
#DSALEN  EQU   *-CEEDSA            LENGTH OF DSA              2024448   00001205
         TITLE 'SIDCMLO  - ROUTINE TO CALCULATE LOW DATE BY MONTHS'     00001300
* THE FOLLOWING STATEMENT(S) MODIFIED BY                      2024448   00001399
SIDCMLO CEEENTRY ,                                                     X00001400
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
* THE FOLLOWING STATEMENT(S) MODIFIED BY                      2024448   00001562
PPA      CEEPPA ,                                                      X00001565
               LIBRARY=NO,                                             X00001570
               PPA2=YES,                                               X00001575
               EXTPROC=YES,                                            X00001580
               TSTAMP=YES,                                             X00001585
               PEP=YES,                                                X00001590
               INSTOP=YES,                                             X00001595
               EPNAME=SIDCMLO,                                         X00001600
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
         LM    R2,R4,0(R1)             LOAD PARAMETER ADDRESSES         00001700
         STM   R2,R4,PARMADDR          SAVE PARAMETER ADDRESSES         00001800
         LTR   R2,R2                   IS THE 1ST PARM THE LAST PARM    00001900
         BM    A0010                   YES - MOVE ONE PARM              00002000
         MVC   HIDATE,0(R3)            MOVE HIGH DATE TO WORK AREA      00002100
         ZAP   MOINCRM,0(3,R4)         MOVE NO. OF DAYS                 00002200
         B     A0020                                                    00002300
A0010    EQU   *                                                        00002400
         MVC   DATEAREA,0(R2)          MOVE PARMS TO WORK AREA          00002500
A0020    EQU   *                                                        00002600
         TRT   HIDATE,NUMTBL           CHK FOR VALID HIGH DATE          00002700
         BNZ   ERREXIT                 SET LOW DATE ALL ZEROS IF BAD    00002800
         MVC   LODA,HIDA               MOVE HIGH DAY TO LOW DAY         00002900
         PACK  WKMO,HIMO                                                00003000
         SP    WKMO,MOINCRM            SUBTRACT INCREMENT FROM HIGH MO  00003100
         PACK  DWORD,HIYR                                               00003200
ADJYR    EQU   *                                                        00003300
         CP    WKMO,P1                                                  00003400
         BNL   CHKDAY                                                   00003500
         SP    DWORD,P1                SUBTRACT 1 FROM YEAR             00003600
         AP    WKMO,P12                ADD 12 TO MONTHS                 00003700
         B     ADJYR                                                    00003800
CHKDAY   EQU   *                                                        00003900
         UNPK  LOYR,DWORD+5(3)         PUT YEAR IN LOW YEAR             00004000
         UNPK  LOMO,WKMO               PUT MONTH IN LOW MONTH           00004100
         OI    LOYR+3,X'F0'            FIX ZONE                         00004200
         OI    LOMO+1,X'F0'            FIX ZONE                         00004300
         ZAP   DWORD,WKMO              FIND MONTH-DAYS IN TABLE         00004400
         CVB   R2,DWORD                                                 00004500
         SLL   R2,1                                                     00004600
         LA    R3,MOTABLE-2(R2)                                         00004700
         MVC   DAYSINMO,0(R3)                                           00004800
         CP    WKMO,P2                                                  00004900
         BNE   TESTDAY                                                  00005000
         PACK  DWORD,LOYR                                               00005100
         CVB   R2,DWORD                 CHK FOR LEAP YEAR               00005200
         STC   R2,DWORD                                                 00005300
         TM    DWORD,X'03'                                              00005400
         BNZ   TESTDAY                                                  00005500
         MVI   DAYSINMO+1,C'9'                                          00005600
TESTDAY  EQU   *                                                        00005700
         CLC   LODA,DAYSINMO            CHK FOR INVALID LOW DAYS        00005800
         BNH   EXIT                                                     00005900
         MVC   LODA,DAYSINMO                                            00006000
         B     EXIT                                                     00006100
ERREXIT  EQU   *                                                        00006200
         MVC   LODATE,ZEROS                                             00006300
EXIT     EQU   *                                                        00006400
         L     R4,PARMADDR              GET PARAMETER ADDRESS           00006500
         MVC   0(8,R4),LODATE           MOVE NO-DAYS TO PARM AREA       00006600
         AIF   (&CICS).CICSRET                                          00006700
         CEETERM RC=(15)           RETURN                     2024448   00006800
         AGO   .DATA                                                    00006900
.CICSRET ANOP                                                           00007000
         EXEC  CICS RETURN                                              00007100
         LTORG                                                          00007200
.DATA    ANOP                                                           00007300
         EJECT                                                          00007500
         DS    0D                                                       00007600
ZEROS    DC    CL8'00000000'                                            00007700
P1       DC    PL1'1'                                                   00007800
P2       DC    PL1'2'                                                   00007900
P12      DC    PL2'012'                                                 00008000
MOTABLE  DC    CL24'312831303130313130313031'                           00008100
         DS    0D                                                       00008200
NUMTBL   DC    256X'FF'                                                 00008300
         ORG   NUMTBL+X'F0'                                             00008400
         DC    10X'00'                                                  00008500
         ORG                                                            00008600
         SPACE 3                                                        00008700
         AIF   ('&SYSPARM' NE 'CICS').DTSBAT                            00010585
FIDCMLO  CSECT                                                          00010586
         AGO   .DTSMRG                                                  00010587
.DTSBAT  ANOP                                                           00010588
SIDCMLO  CSECT                                                          00010589
.DTSMRG  ANOP                                                           00010590
         LTORG                                                          00010591
         DS    0D                                                       00010592
SITMSTMP DC    CL64'SIDCMLO   -----TSD-             06/24/02  10.55.07' 00010593
*        THIS PROGRAM CONTAINS TRADE SECRETS THAT BELONG                00010594
*        TO ALLTEL INFORMATION SERVICES, INC. AND IS                    00010595
*        LICENSED BY AN AGREEMENT.  ANY UNAUTHORIZED ACCESS,            00010596
*        USE, DUPLICATION, OR DISCLOSURE IS UNLAWFUL.                   00010597
*        COPYRIGHT ALLTEL INFORMATION SERVICES, INC.                    00010598
*        2002, ALL RIGHTS RESERVED.                                     00010599
         END                                                            00010600
