*ASM XOPTS(NOEPILOG)                                                    00000100
*     * 802262 12/11/20 PROYECTO UPGRADE GN172 EDVR
         SIRENT                                               2024448   00000102
**********************************************************************  00000120
*                      HISTORY OF REVISIONS                          *  00000121
*DESCRIPTION                                                  CHG ID *  00000122
*--------------------------------------------------------------------*  00000123
* 04/10/02 LE ENABLE, REENTRANT                               2024448*  00000182
* 01/07/00 ENABLE FOR 31-BIT                                  2024183*  00000184
* 08/08/97 SET RC FOR COBOL/VSE                               9913437 * 00000185
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
         TITLE 'FIDCMHI - ROUTINE TO CALCULATE HIGH DATE BY MONTHS'     00000500
FIDCMHI  DFHEIENT CODEREG=(12),DATAREG=(13),EIBREG=(11)                 00000600
FIDCMHI  AMODE 31                                             9913165 / 00000635
FIDCMHI  RMODE ANY                                            9913165 / 00000640
         L     R1,DFHEICAP         LOAD ADDR OF COMMON AREA             00000800
         B     *+24                                                     00000900
         DC    CL20'FIDCMHI (SIDCMHI )'                                 00001000
         AGO   .GEN2                                                    00001100
.GEN1    ANOP                                                           00001200
#DSALEN  EQU   *-CEEDSA            LENGTH OF DSA              2024448   00001205
         TITLE 'SIDCMHI - ROUTINE TO CALCULATE HIGH DATE BY MONTHS'     00001300
* THE FOLLOWING STATEMENT(S) MODIFIED BY                      2024448   00001399
SIDCMHI CEEENTRY ,                                                     X00001400
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
               EPNAME=SIDCMHI,                                         X00001600
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
         MVC   LODATE,0(R2)             MOVE LOW DATE TO WORK AREA      00002100
         ZAP   MOINCRM,0(3,R4)          MOVE NO. OF DAYS                00002200
         B     A0020                                                    00002300
A0010    EQU   *                                                        00002400
         MVC   DATEAREA,0(R2)           MOVE PARMS TO WORK AREA         00002500
A0020    EQU   *                                                        00002600
         MVC   HIDATE,LODATE            MOVE INPUT DATE TO OUTPUT DATE  00002700
         TRT   LODATE,NUMTBL            CHK FOR VALID LOW DATE          00002800
         BNZ   ERREXIT                  SET HIGH DATE ALL ZEROS IF BAD  00002900
         CLC   LODATE,ZEROS             IS DATE ALL ZEROS               00003000
         BE    ERREXIT                  YES                             00003100
         MVC   HIDA,LODA                MOVE LOW DAY TO HIGH DAY        00003200
         PACK  WKMO,LOMO                                                00003300
         AP    WKMO,MOINCRM             ADD INCREMENT TO LOW MONTH      00003400
         PACK  DWORD,LOYR                                               00003500
ADJYR    EQU   *                                                        00003600
         CP    WKMO,P12                 CHK MONTH GREATER THAN 12       00003700
         BNH   CHKDAY                                                   00003800
         AP    DWORD,P1                 ADD 1 TO YEAR                   00003900
         SP    WKMO,P12                 SUBTRACT 12 FROM MONTHS         00004000
         B     ADJYR                                                    00004100
CHKDAY   EQU   *                                                        00004200
         UNPK  HIYR,DWORD+5(3)          PUT YEAR IN HIGH YEAR           00004300
         UNPK  HIMO,WKMO                PUT MONTH IN HIGH MONTH         00004400
         OI    HIYR+3,X'F0'             FIX ZONE                        00004500
         OI    HIMO+1,X'F0'             FIX ZONE                        00004600
         ZAP   DWORD,WKMO               FIND MONTH-DAYS IN TABLE        00004700
         CVB   R2,DWORD                                                 00004800
         SLL   R2,1                                                     00004900
         LA    R3,MOTABLE-2(R2)                                         00005000
         MVC   DAYSINMO,0(R3)                                           00005100
         CP    WKMO,P2                   IS IT FEBRUARY                 00005200
         BNE   TESTDAY                   NO                             00005300
         PACK  DWORD,HIYR                                               00005400
         CVB   R2,DWORD                  CHK FOR LEAP YEAR              00005500
         STC   R2,DWORD                                                 00005600
         TM    DWORD,X'03'                                              00005700
         BNZ   TESTDAY                                                  00005800
         MVI   DAYSINMO+1,C'9'                                          00005900
TESTDAY  EQU   *                                                        00006000
         CLC   HIDA,DAYSINMO             CHK FOR INVALID HIGH DAYS      00006100
         BNH   EXIT                                                     00006200
         MVC   HIDA,DAYSINMO                                            00006300
         B     EXIT                                                     00006400
ERREXIT  EQU   *                                                        00006500
         MVC   HIDATE,ZEROS                                             00006600
EXIT     EQU   *                                                        00006700
         TM    PARMADDR,X'80'           IS 1ST PARM THE ONLY PARM       00006800
         BO    EXIT10                   YES                             00006900
         L     R4,PARMADDR+4            GET LAST PARM ADDR (NO-DAYS)    00007000
         B     EXIT20                                                   00007100
EXIT10   EQU   *                                                        00007200
         L     R4,PARMADDR              GET PARAMETER ADDRESS           00007300
         LA    R4,8(0,R4)               POINT TO NO-DAYS AREA           00007400
EXIT20   EQU   *                                                        00007500
         MVC   0(8,R4),HIDATE           MOVE NO-DAYS TO PARM AREA       00007600
         AIF   (&CICS).CICSRET                                          00007700
         CEETERM RC=(15)           RETURN                     2024448   00007800
         AGO   .DATA                                                    00007900
.CICSRET ANOP                                                           00008000
         EXEC  CICS RETURN                                              00008100
         LTORG                                                          00008200
.DATA    ANOP                                                           00008300
         EJECT                                                          00008400
         DS    0D                                                       00008500
ZEROS    DC    CL8'00000000'                                            00008600
P1       DC    PL1'1'                                                   00008700
P2       DC    PL1'2'                                                   00008800
P12      DC    PL2'012'                                                 00008900
MOTABLE  DC    CL24'312831303130313130313031'                           00009000
         DS    0D                                                       00009100
NUMTBL   DC    256X'FF'                                                 00009200
         ORG   NUMTBL+X'F0'                                             00009300
         DC    10X'00'                                                  00009400
         ORG                                                            00009500
         SPACE 3                                                        00009600
         AIF   ('&SYSPARM' NE 'CICS').DTSBAT                            00011485
FIDCMHI  CSECT                                                          00011486
         AGO   .DTSMRG                                                  00011487
.DTSBAT  ANOP                                                           00011488
SIDCMHI  CSECT                                                          00011489
.DTSMRG  ANOP                                                           00011490
         LTORG                                                          00011491
         DS    0D                                                       00011492
SITMSTMP DC    CL64'SIDCMHI   -----TSD-             06/24/02  10.51.24' 00011493
*        THIS PROGRAM CONTAINS TRADE SECRETS THAT BELONG                00011494
*        TO ALLTEL INFORMATION SERVICES, INC. AND IS                    00011495
*        LICENSED BY AN AGREEMENT.  ANY UNAUTHORIZED ACCESS,            00011496
*        USE, DUPLICATION, OR DISCLOSURE IS UNLAWFUL.                   00011497
*        COPYRIGHT ALLTEL INFORMATION SERVICES, INC.                    00011498
*        2002, ALL RIGHTS RESERVED.                                     00011499
         END                                                            00011500
