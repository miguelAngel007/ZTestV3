*ASM XOPTS(NOEPILOG)                                                    00000100
*     * 802262 12/11/20 PROYECTO UPGRADE GN172 EDVR
         SIRENT                                               2024448   00000105
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
         TITLE 'FIDLO - CALCULATE LOW DATE'                             00000500
FIDLO    DFHEIENT CODEREG=(12),DATAREG=(13),EIBREG=(11)                 00000600
FIDLO    AMODE 31                                             9913165 / 00000635
FIDLO    RMODE ANY                                            9913165 / 00000640
         L     R1,DFHEICAP         LOAD ADDR OF COMMON AREA             00000800
         B     *+24                                                     00000900
         DC    CL20'FIDLO   (SIDLO   )'                                 00001000
         AGO   .GEN2                                                    00001100
.GEN1    ANOP                                                           00001200
#DSALEN  EQU   *-CEEDSA            LENGTH OF DSA              2024448   00001205
         TITLE 'SIDLO - CALCULATE LOW DATE'                             00001300
* THE FOLLOWING STATEMENT(S) MODIFIED BY                      2024448   00001399
SIDLO CEEENTRY ,                                                       X00001400
               PPA=PPA,                                                X00001505
               AUTO=#DSALEN,                                           X00001510
               MAIN=NO,                                                X00001515
               NAB=NO,                                                 X00001520
               PARMREG=1,                                              X00001525
               BASE=9                                                   00001530
         XR    R15,R15                                        2024448   00001535
         B     INIT000                                        2024448   00001540
         USING CEECAA,R12                                     2024448   00001545
* -----------------------------------------------------------*2024448   00001550
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
               EPNAME=SIDLO,                                           X00001600
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
*-------------------------------------------------------------2024448 * 00001650
*                   ** PROGRAM DESCRIPTION **                         * 00001700
*                                                                     * 00001800
*        THIS ROUTINE DETERMINES A LOW DATE GIVEN A HIGH STARTING     * 00001900
*              DATE AND THE NO. OF DAYS BETWEEN THE DATES.            * 00002000
*        THE DATES MUST ZONED DECIMAL MONTH, DAY, YEAR.               * 00002100
*        THE DATA AREA BEING USED MUST BE IN THE ORDER:               * 00002200
*              LOW-DATE, HIGH-DATE, NO-OF-DAYS.                       * 00002300
*        THE NO-OF-DAYS MUST BE A 3 BYTE PACKED FIELD.                * 00002400
*********************************************************************** 00002500
*                                                                       00002600
*                                                                     * 00002610
*---------------------------------------------------------------------* 00002615
*                  ** HISTORY OF REVISIONS **                         * 00002620
*                                                                     * 00002625
* DESCRIPTION                                                 CHNGID  * 00002630
* __________________________________________________________  _______ * 00002635
*                                                                     * 00002640
* 04/22/00 LE ENABLED, REENTRANT                              2024448 * 00002673
* 01/13/00 ENABLE FOR 31-BIT                                  2024183 * 00002675
* 08/08/97 SET RC FOR COBOL/VSE.                              9913437 * 00002676
* 05/13/96 ADDED CAPABILITY FOR 31-BIT ADDRESSING             9913165 * 00002678
* 09/05/95 YEAR 2000.                                         2602675 * 00002680
*                                                                     * 00002685
*---------------------------------------------------------------------* 00002690
         EJECT                                                          00002695
         L     R1,0(0,R1)          LOAD ADDR OF DATE AREA INTO R1       00002700
         MVC   0(6,R1),6(R1)       MOVE INPUT DATE TO OUTPUT DATE       00002710
         TRT   0(6,R1),NUMTBL      IS DATE NUMERIC                      00002720
         BNZ   EXIT                NO                                   00002730
         CLC   0(6,R1),ZEROS       IS DATE ALL ZEROS                    00002740
         BE    EXIT                YES                                  00002750
         PACK  0(2,R1),6(2,R1)     PACK HIGH MONTH INTO LOW MONTH       00002800
         PACK  2(2,R1),8(2,R1)     PACK HIGH DAY INTO LOW DAY           00002900
         PACK  4(2,R1),10(2,R1)    PACK HIGH YEAR INTO LOW YEAR         00003000
         ZAP   HOLDDAYS,12(3,R1)   MOVE NO. OF DAYS TO HOLDDAYS         00003100
         AP    4(2,R1),=P'100'     YES - MAKE HI YR NEXT CENTURY        00003130
COMPDAY  CP    HOLDDAYS,2(2,R1)    CHK HOLDDAYS < DAYS IN MONTH         00003200
         BNL   SUBDAYS             GO TO SUBDAYS IF >=                  00003300
         SP    2(2,R1),HOLDDAYS    SUBTRACT HOLDDAYS FROM DAYS IN MON   00003400
         B     FINISH              GO TO FINISH                         00003500
SUBDAYS  SP    HOLDDAYS,2(2,R1)    SUBT DAYS IN MONTH FROM HOLDDAYS     00003600
         CP    0(2,R1),P1          CHK FOR MONTH = 1                    00003700
         BNE   SUBMO               GO TO SUBMO IF NOT EQUAL             00003800
         ZAP   0(2,R1),P13         MOVE 13 TO MONTH                     00003900
         SP    4(2,R1),P1          SUBTRACT 1 FROM YEAR                 00004000
SUBMO    SP    0(2,R1),P1          SUBTRACT 1 FROM MONTH                00004100
         ZAP   DBLWORD,0(2,R1)     MOVE MONTH TO DBLWORD                00004200
         CVB   R3,DBLWORD          CONVERT MONTH TO BINARY              00004300
         SLL   R3,1                MULTIPLY MONTH BY 2                  00004400
         LA    R3,DAYTBL-2(R3)     CALC TABLE ENTRY                     00004500
         ZAP   2(2,R1),0(2,R3)     MOVE DAYTBL DAYS TO LOW DAYS         00004600
         CP    0(2,R1),P2          CHK FOR MONTH = 2                    00004700
         BNE   COMPDAY             GO TO COMPDAY IF NOT = 2             00004800
         ZAP   DBLWORD,4(2,R1)     MOVE YEAR TO DBLWORD                 00004900
         CVB   R3,DBLWORD          CONVERT YEAR TO BINARY               00005000
         STC   R3,DBLWORD          STORE YEAR IN DBLWORD                00005100
         TM    DBLWORD,X'03'       CHK FOR LEAP YEAR                    00005200
         BNZ   COMPDAY             GO TO COMPDAY IF NOT LEAP YEAR       00005300
         AP    2(2,R1),P1          ADD ONE MORE DAY TO LOW DAYS         00005400
         B     COMPDAY             GO TO COMPDAY                        00005500
FINISH   UNPK  0(2,R1),0(2,R1)     UNPACK LOW DAY                       00005600
         UNPK  2(2,R1),2(2,R1)     UNPACK LOW MONTH                     00005700
         UNPK  4(2,R1),4(2,R1)     UNPACK LOW YEAR                      00005800
         OI    1(R1),X'F0'         CHANGE SIGN OF LOW DAY               00005900
         OI    3(R1),X'F0'         CHANGE SIGN OF LOW MONTH             00006000
         OI    5(R1),X'F0'         CHANGE SIGN OF LOW YEAR              00006100
EXIT     EQU   *                                                        00006110
         AIF   (&CICS).CICSRET                                          00006200
         CEETERM RC=(15)           RETURN                     2024448   00006300
         AGO   .DATA                                                    00006400
.CICSRET ANOP                                                           00006500
         EXEC  CICS RETURN                                              00006600
         LTORG                                                          00006700
.DATA    ANOP                                                           00006800
         EJECT                                                          00006900
P1       DC    PL1'1'                                                   00007000
P2       DC    PL1'2'                                                   00007100
P13      DC    PL2'013'                                                 00007200
DAYTBL   DC    PL2'031,028,031,030,031,030,031,031,030,031,030,031'     00007300
ZEROS    DC    CL6'000000'                                              00007310
NUMTBL   DC    256X'FF'                                                 00007320
         ORG   NUMTBL+X'F0'                                             00007330
         DC    10X'00'                                                  00007340
         ORG                                                            00007350
         AIF   ('&SYSPARM' NE 'CICS').DTSBAT                            00008085
FIDLO    CSECT                                                          00008086
         AGO   .DTSMRG                                                  00008087
.DTSBAT  ANOP                                                           00008088
SIDLO    CSECT                                                          00008089
.DTSMRG  ANOP                                                           00008090
         LTORG                                                          00008091
         DS    0D                                                       00008092
SITMSTMP DC    CL64'SIDLO     -----TSD-             06/24/02  18.50.50' 00008093
*        THIS PROGRAM CONTAINS TRADE SECRETS THAT BELONG                00008094
*        TO ALLTEL INFORMATION SERVICES, INC. AND IS                    00008095
*        LICENSED BY AN AGREEMENT.  ANY UNAUTHORIZED ACCESS,            00008096
*        USE, DUPLICATION, OR DISCLOSURE IS UNLAWFUL.                   00008097
*        COPYRIGHT ALLTEL INFORMATION SERVICES, INC.                    00008098
*        2002, ALL RIGHTS RESERVED.                                     00008099
         END                                                            00008100
