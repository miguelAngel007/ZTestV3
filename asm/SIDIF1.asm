*ASM XOPTS(NOEPILOG)                                                    00000100
*     * 802262 12/11/20 PROYECTO UPGRADE GN172 EDVR
         SIRENT                                               2024448   00000110
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
LOWDAYS  DS    PL2                                            2024448   00000268
HIGHDAYS DS    PL2                                            2024448   00000270
LOWYR    DS    PL2                                            2024448   00000272
HIGHYR   DS    PL2                                            2024448   00000274
BASEYR   DS    PL2                                            2024448   00000276
SAVEMO   DS    X                                              2024448   00000280
#UDSALEN EQU   *-UDSABEG           LENGTH OF DSA              2024448   00000290
         AIF   ('&SYSPARM' NE 'CICS').GEN1                              00000300
&CICS    SETB  1                                                        00000400
         TITLE 'FIDIF1 - DETERMINE NUMBER OF DAYS BETWEEN TWO DATES'    00000500
FIDIF1   DFHEIENT CODEREG=(12),DATAREG=(13),EIBREG=(11)                 00000600
FIDIF1   AMODE 31                                             9913165 / 00000635
FIDIF1   RMODE ANY                                            9913165 / 00000640
         L     R1,DFHEICAP         LOAD ADDR OF COMMON AREA             00000800
         B     *+24                                                     00000900
         DC    CL20'FIDIF1  (SIDIF1  )'                                 00001000
         AGO   .GEN2                                                    00001100
.GEN1    ANOP                                                           00001200
#DSALEN  EQU   *-CEEDSA            LENGTH OF DSA              2024448   00001205
         TITLE 'SIDIF1 - DETERMINE NUMBER OF DAYS BETWEEN TWO DATES'    00001300
* THE FOLLOWING STATEMENT(S) MODIFIED BY                      2024448   00001399
SIDIF1 CEEENTRY ,                                                      X00001400
               PPA=PPA,                                                X00001505
               AUTO=#DSALEN,                                           X00001510
               MAIN=NO,                                                X00001515
               NAB=NO,                                                 X00001520
               PARMREG=1,                                              X00001525
               BASE=6                                                   00001530
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
               EPNAME=SIDIF1,                                          X00001600
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
*        THIS ROUTINE DETERMINES THE NO. OF DAYS BETWEEN TWO DATES.   * 00001900
*        THE DATES MUST BE ZONED DECIMAL MONTH, DAY, YEAR.            * 00002000
*        THE DATA AREA BEING USED MUST BE IN THE ORDER:               * 00002100
*             LOW-DATE, HIGH-DATE, NO-OF-DAYS.                        * 00002200
*        THE NO-OF-DAYS FIELD MUST BE A 3 BYTE PACKED FIELD.          * 00002300
*        YEAR NUMBERS LESS THAN 31 WILL BE ASSUMED TO BE IN THE NEXT  * 00002400
*        CENTURY, THAT IS, LESS THAN 2031, NOT LESS THAN 1931         * 00002500
*        THE COBOL CALL STATEMENT IS                                  * 00002600
*             CALL 'DATDIF1' USING DATE-AREA.                         * 00002700
*                                                                     * 00002705
*---------------------------------------------------------------------* 00002710
         EJECT                                                          00002715
*---------------------------------------------------------------------* 00002720
*                  ** HISTORY OF REVISIONS **                         * 00002725
*                                                                     * 00002730
* DESCRIPTION                                                 CHNGID  * 00002735
* __________________________________________________________  _______ * 00002740
* 04/16/01 LE ENABLED, REENTRANT                              2024448 * 00002873
* 01/07/00 ENABLE FOR 31-BIT                                  2024183 * 00002875
* 08/08/97 SET RC FOR COBOL/VSE.                              9913437 * 00002876
* 05/13/96 ADDED CAPABILITY FOR 31-BIT ADDRESSING             9913165 * 00002878
* 08/30/95 YEAR 2000                                          2602675 * 00002880
*                                                                     * 00002885
*---------------------------------------------------------------------* 00002890
         EJECT                                                2600746   00002895
         PACK  BASEYR,C50         DEFAULT BASE YEAR           2602675   00002900
         TM    0(R1),X'80'        BASE YEAR PARAMETER PRESENT 2602675   00002910
         BO    BYOK               NO, USE DEFAULT             2602675   00002920
         L     R4,4(R1)           ADDRESS BASE YEAR           2602675   00002930
         PACK  BASEYR,0(2,R4)     PACK BASE YEAR              2602675   00002940
BYOK     EQU   *                                              2602675   00002950
         L     R1,0(0,R1)           LOAD ADDR OF LOW-DATE INTO R1       00003000
         ZAP   12(3,R1),P0         ZERO NO-OF-DAYS                      00003100
         TRT   0(12,R1),NUMTBL    CHK FOR NUMERIC DATES                 00003200
         BNZ   FINISH                                                   00003300
         PACK  LOWYR,4(2,R1)      PACK LOW YEAR                         00003400
         PACK  HIGHYR,10(2,R1)    PACK HIGH YEAR                        00003500
         CP    LOWYR,BASEYR       CHK FOR LOW YEAR < BASEYR   2602675   00003600
         BNL   CKHIYR             GO TO CHHIYR IF NOT         2602675   00003700
         AP    LOWYR,P100         ADD 100 TO LOW YEAR                   00003800
CKHIYR   CP    HIGHYR,BASEYR      CHK FOR HIGH YEAR < BASEYR  2602675   00003900
         BNL   COMPYRS            GO TO COMPYRS IF NOT        2602675   00004000
         AP    HIGHYR,P100        ADD 100 TO HIGH YEAR                  00004100
COMPYRS  CP    LOWYR,HIGHYR       CHK FOR LOW YEAR GT HIGH YEAR         00004200
         BH    FINISH             GO TO FINISH IF LOW YEAR GT HIGH YEAR 00004300
         BL    STARTLOW           GO TO STARTLOW IF LOW YR LT HIGH YR   00004400
         CLC   0(4,R1),6(R1)      CHK FOR LOW-MO-DA GT HIGH MO-DA       00004500
         BNL   FINISH             GO TO FINISH IF LOW MO-DA GE HI MO-DA 00004600
STARTLOW EQU   *                                                        00004700
         LR    R7,R1              LOAD ADDR OF LOW-DATE INTO R7         00004800
         LA    R8,LOWDAYS         LOAD ADDR OF LOWDAYS INTO R8          00004900
         LA    R9,STARTHI         LOAD ADDR OF STARTHI INTO R9          00005000
CALCDAYS PACK  DBLWORD,0(2,R7)    PACK MONTH                            00005100
         CVB   R4,DBLWORD         CONVERT MONTH TO BINARY               00005200
         CH    R4,B1              CHK MONTH LT 1                        00005300
         BL    FINISH             GO TO FINISH IF LT 1                  00005400
         CH    R4,B12             CHK FOR MONTH GT 12                   00005500
         BH    FINISH             GO TO FINISH IF GT 12                 00005600
         STC   R4,SAVEMO                                                00005700
         SLL   R4,1               MULTIPLY R4 BY 2                      00005800
         LA    R2,DAYTBL-2(R4)                                          00005900
         MVC   0(2,R8),0(R2)      MOVE DAY TABLE DAYS TO HOLD-DAYS LOC  00006000
         PACK  DBLWORD,2(2,R7)    PACK DAYS INTO DLBWORD                00006100
         CP    DBLWORD+6(2),P1    CHK FOR DAY LT 1                      00006200
         BL    FINISH             GO TO FINISH IF LT 1                  00006300
         CP    DBLWORD+6(2),DAYTBL+2(2) CHK FOR DAYS GT 31              00006400
         BH    FINISH             GO TO FINISH IF GT 31                 00006500
         AP    0(2,R8),DBLWORD+6(2) ADD DAYS TO HOLD DAYS LOCATION      00006600
         PACK  DBLWORD,4(2,R7)    PACK YEAR                             00006700
         CVB   R5,DBLWORD         CONVERT YEAR TO BINARY                00006800
         STC   R5,DBLWORD         STORE YEAR IN DBLWORD                 00006900
         TM    DBLWORD,X'03'      CHK FOR YEAR DIVISIBLE BY 4           00007000
         BNZR  R9                 GO TO LINK ADDR IF NOT LEAP YEAR      00007100
         CLI   SAVEMO,X'02'       CHK FOR FEBRUARY                      00007200
         BNHR  R9                 GO TO LINK ADDR IF JAN OR FEB         00007300
         AP    0(2,R8),P1         ADD 1 TO HOLD-DAYS LOCATION           00007400
         BR    R9                 GO TO LINK ADDR                       00007500
STARTHI  LA    R7,6(R1)           LOAD ADDR OF HIGH-DATE INTO R7        00007600
         LA    R8,HIGHDAYS        LOAD ADDR OF HIGHDAYS INTO R8         00007700
         BAS   R9,CALCDAYS        BR & LINK ON R9 TO CALCDAYS 2024183   00007800
         ZAP   12(3,R1),HIGHDAYS  MOVE HIGHDAYS TO NO-OF-DAYS           00007900
         SP    12(3,R1),LOWDAYS   SUBTRACT LOWDAYS                      00008000
         ZAP   DBLWORD,LOWYR                                            00008100
         CVB   R4,DBLWORD         CONVERT LOW YEAR TO BINARY            00008200
         ZAP   DBLWORD,HIGHYR                                           00008300
         CVB   R5,DBLWORD         CONVERT HIGH TO BINARY                00008400
CKLEAPYR CR    R4,R5              CHK FOR LOW-YEAR LT HIGH-YEAR         00008500
         BNL   FINISH             GO TO FINISH IF NOT LOW               00008600
         AP    12(3,R1),P365      ADD 365 TO NO-OF-DAYS                 00008700
         STC   R4,DBLWORD         STORE LOW YEAR IN DBLWORD             00008800
         TM    DBLWORD,X'03'      CHK FOR YEAR DIVISIBLE BY 4           00008900
         BNZ   ADDYR              GO TO ADDYR IF NOT LEAP YEAR          00009000
         AP    12(3,R1),P1        ADD 1 TO NO-OF-DAYS                   00009100
ADDYR    LA    R4,1(0,R4)         ADD 1 TO LOW YEAR                     00009200
         B     CKLEAPYR           GO TO CKLEAPYR                        00009300
FINISH   EQU   *                  RESTORE REGISTERS                     00009400
         AIF   (&CICS).CICSRET                                          00009500
         CEETERM RC=(15)           RETURN                     2024448   00009600
         AGO   .DATA                                                    00009700
.CICSRET ANOP                                                           00009800
         EXEC  CICS RETURN                                              00009900
         LTORG                                                          00010000
.DATA    ANOP                                                           00010100
C50      DC    CL2'50'                                        2602675   00010110
P0       DC    PL1'0'                                                   00010200
P1       DC    PL1'1'                                                   00010300
P100     DC    PL2'100'                                                 00010400
P365     DC    PL2'365'                                                 00010500
DAYTBL   DC    PL2'000'                                                 00010600
         DC    PL2'031'                                                 00010700
         DC    PL2'059'                                                 00010800
         DC    PL2'090'                                                 00010900
         DC    PL2'120'                                                 00011000
         DC    PL2'151'                                                 00011100
         DC    PL2'181'                                                 00011200
         DC    PL2'212'                                                 00011300
         DC    PL2'243'                                                 00011400
         DC    PL2'273'                                                 00011500
         DC    PL2'304'                                                 00011600
         DC    PL2'334'                                                 00011700
B1       DC    H'1'                                                     00011800
B12      DC    H'12'                                                    00011900
NUMTBL   DC    256X'FF'                                                 00012000
         ORG   NUMTBL+X'F0'                                             00012100
         DC    10X'00'                                                  00012200
         ORG                                                            00012300
         AIF   ('&SYSPARM' NE 'CICS').DTSBAT                            00013485
FIDIF1   CSECT                                                          00013486
         AGO   .DTSMRG                                                  00013487
.DTSBAT  ANOP                                                           00013488
SIDIF1   CSECT                                                          00013489
.DTSMRG  ANOP                                                           00013490
         LTORG                                                          00013491
         DS    0D                                                       00013492
SITMSTMP DC    CL64'SIDIF1    -----TSD-             06/24/02  13.15.19' 00013493
*        THIS PROGRAM CONTAINS TRADE SECRETS THAT BELONG                00013494
*        TO ALLTEL INFORMATION SERVICES, INC. AND IS                    00013495
*        LICENSED BY AN AGREEMENT.  ANY UNAUTHORIZED ACCESS,            00013496
*        USE, DUPLICATION, OR DISCLOSURE IS UNLAWFUL.                   00013497
*        COPYRIGHT ALLTEL INFORMATION SERVICES, INC.                    00013498
*        2002, ALL RIGHTS RESERVED.                                     00013499
         END                                                            00013500
