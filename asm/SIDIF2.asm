*ASM XOPTS(NOEPILOG)                                                    00000100
*     * 802262 12/11/20 PROYECTO UPGRADE GN172 EDVR
         SIRENT                                               2024448   00000102
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
LOWFEB   DS    PL2                                            2024448   00000268
HIGHFEB  DS    PL2                                            2024448   00000270
WORK1    DS    PL4                                            2024448   00000272
WORK2    DS    PL4                                            2024448   00000274
BASEYR   DS    PL2                                            2024448   00000276
#UDSALEN EQU   *-UDSABEG           LENGTH OF DSA              2024448   00000278
         AIF   ('&SYSPARM' NE 'CICS').GEN1                              00000300
&CICS    SETB  1                                                        00000400
         TITLE 'FIDIF2 - CALC DAYS BETWEEN TWO DATES - 30-DAY MONTHS'   00000500
FIDIF2   DFHEIENT CODEREG=(12),DATAREG=(13),EIBREG=(11)                 00000600
FIDIF2   AMODE 31                                             9913165 / 00000635
FIDIF2   RMODE ANY                                            9913165 / 00000640
         L     R1,DFHEICAP         LOAD ADDR OF COMMON AREA             00000800
         B     *+24                                                     00000900
         DC    CL20'FIDIF2  (SIDIF2  )'                                 00001000
         AGO   .GEN2                                                    00001100
.GEN1    ANOP                                                           00001200
#DSALEN  EQU   *-CEEDSA            LENGTH OF DSA              2024448   00001205
         TITLE 'SIDIF2 - CALC DAYS BETWEEN TWO DATES - 30-DAY MONTHS'   00001300
* THE FOLLOWING STATEMENT(S) MODIFIED BY                      2024448   00001399
SIDIF2 CEEENTRY ,                                                      X00001400
               PPA=PPA,                                                X00001505
               AUTO=#DSALEN,                                           X00001510
               MAIN=NO,                                                X00001515
               NAB=NO,                                                 X00001520
               PARMREG=1,                                              X00001525
               BASE=6                                                   00001530
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
               EPNAME=SIDIF2,                                          X00001600
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
*        THIS ROUTINE DETERMINES THE NO. OF DAYS BETWEEN TWO DATES    * 00001900
*        BASED ON 30-DAY MONTHS.                                      * 00002000
*        THE DATES MUST BE ZONED DECIMAL MONTH, DAY, YEAR.            * 00002100
*        THE DATA AREA BEING USED MUST BE IN THE ORDER:               * 00002200
*             LOW-DATE, HIGH-DATE, NO-OF-DAYS.                        * 00002300
*        THE NO-OF-DAYS FIELD MUST BE A 3 BYTE PACKED FIELD.          * 00002400
*        YEAR NUMBERS LESS THAN 31 WILL BE ASSUMED TO BE IN THE NEXT  * 00002500
*        CENTURY, THAT IS, LESS THAN 2031, NOT LESS THAN 1931         * 00002600
*        THE COBOL CALL STATEMENT IS                                  * 00002700
*             CALL 'DATDIF2' USING DATE-AREA.                         * 00002800
*                                                                     * 00002830
*---------------------------------------------------------------------* 00002835
         EJECT                                                          00002840
*---------------------------------------------------------------------* 00002845
*                  ** HISTORY OF REVISIONS **                         * 00002850
*                                                                     * 00002855
* DESCRIPTION                                                 CHNGID  * 00002860
* __________________________________________________________  _______ * 00002865
* 04/17/02 LE ENABLED, REENTRANT                              2024448 * 00002972
* 01/07/00 ENABLE FOR 31-BIT                                  2024183 * 00002974
* 06/07/99 FIX CALCULATION FOR FEBRUARY 2000 (LEAP YR)        9914086 * 00002975
* 08/08/97 SET RC FOR COBOL/VSE.                              9913437 * 00002976
* 05/13/96 ADDED CAPABILITY FOR 31-BIT ADDRESSING             9913165 * 00002978
* 08/30/95 YEAR 2000                                          2602675 * 00002980
*                                                                     * 00002985
*---------------------------------------------------------------------* 00002990
         EJECT                                                2602675   00002995
         PACK  BASEYR,C50         DEFAULT BASE YEAR           2602675   00003000
         TM    0(R1),X'80'        BASE YEAR PARAMETER PRESENT 2602675   00003010
         BO    BYOK               NO, USE DEFAULT             2602675   00003020
         L     R4,4(R1)           ADDRESS BASE YEAR           2602675   00003030
         PACK  BASEYR,0(2,R4)     PACK BASE YEAR              2602675   00003040
BYOK     EQU   *                                              2602675   00003050
         L     R1,0(0,R1)         LOAD ADDR OF DATE AREA INTO R1        00003100
         ZAP   12(3,R1),P0        ZERO NO-OF-DAYS                       00003200
         ZAP   LOWFEB,P28         SET FEBRUARY DAYS TO 28               00003300
         ZAP   HIGHFEB,P28        *                                     00003400
         TRT   0(12,R1),NUMTBL                                          00003500
         BNZ   FINISH                                                   00003600
         PACK  WORK1,4(2,R1)      PACK LOW YR INTO WORK1                00003700
         ZAP   DBLWORD,WORK1      CHECK TO SEE IF LOW DATE              00003800
         CVB   4,DBLWORD          *                                     00004000
         STC   4,DBLWORD          *                                     00004100
         TM    DBLWORD,X'03'      IS IT A LEAP YEAR                     00004200
         BNZ   LONOTLY            NO                                    00004300
         ZAP   LOWFEB,P29         YES - FEBRUARY HAS 29 DAYS            00004400
LONOTLY  EQU   *                                                        00004500
         PACK  WORK2,10(2,R1)     PACK HIGH YR INTO WORK2               00004600
         ZAP   DBLWORD,WORK2      CHECK TO SEE IF HIGH DATE             00004700
         CVB   4,DBLWORD          *                                     00004900
         STC   4,DBLWORD          *                                     00005000
         TM    DBLWORD,X'03'      IS IT A LEAP YEAR                     00005100
         BNZ   HINOTLY            NO                                    00005200
         ZAP   HIGHFEB,P29        YES - FEBRUARY HAS 29 DAYS            00005300
HINOTLY  EQU   *                                                        00005400
         CP    WORK1,BASEYR       CHK FOR LOW YEAR LT BASEYR  2602675   00005500
         BNL   CKHIYR             GO TO CKHIYR IF NOT         2602675   00005600
         AP    WORK1,P100         ADD 100 TO LOW YEAR                   00005700
CKHIYR   CP    WORK2,BASEYR       CHK FOR HIGH YEAR < BASEYR  2602675   00005800
         BNL   CALCYRS            GO TO CALCYRS IF NOT        2602675   00005900
         AP    WORK2,P100         ADD 100 TO HIGHYEAR                   00006000
CALCYRS  SP    WORK2,WORK1        SUBTRACT LOW YR FROM HIGH YR          00006100
         MP    WORK2,P360         MULTIPLY NO. OF YEARS BY 360          00006200
         ZAP   12(3,R1),WORK2+1(3) MOVE NO. OF YEARS DAYS TO NO-OF-DAYS 00006300
         PACK  WORK1,0(2,R1)      PACK LOW MONTH INTO WORK1             00006400
         PACK  WORK2,6(2,R1)      PACK HIGH MONTH INTO WORK2            00006500
         CP    WORK1,P1           CHK LOW MONTH LT 1                    00006600
         BL    CLRDAYS            GO TO CLEAR DAYS IF LOW               00006700
         CP    WORK2,P1           CHK HIGH MONTH LT 1                   00006800
         BL    CLRDAYS            GO TO CLEAR DAYS IF LOW               00006900
         CP    WORK1,P12          CHK LOW MONTH GT 12                   00007000
         BH    CLRDAYS            GO TO CLEAR DAYS IF HIGH              00007100
         CP    WORK2,P12          CHK HIGH MONTH GT 12                  00007200
         BH    CLRDAYS            GO TO CLEAR DAYS IF HIGH              00007300
         SP    WORK2,WORK1        SUBTRACT LOW MONTH FROM HIGH MONTH    00007400
         MP    WORK2,P30          MULTIPLY NO. OF MONTHS BY 30          00007500
         AP    12(3,R1),WORK2     ADD WORK2 TO NO-OF-DAYS               00007600
         PACK  WORK1,2(2,R1)      PACK LOW DAY INTO WORK1               00007700
         PACK  WORK2,8(2,R1)      PACK HIGH DAY INTO WORK2              00007800
         CP    WORK1,P1           CHK LOW DAY LT 1                      00007900
         BL    CLRDAYS            GO TO CLEAR DAYS IF LOW               00008000
         CP    WORK2,P1           CHK HIGH DAY LT 1                     00008100
         BL    CLRDAYS            GO TO CLEAR DAYS IF LOW               00008200
         CP    WORK1,P31          CHK LOW DAY GT 31                     00008300
         BH    CLRDAYS            GO TO CLEAR DAYS IF HIGH              00008400
         CP    WORK2,P31          CHK HIGH DAY GT 31                    00008500
         BH    CLRDAYS            GO TO CLEAR DAYS IF HIGH              00008600
         CP    WORK1,P30          CHK LOW DAY GT 30                     00008700
         BNH   CHKHIDA            GO TO CHKHIDA IF NOT GT 30            00008800
         ZAP   WORK1,P30          MOVE 30 TO LOW DAYS                   00008900
CHKHIDA  CP    WORK2,P30          CHK HIGH DAY GT 30                    00009000
         BNH   CHKLOFEB           GO TO CHKLOFEB IF NOT GT 30           00009100
         ZAP   WORK2,P30          MOVE 30 TO HIGH DAYS                  00009200
CHKLOFEB CLC   0(2,R1),C02        CHK LOW MONTH TO 02                   00009300
         BE    FEBLO              GO TO FEBLO IF 02                     00009400
         CLC   6(2,R1),C02        CHK HIGH MONTH TO 02                  00009500
         BE    FEBHI              GO TO FEBHI IF 02                     00009600
         B     ADDDAYS            GO TO ADDDAYS                         00009700
FEBLO    CLC   6(2,R1),C02        CHK HIGH MONTH TO 02                  00009800
         BE    BOTHFEB            GO TO BOTHFEB IF 02                   00009900
         CP    WORK1,LOWFEB       CHK LOW DAYS TO 28                    00010000
         BL    ADDDAYS            GO TO ADDDAYS IF LT 28                00010100
         CP    WORK2,HIGHFEB      CHK HIGH DAYS TO 28                   00010200
         BL    FEBLO1             GO TO FEBLO1 IF LT 28                 00010300
         ZAP   WORK1,WORK2        MOVE HIGH DAYS TO LOW DAYS            00010400
         B     ADDDAYS            GO TO ADDDAYS                         00010500
FEBLO1   ZAP   WORK1,P30          MOVE 30 TO LOW DAYS                   00010600
         B     ADDDAYS            GO TO ADDDAYS                         00010700
FEBHI    CP    WORK2,HIGHFEB      CHK HIGH DAYS TO 28                   00010800
         BL    ADDDAYS            GO TO ADDDAYS IF LT 28                00010900
         CP    WORK1,LOWFEB       CHK LOW DAYS TO 28                    00011000
         BL    FEBHI1             GO TO FEBHI1 IF LT 28                 00011100
         ZAP   WORK2,WORK1        MOVE LOW DAYS TO HIGH DAYS            00011200
         B     ADDDAYS            GO TO ADDDAYS                         00011300
FEBHI1   ZAP   WORK2,P30          MOVE 30 TO HIGH DAYS                  00011400
         B     ADDDAYS            GO TO ADDDAYS                         00011500
BOTHFEB  CP    WORK1,LOWFEB       CHK LOW DAYS TO 28                    00011600
         BL    BOTHFEB1           GO TO BOTHFEB1 IF LT 28               00011700
         ZAP   WORK1,P30          MOVE 30 TO LOW DAYS                   00011800
BOTHFEB1 CP    WORK2,HIGHFEB      CHK HIGH DAYS TO 28                   00011900
         BL    ADDDAYS            GO TO ADDDAYS IF LT 28                00012000
         ZAP   WORK2,P30          MOVE 30 TO HIGH DAYS                  00012100
ADDDAYS  AP    12(3,R1),WORK2     ADD HIGH DAYS TO NO-OF-DAYS           00012200
         SP    12(3,R1),WORK1     SUBTRACT LOW DAYS FROM NO-OF-DAYS     00012300
         CP    12(3,R1),P0        COMPARE NO-OF-DAYS TO ZERO            00012400
         BNL   FINISH             GO TO FINISH IF NOT LT 0              00012500
CLRDAYS  ZAP   12(3,R1),P0        MOVE ZERO TO NO-OF-DAYS               00012600
FINISH   EQU   *                  RESTORE REGISTERS                     00012700
         AIF   (&CICS).CICSRET                                          00012800
         CEETERM RC=(15)           RETURN                     2024448   00012900
         AGO   .DATA                                                    00013000
.CICSRET ANOP                                                           00013100
         EXEC  CICS RETURN                                              00013200
         LTORG                                                          00013300
.DATA    ANOP                                                           00013400
C02      DC    C'02'                                                    00013500
C50      DC    C'50'                                          2602675   00013510
P0       DC    PL1'0'                                                   00013600
P1       DC    PL1'1'                                                   00013700
P12      DC    PL2'012'                                                 00013800
P28      DC    PL2'028'                                                 00013900
P29      DC    PL2'029'                                                 00014000
P30      DC    PL2'030'                                                 00014100
P31      DC    PL2'031'                                                 00014200
P100     DC    PL2'100'                                                 00014300
P360     DC    PL2'360'                                                 00014400
NUMTBL   DC    256X'FF'                                                 00014500
         ORG   NUMTBL+X'F0'                                             00014600
         DC    10X'00'                                                  00014700
         ORG                                                            00014800
         AIF   ('&SYSPARM' NE 'CICS').DTSBAT                            00015885
FIDIF2   CSECT                                                          00015886
         AGO   .DTSMRG                                                  00015887
.DTSBAT  ANOP                                                           00015888
SIDIF2   CSECT                                                          00015889
.DTSMRG  ANOP                                                           00015890
         LTORG                                                          00015891
         DS    0D                                                       00015892
SITMSTMP DC    CL64'SIDIF2    -----TSD-             06/24/02  18.23.22' 00015893
*        THIS PROGRAM CONTAINS TRADE SECRETS THAT BELONG                00015894
*        TO ALLTEL INFORMATION SERVICES, INC. AND IS                    00015895
*        LICENSED BY AN AGREEMENT.  ANY UNAUTHORIZED ACCESS,            00015896
*        USE, DUPLICATION, OR DISCLOSURE IS UNLAWFUL.                   00015897
*        COPYRIGHT ALLTEL INFORMATION SERVICES, INC.                    00015898
*        2002, ALL RIGHTS RESERVED.                                     00015899
         END                                                            00015900
