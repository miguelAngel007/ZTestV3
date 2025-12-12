*ASM XOPTS(NOEPILOG)                                                    00000100
*     * 802262 12/11/20 PROYECTO UPGRADE GN172 EDVR
         SIRENT                                               2024448   00000102
         LCLB  &CICS                                                    00000200
         SIEQREG                                              2024448   00000202
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
#UDSALEN EQU   *-UDSABEG           LENGTH OF DSA              2024448   00000268
         AIF   ('&SYSPARM' NE 'CICS').GEN1                              00000300
&CICS    SETB  1                                                        00000400
         TITLE 'FIDJUL - CALCULATE JULIAN DATE'                         00000500
FIDJUL   DFHEIENT CODEREG=(12),DATAREG=(13),EIBREG=(11)                 00000600
FIDJUL   AMODE 31                                             9913165 / 00000635
FIDJUL   RMODE ANY                                            9913165 / 00000640
         L     R1,DFHEICAP         LOAD ADDR OF COMMON AREA             00000800
         B     *+24                                                     00000900
         DC    CL20'FIDJUL  (SIDJUL  )'                                 00001000
         AGO   .GEN2                                                    00001100
.GEN1    ANOP                                                           00001200
#DSALEN  EQU   *-CEEDSA            LENGTH OF DSA              2024448   00001205
         TITLE 'SIDJUL - CALCULATE JULIAN DATE'                         00001300
* THE FOLLOWING STATEMENT(S) MODIFIED BY                      2024448   00001399
SIDJUL CEEENTRY ,                                                      X00001400
               PPA=PPA,                                                X00001505
               AUTO=#DSALEN,                                           X00001510
               MAIN=NO,                                                X00001515
               NAB=NO,                                                 X00001520
               PARMREG=1,                                              X00001525
               BASE=9                                                   00001530
         XR    R15,R15                                        2024448   00001535
         B     INIT000                                        2024448   00001540
         USING CEECAA,R12                                     2024448   00001545
* ------------------------------------------------------------------- * 00001550
*                  PROLOG AREA                                        * 00001555
* ------------------------------------------------------------------- * 00001560
* THE FOLLOWING STATEMENT(S) MODIFIED BY                      2024448   00001564
PPA      CEEPPA ,                                                      X00001565
               LIBRARY=NO,                                             X00001570
               PPA2=YES,                                               X00001575
               EXTPROC=YES,                                            X00001580
               TSTAMP=YES,                                             X00001585
               PEP=YES,                                                X00001590
               INSTOP=YES,                                             X00001595
               EPNAME=SIDJUL,                                          X00001600
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
*        THIS ROUTINE DETERMINES THE JULIAN DATE OF A GIVEN DATE.     * 00001900
*        THE COBOL CALL STATEMENT IS                          2602675 * 00002000
*              CALL 'DATEJUL' USING DATE-AREA.                2602675 * 00002010
*              CALL 'DATEJUL' USING DATE-AREA FORMAT.         2602675 * 00002020
*        FORMAT VALUE OF '8' TO CONVERT MMDDCCYY TO CCYYDDD.  2602675 * 00002030
*        ANY OTHER FORMAT VALUE TO CONVERT MMDDYY TO YYDDD.   2602675 * 00002040
*        THE DATA AREA BEING USED MUST BE IN THE ORDER:               * 00002100
*              DATE, YEAR, DAYS.                                      * 00002200
*        THE DATE MUST BE ZONED DECIMAL MONTH, DAY, YEAR.     2602675 * 00002210
*        FOR FORMAT '8' MONTH, DAY, CENTURY, YEAR.            2602675 * 00002300
*        THE YEAR FIELD MUST BE 2 BYTES PACKED.               2602675 * 00002310
*        FOR FORMAT '8' THE YEAR FIELD MUST BE 3 BYTES PACKED.2602675 * 00002320
*        THE DAYS FIELD MUST BE 2 BYTES PACKED.               2602675 * 00002330
*********************************************************************** 00002600
*                                                                       00002700
*                                                                     * 00002720
*---------------------------------------------------------------------* 00002725
*                  ** HISTORY OF REVISIONS **                         * 00002730
*                                                                     * 00002735
* DESCRIPTION                                                 CHNGID  * 00002740
* __________________________________________________________  _______ * 00002745
*                                                                     * 00002750
* 04/19/02 LE ENABLED, REENTRANT                              2024448 * 00002773
* 01/13/00 ENABLE FOR 31-BIT                                  2024183 * 00002775
* 08/08/97 SET RC FOR COBOL/VSE                               9913437 * 00002776
* 05/13/96 ADDED CAPABILITY FOR 31-BIT ADDRESSING             9913165 * 00002778
* 09/05/95 YEAR 2000                                          2602675 * 00002780
*                                                                     * 00002785
*---------------------------------------------------------------------* 00002790
         EJECT                                                          00002795
         LA    R2,FORM6              DEFAULT FORMAT           2602675   00002800
         TM    0(R1),X'80'           FORMAT PRESENT?          2602675   00002810
         BO    FORMOK                NO, DEFAULT              2602675   00002820
         L     R2,4(R1)              ADDRESS FORMAT           2602675   00002830
FORMOK   EQU   *                                              2602675   00002840
         L     R1,0(0,R1)            LOAD ADDR PARAMETERS     2602675   00002850
         CLI   0(R2),C'8'            CENTURY IN YEAR?         2602675   00002860
         BE    FORM8RT               YES, GO PROCESS          2602675   00002870
         PACK  6(2,R1),4(2,R1)       PACK YEAR INTO RETURN YEAR         00002900
         PACK  8(2,R1),2(2,R1)      PACK DAYS INTO RETURN DAYS          00003000
         PACK  DBLWORD,0(2,R1)      PACK MONTH INTO DBLWORD             00003100
         CVB   R2,DBLWORD           CONVERT MONTH TO BINARY             00003200
         SLL   R2,1                 MULTIPLY MONTH BY 2                 00003300
         LA    R3,DAYTBL-2(2)       POINT TO DAYTBL ENTRY               00003400
         AP    8(2,R1),0(2,R3)      ADD DAYTBL DAYS TO RETURN DAYS      00003500
         CP    DBLWORD+6(R2),P2     CHK MONTH GT 2                      00003600
         BNH   FINISH               GO TO FINISH IF LE                  00003700
         ZAP   DBLWORD,6(2,R1)      MOVE YEAR TO DBLWORD                00003800
         CVB   R3,DBLWORD           CONVERT YEAR TO BINARY              00003900
         STC   R3,DBLWORD           STORE YEAR IN DBLWORD               00004000
         TM    DBLWORD,X'03'        CHK FOR LEAP YEAR                   00004100
         BNZ   FINISH               GO TO FINISH IF NOT LEAP YEAR       00004200
         AP    8(2,R1),P1           ADD 1 TO RETURN DAYS                00004300
         B     FINISH               GO TO FINISH              2602675   00004305
FORM8RT  EQU   *                                              2602675   00004310
         PACK  8(3,R1),4(4,R1)      PACK CCYY INTO RETURN CCYY2602675   00004315
         PACK  11(2,R1),2(2,R1)     PACK DAYS INTO RETURN DAYS2602675   00004320
         PACK  DBLWORD,0(2,R1)      PACK MONTH INTO DBLWORD   2602675   00004325
         CVB   R2,DBLWORD           CONVERT MONTH TO BINARY   2602675   00004330
         SLL   R2,1                 MULTIPLY MONTH BY 2       2602675   00004335
         LA    R3,DAYTBL-2(2)       POINT TO DAYTBL ENTRY     2602675   00004340
         AP    11(2,R1),0(2,R3)     ADD DAYTBL DAYS, RET DAYS 2602675   00004345
         CP    DBLWORD+6(R2),P2     CHK MONTH GT 2            2602675   00004350
         BNH   FINISH               GO TO FINISH IF LE        2602675   00004355
         PACK  DBLWORD,6(2,R1)      MOVE YEAR TO DBLWORD      2602675   00004360
         CP    DBLWORD,=P'0'        CENTURY?                  2602675   00004365
         BNE   FORM8RTA             NO, GO CHECK LEAP YEAR    2602675   00004370
         PACK  DBLWORD,4(2,R1)      MOVE CENTURY TO DBLWORD   2602675   00004375
FORM8RTA EQU   *                                              2602675   00004380
         CVB   R3,DBLWORD           CONVERT TO BINARY         2602675   00004383
         STC   R3,DBLWORD           STORE YEAR IN DBLWORD     2602675   00004386
         TM    DBLWORD,X'03'        CHK FOR LEAP YEAR         2602675   00004389
         BNZ   FINISH               GO TO FINISH IF NOT       2602675   00004392
         AP    11(2,R1),P1          ADD 1 TO RETURN DAYS      2602675   00004395
FINISH   EQU   *                                                        00004400
         AIF   (&CICS).CICSRET                                          00004500
         CEETERM RC=(15)           RETURN                     2024448   00004600
         AGO   .DATA                                                    00004700
.CICSRET ANOP                                                           00004800
         EXEC  CICS RETURN                                              00004900
         LTORG                                                          00005000
.DATA    ANOP                                                           00005100
FORM6    DC    CL1'6'                                         2602675   00005110
P1       DC    PL1'1'                                                   00005200
P2       DC    PL1'2'                                                   00005300
DAYTBL   DC    PL2'000'                                                 00005400
         DC    PL2'031'                                                 00005500
         DC    PL2'059'                                                 00005600
         DC    PL2'090'                                                 00005700
         DC    PL2'120'                                                 00005800
         DC    PL2'151'                                                 00005900
         DC    PL2'181'                                                 00006000
         DC    PL2'212'                                                 00006100
         DC    PL2'243'                                                 00006200
         DC    PL2'273'                                                 00006300
         DC    PL2'304'                                                 00006400
         DC    PL2'334'                                                 00006500
         AIF   ('&SYSPARM' NE 'CICS').DTSBAT                            00006985
FIDJUL   CSECT                                                          00006986
         AGO   .DTSMRG                                                  00006987
.DTSBAT  ANOP                                                           00006988
SIDJUL   CSECT                                                          00006989
.DTSMRG  ANOP                                                           00006990
         LTORG                                                          00006991
         DS    0D                                                       00006992
SITMSTMP DC    CL64'SIDJUL    -----TSD-             06/24/02  18.40.53' 00006993
*        THIS PROGRAM CONTAINS TRADE SECRETS THAT BELONG                00006994
*        TO ALLTEL INFORMATION SERVICES, INC. AND IS                    00006995
*        LICENSED BY AN AGREEMENT.  ANY UNAUTHORIZED ACCESS,            00006996
*        USE, DUPLICATION, OR DISCLOSURE IS UNLAWFUL.                   00006997
*        COPYRIGHT ALLTEL INFORMATION SERVICES, INC.                    00006998
*        2002, ALL RIGHTS RESERVED.                                     00006999
         END                                                            00007000
