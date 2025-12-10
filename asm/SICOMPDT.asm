*ASM XOPTS(NOEPILOG)                                                    00000100
*     * 802262 12/11/20 PROYECTO UPGRADE GN172 EDVR
         SIRENT                                               2024448   00000105
         COPY  SIOPTNS                                        9913165   00000190
         LCLB  &CICS                                          2024448   00000220
         SIEQREG                                              2024448   00000230
         AIF   ('&SYSPARM' NE 'CICS').BTCHDTA                 2024448   00000235
* THE FOLLOWING STATEMENT(S) MODIFIED BY                      2024448   00000239
DFHEISTG DSECT                                                          00000240
* THE FOLLOWING STATEMENT(S) MODIFIED BY                      2024448   00000244
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
DATEWRK1 DS    CL8                                            2024448   00000266
DATEWRK2 DS    CL8                                            2024448   00000268
#UDSALEN EQU   *-UDSABEG           LENGTH OF DSA              2024448   00000270
         AIF   ('&SYSPARM' NE 'CICS').GEN1                              00000300
&CICS    SETB  1                                                        00000400
         TITLE 'FICOMPDT - DATE COMPARE ROUTINE'                        00000500
FICOMPDT DFHEIENT CODEREG=(12),DATAREG=(13),EIBREG=(11)                 00000600
FICOMPDT AMODE 31                                             9913165 / 00000635
FICOMPDT RMODE ANY                                            9913165 / 00000640
         L     R1,DFHEICAP         LOAD ADDR OF COMMON AREA             00000800
         B     *+24                                                     00000900
         DC    CL20'FICOMPDT(SICOMPDT)'                                 00001000
         AGO   .GEN2                                                    00001100
.GEN1    ANOP                                                           00001200
#DSALEN  EQU   *-CEEDSA            LENGTH OF DSA              2024448   00001210
         TITLE 'SICOMPDT - DATE COMPARE ROUTINE'                        00001300
* THE FOLLOWING STATEMENT(S) MODIFIED BY                      2024448   00001399
SICOMPDT CEEENTRY ,                                                    X00001400
               PPA=PPA,                                                X00001505
               AUTO=#DSALEN,                                           X00001510
               MAIN=NO,                                                X00001515
               NAB=NO,                                                 X00001520
               PARMREG=1,                                              X00001525
               BASE=11                                                  00001530
         XR    R15,R15                                        2024448   00001535
         ST    R1,PGMPLIST                                    2024448   00001537
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
               EPNAME=SICOMPDT,                                        X00001600
               VER=01,                                                 X00001605
               REL=01,                                                 X00001610
               MOD=00,                                                 X00001615
               DSA=YES                                                  00001620
         SPACE 2                                                        00001625
         LTORG                                                2024448   00001630
INIT000  EQU   *                                              2024448   00001635
         L     R1,PGMPLIST                                    2024448   00001640
.GEN2    ANOP                                                           00001645
*---------------------------------------------------------------------* 00001650
*                   ** PROGRAM DESCRIPTION **                         * 00001655
*                                                                     * 00001660
**************************************************************2602675** 00001700
*                                                                     * 00001800
*    CALL 'SICOMPDT' USING DATEA, DATEB, FLAG, FORMAT, (BASE-YEAR)    * 00001900
*                                                                     * 00002000
*      WHERE,                                                         * 00002100
*        DATEA    PIC X(6) OR X(8).                                   * 00002200
*        DATEB    PIC X(6) OR X(8).                                   * 00002300
*        FLAG     PIC X.                                              * 00002400
*        FORMAT   PIC XX.                                             * 00002500
*           1ST BYTE IS DATEA FORMAT                                  * 00002600
*           2ND BYTE IS DATEB FORMAT                                  * 00002700
*                            1 = MMDDYY FORMAT                        * 00002800
*                            2 = YYMMDD FORMAT                        * 00002900
*                            3 = MMDDYYYY FORMAT                      * 00003000
*                            4 = YYYYMMDD FORMAT                      * 00003100
*        BASE-YEAR PIC XX (DEFAULTS TO 50).                           * 00003110
*                                                                     * 00003200
*      IF  DATEA GREATER THAN DATEB, FLAG RETURNED H.                 * 00003300
*      IF  DATEA LESS THAN    DATEB, FLAG RETURNED L.                 * 00003400
*      IF  DATEA EQUAL TO     DATEB, FLAG RETURNED E.                 * 00003500
*                                                                     * 00003600
*********************************************************************** 00003700
*                                                                     * 00003702
*---------------------------------------------------------------------* 00003704
         EJECT                                                          00003706
*---------------------------------------------------------------------* 00003708
*                  ** HISTORY OF REVISIONS **                         * 00003710
*                                                                     * 00003712
* DESCRIPTION                                                 CHNGID  * 00003714
* __________________________________________________________  _______ * 00003716
* 03/15/02 LE ENABLED, REENTRANT                              2024448 * 00003787
* 01/06/00 ENABLE FOR 31-BIT                                  2024183 * 00003789
* 08/08/97 SET RC FOR COBOL/VSE                               9913437 * 00003790
* 05/09/96 ADDED CAPABILITY FOR 31-BIT ADDRESSING             9913165 * 00003792
* 08/30/95 YEAR 2000                                          2602675 * 00003794
*                                                                     * 00003796
*---------------------------------------------------------------------* 00003798
         EJECT                                                          00003800
         LM    R3,R6,0(R1)             LOAD PARAMETER LIST              00003900
         LA    R7,FIFTY                DEFAULT BASE YEAR 50   2602675   00003910
         TM    12(R1),X'80'            FOUR PARAMETERS?       2602675   00003920
         BO    CHKFMT4A                YES, NO BASE YEAR      2602675   00003930
         L     R7,16(R1)               LOAD BASE YEAR ADDRESS 2602675   00003940
         SPACE 3                                                        00004000
CHKFMT4A EQU   *                                                        00004100
         CLI   0(R6),C'4'              IS FORMAT YYYYMMDD               00004200
         BNE   CHKFMT3A                NO                               00004300
         MVC   DATEWRK1,0(R3)          MOVE DATEA TO WORKAREA           00004400
         B     CHKFMT4B                                                 00004500
         SPACE 3                                                        00004600
CHKFMT3A EQU   *                                                        00004700
         CLI   0(R6),C'3'              IS FORMAT MMDDYYYY               00004800
         BNE   CHKFMT2A                NO - CHK FOR FORMAT 2            00004900
         MVC   DATEWRK1+4(4),0(R3)     MOVE DATEA MMDD TO WORKAREA      00005000
         MVC   DATEWRK1(4),4(R3)       MOVE DATEA YYYY TO WORKAREA      00005100
         B     CHKFMT4B                                                 00005200
         SPACE 3                                                        00005300
CHKFMT2A EQU   *                                                        00005400
         CLI   0(R6),C'2'              IS FORMAT YYMMDD                 00005500
         BNE   CHKFMT1A                NO                               00005600
         MVC   DATEWRK1+2(6),0(R3)     MOVE DATEA TO WORKDATE           00005700
         B     CHKACEN                 SET DATEA CENTURY                00005800
         SPACE 3                                                        00005900
CHKFMT1A EQU   *                                                        00006000
         MVC   DATEWRK1+2(2),4(R3)     MOVE DATEA YY TO WORKDATE        00006100
         MVC   DATEWRK1+4(4),0(R3)     MOVE DATEA MMDD TO WORKDATE      00006200
         SPACE 3                                                        00006300
CHKACEN  EQU   *                                                        00006400
         MVC   DATEWRK1(2),NINETEEN    MOVE 19 TO DATE A CENTURY        00006500
         CLC   DATEWRK1+2(6),NINES     IF DATEA EQUAL 999999            00006600
         BE    PLUG20A                     GO TO PLUG20A                00006700
         CLC   DATEWRK1+2(2),0(R7)     YEAR > BASE YEAR       2602675   00006800
         BNL   CHKFMT4B                    GO TO CHKFMT4B     2602675   00006900
         CLC   DATEWRK1+2(6),ZEROS     IF DATEA EQUAL ZEROS             00007000
         BE    CHKFMT4B                    GO TO CHKFMT4B               00007100
         CLC   DATEWRK1+2(6),SPACES    IF DATEA EQUAL SPACES            00007200
         BE    CHKFMT4B                    GO TO CHKFMT4B               00007300
PLUG20A  MVC   DATEWRK1(2),TWENTY      MOVE 20 TO DATE A CENTURY        00007400
         EJECT                                                          00007500
CHKFMT4B EQU   *                                                        00007600
         CLI   1(R6),C'4'              IS FORMAT YYYYMMDD               00007700
         BNE   CHKFMT3B                NO                               00007800
         MVC   DATEWRK2,0(R4)          MOVE DATEB TO WORKAREA           00007900
         B     CHKDATES                                                 00008000
         SPACE 3                                                        00008100
CHKFMT3B EQU   *                                                        00008200
         CLI   1(R6),C'3'              IS FORMAT MMDDYYYY               00008300
         BNE   CHKFMT2B                NO - CHK FOR FORMAT 2            00008400
         MVC   DATEWRK2+4(4),0(R4)     MOVE DATEA MMDD TO WORKAREA      00008500
         MVC   DATEWRK2(4),4(R4)       MOVE DATEA YYYY TO WORKAREA      00008600
         B     CHKDATES                                                 00008700
         SPACE 3                                                        00008800
CHKFMT2B EQU   *                                                        00008900
         CLI   1(R6),C'2'              IS FORMAT YYMMDD                 00009000
         BNE   CHKFMT1B                NO                               00009100
         MVC   DATEWRK2+2(6),0(R4)     MOVE DATEB TO WORKDATE           00009200
         B     CHKBCEN                                                  00009300
         SPACE 3                                                        00009400
CHKFMT1B EQU   *                                                        00009500
         MVC   DATEWRK2+2(2),4(R4)     MOVE DATEB YY TO WORKDATE        00009600
         MVC   DATEWRK2+4(4),0(R4)     MOVE DATEB MMDD TO WORKDATE      00009700
         SPACE 3                                                        00009800
CHKBCEN  EQU   *                                                        00009900
         MVC   DATEWRK2(2),NINETEEN    MOVE 19 TO DATE B CENTURY.       00010000
         CLC   DATEWRK2+2(6),NINES     IF DATEB EQUAL 999999            00010100
         BE    PLUG20B                     GO TO PLUG20B                00010200
         CLC   DATEWRK2+2(2),0(R7)     YEAR > BASE YEAR       2602675   00010300
         BNL   CHKDATES                    GO TO CHKDATES     2602675   00010400
         CLC   DATEWRK2+2(6),ZEROS     IF DATEB EQUAL ZEROS             00010500
         BE    CHKDATES                    GO TO CHKDATES               00010600
         CLC   DATEWRK2+2(6),SPACES    IF DATEB EQUAL SPACES            00010700
         BE    CHKDATES                    GO TO CHKDATES               00010800
PLUG20B  MVC   DATEWRK2(2),TWENTY      MOVE 20 TO DATE B CENTURY        00010900
         EJECT                                                          00011000
CHKDATES EQU   *                                                        00011100
         CLC   DATEWRK1,DATEWRK2       COMPARE DATEA TO DATEB           00011200
         BL    ALOW                    IF DATE A LT DATE B GO TO ALOW.  00011300
         BH    AHIGH                   IF DATE A GT DATE B GO TO AHIGH. 00011400
         MVI   0(R5),C'E'              MOVE E TO FLAG.                  00011500
         B     ENDRTE                  GO TO ENDRTE.                    00011600
AHIGH    EQU   *                                                        00011700
         MVI   0(R5),C'H'              MOVE H TO FLAG.                  00011800
         B     ENDRTE                  GO TO ENDRTE.                    00011900
ALOW     EQU   *                                                        00012000
         MVI   0(R5),C'L'              MOVE L TO FLAG.                  00012100
ENDRTE   EQU   *                                                        00012200
         AIF   (&CICS).CICSRET                                          00012300
         CEETERM RC=(15)               RETURN                 2024448   00012400
         AGO   .DATA                                                    00012500
.CICSRET ANOP                                                           00012600
         EXEC  CICS RETURN                                              00012700
         LTORG                                                          00012800
.DATA    ANOP                                                           00012900
NINETEEN DC    CL2'19'                                                  00013000
TWENTY   DC    CL2'20'                                                  00013100
FIFTY    DC    CL2'50'                                                  00013200
SPACES   DC    CL6' '                                                   00013300
ZEROS    DC    CL6'000000'                                              00013400
NINES    DC    CL6'999999'                                              00013500
         SPACE 3                                                        00013600
         AIF   ('&SYSPARM' NE 'CICS').DTSBAT                            00014185
FICOMPDT CSECT                                                          00014186
         AGO   .DTSMRG                                                  00014187
.DTSBAT  ANOP                                                           00014188
SICOMPDT CSECT                                                          00014189
.DTSMRG  ANOP                                                           00014190
         LTORG                                                          00014191
         DS    0D                                                       00014192
SITMSTMP DC    CL64'SICOMPDT  -----TSD-             06/24/02  10.02.33' 00014193
*        THIS PROGRAM CONTAINS TRADE SECRETS THAT BELONG                00014194
*        TO ALLTEL INFORMATION SERVICES, INC. AND IS                    00014195
*        LICENSED BY AN AGREEMENT.  ANY UNAUTHORIZED ACCESS,            00014196
*        USE, DUPLICATION, OR DISCLOSURE IS UNLAWFUL.                   00014197
*        COPYRIGHT ALLTEL INFORMATION SERVICES, INC.                    00014198
*        2002, ALL RIGHTS RESERVED.                                     00014199
         END                                                            00014200
