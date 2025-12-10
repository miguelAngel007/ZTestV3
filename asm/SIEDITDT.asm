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
HOLDYR   DS    D                                              2024448   00000264
PGMPLIST DS    F                   ORIGINAL R1 SAVE AREA      2024448   00000266
#UDSALEN EQU   *-UDSABEG           LENGTH OF DSA              2024448   00000270
         AIF   ('&SYSPARM' NE 'CICS').GEN1                              00000300
&CICS    SETB  1                                                        00000400
         TITLE 'FIEDITDT - DATE EDIT ROUTINE'                           00000500
FIEDITDT DFHEIENT CODEREG=(12),DATAREG=(13),EIBREG=(11)                 00000600
FIEDITDT AMODE 31                                             9913165 / 00000635
FIEDITDT RMODE ANY                                            9913165 / 00000640
         L     R1,DFHEICAP         LOAD ADDR OF COMMON AREA             00000800
         B     *+24                                                     00000900
         DC    CL20'FIEDITDT(SIEDITDT)'                                 00001000
         AGO   .GEN2                                                    00001100
.GEN1    ANOP                                                           00001200
#DSALEN  EQU   *-CEEDSA            LENGTH OF DSA              2024448   00001205
         TITLE 'SIEDITDT - DATE EDIT ROUTINE'                           00001300
* THE FOLLOWING STATEMENT(S) MODIFIED BY                      2024448   00001399
SIEDITDT CEEENTRY ,                                                    X00001400
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
               EPNAME=SIEDITDT,                                        X00001600
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
*        THE DATA AREA BEING CHECKED MUST HAVE A DATE IN THE FIRST    * 00001900
*        SIX BYTES AND A ONE BYTE FLAG IN THE BYTE FOLLOWING THE DATE.* 00002000
*        THE DATE MUST BE A ZONED DECIMAL NUMBER WITH ALL ZONES       * 00002100
*        BEING HEX 'F' AND IN MONTH, DAY, YEAR FORMAT.                * 00002200
*        THE FLAG WILL BE INITIALIZED TO A C'0' AND WILL BE SET       * 00002300
*        TO A C'1' IF THE DATE IS INVALID.                            * 00002400
*        THE COBOL CALL STATEMENT IS                                  * 00002500
*              CALL 'SIEDITDT' USING DATE-AREA.                       * 00002600
*                                                                     * 00002602
*        OPTIONAL FORMAT                                              * 00002604
*              CALL 'SIEDITDT' USING DATE-AREA, FORMAT.               * 00002606
*        WHERE FORMAT IS A '6' OR AN '8'.                             * 00002608
*        IF FORMAT IS AN 8, THE DATE IS MMDDCCYY FORMAT               * 00002610
*        AND IS FOLLOWED BY A ONE BYTE FLAG.                          * 00002615
*                                                                     * 00002620
*---------------------------------------------------------------------* 00002625
*                  ** HISTORY OF REVISIONS **                         * 00002630
*                                                                     * 00002635
* DESCRIPTION                                                 CHNGID  * 00002640
* __________________________________________________________  _______ * 00002645
*                                                                     * 00002650
* 04/11/02 LE ENABLED, REENTRANT                              2024448 * 00002770
* 01/18/00 ENABLE FOR 31-BIT                                  2024183 * 00002775
* 08/08/97 SET RC FOR COBOL/VSE.                              9913437 * 00002776
* 03/06/97 ADDED CAPABILITY FOR 31-BIT ADDRESSING             9913165 * 00002778
* 09/05/95 YEAR 2000.                                         2602675 * 00002780
*                                                                     * 00002785
*---------------------------------------------------------------------* 00002790
         EJECT                                                          00002795
         LA    2,FORMAT6           DEFAULT FORMAT             2602675   00002800
         TM    0(1),X'80'          FORMAT PRESENT?            2602675   00002810
         BO    FORMOK              NO, USE DEFAULT            2602675   00002820
         L     2,4(0,1)            ADDRESS FORMAT             2602675   00002830
FORMOK   EQU   *                                              2602675   00002840
         L     1,0(0,1)            LOAD ADDR OF DATE AREA INTO R1       00002900
         CLI   0(2),C'8'                                      2602675   00002910
         BE    INIT8                                          2602675   00002920
         MVI   6(1),C'0'           SET ERROR FLAG OFF                   00003000
         B     INITMRG                                        2602675   00003010
INIT8    EQU   *                                              2602675   00003020
         MVI   8(1),C'0'           SET ERROR FLAG OFF         2602675   00003030
INITMRG  EQU   *                                              2602675   00003040
         CLI   0(1),C'0'           CHK BYTE 1 LOW                       00003100
         BL    SETERR                                                   00003200
         CLI   0(1),C'1'           CHK BYTE 1 HIGH                      00003300
         BH    SETERR                                                   00003400
         CLI   1(1),C'0'           CHK BYTE 2 LOW                       00003500
         BL    SETERR                                                   00003600
         CLI   1(1),C'9'           CHK BYTE 2 HIGH                      00003700
         BH    SETERR                                                   00003800
         CLC   0(2,1),M01          CHK MONTH LT 1                       00003900
         BL    SETERR                                                   00004000
         CLC   0(2,1),M12          CHK MONTH GT 12                      00004100
         BH    SETERR                                                   00004200
         CLI   2(1),C'0'           CHK BYTE 3 LOW                       00004300
         BL    SETERR                                                   00004400
         CLI   2(1),C'3'           CHK BYTE 3 HIGH                      00004500
         BH    SETERR                                                   00004600
         CLI   3(1),C'0'           CHK BYTE 4 LOW                       00004700
         BL    SETERR                                                   00004800
         CLI   3(1),C'9'           CHK BYTE 4 HIGH                      00004900
         BH    SETERR                                                   00005000
         CLC   2(2,1),M01          CHK DAY LT 1                         00005100
         BL    SETERR                                                   00005200
         CLC   2(2,1),D31          CHK DAY GT 31                        00005300
         BH    SETERR                                                   00005400
         CLI   4(1),C'0'           CHK BYTE 5 LOW                       00005500
         BL    SETERR                                                   00005600
         CLI   4(1),C'9'           CHK BYTE 5 HIGH                      00005700
         BH    SETERR                                                   00005800
         CLI   5(1),C'0'           CHK BYTE 6 LOW                       00005900
         BL    SETERR                                                   00006000
         CLI   5(1),C'9'           CHK BYTE 6 HIGH                      00006100
         BH    SETERR                                                   00006200
         CLI   0(2),C'8'           MMDDCCYY?                  2602675   00006205
         BNE   NOT8                NO, CONTINUE               2602675   00006210
         CLI   6(1),C'0'           CHK BYTE 5 LOW             2602675   00006215
         BL    SETERR                                         2602675   00006220
         CLI   6(1),C'9'           CHK BYTE 5 HIGH            2602675   00006225
         BH    SETERR                                         2602675   00006230
         CLI   7(1),C'0'           CHK BYTE 6 LOW             2602675   00006235
         BL    SETERR                                         2602675   00006240
         CLI   7(1),C'9'           CHK BYTE 6 HIGH            2602675   00006245
         BH    SETERR                                         2602675   00006250
         CLC   4(2,1),=C'19'       CENTURY 19 OR 20           2602675   00006255
         BL    SETERR                                         2602675   00006260
         CLC   4(2,1),=C'20'       CENTURY 19 OR 20           2602675   00006270
         BH    SETERR                                         2602675   00006280
NOT8     EQU   *                                              2602675   00006290
         CLC   0(2,1),M02          CHK FOR FEBRUARY                     00006300
         BE    CHKFEB                                                   00006400
         CLC   2(2,1),D31          CHK FOR DAYS LT 31                   00006500
         BL    EDTEXIT             RETURN IF DAYS LT 31                 00006600
         CLI   1(1),C'4'           CHK FOR APRIL                        00006700
         BE    SETERR                                                   00006800
         CLI   1(1),C'6'           CHK FOR JUNE                         00006900
         BE    SETERR                                                   00007000
         CLI   1(1),C'9'           CHK FOR SEPTEMBER                    00007100
         BE    SETERR                                                   00007200
         CLC   0(2,1),M11          CHK FOR NOVEMBER                     00007300
         BE    SETERR                                                   00007400
         B     EDTEXIT                                                  00007500
CHKFEB   CLI   2(1),C'2'           CHK FOR DAYS LT 20                   00007600
         BL    EDTEXIT             RETURN IF DAYS LT 20                 00007700
         CLI   2(1),C'3'           CHK FOR DAYS LT 30                   00007800
         BNL   SETERR                                                   00007900
         CLI   3(1),C'9'           CHK FOR DAYS LT 29                   00008000
         BL    EDTEXIT             RETURN IF DAYS LT 29                 00008100
         PACK  HOLDYR,4(2,1)       PACK YEAR                            00008200
         CLI   0(2),C'8'           MMDDCCYY?                  2602675   00008210
         BNE   CHKFEB8             NO, CONTINUE               2602675   00008220
         CLC   6(2,1),=C'00'       IS IT A CENTURY?           2602675   00008230
         BE    CHKFEB8             YES, TEST CENTURY          2602675   00008240
         PACK  HOLDYR,6(2,1)       PACK YEAR                  2602675   00008250
CHKFEB8  EQU   *                                              2602675   00008260
         CVB   0,HOLDYR            CONVERT YEAR TO BINARY               00008300
         STC   0,HOLDYR            STORE YEAR IN HOLDYR                 00008400
         TM    HOLDYR,X'03'        TEST FOR YEAR DIVISIBLE BY 4         00008500
         BE    EDTEXIT             RETURN IF EVENLY DIVISIBLE BY FOUR   00008600
SETERR   CLI   0(2),C'8'           8-BYTE DATE?               2602675   00008610
         BE    SETERR8             YES, GO MOVE               2602675   00008620
         MVI   6(1),C'1'           SET ERROR FLAG 6-BYTE      2602675   00008630
         B     EDTEXIT                                        2602675   00008640
SETERR8  EQU   *                   8-BYTE DATE                2602675   00008700
         MVI   8(1),C'1'           SET ERROR FLAG             2602675   00008710
EDTEXIT  EQU   *                                                        00008800
         AIF   (&CICS).CICSRET                                          00008900
         CEETERM RC=(15)           RETURN                     2024448   00009000
         AGO   .DATA                                                    00009100
.CICSRET ANOP                                                           00009200
         EXEC  CICS RETURN                                              00009300
         LTORG                                                          00009400
.DATA    ANOP                                                           00009500
         EJECT                                                          00009600
FORMAT6  DC    C'6'                                           2602675   00009610
M01      DC    C'01'                                                    00009700
M02      DC    C'02'                                                    00009800
M11      DC    C'11'                                                    00009900
M12      DC    C'12'                                                    00010000
D31      DC    C'31'                                                    00010100
         AIF   ('&SYSPARM' NE 'CICS').DTSBAT                            00010585
FIEDITDT CSECT                                                          00010586
         AGO   .DTSMRG                                                  00010587
.DTSBAT  ANOP                                                           00010588
SIEDITDT CSECT                                                          00010589
.DTSMRG  ANOP                                                           00010590
         LTORG                                                          00010591
         DS    0D                                                       00010592
SITMSTMP DC    CL64'SIEDITDT  -----TSD-             06/24/02  19.10.11' 00010593
*        THIS PROGRAM CONTAINS TRADE SECRETS THAT BELONG                00010594
*        TO ALLTEL INFORMATION SERVICES, INC. AND IS                    00010595
*        LICENSED BY AN AGREEMENT.  ANY UNAUTHORIZED ACCESS,            00010596
*        USE, DUPLICATION, OR DISCLOSURE IS UNLAWFUL.                   00010597
*        COPYRIGHT ALLTEL INFORMATION SERVICES, INC.                    00010598
*        2002, ALL RIGHTS RESERVED.                                     00010599
         END                                                            00010600
