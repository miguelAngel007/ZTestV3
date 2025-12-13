*ASM XOPTS(NOEPILOG)                                                    00000100
*     * 802262 12/11/20 PROYECTO UPGRADE GN172 EDVR
         SIRENT                                               2024448   00000102
*     * FO5238 * 06/26/11 PROYECTO REBORN
**********************************************************************  00000120
*                      HISTORY OF REVISIONS                          *  00000121
*DESCRIPTION                                                  CHG ID *  00000122
*--------------------------------------------------------------------*  00000123
* 08/25/10 COMMENT OUT WLSTNAME,=X'FF'                        SIIB9701  00000124
* 03/27/17 DBCS SUPPORT - CORRECT TRUNCATED KEY               1727183   00000180
* 01/21/16 DBCS SUPPORT - BUILD CHINESE KEY                   1626788   00000181
* 04/11/02 LE ENABLED, REENTRANT                              2024448*  00000182
* 02/29/00 ENABLE FOR 31-BIT ADDRESSING                       2024183*  00000184
* 08/08/97 SET RC FOR COBOL/VSE.                              9913437 * 00000185
*05/15/96  ADDED CAPABILITY FOR 31-BIT ADDRESSING             9913165*  00000187
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
NUMTEST  DS    CL20                                           2024448   00000268
SAVEWORD DS    CL240                                          2024448   00000270
NOCHAR   DS    H                                              2024448   00000272
NOREM    DS    H                                              2024448   00000274
NOWORDS  DS    PL2                                            2024448   00000276
#UDSALEN EQU   *-UDSABEG           LENGTH OF DSA              2024448   00000278
         AIF   ('&SYSPARM' NE 'CICS').GEN1                              00000300
&CICS    SETB  1                                                        00000400
         TITLE 'FIKEYGNR - FIAS ALPHA KEY GENERATOR ROUTINE'            00000500
FIKEYGNR DFHEIENT CODEREG=(12),DATAREG=(13),EIBREG=(11)                 00000600
FIKEYGNR AMODE 31                                             9913165 / 00000635
FIKEYGNR RMODE ANY                                            9913165 / 00000640
         L     R1,DFHEICAP         LOAD ADDR OF COMMON AREA             00000800
         B     *+24                                                     00000900
         DC    CL20'FIKEYGNR(SIKEYGNR)'                                 00001000
         AGO   .GEN2                                                    00001100
.GEN1    ANOP                                                           00001200
#DSALEN  EQU   *-CEEDSA            LENGTH OF DSA              2024448   00001205
         TITLE 'SIKEYGNR - FIAS ALPHA KEY GENERATOR ROUTINE'            00001300
* THE FOLLOWING STATEMENT(S) MODIFIED BY                      2024448   00001399
SIKEYGNR CEEENTRY ,                                                    X00001400
               PPA=PPA,                                                X00001505
               AUTO=#DSALEN,                                           X00001510
               MAIN=NO,                                                X00001515
               NAB=NO,                                                 X00001520
               PARMREG=1,                                              X00001525
               BASE=10                                                  00001530
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
               EPNAME=SIKEYGNR,                                        X00001600
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
*                                                                       00001700
         USING WORDAREA,R4                                              00001800
         L     R5,0(0,1)                GET LENGTH OF INPUT FIELD       00001900
         LH    R5,0(0,R5)                                               00002000
         L     R2,4(0,1)                GET ADDR OF INPUT FIELD         00002100
         L     R3,8(0,1)                GET ADDR OF OUTPUT FIELD        00002200
         LR    R6,R2                    CALC ADDR OF END OF INPUT       00002300
         AR    R6,R5                                                    00002400
         BCTR  R6,0                                                     00002500
         LA    R4,SAVEWORD              POINT TO WORD WORK AREA         00002600
         LA    R8,10                    SET NO. WORDS TO CLEAR          00002700
CLRSAVE  EQU   *                                                        00002800
         MVC   WORDSAVE,=CL20' '        CLEAR WORD WORK AREA            00002900
         XC    WORDLNG(4),WORDLNG                                       00003000
         LA    R4,24(0,R4)              BUMP TO NEXT WORD               00003100
         BCT   R8,CLRSAVE                                               00003200
         EJECT                                                          00003300
         ZAP   NOWORDS,=P'0'            SET UP FOR WORD SCAN            00003400
         LA    R4,SAVEWORD              POINT TO WORD WORK AREA         00003500
         CLI   0(R2),X'0E'              DBCS SO CHARACTER     1626788   00003510
         BE    SETDBCS                  YES                   1626788   00003520
FINDNOBL EQU   *                                                        00003600
         CLI   0(R2),C','               IS INPUT CHAR A COMMA           00003700
         BE    NXTCHAR                  YES                             00003800
         CLI   0(R2),X'0E'              IS INPUT CHAR A SO    1727183   00003810
         BE    NXTCHAR                  YES                   1626788   00003820
         CLI   0(R2),C' '               IS INPUT CHAR NOT A SPACE       00003900
         BNE   SAVEKEY                  YES                             00004000
NXTCHAR  EQU   *                                                        00004100
         LA    R2,1(0,R2)               BUMP TO NEXT INPUT CHAR         00004200
         CR    R2,R6                    IS THIS PAST THE INPUT FIELD    00004300
         BNH   FINDNOBL                 NO                              00004400
         CP    NOWORDS,=P'0'            IS THIS A BLANK INPUT AREA      00004500
         BE    SETENDFL                 YES                             00004600
         B     FLGLSTWD                 FLAG THE LAST WORD AS THE END   00004700
SAVEKEY  EQU   *                                                        00004800
         LR    R9,R2                    SAVE ADDR OF WORD IN INPUT AREA 00004900
FINDBLNK EQU   *                                                        00005000
         CLI   0(R2),C' '               LOOK FOR NEXT BLANK             00005100
         BE    MOVEWORD                                                 00005200
         CLI   0(R2),C','               LOOK FOR COMMA                  00005300
         BE    MOVEWORD                                                 00005400
         CLI   0(R2),X'0F'              LOOK FOR SI           1727183   00005410
         BE    MOVEWORD                                       1626788   00005420
         LA    R2,1(0,R2)               BUMP TO NEXT INPUT CHAR         00005500
         CR    R2,R6                    AT END OF INPUT AREA            00005600
         BNH   FINDBLNK                 NO - GO BACK                    00005700
         B     MOVEWRD1                 YES                             00005800
MOVEWORD EQU   *                                                        00005900
         CLI   0(R2),C','               CHK FOR COMMA AFTER LAST NAME   00006000
         BNE   MOVEWRD1                 NO COMMA - NOT A LAST NAME      00006100
         LA    R4,SAVEWORD              GET FIRST WORD AREA ADDR        00006200
*        MVI   WLSTNAME,X'FF'           SET LAST NAME FLAG    SIIB9701  00006300
         CP    NOWORDS,=P'0'            CHK FOR FIRST WORD              00006400
         BE    MOVEWRD2                 GO TO MOVEWRD2 IF FIRST WORD    00006500
         ZAP   NOWORDS,=P'1'            SET NOWORDS TO 1                00006600
         B     CHKEND                   GO TO CHKEND                    00006700
MOVEWRD1 EQU   *                                                        00006800
         CLI   0(R9),C'/'               CHK FOR SLASH BEFORE LAST NAME  00006900
         BNE   MOVEWRD2                 NO SLASH                        00007000
         LA    R9,1(0,R9)               GET NEXT CHAR AFTER SLASH       00007100
         CR    R9,R6                    IS SLASH THE LAST CHARACTER     00007200
         BH    SETENDFL                 YES - DONT SAVE IT              00007300
         MVI   WLSTNAME,X'FF'           SET LAST NAME FLAG              00007400
         CR    R2,R6                    ARE WE PAST END OF INPUT        00007500
         BH    MOVEWRD2                 YES                             00007600
         CLI   0(R9),C' '               CHK FOR BLANK AFTER SLASH       00007700
         BNE   MOVEWRD2                 NO BLANK - MOVE THIS WORD       00007800
         LR    R2,R9                    RESTORE POINTER TO WORD         00007900
         B     FINDNOBL                 FIND NEXT WORD                  00008000
MOVEWRD2 EQU   *                                                        00008100
         LR    R8,R2                    CALC LENGTH OF WORD             00008200
         SR    R8,R9                                                    00008300
         BCTR  R8,0                                                     00008400
         CH    R8,=H'19'                IS WORD LONGER THAN 20 CHAR     00008410
         BNH   *+8                      NO                              00008420
         LH    R8,=H'19'                YES - FORCE LENGTH TO 20        00008430
         EX    R8,MVCWORD               MOVE WORD TO WORK AREA          00008500
         STC   R8,WORDLNG               SAVE LENGTH OF WORD             00008600
         CP    NOWORDS,=P'0'            FIRST WORD OF THE NM? SIIB9701  00008605
         BNE   SKIP1                    NO                    SIIB9701  00008610
         MVI   WLSTNAME,X'FF'           SET LAST NAME FLAG    SIIB9701  00008615
SKIP1    EQU   *                        SKIP1 LABEL           SIIB9701  00008620
         AP    NOWORDS,=P'1'            ADD TO NO OF WORDS FOUND        00008700
CHKEND   EQU   *                                                        00008800
         CR    R2,R6                    AT END OF INPUT FIELD           00008900
         BNL   SETENDFL                 YES                             00009000
         LA    R4,24(0,R4)              BUMP TO NEXT WORD WORK AREA     00009100
         CP    NOWORDS,=P'10'           AT END OF TABLE                 00009200
         BNL   FLGLSTWD                 YES                             00009300
         LA    R2,1(0,R2)               BUMP TO NEXT INPUT CHARACTER    00009400
         B     FINDNOBL                 FIND NEXT WORD                  00009500
FLGLSTWD EQU   *                                                        00009600
         SH    R4,=H'24'                BACK UP TO PREV WORD IN WORK    00009700
SETENDFL EQU   *                                                        00009800
         MVI   WLINEND,X'FF'            SET LAST-WORD FLAG              00009900
         MVC   0(16,R3),CLEARKEY        CLEAR OUTPUT WORK AREA          00010000
         SR    R5,R5                                                    00010100
         SR    R6,R6                                                    00010200
         LA    R4,SAVEWORD                                              00010300
FINDAST  EQU   *                                                        00010400
         CLI   WLSTNAME,X'FF'           DID I GET A LAST NAME           00010500
         BE    STRTPERS                 YES - GEN PERSONAL KEY          00010600
         CLI   WLINEND,X'FF'            AT END OF WORDS                 00010700
         BE    STRTCOMM                 YES - GEN COMMERCIAL KEY        00010800
         LA    R4,24(0,R4)              BUMP TO NEXT WORD               00010900
         B     FINDAST                  LOOP BACK                       00011000
         EJECT                                                          00011100
STRTPERS EQU   *                                                        00011200
         MVI   0(R3),C'P'               MOVE PERSONAL CODE              00011300
         LA    R3,1(0,R3)               BUMP TO NEXT OUTPUT POSITION    00011400
         MVC   NOREM,=H'7'              SET MAX CHAR WANTED             00011500
         MVC   NOCHAR,=H'8'                                             00011600
         LA    R7,FRSTNM                SET BUILDKEY RETURN ADDR        00011700
BUILDKEY EQU   *                                                        00011800
         SR    R5,R5                    CLEAR R5                        00011900
         IC    R5,WORDLNG               GET LENGTH OF WORD              00012000
         LR    R6,R5                    SAVE LENGTH OF WORD             00012100
         CH    R5,NOREM                 CHK FOR TOO MANY CHAR           00012200
         BNH   MOVENM                   LENGTH OK                       00012300
         LH    R5,NOREM                 FORCE LENGTH TO MAX IN FLD      00012400
MOVENM   EQU   *                                                        00012500
         EX    R5,MVCNAME               MOVE WORD TO OUTPUT KEY         00012600
         AH    R3,NOCHAR                GET NEXT AVAILABLE CHAR ADDR    00012700
         SR    R6,R5                    CALC NO CHARS REMAINING         00012800
         CVD   R6,DBLWORD                                               00012900
         CP    DBLWORD+6(2),=P'9'       MAX REMAINING IS 9              00013000
         BNH   UPKREM                                                   00013100
         ZAP   DBLWORD+6(2),=P'9'                                       00013200
UPKREM   EQU   *                                                        00013300
         UNPK  0(1,R3),DBLWORD+7(1)     UNPACK NO REMAINING CHAR        00013400
         OI    0(R3),X'F0'              FIX ZONE                        00013500
         LA    R3,1(0,R3)               GET NEXT CHAR ADDR              00013600
         BR    R7                       GO TO RETURN ADDR               00013700
         SPACE 3                                                        00013800
FRSTNM   EQU   *                        CHK FOR LAST NAME FIRST         00013900
         LA    R4,SAVEWORD              GET FIRST WORD ADDR             00014000
         CLI   WLINEND,X'FF'            CHK FOR ONLY ONE WORD           00014100
         BE    NOFRSTNM                 YES - DON'T HAVE A FIRST NAME   00014200
         CLI   WLSTNAME,X'FF'           IS THIS THE LAST NAME           00014300
         BNE   FRSTNM1                  NO                              00014400
         LA    R4,24(0,R4)              YES - BUMP TO NEXT WORD         00014500
FRSTNM1  EQU   *                                                        00014600
         MVC   NOCHAR,=H'3'             SET UP FOR FIRST NAME           00014700
         MVC   NOREM,=H'2'                                              00014800
         BAS   R7,BUILDKEY        PUT FIRST NAME IN KEY       2024183   00014900
         CLI   WLINEND,X'FF'            CHK FOR LAST WORD               00015000
         BE    KEYGNXIT                 EXIT IF LAST WORD               00015100
         LA    R4,24(0,R4)              GET NEXT WORD ADDR              00015200
         CLI   WLSTNAME,X'FF'           CHK FOR LAST NAME               00015300
         BE    KEYGNXIT                 EXIT IF LAST NAME               00015400
         MVC   0(1,R3),WORDSAVE         MOVE MIDDLE INITIAL             00015500
         B     KEYGNXIT                 EXIT                            00015600
NOFRSTNM EQU   *                                                        00015700
         MVI   3(R3),C'0'               FIX FOR NO FIRST NAME           00015800
         B     KEYGNXIT                                                 00015900
         EJECT                                                          00016000
SETDBCS  EQU   *                                              1626788   00016010
         BCTR  R5,0                     DEC BY 1              1626788   00016011
         CH    R5,=H'16'                LEN > 16?             1626788   00016012
         BNH   SETDBCS1                 NO  - CONTINUE        1727183   00016014
         MVC   0(15,R3),0(R2)                                 1727183   00016015
         MVI   15(R3),X'0F'             MOVE SI               1727183   00016016
         B     KEYGNXIT                 RETURN TO CALLER      1727183   00016017
SETDBCS1 EQU   *                                              1727183   00016018
         EX    R5,MOVEDBCS              MOVE ALL DATA         1626788   00016020
         B     KEYGNXIT                 RETURN TO CALLER      1626788   00016030
*                                                             1626788   00016040
MOVEDBCS MVC   0(0,R3),0(R2)                                  1626788   00016050
*                                                             1626788   00016060
STRTCOMM EQU   *                                                        00016100
         LA    R4,SAVEWORD              POINT TO FIRST WORD             00016200
         CLI   WLINEND,X'FF'            CHK FOR LAST WORD               00016300
         BNE   SETCOMM                  NO - NORMAL COMMERCIAL          00016400
         SR    R5,R5                    CLEAR LENGTH REG                00016500
         IC    R5,WORDLNG               GET LENGTH OF FIRST WORD        00016600
         MVC   NUMTEST,=20CL1'0'        CLEAR NUMERIC TEST FIELD        00016700
         EX    R5,MOVENUM               MOVE NUMERIC PART OF 1ST WORD   00016800
         EX    R5,CHKNUM                IS FIRST WORD NUMERIC           00016900
         BNE   SETCOMM                  NO - NORMAL COMMERCIAL          00017000
         MVI   0(R3),C'T'               MOVE TAX ID CODE                00017100
         LA    R3,1(0,R3)               GET NEXT CHAR ADDR              00017200
         STH   R5,NOCHAR                SET LENGTH OF WORD              00017300
         CH    R5,=H'14'                ENSURE ONLY 15 DIGITS           00017400
         BNH   *+10                                                     00017500
         MVC   NOCHAR,=H'14'                                            00017600
         ZAP   NOWORDS,=P'1'            SET NO. WORDS TO PROCESS        00017700
         LA    R7,KEYGNXIT              SET EXIT ADDR                   00017800
         B     MVCOMNM                  MOVE                            00017900
MOVENUM  MVN   NUMTEST(0),WORDSAVE                                      00018000
CHKNUM   CLC   NUMTEST(0),WORDSAVE                                      00018100
SETCOMM  EQU   *                                                        00018200
         MVI   0(R3),C'C'               MOVE COMMERCIAL CODE            00018300
         MVI   12(R3),C' '                                    2601126   00018400
         LA    R3,1(0,R3)               GET NEXT CHAR ADDR              00018500
         LA    R4,SAVEWORD                                              00018600
         MVC   NOCHAR,=H'3'             SET FIRST WORD -  4 CHAR        00018700
         BAS   R7,MVCOMNM                                     2024183   00018800
         MVC   NOCHAR,=H'4'             SET SECOND WORD - 5 CHAR        00018900
         BAS   R7,MVCOMNM                                     2024183   00019000
         MVC   NOCHAR,=H'2'             SET THIRD WORD -  3 CHAR        00019100
         BAS   R7,MVCOMNM                                     2024183   00019200
         MVC   NOCHAR,=H'1'             SET FOURTH WORD - 2 CHAR        00019300
         BAS   R7,MVCOMNM                                     2024183   00019400
         MVC   NOCHAR,=H'0'             SET FIFTH WORD -  1 CHAR        00019500
         ZAP   NOWORDS,=P'1'                                            00019600
         LA    R7,KEYGNXIT                                              00019700
         SPACE 3                                                        00019800
MVCOMNM  EQU   *                                                        00019900
         IC    R5,WORDLNG               GET WORD LENGTH                 00020000
         CP    NOWORDS,=P'1'            CHK FOR LAST WORD               00020100
         BNE   MOVECOM2                                                 00020200
         CH    R5,NOCHAR                CHK FOR TOO MANY CHAR           00020300
         BNH   MOVECOM1                                                 00020400
         LH    R5,NOCHAR       DO NOT USE MORE OF LAST WORD THAN NORMAL 00020500
MOVECOM1 EQU   *                                                        00020600
         EX    R5,MVCNAME               MOVE WORD                       00020700
         B     KEYGNXIT                                                 00020800
MOVECOM2 EQU   *                                                        00020900
         CH    R5,NOCHAR                CHK FOR TOO MANY CHAR           00021000
         BNH   MOVECOM3                                                 00021100
         LH    R5,NOCHAR                                                00021200
MOVECOM3 EQU   *                                                        00021300
         EX    R5,MVCNAME               MOVE WORD                       00021400
         LA    R3,1(0,R3)               BUMP TO NEXT AVAILABLE CHAR     00021500
         AH    R3,NOCHAR                                                00021600
         SP    NOWORDS,=P'1'            SUBTRACT 1 FROM WORD COUNT      00021700
         LA    R4,24(0,R4)              GET NEXT WORD ADDR              00021800
         BR    R7                                                       00021900
         SPACE 3                                                        00022000
KEYGNXIT EQU   *                                                        00022100
         AIF   (&CICS).CICSRET                                          00022200
         CEETERM RC=(15)           RETURN                     2024448   00022300
         AGO   .DATA                                                    00022400
.CICSRET ANOP                                                           00022500
         EXEC  CICS RETURN                                              00022600
.DATA    ANOP                                                           00022700
         EJECT                                                          00022800
MVCNAME  MVC   0(0,R3),WORDSAVE                                         00022900
MVCWORD  MVC   WORDSAVE(0),0(R9)                                        00023000
CLEARKEY DC    16C' '                                         2601126   00023100
         DC    X'0000'                                                  00023200
         LTORG                                                          00023300
WORDAREA DSECT                                                          00024300
WORDSAVE DS    CL20                                                     00024400
WORDLNG  DS    CL1                                                      00024500
WLINEND  DS    CL1                                                      00024600
WLSTNAME DS    CL1                                                      00024700
         DS    CL1                                                      00024800
         AIF   ('&SYSPARM' NE 'CICS').DTSBAT                            00025083
FIKEYGNR CSECT                                                          00025084
         AGO   .DTSMRG                                                  00025085
.DTSBAT  ANOP                                                           00025086
SIKEYGNR CSECT                                                          00025087
.DTSMRG  ANOP                                                           00025088
         LTORG                                                          00025089
         DS    0D                                                       00025090
SITMSTMP DC    CL64'SIKEYGNR  -----TSD-             05/08/17  09.46.12' 00025091
* (C) 2017                                                              00025092
* FIDELITY NATIONAL INFORMATION SERVICES, INC. AND/OR ITS SUBSIDIARIES  00025093
* - ALL RIGHTS RESERVED WORLDWIDE.  THIS DOCUMENT IS PROTECTED UNDER    00025094
* TRADE SECRET AND COPYRIGHT LAWS AS THE PROPERTY OF FIDELITY NATIONAL  00025095
* INFORMATION SERVICES, INC. AND/OR ITS  SUBSIDIARIES.  COPYING,        00025096
* REPRODUCTION OR DISTRIBUTION SHOULD BE LIMITED AND ONLY TO EMPLOYEES  00025097
* WITH A "NEED TO KNOW" TO DO THEIR JOB. ANY DISCLOSURE OF THIS         00025098
* DOCUMENT TO THIRD PARTIES IS STRICTLY PROHIBITED.                     00025099
         END                                                            00025100
