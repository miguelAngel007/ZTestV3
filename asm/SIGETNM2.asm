*ASM XOPTS(NOEPILOG)                                                    00000100
*     * 802262 12/11/20 PROYECTO UPGRADE GN172 EDVR
         COPY  SIOPTNS                                                  00000200
         AIF   ('&SYSPARM' NE 'CICS').GEN1                              00000300
         SIRENT                                               1216075   00000310
         TITLE 'FIGETNM2 -  FIAS NAME EXTRACT/FORMAT ROUTINE'           00000400
*---------------------------------------------------------------------* 00000410
*                  ** HISTORY OF REVISIONS **                         * 00000420
*                                                                     * 00000430
* DESCRIPTION                                                 CHNGID  * 00000440
* __________________________________________________________  _______ * 00000450
* 07/20/11 MODIFIED FOR THREADSAFE COMPLIANCE                 1216075   00000484
* 03/04/09 RETURN FATAL ERROR FOR NAME MORE THAN FIVE WORDS   1015785   00000486
* 05/17/04 MODIFY CALL FOR COBOL/VSE                          0444987   00000488
* 03/13/03 MODIFY TO RMODE24 FOR VSE                          0344701   00000489
* 04/17/02 ADD LANGUAGE CODE AS OPTIONAL 3RD PARAMETER        2024478 * 00000490
* 01/24/00 ENABLE FOR 31-BIT ADDRESSING/CHANGE BAL TO BAS     2024183 * 00000492
* 12/10/97 ADDED SASIOPTE PROCESSING                          9913483 * 00000493
* 03/20/98 CORRECT TITLE WORD PROCESSING                      9910768 * 00000494
* 07/16/98 CORRECT CDLOAD PROCESSING                          9913845 * 00000495
* 05/15/96 ADDED CAPABILITY FOR 31-BIT ADDRESSING             9913165 * 00000496
* 06/30/95 ADDED CHECK FOR &LANG FOR VALID CHAR TRANSLATION   2602409 * 00000497
*---------------------------------------------------------------------* 00000498
FIGETNM2 DFHEIENT CODEREG=(11,12),DATAREG=(13),EIBREG=(10)              00000500
         LTORG ,                   FORCE DFHEIENT LITERALS IN RANGE
FIGETNM2 AMODE 31                                             9913165 / 00000535
FIGETNM2 RMODE ANY                                            9913165 / 00000540
         SIEQREG                                                        00000600
         L     R1,DFHEICAP         LOAD ADDR OF COMMON AREA             00000700
         B     *+24                                           1216075   00000800
         DC    CL20'FIGETNM2(SIGETNM2)'                                 00000900
         AGO   .GEN2                                                    00001000
.GEN1    ANOP                                                           00001100
         TITLE 'SIGETNM2 - NAME EXTRACT/FORMAT ROUTINE'                 00001200
SIGETNM2 START                                                          00001300
         LTORG ,                   FORCE DFHEIENT LITERALS IN RANGE
         AIF   (&VSE).VSE24                                   0344701   00001310
         SIBASE BASEREG=(11,12),RWNUM=0021,RWSUB=000                    00001400
         AGO   .GEN2                                          0344701   00001420
* THE FOLLOWING STATEMENT(S) MODIFIED BY                      0344701   00001424
.VSE24   ANOP                                                           00001425
* THE FOLLOWING STATEMENT(S) MODIFIED BY                      0344701   00001429
         SIBASE BASEREG=(11,12),RWNUM=0021,RWSUB=000,SETMODE=3124       00001430
.GEN2    ANOP                                                           00001500
         EJECT                                                          00001600
*                                                                       00001700
*        REGISTER USAGE                                                 00001800
*                                                                       00001900
*        R0    WORK REG                                                 00002000
*        R1    WORK REG                                                 00002100
*        R2    WORK REG, TEMP FOR TRT                                   00002200
*        R3    NAME/ADDRESS INPUT AREA                                  00002300
*        R4    COMMUNICATIONS AND RETURN AREA (FLAGS, ETC.)             00002400
*        R5    WORK REG                                                 00002500
*        R6    WORK REG                                                 00002600
*        R7    WORK REG                                                 00002700
*        R8    WORK REG                                                 00002800
*        R9    WORK REG                                                 00002900
*        R10   EIB  REG (CICS ONLY)                                     00003000
*        R11   BASE REG                                                 00003100
*        R12   BASE REG                                                 00003200
*        R14   WORK REG                                                 00003300
*        R15   WORK REG                                                 00003400
*                                                                       00003500
*                                                                       00003600
         USING NAAREA,R3                                                00003700
         USING GNM2WKAR,R4                                              00003800
         USING WORDAREA,R5                                              00003900
         USING NAMEPTAR,R6                                              00004000
         EJECT                                                          00004100
A0010    EQU   *                                                        00004200
         CLI   INITSW,X'FF'        CHK IF INITIALIZED                   00004300
         BNE   INIT                BR IF NOT                            00004400
         LM    R3,R4,0(R1)         LOAD PARAMETER ADDRESSES             00004500
         ST    R3,WKNMADDR         SAVE INPUT AREA ADDR IN WORK AREA    00004600
         MVC   NAMEWORK(250),0(R3) COPY INPUT AREA                      00004700
         MVC   NAMEWORK+250(50),250(R3) COPY INPUT AREA                 00004800
         LA    R3,NAMEWORK         POINT R3 TO NAME WORK AREA           00004900
         LA    R6,L'NAMEWORK       GET LENGTH OF NAME WORK AREA         00005000
         CH    R6,WKARLNG          IS LNG OF WORK AREA LESS THAN INPUT  00005100
         BNH   *+8                 YES                                  00005200
         LH    R6,WKARLNG          GET LENGTH OF INPUT AREA             00005300
         AR    R6,R3               ADD ADDR OF NAME WORK AREA           00005400
         ST    R6,INPEND           SAVE END-OF-INPUT ADDR               00005500
         MVI   WKRETCOD+1,X'00'    RESET RETURN CODE                    00005600
         MVI   WKNONAME,X'0C'      MOVE ZERO TO NO. NAMES               00005700
         MVI   WKLSTLNE,X'0C'      MOVE ZERO TO LAST LINE               00005800
         MVI   COMMIND,X'00'       SET COMMERCIAL WORD INDICATOR OFF    00005900
         MVI   LNMIND,X'00'        SET LAST NAME WORD INDICATOR OFF     00006000
         LA    R6,NAMETBL          GET ADDR OF NAMETBL                  00006100
         LA    R7,NONMRET          SET NO. OF ENTRIES IN TABLE          00006200
A0015    EQU   *                                                        00006300
         XC    0(NMARLNG,R6),0(R6) CLEAR NAME POINTER AREA              00006400
         LA    R6,NMARLNG(0,R6)    BUMP TO NEXT AREA                    00006500
         BCT   R7,A0015            LOOP BACK                            00006600
         LA    R6,WKNAMES          GET ADDR OF NAME RETURN AREA         00006700
         LA    R7,WKNAMELN         LENGTH OF NAME RETURN AREA           00006800
         LA    R8,*                DUMMY FROM ADDR                      00006900
         L     R9,CLRPADSP         LOAD SPACE AS PAD CHARACTER          00007000
         MVCL  R6,R8               CLEAR NAME RETURN AREA               00007100
         MVC   WKERRFLG,ZEROS      RESET ERROR FLAGS                    00007200
         CLI   WKFORCE,C'C'        IS COMMERCIAL NAME FORCED            00007300
         BE    A0020               YES - SET COMMERCIAL NAME FORCED     00007400
         LR    R15,R3              GET ADDR OF LAST BYTE OF FIRST LINE  00007500
         AH    R15,WKLNOFS                                              00007600
         AH    R15,WKLNLNG                                              00007700
         BCTR  R15,0                                                    00007800
         MVI   COMMFLAG,X'00'      RESET COMMERCIAL NAME FLAG           00007900
         CLI   0(R15),C'*'         ASTERISK IN LAST BYTE FORCES COMM    00008000
         BNE   A0030                                                    00008100
A0020    EQU   *                                                        00008200
         MVI   COMMFLAG,X'FF'      SET COMMERCIAL NAME FORCED FLAG      00008300
A0030    EQU   *                                                        00008400
         CLI   WKLNOFS+1,X'00'     IS THERE ANY LEADING LINE OFFSET     00008500
         BNE   A0040               YES - CLEAR OFFSETS                  00008600
         CLI   WKENLNOF+1,X'00'    IS THERE ANY END LINE OFFSET         00008700
         BE    A0100               NO - DONT NEED TO CLEAR ANYTHING     00008800
A0040    EQU   *                                                        00008900
         LR    R5,R3               GET NAME WORK AREA ADDR IN R5        00009000
         SR    R6,R6               CLEAR R6                             00009100
A0050    EQU   *                                                        00009200
         ICM   R6,B'0001',WKLNOFS+1  GET LEADING LINE OFFSET            00009300
         BZ    A0060               BR IF NO OFFSET                      00009400
         BCTR  R6,0                SUBTRACT 1 FOR EXECUTE               00009500
         EX    R6,CLROFS           CLEAR OFFSET AREA                    00009600
         AH    R5,WKLNOFS          ADD LEADING LINE OFFSET              00009700
A0060    EQU   *                                                        00009800
         AH    R5,WKLNLNG          ADD LINE LENGTH                      00009900
         ICM   R6,B'0001',WKENLNOF+1 GET END OF LINE OFFSET             00010000
         BZ    A0070               BR IF NO OFFSET                      00010100
         BCTR  R6,0                SUBTRACT 1 FOR EXECUTE               00010200
         EX    R6,CLROFS           CLEAR OFFSET AREA                    00010300
         AH    R5,WKENLNOF         ADD END OF LINE OFFSET               00010400
A0070    EQU   *                                                        00010500
         C     R5,NAMEWKEN         ARE WE AT END OF NAME AREA           00010600
         BL    A0050               NO - CLEAR NEXT OFFSET               00010700
         EJECT                                                          00010800
*********************************************************************** 00010900
*                                                                     * 00011000
*        WORD SEPARATOR ROUTINE                                       * 00011100
*                                                                     * 00011200
*********************************************************************** 00011300
*        REGISTER USAGE                                               * 00011400
*                                                                     * 00011500
*        R5    POINTER TO ENTRY IN SAVEWORD WORK AREA                 * 00011600
*        R6    LENGTH OF WORD                                         * 00011700
*                                                                     * 00011800
*********************************************************************** 00011900
*        ANYTHING IN PARENTHESIS GETS CLEARED                         * 00012000
*********************************************************************** 00012100
A0100    EQU   *                                                        00012200
         XC    TRTTBL2,TRTTBL2          CLEAR TRANSLATE TABLE 2         00012300
         MVI   TRTTBL2+C'(',C'('        STOP ON LEFT PAREN              00012400
         TRT   NADATA,TRTTBL2           CHK FOR LEFT PAREN              00012500
         BZ    A0200                    NO LEFT PAREN FOUND             00012600
         LA    R5,0(0,R1)               SAVE LEFT PAREN ADDR            00012700
         LA    R15,NADATA+L'NADATA-1    CALC REMAINING LENGTH           00012800
         SR    R15,R5                                                   00012900
         MVI   TRTTBL2+C'(',X'00'       CLEAR LEFT PAREN                00013000
         MVI   TRTTBL2+C')',C')'        STOP ON RIGHT PAREN             00013100
         EX    R15,FNDRPARN             FIND RIGHT PAREN                00013200
         BZ    A0200                    NO RIGHT PAREN FOUND            00013300
         MVI   TRTTBL2+C')',X'00'       CLEAR RIGHT PAREN               00013400
         LA    R1,0(0,R1)               BUMP PAST RIGHT PAREN           00013500
A0110    EQU   *                                                        00013600
         MVI   0(R5),C' '               CLEAR BETWEEN PARENTHESES       00013700
         LA    R5,1(0,R5)                                               00013800
         CR    R5,R1                    ARE WE PAST RIGHT PAREN         00013900
         BNH   A0110                    NOT YET                         00014000
         B     A0100                    GO BACK AND DO IT AGAIN         00014100
         EJECT                                                          00014200
*********************************************************************** 00014300
*        SEPARATE NAME AREA INTO INDIVIDUAL WORDS                     * 00014400
*        SAVE THE LENGTH OF EACH WORD                                 * 00014500
*        SET FLAGS AT LAST WORD OF A LINE AND AT END OF INPUT AREA    * 00014600
*********************************************************************** 00014700
A0200    EQU   *                                                        00014800
         MVI   TRTTBL2+C'(',X'00'       CLEAR LEFT PAREN                00014900
         MVI   TRTTBL2+C' ',C' '        STOP ON SPACE                   00015000
         MVI   TRTTBL2+C',',C','        STOP ON COMMA                   00015100
         MVI   WORDFLAG,X'00'           CLEAR WORDFLAG                  00015200
         LA    R6,SAVEWORD              GET TABLE AREA ADDR             00015300
         LA    R7,SAVEWDLN              GET TABLE AREA SIZE             00015400
         LA    R8,*                     DUMMY ADDRESS FOR MOVE          00015500
         SR    R9,R9                    PADDING CHAR AND ZERO FROM LNG  00015600
         MVCL  R6,R8                    CLEAR WORD AREA WITH X'00'      00015700
         MVI   SAVEWORD+SAVEWDLN-3,ENDOFNAM+ENDOFLIN PREVENT OVERRUN    00015800
         MVI   SAVEWORD+SAVEWDLN-2,GARBWD            PREVENT OVERRUN    00015900
         CLC   WLANG,=C'EN'        ENGLISH PROCESSING?        9913483   00015910
         BNE   A0200O              N, GO DO OTHER             9913483   00015920
         TR    NAMEWORK(250),CLRSPCHE   BLANK ANY SPECIAL CHA 9913483   00016000
         TR    NAMEWORK+250(50),CLRSPCHE BLANK ANY SPECIAL CH 9913483   00016100
         B     A0200X              CONTINUE                   9913483   00016110
A0200O   EQU   *                                              9913483   00016120
         TR    NAMEWORK(250),CLRSPCHO   BLANK ANY SPECIAL CHA 9913483   00016130
         TR    NAMEWORK+250(50),CLRSPCHO BLANK ANY SPECIAL CH 9913483   00016140
A0200X   EQU   *                                              9913483   00016150
         LH    R15,WKLNLNG              GET LINE LENGTH                 00016200
         AH    R15,WKLNOFS              ADD DATA OFFSET                 00016300
         AH    R15,WKENLNOF             ADD END OF LINE OFFSET          00016400
         STH   R15,RLLNLNG              SAVE REAL LINE LENGTH           00016500
         LA    R15,NAMEWORK(R15)        CALC NEXT LINE START ADDR       00016600
         ST    R15,NXTLNADR             AND SAVE IT                     00016700
         LA    R5,SAVEWORD              GET WORD AREA ADDR              00016800
         LH    R6,RLLNLNG               LOAD REAL LINE LENGTH           00016900
         OI    ENDLNFLG,BEGOFLIN        SET BEGINNING-OF-LINE FLAG      00017000
A0210    EQU   *                                                        00017100
*        FIND NEXT NON-BLANK                                            00017200
         BCTR  R6,0                     SUBTRACT 1 FOR EXECUTE          00017300
         EX    R6,TRNSL1                FIND FIRST NON-BLANK            00017400
         BZ    A0240                    GO TO A0240 IF ALL BLANKS       00017500
         NI    ENDLNFLG,X'FF'-ENDOFLIN  RESET END-OF-LINE FLAG          00017600
         LR    R3,R1                    LOAD ADDR OF FIRST NON-BLANK    00017700
         L     R6,NXTLNADR              CALC. LENGTH OF LINE REMAINING  00017800
         SR    R6,R1                                                    00017900
         BCTR  R6,0                     SUBTRACT 1 FOR EXECUTE          00018000
         EX    R6,TRNSL2                FIND NEXT BLANK OR COMMA        00018100
         BC    4,A0220                  BR IF BLNK FOUND AND NOT AT END 00018200
         OI    ENDLNFLG,ENDOFLIN        SET END-OF-LINE FLAG            00018300
         BC    2,A0220                  BR IF BLNK FOUND AND AT END     00018400
         L     R1,NXTLNADR              FORCE END-OF-WORD TO END-OF-LIN 00018500
A0220    EQU   *                                                        00018600
         LR    R6,R1                    GET END OF WORD ADDR            00018700
         SR    R6,R3                    SUBTRACT BEGINNING OF WORD ADDR 00018800
         LA    R7,20                    LOAD 20 INTO R7                 00018900
         CR    R6,R7                    CHK FOR LENGTH GREATER THAN 20  00019000
         BNH   A0230                    BR IF LENGTH OK                 00019100
         LA    R6,20                    FORCE LENGTH TO 20 BYTES        00019200
A0230    EQU   *                                                        00019300
         MVC   WORDSAVE,SPACES          CLEAR WORD SAVE ENTRY           00019400
         BCTR  R6,0                     SUBTRACT 1 FOR EXECUTE          00019500
         EX    R6,MVCWORD               MOVE WORD TO LIST               00019600
         STC   R6,WORDLNG               STORE WORD LENGTH - 1           00019700
         MVC   WLINEND,ENDLNFLG         MOVE IN END-OF-LINE FLAG        00019800
         NI    ENDLNFLG,X'FF'-BEGOFLIN  RESET BEGINNING-OF-LINE FLAG    00019900
         MVI   WORDFLAG,X'FF'           SET WORD FLAG                   00020000
         CLI   0(R1),C','               DID LAST SCAN END ON A COMMA    00020100
         BNE   *+8                      NO                              00020200
         OI    WRDSTAT2,COMINWD         SET COMMA-AT-END-OF-WORD FLAG   00020300
         LA    R5,24(0,R5)              GET ADDR OF NEXT WORD IN LIST   00020400
         C     R5,WORDEND               CHK FOR END OF WORD LIST        00020500
         BNL   A0260                    BR IF END                       00020600
         TM    ENDLNFLG,ENDOFLIN        CHK FOR END-OF-LINE FLAG ON     00020700
         BO    A0250                    BR IF AT END-OF-LINE            00020800
         LR    R3,R1                    SET NEXT SCAN START ADDR        00020900
         L     R6,NXTLNADR              CALC REMAINING LENGTH           00021000
         SR    R6,R3                                                    00021100
         B     A0210                    GO TO FIND NEXT WORD            00021200
A0240    EQU   *                                                        00021300
*        BR HERE IF NO MORE INPUT DATA ON THE LINE                      00021400
         C     R5,WORDSTRT              CHK FOR FIRST WORD OF LIST      00021500
         BE    A0250                    BR IF NO WORDS SAVED            00021600
         SH    R5,H24                   BACK UP ONE WORD                00021700
         OI    WLINEND,ENDOFLIN         SET LINE-END FLAG IN PREV WORD  00021800
         LA    R5,24(0,R5)              BUMP UP TO CURRENT WORD         00021900
A0250    EQU   *                                                        00022000
         CLI   COMMFLAG,X'FF'           WAS COMMERCIAL NAME FORCED      00022100
         BE    A0260                    YES - SCAN FIRST LINE ONLY      00022200
         CLC   NXTLNADR,INPEND          CHK FOR END OF INPUT AREA       00022300
         BNL   A0260                    BR IF END OF AREA               00022400
         OI    ENDLNFLG,BEGOFLIN        SET BEGINNING OF LINE FLAG      00022500
         L     R3,NXTLNADR              GET NEW LINE ADDR               00022600
         LR    R15,R3                   CALC ADDR OF START OF NEXT LINE 00022700
         AH    R15,RLLNLNG                                              00022800
         ST    R15,NXTLNADR             SAVE NEXT LINE ADDR             00022900
         LH    R6,RLLNLNG               LOAD REAL LINE LENGTH           00023000
         C     R15,INPEND               CHK FOR END OF INPUT AREA       00023100
         BNH   A0210                    GO TO A0210 IF NOT END          00023200
         MVC   NXTLNADR,INPEND          FORCE ENDING ADDR               00023300
         L     R6,INPEND                CALC LENGTH OF LINE REMAINING   00023400
         SR    R6,R3                                                    00023500
         B     A0210                    GO TO A0210                     00023600
A0260    EQU   *                                                        00023700
         CLI   WORDFLAG,X'FF'           CHK FOR WORDS IN TABLE          00023800
         BNE   A0270                    BR IF NO WORDS IN TABLE         00023900
         SH    R5,H24                   BACK UP ONE WORD                00024000
         OI    WLINEND,ENDOFNAM+ENDOFLIN   SET BOTH END FLAGS           00024100
         B     A0290                                                    00024200
A0270    EQU   *                                                        00024300
         MVI   WKRETCOD+1,X'10'         SET RETURN CODE TO 16           00024400
         MVI   WKERRFLG,C'1'            SET FIRST ERROR FLAG - NO INPUT 00024500
         B     Z0010                    EXIT                            00024600
A0290    EQU   *                                                        00024700
         CLI   COMMFLAG,X'FF'           WAS COMMERCIAL NAME FORCED      00024800
         BNE   A0500                    BR IF NOT FORCED COMMERCIAL     00024900
         EJECT                                                          00025000
*********************************************************************** 00025100
*        IF A COMMERCIAL NAME IS FORCED, A COMMERCIAL NAME IS         * 00025200
*        RETURNED USING ONLY THE WORDS IN THE FIRST NAME LINE.        * 00025300
*********************************************************************** 00025400
A0300    EQU   *                                                        00025500
         MVI   WKNONAME,X'1C'           MOVE 1 TO NUM NAMES RETURNED    00025600
         MVI   WKLSTLNE,X'1C'           MOVE 1 TO LAST NAME LINE        00025700
         LA    R7,WKNAMES               GET ADDR OF NAME RETURN AREA    00025800
         USING WKNMAREA,R7              DEFINE DSECT BASE REGISTER      00025900
         MVI   WKNMTYPE,C'C'            SET COMMERCIAL CODE IN NAME     00026000
         MVI   WKNMLINE,X'1C'           MOVE 1 TO NAME LINE NUMBER      00026100
         MVC   WKNMERFL,ZEROS           ZERO NAME ERROR FLAGS           00026200
         LA    R5,SAVEWORD              POINT TO WORD AREA              00026300
         LA    R6,NAMETBL               POINT TO NAME POINTER AREA      00026400
         LA    R8,NMWDADTB              POINT TO NAME WORD ADDR STACK   00026500
         LA    R9,WKNMNAME              POINT TO NAME RETURN AREA       00026600
         SR    R1,R1                    CLEAR REG 1                     00026700
         DROP  R7                       RELEASE DSECT BASE REG          00026800
A0310    EQU   *                                                        00026900
*        BUILD COMMERCIAL NAME IN FIRST NAME AREA                       00027000
*        SINCE THE RETURN NAME AREA IS AS LONG AS OR LONGER THAN ANY    00027100
*        APPLICATION NAME LINE, I DONT HAVE TO CHECK FOR OVERFLOWING    00027200
*        THE OUTPUT NAME LINE.                                          00027300
         IC    R1,WORDLNG               GET LENGTH OF WORD              00027400
         EX    R1,MOVECOM               MOVE WORD TO RETURN AREA        00027500
         ST    R9,NMLSWDAD              SAVE LAST WORD ADDR             00027600
         STC   R1,NMLSWDLN              SAVE LAST WORD LENGTH           00027700
         TR    NMNOWD,ADDONE            ADD 1 TO NO. WORDS IN NAME      00027800
         CLI   NMNOWD,X'05'             MORE THAN 5 WORDS IN NAME       00027900
         BH    *+8                      YES - DONT SAVE ADDR            00028000
         ST    R5,0(0,R8)               STORE ADDR OF WORD IN STACK     00028100
         TM    WLINEND,ENDOFLIN         IS THIS THE END OF LINE 1       00028200
         BO    A0320                    YES - GENERATE COMM KEY         00028300
         LA    R9,2(R1,R9)              BUMP TO NEXT OUTPUT POSITION    00028400
         ST    R9,NMNXWDAD              SAVE NAME NEXT WORD ADDR        00028500
         LA    R5,24(0,R5)              BUMP TO NEXT WORD IN SAVEAREA   00028600
         LA    R8,4(0,R8)               BUMP TO NEXT STACK ADDR         00028700
         B     A0310                    GO MOVE NEXT WORD               00028800
A0320    EQU   *                                                        00028900
         OI    WLINEND,ENDOFNAM         SET END OF NAME FLAG            00029000
         OI    NMNMFLGS,NMEND           SET END OF NAMES FLAG           00029100
         B     D0600                    GO GENERATE COMMERCIAL KEY      00029200
         SPACE 3                                                        00029300
CLROFS   MVC   0(0,R5),SPACES           CLEAR OFFSET AREA               00029400
TRNSL1   TRT   NADATA(0),TRTTBL1        TRT TO FIND NON-BLANK           00029500
TRNSL2   TRT   NADATA(0),TRTTBL2        TRT TO FIND BLANK OR COMMA      00029600
FNDRPARN TRT   0(0,R5),TRTTBL2          TRT TO FIND RIGHT PARENTHESIS   00029700
MVCWORD  MVC   WORDSAVE(0),NADATA       MVC TO MOVE WORD TO WORD TABLE  00029800
MOVECOM  MVC   0(0,R9),WORDSAVE         MVC TO MOVE WORD TO RET AREA    00029900
         EJECT                                                          00030000
*********************************************************************** 00030100
*        CHECK FOR USER KEY WORD TABLE ID ENTERED. IF NOT ENTERED,    * 00030200
*        USER DEFAULT ID.  IF THE KEY WORD TABLE IS THE CURRENT TABLE,* 00030300
*        ALL IS OK SO GO TO B0010.  OTHERWISE LOAD THE KEY WORD       * 00030400
*        TABLE, SORT THE KEY WORDS, STORE THE ADDRESSES IN THE        * 00030500
*        KEY WORD TABLE SAVE AREA, AND MOVE THE ADDRESSES TO THE      * 00030600
*        CURRENT TABLE AREA.                                          * 00030700
*********************************************************************** 00030800
A0500    EQU   *                                                        00030900
         CLI   WKTBLID,X'00'            WAS ANY TABLE ID ENTERED        00031000
         BNE   A0510                    YES                             00031100
         MVC   WKTBLID,=CL2'00'         FORCE DEFAULT TABLE ID          00031200
A0510    EQU   *                                                        00031300
         CLC   KEYWDID,WKTBLID          IS THIS THE CURRENT TABLE ID    00031400
         BE    B0010                    YES - EVERYTHING IS OK          00031500
         LA    R6,KEYADRTB              GET ADDRESS OF KEY ADDRESS TBL  00031600
A0520    EQU   *                                                        00031700
         CLI   0(R6),X'FF'              END OF TABLE                    00031800
         BE    A0530                    YES - LOAD NEW TABLE            00031900
         CLC   WKTBLID,0(R6)            IS THIS THE RIGHT TABLE         00032000
         BE    A0560                    YES - MOVE TO CURRENT TABLE     00032100
         LA    R6,KEYTBLNG(0,R6)        BUMP TO NEXT ENTRY              00032200
         B     A0520                    LOOP BACK                       00032300
A0530    EQU   *                                                        00032400
         MVC   KEYWDID,WKTBLID          SAVE TABLE ID                   00032500
         MVC   KEYTBLNM+6(2),WKTBLID    MAKE KEY TABLE NAME             00032600
         LA    R2,KEYTBLNM              POINT R2 TO KEY TABLE NAME      00032700
         AIF   ('&SYSPARM' EQ 'CICS').A0530C                            00032800
         AIF   (&VSE).A0530V                                            00032900
         LOAD  EPLOC=(2)                LOAD KEY WORD TABLE             00033000
         LR    R2,R0                    GET LOAD ADDR IN R2             00033100
         AGO   .A0530E                                                  00033200
.A0530V  ANOP                                                           00033300
         CDLOAD (2),RETPNF=YES          LOAD KEY WORD TABLE   9913845   00033400
         LTR   R15,R15                  GOOD CDLOAD?          9913845   00033410
         BE    A0530GL                  YES, CONTINUE         9913845   00033420
         MVC   MSSG(8),KEYTBLNM         KEY TABLE NAME        9913845   00033430
         MVC   MSSG+8(15),=CL15' CDLOAD FAILURE'              9913845   00033440
         SIMSGR R14,23                                        9913845   00033450
         CANCEL ALL                                           9913845   00033460
A0530GL  DS    0H                                             9913845   00033470
         LR    R2,R1                    GET LOAD ADDR IN R2             00033500
         AGO   .A0530E                                                  00033600
.A0530C  ANOP                                                           00033700
         EXEC CICS HANDLE CONDITION PGMIDERR NOTAUTH                    00033800
         STM   3,5,INITSAVE    SAVE REGISTERS                 1216075   00033850
* THE FOLLOWING STATEMENT(S) MODIFIED BY                      1216075   00033899
         EXEC CICS LOAD PROGRAM(KEYTBLNM)                              X00033900
               FLENGTH(STORLEN)                                        X00033910
               ENTRY(R3)                                               X00033920
               SET(R2)                                                  00033930
         L     R3,STORLEN                                     1216075   00033935
         STH   R3,HWORD                                       1216075   00033940
* THE FOLLOWING STATEMENT(S) MODIFIED BY                      1216075   00033942
         EXEC CICS GETMAIN SET(R4) LENGTH(HWORD) NOHANDLE               00033945
         L     R1,8(0,R2)        GET KEYWORD ENTRY OFFSET     1216075   00033950
         AR    R1,R2             ADD BASE TO GET ABSOLUTE     1216075   00033955
         ST    R1,WRDTBLAD       SAVE ABSOLUTE KEYWORD ADDR   1216075   00033960
         L     R1,12(0,R2)       GET KEYWORD END OFFSET       1216075   00033965
         AR    R1,R2             ADD BASE TO GET ABSOLUTE     1216075   00033970
         ST    R1,WRDTBLEN       SV ABS KEYWORD END ADDR      1216075   00033975
         LH    R5,HWORD          LENGTH OF WORDTABLE          1216075   00033980
         LR    R3,R5             EVENODD PAIR FOR MVCL        1216075   00033982
         MVCL  R4,R2        WORDTABLE TO AQUIRED STG          1216075   00033985
         SH    R4,HWORD     RESTORE R4 TO PREV STATUS         1216075   00033987
         LR    R2,R4        R2 IS NOW AQUIRED STG PTR         1216075   00033989
         LM    3,5,INITSAVE      RESTORE REGS                 1216075   00033990
.A0530E  ANOP                                                           00034000
         L     R1,8(0,R2)               GET KEYWORD ENTRY OFFSET        00034100
         AR    R1,R2                    ADD BASE TO GET ABSOLUTE        00034200
         ST    R1,WRDTBLAD              SAVE ABSOLUTE KEYWORD ADDR      00034300
         L     R1,12(0,R2)              GET KEYWORD END OFFSET          00034400
         AR    R1,R2                    ADD BASE TO GET ABSOLUTE        00034500
         ST    R1,WRDTBLEN              SAVE ABSOLUTE KEYWORD END ADDR  00034600
         L     R1,16(0,R2)              GET DOUBLEWORD TABLE OFFSET     00034700
         AR    R1,R2                    ADD BASE TO GET ABSOLUTE        00034800
         ST    R1,DBLWORD               SAVE ABSOLUTE DOUBLEWORD ADDR   00034900
         L     R1,20(0,R2)              GET DASHWORD TABLE OFFSET       00035000
         AR    R1,R2                    ADD BASE TO GET ABSOLUTE        00035100
         ST    R1,DSHWORD               SAVE ABSOLUTE DASHWORD ADDR     00035200
*********************************************************************** 00035300
*        SET UP FACTORS FOR BINARY SEARCH ROUTINE                     * 00035400
*********************************************************************** 00035500
         STM   R0,R4,INITSAVE SAVE REGISTERS                            00035600
         L     R1,WRDTBLEN    R1 HAS END OF TABLE                       00035700
         S     R1,WRDTBLAD    CALC TABLE SIZE IN BYTES                  00035800
         LA    R2,TBLWDLNG    PUT TABLE ENTRY SIZE IN REG 2             00035900
         LR    R0,R2          CALC TABLE ENTRY SIZE TO POWERS OF 2      00036000
         SLL   R0,1             UNTIL GREATER THAN TABLE SIZE           00036100
         CR    R0,R1                                                    00036200
         BNH   *-6                                                      00036300
         L     R1,WRDTBLEN    R1 HAS END OF TABLE                       00036400
         LR    R3,R0          CALC BEGINNING ADJUSTMENT TO GET TO       00036500
         SRL   R3,1             CENTER OF SEARCH                        00036600
         SR    R0,R2          CALC BEGINNING ADDRESS                    00036700
         A     R0,WRDTBLAD                                              00036800
         STM   R0,R3,BINFACS  STORE INITIAL BINARY SEARCH FACTORS       00036900
*                                                                       00037000
*********************************************************************** 00037100
*        SORT WORD TABLE FOR BINARY SEARCH ROUTINE                    * 00037200
*********************************************************************** 00037300
SRT10    EQU   *                                                        00037400
         L     R2,WRDTBLAD         GET WORD LIST ADDR                   00037500
         LA    R3,TBLWDLNG(0,R2)   POINT TO SECOND ENTRY                00037600
         SR    R4,R4               CLEAR R4 FOR SORT FLAG               00037700
SRT20    EQU   *                                                        00037800
         CLI   0(R3),X'FF'         END OF TABLE                         00037900
         BE    SRT40               YES                                  00038000
         CLC   0(TBLWDLNG,R2),0(R3)  FIRST FIELD LOWER THAN NEXT        00038100
         BNH   SRT30               YES - DONT SWITCH ENTRIES            00038200
         MVC   WORDWORK(16),0(R2)  SWITCH ENTRIES                       00038300
         MVC   0(TBLWDLNG,R2),0(R3)                                     00038400
         MVC   0(TBLWDLNG,R3),WORDWORK                                  00038500
         LA    R4,1                SET FLAG FOR ENTRIES SWITCHED        00038600
SRT30    EQU   *                                                        00038700
         LA    R2,TBLWDLNG(0,R2)   BUMP TO NEXT ENTRIES                 00038800
         LA    R3,TBLWDLNG(0,R3)                                        00038900
         B     SRT20                                                    00039000
SRT40    EQU   *                                                        00039100
         LTR   R4,R4               ANY ENTRIES SWITCHED                 00039200
         BNZ   SRT10               YES - DO IT AGAIN                    00039300
         LM    R0,R4,INITSAVE      RESTORE REGISTERS                    00039400
         LA    R6,KEYADRTB         GET ADDRESS OF KEY ADDRESS TBL       00039500
A0540    EQU   *                                                        00039600
         CLI   0(R6),X'00'         IS THIS AN EMPTY SLOT                00039700
         BE    A0550               YES - MOVE CURRENT TABLE TO SLOT     00039800
         CLI   KEYTBLNG(R6),X'FF'  END OF TABLE                         00039900
         BE    A0550               YES - REUSE LAST SLOT                00040000
         LA    R6,KEYTBLNG(0,R6)   BUMP TO NEXT ENTRY                   00040100
         B     A0540               LOOP BACK                            00040200
A0550    EQU   *                                                        00040300
         MVC   0(KEYTBLNG,R6),KEYWDID  MOVE CURRENT TABLE TO SLOT       00040400
         B     B0010                                                    00040500
A0560    EQU   *                                                        00040600
         MVC   KEYWDID(KEYTBLNG),0(R6) MOVE SLOT TO CURRENT TABLE       00040700
         B     B0010                                                    00040800
         EJECT                                                          00040900
*********************************************************************** 00041000
*        FIND ANY WORDS WITH A DASH IN THEM.                          * 00041100
*        IF A WORD BEGINS WITH A DASH, THEY CAN BE FLAGGED BY KEY     * 00041200
*        WORDS IN THE KEY WORD TABLE.                                 * 00041300
*        IF A WORD HAS AN IMBEDDED DASH, CHECK TO ELIMINATE THE DASH  * 00041400
*        AND ANY TEXT IN THE WORD FOLLOWING THE DASH.                 * 00041500
*********************************************************************** 00041600
B0010    EQU   *                                                        00041700
         L     R6,DSHWORD               GET ADDR OF DASH WORD TABLE     00041800
         CLI   0(R6),X'FF'              ANY DASH WORD TABLE             00041900
         BE    B0100                    NO - CAN'T CHECK ANYTHING       00042000
         XC    TRTTBL2,TRTTBL2          CLEAR TRT TABLE 2               00042100
         MVI   TRTTBL2+C'-',C'-'        PUT DASH IN TRT TABLE 2         00042200
         LA    R5,SAVEWORD              GET ADDR OF WORD TABLE          00042300
B0020    EQU   *                                                        00042400
         TRT   WORDSAVE,TRTTBL2         CHK FOR DASH IN WORD            00042500
         BNZ   B0040                    BR IF DASH FOUND                00042600
B0030    EQU   *                                                        00042700
         TM    WLINEND,ENDOFNAM         END OF NAMES                    00042800
         BO    B0100                    YES - SCAN KEY WORD TABLE       00042900
         LA    R5,24(0,R5)              BUMP TO NEXT WORD               00043000
         B     B0020                    CHK FOR DASH IN THIS WORD       00043100
B0040    EQU   *                                                        00043200
         L     R6,DSHWORD               GET ADDR OF DASH WORD TABLE     00043300
         SR    R7,R7                    CLEAR REG 7 FOR LNG             00043400
B0050    EQU   *                                                        00043500
         IC    R7,0(0,R6)               GET COMPARE LENGTH              00043600
         EX    R7,COMPDASH              COMPARE WORD                    00043700
         BE    B0060                    BR IF EQUAL                     00043800
         LA    R6,16(0,R6)              BUMP TO NEXT DASH WORD          00043900
         CLI   0(R6),X'FF'              END OF TABLE                    00044000
         BNE   B0050                    NO - CHK NEXT DASH WORD         00044100
         B     B0030                    CHECK NEXT WORD IN TABLE        00044200
B0060    EQU   *                                                        00044300
         SR    R1,R5                    CALC LNG OF GOOD WORD           00044400
         BZ    B0030                    BR IF DASH IS ONLY CHARACTER    00044500
         BCTR  R1,0                     SUBTRACT 1                      00044600
         STC   R1,WORDLNG               PUT NEW LNG IN TABLE            00044700
         B     B0030                                                    00044800
COMPDASH CLC   1(0,R1),1(R6)            CLC FOR DASH WORD COMPARE       00044900
         EJECT                                                          00045000
B0100    EQU   *                                                        00045100
*********************************************************************** 00045200
*        COMPARE INDIVIDUAL WORDS WITH KEY WORDS IN THE WORD TABLE    * 00045300
*        TO IDENTIFY SPECIAL WORDS - COMMERCIAL KEY WORDS,            * 00045400
*        CONNECTOR WORDS, SALUTATIONS (E.G. MR., MRS., DR.),          * 00045500
*        TITLES (E.G. JR., SR., MD.), AND GARBAGE WORDS (WE DON'T     * 00045600
*        CARE ABOUT THESE).                                           * 00045700
*********************************************************************** 00045800
         LA    R5,SAVEWORD              GET ADDR OF WORD TABLE          00045900
         STM   R3,R4,SAVE2              SAVE REGS 3 AND 4               00046000
         XC    TRTTBL2,TRTTBL2          CLEAR TRT TABLE 2               00046100
         MVI   TRTTBL2+C'&&',X'04'      PUT AMPERSAND INDEX INTO TABLE  00046200
         MVI   TRTTBL2+C'''',X'08'      PUT QUOTE INDEX INTO TABLE      00046300
         MVI   TRTTBL2+C'#',X'0C'       PUT POUND SIGN INDEX INTO TABLE 00046400
B0120    EQU   *                                                        00046500
*********************************************************************** 00046600
*        BINARY SEARCH OF WORD TABLE                                  * 00046700
*        REGS  R1 = SEARCH ADDR                                       * 00046800
*              R2 = END OF TABLE ADDR                                 * 00046900
*              R3 = SIZE OF TABLE ENTRY                               * 00047000
*              R4 = SEARCH ADJUSTMENT FACTOR                          * 00047100
*********************************************************************** 00047200
         LM    R1,R4,BINFACS       LOAD TABLE SEARCH FACTORS            00047300
B0130    EQU   *                                                        00047400
         SR    R1,R4               ADJUST SEARCH ADDR DOWN              00047500
         CR    R4,R3               IF ADJUST NOT LESS THAN ENTRY SIZE   00047600
         BNL   B0150                  CONTINUE SEARCH                   00047700
         SR    R1,R4               POINT TO NEXT LOWER ENTRY            00047800
         B     B0160               GO COMPARE LAST ENTRY                00047900
B0140    EQU   *                                                        00048000
         AR    R1,R4               ADJUST SEARCH ADDR UP                00048100
         CR    R4,R3               IF ADJUST NOT LESS THAN ENTRY SIZE   00048200
         BNL   B0150                  CONTINUE SEARCH                   00048300
         AR    R1,R4               POINT TO NEXT HIGHER ENTRY           00048400
         B     B0160               GO COMPARE LAST ENTRY                00048500
B0150    EQU   *                                                        00048600
         SRL   R4,1                HALVE THE SEARCH ADJUSTMENT FOR NEXT 00048700
         CR    R1,R2               SEARCH ADDR PAST PHYSICAL TABLE END  00048800
         BNL   B0130               YES - ADJUST DOWN                    00048900
         CLC   WORDSAVE(15),0(R1)  COMPARE WORD TO TABLE                00049000
         BL    B0130               LOW - GO TO ADJUST DOWN              00049100
         BH    B0140               HIGH - GO TO ADJUST UP               00049200
         B     B0170               EQUAL - GO SAVE WORD TYPE            00049300
B0160    EQU   *                                                        00049400
         CLC   WORDSAVE(15),0(R1)  IS LAST WORD EQUAL                   00049500
         BNE   B0200               NO - CHK SPECIAL                     00049600
B0170    EQU   *                   BR HERE WHEN SEARCH FINDS WORD       00049700
         MVC   WORDSTAT,15(R1)     MOVE TYPE FLAGS                      00049800
         TM    WORDSTAT,COMMWD     IS THIS A COMMERCIAL WORD            00049900
         BZ    B0180               NO                                   00050000
         MVI   COMMIND,X'FF'       SET COMMERCIAL WORD INDICATOR FLAG   00050100
B0180    EQU   *                                                        00050200
         TM    WORDSTAT,LNMWD      IS THIS A LAST NAME WORD             00050300
         BZ    B0200               NO                                   00050400
         MVI   LNMIND,X'FF'        SET LAST NAME WORD INDICATOR FLAG    00050500
         EJECT                                                          00050600
*********************************************************************** 00050700
*        SPECIAL PROCESSING FOR WORDS WHICH CONTAIN AMPERSANDS,       * 00050800
*        QUOTES, OR POUND SIGNS                                       * 00050900
*        ALSO CHECK FOR SPECIAL WORD PAIRS                            * 00051000
*********************************************************************** 00051100
B0200    EQU   *                                                        00051200
         LM    R3,R4,SAVE2              RESTORE REGS 3 AND 4            00051300
         SR    R2,R2                    CLEAR R2 FOR TRT                00051400
         TRT   WORDSAVE,TRTTBL2         CHK FOR &, #, OR QUOTE          00051500
         BZ    B0240                    NONE FOUND - CHK FOR PAIR       00051600
         B     *(R2)                    BR WITH INDEX                   00051700
         B     B0210                    BR IF AMPERSAND                 00051800
         B     B0220                    BR IF QUOTE - SET COMM IF 'S    00051900
         B     B0230                    BR IF POUND SIGN - SET COMM     00052000
B0210    EQU   *                                                        00052100
         CLI   WORDLNG,X'00'            IS THE AMPERSAND ONE LETTER     00052200
         BNE   B0240                    NO - CHK FOR PAIR               00052300
         TM    WLINEND,ENDOFLIN         END OF LINE                     00052400
         BO    B0240                    YES - CHK FOR PAIR              00052500
*        THE WORD BEFORE AND THE WORD AFTER THE AMPERSAND MUST          00052600
*        BE ONE LETTER EACH.  E.G. J & S CAFE                           00052700
         SH    R5,H24                   BACK UP ONE WORD                00052800
         CLI   WORDLNG,X'00'            IS IT ONE LETTER                00052900
         LA    R5,24(0,R5)              BUMP BACK TO WORD WITH AMPER    00053000
         BNE   B0240                    NO - CHK FOR PAIR               00053100
         CLI   WORDLNG+24,X'00'         IS NEXT WORD ONE LETTER         00053200
         BNE   B0240                    NO - CHK FOR PAIR               00053300
         NI    WORDSTAT,X'FF'-CONNWD    RESET CONNECTOR FLAG            00053400
         OI    WORDSTAT,COMMWD          SET COMMERCIAL FLAG IN WORD     00053500
         MVI   COMMIND,X'FF'            SET COMMERCIAL WORD INDICATOR   00053600
         B     B0280                    CHK FOR END                     00053700
B0220    EQU   *                                                        00053800
         CLC   1(2,R1),=CL2'S '         CHK FOR 'S                      00053900
         BNE   B0240                    CHK FOR PAIR IF NOT 'S          00054000
B0230    EQU   *                                                        00054100
         OI    WORDSTAT,COMMWD          SET COMMERCIAL FLAG IN WORD     00054200
         MVI   COMMIND,X'FF'            SET COMMERCIAL WORD INDICATOR   00054300
B0240    EQU   *                        CHK FOR WORD PAIRS              00054400
         L     R6,DBLWORD               GET ADDR OF WORD PAIR TABLE     00054500
B0250    EQU   *                                                        00054600
         CLC   WORDSAVE(12),0(R6)       CHK FOR FIRST WORD OF PAIR      00054700
         BH    B0260                    IF HIGH, GET NEXT PAIR          00054800
         BL    B0280                    IF LOW, DONT CHK ANY MORE       00054900
         CLC   WORDSAVE+24(12),12(R6)   CHK FOR SECOND WORD OF PAIR     00055000
         BE    B0270                    IF EQ, SET COMM FLAGS           00055100
B0260    EQU   *                                                        00055200
         LA    R6,24(0,R6)              BUMP TO NEXT PAIR               00055300
         CLI   0(R6),X'FF'              AT END OF WORD PAIRS            00055400
         BNE   B0250                    NO - GO CHECK NEXT PAIR         00055500
         B     B0280                    YES - DO NEXT WORD              00055600
B0270    EQU   *                                                        00055700
         MVI   COMMIND,X'FF'            SET COMMERCIAL WORD INDICATOR   00055800
         MVI   WORDSTAT,COMMWD          SET COMM FLAG IN WORD           00055900
         MVI   WORDSTAT+24,COMMWD       SET COMM FLAG IN NEXT WORD      00056000
         LA    R5,24(0,R5)              BUMP TO NEXT WORD               00056100
B0280    EQU   *                                                        00056200
         TM    WLINEND,ENDOFNAM         ARE WE AT THE END OF NAMES      00056300
         BO    C0010                    YES                             00056400
         LA    R5,24(0,R5)              BUMP TO NEXT WORD               00056500
         B     B0120                    GO BACK AND CHECK NEXT WORD     00056600
         EJECT                                                          00056700
*********************************************************************** 00056800
*        START ANALYZING THE WORDS                                    * 00056900
*********************************************************************** 00057000
*        FIND LOGICAL END OF NAMES IN SAVEWORD TABLE                  * 00057100
*********************************************************************** 00057200
C0010    EQU   *                                                        00057300
*********************************************************************** 00057400
*        IF THE FIRST LINE CONTAINS ONLY ONE GARBAGE WORD,            * 00057500
*        ADD TO THE LAST LINE COUNTER.                                * 00057600
*********************************************************************** 00057700
         LA    R5,SAVEWORD              GET ADDR OF SAVEWORD AREA       00057800
         TM    WORDSTAT,GARBWD          IS FIRST WORD A GARBAGE WORD    00057900
         BZ    C0020                    NO - FIND END OF LINE           00058000
         TM    WLINEND,ENDOFLIN         ARE WE AT END OF LINE           00058100
         BZ    C0020                    NO - FIND END OF LINE           00058200
         AP    WKLSTLNE,P1              ADD 1 TO LAST LINE NUMBER       00058300
*********************************************************************** 00058400
*        FIND END OF CURRENT LINE                                     * 00058500
*********************************************************************** 00058600
C0020    EQU   *                                                        00058700
         TM    WLINEND,ENDOFLIN         END OF LINE                     00058800
         BO    C0040                    YES                             00058900
C0030    EQU   *                                                        00059000
         LA    R5,24(0,R5)              BUMP TO NEXT WORD               00059100
         B     C0020                    FIND END OF LINE                00059200
C0040    EQU   *                                                        00059300
         AP    WKLSTLNE,P1              ADD 1 TO LAST LINE NUMBER       00059400
         TM    WLINEND,ENDOFNAM         ARE WE AT THE END OF THE NAMES  00059500
         BO    C0090                    YES - THATS ALL WE NEED TO DO   00059600
         TM    WORDSTAT,CONNWD          CONNECTOR AT END OF LINE        00059700
         BO    C0030                    YES - FIND END OF NEXT LINE     00059800
         LR    R0,R5                    SAVE R5                         00059900
C0050    EQU   *                                                        00060000
         LA    R5,24(0,R5)              BUMP TO FIRST WORD OF NEXT LINE 00060100
         TM    WORDSTAT,CONNWD          IS THIS A CONNECTOR             00060200
         BO    C0020                    YES - FIND END OF THIS LINE     00060300
         TM    WORDSTAT,SALUTWD         IS THIS A SALUTATION            00060400
         BZ    C0060                    NO - CHK FOR OTHER              00060500
         OI    WORDSTAT,CONNWD          MAKE SALUTATION ALSO A CONNECTR 00060600
         B     C0020                    GO FIND END OF THIS LINE        00060700
C0060    EQU   *                                                        00060800
         TM    WORDSTAT,X'FF'-COMMWD    ANY SPECIAL WORD NOT COMM       00060900
         BZ    C0070                    NO - FLAG PREV WORD AS ENDOFNM  00061000
*              FOR OTHER WORDS (TITLE, GARBAGE), CHECK FOR END-OF-LINE  00061100
*              TO CALCULATE THE LAST LINE AND THEN CHECK THE NEXT WORD  00061200
         TM    WLINEND,ENDOFLIN         IS THIS THE END-OF-LINE         00061300
         BZ    C0050                    NO - CHECK NEXT WORD            00061400
         AP    WKLSTLNE,P1              ADD 1 TO LAST LINE NUMBER       00061500
         B     C0050                    CHECK NEXT WORD                 00061600
C0070    EQU   *                                                        00061700
         CLI   WKNMONLY,C'1'            WAS I GIVEN NAMES ONLY          00061800
         BNE   C0075                    YES                             00061900
         LR    R5,R0                    RESTORE R5                      00062000
         OI    WRDSTAT2,CONNEXT         SET CONNECT-TO-NEXT-NAME FLAG   00062100
         B     C0030                    FIND END OF NEXT LINE           00062200
C0075    EQU   *                                                        00062300
         SH    R5,H24                   BACK UP ONE WORD                00062400
         TM    WLINEND,ENDOFLIN         IS THIS END OF LINE             00062500
         BO    C0080                    YES - SET NEW END               00062600
         AP    WKLSTLNE,P1              ADD 1 TO LAST LINE NUMBER       00062700
C0080    EQU   *                                                        00062800
         OI    WLINEND,ENDOFNAM+ENDOFLIN  SET NEW END-OF-NAMES/LINE     00062900
         EJECT                                                          00063000
*********************************************************************** 00063100
*        REMOVE DANGLING CONNECTORS OR SALUTATIONS                    * 00063200
*        (CONNECTORS OR SALUTATIONS AT THE END THAT DONT CONNECT      * 00063300
*        TO ANYTHING                                                  * 00063400
*        R5 NOW POINTS TO NEW END-OF-NAMES WORD                       * 00063500
*********************************************************************** 00063600
C0090    EQU   *                                                        00063700
         TM    WORDSTAT,X'FF'-COMMWD    IS THIS NOT A SPECIAL WORD      00063800
         BZ    C0150                    YES - END OF THIS ROUTINE       00063900
         TM    WORDSTAT,CONNWD+SALUTWD  IS THIS A CONNECTOR OR SALUT    00064000
         BNZ   C0100                    YES - SET PREV WORD AS END      00064100
         SH    R5,H24                   BACK UP ONE WORD                00064200
         B     C0090                    CHECK NEXT WORD                 00064300
C0100    EQU   *                                                        00064400
         SH    R5,H24                   BACK UP ONE WORD                00064500
         OI    WLINEND,ENDOFNAM+ENDOFLIN   SET NEW END OF NAMES         00064600
         B     C0090                    CHECK NEXT WORD                 00064700
         SPACE 3                                                        00064800
*********************************************************************** 00064900
*        REMOVE MULTIPLE CONSECUTIVE CONNECTORS                       * 00065000
*********************************************************************** 00065100
C0150    EQU   *                                                        00065200
         LA    R5,SAVEWORD              GET ADDR OF SAVEWORD AREA       00065300
         MVI   CONNFND,X'00'            RESET CONNECTOR FOUND SWITCH    00065400
C0160    EQU   *                                                        00065500
         TM    WORDSTAT,CONNWD          IS THIS A CONNECTOR             00065600
         BO    C0180                    YES - CHK CONN SWITCH           00065700
         TM    WORDSTAT,TITLEWD         IS THIS WORD A TITLE            00065800
         BO    C0290                    YES - CHK PREV WORD             00065900
         TM    WORDSTAT,X'FF'-COMMWD    IS THIS A PERSONAL/COMM WORD    00066000
         BNZ   C0170                    NO - CHK END OF NAMES           00066100
         NI    CONNFND,X'7F'            RESET CONNECTOR FOUND SWITCH    00066200
C0170    EQU   *                                                        00066300
         TM    WLINEND,ENDOFNAM         IS THIS THE END-OF-NAMES        00066400
         BO    C0300                    YES - END OF THIS               00066500
         OI    CONNFND,X'40'            SET WORD FOUND SWITCH           00066600
         LA    R5,24(0,R5)              BUMP TO NEXT WORD               00066700
         B     C0160                    GO CHK FOR CONNECTOR            00066800
C0180    EQU   *                                                        00066900
         TM    CONNFND,X'40'            WAS A WORD ALREADY FOUND        00067000
         BZ    C0250                    NO - ERASE CONNECTOR            00067100
         TM    CONNFND,X'80'            WAS A CONNECTOR ALREADY FOUND   00067200
         BO    C0250                    YES - ERASE CONNECTOR           00067300
         OI    CONNFND,X'80'            SET CONNECTOR FOUND SWITCH      00067400
         LR    R15,R5                   SAVE POINTER TO WORD LIST       00067500
C0190    EQU   *                                                        00067600
         SH    R5,H24                   BACK UP ONE WORD                00067700
         TM    WORDSTAT,SALUTWD         IS IT A SALUTATION WORD         00067800
         BO    C0200                    YES                             00067900
         LR    R5,R15                   NO - POINT BACK TO CONNECTOR    00068000
         B     C0170                    GO CHK FOR END                  00068100
C0200    EQU   *                                                        00068200
         LA    R5,48(0,R5)              BUMP UP TO WORD AFTER CONN      00068300
C0210    EQU   *                                                        00068400
         TM    WORDSTAT,CONNWD          IS THIS A CONNECTOR             00068500
         BZ    C0220                    NO - CHK FOR SALUTATION         00068600
         LA    R5,24(0,R5)              BUMP TO NEXT WORD               00068700
         B     C0210                    GO CHK FOR CONNECTOR            00068800
C0220    EQU   *                                                        00068900
         TM    WORDSTAT,SALUTWD         IS THIS A SALUTATION WORD       00069000
         LR    R5,R15                   POINT BACK TO CONNECTOR         00069100
         BZ    C0170                    NO - GO CHK FOR END             00069200
         OI    WORDSTAT,SALUTWD+SALCONWD FLAG WORD AS SALUTATION CONN   00069300
         NI    WORDSTAT,X'FF'-CONNWD    RESET CONNECTOR FLAG            00069400
         B     C0170                    GO CHK FOR END                  00069500
C0250    EQU   *                                                        00069600
         LR    R15,R5                   SAVE POINTER TO WORD LIST       00069700
C0260    EQU   *                                                        00069800
         TM    WORDSTAT,CONNONLY        IS THIS A CONN ONLY             00069900
         BZ    C0280                    YES - MAKE IT A GARBAGE WORD    00070000
C0270    EQU   *                                                        00070100
         SH    R5,H24                   BACK UP ONE WORD                00070200
         TM    WORDSTAT,CONNWD+SALCONWD IS THIS A CONNECTOR             00070300
         BZ    C0270                    NO - BACK UP AGAIN              00070400
         TM    WORDSTAT,CONNONLY        IS THIS A CONN ONLY             00070500
         BZ    C0280                    YES - MAKE IT A GARBAGE WORD    00070600
         LR    R5,R15                   POINT TO ORIGINAL CONNECTOR     00070700
         OI    WORDSTAT,GARBWD          MAKE CONNECTOR A GARBAGE WORD   00070800
         B     C0170                    GO CHK FOR END                  00070900
C0280    EQU   *                                                        00071000
         OI    WORDSTAT,GARBWD          MAKE CONNECTOR A GARBAGE WORD   00071100
         LR    R5,R15                   POINT BACK TO CURRENT WORD      00071200
         B     C0170                    GO CHK FOR END                  00071300
         SPACE 3                                                        00071400
*********************************************************************** 00071500
*        CHECK FOR A TITLE WORD AS PART OF SALUTATION                 * 00071600
*********************************************************************** 00071700
C0290    EQU   *                                                        00071800
         SH    R5,H24                   BACK UP ONE WORD                00071900
         TM    WORDSTAT,SALUTWD         IS THIS WORD A SALUTATION       00072000
         LA    R5,24(0,R5)              BUMP UP TO CURRENT WORD         00072100
         BZ    C0170                    IF NOT SALUT, TRY NEXT WORD     00072200
         OI    WORDSTAT,SALUTWD         MAKE WORD A SALUTATION          00072300
         NI    WORDSTAT,X'FF'-TITLEWD   CLEAR TITLE WORD FLAG           00072400
         B     C0170                                                    00072500
         EJECT                                                          00072600
*********************************************************************** 00072700
*        CHECK FOR A COMMERCIAL NAME                                  * 00072800
*        ALL WORDS IN A LINE CONTAINING A COMMERCIAL KEYWORD ARE      * 00072900
*        CONSIDERED TO BE PART OF THE COMMERCIAL NAME.  HOWEVER,      * 00073000
*        IF A CONNECTOR IS THE FIRST WORD OR THE LAST WORD OF A LINE, * 00073100
*        IT IS NOT CONSIDERED PART OF THE COMMERCIAL NAME.            * 00073200
*********************************************************************** 00073300
C0300    EQU   *                                                        00073400
         CLI   COMMIND,X'FF'       DID WE FIND ANY COMMERCIAL WORDS     00073500
         BNE   C0400               NO                                   00073600
         LA    R5,SAVEWORD         GET ADDR OF SAVEWORD AREA            00073700
C0310    EQU   *                   FIND COMMERCIAL WORD                 00073800
         TM    WORDSTAT,COMMWD     IS THIS A COMMERCIAL WORD            00073900
         BO    C0330               YES                                  00074000
C0320    EQU   *                                                        00074100
         TM    WLINEND,ENDOFNAM    IS THIS END-OF-NAMES                 00074200
         BO    C0400               YES - CHK FOR MULTIPLE WORD LAST NM  00074300
         LA    R5,24(0,R5)         BUMP TO NEXT WORD                    00074400
         B     C0310                                                    00074500
C0330    EQU   *                   BACK UP TO BEGINNING OF LINE         00074600
         TM    WLINEND,BEGOFLIN    IS THIS THE BEGINNING OF THE LINE    00074700
         BO    C0340               YES                                  00074800
         SH    R5,H24              BACK UP ONE WORD                     00074900
         B     C0330               CHECK AGAIN                          00075000
C0340    EQU   *                   PROCESS FIRST WORD OF A LINE         00075100
         TM    WORDSTAT,CONNWD     IS THIS WORD A CONNECTOR             00075200
         BZ    C0350               NO - FLAG AS COMMERCIAL              00075300
         TM    WLINEND,ENDOFLIN    IS THIS THE END OF A LINE            00075400
         BO    C0320               YES - FIND NEXT COMMERCIAL WORD      00075500
         LA    R5,24(0,R5)         BUMP OVER CONNECTOR                  00075600
C0350    EQU   *                                                        00075700
         TM    WLINEND,ENDOFLIN    IS THIS THE END OF THE LINE          00075800
         BO    C0360               YES - CHK FOR CONNECTOR              00075900
         MVI   WORDSTAT,COMMWD     FLAG WORD AS COMMERCIAL              00076000
         LA    R5,24(0,R5)         BUMP TO NEXT WORD                    00076100
         B     C0350                                                    00076200
C0360    EQU   *                   PROCESS LAST WORD OF A LINE          00076300
         TM    WORDSTAT,CONNWD     IS THIS A CONNECTOR                  00076400
         BO    C0320               YES - CHK FOR END OF NAMES           00076500
         MVI   WORDSTAT,COMMWD     FLAG WORD AS COMMERCIAL              00076600
         B     C0320               CHK FOR END OF NAMES                 00076700
         SPACE 3                                                        00076800
*********************************************************************** 00076900
*        CONDENSE MULTIPLE WORD LAST NAMES                            * 00077000
*********************************************************************** 00077100
C0400    EQU   *                                                        00077200
         CLI   LNMIND,X'FF'             DID I FIND ANY MULTIWORD NAMES  00077300
         BNE   D0010                    NO                              00077400
         LA    R5,SAVEWORD              GET ADDR OF SAVEWORD AREA       00077500
C0410    EQU   *                                                        00077600
         LR    R7,R5                    MAKE R7 = R5                    00077700
         TM    WORDSTAT,LNMWD           IS THIS A MULTI-LAST-NAME WORD  00077800
         BO    C0420                    YES                             00077900
         TM    WLINEND,ENDOFNAM         IS THIS THE END-OF-NAMES        00078000
         BO    D0010                    YES - END OF THIS               00078100
         LA    R5,24(0,R5)              BUMP TO NEXT WORD               00078200
         B     C0410                    GO CHK FOR MULTI LAST NAME      00078300
C0420    EQU   *                                                        00078400
         LA    R7,24(0,R7)              POINT TO NEXT WORD              00078500
         USING NXWDAREA,R7                                              00078600
         TM    NXWDSTAT,SPCLWORD        IS THIS ANY SPECIAL WORD        00078700
         BZ    C0430                    NO - USE THIS WORD              00078800
         TM    NXLINEND,ENDOFNAM        IS THIS THE END-OF-NAMES        00078900
         BO    D0010                    YES - END OF THIS               00079000
         B     C0420                    BUMP TO NEXT WORD               00079100
C0430    EQU   *                                                        00079200
*        COMBINE WORDS POINTED TO BY R5 AND R7 INTO ONE WORD            00079300
*        MAKE WORD AT R7 A GARBAGE WORD SO IT WILL BE DISCARDED         00079400
         SR    R0,R0                                                    00079500
         SR    R2,R2                                                    00079600
         IC    R2,WORDLNG               GET LENGTH OF FIRST WORD        00079700
         LA    R1,L'SAVEWORD-1          SET MAX WORD LNG                00079800
         SR    R1,R2                    CALC LNG REMAINING IF FIRST WD  00079900
         CLM   R1,B'0001',NXWDLNG       WILL SECOND WORD FIT            00080000
         BNL   C0440                    YES                             00080100
         STC   R1,NXWDLNG               FORCE SECOND WORD LNG TO FIT    00080200
C0440    EQU   *                                                        00080300
         IC    R1,NXWDLNG               GET LENGTH OF SECOND WORD       00080400
         LA    R6,WORDSAVE+1(R2)        OFFSET INTO FIRST WORD          00080500
         EX    R1,MV2NDWD               MOVE SECOND WORD INTO FIRST     00080600
         LA    R2,1(R1,R2)              CALC NEW WORD LNG               00080700
         STC   R2,WORDLNG               SAVE NEW WORD LNG               00080800
         MVC   WORDSTAT,NXWDSTAT        PROPOGATE STATUS FLAGS          00080900
         OI    NXWDSTAT,GARBWD          CHANGE SECOND WORD TO GARBAGE   00081000
         NI    NXWDSTAT,X'FF'-LNMWD     TURN OFF MULTI-WORD FLAG        00081100
         B     C0410                    GO BACK AND CHK AGAIN           00081200
MV2NDWD  MVC   0(0,R6),NXWDSAVE                                         00081300
         EJECT                                                          00081400
*********************************************************************** 00081500
*        CLEAR STANDARD NAME BUILD AREA                               * 00081600
*********************************************************************** 00081700
D0010    EQU   *                                                        00081800
         LA    R6,NAMETBL               GET NAME POINTER AREA ADDR      00081900
         LA    R7,WKNAMES               GET NAME RETURN AREA ADDR       00082000
         USING WKNMAREA,R7                                              00082100
         LA    R2,NONMRET               SET LOOP COUNT                  00082200
D0020    EQU   *                                                        00082300
         MVI   WKNMLINE,X'0C'           RESET INPUT LINE NUMBER         00082400
         MVC   WKNMERFL,ZEROS           RESET ERROR FLAGS               00082500
*        THE REST OF THE WKNMAREA WAS CLEARED IN A0010                  00082600
         ST    R7,NMWKNMAD              SAVE ADDR OF WORK NAME AREA     00082700
         LA    R8,WKNMSAL               POINT TO SALUTATION AREA        00082800
         ST    R8,SLNXWDAD              SET SALUTATION NEXT WORD ADDR   00082900
         LA    R8,WKNMNAME              POINT TO NAME AREA              00083000
         ST    R8,NMNXWDAD              SET NEXT WORD ADDR              00083100
         ST    R8,NMLSWDAD              SET LAST WORD ADDR              00083200
         MVI   SLREMLNG,X'0F'           SET MAX. LNG OF SALUT AREA      00083300
         MVI   TLREMLNG,X'0F'           SET MAX. LNG OF TITLE AREA      00083400
         LA    R8,WKNMTTL               POINT TO TITLE AREA             00083500
         ST    R8,TLNXWDAD              SET TITLE NEXT WORD ADDR        00083600
         LA    R6,NMARLNG(0,R6)         BUMP TO NEXT NAME POINTER AREA  00083700
         LA    R7,WKNMARLN(0,R7)        BUMP TO NEXT NAME AREA          00083800
         BCT   R2,D0020                 LOOP BACK                       00083900
         SPACE 3                                                        00084000
*********************************************************************** 00084100
*        BUILD NAMES                                                  * 00084200
*********************************************************************** 00084300
D0030    EQU   *                                                        00084400
         SR    R1,R1                    CLEAR R1                        00084500
         LA    R5,SAVEWORD              GET ADDR OF SAVEWORD AREA       00084600
         LA    R6,NAMETBL               GET ADDR OF NAME POINTER AREA   00084700
         L     R7,NMWKNMAD              GET ADDR OF NAME RETURN AREA    00084800
         LA    R8,NMWDADTB              GET ADDR OF NAME WORD STACK     00084900
         MVI   LINECTR,X'1C'            SET TO FIRST LINE               00085000
         MVI   WKNMLINE,X'1C'           SET TO FIRST LINE               00085100
         MVI   LSTNMFST,X'00'           RESET LAST-NAME-FIRST FLAG      00085200
         MVI   FIRSTWRD,X'FF'           SET FIRST-WORD-IN-LINE FLAG     00085300
D0040    EQU   *                                                        00085400
         TM    WORDSTAT,SPCLWORD        ANY SPECIAL WORD                00085500
         BNZ   D0080                    YES                             00085600
         CLI   FIRSTWRD,X'FF'           IS THIS FIRST WORD IN LINE      00085700
         BNE   D0050                    NO - CANT BE A LAST NAME        00085800
         MVI   FIRSTWRD,X'00'           RESET FIRST-WORD-IN-LINE FLAG   00085900
         TM    WRDSTAT2,COMINWD         WAS THERE A COMMA IN THIS WORD  00086000
         BO    D0070                    YES - PROCESS AS A LAST NAME    00086100
         CLI   WORDLNG,X'00'            IS FIRST WORD ONE CHARACTER     00086200
         BNE   D0050                    NO                              00086300
         OI    NMNMFLGS,NMLSNMMV        SET NEED-LAST-NAME FLAG         00086400
D0050    EQU   *                                                        00086500
         ZAP   WKNMLINE,LINECTR         MOVE LINE COUNTER               00086600
         CLI   NMNOWD,X'01'             DO I ALREADY HAVE ONE WORD      00086700
         BNE   *+8                      NO - SKIP RESET                 00086800
         NI    NMNMFLGS,X'FF'-NMLSNMMV  RESET NEED-LAST-NAME FLAG       00086900
         L     R9,NMNXWDAD              GET ADDR OF NEXT WORD ADDR      00087000
         IC    R1,WORDLNG               GET LENGTH OF WORD              00087100
         EX    R1,MOVEWRD               MOVE WORD TO RETURN AREA        00087200
         ST    R9,NMLSWDAD              SAVE LAST WORD ADDR             00087300
         STC   R1,NMLSWDLN              SAVE LAST WORD LENGTH           00087400
         OC    NMWDFLGS,WORDSTAT        OR IN CURRENT WORD FLAGS        00087500
         LA    R9,2(R1,R9)              BUMP TO NEXT OUTPUT POSITION    00087600
         ST    R9,NMNXWDAD              SAVE NAME NEXT WORD ADDR        00087700
         TR    NMNOWD,ADDONE            ADD 1 TO NO. WORDS IN NAME      00087800
         CLI   NMNOWD,X'05'             MORE THAN 5 WORDS IN NAME       00087900
         BH    D0055                    YES - DONT SAVE ADDR            00088000
         IC    R1,NMNOWD                GET NO. WORDS                   00088100
         SLL   R1,2                     MULTIPLY BY 4                   00088200
         LA    R8,NMWDADTB-4(R1)        POINT TO CORRECT WORD SLOT      00088300
         ST    R5,0(0,R8)               STORE ADDR OF WORD IN STACK     00088400
D0055    EQU   *                                                        00088500
         TM    WLINEND,ENDOFNAM         IS THIS THE END OF NAMES        00088600
         BO    D0130                    YES - END OF BUILDING NAMES     00088700
         TM    WLINEND,ENDOFLIN         IS THIS THE END OF A LINE       00088800
         BZ    D0065                    NO - GET NEXT WORD              00088900
D0060    EQU   *                                                        00089000
         AP    LINECTR,P1               ADD ONE TO LINE COUNTER         00089100
         TM    WRDSTAT2,CONNEXT         TREAT WORD AS CONNECTOR TOO     00089200
         BZ    D0065                    NO - GET NEXT WORD              00089300
         LR    R0,R6                    SAVE R6                         00089400
         LA    R6,NMARLNG(0,R6)         GET ADDR OF NEXT NAME TBL AREA  00089500
         C     R6,NMTBLEND              END OF NAME TABLE               00089600
         BNL   D0119                    YES - THATS ALL OF THIS         00089700
         AP    WKNONAME,P1              ADD ONE TO NUMBER OF NAMES      00089800
         L     R7,NMWKNMAD              GET ADDR OF NEXT NAME RET AREA  00089900
         LA    R8,NMWDADTB              GET ADDR OF NAME WORD STACK     00090000
         MVI   FIRSTWRD,X'FF'           SET FIRST-WORD-IN-LINE FLAG     00090100
D0065    EQU   *                                                        00090200
         LA    R5,24(0,R5)              BUMP TO NEXT WORD IN SAVEAREA   00090300
         B     D0040                    PROCESS NEXT WORD               00090400
MOVEWRD  MVC   0(0,R9),WORDSAVE         MVC TO MOVE WORD TO RET AREA    00090500
*********************************************************************** 00090600
*        MOVE WORD AS LAST NAME                                       * 00090700
*********************************************************************** 00090800
D0070    EQU   *                                                        00090900
         MVI   LSTNMFST,X'FF'           SET LAST-NAME-FIRST FLAG        00091000
         OI    NMNMFLGS,NMLSNMFS        SET LAST-NAME-FIRST FLAG        00091100
         IC    R1,WORDLNG               GET LENGTH OF WORD              00091200
         LA    R9,WKNMNAME+L'WKNMNAME-1 POINT TO LAST BYTE OF NAME AREA 00091300
         SR    R9,R1                    BACK UP                         00091400
         EX    R1,MOVEWRD               MOVE LAST NAME                  00091500
         ST    R9,NMLSNMAD              SAVE ADDR OF LAST NAME          00091600
         ST    R5,NMLSNMPT              SAVE ADDR OF LAST NAME IN TABLE 00091700
         STC   R1,NMLSNMLN              SAVE LENGTH OF LAST NAME        00091800
         OC    NMWDFLGS,WORDSTAT        OR IN CURRENT WORD FLAGS        00091900
         B     D0120                                                    00092000
*********************************************************************** 00092100
*        CHECK SPECIAL WORDS (GARBAGE, SALUTATION, TITLE, CONNECTOR)  * 00092200
*********************************************************************** 00092300
D0080    EQU   *                                                        00092400
         TM    WORDSTAT,GARBWD          IS THIS A GARBAGE WORD          00092500
         BO    D0120                    YES - SKIP IT                   00092600
         TM    WORDSTAT,CONNWD          IS THIS A CONNECTOR             00092700
         BO    D0110                    YES                             00092800
         TM    WORDSTAT,SALUTWD         IS THIS A SALUTATION            00092900
         BO    D0090                    YES                             00093000
         TM    WORDSTAT,TITLEWD         IS THIS A TITLE WORD            00093100
         BO    D0100                    YES                             00093200
         TM    WORDSTAT,COMMWD          IS THIS A COMMERCIAL KEY WORD   00093300
         BZ    D0120                    NO - SKIP IT                    00093400
         MVI   COMMFLAG,X'FF'           SET COMMERCIAL NAME FLAG        00093500
         B     D0050                    GO MOVE IN WORD                 00093600
*********************************************************************** 00093700
*        CHECK SALUTATION WORDS                                       * 00093800
*********************************************************************** 00093900
D0090    EQU   *                                                        00094000
         CLI   WKRETSAL,C'0'            DO I RETURN SALUTATIONS         00094100
         BE    D0120                    NO - SKIP THIS WORD             00094200
         SR    R0,R0                    CLEAR R0                        00094300
         ICM   R0,B'0001',SLREMLNG      GET REMAINING LENGTH            00094400
         BZ    D0120                    BR IF NO AVAILABLE AREA         00094500
         IC    R1,WORDLNG               GET LENGTH OF WORD              00094600
         CR    R1,R0                    WILL WORD FIT IN AREA           00094700
         BNH   *+8                      YES                             00094800
         IC    R1,SLREMLNG              FORCE LENGTH TO AVAILABLE AREA  00094900
         L     R9,SLNXWDAD              GET SALUTATION NEXT WORD ADDR   00095000
         EX    R1,MOVEWRD               MOVE WORD TO RETURN AREA        00095100
         LR    R14,R9                   SAVE R9                         00095200
         LA    R9,2(R1,R9)              BUMP TO NEXT OUTPUT POSITION    00095300
         ST    R9,SLNXWDAD              SAVE SALUT NEXT WORD ADDR       00095400
         SR    R0,R1                    SUBTRACT NEW WORD LNG           00095500
         BCTR  R0,0                     SUBTRACT ONE MORE               00095600
         STC   R0,SLREMLNG              SAVE REMAINING LENGTH           00095700
         TR    SLNOWD,ADDONE            ADD ONE TO NO. WORDS IN SAL     00095800
         CLI   SLNOWD,X'03'             MORE THAN THREE WORDS SAVED     00095900
         BH    D0099                    YES                             00096000
         IC    R1,SLNOWD                GET NO. WORDS                   00096100
         SLL   R1,2                     MULTIPLY BY 4                   00096200
         LA    R15,NMSLADTB-4(R1)       POINT TO CORRECT WORD SLOT      00096300
         ST    R14,0(0,R15)             SAVE WORD ADDR                  00096400
D0099    EQU   *                                                        00096500
         TM    WORDSTAT,SALCONWD        IS WORD ALSO A CONNECTOR        00096600
         BZ    D0120                    NO                              00096700
         OI    NMNMFLGS,NMCONSAL        SET CONN-IN-SALUT FLAG          00096800
         B     D0120                                                    00096900
*********************************************************************** 00097000
*        CHECK TITLE WORDS                                            * 00097100
*********************************************************************** 00097200
D0100    EQU   *                                                        00097300
         CLI   WKRETTTL,C'1'            DO I RETURN ALL TITLES          00097400
         BE    D0105                    YES                             00097500
         CLI   WKRETTTL,C'2'            DO I RETURN CIF TITLES ONLY     00097600
         BNE   D0107                    NO - SKIP TITLE WD    9910768   00097700
         LA    R9,CIFTITLE              POINT TO CIF TITLE TABLE        00097800
D0101    EQU   *                                                        00097900
         CLC   WORDSAVE(4),0(R9)        DO I WANT THIS TITLE            00098000
         BE    D0105                    YES                             00098100
         LA    R9,4(0,R9)               BUMP TO NEXT TITLE WORD         00098200
         CLI   0(R9),X'FF'              AT END OF TABLE                 00098300
         BNE   D0101                    NO - CHK NEXT TITLE             00098400
         B     D0107                    SKIP THIS TITLE WORD  9910768   00098500
D0105    EQU   *                                                        00098600
         SR    R0,R0                    CLEAR R0                        00098700
         ICM   R0,B'0001',TLREMLNG      GET REMAINING LENGTH            00098800
         BZ    D0107                    BR IF NO AVAIL AREA   9910768   00098900
         IC    R1,WORDLNG               GET LENGTH OF WORD              00099000
         CR    R1,R0                    WILL WORD FIT IN AREA           00099100
         BNH   *+8                      YES                             00099200
         IC    R1,TLREMLNG              FORCE LENGTH TO AVAILABLE AREA  00099300
         L     R9,TLNXWDAD              GET SALUTATION NEXT WORD ADDR   00099400
         EX    R1,MOVEWRD               MOVE WORD TO RETURN AREA        00099500
         LA    R9,2(R1,R9)              BUMP TO NEXT OUTPUT POSITION    00099600
         ST    R9,TLNXWDAD              SAVE SALUT NEXT WORD ADDR       00099700
         SR    R0,R1                    SUBTRACT NEW WORD LNG           00099800
         BCTR  R0,0                     SUBTRACT ONE MORE               00099900
         STC   R0,TLREMLNG              SAVE REMAINING LENGTH           00100000
D0107    EQU   *                                              9910768   00100100
         TM    WLINEND,ENDOFNAM         IS THIS THE END OF NAM9910768   00100110
         BO    D0120                    YES                   9910768   00100120
         TM    WLINEND,ENDOFLIN         IS THIS THE END LINE  9910768   00100130
         BZ    D0120                    YES                   9910768   00100140
*********************************************************************** 00100200
*        CHECK CONNECTOR WORDS                                        * 00100300
*********************************************************************** 00100400
D0110    EQU   *                                                        00100500
         LR    R0,R6                    SAVE R6                         00100600
         LA    R6,NMARLNG(0,R6)         GET ADDR OF NEXT NAME TBL AREA  00100700
         C     R6,NMTBLEND              END OF NAME TABLE               00100800
         BNL   D0119                    YES - THATS ALL OF THIS         00100900
         AP    WKNONAME,P1              ADD ONE TO NUMBER OF NAMES      00101000
         L     R7,NMWKNMAD              GET ADDR OF NEXT NAME RET AREA  00101100
         LA    R8,NMWDADTB              GET ADDR OF NAME WORD STACK     00101200
         MVI   FIRSTWRD,X'FF'           SET FIRST-WORD-IN-LINE FLAG     00101300
         TM    WORDSTAT,TITLEWD         IS THIS A TITLE WORD? 9910768   00101310
         BO    D0120                    YES                   9910768   00101320
         TM    WORDSTAT,SALUTWD         ALSO A SALUTATION WORD          00101400
         BO    D0090                    YES - GO DO SALUTATION          00101500
         TM    WORDSTAT,COMMWD          ALSO A COMMERCIAL KEY WORD      00101600
         BZ    D0120                    NO - SKIP IT                    00101700
*        IF A CONNECTOR IS ALSO A COMMERCIAL KEY WORD, FLAG THE NEXT    00101800
*        NAME AS A COMMERCIAL NAME (E.G. JOHN DOE  DBA  DOE FLORISTS)   00101900
         MVI   COMMFLAG,X'FF'           SET COMMERCIAL NAME FLAG        00102000
         OI    NMWDFLGS,COMMWD          SET COMMERCIAL NAME FLAG        00102100
         B     D0120                    SKIP CONNECTOR                  00102200
D0119    EQU   *                                                        00102300
         LR    R6,R0                    RELOAD R6                       00102400
         B     D0130                                                    00102500
*********************************************************************** 00102600
*        CHK FOR END OF NAMES                                         * 00102700
*********************************************************************** 00102800
D0120    EQU   *                                                        00102900
         TM    WLINEND,ENDOFNAM         IS THIS THE END OF NAMES        00103000
         BO    D0130                    YES                             00103100
         TM    WLINEND,ENDOFLIN         IS THIS THE END OF A LINE       00103200
         BZ    *+10                     NO                              00103300
         AP    LINECTR,P1               ADD ONE TO LINE COUNTER         00103400
         LA    R5,24(0,R5)              BUMP TO NEXT WORD IN SAVEAREA   00103500
         B     D0040                    PROCESS NEXT WORD               00103600
*********************************************************************** 00103700
*        END OF NAME BUILD                                            * 00103800
*********************************************************************** 00103900
D0130    EQU   *                                                        00104000
         AP    WKNONAME,P1              ADD ONE TO NUMBER OF NAMES      00104100
         OI    NMNMFLGS,NMEND           SET END OF NAME FLAG            00104200
         CLI   NMNOWD,X'05'             MORE THAN 5 WORDS     1015785   00104210
         BNH   D0135                    NO                    1015785   00104220
         MVI   WKRETCOD+1,X'10'         SET RETURN CODE TO 16 1015785   00104230
         MVI   WKERRFLG,C'1' SET FIRST ERROR FLAG - NO INPUT  1015785   00104240
         B     Z0010                    EXIT                  1015785   00104250
D0135    EQU   *                                              1015785   00104270
         CLI   LSTNMFST,X'FF'           ANY LAST-NAME-FIRST             00104300
         BNE   D0200                    NO                              00104400
*********************************************************************** 00104500
*        SHIFT LAST NAMES NEXT TO OTHER WORDS IN NAME                 * 00104600
*********************************************************************** 00104700
         LA    R6,NAMETBL               GET NAME POINTER AREA ADDR      00104800
D0140    EQU   *                                                        00104900
         TM    NMNMFLGS,NMLSNMFS        IS THIS NAME LAST NAME FIRST    00105000
         BZ    D0150                    NO - CHK NEXT NAME              00105100
         IC    R1,NMLSNMLN              GET LENGTH OF LAST NAME         00105200
         L     R7,NMNXWDAD              GET ADDR OF NEXT FREE BYTE      00105300
         L     R8,NMLSNMAD              GET ADDR OF LAST NAME WORD      00105400
         EX    R1,SVLSNM                SAVE LAST NAME WORD             00105500
         EX    R1,CLRLSNM               CLEAR LAST NAME WORD            00105600
         EX    R1,RSTLSNM               MOVE LAST NAME WORD BACK IN     00105700
         ST    R7,NMLSWDAD              SAVE LAST WORD ADDR             00105800
         ST    R7,NMLSNMAD              SAVE LAST NAME ADDR             00105900
         STC   R1,NMLSWDLN              SAVE LAST WORD LENGTH           00106000
         LA    R7,2(R1,R7)              BUMP TO NEXT OUTPUT POSITION    00106100
         ST    R7,NMNXWDAD              SAVE NAME NEXT WORD ADDR        00106200
         TR    NMNOWD,ADDONE            ADD 1 TO NO. WORDS IN NAME      00106300
         CLI   NMNOWD,X'05'             MORE THAN 5 WORDS IN NAME       00106400
         BH    D0150                    YES - DONT SAVE ADDR            00106500
         IC    R1,NMNOWD                GET NO. WORDS                   00106600
         SLL   R1,2                     MULTIPLY BY 4                   00106700
         LA    R7,NMWDADTB-4(R1)        POINT TO CORRECT WORD SLOT      00106800
         MVC   0(4,R7),NMLSNMPT         STORE ADDR OF WORD IN STACK     00106900
D0150    EQU   *                                                        00107000
         TM    NMNMFLGS,NMEND           END OF NAMES                    00107100
         BO    D0200                    YES                             00107200
         LA    R6,NMARLNG(0,R6)         BUMP TO NEXT NAME AREA          00107300
         B     D0140                    GO DO NEXT NAME                 00107400
SVLSNM   MVC   WORDWORK(0),0(R8)        MVC TO SAVE LAST NAME           00107500
CLRLSNM  MVC   0(0,R8),SPACES           MVC TO CLEAR LAST NAME          00107600
RSTLSNM  MVC   0(0,R7),WORDWORK         MVC TO MOVE LAST NAME BACK      00107700
         EJECT                                                          00107800
*********************************************************************** 00107900
*        CHECK FOR ERROR CONDITIONS                                   * 00108000
*********************************************************************** 00108100
D0200    EQU   *                                                        00108200
         LA    R6,NAMETBL               GET NAME POINTER AREA ADDR      00108300
D0210    EQU   *                                                        00108400
         L     R7,NMWKNMAD              GET ADDR OF NAME RETURN AREA    00108500
         CLI   WKFORCE,C'P'             WAS PERSONAL NAME FORCED        00108600
         BE    D0220                    YES                             00108700
         TM    NMWDFLGS,COMMWD          WAS ANY WORD A COMMERCIAL WORD  00108800
         BO    D0600                    YES - GENERATE COMMERCIAL KEY   00108900
D0220    EQU   *                                                        00109000
         LR    R2,R6                    POINT TO CURRENT NAME POINTER   00109100
         USING NEXTPTAR,R2              SET NEXT NAME AREA DSECT        00109200
         BAS   R9,D0300                 FIND ANOTHER NAME?    2024183   00109300
         CLI   NMNOWD,X'01'             DOES THIS NAME HAVE ONE WORD    00109400
         BNE   D0230                    NO                              00109500
         TM    NEXTNMSW,X'80'           IS THERE ANOTHER NAME           00109600
         BZ    D0600                    NO - GEN COMMERCIAL KEY         00109700
D0230    EQU   *                                                        00109800
         CLI   NMNOWD,X'03'             MORE THAN THREE WORDS IN NAME   00109900
         BNH   D0240                    NO - CHK LAST NAMES             00110000
*        SET ERROR CODE - MORE THAN THREE WORDS IN NAME                 00110100
         LA    R15,3                    SET ERROR CODE                  00110200
         B     D0350                    SET CODE AND GEN PERSONAL KEY   00110300
D0240    EQU   *                                                        00110400
         CLI   NMLSWDLN,X'00'           IS LAST WORD ONE LETTER         00110500
         BE    D0260                    YES - CHK LAST NAMES            00110600
         CLI   NMNOWD,X'02'             TWO WORDS IN THE NAME           00110700
         BH    D0500                    THREE - GEN PERSONAL KEY        00110800
         BL    D0260                    LESS - CHK LAST NAMES           00110900
*        NOW I HAVE A TWO-WORD NAME                                     00111000
         TM    NMNMFLGS,NMLSNMMV        DO I NEED TO GET A LAST NAME    00111100
         BO    D0260                    YES                             00111200
         TM    NEXTNMSW,X'80'           IS THERE ANOTHER NAME           00111300
         BZ    D0500                    NO - GEN PERSONAL KEY           00111400
         TM    NEXTNMSW,X'40'           IS LAST NAME SAME               00111500
         BO    D0500                    YES - GEN PERSONAL KEY          00111600
*        SET ERROR CODE - LAST NAME CANNOT BE DETERMINED                00111700
         LA    R15,1                    SET ERROR CODE                  00111800
         B     D0350                    SET CODE AND GEN PERSONAL KEY   00111900
*********************************************************************** 00112000
*        FIND NEXT/PREVIOUS NAME WHICH HAS A LAST NAME WORD           * 00112100
*********************************************************************** 00112200
D0250    EQU   *                                                        00112300
         BAS   R9,D0300                 FIND ANOTHER NAME?    2024183   00112400
D0260    EQU   *                                                        00112500
         TM    NEXTNMSW,X'80'           IS THERE ANOTHER NAME           00112600
         BO    D0270                    YES                             00112700
         CLI   LSTNMFST,X'FF'           LAST-NAME-FIRST                 00112800
         BE    D0500                    YES - GEN PERSONAL KEY          00112900
*        SET ERROR CODE - LAST NAME NOT FOUND                           00113000
         LA    R15,2                    SET ERROR CODE                  00113100
         B     D0350                    SET CODE AND GEN PERSONAL KEY   00113200
D0270    EQU   *                                                        00113300
         CLI   NXLSWDLN,X'00'           IS NEXT LAST WORD ONE CHAR      00113400
         BE    D0250                    YES - FIND NEXT NAME            00113500
         TM    NXNMFLGS,NMLSNMMV        DOES IT ALSO NEED A LAST NAME   00113600
         BO    D0250                    YES - FIND NEXT NAME            00113700
         TM    NEXTNMSW,X'40'           ARE LAST WORDS EQUAL            00113800
         BO    D0500                    YES - GEN PERSONAL KEY          00113900
         CLI   NXNOWD,X'01'             NEXT NAME IS ONE WORD           00114000
         BE    D0250                    YES - FIND NEXT NAME            00114100
*********************************************************************** 00114200
*        MOVE NEXT NAME LAST-NAME TO CURRENT NAME                     * 00114300
*********************************************************************** 00114400
         L     R14,NMNXWDAD             GET ADDR TO MOVE TO             00114500
         L     R15,NXLSWDAD             GET ADDR TO MOVE FROM           00114600
         SR    R1,R1                    CLEAR R1                        00114700
         IC    R1,NXLSWDLN              GET WORD LENGTH                 00114800
         EX    R1,MVLSTNM               MOVE LAST NAME                  00114900
         NI    NMNMFLGS,X'FF'-NMLSNMMV  RESET NEED-LAST-NAME FLAG       00115000
         ST    R14,NMLSWDAD             SAVE LAST WORD ADDR             00115100
         STC   R1,NMLSWDLN              SAVE LAST WORD LENGTH           00115200
         LA    R14,2(R1,R14)            BUMP TO NEXT OUTPUT POSITION    00115300
         ST    R14,NMNXWDAD             SAVE NAME NEXT WORD ADDR        00115400
         TR    NMNOWD,ADDONE            ADD 1 TO NO. WORDS IN NAME      00115500
         CLI   NMNOWD,X'05'             MORE THAN 5 WORDS IN NAME       00115600
         BH    D0280                    YES - DONT SAVE ADDR            00115700
         IC    R1,NMNOWD                GET NO. WORDS IN NAME           00115800
         SLL   R1,2                     MULTIPLY BY 4                   00115900
         LA    R14,NMWDADTB-4(R1)       POINT TO WORD ADDR IN STACK     00116000
         IC    R1,NXNOWD                GET NO. WORDS IN NAME           00116100
         SLL   R1,2                     MULTIPLY BY 4                   00116200
         LA    R15,NXWDADTB-4(R1)       POINT TO WORD ADDR IN NEXT STK  00116300
         MVC   0(4,R14),0(R15)          MOVE WORD ADDR                  00116400
D0280    EQU   *                                                        00116500
         B     D0500                    GO GENERATE PERSONAL KEY        00116600
MVLSTNM  MVC   0(0,R14),0(R15)                                          00116700
         EJECT                                                          00116800
*********************************************************************** 00116900
*        ROUTINE TO FIND NEXT/PREVIOUS NAME                           * 00117000
*********************************************************************** 00117100
D0300    EQU   *                                                        00117200
         MVI   NEXTNMSW,X'00'           RESET NEXT-NAME SWITCHES        00117300
         CLI   LSTNMFST,X'FF'           ANY LAST-NAME-FIRST             00117400
         BNE   D0310                    NO - PROCESS LAST-NAME-LAST     00117500
*        LAST NAME FIRST PROCESSING - BACK UP TO GET PREVIOUS NAME      00117600
         SH    R2,NMTBLNG               SUBTRACT LNG OF TABLE ENTRY     00117700
         C     R2,NMTBLAD               ARE WE BEFORE BEGINNING OF TBL  00117800
         BLR   R9                       YES - NO PREV NAME - RETURN     00117900
         B     D0320                    CHK FOR LAST NAMES EQUAL        00118000
D0310    EQU   *                                                        00118100
         LA    R2,NMARLNG(0,R2)         ADD LNG OF TABLE ENTRY          00118200
         C     R2,NMTBLEND              ARE WE PAST END OF TABLE        00118300
         BNLR  R9                       YES - NO NEXT NAME - RETURN     00118400
         CLI   NXNOWD,X'00'             PAST LAST ENTRY                 00118500
         BER   R9                       YES - NO NEXT NAME - RETURN     00118600
D0320    EQU   *                                                        00118700
         MVI   NEXTNMSW,X'80'           SET NEXT-NAME-FOUND SWITCH      00118800
         CLC   NMLSWDLN,NXLSWDLN        ARE LAST WORDS EQUAL LNG        00118900
         BNER  R9                       NO - RETURN                     00119000
         SR    R1,R1                    CLEAR R1                        00119100
         IC    R1,NMLSWDLN              GET LAST WORD LENGTH            00119200
         L     R14,NMLSWDAD             GET LAST WORD ADDRESS           00119300
         L     R15,NXLSWDAD             GET NEXT LAST WORD ADDRESS      00119400
         EX    R1,CHKLSTNM              CHK FOR LAST WORDS EQUAL        00119500
         BNER  R9                       NOT SAME - RETURN               00119600
         OI    NEXTNMSW,X'40'           SET LAST-WORD-EQUAL SWITCH      00119700
         BR    R9                       RETURN                          00119800
CHKLSTNM CLC   0(0,R14),0(R15)                                          00119900
         SPACE 3                                                        00120000
*********************************************************************** 00120100
*        SET ERROR FLAG, ERROR CODE, AND RET CODE                     * 00120200
*********************************************************************** 00120300
D0350    EQU   *                                                        00120400
         LA    R1,WKRTNOPT-1(R15)       POINT TO RETURN OPTION FLAGS    00120500
         CLI   0(R1),C'1'               IS FLAG ON TO RETURN THIS ERROR 00120600
         BNE   D0500                    NO - PROCESS PERSONAL NAME      00120700
         LA    R1,WKNMERFL-1(R15)       POINT TO NAME ERROR FLAGS       00120800
         MVI   0(R1),C'1'               SET ERROR CODE                  00120900
         MVI   WKNMERIN,C'E'            SET ERROR CODE                  00121000
         OI    WKRETCOD+1,X'04'         SET RETURN CODE                 00121100
         B     D0500                    GENERATE PERSONAL KEY           00121200
         EJECT                                                          00121300
*********************************************************************** 00121400
*        PROCESS PERSONAL NAMES                                       * 00121500
*********************************************************************** 00121600
D0500    EQU   *                                                        00121700
         MVI   WKNMTYPE,C'P'            SET PERSONAL CODE               00121800
*        PUT A SLASH BEFORE THE LAST NAME                               00121900
         SR    R1,R1                    CLEAR R1                        00122000
         IC    R1,NMLSWDLN              GET LENGTH OF LAST WORD         00122100
         L     R2,NMLSWDAD              GET ADDR OF LAST WORD           00122200
         EX    R1,SAVENAME              SAVE LAST WORD                  00122300
         MVI   0(R2),C'/'               PUT SLASH IN NAME AREA          00122400
         LA    R2,1(0,R2)               MOVE OVER ONE BYTE              00122500
         EX    R1,RESTNAME              RESTORE LAST WORD               00122600
         ST    R2,NMLSWDAD              SAVE NEW ADDR OF LAST WORD      00122700
         L     R2,NMNXWDAD              GET NEXT WORD ADDR              00122800
         LA    R2,1(0,R2)               ADD ONE TO IT                   00122900
         ST    R2,NMNXWDAD              SAVE NEXT WORD ADDR             00123000
         B     D0510                                                    00123100
SAVENAME MVC   NAMESAVE,0(R2)                                           00123200
RESTNAME MVC   0(0,R2),NAMESAVE                                         00123300
*********************************************************************** 00123400
*        ROUTINE TO GENERATE PERSONAL KEYS                            * 00123500
*********************************************************************** 00123600
D0510    EQU   *                                                        00123700
         CLI   WKRETKEY,C'0'            DO I WANT THE CIF KEY           00123800
         BE    D0900                    NO                              00123900
         MVC   WKNMKEY,CLRPKEY          CLEAR CIF KEY                   00124000
         MVI   WKNMKEY,C'P'             SET PERSONAL CODE               00124100
         IC    R1,NMNOWD                GET NO. WORDS IN NAME           00124200
         SLL   R1,2                     MULTIPLY BY 4                   00124300
         LA    R14,NMWDADTB-4(R1)       POINT TO WORD ADDR IN STACK     00124400
         L     R5,0(0,R14)              POINT TO WORD TABLE ENTRY       00124500
         LA    R15,WKNMKEY+1            POINT TO LAST NAME POSITION     00124600
         LA    R0,7                     SET MAX LNG-1 OF LAST NAME      00124700
         BAS   R14,D0550                GENERATE LAST KEY NAME2024183   00124800
         CLI   NMNOWD,X'01'             ONE WORD NAME                   00124900
         BE    D0900                    YES - CHK NEXT NAME             00125000
         L     R5,NMWDADTB              POINT TO FIRST NAME             00125100
         LA    R15,WKNMKEY+10           POINT TO FIRST NAME POSITION    00125200
         LA    R0,2                     SET MAX LNG-1 OF FIRST NAME     00125300
         BAS   R14,D0550                GENERATE FIRST KEYNAME2024183   00125400
         CLI   NMNOWD,X'02'             TWO WORDS IN NAME               00125500
         BE    D0900                    YES - CHK NEXT NAME             00125600
         L     R5,NMWDADTB+4            POINT TO MIDDLE NAME            00125700
         MVC   WKNMKEY+14(1),WORDSAVE   MOVE MIDDLE INITIAL             00125800
         B     D0900                    CHECK NEXT NAME                 00125900
         SPACE 3                                                        00126000
D0550    EQU   *                                                        00126100
         IC    R1,WORDLNG               GET WORD LENGTH                 00126200
         LR    R2,R1                    SAVE WORD LENGTH                00126300
         CR    R1,R0                    IS LNG TOO LONG                 00126400
         BNH   *+6                      NO                              00126500
         LR    R1,R0                    FORCE LNG TO MAX                00126600
         EX    R1,MVCKEYWD              MOVE WORD TO KEY FIELD          00126700
         SR    R2,R1                    GET NO CHAR REMAINING           00126800
         CH    R2,H9                    IS REMAINING LNG GT 9           00126900
         BNH   *+8                      NO                              00127000
         LH    R2,H9                    FORCE REM LNG TO 9              00127100
         AH    R2,H240                  MAKE A CHAR 9 IN REG 2          00127200
         LA    R15,1(0,R15)             BUMP TO REMAINDER POSITION      00127300
         AR    R15,R0                                                   00127400
         STC   R2,0(0,R15)              PUT NUMBER REMAINING IN KEY     00127500
         BR    R14                      RETURN                          00127600
MVCKEYWD MVC   0(0,R15),WORDSAVE                                        00127700
         EJECT                                                          00127800
*********************************************************************** 00127900
*        ROUTINE TO GENERATE COMMERCIAL KEYS                          * 00128000
*********************************************************************** 00128100
D0600    EQU   *                                                        00128200
         MVI   WKNMTYPE,C'C'            SET PERSONAL CODE               00128300
         CLI   WKRETKEY,C'0'            DO I WANT THE CIF KEY           00128400
         BE    D0900                    NO                              00128500
         MVC   WKNMKEY,CLRCKEY          CLEAR CIF KEY                   00128600
         MVI   WKNMKEY,C'C'             SET COMMERCIAL CODE             00128700
         SR    R1,R1                    CLEAR R1                        00128800
         SR    R14,R14                  CLEAR R14                       00128900
         IC    R14,NMNOWD               GET NO. WORDS IN NAME           00129000
         CH    R14,H5                   MORE THAN FIVE WORDS            00129100
         BNH   *+8                      NO                              00129200
         LH    R14,H5                   USE ONLY FIVE WORDS             00129300
         LA    R2,LNGTBL                POINT TO WORD LENGTH TBL        00129400
         LA    R9,NMWDADTB              POINT TO WORD ADDRESS TABLE     00129500
         LA    R15,WKNMKEY+1            POINT TO KEY FIELD              00129600
D0610    EQU   *                                                        00129700
         IC    R1,0(0,R2)               GET TO FIELD LENGTH             00129800
         L     R5,0(0,R9)               GET ADDR OF WORD                00129900
         CLM   R1,B'0001',WORDLNG       IS WORD SHORTER THAN TO FIELD   00130000
         BL    *+8                      NO                              00130100
         IC    R1,WORDLNG               USE WORD LENGTH                 00130200
         EX    R1,MVCKEYWD              MOVE WORD TO KEY FIELD          00130300
         IC    R1,0(0,R2)               GET TO FIELD LENGTH             00130400
         LA    R15,1(R1,R15)            BUMP TO NEXT OUTPUT POSITION    00130500
         LA    R2,1(0,R2)               BUMP TO NEXT LENGTH ENTRY       00130600
         LA    R9,4(0,R9)               BUMP TO NEXT WORD ADDRESS       00130700
         BCT   R14,D0610                LOOP BACK                       00130800
         B     D0900                    GO CHK FOR ANOTHER NAME         00130900
LNGTBL   DC    XL5'0304020100'          CHAR PER WORD - 4 5 3 2 1       00131000
         DS    0H                       REALIGN INSTRUCTIONS            00131100
         EJECT                                                          00131200
*********************************************************************** 00131300
*        CHECK FOR ANOTHER NAME                                       * 00131400
*********************************************************************** 00131500
D0900    EQU   *                                                        00131600
         TM    NMNMFLGS,NMEND           AT END OF NAMES                 00131700
         BO    E0010                    YES                             00131800
         LA    R6,NMARLNG(0,R6)         BUMP TO NEXT NAME AREA          00131900
         C     R6,NMTBLEND              AT END OF TABLE                 00132000
         BL    D0210                    NO - GO CHK NEXT NAME           00132100
         B     E0010                    YES                             00132200
         SPACE 3                                                        00132300
*********************************************************************** 00132400
*        CHECK TO SPLIT NAME BASED ON SALUTATIONS                     * 00132500
*********************************************************************** 00132600
E0010    EQU   *                                                        00132700
         CLI   WKRETSAL,C'2'            DO I SPLIT SALUTATIONS          00132800
         BNE   Z0010                    NO - RETURN                     00132900
         CP    WKNONAME,P5              IS NAME AREA FULL               00133000
         BNL   Z0010                    YES - CANT SPLIT ANYTHING       00133100
         LA    R6,NAMETBL               GET NAME POINTER AREA ADDR      00133200
E0020    EQU   *                                                        00133300
         CLI   SLNOWD,X'00'             ARE THERE ANY SALUTATION WORDS  00133400
         BE    E0090                    NO - CHECK NEXT NAME            00133500
         L     R7,NMWKNMAD              GET NAME RETURN AREA ADDR       00133600
         TM    NMNMFLGS,NMCONSAL        DID I FIND A CONN IN SALUT      00133700
         BO    E0030                    YES                             00133800
         CLI   SLNOWD,X'01'             IS THERE ONLY ONE WORD IN SALUT 00133900
         BNE   E0090                    NO - CHECK NEXT NAME            00134000
*        CHECK FOR MULTIPLE NAME BUT ONE WORD SALUTATION (M/M, MR&MRS)  00134100
         SR    R1,R1                    CLEAR R1                        00134200
         XC    TRTTBL2,TRTTBL2          CLEAR TRT TABLE                 00134300
         MVI   TRTTBL2+C'/',C'/'        PUT SLASH IN TRT TABLE          00134400
         MVI   TRTTBL2+C'&&',C'&&'      PUT AMPERSAND IN TRT TABLE      00134500
         TRT   WKNMSAL,TRTTBL2          IS SLASH OR AMP IN SALUTATION   00134600
         BZ    E0090                    NO - CHECK NEXT NAME            00134700
         MVI   SLNOWD,X'03'             FORCE 3 WORDS IN SALUTATION     00134800
         ST    R1,NMSLADTB+4            STORE ADDR OF CONNECTOR         00134900
         LA    R1,1(0,R1)               BUMP TO POSITION AFTER CONN     00135000
         ST    R1,NMSLADTB+8            STORE ADDR OF 2ND SALUTATION    00135100
         OI    NMNMFLGS,NMCONSAL        SET CONN-IN-SALUT FLAG          00135200
*        SPLIT INTO TWO NAMES - ONE FOR EACH SALUTATION                 00135300
E0030    EQU   *                                                        00135400
         SR    R14,R14                  CLEAR R14                       00135500
         IC    R14,WKNONAME             GET NO. OF NAMES ALREADY SET    00135600
         SRL   R14,4                    SHIFT OFF PACKED DECIMAL SIGN   00135700
         MH    R14,NMTBLNG              MULTIPLY BY ENTRY LENGTH        00135800
         LA    R14,NAMETBL(R14)         POINT TO FREE ENTRY IN TABLE    00135900
         L     R15,0(0,R14)             POINT TO FREE NAME WORK AREA    00136000
         STM   R14,R15,SVSPLTRG         SAVE POINTERS TO NEW AREA       00136100
         MVC   0(NMARLNG,R14),NAMEPTAR  COPY TABLE ENTRY TO NEW AREA    00136200
         ST    R15,0(0,R14)             RESTORE NAME WORK AREA ADDR     00136300
         LR    R0,R15                   GET ADDR OF WORK AREA IN R0     00136400
         S     R0,NMWKNMAD              CALC OFFSET OF NEW WORK AREA    00136500
         L     R1,NMNXWDAD-NMWKNMAD(0,R14)   CALC NEW NMNXWDAD          00136600
         AR    R1,R0                                                    00136700
         ST    R1,NMNXWDAD-NMWKNMAD(0,R14)                              00136800
         L     R1,NMLSWDAD-NMWKNMAD(0,R14)   CALC NEW NMLSWDAD          00136900
         AR    R1,R0                                                    00137000
         ST    R1,NMLSWDAD-NMWKNMAD(0,R14)                              00137100
         L     R1,NMLSNMAD-NMWKNMAD(0,R14)   CALC NEW NMLSNMAD          00137200
         AR    R1,R0                                                    00137300
         ST    R1,NMLSNMAD-NMWKNMAD(0,R14)                              00137400
         L     R1,SLNXWDAD-NMWKNMAD(0,R14)   CALC NEW SLNXWDAD          00137500
         AR    R1,R0                                                    00137600
         ST    R1,SLNXWDAD-NMWKNMAD(0,R14)                              00137700
         L     R1,TLNXWDAD-NMWKNMAD(0,R14)   CALC NEW TLNXWDAD          00137800
         AR    R1,R0                                                    00137900
         ST    R1,TLNXWDAD-NMWKNMAD(0,R14)                              00138000
         L     R1,NMSLADTB-NMWKNMAD(0,R14)   CALC NEW NMSLADTB          00138100
         AR    R1,R0                                                    00138200
         ST    R1,NMSLADTB-NMWKNMAD(0,R14)                              00138300
         L     R1,NMSLADTB-NMWKNMAD+4(0,R14) CALC NEW NMSLADTB+4        00138400
         AR    R1,R0                                                    00138500
         ST    R1,NMSLADTB-NMWKNMAD+4(0,R14)                            00138600
         L     R1,NMSLADTB-NMWKNMAD+8(0,R14) CALC NEW NMSLADTB+8        00138700
         AR    R1,R0                                                    00138800
         ST    R1,NMSLADTB-NMWKNMAD+8(0,R14)                            00138900
         MVC   0(WKNMARLN,R15),WKNMAREA COPY NAME ENTRY TO NEW AREA     00139000
         OI    NMNMFLGS,NMSLSPLT        SET SALUTATION SPLIT FLAG       00139100
         L     R14,NMSLADTB+4           GET ADDR OF SALUT CONNECTOR     00139200
         LA    R15,WKNMSAL+L'WKNMSAL-1  POINT TO LAST BYTE OF SALUT     00139300
         SR    R15,R14                  SUBTRACT ADDR OF CONNECTOR      00139400
         EX    R15,CLRSAL1              CLEAR REMAINDER OF SALUT        00139500
         LA    R14,1(0,R14)             BUMP TO NEXT CHAR               00139600
         BCTR  R15,0                    SUBTRACT 1 FROM LENGTH          00139700
         ST    R14,SLNXWDAD             SAVE SALUT NEXT WORD ADDR       00139800
         STC   R15,SLREMLNG             SAVE REMAINING SALUT AREA LNG   00139900
         MVI   SLNOWD,X'01'             SET NO. WORDS IN SALUTATION     00140000
         XC    NMSLADTB+4(8),NMSLADTB+4 CLEAR SALUT WORD ADDR STACK     00140100
         STM   R6,R7,SAVE2              SAVE REGS 6 AND 7               00140200
         LM    R6,R7,SVSPLTRG           LOAD POINTERS TO NEW AREA       00140300
         OI    NMNMFLGS,NMSLSPLT        SET SALUTATION SPLIT FLAG       00140400
         L     R14,NMSLADTB+8           GET ADDR OF LAST SALUT WORD     00140500
         LA    R15,WKNMSAL+L'WKNMSAL-1  POINT TO LAST BYTE OF SALUT     00140600
         SR    R15,R14                  SUBTRACT ADDR OF LAST WORD      00140700
         MVC   WORDWORK,SPACES          CLEAR WORK AREA                 00140800
         EX    R15,MVCSAL1              MOVE LAST SALUT WORD TO WORK    00140900
         MVC   WKNMSAL,WORDWORK         MOVE WORD BACK TO SALUT AREA    00141000
         L     R15,SLNXWDAD             GET ADDR OF NEXT WORD ADDR      00141100
         S     R15,NMSLADTB+8           SUB ADDR OF 3RD WORD-LNG OF WD  00141200
         LA    R14,WKNMSAL(R15)         CALC NEW ADDR OF NEXT SALUT WD  00141300
         ST    R14,SLNXWDAD             SAVE ADDR OF NEXT WORD          00141400
         LA    R14,L'WKNMSAL-2          CALC LNG OF                     00141500
         SR    R14,R15                      REMAINING AREA              00141600
         STC   R14,SLREMLNG                     IN SALUTATION           00141700
         MVI   SLNOWD,X'01'             SET NO. WORDS IN SALUTATION     00141800
         XC    NMSLADTB+4(8),NMSLADTB+4 CLEAR SALUT WORD ADDR STACK     00141900
         LM    R6,R7,SAVE2              RESTORE REGS 6 AND 7            00142000
         AP    WKNONAME,P1              ADD 1 TO NO. NAMES RETURNED     00142100
E0090    EQU   *                                                        00142200
         TM    NMNMFLGS,NMEND           AT END OF NAMES                 00142300
         BO    Z0010                    YES - RETURN                    00142400
         LA    R6,NMARLNG(0,R6)         BUMP TO NEXT NAME AREA          00142500
         C     R6,NMTBLEND              AT END OF TABLE                 00142600
         BL    E0020                    NO - GO CHK NEXT NAME           00142700
         B     Z0010                    YES - RETURN                    00142800
CLRSAL1  MVC   0(0,R14),SPACES                                          00142900
MVCSAL1  MVC   WORDWORK(0),0(R14)                                       00143000
         EJECT                                                          00143100
*********************************************************************** 00143200
*        RETURN TO CALLING PROGRAM                                    * 00143300
*********************************************************************** 00143400
Z0010    EQU   *                                                        00143500
         AIF   ('&SYSPARM' EQ 'CICS').CICSRET                           00143600
         LH    R15,WKRETCOD                                             00143700
         SIRETRN RC=(15)           RETURN                               00143800
         AGO   .NOCICS1                                                 00143900
.CICSRET ANOP                                                           00144000
         EXEC  CICS RETURN                                              00144100
.NOCICS1 ANOP                                                           00144200
         EJECT                                                          00144300
*********************************************************************** 00144400
*        ROUTINE INITIALIZATION PROCEDURES                            * 00144500
*********************************************************************** 00144600
INIT     EQU   *                                                        00144700
         ST    R1,SAVCOMM          SAVE COMM ADDRESS          9913483   00144710
         MVI   INITSW,X'FF'                                             00144800
         LA    R15,NAMETBL              GET ADDR OF NAME TABLE          00144900
         ST    R15,NMTBLAD              AND SAVE IT                     00145000
         LA    R15,NAMETBEN             GET ADDR OF END OF NAME TABLE   00145100
         ST    R15,NMTBLEND             AND SAVE IT                     00145200
         LA    R15,SAVEWORD             GET WORD AREA ADDR              00145300
         ST    R15,WORDSTRT             SAVE WORD AREA START            00145400
         LA    R15,480(0,R15)           GET WORD AREA END ADDR          00145500
         ST    R15,WORDEND              SAVE WORD AREA END              00145600
         LA    R15,NAMEWORK+256         GET NAME WORK AREA END ADDR     00145700
         ST    R15,NAMEWKEN             SAVE NAME WORK AREA END ADDR    00145800
         MVC   KEYTBLNM,=CL8'SIGN2T00'  SET DEFAULT KEY WORD TABLE NAME 00145900
         MVC   KEYADREN,=F'-1'          SET KEY ADDR TABLE END FLAG     00146000
         XC    WLANG,WLANG              CLEAR LANGUAGE CODE   2024478   00146002
         TM    4(R1),X'80'              2 PARAMETERS ONLY?    2024478   00146004
         BO    OPTE100                  YES, CONTINUE         2024478   00146006
         L     R15,8(R1)                THIRD PARAMETER       2024478   00146008
         MVC   WLANG,0(R15)             LANGUAGE CODE         2024478   00146009
* *** GET SASIOPTE RECORD                                     9913483   00146010
OPTE100  EQU   *                                              9913483   00146020
         CLI   WLANG,X'00'         IS THERE A LANG CODE?      2024478   00146023
         BNE   OPTE110             YES, BYPASS CALL           2024478   00146026
         USING SASIOPTE,R4         ADDRESS DSECT              9913483   00146030
         LA    R4,OPTEREC         GET SASIOPTE REC ADDR       9913483   00146040
         CLC   SIOPESN,=CL8'SASIOPTE'  RECORD PRESENT?        9913483   00146050
         BE    OPTE110            YES, GO PROCESS             9913483   00146060
         MVC   SIOPESN,=CL8'SASIOPTE'  SET FOR READ           9913483   00146070
         MVC   SIOPEC1,=X'FFFF'    SET CTL1 DFLT              9913483   00146080
         MVI   SIOPESK,C' '        CLEAR SEARCH KEY           9913483   00146090
         MVC   SIOPESK+1(L'SIOPESK-1),SIOPESK   ''            9913483   00146100
         AIF   ('&SYSPARM' NE 'CICS').BATCH                   9913483   00146110
         EXEC CICS LINK PROGRAM('SIPCCDIO')                            X00146120
                        COMMAREA(OPTEREC)                              X00146130
                        LENGTH(=Y(OPTELEN))                             00146140
         AGO   .COMMN                                         9913483   00146150
.BATCH   ANOP                                                 9913483   00146160
         SIPLIST PARMLIST,(OPTEREC)                           0444987   00146165
         ICM   R15,15,SIPBCDAD     SIPBCDIO ADDR              0444987   00146170
         BNZ   OPTE105                                        0444987   00146171
         SACALL SIPBCDIO,TYPE=LOAD                            0444987   00146175
         ST    R1,SIPBCDAD         SAVE ADDR                  0444987   00146180
         LR    R15,R1              LOAD ENTRY POINT           0444987   00146182
OPTE105  EQU   *                   GET SASIOPTE RECORD        0444987   00146185
         LA    R1,PARMLIST                                    9913483   00146190
         BASSM R14,R15             CALL ROUTINE               0444987   00146200
.COMMN   ANOP                                                 9913483   00146210
         MVC   WLANG,LANG          SAVE LANG VALUE            9913483   00146220
         DROP  R4                  RELEASE DSECT              9913483   00146230
OPTE110  EQU   *                                              9913483   00146240
         L     R1,SAVCOMM          RESTORE COMM ADDRESS       9913483   00146250
         B     A0010                                          9913483   00146260
         EJECT                                                9913483   00146270
         DS    0D                                                       00146300
CLRPADSP DC    CL1' '              SPACE IS PAD CHARACTER FOR MVCL      00146400
         DC    AL3(0)              ZERO FROM LENGTH FOR MVCL            00146500
H5       DC    H'5'                                                     00146600
H9       DC    H'9'                                                     00146700
H24      DC    H'24'                                                    00146800
H240     DC    H'240'                                                   00146900
P1       DC    P'1'                                                     00147000
P5       DC    P'5'                                                     00147100
NMTBLNG  DC    AL2(NMARLNG)                                             00147200
SPACES   DC    CL40' '                                                  00147300
ZEROS    DC    CL8'00000000'                                            00147400
CLRPKEY  DC    CL16'         0   0  '                         2601126   00147500
CLRCKEY  DC    CL16'                '                         2601126   00147600
ADDONE   DC    X'0102030405060708090A0B0C0D0E0F10111213141516171819'    00147700
SUBONE   DC    X'00000102030405060708090A0B0C0D0E0F1011121314151617'    00147800
         DS    0F                                                       00147900
CIFTITLE DC    CL4'SR'                                                  00148000
         DC    CL4'JR'                                                  00148100
         DC    CL4'IV'                                                  00148200
         DC    CL4'II'                                                  00148300
         DC    CL4'III'                                                 00148400
         DC    CL4'3RD'                                                 00148500
         DC    X'FF'               * END OF TITLES                      00148600
         EJECT                                                          00148700
         DS    0D                                                       00148800
* *** ENGLISH TRANSLATE TABLE                                 9913483   00148830
CLRSPCHE DC    C'                '   TRANSLATE TABLE TO REMOV 9913483   00148900
         DC    C'                '   SPECIAL CHARACTERS                 00149000
         DC    C'                '                                      00149100
         DC    C'                '                                      00149200
         DC    C'                '                                      00149300
         DC    C'&&               '                                     00149400
         DC    C'-/         , _  '                                      00149500
         DC    C'           # ''  '                                     00149600
         DC    C' ABCDEFGHI      '                                      00149700
         DC    C' JKLMNOPQR      '                                      00149800
         DC    C'  STUVWXYZ      '                                      00149900
         DC    C'                '                                      00150000
         DC    C' ABCDEFGHI      '                                      00150100
         DC    C' JKLMNOPQR      '                                      00150200
         DC    C'  STUVWXYZ      '                                      00150300
         DC    C'0123456789      '                                      00150400
* THIS IS THE VALID CHARACTER TABLE FOR ALL  CHARACTERS, CAN  2602409   00150406
* BE CHANGED TO BLANK OUT CHARS FOR ANY LANG NEEDED           2602409   00150408
CLRSPCHO DC    X'000102030405060708090A0B0C0D0E0F'    00-0F   9913483   00150410
         DC    X'101112131415161718191A1B1C1D1E1F'    10-0F   2602409   00150412
         DC    X'202122232425262728292A2B2C2D2E2F'    20-0F   2602409   00150414
         DC    X'303132333435363738393A3B3C3D3E3F'    30-0F   2602409   00150416
         DC    X'404142434445464748494A4B4C4D4E4F'    40-0F   2602409   00150418
         DC    X'505152535455565758595A5B5C5D5E5F'    50-0F   2602409   00150420
         DC    X'606162636465666768696A6B6C6D6E6F'    60-0F   2602409   00150422
         DC    X'707172737475767778797A7B7C7D7E7F'    70-0F   2602409   00150424
         DC    X'808182838485868788898A8B8C8D8E8F'    80-0F   2602409   00150426
         DC    X'909192939495969798999A9B9C9D9E9F'    90-0F   2602409   00150428
         DC    X'A0A1A2A3A4A5A6A7A8A9AAABACADAEAF'    A0-0F   2602409   00150430
         DC    X'B0B1B2B3B4B5B6B7B8B9BABBBCBDBEBF'    B0-0F   2602409   00150432
         DC    X'C0C1C2C3C4C5C6C7C8C9CACBCCCDCECF'    C0-0F   2602409   00150434
         DC    X'D0D1D2D3D4D5D6D7D8D9DADBDCDDDEDF'    D0-0F   2602409   00150436
         DC    X'E0E1E2E3E4E5E6E7E8E9EAEBECEDEEEF'    E0-0F   2602409   00150438
         DC    X'F0F1F2F3F4F5F6F7F8F9FAFBFCFDFEFF'    F0-0F   2602409   00150440
*                                                             9913483   00150444
TRTTBL1  DC    X'000102030405060708090A0B0C0D0E0F'                      00150500
         DC    X'101112131415161718191A1B1C1D1E1F'                      00150600
         DC    X'202122232425262728292A2B2C2D2E2F'                      00150700
         DC    X'303132333435363738393A3B3C3D3E3F'                      00150800
         DC    X'004142434445464748494A4B4C4D4E4F'                      00150900
         DC    X'505152535455565758595A5B5C5D5E5F'                      00151000
         DC    X'606162636465666768696A006C6D6E6F'                      00151100
         DC    X'707172737475767778797A7B7C7D7E7F'                      00151200
         DC    X'808182838485868788898A8B8C8D8E8F'                      00151300
         DC    X'909192939495969798999A9B9C9D9E9F'                      00151400
         DC    X'A0A1A2A3A4A5A6A7A8A9AAABACADAEAF'                      00151500
         DC    X'B0B1B2B3B4B5B6B7B8B9BABBBCBDBEBF'                      00151600
         DC    X'C0C1C2C3C4C5C6C7C8C9CACBCCCDCECF'                      00151700
         DC    X'D0D1D2D3D4D5D6D7D8D9DADBDCDDDEDF'                      00151800
         DC    X'E0E1E2E3E4E5E6E7E8E9EAEBECEDEEEF'                      00151900
         DC    X'F0F1F2F3F4F5F6F7F8F9FAFBFCFDFEFF'                      00152000
*                                                             9913483   00152010
SIPBCDAD DC    F'0'               BATCH I/O RTN ADDR          0444987   00152015
SIPBCDIO DC    CL8'SIPBCDIO'      BATCH SASIOPTE I/O RTN      9913483   00152020
SIPCCDIO DC    CL8'SIPCCDIO'      ONLINE SASIOPTE I/O RTN     9913483   00152030
*                                                                       00152100
         LTORG                                                          00152200
         EJECT                                                          00152300
         AIF   ('&SYSPARM' NE 'CICS').UNITE                   1216075   00152400
DFHEISTG DSECT                                                          00152500
STORADR  DS    F         FOR GETMAIN / WORDS LIST             1216075   00152520
HWORD    DS    H         HALF WORD USED WITH GETMAIN          1216075   00152530
DBLWORD  DS    F (DBLWORDS)       WORD PAIR LIST ADR          1216075   00152540
DSHWORD  DS    F (DSHWORDS)       WORDS WITH HYPHENS          1216075   00152550
KYWDTBL  DS    F (KEYWDTBL)       GARBAGE WORDS ETC           1216075   00152560
STORLEN  DS    F     AL4(WRDTBLEN-DBLWORDS)                   1216075   00152570
.UNITE   ANOP                                                 1216075   00152580
SAVCOMM  DS    F                  SAVE COMM ADDRESS           9913483   00152610
INITSAVE DC    5F'0'                                                    00152700
SVSPLTRG DC    2F'0'                                                    00152800
SAVE2    DC    2F'0'                                                    00152900
WORDSTRT DC    F'0'                                                     00153000
WORDEND  DC    F'0'                                                     00153100
INPEND   DC    F'0'                                                     00153200
NMTBLAD  DC    F'0'      ADDRESS OF NAMETBL                             00153300
NMTBLEND DC    F'0'      ADDRESS OF BYTE PAST NAMETBL                   00153400
NAMEWKEN DC    F'0'                                                     00153500
NXTLNADR DC    F'0'                                                     00153600
RLLNLNG  DC    H'0'                                                     00153700
KEYTBLNM DC    CL8' '                                                   00153800
ENDLNFLG DC    X'00'                                                    00153900
WORDFLAG DC    X'00'                                                    00154000
COMMFLAG DC    X'00'                                                    00154100
COMMIND  DC    X'00'                                                    00154200
LNMIND   DC    X'00'                                                    00154300
INITSW   DC    X'00'                                                    00154400
NAMESW   DC    X'00'                                                    00154500
WDSW     DC    X'00'                                                    00154600
CONNFND  DC    X'00'                                                    00154700
NEXTNMSW DC    X'00'                                                    00154800
FIRSTWRD DC    X'00'                                                    00154900
LSTNMFST DC    X'00'                                                    00155000
LINECTR  DC    X'00'                                                    00155100
WORDWORK DS    CL20                                                     00155200
         DS    0D                                                       00155300
NAMESAVE DS    CL40     NAME WORK AREA                                  00155400
NAMEWORK DS    CL300    INPUT NAME WORK AREA                            00155500
         DS    0D                                                       00155600
NONMRET  EQU   5        NUMBER OF NAMES IN RETURN AREA                  00155700
NAMETBL  DS    (NONMRET)CL68    OUTPUT NAME POINTER AREA                00155800
NAMETBEN EQU   *                                                        00155900
         DS    0D                                                       00156000
SAVEWORD DS    21CL24   WORD WORK AREA                                  00156100
SAVEWDLN EQU   *-SAVEWORD                                               00156200
         DS    0D                                                       00156300
TRTTBL2  DS    CL256                                                    00156400
         DS    0D                                                       00156500
KEYTBLNG EQU   36       LENGTH OF EACH KEY TABLE ENTRY                  00156600
KEYWDID  DC    CL2' '   CURRENT KEY WORD TABLE ID                       00156700
         DC    XL2'00'                                                  00156800
WRDTBLAD DC    F'0'     ADDRESS OF KEY WORD TABLE                       00156900
WRDTBLEN DC    F'0'     ADDRESS OF END OF WORD TABLE                    00157000
         AIF ('&SYSPARM' EQ 'CICS').BACK                      1216075   00157050
DBLWORD  DC    F'0'     ADDRESS OF WORD PAIR LIST                       00157100
DSHWORD  DC    F'0'     ADDRESS OF DASH WORD TABLE                      00157200
.BACK    ANOP                                                 1216075   00157250
BINFACS  DC    4F'0'    SAVE CALCULATED INITIAL SEARCH FACTORS          00157300
KEYADRTB DC    180F'0'  TWENTY ENTRIES AT 9 FULLWORDS PER ENTRY         00157400
KEYADREN DC    F'-1'    END OF TABLE FLAG                               00157500
         EJECT                                                          00157600
*                                                             9913483   00157610
WLANG    DS    CL2                SASIOPTE LANG FIELD         9913483   00157620
PARMLIST DS    2F                 BATCH SIPBCDIO LIST         9913483   00157630
OPTEREC  DS    CL512              SASIOPTE RECORD             9913483   00157640
*                                                             9913483   00157650
TABLEWRD DSECT                                                          00157700
TBLWRD   DS    CL15                                                     00157800
TBLWRDFL DS    CL1                                                      00157900
TBLWDLNG EQU   *-TBLWRD                                                 00158000
         SPACE 3                                                        00158100
WORDAREA DSECT                                                          00158200
WORDSAVE DS    CL20                                                     00158300
WORDLNG  DS    CL1                                                      00158400
WLINEND  DS    CL1                                                      00158500
ENDOFNAM EQU   X'80'    END OF INPUT                                    00158600
ENDOFLIN EQU   X'40'    END OF LINE                                     00158700
BEGOFLIN EQU   X'20'    BEGINNING OF LINE                               00158800
WORDSTAT DS    CL1                                                      00158900
COMMWD   EQU   X'80'    COMMERCIAL WORD                                 00159000
CONNWD   EQU   X'40'    CONNECTOR WORD                                  00159100
SALUTWD  EQU   X'20'    SALUTATION WORD                                 00159200
TITLEWD  EQU   X'10'    TITLE WORD                                      00159300
LNMWD    EQU   X'08'    MULTIPLE WORD LAST NAME                         00159400
*        EQU   X'04'                                                    00159500
SALCONWD EQU   X'02'    SALUTATION CONNECTOR WORD                       00159600
GARBWD   EQU   X'01'    GARBAGE WORD                                    00159700
CONNONLY EQU   X'FF'-COMMWD-CONNWD-GARBWD                               00159800
SPCLWORD EQU   X'FF'-LNMWD    ANY TYPE EXCEPT MULTIPLE-WORD LAST NAME   00159900
WRDSTAT2 DS    CL1                                                      00160000
COMINWD  EQU   X'80'    COMMA AT END OF INPUT WORD                      00160100
CONNEXT  EQU   X'40'    TREAT WORD AS CONNECTOR                         00160200
WDSVLNG  EQU   *-WORDSAVE                                               00160300
         SPACE 3                                                        00160400
NXWDAREA DSECT                                                          00160500
NXWDSAVE DS    CL20                                                     00160600
NXWDLNG  DS    CL1                                                      00160700
NXLINEND DS    CL1                                                      00160800
NXWDSTAT DS    CL1                                                      00160900
NXWDSTA2 DS    CL1                                                      00161000
         SPACE 3                                                        00161100
NAAREA   DSECT                                                          00161200
NADATA   DS    CL256                                                    00161300
         EJECT                                                          00161400
NAMEPTAR DSECT          NAME POINTER DSECT                              00161500
NMWKNMAD DS    F        ADDR OF NAME WORK AREA                          00161600
NMNXWDAD DS    F        ADDR OF NEXT POSITION TO PUT WORD IN NAME       00161700
NMLSWDAD DS    F        ADDR OF THE LAST WORD PUT IN NAME               00161800
NMLSNMAD DS    F        ADDR OF THE LAST NAME WORD                      00161900
NMLSNMPT DS    F        ADDR OF THE LAST NAME WORD IN WORD TABLE        00162000
SLNXWDAD DS    F        ADDR OF NEXT POSITION TO PUT WORD IN SALUTATION 00162100
TLNXWDAD DS    F        ADDR OF NEXT POSITION TO PUT WORD IN TITLE      00162200
NMLSWDLN DS    XL1      LENGTH-1 OF THE LAST WORD PUT IN NAME           00162300
NMLSNMLN DS    XL1      LENGTH-1 OF THE LAST NAME WORD                  00162400
SLREMLNG DS    XL1      LENGTH OF REMAINING AREA FOR SALUTATION         00162500
TLREMLNG DS    XL1      LENGTH OF REMAINING AREA FOR TITLE              00162600
NMNOWD   DS    XL1      NUMBER OF WORDS PUT IN NAME                     00162700
SLNOWD   DS    XL1      NUMBER OF WORDS PUT IN SALUTATION               00162800
NMNMFLGS DS    XL1      FLAGS FOR PROCESSING NAME                       00162900
NMLSNMMV EQU   X'80'    LAST NAME NEEDS TO BE FOUND                     00163000
NMLSNMFS EQU   X'40'    LAST NAME FIRST                                 00163100
NMCONSAL EQU   X'04'    CONNECTOR IN SALUTATION FLAG                    00163200
NMSLSPLT EQU   X'02'    NAME SPLIT ON SALUTATION                        00163300
NMEND    EQU   X'01'    END OF NAMES FLAG                               00163400
NMWDFLGS DS    XL1      WORK AREA TO IDENTIFY COMMERCIAL NAMES          00163500
NMWDADTB DS    5F       STACK TO SAVE WORD ADDR OF EACH NAME WORD       00163600
NMSLADTB DS    3F       STACK TO SAVE WORD ADDR OF EACH SALUT WORD      00163700
NMARLNG  EQU   *-NMWKNMAD LENGTH OF THIS DSECT                          00163800
         SPACE 3                                                        00163900
NEXTPTAR DSECT          NEXT NAME POINTER DSECT                         00164000
NXWKNMAD DS    F        NEXT ADDR OF NAME WORK AREA                     00164100
NXNXWDAD DS    F        NEXT ADDR OF NEXT POSITION TO PUT WORD IN NAME  00164200
NXLSWDAD DS    F        NEXT ADDR OF THE LAST WORD PUT IN NAME          00164300
NXLSNMAD DS    F        NEXT ADDR OF THE LAST NAME WORD                 00164400
         DS    3F                                                       00164500
NXLSWDLN DS    XL1      NEXT LENGTH-1 OF THE LAST WORD PUT IN NAME      00164600
NXLSNMLN DS    XL1      NEXT LENGTH-1 OF THE LAST NAME WORD             00164700
         DS    XL2                                                      00164800
NXNOWD   DS    XL1      NEXT NUMBER OF WORDS PUT IN NAME                00164900
         DS    XL1                                                      00165000
NXNMFLGS DS    XL1      NEXT FLAGS FOR PROCESSING NAME                  00165100
NXWDFLGS DS    XL1      NEXT WORK AREA TO IDENTIFY COMMERCIAL NAMES     00165200
NXWDADTB DS    5F       STACK TO SAVE WORD ADDR OF EACH NAME WORD       00165300
NXSLADTB DS    3F       STACK TO SAVE WORD ADDR OF EACH SALUT WORD      00165400
         EJECT                                                          00165500
GNM2WKAR DSECT                                                          00165600
         DS    CL8      WORK AREA EYE-CATCHER                           00165700
WKNMADDR DS    CL4      NAME INPUT AREA ADDRESS                         00165800
WKARLNG  DS    XL2      INPUT AREA LENGTH                               00165900
WKLNLNG  DS    XL2      NAME LINE LENGTH (ACTUAL NAME AREA)             00166000
WKLNOFS  DS    XL2      NAME DATA OFFSET IN INPUT LINE                  00166100
WKRETCOD DS    XL2      RETURN CODE                                     00166200
*                           0 - NO ERRORS                               00166300
*                           4 - NAME ONLY ERRORS                        00166400
*                           8 - SERIOUS ROUTINE ERRORS                  00166500
*                          12 - COMBINATION OF 8 AND 4 RETURN CODES     00166600
*                          16 - ROUTINE FATAL ERRORS - NO OUTPUT        00166700
WKENLNOF DS    XL2      END OF LINE OFFSET (TO IGNORE AT END OF A LINE) 00166800
WKTBLID  DS    CL2      KEY WORD TABLE MODULE ID                        00166900
WKFORCE  DS    CL1      TYPE FORCE CODE                                 00167000
*                           SPACE - DONT FORCE                          00167100
*                           P     - FORCE PERSONAL                      00167200
*                           C     - FORCE COMMERCIAL                    00167300
WKRETKEY DS    CL1      RETURN CIF KEY                                  00167400
WKRETSAL DS    CL1      RETURN SALUTATION FLAG                          00167500
WKRETTTL DS    CL1      RETURN TITLE FLAG                               00167600
WKNMONLY DS    CL1      ONLY NAME LINES PASSED                          00167700
         DS    CL3      UNUSED OPTION FLAGS                             00167800
WKRTNOPT DS    CL8      ERROR RETURN OPTION FLAGS                       00167900
         DS    CL6      FILLER                                          00168000
WKNONAME DS    PL1      NUMBER OF NAMES RETURNED                        00168100
WKLSTLNE DS    PL1      LAST INPUT LINE A NAME WAS FOUND ON             00168200
WKNAMES  DS    (NONMRET)CL104    NAME RETURN AREA                       00168300
WKNAMELN EQU   *-WKNAMES                                                00168400
WKNMERMS DS    CL256    NAME ERROR MESSAGES TEXT                        00168500
WKERRFLG DS    CL8      ERROR FLAGS                                     00168600
         DS    CL6      FILLER                                          00168700
WKERREND DS    XL2      NUMBER OF ERROR MESSAGES FOLLOWING              00168800
WKERRMSG DS    0CL32    ROUTINE ERROR MESSAGES TEXT                     00168900
         SPACE 3                                                        00169000
WKNMAREA DSECT                                                          00169100
WKNMTYPE DS    CL1      TYPE OF NAME                                    00169200
*                           P - PERSONAL                                00169300
*                           C - COMMERCIAL                              00169400
WKNMLINE DS    PL1      INPUT LINE NUMBER NAME WAS FOUND ON             00169500
         DS    CL5      FILLER                                          00169600
WKNMERIN DS    CL1      NAME ERROR INDICATOR                            00169700
*                           SPACE - NO ERROR FOUND                      00169800
*                           E     - ERROR ON NAME - CHECK NAME FLAGS    00169900
WKNMERFL DS    CL8      NAME ERROR FLAGS                                00170000
WKNMSAL  DS    CL16     SALUTATION                                      00170100
WKNMNAME DS    CL40     STANDARD NAME                                   00170200
WKNMTTL  DS    CL16     TITLE                                           00170300
WKNMKEY  DS    CL16     CIF KEY                                         00170400
WKNMARLN EQU   *-WKNMTYPE                                               00170500
         EJECT                                                9913483   00170510
         COPY  SASIOPEA                                       9913483   00170520
         AIF   ('&SYSPARM' NE 'CICS').DTSBAT                            00170785
FIGETNM2 CSECT                                                          00170786
         AGO   .DTSMRG                                                  00170787
.DTSBAT  ANOP                                                           00170788
SIGETNM2 CSECT                                                          00170789
.DTSMRG  ANOP                                                           00170790
         LTORG                                                          00170791
         DS    0D                                                       00170792
SITMSTMP DC    CL64'SIGETNM2  -----TSD-             10/26/11  14.49.28' 00170793
*        THIS PROGRAM CONTAINS TRADE SECRETS THAT BELONG                00170794
*        TO FIDELITY INFORMATION SERVICES AND IS                        00170795
*        LICENSED BY AN AGREEMENT.  ANY UNAUTHORIZED ACCESS,            00170796
*        USE, DUPLICATION, OR DISCLOSURE IS UNLAWFUL.                   00170797
*        COPYRIGHT FIDELITY INFORMATION SERVICES                        00170798
*        2011, ALL RIGHTS RESERVED.                                     00170799
         END                                                            00170800
