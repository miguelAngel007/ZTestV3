*     * 802262 12/11/20 PROYECTO UPGRADE GN172 EDVR
         TITLE 'SISSBLD - S S R  INCORE TABLE BUILD MODULE'             00000100
*---------------------------------------------------------------------* 00000200
*                   ** PROGRAM DESCRIPTION **                         * 00000300
*                                                                     * 00000400
*    TITLE : SISSBLD - SSR  INCORE TABLE BUILD MODULE                 * 00000500
*                                                                     * 00000600
*    FUNCTION :                                                       * 00000700
*       THIS PROGRAM IS USED TO BUILD AN INCORE COPY OF THE SEGMENT   * 00000800
*       SPECIFICATION CONTROL CORE FILE.  THIS IS DONE AT BEGINNING   * 00000900
*       OF A BATCH PROGRAM SO THAT PERFORMANCE IS MAXIMIZED WHEN      * 00001000
*       PERFORMING I/O TO THE SEGMENTED/GROUPED FILES (I.E., NO       * 00001100
*       ADDITIONAL ACCESSES TO THE SSR FILE ARE NEEDED).              * 00001200
*                                                                     * 00001300
*       A FOURTH PARAMETER CAN BE PASSED TO THIS MODULE. IF   0525104 * 00001310
*       THE FOURTH PARAMETER IS PRESENT, MAXIMUM NUMBER OF    0525104 * 00001312
*       ENTIRES IN THE SSR TABLE IS SET TO 299 AND THE MAXIMUM0525104 * 00001314
*       NUMBER OF SEGMENTS FOR THE STACK IS SET TO 200. IF THE0525104 * 00001316
*       FOURTH PARAMETER IS NOT PASSED, THE MAXIMUM NUMBER OF 0525104 * 00001318
*       ENTIRES IN THE SSR TABLE IS SET TO 199 AND THE MAXIMUM0525104 * 00001320
*       NUMBER OF SEGMENTS FOR THE STACK IS SET TO 25.        0525104 * 00001322
*********************************************************************** 00001400
*                                                                     * 00001500
* NEW PROGRAM TO SUPPORT FILE SEGMENTATION                            * 00001600
*                                                                     * 00001700
*********************************************************************** 00001800
*                                                                     * 00001810
*---------------------------------------------------------------------* 00001820
         EJECT                                                          00001830
*---------------------------------------------------------------------* 00001840
*                  ** HISTORY OF REVISIONS **                         * 00001850
*                                                                     * 00001860
* DESCRIPTION                                                 CHNGID  * 00001870
* __________________________________________________________  _______ * 00001880
* 10/12/04 INCREASED THE MAXIMUM NUMBER OF SSR SEGMENTS TO    0525104 * 00001936
* 200 AND THE MAXIMUM NUMBER OF ENTRIES IN THE SSR TABLE      0525104 * 00001937
* TO 299.                                                     0525104 * 00001938
* 03/07/00 CHANGE BAL TO BAS                                  2024183 * 00001939
* 12/19/95 MODIFY FOR ACCOUNT TO FOLLOW ACTUAL CONTROLS       2602017 * 00001940
* 07/26/94 MODIFY TO ALLOW ALPHANUMERIC GROUPS                2602266 * 00001960
*                                                                     * 00001970
*---------------------------------------------------------------------* 00001980
         EJECT                                                          00001990
         COPY SIOPTNS                                                   00002000
SSBLD    TITLE 'SISSBLD - DSECT TABLES'                                 00002100
*                                                                     * 00002200
*********************************************************************** 00002300
*                                                                     * 00002400
* * * *                   DSECT TABLES                          * * * * 00002500
*                                                                     * 00002600
*********************************************************************** 00002700
         SPACE 3                                                        00002800
         COPY SIDSSBLD             DSECT FOR SSR BATCH TABLE            00002900
         SPACE 3                                                        00003000
*                                                                       00003100
*                                                                     * 00003200
*        ----------------------------------                             00003300
*        SEGMENT SPECIFICATION RECORD DSECT                             00003400
*        ----------------------------------                             00003500
         SPACE 3                                                        00003600
         COPY  TSTS2SSR            RECORD LAYOUT OF THE SSR RECORDS     00003700
         SPACE 3                                                        00003800
*        -----------------------------                                  00003900
*        STANDARD SI I/O CONTROL DSECT                                  00004000
*        -----------------------------                                  00004100
         SPACE 3                                                        00004200
         COPY  SIDSCNTL                                                 00004300
         TITLE 'SISSBLD - PROGRAM ENTRY POINT AND MAINLINE PROCESSING'  00004400
*                                                                     * 00004500
*********************************************************************** 00004600
*                                                                     * 00004700
* * * *        PROGRAM ENTRY POINT AND GENERAL HOUSEKEEPING     * * * * 00004800
*                                                                     * 00004900
*********************************************************************** 00005000
*   REGISTER USEAGE                                                     00005100
*   REG 1 -  PARMS PASSING BETWEEN CALLING AND CALLED PGMS              00005200
*   REG 3 -  APPLICATION REQUEST                                        00005300
*   REG 4 -  INCORE TABLE ADRESS                                        00005400
*   REG 5 -  STACK TABLE ADDRESS                                        00005500
*   REG 9  - IO REQUEST REGISTER                                        00005600
*   REG 10 - CURRENT SSR TABLE ENTRY POINTER                            00005700
*   REG 11 - SSR TABLE PASSED FROM APPLICATION                          00005800
*                                                                     * 00005900
         SPACE  3                                                       00006000
SISSBLD  CSECT                                                          00006100
         SIBASE BASEREG=(BASEREG),ID=SISSBLD,RWNUM=0022,RWSUB=000       00006200
         LA    R9,SIIOREQ                                               00006300
         USING SIDSCNTL,R9         GIVE ADDRESSABILITY FOR IO CALL      00006400
         STCM  R1,B'1111',ORIG1   SAVE ORIGINAL R1 FOR LATER            00006500
         LM    R3,R5,0(R1)        LOAD PARAMETER ADDRESSES              00006600
         USING SIBLDPA,R3          NOTIFY ASSEMBLER OF DSECT BASE REGS  00006700
         USING SIBLTBL,R4          ...                                  00006800
         USING SIBLSTKT,R5         ...                                  00006900
*                                     REG 3 = APPLICATION REQUEST PARMS 00007000
*                                     REG 4 = APPLICATION TABLE AREA    00007100
*                                     REG 5 = APPLICATION STACK AREA    00007200
         STM   R6,R9,SAVEREGS        BACKUP REGISTERS         0525104   00007325
         LA    R6,SIBLSTNT           LOAD ADDRESS OF SIBLSTNT 0525104   00007330
         LA    R7,SIBLSTLN           LOAD LENGTH  OF SIBLSTNT 0525104   00007335
         LA    R9,1                  CLEAR THE REG            0525104   00007340
         LA    R8,BIN0               MOVE FROM LOW VALUES     0525104   00007345
         MVCL  R6,R8                 MOVE LOW VALUES          0525104   00007350
         LM    R6,R9,SAVEREGS        RESTORE REGISTERS        0525104   00007355
         MVI   SIBLDRET,SIBNRMRT   SET RETURN CODE TO ZERO              00007400
         TITLE 'SISSBLD - MAINLINE PROCESSING'                          00007500
*                                                                     * 00007600
*********************************************************************** 00007700
*                                                                     * 00007800
* * * *                  MAINLINE PROCESSING                    * * * * 00007900
*                                                                     * 00008000
*********************************************************************** 00008100
*                                                                     * 00008200
*        ----------------                                               00008300
*        SSR READ ROUTINE                                               00008400
*        ----------------                                               00008500
         SPACE 3                                                        00008600
         TM    8(R1),X'80'         3 PARAMETERS ONLY?         0525104   00008610
         BO    SETLMT              YES, GO SET LIMITS         0525104   00008612
         ZAP   MAXTBNTS,=P'299'    SET TABLE LIMIT            0525104   00008614
         ZAP   MAXSEGCT,=P'200'    SET SEGMENT LIMIT          0525104   00008616
         B     READRTN             CONTINUE                   0525104   00008618
SETLMT   DS    0H                                             0525104   00008620
         ZAP   MAXTBNTS,=P'199'    SET TABLE LIMIT            0525104   00008622
         ZAP   MAXSEGCT,=P'25'     SET SEGMENT LIMIT          0525104   00008624
READRTN  DS    0H                                                       00008700
*  OPEN TSSSR FILE FOR INPUT ***                                        00008800
         MVC   TSSSRMV,SIBLDFIL    MOVE IN LOAD MODULE FOR I/O          00008900
         MVI   TSSSRMV+7,SPACE     PAD LOAD MODULE NAME WITH SPACES     00009000
         LOAD  EPLOC=TSSSRMV       LOAD IO MODULE FOR OPEN              00009100
         ST    R0,AIORTN           SAVE ADDRESS FOR FUTURE CALLS        00009200
         LR    R15,R0              GET ADDRESS INTO R15                 00009300
         LA    R1,SIIOPARM         SET UP PARM LIST FOR CALL            00009400
         ST    R9,0(R1)            SET UP IOREQ AS PARAMETER FOR IOMOD  00009500
         LA    R11,SSRREC         POINTER TO CURRENT SSR RECORD         00009600
         USING TSTSSSR,R11        ADDRESSABILITY TO SSR RECORD          00009700
         ST    R11,4(R1)          SECOND PARM IS IOAREA FOR READ      * 00009800
         OI    4(R1),X'80'         SET END OF LIST                      00009900
         MVI   IOOPER,C'O'         INDICATE OPEN REQUEST                00010000
         MVI   IOACCESS,C'I'         FOR INPUT                          00010100
         CALL  (15)                CALL IORTN                           00010200
         OC    IORETCOD,IORETCOD   ANY ERRORS?                          00010300
         BNZ   OPENERR             YES, GO TO ERROR RTN AND QUIT        00010400
*** HERE, SSR IS SUCCESSFULLY OPENED ***                                00010500
         LA    R1,SIIOPARM        SET UP PARAMETER LIST AGAIN           00010600
         ST    R9,0(R1)           PASS REQUEST LIST                     00010700
         MVI   IOOPER,C'T'        READ SEQUENTIAL                       00010800
         LR    R10,R4             POINT TO BEGINNING OF TABLE           00010900
         MVC   SSRAPP,SIBLDAPP    MOVE IN APPLICATION ID FOR KEY        00011000
         MVI   SSRCTLS,SPACE      INITIALIZE CONTROLS FOR KEY           00011100
         MVC   SSRCTLS+1(L'SSRCTLS-1),SSRCTLS PROPOGATE SPACES          00011200
*** THE ABOVE LINE IS TO INSURE THAT FIRST ENTRY LOGIC WORKS ****       00011300
***   IT ASSUMES THAT SUMMARY RECORD IS SAME LENGTH AS DETAIL ***       00011400
         USING SIBLDET,R10        ADDRESSABILITY TO CURRENT RCD         00011500
         ZAP   SIBLSNOE,PKED0     SET NUMBER OF ENTRIES TO ZERO         00011600
         L     R15,AIORTN          GET IORTN ADDRESS                    00011700
         CALL  (15)                CALL IORTN FOR STARTBROWSE           00011800
         OC    IORETCOD,IORETCOD   ANY ERRORS?                          00011900
         BNZ   TSTEOF             NO, CONTINUE                          00012000
READLOOP DS    0H                                                       00012100
         L     R15,AIORTN          GET IORTN ADDRESS                    00012200
         CALL  (15)                CALL IORTN                           00012300
         CLC   IORETCOD,BIN0      IS IT SUCCESSFUL                      00012400
         BNE   TSTEOF             NO, CHECK FOR END OF FILE VS ERROR    00012500
         CLC   SIBLDAPP,SSRAPP    IS IT THE CORRECT APPLICATION * * * * 00012600
         BL    ENDREAD            BEYOND APPLICATION -        2602266   00012700
         BNE   READLOOP           BEFORE APPLID, BYPASS THIS RECORD     00012800
         CLC   SSRFILID,HEXFFS    IS THIS AN APPL DEFAULT ENTRY?        00012810
         BNE   READLOOP           NO, SKIP SPECIFIC FILES FOR ONINE GP  00012820
*** BY NOW IT'S A RECORD FOR THE CORRECT APPLICATION - VALIDATE  *      00012900
         DS    0H                                                       00013000
         CLC   SSRBTSEG,SPACES    SEGMENT # < SPACES          2602266   00013100
         BL    BADSEG             YES, ERROR                  2602266   00013400
*** YES, SEGMENT NUMBER IS VALID --- CHECK IF IT'S SAME AS LAST ENTRY   00013700
VALIDSEG DS    0H                                                       00013800
         CP    SIBLSNOE,PKED0        IS THIS THE FIRST ENTRY            00013900
         BE    ADDENT                YES, BYPASS DUPCHECK               00014000
         CLC   SSRBTSEG(SIBLDGRL),SIBLDSEG CHECK CURRENT VS LAST        00014100
         BE    READLOOP           IF ITS SAME AS LAST, DONT ADD         00014200
ADDENT   DS    0H                 IT'S A VALID NEW ENTRY, ADD IT        00014300
         CP    SIBLSNOE,MAXTBNTS  BEYOND TABLE?                         00014400
         BNL   TBLFLERR           YES, SET ERROR                        00014500
         AP    SIBLSNOE,PKED1     INCREMENT ENTRY NUMBER                00014600
         LA    R10,SIBLDLEN(R10)  POINT TO NEW ENTRY                    00014700
         MVC   SIBLDEAP,SSRAPP    LOAD APPLICATION                      00014800
         MVC   SIBLDALL,SSRCTLS   YES, MOVE 28 BYTES          2511795   00014803
         MVC   SIBLDSEG,SSRBTSEG  LOAD BATCH SEG                        00014820
         MVC   SIBLDMGR,SSRMSTGP  LOAD MST GROUP                        00014830
         MVC   SIBLDCGR,SSRCTLGP  LOAD CTL GROUP                        00014840
         MVC   SIBLDAGR,SSRALPGP  LOAD ALPHA GROUP                      00014850
         B     READLOOP           TO THE TABLE AND CONTINUE             00014900
TSTEOF   DS    0H                 ON NON-ZERO RETURN FROM READ          00015000
         CLC   IORETCOD,=H'4'     IS IT END OF FILE?                    00015100
         BE    ENDREAD            YES, CHECK FOR ENTRIES                00015200
         CLC   IORETCOD,=H'8'     IS IT RECORD NOT FOUND FROM BROWSE?   00015300
         BE    EMPTYERR           YES, ITS AN ERROR                     00015400
         CLC   IORETCOD,=H'12'    IS IT EMPTY FILE?                     00015500
         BE    EMPTYERR           YES, RETURN AN ERROR                  00015600
         B     IOERR              OTHERWISE, ITS REAL I/O ERROR         00015700
         SPACE 3                                                        00015800
*        -------------------------                                      00015900
*        CREATE SSR SUMMARY RECORD                                      00016000
*        -------------------------                                      00016100
         SPACE 3                                                        00016200
*********************************************************************** 00016300
*** SSR TABLE HAS BEEN BUILT SUCCESSFULLY - START NEXT PHASE OF PGM *** 00016400
*** MOVE FIELDS TO SUMMARY RECORD                                       00016500
*********************************************************************** 00016600
ENDREAD  DS    0H                 COME HERE FOR REAL OR SIMULATED EOF   00016700
         CP    SIBLSNOE,PKED0     ANY ENTRIES MOVED INTO TABLE?         00016800
         BNH   EMPTYERR           NO, RETURN ERROR                      00016900
         LA    R0,SIBLDLEN        PICK UP TABLE LENGTH IN BINARY        00017000
         CVD   R0,DWORD           CONVERT TO PACKED DECIMAL             00017100
         MVC   SIBLSLN,DWORD+6    ONLY MOVE LOW ORDER 2 BYTES           00017200
*** CALCULATE NUMBER OF UNIQUE GROUPS/SEGEMTNS FOR THE SUMMARY RCD ***  00017300
         LA    SSRPTR,SIBLTBDE+SIBLDAGR-SIBLDET POINT TO ALPHA GROUPS   00017400
         BAS   RETRG,UNIQRTN       CALL CALCULATOR RTN        2024183   00017500
         MVC   SIBLSACT,UNIQTOT    MOVE IN RESULT TO SUMMARY RCD        00017600
         LA    SSRPTR,SIBLTBDE+SIBLDCGR-SIBLDET POINT TO CONTROL GROUPS 00017700
         BAS   RETRG,UNIQRTN       CALL CALCULATOR RTN        2024183   00017800
         MVC   SIBLSCCT,UNIQTOT    MOVE IN RESULT TO SUMMARY RCD        00017900
         LA    SSRPTR,SIBLTBDE+SIBLDMGR-SIBLDET POINT TO MASTER GROUPS  00018000
         BAS   RETRG,UNIQRTN       CALL CALCULATOR RTN        2024183   00018100
         MVC   SIBLSMCT,UNIQTOT    MOVE IN RESULT TO SUMMARY RCD        00018200
         LA    SSRPTR,SIBLTBDE+SIBLDSEG-SIBLDET POINT TO SEGMENT ID'S   00018300
         BAS   RETRG,UNIQRTN       CALL CALCULATOR RTN        2024183   00018400
         MVC   SIBLSSCT,UNIQTOT    MOVE IN RESULT TO SUMMARY RCD        00018500
         SPACE 3                                                        00018600
*        --------------------------                                     00018700
*        BUILD SEGMENT NUMBER STACK                                     00018800
*        --------------------------                                     00018900
         SPACE 3                                                        00019000
*********************************************************************** 00019100
*** SSR SUMMARY RECORD IS NOW COMPLETE.  THE LAST CALL TO UNIQRTN   *** 00019200
*** BUILT A LIST OF UNIQUE SEGMENTS.  MOVE THESE TO THE CALLERS     *** 00019300
*** RESPONSE AREA.                                                  *** 00019400
*********************************************************************** 00019500
         SPACE 3                                                        00019600
         LA    WORKREG,UNIQENT      POINT TO CURRENT UNIQUE TABLE       00019700
         CP    UNIQTOT,MAXSEGCT     SEGMENT COUNT WITHIN DESIGN LIMIT?  00019900
         BH    MAXSEG               NO RETURN BAD RESPONSE TO USER      00020000
         LA    SSRPTR,SIBLSTKT      POINT TO STACK TABLE                00020100
         ZAP   STKNUMRT,PKED0       ZERO OUT FIELD                      00020200
STACKLP  DS    0H                                                       00020300
         CLC   0(L'SIBLSTNT,WORKREG),BIN0        IS IT INITIALIZED      00020400
         BE    ENDSTAK                  NO, END OF ENTRIES    2602266   00020500
         MVC   0(L'SIBLSTNT,SSRPTR),0(WORKREG) SEGMENT IS USED-MOVE IT  00020600
         ZAP   STKNUMRT,PKED1           INCREMENT FIELD                 00020700
         LA    SSRPTR,L'SIBLSTNT(SSRPTR) POINT TO NEXT STACK AREA       00020800
DONTSTAK DS    0H                                                       00020900
         LA    WORKREG,2(WORKREG)       POINT TO NEXT UNIQTBL ENTRY     00021000
         B     STACKLP                                        2602266   00021100
ENDSTAK  DS    0H                                             2602266   00021110
         CP    STKNUMRT,PKED1           ANY ENTRIES RETURNED?           00021200
         BNL   BINSRCH                  YES, DONT SET RETURN CODE       00021300
         MVI   SIBLDRET,SIBMPTRT  INDICATE NO STACK, BUT CONTINE BLD    00021400
         SPACE 3                                                        00021500
*        -------------------------------------                          00021600
*        CALCULATE MIDPOINTS FOR BINARY SEARCH                          00021700
*        -------------------------------------                          00021800
         SPACE 3                                                        00021900
**********************************************************              00022000
* BINARY MIDPOINT CALCULATION FOR LATER SEARCHES OF TABLE*              00022100
**********************************************************              00022200
          SPACE 3                                                       00022300
BINSRCH  DS    0H                                                       00022400
         LA    SSRPTR,SIBLSMPT   POINT TO MIDPOINT TABLE                00022500
         LA    R0,MAXMDPTS       PICK UP TOTAL # OF MDPTS               00022600
         ZAP   DWORD,SIBLSLN      CONVERT THE PACKED ENTRY LENGTH       00022700
         CVB   WORKREG,DWORD       INTO BINARY                          00022800
         STH   WORKREG,ENTRYLEN    AND SAVE FOR MULTIPLY                00022900
MDPTINIT DS    0H                                                       00023000
         MVC   0(L'SIBLSMP1,SSRPTR),ENTRYLEN INIT MDPTS TO ENTRY LEN    00023100
         LA    SSRPTR,L'SIBLSMP1(SSRPTR)    POINT TO NEXT MIDPOINT      00023200
         BCT   R0,MDPTINIT         INITIALIZE UNTIL END OF MIDPOINTS    00023300
         LA    R0,MAXMDPTS         REINIT TO TOTAL MIDPOINTS            00023400
         LA    SSRPTR,SIBLSMPT       AND MIDPOINT TABLE PTR             00023500
         ZAP   DWORD,SIBLSNOE    GET TOTAL ENTRIES IN DOUBLEWORD        00023600
         CVB   WORKREG,DWORD    AND CONVERT TO BINARY                   00023700
         STH   WORKREG,WORKMDPT    SAVE MIDPOINT FOR NEXT OPERATION     00023800
MIDCALC  DS    0H                                                       00023900
         LH    WORKREG,WORKMDPT    PICK UP CURRENT MIDPOINT             00024000
         LA    WORKREG,1(WORKREG)  ADD 1 TO REMAINING ENTRIES           00024100
         SRL   WORKREG,1           DIVIDE NUMBER ENTRIES BY 2           00024200
         CH    WORKREG,BIN1        IS IT ONE                            00024300
         BNH   MIDEND              YES, ALL FINISHED                    00024400
         STH   WORKREG,WORKMDPT     SAVE FOR NEXT HALVING OPERATION     00024500
         MH    WORKREG,ENTRYLEN    MULTIPLY BY ENTRYLEN                 00024600
         STH   WORKREG,0(SSRPTR)                                        00024700
         LA    SSRPTR,L'SIBLSMP1(SSRPTR)   POINT TO NEXT MIDPOINT       00024800
         BCT   R0,MIDCALC          LOOP UNTIL ALL MIDPOINTS INIT'D      00024900
MIDEND   DS    0H                 FINISHED WITH MIDPOINT TABLE          00025000
         SPACE 3                                                        00025100
*        ----------------------------                                   00025200
*        CLEANUP AND RETURN TO CALLER                                   00025300
*        ----------------------------                                   00025400
         SPACE 3                                                        00025500
CLSRET   DS    0H                                                       00025600
*** CLOSE SSR FILE BEFORE RETURNING TO CALLER ***                       00025700
         MVI   IOOPER,C'E'        CLOSE SSR FILE                        00025800
         LA    R1,SIIOPARM                                              00025900
         ST    R9,0(R1)           SAVE PARAMETS FOR CLOSE               00026000
         L     R15,AIORTN          GET IORTN ADDRESS                    00026100
         CALL  (15)               CALL IORTN                            00026200
         LH    R15,IORETCOD       R15 - SET STATUS                      00026300
RETURN   DS    0H                                                       00026400
SEGRET   SIRETRN RC=(15)          PASS CONTROL BACK TO CALLING PGM      00026500
         TITLE 'SISSBLD - MISCELLANEOUS ROUTINES'                       00026600
*                                                                     * 00026700
*********************************************************************** 00026800
*                                                                     * 00026900
* * * *              MISCELLANEOUS ROUTINES                     * * * * 00027000
*                                                                     * 00027100
*********************************************************************** 00027200
*                                                                     * 00027300
*        --------------                                                 00027400
*        UNIQUE ROUTINE                                                 00027500
*        --------------                                                 00027600
         SPACE 3                                                        00027700
*******************************************************************     00027800
*** DETERMINE UNIQUE TABLE ENTRY ROUTINE ***                            00027900
***   INPUT: SIBLTDTE (DETAIL TABLE)                                    00028000
***          SSRPTR  - ADDRESS TO FIRST GROUP NO NEEEDED                00028100
***          WORKREG - USED BY ROUTINE                                  00028200
***          SIBLSNOE - NUMBER ENTRIES FOR THE DETAILS                  00028300
***  OUTPUT: UNIQTBL WITH ENTRIES FILLED IN                             00028400
**** PROCESSING: SSRPTR POINTS TO FIRST SSR TBL GROUP/SEGMENT NUMBER    00028500
***              TO BE VALIDATED.  THIS ROUTINE INSPECTS EACH ENTRY     00028600
***              USING THE GROUP/SEGMENT NUMBER AS AN INDEX INTO THE    00028700
***              UNIQTBL.  IF AN EQUAL IS FOUND, ENTRY IS A DUPLICATE   00028800
***              AND WILL NOT BE PROCESSED.  IF NOT EQUAL, THE VALUE    00028900
***              OF THE GROUP NUMBER WILL BE MOVED INTO THE UNIQTBL     00029000
***              AND THE UNIQTOT COUNTER WILL BE UPDATED.               00029100
***  ROUTINE RETURNS VIA RETRG                                          00029200
*******************************************************************     00029300
UNIQRTN  DS    0H                                                       00029400
         STM   R6,R9,SAVEREGS       BACKUP REGISTERS          0525104   00029525
         LA    R6,UNIQTBL           LOAD ADDRESS OF UNIQTBL   0525104   00029530
         LA    R7,TOTULEN           LOAD LENGTH  OF UNIQTBL   0525104   00029535
         LA    R9,1                 CLEAR THE REG             0525104   00029540
         LA    R8,BIN0              MOVE FROM LOW VALUES      0525104   00029545
         MVCL  R6,R8                MOVE LOW VALUES           0525104   00029550
         LM    R6,R9,SAVEREGS       RESTORE REGISTERS         0525104   00029555
         LA    WORKREG,UNIQENT                                          00029600
         ZAP   UNIQTOT,PKED0      ZERO OUT FIELD FOR LOOP               00029700
         ZAP   DWORD,SIBLSNOE     MOVE IN PACKED TOTAL ENTRIES          00029800
         CVB   R0,DWORD           GET THE BINARY EWQUIVALENT            00029900
UNIQLOOP DS    0H                                                       00030000
         LA    WORKREG,UNIQENT    ADDRESS UNIQUE TABLE        2602266   00030100
UNIQTBLP DS    0H                 LOOP THRU TABLE             2602266   00030200
         CLC   0(L'SIBLDSEG,SSRPTR),0(WORKREG)                2602266   00030300
         BE    NOTUNIQ            NOT UNIQUE, GO TO NEXT ENTRY2602266   00030400
         CLC   0(L'SIBLDSEG,WORKREG),BIN0                     2602266   00030500
         BE    FNDUNIQ                                        2602266   00030600
         LA    WORKREG,L'SIBLDSEG(WORKREG) BUMP TO NEXT ENTRY 2602266   00030700
         C     WORKREG,=A(UNIQEND)         AT END OF TABLE?   2602266   00030800
         BNL   NOTUNIQ                     TABLE FULL BYPASS  2602266   00030900
         B     UNIQTBLP                    CONTINUE SEARCH    2602266   00031000
FNDUNIQ  DS    0H                                             2602266   00031200
         MVC   0(L'SIBLDSEG,WORKREG),0(SSRPTR) UNIQUE - MOVE INTO TBL   00031300
         AP    UNIQTOT,PKED1      INCREMENT UNIQUE TOTAL                00031400
NOTUNIQ  DS    0H                 GO TO NEXT INPUT ENTRY                00031500
         LA    SSRPTR,SIBLDLEN(SSRPTR) INCREMENT TO NEXT ENTRY          00031600
         BCT   R0,UNIQLOOP       CONTINUE FOR EACH SSRTBL ENTRY         00031700
UNIQRET  DS    0H                                                       00031800
         BR    RETRG              RETURN TO CALLER                      00031900
         TITLE 'SISSBLD - ERROR ROUTINES'                               00032000
EMPTYERR DS    0H                 EMPTY FILE OR NO RCDS FOUND FOR APPL  00032100
         MVI   SIBLDRET,SIBNRCRT  INDICATE EMPTY ERROR                  00032200
         B     CLSRET             RETURN TO CALLER                      00032300
TBLFLERR DS    0H                 SET INDICATOR FOR OVERFLOW            00032400
         MVI   SIBLDRET,SIBMSRRT  INDICATE TOO MANY SSR RCDS FOR TABLE  00032500
         B     CLSRET             RETURN TO CALLER                      00032600
MAXSEG   DS    0H                 SET INDICATOR FOR OVERFLOW            00032700
         MVI   SIBLDRET,SIBMSTRT  INDICATE TOO MANY SEG RCDS FOR STACK  00032800
         B     CLSRET             RETURN TO CALLER                      00032900
BADSEG   DS    0H                 INVALID SEGMENT NUMBER FOUND          00033000
         MVI   SIBLDRET,SIBINSRT  INDICATE SEGMENT NBR ERROR            00033100
         B     CLSRET             RETURN TO CALLER                      00033200
OPENERR  DS    0H                 ERROR AT OPEN TIME                    00033300
         MVI   SIBLDRET,SIBOPNRT  INDICATE OPEN ERROR                   00033400
         B     RETURN             RETURN TO CALLER                      00033500
IOERR    DS    0H                                                       00033600
         MVI   SIBLDRET,SIBIOERT  INDICAT IO ERROR                      00033700
         B     CLSRET             RETURN TO CALLER                      00033800
         TITLE 'SISSBLD-STATIC STORAGE+LITERALS,EQUATES,AND CONSTANTS'  00033900
*                                                                     * 00034000
*********************************************************************** 00034100
*                                                                     * 00034200
* * * *    STATIC STORAGE + LITERALS, EQUATES, AND CONSTANTS    * * * * 00034300
*                                                                     * 00034400
*********************************************************************** 00034500
*                                                                     * 00034600
*** STATIC STORAGE ***                                                  00034700
          DC    0D'0'                                                   00034800
WKSTGSEG  DC    CL12'**WKSTGSEG**'                                      00034900
PGMID     DC    CL8'SISSBLD'                                            00035000
          SPACE 3                                                       00035100
SSRPTR    EQU   6                                                       00035200
WORKREG   EQU   7                                                       00035300
BASEREG   EQU   12                                                      00035400
RETRG     EQU   14                                                      00035500
MAXMDPTS  EQU   10          MAXIMUM MIDPOINTS FOR LOOP CTL              00035600
SPACE     EQU   C' '                                                    00035700
PKED0     DC    PL1'0'                                                  00036200
PKED1     DC    PL1'1'                                                  00036300
BIN0      DC    H'0'                                                    00036400
BIN1      DC    H'1'                                                    00036500
         TITLE 'SISSBLD - GENERATED LITERALS, EQUATES, AND CONSTANTS'   00036600
*                                                                     * 00036700
*********************************************************************** 00036800
*                                                                     * 00036900
* * * *          GENERATED LITERALS, EQUATES, AND CONSTANTS     * * * * 00037000
*                                                                     * 00037100
*********************************************************************** 00037200
*                                                                     * 00037300
          LTORG                                                         00037400
         TITLE 'SISSBLD'  - DYNAMIC STORAGE'                            00037500
*                                                                     * 00037600
*********************************************************************** 00037700
*                                                                     * 00037800
* * * *                  DYNAMIC STORAGE                        * * * * 00037900
*                                                                     * 00038000
*********************************************************************** 00038100
*                                                                     * 00038200
*** THESE AREAS COULD BE MOVED TO GETMAINED STORAGE TO ALLOW            00038300
*** THIS PROGRAM TO BE REENTRANT                                        00038400
DWORD     DC    D'0'        USED FOR BINARY TO DECIMAL CONVERSIONS      00038500
ORIG1     DC    F'0'        REGISTER SAVEAREA                           00038600
SAVEREGS  DC    4F'0'       REGISTER SAVEAREA                 0525104   00038650
AIORTN    DC    A(0)        AREA FOR IORTN ADDRESS                      00038700
SIIOPARM  DC    2F'0'       USED FOR PARAMETER PASSING TO IORTN         00038800
WORKMDPT  DC    H'0'        MIDPOINT WORKAREA                           00038900
ENTRYLEN  DC    H'0'        SSR ENTRY LENGTH                            00039000
SIIOREQ   DC    12C' '      AREA FOR IO REQUEST LIST                    00039100
SSRREC    DC    CL80' '     AREA FOR READ OF SSRREC                     00039200
STKNUMRT  DC    PL2'0'      NUMBER RETURNED FOR STACK                   00039300
TSSSRMV   DC    CL8'        '  I/O MODULE NAME TO CALL FOR SSR ACCESS   00039400
SPACES    DC    CL2'  '                                                 00039500
HEXFFS    DC    3XL1'FF'    HIGH VALUES                                 00039550
MAXTBNTS  DC    PL2'0'      MAX NO OF ENTRIES IN SSR TABLE    0525104   00039560
MAXSEGCT  DC    PL2'0'      MAX NO OF SEGMENTS FOR THE STACK  0525104   00039565
          SPACE 3                                                       00039600
*** TABLE FOR UNIQUE NUMBER CALCULATIONS KEEP TOGETHER ***              00039700
UNIQTBL   DS    0C                                                      00039800
UNIQTOT   DC    PL2'0'      TOTAL UNIQUE ENTRIES                        00039900
UNIQENT   DC    201XL2'0000'   201 ENTRIES                    0525104   00040000
UNIQEND  EQU    *             END OF UNIQUE TABLE             2602266   00040100
TOTULEN   EQU   *-UNIQTBL     TOTAL TABLE LENGTH FOR CLEARING OUT       00040200
*** END OF UNIQUE TABLE - ABOVE STORAGE SHOULD REMAIN TOGETHER          00040300
SISSBLD  CSECT                                                          00040396
         LTORG                                                          00040397
         DS    0D                                                       00040398
SITMSTMP DC    CL64'SISSBLD   -----TSD-             11/12/04  14.16.14' 00040399
*        THIS PROGRAM CONTAINS TRADE SECRETS THAT BELONG                00040400
*        TO FIDELITY INFORMATION SERVICES AND IS                        00040401
*        LICENSED BY AN AGREEMENT.  ANY UNAUTHORIZED ACCESS,            00040402
*        USE, DUPLICATION, OR DISCLOSURE IS UNLAWFUL.                   00040403
*        COPYRIGHT FIDELITY INFORMATION SERVICES                        00040404
*        2004, ALL RIGHTS RESERVED.                                     00040405
         END                                                            00040600
