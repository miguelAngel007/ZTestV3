*     * 802262 12/11/20 PROYECTO UPGRADE GN172 EDVR
NUC      TITLE 'SADSNUC  - DATA DICTIONARY NUCLEUS - BATCH VERSION'     00001000
*********************************************************************** 00002000
* THIS ROUTINE DOES COMMAND PROCESSING FOR THE DATA DICTIONARY SYSTEM.* 00003000
* REGISTER ONE POINTS TO THE PARAMETER ADDRESS LIST.                  * 00004000
*          1ST ADDRESS IS THE COMMAND TO BE PERFORMED.                * 00005000
*          2ND ADDRESS IS THE INPUT OUTPUT AREA.                      * 00006000
*          3RD ADDRESS IS THE FEEDBACK AREA.                          * 00007000
* REGISTER THIRTEEN POINTS TO THE REGISTER SAVE AREA ADDRESS.         * 00008000
* REGISTER FOURTEEN IS THE RETURN ADDRESS.                            * 00009000
* REGISTER FIFTEEN IS THE ENTRY POINT ADDRESS FOR DIDICMDS.           * 00010000
*                                                                     * 00011000
* THE COMMAND IS PARSED AND THEN EXAMINED FOR VALID OPERANDS.         * 00012000
* VARIOUS SUBROUTINES ARE CALLED BASED UPON THE COMMAND BEING         * 00013000
* PROCESSED.                                                          * 00014000
*********************************************************************** 00015000
*                     HISTORY OF REVISIONS                            * 00015100
* DESCRIPTION                                                 CHNGID  * 00015200
* ----------------------------------------------------------  --------* 00015300
* 03/03/00 MODIFY FOR 31-BIT                                  2024183 * 00015988
* 12/18/96 BIG-DAG SUPPORT / REPOSITORY MULTIPLE INDEXES      9913569 * 00015990
* 09/25/96 CORRECT IFLAG MASK FOR 88-LEVELS & NESTED DAGS     2602105 * 00015992
* 03/01/95 MOD FOR AUDIT FACILITY & GN26 FILE FORMAT LEVEL    2602105 * 00015994
*                                                                     * 00015996
**********************************************************************  00015998
         GBLB  &NOREG                                                   00016000
         COPY  SIOPTNS                                                  00017000
&NOREG   SETB  0                                                        00018000
         EJECT                                                          00019000
         TITLE 'SADSCOMM - DATA DICTIONARY COMMUNICATION AREA'          00020000
         COPY  SADSCOMA                                                 00021000
         EJECT                                                          00022000
         COPY  SADSCMAA                                                 00023000
         EJECT                                                          00024000
         SAD9AXCA DSECT=YES                                             00025000
         EJECT                                                          00026000
DSLOCAL  DSECT                                                          00027000
         COPY  SADSLOCA                                                 00028000
         TITLE 'SADSNUC  - DATA DICTIONARY NUCLEUS'                     00029000
*********************************************************************** 00030000
*        INITIALIZE DICTIONARY COMMON AREA                            * 00031000
*********************************************************************** 00032000
         USING SADSNUCB,R2        TELL ASSEMBLER OF BASE REGISTER       00033000
         USING DSCMAREA,R3        DICTIONARY COMMUNICATION AREA         00034000
SADSNUCB CSECT                                                          00035000
SADSNUCB AMODE 31                                             2024183   00035010
SADSNUCB RMODE 24                                             2024183   00035020
         STM   R14,R12,12(R13)    SAVE CALLERS REGISTERS                00036000
         LR    R2,R15             SET BASE REGISTER                     00037000
         B     DINUCBEG           BRANCH AROUND DATE / TIME STAMPS      00038000
         DC    C'SYSTEMATICS, INC. DATA DICTIONARY SYSTEM  COPYRIGHT '  00039000
         DC    C'1986 VER.RLS.LVL 0.00.00'                              00040000
         AIF   (&VSE).ML00000                                           00041000
         DC    C'COMPILED &SYSDATE &SYSTIME'                            00042000
.ML00000 ANOP                                                           00043000
DINUCBEG DS    0H                                                       00044000
         LM    R3,R5,0(R1)        LOAD PARAMETER    ADDRESSES           00045000
         ST    R13,DICMREGS+4     SAVE CALLERS REG 13                   00046000
         LA    R15,DICMREGS       GET DICTIONARY REGISTER SAVE AREA     00047000
         ST    R15,8(R13)         CHAIN SAVE AREA ADDRESSES             00048000
         LR    R13,R15            SET NEW SAVEAREA ADDRESS              00049000
         MVC   DIOPERID,=C'BATCHJOB'  MY SESSION ID IS BATCHJOB         00050000
         XC    DICMCTRL,DICMCTRL  CLEAR DICTIONARY CONTROL AREA         00051000
         CLC   DICMDDN,=8C' '    REP DD NAME SPECIFIED ?                00052000
         BE    DINRPDFL           NO - USE DEFAULT                      00053000
         CLC   DICMDDN,=8X'00'   REP DD NAME SPECIFIED ?                00054000
         BE    DINRPDFL           NO - USE DEFAULT                      00055000
         B     DINAXCHK           CHECK AUX                             00056000
DINRPDFL DS    0H                                                       00057000
         MVC   DICMDDN,=CL8'SAREP' DEFAULT NAME OF DICTIONARY DD        00058000
DINAXCHK DS    0H                                                       00059000
         CLC   DICMAUX,=8C' '    REP DD NAME SPECIFIED ?                00060000
         BE    DINAXDFL           NO - USE DEFAULT                      00061000
         CLC   DICMAUX,=8X'00'   REP DD NAME SPECIFIED ?                00062000
         BE    DINAXDFL           NO - USE DEFAULT                      00063000
         B     DINUCCRS           CHECK READ ONLY OPEN REQUEST          00064000
DINAXDFL DS    0H                                                       00065000
         MVC   DICMAUX,=CL8'SAAUX' DEFAULT NAME OF AUX DICTIONARY       00066000
DINUCCRS DS    0H                                                       00067000
         CLC   0(5,R4),=C'OPENR'  IS THIS REQUEST FOR READ ONLY OPEN?   00068000
         BE    DINUCROS           YES - THEN SET ACB TO READ ONLY       00069000
*********************************************************************** 00070000
*        PARSE COMMAND LINE                                           * 00071000
*********************************************************************** 00072000
         XC    DICMSTAT,DICMSTAT  CLEAR ALL ERRORS                      00073000
         L     R15,=A(DSDIPARS)   GET DEFINITION PROGRAM ADDRESS        00074000
         BASR  R14,R15            PROCESS DEFINITIONS         2024183   00075000
         ICM   R15,15,DICMSTAT       GET STATUS                         00076000
         LTR   R15,R15            WAS COMPLETION GOOD?                  00077000
         BNZ   DINUCERR           NO - RETURN WITH ERROR                00078000
*********************************************************************** 00079000
*        PROCESS COMMAND                                              * 00080000
*********************************************************************** 00081000
DINUDO   EQU   *                                                        00082000
         CLC   DICMCMD,=C'END   ' IS COMMAND TO CLOSE DICTIONARY?       00083000
         BE    DINUCLO            YES - GO CLOSE DICTIONARY             00084000
         CLC   DICMCMD,=C'AUXIO ' IS THIS AN AUXIO REQUEST?             00085000
         BE    DINUAXIO           YES - GO TO AUXILIARY IO              00086000
         L     R15,=A(DSDIDEFS)   PROCESS DEFINITIONS                   00087000
         BASR  R14,R15            GO PERFORM COMMAND          2024183   00088000
         ICM   R15,15,DICMSTAT       GET STATUS                         00089000
         LTR   R15,R15            WAS COMPLETION GOOD?                  00090000
         BNZ   DINUCERR           NO - RETURN WITH ERROR                00091000
         CLC   DICMCMD,=C'MASTER' IS THIS A MASTER REQUEST?             00092000
         BE    DINUMSIO           YES - GO TO MASTER IO                 00093000
         CLC   DICMCMD,=C'CREATE' IS COMMAND FOR CREATE?                00094000
         BE    DINURET            YES - COMMAND SHOULD BE COMPLETE      00095000
         CLC   DICMCMD,=C'DROP  ' IS COMMAND FOR DROP  ?                00096000
         BE    DINURET            YES - COMMAND SHOULD BE COMPLETE      00097000
         B     DINUCMD            GO DO COMMAND                         00098000
DINUCLO  EQU   *                                                        00099000
         TM    DICMDTS,DICMDOP    IS DICTIONARY OPEN?                   00100000
         BNO   DINUCAX            NO - CHECK AUXILIARY                  00101000
         MVI   DICMEXT,DICMCLOS   SET CLOSE REPOSITORY FUNCTION         00102000
         L     R15,=A(DSDIEXT)    PREPARE TO CLOSE DICTIONARY           00103000
         BASR  R14,R15            GO PERFORM COMMAND          2024183   00104000
DINUCAX  EQU   *                                                        00105000
         TM    DICMDTS,DICMAUXO   IS DICTIONARY OPEN?                   00106000
         BNO   DINURET            NO - RETURN TO CALLER                 00107000
         MVI   DICMEXT,DICMACLO   SET CLOSE AUXILIARY FUNCTION          00108000
         L     R15,=A(DSDIEXT)    PREPARE TO CLOSE DICTIONARY           00109000
         BASR  R14,R15            GO PERFORM COMMAND          2024183   00110000
         NI    DICMDTS,255-DICMAUXO SET STATUS: AUXILIARY CLOSED        00111000
         B     DINURET            RETURN TO CALLER                      00112000
DINUCMD  EQU   *                                                        00113000
         L     R15,=A(DSDIENT)    PREPARE TO PERFORM COMMAND            00114000
         BASR  R14,R15            GO PERFORM COMMAND          2024183   00115000
         ICM   R15,15,DICMSTAT       GET STATUS                         00116000
         LTR   R15,R15            WAS COMPLETION GOOD?                  00117000
         BNZ   DINUCERR           NO - RETURN WITH ERROR                00118000
         B     DINURET            RETURN TO CALLER                      00119000
*********************************************************************** 00120000
*        SERVICE REQUEST MASTER I/O                                   * 00121000
*********************************************************************** 00122000
DINUMSIO EQU   *                                                        00123000
********* MASTER FUNCTION HAS BEEN PLUGGED BY PARSING ROUTINE           00124000
         CLI   DICMEXT,DICMWRIT   IS IT A MASTER INSERT                 00125000
         BNE   DINUMSI1                                                 00126000
         MVC   DICMCMD,=C'INSERT'                                       00127000
DINUMSI1 EQU *                                                          00128000
         CLI   DICMEXT,DICMUPDT   IS IT A MASTER UPDATE                 00129000
         BNE   DINUMSI2                                                 00130000
         MVC   DICMCMD,=C'UPDATE'                                       00131000
DINUMSI2 EQU *                                                          00132000
         CLI   DICMEXT,DICMDELE   IS IT A MASTER DELETE                 00133000
         BNE   DINUMSI3                                                 00134000
         MVC   DICMCMD,=C'DELETE'                                       00135000
DINUMSI3 EQU *                                                          00136000
         LA    R0,SAXF#ENT         R0  = #ENTRIES/REP-AUX XRF 2602105   00136050
         L     R1,=A(SAXFBEG)      R1  = A(1ST XREF TBL ENTRY)2602105   00136100
DINUMSI4 DS   0H                                              2602105   00136150
         CLC   DICMKFRM,(SAXFRTYP-SAXFBEG)(R1) REP ENT TYPE?  2602105   00136200
         BE    *+4+4+4               Y. SKIP TO CK ENT ARCH   2602105   00136250
         LA    R1,SAXFELEN(,R1)      N. SET R1=NXT REPENT TYP 2602105   00136300
         BCT   R0,DINUMSI4           N. LOOP TO FIND ENTTYPE  2602105   00136350
*+4+4+4  EQU   *                                              2602105   00136400
         CLI   (SAXFRETY-SAXFBEG)(R1),SAXFHIER IS ENT HIER    2602105   00136450
         BNE   DINUMSI5              N. LEAVE FLD NAME AS IS  2602105   00136500
         CLC   DICMKNDX,=F'0'      IS READ FOR ROOT           2602105   00136550
         BE    DINUMSI5              Y. LEAVE KEY AS IS       2602105   00136600
         MVC   DICMKFLD,=CL8'ITEM' SET ITEM EYECATCHER IN KEY 2602105   00136650
DINUMSI5 DS   0H                                              2602105   00136700
         MVC   2(L'DICMKEY,R5),DICMKEY MAKE SURE KEY IS IN BUFFER       00137000
         L     R15,=A(DSDIEXT)    GET SERVICE MODULE ADDRESS            00138000
         BASR  R14,R15            GO PERFORM COMMAND          2024183   00139000
         ICM   R15,15,DICMSTAT       GET STATUS                         00140000
         LTR   R15,R15            WAS COMPLETION GOOD?                  00141000
         BNZ   DINUCERR           NO - RETURN WITH ERROR                00142000
         ICM   R15,15,DICMNACC    GET NUMBER OF ACCESSES THIS REQUEST   00143000
         LA    R15,1(R15)         INCREMENT BY ONE                      00144000
         STCM  R15,15,DICMNACC    RESTORE NUMBER OF ACCESSES            00145000
         B     DINURET            RETURN TO CALLER                      00146000
*********************************************************************** 00147000
*        SERVICE REQUEST AUXILIARY I/O                                * 00148000
*********************************************************************** 00149000
DINUAXIO EQU   *                                                        00150000
********* AUXILIARY FUNCTION HAS BEEN PLUGGED BY PARSING ROUTINE        00151000
         L     R15,=A(DSDIEXT)    GET SERVICE MODULE ADDRESS            00152000
         BASR  R14,R15            GO PERFORM COMMAND          2024183   00153000
         ICM   R15,15,DICMSTAT       GET STATUS                         00154000
         LTR   R15,R15            WAS COMPLETION GOOD?                  00155000
         BNZ   DINUCERR           NO - RETURN WITH ERROR                00156000
         ICM   R15,15,DICMNACC    GET NUMBER OF ACCESSES THIS REQUEST   00157000
         LA    R15,1(R15)         INCREMENT BY ONE                      00158000
         STCM  R15,15,DICMNACC    RESTORE NUMBER OF ACCESSES            00159000
         B     DINURET            RETURN TO CALLER                      00160000
*********************************************************************** 00161000
*        SERVICE REQUEST READ ONLY OPEN FOR REPOSITORY                * 00162000
*********************************************************************** 00163000
DINUCROS EQU   *                                                        00164000
         MVI   DICMEXT,X'00'      SET READ ONLY REQUEST                 00165000
         L     R15,=A(DSDIEXT)    GET SERVICE MODULE ADDRESS            00166000
         BASR  R14,R15            GO PERFORM COMMAND          2024183   00167000
         ICM   R15,15,DICMSTAT       GET STATUS                         00168000
         LTR   R15,R15            WAS COMPLETION GOOD?                  00169000
         BNZ   DINUCERR           NO - RETURN WITH ERROR                00170000
         B     DINURET            RETURN TO CALLER                      00171000
*********************************************************************** 00172000
*        RETURN WITH ERROR                                            * 00173000
*********************************************************************** 00174000
DINUCERR EQU   *                                                        00175000
         L     R15,=A(DSDIMSG)    GET MESSAGE PROCESSOR ADDRESS         00176000
         BASR  R14,R15            PROCESS ERROR MESSAGE       2024183   00177000
         B     DINURET            RETURN TO CALLER                      00178000
*********************************************************************** 00179000
*        RETURN TO CALLER                                             * 00180000
*********************************************************************** 00181000
DINURET  EQU   *                                                        00182000
*        MVI   DICMEXT,DICMFLUS   SET FLUSH REPOSITORY FUNCTION         00183000
*        L     R15,=A(DSDIEXT)    PREPARE TO FLUSH BUFFERS              00184000
*        BASR  R14,R15            GO PERFORM COMMAND          2024183   00185000
         L     R13,4(R13)         RESTORE CALLERS REGISTER SAVE AREA    00186000
         XR    R1,R1              CLEAR WORK REGISTER                   00187000
         ICM   R1,3,DICMSTAT      GET DICTIONARY RETURN CODE            00188000
         STCM  R1,15,16(R13)      SET RETURN CODE IN CALLERS SAVE AREA  00189000
         LM    R14,R12,12(R13)    RESTORE CALLERS REGISTERS             00190000
         BSM   0,R14              RETURN TO CALLER            2024183   00191000
         TITLE 'LTORG    - NUCLEUS LITERAL POOL'                        00192000
         SAD9AXRF , DICTIONARY ENTITY XREF TABLE              2602105   00192100
         LTORG                                                          00193000
         TITLE 'DSDIEXT  - DATA DICTIONARY EXTERNAL ROUTINE - BATCH'    00194000
*********************************************************************** 00195000
* DSDIEXT RTN DOES ALL EXTERNAL REQUESTS FOR THE DATA DICTIONARY.     * 00196000
* REGISTER USAGE:                                                     * 00197000
*   REG 01 POINTS TO THE PARAMETER ADDRESS LIST.                      * 00198000
*   REG 03 POINTS TO THE DICTIONARY COMMUNICATION AREA ADDRESS.       * 00199000
*   REG 05 POINTS TO THE INPUT OUTPUT AREA.                           * 00200000
*   REG 13 POINTS TO THE REGISTER SAVE AREA ADDRESS.                  * 00201000
*   REG 14 IS THE RETURN ADDRESS.                                     * 00202000
*   REG 15 IS THE ENTRY POINT ADDRESS FOR DIDIEXT.                    * 00203000
*                                                                     * 00204000
* CURRENTLY, ONLY INPUT/OUTPUT REQUESTS ARE HANDLED BY THIS ROUTINE.  * 00205000
*********************************************************************** 00206000
         PRINT GEN                                                      00207000
DSDIEXT  CSECT                                                          00208000
DSDIEXT  AMODE 31                                             2024183   00208001
DSDIEXT  RMODE 24                                             2024183   00208002
         STM   R14,R12,12(R13)    SAVE CALLERS REGISTERS                00209000
         LR    R2,R15             SET BASE REGISTER                     00210000
         USING DSDIEXT,R2         TELL ASSEMBLER OF BASE REGISTER       00211000
         ST    R13,DICMEREG+4     SAVE CALLERS REGISTER 13              00212000
         LA    R15,DICMEREG       GET EXTERNAL REGISTER SAVE ADDRESS    00213000
         ST    R15,8(R13)         CHAIN IN NEW SAVEAREA                 00214000
         LR    R13,R15            SET NEW REG SAVE AREA                 00215000
         L     R15,=A(DILBCHK)    LOAD THE ADDRESS OF LB ROUTINES       00216000
         ST    R15,DILBADDR       SAVE THE ADDRESS                      00217000
         CLI   DICMEXT,DICMFLUS   IS IT A FLUSH                         00218000
         BE    DIEXCNT            YES, DO NOT CLEAR THE ERROR           00219000
         TM    DICMEXFL,DICMBTAB  IS IT THE FIRST TIME THRU             00220000
         BZ    DIEXCNT            NO, DO NOT CLEAR THE ERROR            00221000
         XC    DICMSTAT,DICMSTAT  CLEAR ALL ERRORS                      00222000
*********************************************************************** 00223000
*        DECODE REQUEST                                               * 00224000
*********************************************************************** 00225000
DIEXCNT  EQU   *                                                        00226000
         BAS   R10,DIRPLOC       LOCATE CORRECT RPL TO USE    2024183   00227000
         XR    R7,R7              CLEAR WORK REGISTER         2602105   00227100
         IC    R7,DICMEXT         GET REQUEST CODE            2602105   00227200
         CLC   DICMDAPL(2,5),=C'$$'  IS THIS A DEFINITION RECORD        00228000
         BE    DIEXTABD(R7)         Y. PROCESS OLD STYLE      2602105   00229000
         CLC   DICMDFRM(3,5),=C'ALI' IS THIS AN ALIAS RECORD            00230000
         BE    DIEXTABD(R7)         Y. PROCESS OLD STYLE      2602105   00231000
         B     DIEXTAB(R7)        GO DO REQUESTED FUNCTION              00234000
DIEXTAB  EQU   *                                                        00239000
         B     DIEXROS            READ ONLY OPEN FOR REPOSITORY         00240000
         B     DIEXBRW            BROWSE FUNCTION                       00241000
         B     DIEXREA            READ FUNCTION                         00242000
         B     DIEXWRI            WRITE FUNCTION                        00243000
         B     DIEXRWI            RE-WRITE FUNCTION                     00244000
         B     DIEXDEL            DELETE FUNCTION                       00245000
         B     DIEXUPD            READ FOR UPDATE FUNCTION              00246000
         B     DIEXCLS            CLOSE DICTIONARY                      00247000
         B     DIEXFLS            FLUSH LOCAL BUFFERS                   00248000
         B     DIAUXIO            AUXILIARY READ                        00249000
         B     DIAUXIO            AUXILIARY WRITE                       00250000
         B     DIAUXIO            AUXILIARY DELETE                      00251000
         B     DIAUXIO            AUXILIARY CLOSE                       00252000
         B     DIAUXIO            AUXILIARY BROWSE                      00253000
         B     DIEXERR            READ LOCATE REPOSITORY(ERROR)         00254000
DIEXTABD EQU   *                                                        00255000
         B     DIEXROS            READ ONLY OPEN FOR REPOSITORY         00256000
         B     DIEXBRWD           BROWSE FUNCTION                       00257000
         B     DIEXREAD           READ FUNCTION                         00258000
         B     DIEXWRID           WRITE FUNCTION                        00259000
         B     DIEXRWID           RE-WRITE FUNCTION                     00260000
         B     DIEXDELD           DELETE FUNCTION                       00261000
         B     DIEXUPDD           READ FOR UPDATE FUNCTION              00262000
         B     DIEXCLS            CLOSE DICTIONARY                      00263000
         B     DIEXFLS            FLUSH LOCAL BUFFERS                   00264000
         B     DIAUXIO            AUXILIARY READ                        00265000
         B     DIAUXIO            AUXILIARY WRITE                       00266000
         B     DIAUXIO            AUXILIARY DELETE                      00267000
         B     DIAUXIO            AUXILIARY CLOSE                       00268000
         B     DIAUXIO            AUXILIARY BROWSE                      00269000
         B     DIEXREL            READ LOCATE REPOSITORY                00270000
         EJECT                                                          00271000
*********************************************************************** 00272000
*  AS OF SA0004, THE NUCLEUS HAS BEEN MODIFIED TO CALL SADAUXIB       * 00273000
*  FOR ALL AUXILIARY I/O REQUESTS.  DIAUXIO IS REPLACEMENT FOR ALL    * 00274000
*  PREVIOUS AUXILIARY INTERFACE CODE.  CHANGE ID:  0040-0290          * 00275000
*  TRANSLATION OF REQUEST CODES IS AS FOLLOWS:                        * 00276000
*      EXTERNAL         SADAUXIB (SAD9AXCA: SADPRFUN)                 * 00277000
*      ---------        -----------------------------                 * 00278000
*      AUXIO GET          04  (GET MOVE)                              * 00279000
*      AUXIO PUT          10  (PUT MOVE)                              * 00280000
*      AUXIO DEL          11                                          * 00281000
*      AUXIO CLO          01                                          * 00282000
*      AUXIO BRO          06  (GET MOVE/ SEG 1 PAGE 1 ONLY)           * 00283000
*                                                                     * 00284000
*  DIAUXIO WILL INITIALIZE AN SAD9AXCA CONTROL BLOCK FOR THE CALL TO  * 00285000
*  SADAUXIB, CALL SADAUXIB, AND INTERPRET ERRORS.  DATA KEY MAY BE    * 00286000
*  EITHER LOGICAL OR PHYSICAL, AS PASSED TO SADSNUCB. ERROR CODES WILL* 00287000
*  BE THE BASIC VSAM CODES (NOT FOUND, ETC.) OR SDMS ERROR CODES.     * 00288000
*                                                                     * 00289000
*                                                                     * 00290000
*********************************************************************** 00291000
         SPACE                                                          00292000
DIAUXIO  DS    0H                                                       00293000
         XC    DICMSTAT,DICMSTAT  CLEAR STATUS CODE                     00294000
         LA    R1,DICMWORK+4      R1 = A(WORKAREA FOR SAD9AXCA REQ BLK) 00295000
         ST    R1,DICMWORK         SAVE A(WKAREA FOR SAD9AXCA REQ BLK)  00296000
         OI    DICMWORK,X'80'      PASS ONLY ONE ADDRESS IN PARMLIST    00297000
         LA    R1,DICMWORK         R1 = A(SADAUXIB PARMLIST)            00298000
         USING SAD9AXCA-4,R1                                            00299000
         MVI   SAD9AXCA,0         CLEAR SAD9AXCA REQUEST BLOCK          00300000
         MVC   SAD9AXCA+1(SADPRBLN-1),SAD9AXCA                          00301000
         MVC   SADPREYE,=CL8'SAD9AXCD' INIT SAD9AXCA: EYEBALL 9913569   00301100
         MVC   SADPRRBL,=AL2(SADPRLVL) INIT SAD9AXCA: RB LEVL 9913569   00301200
         LA    R0,DICMAREA         R0 = BR INDEX FOR 1ST AUX REQ RTN    00302000
         SR    R7,R0               R7 = BR INDEX FOR AUX REQ REL TO 1ST 00303000
         SRL   R7,2                INDEX FOR SADAUXIB REQ TYPE          00304000
         LA    R14,=HL1'4,10,11,1,6' R14 = A(SADAUXIB REQ TYPES)        00305000
         LA    R14,0(R7,R14)       R14 = A(SADAUXIB REQ TYPE SPECIFIED) 00306000
         MVC   SADPRFUN,0(R14)     INIT SAD9AXCA: REQ TYPE              00307000
         MVC   SADPRDD,DICMAUX     INIT SAD9AXCA: AUX DD/DLBL NAME      00308000
         MVC   SADPRKEY,DICPKEY    INIT SAD9AXCA: AUX ENT LOGICAL KEY   00309000
         CLI   SADPRFUN,SADPRWRI   IS THIS AN AUX WRITE REQUEST         00310000
         BNE   *+4+6                 N. NO NEED TO INIT AUX ENTITY LENG 00311000
         MVC   SADPRLEN,DIAUDLEN(R5) Y. INIT AUX ENTITY LENGTH          00312000
*+4+6    EQU   *                                                        00313000
         STCM  R5,B'1111',SADPRADR INIT SAD9AXCA: AUX ENT BUFFER AREA   00314000
         ICM   R15,B'1111',DI@AUXIB FETCH A(SADAUXIB)                   00315000
         BNZ   DIAUXIOC            SKIP SADAUXIB LOAD IF ALREADY DONE   00316000
         SACALL SADAUXIB,,TYPE=LOAD LOAD SADAUXIB IF NOT DONE YET       00317000
         LR    R15,R1              R15 = A(SADAUXIB)                    00318000
         STCM  R15,B'1111',DI@AUXIB SAVE A(SADAUXIB)                    00319000
         LA    R1,DICMWORK         R1 = A(SADAUXIB PARMLIST) - RESET    00320000
DIAUXIOC DS   0H                                                        00321000
* THE FOLLOWING STATEMENT(S) MODIFIED BY                      2024183   00321999
         BASR  R14,R15             CALL SADAUXIB TO SERVICE REQUEST     00322000
         MVC   DICMSTAT,SADPRST    SET REQUEST STATUS IN DICCAREA       00323000
         ICM   R0,B'1111',DICMSTAT WAS REQUEST SATISFIED W/O ERRORS     00324000
         BNZ   *+4+4                 N. LEAVE STATUS AS IS              00325000
         OI    DICMDTS,DICMAUXO      Y. SET STATUS FLAG: AUX OPEN       00326000
*+4+4    EQU   *                                                        00327000
         XC    DICMRECL,DICMRECL   CLEAR RECORD LENGTH                  00328000
         MVC   DICMRECL,SADPRLEN SET RETURN RECORD LENGTH     9913569   00329000
         ST    R15,DICMSV15      SAVE R15 FOR DEBUGGING                 00330000
         L     R5,12+(7*4)(,R13) R5 = A(ORIGINAL AUX RECORD)            00331000
         B     DIEXITF            RETURN TO CALLER                      00332000
         DROP  R1                                                       00333000
         LTORG                                                          00334000
         EJECT                                                          00335000
*********************************************************************** 00336000
*        BROWSE REQUEST - ACTUALLY A GREATER THAN OR EQUAL READ       * 00337000
*********************************************************************** 00338000
DIEXBRW  EQU   *                                                        00339000
         OI    DICMDTS,DICMDBR    TURN BROWSE MODE ON                   00340000
* THE FOLLOWING STATEMENT(S) MODIFIED BY                      2024183   00340999
         BAS   R10,DIEXOPN        CHECK DICTIONARY OPEN STATUS          00341000
         LA    R1,DILBKGE         SET R1 TO DO KGE READ                 00342000
         L     R15,=A(DILBCHK)    IZT IN THE LOCAL BUFFER               00343000
         BASR  R10,R15                                        2024183   00344000
         BE    DIEXREA1           YES, DO NOT READ                      00345000
         L     R15,=A(DILBREA)    NO, READ THE CORRECT ONE              00346000
         BASR  R10,R15                                        2024183   00347000
DIEXBRW1 EQU *                                                          00348000
         L     R15,=A(DILBHGH)    IS THE FIELD NUMBER TO HIGH           00349000
         BASR  R10,R15                                        2024183   00350000
         BE    DIEXREAE                                                 00351000
         L     R15,=A(DILBFLD)    GO FIND THE FIELD NUMBER              00352000
         BASR  R10,R15                                        2024183   00353000
         L     R15,=A(DILBMVU)    MOVE TO USERS BUFFER                  00354000
         BASR  R10,R15                                        2024183   00355000
         NI    DICMEXFL,255-DICMBTAB  NO LONGER FIRST, TURN OFF FLAG    00356000
         B     DIEXEND            RETURN TO CALLER                      00357000
* BROWSE REQUEST - FOR DEFINITION RECORD                                00358000
DIEXBRWD EQU   *                                                        00359000
         OI    DICMDTS,DICMDBR    TURN BROWSE MODE ON                   00360000
         LA    R4,2000            SET RECORD LENGTH                     00361000
* THE FOLLOWING STATEMENT(S) MODIFIED BY                      2024183   00361999
         BAS   R10,DIEXOPN        CHECK DICTIONARY OPEN STATUS          00362000
         ICM   R11,15,DIMODPTR                                          00363000
* THE FOLLOWING STATEMENT(S) MODIFIED BY                      2024183   00363999
         MODCB RPL=(6),RECLEN=(4),OPTCD=(KGE),AREA=(S,2(5)),           X00364000
               ARG=(S,DICMKEY),MF=(G,(11))                              00365000
         LR    R1,R6              SAVE RPL ADDRESS                      00366000
         LTR   R15,R15            ANY ERRORS?                           00367000
         BNZ   DIEVERR            YES - TRANSLATE ERRORS                00368000
         GET   RPL=(6)                                                  00369000
         LTR   R15,R15            ANY ERRORS?                           00370000
         BNZ   DIEVERR            YES - TRANSLATE ERRORS                00371000
         B     DIEXENDX           RETURN TO CALLER                      00372000
*********************************************************************** 00373000
*        READ   REQUEST                                               * 00374000
*********************************************************************** 00375000
DIEXREA  EQU   *                                                        00376000
         LA    R4,2000            SET RECORD LENGTH                     00377000
* THE FOLLOWING STATEMENT(S) MODIFIED BY                      2024183   00377999
         BAS   R10,DIEXOPN        CHECK DICTIONARY OPEN STATUS          00378000
         CLC   DICMDFLD(6,5),=C'HINDEX'  IZT HINDEX                     00379000
         BE    DIEXRDH            YES, GO READ HINDEX                   00380000
         LA    R1,DILBKEQ         SET R1 TO DO KEQ READ                 00381000
         L     R15,=A(DILBCHK)    IZT IN THE LOCAL BUFFER               00382000
         BASR  R10,R15                                        2024183   00383000
         BE    DIEXREA1           YES, DO NOT READ                      00384000
         L     R15,=A(DILBREA)    NO, READ THE CORRECT ONE              00385000
         BASR  R10,R15                                        2024183   00386000
DIEXREA1 EQU *                                                          00387000
         L     R15,=A(DILBHGH)    IS THE FIELD NUMBER TO HIGH           00388000
         BASR  R10,R15                                        2024183   00389000
         BE    DIEXREAE                                                 00390000
         L     R15,=A(DILBFLD)    GO FIND THE FIELD NUMBER              00391000
         BASR  R10,R15                                        2024183   00392000
         L     R15,=A(DILBMVU)    MOVE TO USERS BUFFER                  00393000
         BASR  R10,R15                                        2024183   00394000
         NI    DICMEXFL,255-DICMBTAB  NO LONGER FIRST, TURN OFF FLAG    00395000
         B     DIEXEND            RETURN TO CALLER                      00396000
DIEXREAE EQU *                                                          00397000
         LA    R1,L'SAREPKEY      LENGTH IS KEY LENGTH ONLY   2602105   00398000
         STCM  R1,3,0(5)          SAVE THE LENGTH                       00399000
         B     DIEXEND            RETURN TO CALLER                      00400000
* PROCESS REQUEST FOR HINDEX                                            00401000
DIEXRDH  EQU *                                                          00402000
         LA    R15,1                                                    00403000
         STCM  R15,3,DICMDNDX+2(5) SET INDEX TO 1                       00404000
         LA    R1,DILBKEQ         SET R1 TO DO KEQ READ                 00405000
         L     R15,=A(DILBCHK)    GO READ THE INDEX                     00406000
         BASR  R10,R15                                        2024183   00407000
         L     R12,DILOADDR        ESTABLISH ADDRESSABILITY             00408000
         L     R7,=F'4096'                                              00409000
         AR    R7,R12                                                   00410000
         USING DILOCAL,R12,R7                                           00411000
         XR    R1,R1                                                    00412000
         ICM   R1,3,DICIDNUM      GET THE NBR OF FIELDS                 00413000
         STCM  R1,15,DICMDNUM(5)   SAVE THE HINDEX                      00414000
         LA    R1,L'SAREPKEY+4                                2602105   00415000
         STCM  R1,3,0(5)          SAVE THE LENGTH                       00416000
         DROP  R7                                                       00417000
         B     DIEXEND                                                  00418000
* READ REQUEST - FOR DEFINITION RECORD                                  00419000
DIEXREAD EQU   *                                                        00420000
         LA    R4,2000            SET RECORD LENGTH TO READ             00421000
* THE FOLLOWING STATEMENT(S) MODIFIED BY                      2024183   00421999
         BAS   R10,DIEXOPN        CHECK DICTIONARY OPEN STATUS          00422000
         ICM   R11,15,DIMODPTR                                          00423000
* THE FOLLOWING STATEMENT(S) MODIFIED BY                      2024183   00423999
         MODCB RPL=(6),RECLEN=(4),OPTCD=(KEQ,NUP),AREA=(S,2(5)),       X00424000
               ARG=(S,DICMKEY),MF=(G,(11))                              00425000
         LR    R1,R6              SAVE RPL ADDRESS                      00426000
         LTR   R15,R15            ANY ERRORS?                           00427000
         BNZ   DIEVERR            YES - TRANSLATE ERRORS                00428000
         GET   RPL=(6)                                                  00429000
         LTR   R15,R15            ANY ERRORS?                           00430000
         BNZ   DIEVERR            YES - TRANSLATE ERRORS                00431000
         B     DIEXENDX           RETURN TO CALLER                      00432000
* READ LOCATE REQUEST - FOR DEFINITION RECORD                           00433000
DIEXREL  EQU   *                                                        00434000
         LA    R4,2000            SET RECORD LENGTH TO READ             00435000
* THE FOLLOWING STATEMENT(S) MODIFIED BY                      2024183   00435999
         BAS   R10,DIEXOPN        CHECK DICTIONARY OPEN STATUS          00436000
         ICM   R11,15,DIRPLDD      GET RPL ADDRESS                      00437000
         ICM   R1,15,DIMODPTR                                           00438000
* THE FOLLOWING STATEMENT(S) MODIFIED BY                      2024183   00438999
         MODCB RPL=(11),RECLEN=(4),AREA=(S,DICMAFLD),ARG=(S,DICMKEY),  X00439000
               MF=(G,(1))                                               00440000
         LR    R1,R11             SET RPL ADDRESS                       00441000
         LTR   R15,R15            ANY ERRORS?                           00442000
         BNZ   DIEVERR            YES - TRANSLATE ERRORS                00443000
         GET   RPL=(1)                                                  00444000
         LTR   R15,R15            ANY ERRORS?                           00445000
         BNZ   DIEVERR            YES - TRANSLATE ERRORS                00446000
         L     R5,DICMAFLD        GET THE ADDRESS                       00447000
         BCTR  R5,0                                                     00448000
         BCTR  R5,0                                                     00449000
         L     R13,DICMEREG+4     RESTORE CALLERS REGISTER SAVE         00450000
         ST    R5,40(R13)         SET THE NEW BUFFER POINTER            00451000
         B     DIEXITF            RETURN TO CALLER                      00452000
*********************************************************************** 00453000
*        WRITE  REQUEST                                               * 00454000
*********************************************************************** 00455000
DIEXWRI  EQU   *                                                        00456000
* THE FOLLOWING STATEMENT(S) MODIFIED BY                      2024183   00456999
         BAS   R10,DIEXOPN        CHECK DICTIONARY OPEN STATUS          00457000
         LA    R1,DILBKEQ         SET R1 TO DO KEQ READ                 00458000
         L     R15,=A(DILBCHK)    IZT IN THE LOCAL BUFFER               00459000
         BASR  R10,R15                                        2024183   00460000
         BE    DIEXWRI1           YES, DO NOT READ                      00461000
         L     R15,=A(DILBREA)    NO, READ THE CORRECT ONE              00462000
         BASR  R10,R15                                        2024183   00463000
DIEXWRI1 EQU *                                                          00464000
         L     R15,=A(DILBFLD)    GO FIND THE FIELD NUMBER              00465000
         BASR  R10,R15                                        2024183   00466000
         L     R15,=A(DILBMVL)    MOVE TO LOCAL BUFFER                  00467000
         BASR  R10,R15                                        2024183   00468000
         NI    DICMEXFL,255-DICMBTAB  NO LONGER FIRST, TURN OFF FLAG    00469000
         B     DIEXEND            RETURN TO CALLER                      00470000
* WRITE REQUEST - FOR DEFINITION RECORD                                 00471000
DIEXWRID EQU   *                                                        00472000
         XR    R4,R4              CLEAR LENGTH REG                      00473000
         ICM   R4,3,0(R5)         GET RECORD LENGTH TO WRITE            00474000
* THE FOLLOWING STATEMENT(S) MODIFIED BY                      2024183   00474999
         BAS   R10,DIEXOPN        CHECK DICTIONARY OPEN STATUS          00475000
         ICM   R11,15,DIMODPTR                                          00476000
* THE FOLLOWING STATEMENT(S) MODIFIED BY                      2024183   00476999
         MODCB RPL=(6),RECLEN=(4),OPTCD=(KEQ,NUP),AREA=(S,2(5)),       X00477000
               ARG=(S,DICMKEY),MF=(G,(11))                              00478000
         LR    R1,R6              SAVE RPL ADDRESS                      00479000
         LTR   R15,R15            ANY ERRORS?                           00480000
         BNZ   DIEVERR            YES - TRANSLATE ERRORS                00481000
         PUT   RPL=(6)                                                  00482000
         LTR   R15,R15            ANY ERRORS?                           00483000
         BNZ   DIEVERR            YES - TRANSLATE ERRORS                00484000
         B     DIEXENDX           RETURN TO CALLER                      00485000
*********************************************************************** 00486000
*        DELETE REQUEST                                               * 00487000
*********************************************************************** 00488000
DIEXDEL  EQU   *                                                        00489000
         ICM   R6,15,DIRPLD       SET RPL ADDRESS FOR DELETES           00490000
         LA    R1,L'DICMKEY       LENGTH IS 0                           00491000
         STCM  R1,3,0(R5)                                               00492000
         CLI   DICMDFLD+7(R5),X'00'  IS IT FIELD 0                      00493000
         BNE   DIEXWRI            NO, GO DELETE THE FIELD               00494000
         BAS   R10,DIEXOPN                                    2024183   00495000
         CLC   DICMDNDX(4,R5),=F'0'  IS IT INDEX 0                      00496000
         BNE   DIEXDEL1           NO, DELETE 1 ITERATION                00497000
         L     R15,=A(DILBDEL)    GO DELETE THE WHOLE THING             00498000
         BASR  R10,R15                                        2024183   00499000
         B     DIEXEND            RETURN TO CALLER                      00500000
DIEXDEL1 EQU *                                                          00501000
         LA    R1,DILBKEQ         SET R1 TO DO KEQ READ                 00502000
         L     R15,=A(DILBCHK)    IZT IN THE LOCAL BUFFER               00503000
         BASR  R10,R15                                        2024183   00504000
         BE    DIEXDEL2           YES, DO NOT READ                      00505000
         BAS   R10,DIRPLOC        LOCATE RPL ADDRESS FOR READ 2024183   00505100
         L     R15,=A(DILBREA)    NO, READ THE CORRECT ONE              00506000
         BASR  R10,R15                                        2024183   00507000
         ICM   R6,15,DIRPLD       SET RPL ADDRESS FOR DELETES 2602105   00507100
DIEXDEL2 EQU *                                                          00508000
         L     R15,=A(DILBDL1)    DELETE THIS ONE                       00509000
         BASR  R10,R15                                        2024183   00510000
         NI    DICMEXFL,255-DICMBTAB  NO LONGER FIRST, TURN OFF FLAG    00511000
         B     DIEXEND                                                  00512000
* DELETE REQUEST - FOR DEFINITION RECORD                                00513000
DIEXDELD EQU   *                                                        00514000
         LH    R4,=H'4096'        SET RECORD LENGTH TO READ             00515000
* THE FOLLOWING STATEMENT(S) MODIFIED BY                      2024183   00515999
         BAS   R10,DIEXOPN        CHECK DICTIONARY OPEN STATUS          00516000
         LA    R10,DICMSAVE                                             00517000
         ICM   R11,15,DIRPLD       GET RPL ADDRESS                      00518000
         ICM   R1,15,DIMODPTR                                           00519000
* THE FOLLOWING STATEMENT(S) MODIFIED BY                      2024183   00519999
         MODCB RPL=(11),OPTCD=(UPD),RECLEN=(4),AREA=(10),              X00520000
               ARG=(S,DICMKEY),MF=(G,(1))                               00521000
         LR    R1,R11             SAVE RPL ADDRESS                      00522000
         LTR   R15,R15            ANY ERRORS?                           00523000
         BNZ   DIEVERR            YES - TRANSLATE ERRORS                00524000
         ICM   R1,15,DIRPLD       GET RPL ADDRESS                       00525000
         GET   RPL=(1)                                                  00526000
         LTR   R15,R15            ANY ERRORS?                           00527000
         BNZ   DIEVERR            YES - TRANSLATE ERRORS                00528000
         ICM   R1,15,DIRPLD       GET RPL ADDRESS                       00529000
         ERASE RPL=(1)                                                  00530000
         LTR   R15,R15            ANY ERRORS?                           00531000
         BNZ   DIEVERR            YES - TRANSLATE ERRORS                00532000
         B     DIEXEND            RETURN TO CALLER                      00533000
*********************************************************************** 00534000
*        REWRITE REQUEST                                              * 00535000
*********************************************************************** 00536000
DIEXRWI  EQU   *                                                        00537000
         B     DIEXWRI                                                  00538000
* REWRITE REQUEST - FOR DEFINITION RECORD                               00539000
DIEXRWID EQU   *                                                        00540000
         PUT   RPL=(6)                                                  00541000
         LTR   R15,R15            ANY ERRORS?                           00542000
         BNZ   DIEVERR            YES - TRANSLATE ERRORS                00543000
         B     DIEXENDX           RETURN TO CALLER                      00544000
*********************************************************************** 00545000
*        READ FOR UPDATE REQUEST                                      * 00546000
*********************************************************************** 00547000
DIEXUPD  EQU   *                                                        00548000
* THE FOLLOWING STATEMENT(S) MODIFIED BY                      2024183   00548999
         BAS   R10,DIEXOPN        CHECK DICTIONARY OPEN STATUS          00549000
         LA    R1,DILBKEQ         SET R1 TO DO KEQ READ                 00550000
         L     R15,=A(DILBCHK)    IZT IN THE LOCAL BUFFER               00551000
         BASR  R10,R15                                        2024183   00552000
         BE    DIEXUPD1           YES, DO NOT READ                      00553000
         L     R15,=A(DILBREA)    NO, READ THE CORRECT ONE              00554000
         BASR  R10,R15                                        2024183   00555000
DIEXUPD1 EQU *                                                          00556000
         L     R15,=A(DILBHGH)    IS THE FIELD NUMBER TO HIGH           00557000
         BASR  R10,R15                                        2024183   00558000
         BE    DIEXREAE                                                 00559000
         L     R15,=A(DILBFLD)    GO FIND THE FIELD NUMBER              00560000
         BASR  R10,R15                                        2024183   00561000
         L     R15,=A(DILBMVU)    MOVE TO USERS BUFFER                  00562000
         BASR  R10,R15                                        2024183   00563000
         NI    DICMEXFL,255-DICMBTAB  NO LONGER FIRST, TURN OFF FLAG    00564000
         B     DIEXEND            RETURN TO CALLER                      00565000
* READ FOR UPDATE REQUEST - FOR DEFINITION RECORD                       00566000
DIEXUPDD EQU   *                                                        00567000
         LA    R4,2000            SET RECORD LENGTH TO READ             00568000
* THE FOLLOWING STATEMENT(S) MODIFIED BY                      2024183   00568999
         BAS   R10,DIEXOPN        CHECK DICTIONARY OPEN STATUS          00569000
         ICM   R11,15,DIMODPTR                                          00570000
* THE FOLLOWING STATEMENT(S) MODIFIED BY                      2024183   00570999
         MODCB RPL=(6),RECLEN=(4),OPTCD=(KEQ,UPD),AREA=(S,2(5)),       X00571000
               ARG=(S,DICMKEY),MF=(G,(11))                              00572000
         LR    R1,R6              SAVE RPL ADDRESS                      00573000
         LTR   R15,R15            ANY ERRORS?                           00574000
         BNZ   DIEVERR            YES - TRANSLATE ERRORS                00575000
         GET   RPL=(6)                                                  00576000
         LTR   R15,R15            ANY ERRORS?                           00577000
         BNZ   DIEVERR            YES - TRANSLATE ERRORS                00578000
         B     DIEXEND            RETURN TO CALLER                      00579000
*********************************************************************** 00580000
*        CLOSE DICTIONARY                                             * 00581000
*********************************************************************** 00582000
DIEXCLS  EQU   *                                                        00583000
         L     R15,DILOADDR                                             00584000
         LTR   R15,R15            HAS THE LOCAL BUFFER BEEN ALLOCATED   00585000
         BZ    DIEXCLS2           NO, GO AWAY                           00586000
         L     R15,=A(DILBCLS)    CLOSE THE LOCAL BUFFER                00587000
         BASR  R10,R15                                        2024183   00588000
         L     R0,=A(DILBSIZE)    R0 = FREE-STORAGE SIZE                00589000
         L     R1,DILOADDR        R1 = FREE-STORAGE ADDRESS             00590000
         AIF   (&VSE).VC00100                                           00591000
         FREEMAIN R,LV=(0),A=(1)  FREE VIRTUAL STORAGE                  00592000
         AGO   .ML00100                                                 00593000
.VC00100 ANOP                                                           00594000
         FREEVIS                                                        00595000
.ML00100 ANOP                                                           00596000
         ST    R15,DICMSV15      SAVE R15 FOR DEBUGGING                 00597000
         LTR   R15,R15            WAS FREE-STORAGE SUCCESSFUL           00598000
         BNZ   DIFMERR              N. GO TO ERROR ROUTINE              00599000
DIEXCLS2 EQU *                                                          00600000
         ICM   R11,15,DIREPACB       GET ACB ADDRESS                    00601000
         AIF   (&VSE).VC00110                                 2024183   00601100
         CLOSE ((11)),MODE=31                                 2024183   00601200
         AGO   .ML00110                                       2024183   00601300
* THE FOLLOWING STATEMENT(S) MODIFIED BY                      2024183   00601399
.VC00110 ANOP                                                           00601400
         CLOSE ((11))                                                   00602000
* THE FOLLOWING STATEMENT(S) MODIFIED BY                      2024183   00602099
.ML00110 ANOP                                                           00602100
         NI    DICMDTS,255-DICMDOP   SET REPOSITORY DATA SET CLOSED     00603000
         B     DIEXIT             RETURN TO CALLER                      00604000
*********************************************************************** 00605000
*        FLUSH THE LOCAL BUFFERS                                      * 00606000
*********************************************************************** 00607000
DIEXFLS  EQU   *                                                        00608000
         L     R15,DILOADDR                                             00609000
         LTR   R15,R15            HAS THE LOCAL BUFFER BEEN ALLOCATED   00610000
         BZ    DIEXIT             NO, GO AWAY                           00611000
         L     R15,=A(DILBIPT)    WRITE THE INDEX BUFFER                00612000
         BASR  R10,R15                                        2024183   00613000
         L     R15,=A(DILBPUT)    WRITE THE LOCAL BUFFER                00614000
         BASR  R10,R15                                        2024183   00615000
         BNZ   DIEVERR                                                  00616000
*        LA    R10,DILOADDR               ==> IF ACTIVATED, MAKE SURE   00617000
*        FREEMAIN  E,A=(10),LV=DILBSIZE       IT IS CODED CONDITIONAL   00618000
*        MVC   DILOADDR,=F'0'                 FOR MVS/VSE               00619000
         B     DIEXIT             RETURN TO CALLER                      00620000
*********************************************************************** 00621000
*        OPEN  DICTIONARY                                             * 00622000
*********************************************************************** 00623000
DIEXOPN  EQU   *                                                        00624000
         TM    DICMDTS,DICMDOP    IS DICTIONARY DATA SET OPEN?          00625000
         BOR   R10                YES - CONTINUE                        00626000
         STCM  R10,15,DICMSV10    SAVE RETURN REGISTER                  00627000
         L     R15,=A(DIGENREP)                                         00628000
         BASR  R10,R15            GENERATE ACB STUFF          2024183   00629000
         ICM   R10,15,DICMSV10    RESET RETURN ADDRESS                  00630000
         ICM   R11,15,DIREPACB     GET  ACB ADDRESS                     00631000
         ICM   R1,15,DIMODPTR                                           00632000
         MODCB ACB=(11),MACRF=(OUT),MF=(G,(1)),RMODE31=ALL    2024183   00633000
DIEXOPNR EQU   *                                                        00634000
         ICM   R11,15,DIREPACB     GET  ACB ADDRESS                     00635000
         ICM   R1,15,DIMODPTR    GET MODCB W/A                          00636000
* THE FOLLOWING STATEMENT(S) MODIFIED BY                      2024183   00636999
         MODCB ACB=(11),DDNAME=(*,DICMDDN),MF=(G,(1)),RMODE31=ALL       00637000
         L     R15,=A(DIGNRRPL)   GET RPL GEN ADDRESS                   00638000
         STCM  R10,15,DICMSV10    SAVE RETURN REGISTER                  00639000
         BASR  R10,R15                                        2024183   00640000
         BAS   R10,DIRPLOC        RE-LOCATE RPL ADDRESS       2024183   00641000
         ICM   R10,15,DICMSV10    RESET RETURN ADDRESS                  00642000
         ICM   R11,15,DIREPACB     GET  ACB ADDRESS                     00643000
         AIF   (&VSE).VC00120                                 2024183   00643100
         OPEN  ((11)),MODE=31                                 2024183   00643150
         AGO   .ML00120                                       2024183   00643200
* THE FOLLOWING STATEMENT(S) MODIFIED BY                      2024183   00643299
.VC00120 ANOP                                                           00643300
         OPEN  ((11))                                                   00644000
* THE FOLLOWING STATEMENT(S) MODIFIED BY                      2024183   00644099
.ML00120 ANOP                                                           00644100
         LTR   R15,R15            ANY OPEN ERRORS?                      00645000
         BNZ   DIEOPER            YES - GO REPORT ERROR                 00646000
         OI    DICMDTS,DICMDOP    SET DICTIONARY DATA SET OPEN?         00647000
         BR    R10                RETURN TO CALLER                      00648000
*********************************************************************** 00649000
*        OPEN  DICTIONARY - READ ONLY                                 * 00650000
*********************************************************************** 00651000
DIEXROS  EQU   *                                                        00652000
         TM    DICMDTS,DICMDOP    IS DICTIONARY DATA SET OPEN?          00653000
         BO    DIEXIT             YES - RETURN                          00654000
         L     R15,=A(DIGENREP)                                         00655000
         BASR  R10,R15            GENERATE ACB STUFF          2024183   00656000
         BAS   R10,DIEXOPNR       OPEN DICTIONARY             2024183   00657000
         B     DIEXIT             RETURN                                00658000
*********************************************************************** 00659000
*        LOCATE CORRECT RPL TO USE                                    * 00660000
*********************************************************************** 00661000
DIRPLOC  EQU   *                                                        00662000
         XR    R6,R6              CLEAR RPL POINTER                     00663000
*        CLI   DICMEXT,DICMDELE   REPOSITORY DELETE REQUEST?  2602105   00664000
*        BER   R10                                            2602105   00665000
         CLI   DICMEXT,DICMAREA   AUXILIARY REQUEST?                    00666000
         BNLR  R10                                                      00667000
         LA    R7,5               NUMBER OF RPLS                        00668000
         LA    R6,DIRPLTBL        RPL TABLE                             00669000
DIRPLUP  EQU   *                                                        00670000
         CLC   0(3,R6),DICMKFRM   IS THIS THE CORRECT RPL?              00671000
         BE    DIRPLOK            YES - THEN USE IT                     00672000
         CLC   0(3,R6),=X'000000' IS THIS AN UNUSED RPL?                00673000
         BE    DIRPLSV            YES - THEN USE IT                     00674000
DIRPLDO  EQU   *                                                        00675000
         LA    R6,10(R6)          BUMP TO NEXT ENTRY IN RPL TABLE       00676000
         BCT   R7,DIRPLUP         CONTINUE TO LOCATE RPL                00677000
         ICM   R6,15,4(R6)        OBTAIN RPL ADDRESS                    00678000
         BR    R10                RETURN                                00679000
DIRPLOK  EQU   *                                                        00680000
         CLC   DICMKFRM,=C'ALI'   IS ENTITY TYPE FOR ALIAS?             00681000
         BE    DIRPLOKU           YES - THEN IT'S NOT INDEXED           00682000
         CLC   DICMKNDX,=XL4'00'  IS THIS AN INDEXED FIELD              00683000
         BNE   DIRPLOKI                                                 00684000
DIRPLOKU EQU   *                                                        00685000
         CLI   3(R6),X'00'        IS THIS AN INDEXED ENTRY?             00686000
         BNE   DIRPLDO            NO - CONTINUE SCANNING                00687000
         ICM   R6,15,4(R6)        OBTAIN RPL ADDRESS                    00688000
         BR    R10                RETURN TO CALLER                      00689000
DIRPLOKI EQU   *                                                        00690000
         CLI   3(R6),C'I'         IS THIS AN INDEXED ENTRY?             00691000
         BNE   DIRPLDO            NO - CONTINUE SCANNING                00692000
         ICM   R6,15,4(R6)        OBTAIN RPL ADDRESS                    00693000
         BR    R10                RETURN TO CALLER                      00694000
DIRPLSV  EQU   *                                                        00695000
         MVC   0(3,R6),DICMKFRM   SAVE FORMAT TYPE IN RPL LIST          00696000
         CLC   DICMKFRM,=C'ALI'   IS ENTITY TYPE FOR ALIAS?             00697000
         BE    DIRPLSNI           YES - THEN IT'S NOT INDEXED           00698000
         CLC   DICMKNDX,=XL4'00'  IS THIS AN INDEXED FIELD              00699000
         BE    DIRPLSNI                                                 00700000
         MVI   3(R6),C'I'         INDICATE INDEXED ENTRY                00701000
DIRPLSNI EQU   *                                                        00702000
         ICM   R6,15,4(R6)        OBTAIN RPL ADDRESS                    00703000
         BR    R10                RETURN TO CALLER                      00704000
*********************************************************************** 00705000
*        ERROR PROCESSING                                             * 00706000
*********************************************************************** 00707000
DIEXERR  EQU   *                                                        00708000
         LA    R0,310             INVALID EXTERNAL REQUEST              00709000
         STCM  R0,15,DICMSTAT        SET ERROR CODE IN COMMON           00710000
         B     DIEXIT             RETURN TO NUCLEUS                     00711000
DIGMERR  EQU   *                                                        00712000
         LA    R0,331             GET-STORAGE ERROR                     00713000
         STCM  R0,15,DICMSTAT        SET ERROR CODE IN COMMON           00714000
         B     DIEXITF            RETURN TO NUCLEUS                     00715000
DIFMERR  EQU   *                                                        00716000
         LA    R0,339             FREE-STORAGE ERROR                    00717000
         STCM  R0,15,DICMSTAT        SET ERROR CODE IN COMMON           00718000
         B     DIEXITF            RETURN TO NUCLEUS                     00719000
DIEOPER  EQU   *                                                        00720000
         LA    R0,311             DICTIONARY NOT OPEN ERROR             00721000
         STCM  R0,15,DICMSTAT        SET ERROR CODE IN COMMON           00722000
         B     DIEXIT             RETURN TO NUCLEUS                     00723000
DIEDERR  EQU   *                                                        00724000
DIEVERR  EQU   *                                                        00725000
         STCM  R15,15,DICMSTAT    PUT RETURN CODE IN STATUS             00726000
         CLI   DICMEXT,DICMAREA   WAS REQUEST FOR AUXILIARY?            00727000
         BL    DIEXIT             NO - RETURN TO NUCLEUS                00728000
         CH    R15,=H'100'        IS THE RETURN CODE GT 100             00729000
         BH    DIEXIT             YES, GO WITH IT                       00730000
         XC    DICMRECL,DICMRECL  CLEAR RECORD LENGTH FIELD             00731000
         LR    R9,R15            SAVE RETURN CODE                       00732000
         LA    R10,DICMRECL        AREA TO RECEIVE FEEDBACK CODE        00733000
         LR    R11,R1                                                   00734000
         ICM   R1,15,DIMODPTR       GET W/A                             00735000
         SHOWCB RPL=(11),LENGTH=8,AREA=(10),FIELDS=(RECLEN,FDBK),      X00736000
               MF=(G,(1))                                               00737000
         STCM  R9,3,DICMSTAT      PUT RETURN CODE IN STATUS             00738000
         B     DIEXIT             RETURN TO NUCLEUS                     00739000
*********************************************************************** 00740000
*        RETURN TO CALLER                                             * 00741000
*********************************************************************** 00742000
DIEXEND  EQU   *                                                        00743000
         B     DIEXITF                                                  00744000
DIEXENDX EQU   *                                                        00745000
         LA    R10,DICMRECL        AREA TO RECEIVE DATA                 00746000
         LR    R11,R1                                                   00747000
         ICM   R1,15,DIMODPTR       GET W/A                             00748000
         SHOWCB RPL=(11),LENGTH=8,AREA=(10),FIELDS=(RECLEN,FDBK),      X00749000
               MF=(G,(1))                                               00750000
         B     DIEXIT                                                   00751000
DIEXIT   EQU   *                                                        00752000
         CLC   DICMCMD(3),=C'END' IS THIS AN END REQUEST?               00753000
         BE    DIEXITF            YES - BYPASS LRECL STOW               00754000
         ICM   R0,15,DICMRECL     GET RECORD LENGTH                     00755000
         STCM  R0,3,0(R5)         SAVE LENGTH IN RECORD BUFFER          00756000
DIEXITF  EQU   *                                                        00757000
         L     R13,DICMEREG+4     RESTORE CALLERS REGISTER SAVE         00758000
         LM    R14,R12,12(R13)    RESTORE CALLERS REGISTERS             00759000
         BR    R14                RETURN TO CALLER                      00760000
         SPACE                                                          00761000
         TITLE 'DSDIEXT  - LITERAL POOL'                                00762000
         LTORG                                                          00763000
         TITLE 'DSDIEXT  - AUXILIARY OPEN ROUTINE'                      00764000
         DROP  R12                                                      00765000
         TITLE 'DSDIEXT  - GENCB ROUTINES'                              00766000
*********************************************************************** 00767000
*        REPOSITORY ACCESS CONTROL BLOCK GENERATION                   * 00768000
*********************************************************************** 00769000
         USING DIGENREP,R12                                             00770000
DIGENREP DS    0H                                                       00771000
         LR    R12,R15            SET ROUTINE BASE ADDRESS              00772000
         LA    R0,512             SET GETMAIN STORAGE LENGTH            00773000
*                                                                       00774000
         AIF   (&VSE).OP00200                                           00775000
         GETMAIN R,LV=(0)              GET STORAGE                      00776000
         AGO   .OP00300                                                 00777000
.OP00200 ANOP                                                           00778000
         GETVIS                                                         00779000
.OP00300 ANOP                                                           00780000
         STCM  R1,15,DIMODPTR     STORAGE FOR MODCB'S                   00781000
         STCM  R0,3,DIMODPTL         AND LENGTH                         00782000
* THE FOLLOWING STATEMENT(S) MODIFIED BY                      2024183   00782999
         GENCB BLK=ACB,AM=VSAM,MACRF=(KEY,DIR,IN),STRNO=6,BUFND=7,     X00783000
               BUFNI=1,MF=(G,(1)),LOC=ANY,RMODE31=ALL                   00784000
         STCM  R1,15,DIREPACB     SAVE ACB ADDRESS                      00785000
         BR    R10                                                      00786000
         LTORG                                                          00787000
         EJECT                                                          00788000
         DROP  R12                                                      00789000
*********************************************************************** 00790000
*        REPOSITORY REQUEST PARAMETER LIST GENERATION                 * 00791000
*********************************************************************** 00792000
         USING DIGNRRPL,R12                                             00793000
DIGNRRPL DS    0H                                                       00794000
         LR    R12,R15            SET BASE REGISTER                     00795000
         ICM   R11,15,DIREPACB     GET  ACB ADDRESS                     00796000
         ICM   R1,15,DIMODPTR    GET MODCB W/A                          00797000
* THE FOLLOWING STATEMENT(S) MODIFIED BY                      2024183   00797999
         GENCB BLK=RPL,AM=VSAM,ACB=(11),KEYLEN=25,AREALEN=4096,        X00798000
               OPTCD=(KEQ,DIR,MVE),MF=(G,(1)),LOC=ANY                   00799000
         STCM  R1,15,DIRPLTBL+4   SAVE RPL ADDRESS                      00800000
         STCM  R0,3,DIRPLTBL+8    SAVE RPL LENGTH                       00801000
         ICM   R1,15,DIMODPTR    GET MODCB W/A                          00802000
* THE FOLLOWING STATEMENT(S) MODIFIED BY                      2024183   00802999
         GENCB BLK=RPL,AM=VSAM,ACB=(11),KEYLEN=25,AREALEN=4096,        X00803000
               OPTCD=(KEQ,DIR,MVE),MF=(G,(1)),LOC=ANY                   00804000
         STCM  R1,15,DIRPLTBL+14  SAVE RPL ADDRESS                      00805000
         STCM  R0,3,DIRPLTBL+18   SAVE RPL LENGTH                       00806000
         ICM   R1,15,DIMODPTR    GET MODCB W/A                          00807000
* THE FOLLOWING STATEMENT(S) MODIFIED BY                      2024183   00807999
         GENCB BLK=RPL,AM=VSAM,ACB=(11),KEYLEN=25,AREALEN=4096,        X00808000
               OPTCD=(KEQ,DIR,MVE),MF=(G,(1)),LOC=ANY                   00809000
         STCM  R1,15,DIRPLTBL+24  SAVE RPL ADDRESS                      00810000
         STCM  R0,3,DIRPLTBL+28   SAVE RPL LENGTH                       00811000
         ICM   R1,15,DIMODPTR    GET MODCB W/A                          00812000
* THE FOLLOWING STATEMENT(S) MODIFIED BY                      2024183   00812999
         GENCB BLK=RPL,AM=VSAM,ACB=(11),KEYLEN=25,AREALEN=4096,        X00813000
               OPTCD=(KEQ,DIR,MVE),MF=(G,(1)),LOC=ANY                   00814000
         STCM  R1,15,DIRPLTBL+34  SAVE RPL ADDRESS                      00815000
         STCM  R0,3,DIRPLTBL+38   SAVE RPL LENGTH                       00816000
         ICM   R1,15,DIMODPTR    GET MODCB W/A                          00817000
* THE FOLLOWING STATEMENT(S) MODIFIED BY                      2024183   00817999
         GENCB BLK=RPL,AM=VSAM,ACB=(11),KEYLEN=25,AREALEN=4096,        X00818000
               OPTCD=(KEQ,DIR,MVE),MF=(G,(1)),LOC=ANY                   00819000
         STCM  R1,15,DIRPLTBL+44  SAVE RPL ADDRESS                      00820000
         STCM  R0,3,DIRPLTBL+48   SAVE RPL LENGTH                       00821000
         ICM   R1,15,DIMODPTR    GET MODCB W/A                          00822000
* THE FOLLOWING STATEMENT(S) MODIFIED BY                      2024183   00822999
         GENCB BLK=RPL,AM=VSAM,ACB=(11),KEYLEN=25,AREALEN=4096,        X00823000
               OPTCD=(KEQ,DIR,MVE),MF=(G,(1)),LOC=ANY                   00824000
         STCM  R1,15,DIRPLTBL+54  SAVE RPL ADDRESS                      00825000
         STCM  R0,3,DIRPLTBL+58   SAVE RPL LENGTH                       00826000
         ICM   R1,15,DIMODPTR    GET MODCB W/A                          00827000
* THE FOLLOWING STATEMENT(S) MODIFIED BY                      2024183   00827999
         GENCB BLK=RPL,AM=VSAM,ACB=(11),KEYLEN=25,AREALEN=4096,        X00828000
               OPTCD=(KEQ,DIR,UPD,LOC),MF=(G,(1)),LOC=ANY               00829000
         STCM  R1,15,DIRPLD       SAVE RPL ADDRESS                      00830000
         STCM  R0,3,DIRPLDL       SAVE RPL LENGTH                       00831000
         ICM   R1,15,DIMODPTR    GET MODCB W/A                          00832000
* THE FOLLOWING STATEMENT(S) MODIFIED BY                      2024183   00832999
         GENCB BLK=RPL,AM=VSAM,ACB=(11),KEYLEN=25,AREALEN=4096,        X00833000
               OPTCD=(KEQ,DIR,NUP,LOC),MF=(G,(1)),LOC=ANY               00834000
         STCM  R1,15,DIRPLDD      SAVE RPL ADDRESS                      00835000
         STCM  R0,3,DIRPLDDL      SAVE RPL LENGTH                       00836000
         ICM   R1,15,DIMODPTR    GET MODCB W/A                          00837000
* THE FOLLOWING STATEMENT(S) MODIFIED BY                      2024183   00837999
         GENCB BLK=RPL,AM=VSAM,ACB=(11),KEYLEN=25,AREALEN=4096,        X00838000
               OPTCD=(KEQ,DIR,MVE),MF=(G,(1)),LOC=ANY                   00839000
         STCM  R1,15,DIRPLI       SAVE RPL ADDRESS                      00840000
         STCM  R0,3,DIRPLIL       SAVE RPL LENGTH                       00841000
         BR    R10                                                      00842000
         DROP  R12                                                      00843000
         LTORG                                                          00844000
         EJECT                                                          00845000
*********************************************************************** 00846000
*        LOCAL BUFFER MANIPULATION ROUTINE                            * 00847000
*********************************************************************** 00848000
         TITLE 'DSDLOCAL - LOCAL BUFFER MANIPULATION'                   00849000
         USING DILBCHK,R9,R8                                            00850000
         USING DILOCAL,R12,R7                                           00851000
         CNOP  0,4                                                      00852000
DILBCHK  EQU *                                                          00853000
         STM   R0,R15,DILBSAVE    SAVE ALL REGISTERS                    00854000
         L     R9,DILBADDR                                              00855000
         B     DILBCHKS           GO TO DILBCHK START                   00856000
DILBINIT EQU *                                                          00857000
         L     R9,DILBADDR                                              00858000
         L     R8,=F'4096'                                              00859000
         AR    R8,R9                                                    00860000
         L     R12,DILOADDR                                             00861000
         LTR   R12,R12            HAS THE BUFFER BEEN ALLOCATED         00862000
         BNZ   DILBINI1           YES, CONTINUE                         00863000
* ALLOCATE THE STORAGE                                                  00864000
         L     R0,=A(DILBSIZE)    R0 = FREE-STORAGE SIZE                00865000
*                                                                       00866000
         AIF   (&VSE).VC00400                                           00867000
         GETMAIN R,LV=(0)              GET STORAGE                      00868000
         AGO   .ML00400                                                 00869000
.VC00400 ANOP                                                           00870000
         GETVIS                                                         00871000
.ML00400 ANOP                                                           00872000
*                                                                       00873000
         LTR   R15,R15            WAS STORAGE ACQUISITION SUCCESSFUL    00874000
         BZ    DILBINI0             Y. SKIP BY ERROR ROUTINE            00875000
         LA    R15,331            ERROR IN GETMAIN                      00876000
         B     DILBERR            GO REPORT ERROR                       00877000
DILBINI0 EQU *                                                          00878000
         ST    R1,DILOADDR        STORE ACQUIRED STORAGE ADDRESS        00879000
         LR    R12,R1             R12 = A(ACQUIRED STORAGE)             00880000
         L     R0,=A(DILBSIZE)    R0 = FREE-STORAGE SIZE      9913569   00880100
         AR    R1,R0              R1 = A(FREE STG HI BOUNDARY)9913569   00880200
         ST    R1,DILOCEND        SAVE A(FREE STG HI BOUNDARY)9913569   00880300
DILBINI1 EQU *                                                          00881000
         L     R7,=F'4096'                                              00882000
         AR    R7,R12                                                   00883000
         MVI   DILBPFLG,C' '      TURN OFF LBPUT FLAG                   00884000
         BR    R10                                                      00885000
         EJECT                                                          00888000
*********************************************************************** 00889000
*        THIS ROUTINE WILL CHECK TO SEE IF THE REQUESTED RECORD IS    * 00890000
*        IN THE LOCAL BUFFER                                          * 00891000
*********************************************************************** 00892000
DILBCHKS EQU *                                                          00893000
         BAS   R10,DILBINIT                                   2024183   00894000
         L     R1,DILBSAVE+4      RELOAD THE MODE INDICATOR             00895000
         STC   R1,DILBMODE        SAVE THE READ MODE (KEQ, KGE)         00896000
*                                                                       00897000
* THIS DETERMINES IF THE REQUESTED COMMAND IS A READ-ONLY               00898000
* COMMAND OR A WRITE COMMAND.                                           00899000
         MVI   DILBFUNC,C'R'      INITIALIZE CMD TO A READ-ONLY         00900000
         CLC   DICMCMD,=C'INSERT' IS THIS AN INSERTED FIELD             00901000
         BE    DILBUSEW           YES, GO SET UPDATE FLAG ON            00902000
         CLC   DICMCMD,=C'UPDATE' IS THIS AN UPDATED FIELD              00903000
         BNE   DILBUSE0           NO, CONTINUE                          00904000
DILBUSEW EQU *                                                          00905000
         MVI   DILBFUNC,C'W'      SET COMMAND TO WRITE                  00906000
DILBUSE0 EQU *                                                          00907000
*                                                                       00908000
DILBUSE2 EQU *                                                          00910000
         XR    R15,R15                                                  00916000
         ICM   R15,3,0(R5)        GET THE USERS LENGTH                  00917000
         LA    R0,L'SAREPKEY      SUBTRACT OUT FIXED PORTION  2602105   00918000
         SR    R15,R0                                                   00919000
         BNM   DILBCHK1           IF THE RESULT IS NEGATIVE             00920000
         XR    R15,R15            CLEAR IT                              00921000
DILBCHK1 EQU *                                                          00922000
         STH   R15,DIUSRLEN       SAVE THE USERS LENGTH                 00923000
         MVC   DIFLDNUM,DICMDFLD+7(5)  GET THE FIELD NUMBER TO PROCESS  00924000
         XR    R15,R15                                                  00925000
         ICM   R15,3,DICMDNDX+2(5) LOAD THE REQUESTED INDEX             00926000
         LTR   R15,R15            IS THE INDEX 0                        00927000
         BNE   DILBCH11                                                 00928000
         STCM  R15,15,DICINDX     SET THE PHYSICAL INDEX TO 0           00929000
         B     DILBCHK8           GO PROCESS THE INDEX                  00930000
DILBCH11 EQU *                                                          00931000
         CLC   DICIKEY1,DICMDAPL(5)    IS THE FIRST SEGMENT THE SAME    00932000
         BE    DILBCHK5                                                 00933000
         BAS   R10,DILBIPT        GO WRITE THE INDEX          2024183   00934000
         BNZ   DILBERR                                                  00935000
* NOW READ THE NEW INDEX                                                00936000
DILBCHK2 EQU *                                                          00937000
         MVC   DICIKEY,DICMDAPL(5)     LOAD THE KEY                     00938000
         MVC   DICIKFLD,=CL8'INDEX'    SET FIELD NAME                   00939000
         MVC   DICIKIDX,=F'0'     ZERO OUT INDEX                        00940000
         MVC   DICIDNUM,=H'0'     HINDEX = 0                            00941000
         MVC   DICIDAVL,=H'2'     SET NEXT AVAILABLE INDEX              00942000
         MVC   DIMVLEN,=F'0'      CLEAR FLD FOR INDEX REC LEN 9913569   00943000
         MVC   DIINDXRD,NULLS     CLEAR INDEX REPLACE DATA FLD9913569   00943100
         LA    R4,DICIKEY         R4 = A(INDEX REC BEGINNING) 9913569   00943300
         CLI   DILBMODE,DILBKGE   IZT A KGE READ                        00944000
         BE    DILBCH2A           YES SET KGE FLAG ON                   00945000
DILBI000 DS   0H                                              9913569   00945100
         ICM   R11,15,DIRPLI       GET RPL ADDRESS                      00946000
         ICM   R1,15,DIMODPTR                                           00947000
* THE FOLLOWING STATEMENT(S) MODIFIED BY                      2024183   00947999
         MODCB RPL=(11),MF=(G,(1)),OPTCD=(KEQ,NUP,MVE),       9913569  X00948000
               AREA=(4),ARG=(S,DICIKEY),                      9913569  X00949000
               AREALEN=4096,RECLEN=(S,DIMVLEN)                9913569   00949100
         LR    R1,R11             SAVE RPL ADDRESS                      00950000
         B     DILBCH2B                                                 00951000
DILBCH2A EQU *                                                          00952000
         ICM   R11,15,DIRPLI       GET RPL ADDRESS                      00953000
         ICM   R1,15,DIMODPTR                                           00954000
* THE FOLLOWING STATEMENT(S) MODIFIED BY                      2024183   00954999
         MODCB RPL=(11),MF=(G,(1)),OPTCD=(KGE,NUP,MVE),       9913569  X00955000
               AREA=(4),ARG=(S,DICIKEY),                      9913569  X00956000
               AREALEN=4096,RECLEN=(S,DIMVLEN)                9913569   00956100
         LR    R1,R11             SAVE RPL ADDRESS                      00957000
         MVC   2(L'DICIKEY1,R5),DICIKEY                                 00958000
DILBCH2B EQU *                                                          00959000
         LTR   R15,R15            ANY ERRORS                            00960000
         BNZ   DILBERR            YES - TRANSLATE ERRORS                00961000
         ICM   R1,15,DIRPLI       GET RPL ADDRESS                       00962000
         GET   RPL=(1)                                                  00963000
         LTR   R15,R15            ANY ERRORS                            00964000
         BNZ   DILBCH2C           IF NOT FND, GO CK FOR ERR   9913569   00964050
         CLC   DICIKIDX,=F'0'     IS THIS 1ST INDEX RECORD    9913569   00964100
         BNE   DILBI100             N. SKIP 1ST INDX PROCESS  9913569   00964150
         LA    R6,0                                           9913569   00964200
         ICM   R6,B'0011',DICIDNUM R6 = # ITEMS IN ENTITY     9913569   00964250
         LA    R6,1(,R6)          R6 =  #INDEXES(#ITEMS+ROOT) 9913569   00964300
         SLL   R6,1               R6 = L'INDEXES              9913569   00964350
         LA    R6,DICIDATA(R6)    R6 = A(LOG INDEX HI BNDARY) 9913569   00964400
         ST    R6,DIMVTO          SAVE A(LOG INDEX HI BNDARY) 9913569   00964450
DILBI100 DS   0H                                              9913569   00964500
         CLC   DIINDXRD,NULLS      DID WE OVERLAY INDEX DATA  9913569   00964510
         BE    DILBI200              N. SKIP DATA RESTORE     9913569   00964520
         MVC   0(25,R4),DIINDXRD     Y. RESTORE DATA OVERLAID 9913569   00964530
DILBI200 DS   0H                                              9913569   00964540
         A     R4,=F'4096'        R4 = A(TARG/NXT INDEX REC)  9913569   00964550
         CR    R4,R6              HAVE WE READ ALL OF INDEX   9913569   00964600
         BNL   DILBCHK5             Y. LOGICAL INDEX IS READY 9913569   00964650
         C     R4,DILOCEND        DO WE HAVE MORE INDEX SPACE 9913569   00964700
         BNL   DILBCH2D             N. RETURN W/ERR 335       9913569   00964750
         S     R4,=F'25'          R4 = A(NXT INDX DATA)-L'KEY 9913569   00964800
         MVC   DIINDXRD,0(R4)     SAVE INDEX DATA T/B OVERLAID9913569   00964850
         ICM   R1,B'1111',DICIKIDX R1 = INDEX INDEX/CURR      9913569   00964900
         LA    R1,1(,R1)          R1  = INDEX INDEX/NEXT      9913569   00964950
         STCM  R1,B'1111',DICIKIDX R1 = INDEX INDEX/NEXT      9913569   00965000
         B     DILBI000           LOOP TO READ NEXT INDEX REC 9913569   00965050
DILBCH2C EQU *                                                          00966000
         CLC   DICMCMD,=C'INSERT' IS THIS AN INSERTED FIELD             00967000
         BE    DILBCHK3           YES CONTINUE                          00968000
         CLC   DICMDFLD(6,5),=C'HINDEX'  IZT HINDEX                     00969000
         BE    DILBRET1           YES RETURN OK                         00970000
DILBCH2D DS   0H                                              9913569   00970100
         LA    R15,335            LOAD NOT FOUND ERROR                  00971000
         B     DILBERR                                                  00972000
DILBCHK3 EQU *                                                          00973000
* DO ANY INITIALIZATION OF THE INDEX HERE                               00974000
         MVC   DICIDAVL,=H'2'     SET NEXT AVAILABLE INDEX TO 0         00975000
DILBCHK5 EQU *                                                          00976000
         MVC   DICIKIDX,=F'0'     RESET INDEX INDEX = 0       9913569   00976100
         XR    R6,R6                                                    00977000
         ICM   R6,3,DICIDNUM      LOAD THE NUMBER OF INDEXES            00978000
         XR    R2,R2                                                    00979000
         ICM   R2,3,DICMDNDX+2(5) LOAD THE REQUESTED INDEX              00980000
         CR    R2,R6              IS THE REQUESTED GT HINDEX            00981000
         BNH   DILBCHK6           NO, GO GET THE PHYSICAL INDEX         00982000
         LR    R2,R6              LOAD THE HINDEX INTO REQUESTED        00983000
         CLI   DILBMODE,DILBKGE   IZT A KGE READ                        00984000
         BNE   DILBCH5A           NO, GO PROCESS NORMALLY               00985000
         ICM   R15,15,DICMDFLD-4(5)  LOAD THE KEY                       00986000
         LA    R15,1(,R15)        BUMP TO THE NEXT ONE                  00987000
         STCM  R15,15,DICMDFLD-4(5)  STORE THE KEY                      00988000
         B     DILBUSE2           GO READ THE NEXT ONE                  00989000
DILBCH5A EQU *                                                          00990000
         CLC   DICMCMD,=C'INSERT' IS THIS AN INSERTED FIELD             00991000
         BNE   NOTFND                                                   00992000
         TM    DICMEXFL,DICMBTAB  IS IT THE FIRST INSERT                00993000
         BZ    DILBCHK6           NO, JUST DO AN UPDATE                 00994000
         LA    R2,1(R2)           YES, ADD 1                            00995000
         STCM  R2,3,DICMDNDX+2(5) SAVE THE REQUESTED INDEX              00996000
DILBCHK6 EQU *                                                          00997000
         MVC   DICINDX,=F'0'      INITIALIZE THE PHYSICAL INDEX         00998000
         LTR   R2,R2              IS THE INDEX 0                        00999000
         BZ    DILBCHK8           YES, GO USE INDEX 0                   01000000
         CLC   DICMCMD,=C'INSERT' IS THIS AN INSERTED FIELD             01001000
         BNE   DILBCHK7                                                 01002000
* INSERT CODE FOR INSERTING                                             01003000
         TM    DICMEXFL,DICMBTAB  IS IT THE FIRST INSERT                01004000
         BZ    DILBCHK7           NO, JUST DO AN UPDATE                 01005000
         MVC   DIMVBLK,=F'2'      MOVE TWO BYTES AT A TIME              01006000
         LA    R1,DICIKEY                                               01007000
         L     R15,=A(DICLBSZI)   GET THE TO POSITION                   01008000
         AR    R1,R15                                                   01009000
         ST    R1,DIMVTO                                                01010000
         LA    R15,2                                                    01011000
         SR    R1,R15             GET THE FROM POSITION                 01012000
         ST    R1,DIMVFRM                                               01013000
         LA    R6,DICIDATA                                              01014000
         SLL   R2,1               MULTIPLY BY 2                         01015000
         AR    R6,R2                                                    01016000
         LA    R15,339            LOAD TOO MANY INDEXES ERROR           01017000
         CR    R6,R1              ARE WE OVER THE BOUNDRY               01018000
         BNL   DILBERR            YES - GO REPORT IT                    01019000
         SR    R1,R6              GET THE LENGTH TO MOVE                01020000
         ST    R1,DIMVLEN                                               01021000
         BAS   R10,DSMOVEB        INSERT SPACE FOR INDEX      2024183   01022000
         XR    R1,R1                                                    01023000
         ICM   R1,3,DICIDAVL      LOAD THE NEXT AVAILABLE INDEX         01024000
         LA    R1,1(,R1)          INCREMENT INDEX                       01025000
         STCM  R1,3,0(R6)         SAVE THE PHYSICAL ADDRESS             01026000
         STCM  R1,3,DICIDAVL      SAVE THE NEXT AVAILABLE INDEX         01027000
         ICM   R1,3,DICIDNUM      INCREMENT HINDEX                      01028000
         LA    R1,1(,R1)                                                01029000
         STCM  R1,3,DICIDNUM                                            01030000
         MVI   DICIDMOD,C'M'      INDEX HAS BEEN MODIFIED               01031000
         SRL   R2,1               RESTORE TO ORIGINAL INDEX             01032000
DILBCHK7 EQU *                                                          01033000
         LA    R6,DICIDATA                                              01034000
         SLL   R2,1               MULTIPLY BY 2                         01035000
         AR    R6,R2                                                    01036000
         XR    R4,R4                                                    01037000
         ICM   R4,3,0(R6)         LOAD THE PHYSICAL ADDRESS             01038000
         STCM  R4,15,DICINDX      SAVE THE PHYSICAL INDEX               01039000
         NI    DICINDX+2,X'0F'    CLEAR INDEX FLAG BITS       2602105   01039100
DILBCHK8 EQU *                                                          01040000
* NOW DECIDE IF THE CORRECT FIELD HAS BEEN LOADED                       01041000
         CLC   SAREPKYM,DICMDAPL(5)    IS THE FIRST SEG SAME  2602105   01042000
         BNE   DILBRET1           GOTO NO                               01043000
         CLC   SAREPNDX,DICINDX    IS THE INDEX THE SAME      2602105   01044000
         BNE   DILBRET1           GOTO NO                               01045000
* THIS CODE MAY BE CALLED BY ANY ROUTINE WISHING TO RETURN 0            01046000
DILBRET0 EQU *                                                          01047000
         LM    R0,R15,DILBSAVE    LOAD ALL REGISTERS                    01049000
         XR    R15,R15            YES, IT IS THE SAME RECORD            01050000
         LTR   R15,R15            SET CONDITION CODE                    01051000
         BR    R10                RETURN TO CALLER                      01052000
* THIS CODE MAY BE CALLED BY ANY ROUTINE WISHING TO RETURN 1            01053000
DILBRET1 EQU *                                                          01054000
         LM    R0,R15,DILBSAVE    LOAD ALL REGISTERS                    01056000
         LA    R15,1              NO, IT IS NOT THE SAME RECORD         01057000
         LTR   R15,R15            SET CONDITION CODE                    01058000
         BR    R10                RETURN TO CALLER                      01059000
DILBERR  EQU *                                                          01060000
         STM   R0,R1,DILBSAVE     SAVE FIRST TWO                        01061000
         ST    R15,DILBSAVE+60    SAVE R15                              01062000
         LM    R0,R15,DILBSAVE    LOAD ALL REGISTERS                    01064000
         B     DIEVERR            GO TO ERROR ROUTINE                   01065000
NOTFND   EQU   *                                                        01066000
         LM    R0,R15,DILBSAVE    LOAD ALL REGISTERS                    01067000
         LA    R15,16             SET NOT FOUND INDICATOR               01068000
         STCM  R15,15,DICMSTAT    PUT RETURN CODE IN STATUS             01069000
         B     DIEXIT             RETURN TO EXTERNAL EXIT POINT         01070000
DUPREC   EQU   *                                                        01071000
         LM    R0,R15,DILBSAVE    LOAD ALL REGISTERS                    01072000
         LA    R15,8              SET DUPREC    INDICATOR               01073000
         STCM  R15,15,DICMSTAT    PUT RETURN CODE IN STATUS             01074000
         B     DIEXIT             RETURN TO EXTERNAL EXIT POINT         01075000
         EJECT                                                          01076000
*********************************************************************** 01077000
*        THIS ROUTINE WILL UNLOCK ANY ENTITIES USED BY THIS           * 01078000
*        SESSION.                                                     * 01079000
*********************************************************************** 01080000
DILBCLS  EQU *                                                          01081000
         STM   R0,R15,DILBSAVE    SAVE ALL REGISTERS                    01082000
         L     R9,DILBADDR                                              01083000
         BAS   R10,DILBINIT                                   2024183   01084000
         BAS   R10,DILBIPT        GO WRITE THE INDEX          2024183   01085000
         BAS   R10,DILBPUT        GO WRITE THE RECORD         2024183   01086000
         BNZ   DILBERR                                                  01087000
*                                                                       01088000
DILBCLS1 EQU *                                                          01089000
         B     DILBRET0                                                 01090000
         EJECT                                                          01091000
*********************************************************************** 01092000
*        THIS ROUTINE WILL CHECK TO SEE IF THE FIELD NUMBER IS        * 01093000
*        TOO HIGH.                                                    * 01094000
*********************************************************************** 01095000
DILBHGH  EQU *                                                          01096000
         STM   R0,R15,DILBSAVE    SAVE ALL REGISTERS                    01097000
         L     R9,DILBADDR                                              01098000
         BAS   R10,DILBINIT                                   2024183   01099000
         ICM   R0,1,DIFLDNUM      IS THE FIELD REQUESTED                01100000
         CLM   R0,1,SAREP#FL      HIGHER THAN THE NUM OF FLDS 2602105   01101000
         BH    DILBRET0           YES, GO RETURN 0                      01102000
         B     DILBRET1           NO, GO RETURN 1                       01103000
         TITLE 'DSLOCAL  - LITERAL POOL'                                01104000
         LTORG                                                          01105000
*********************************************************************** 01106000
*        THIS ROUTINE WILL CHECK TO SEE IF THE LOCAL BUFFER           * 01107000
*        NEEDS TO BE WRITTEN, AND WRITE IT IF IT DOES.                * 01108000
*        AND THEN IT WILL READ THE NEW RECORD INTO THE                * 01109000
*        LOCAL BUFFER.                                                * 01110000
*********************************************************************** 01111000
DILBREA  EQU *                                                          01112000
         STM   R0,R15,DILBSAVE    SAVE ALL REGISTERS                    01113000
         L     R9,DILBADDR                                              01114000
         BAS   R10,DILBINIT                                   2024183   01115000
         BAS   R10,DILBPUT        GO WRITE THE RECORD         2024183   01116000
         BNZ   DILBERR                                                  01117000
DILBREA1 EQU *                                                          01118000
         MVC   SAREPKEY,DICMDAPL(5)  MOVE KEY TO LOCAL BUFF   2602105   01119000
         MVC   SAREPSEQ,=CL8' '    SPACE OUT THE FIELD NAME   2602105   01120000
         MVC   SAREPNDX,DICINDX    LOAD THE PHYSICAL INDEX    2602105   01121000
         LA    R0,SAXF#ENT         R0  = #ENTRIES/REP-AUX XRF 2602105   01121050
         L     R1,=A(SAXFBEG)      R1  = A(1ST XREF TBL ENTRY)2602105   01121100
DILBREAI DS   0H                                              2602105   01121150
         CLC   SAREPTYP,(SAXFRTYP-SAXFBEG)(R1) REP ENT TYPE?  2602105   01121200
         BE    *+4+4+4               Y. SKIP TO CK ENT ARCH   2602105   01121250
         LA    R1,SAXFELEN(,R1)      N. SET R1=NXT REPENT TYP 2602105   01121300
         BCT   R0,DILBREAI              LOOP TO FIND ENTTYPE  2602105   01121350
*+4+4+4  EQU   *                                              2602105   01121400
         CLI   (SAXFRETY-SAXFBEG)(R1),SAXFHIER IS ENT HIER    2602105   01121450
         BNE   DILBREA2              N. LEAVE FLD NAME AS IS  2602105   01121500
         CLC   SAREPNDX,=F'0'      IS READ FOR ROOT           2602105   01121550
         BE    DILBREA2              Y. LEAVE KEY AS IS       2602105   01121600
         MVC   SAREPSEQ,=CL8'ITEM' SET ITEM EYECATCHER IN KEY 2602105   01121650
DILBREA2 DS   0H                                              2602105   01121700
         MVI   SAREP#FL,X'00'     SET NUMBER OF FIELDS TO 0   2602105   01122000
         L     R4,=A(SAREPBSZ)    LOAD THE MAXIMUM SIZE       2602105   01123000
         STH   R4,DIMVLEN                                               01124000
         LA    R4,L'SAREPKEY+1     INITIALIZE RECORD LENGTH   2602105   01125000
         STCM  R4,15,DICMRECL     TO THE FIXED LENGTH                   01126000
         CLI   DILBMODE,DILBKGE   IZT A KGE READ                        01127000
         BE    DILBREA3           YES, GO DO IT                         01128000
         ICM   R11,15,DIMODPTR                                          01129000
* THE FOLLOWING STATEMENT(S) MODIFIED BY                      2024183   01129999
         MODCB RPL=(6),RECLEN=(4),OPTCD=(KEQ,NUP),                     X01130000
               AREA=(S,SAREPKEY),ARG=(S,SAREPKEY),MF=(G,(11)) 2602105   01131000
         LR    R1,R6              SAVE RPL ADDRESS                      01132000
         B     DILBREA4                                                 01133000
DILBREA3 EQU *                                                          01134000
         ICM   R11,15,DIMODPTR                                          01135000
* THE FOLLOWING STATEMENT(S) MODIFIED BY                      2024183   01135999
         MODCB RPL=(6),RECLEN=(4),OPTCD=(KGE,NUP),                     X01136000
               AREA=(S,SAREPKEY),ARG=(S,SAREPKEY),MF=(G,(11)) 2602105   01137000
         LR    R1,R6              SAVE RPL ADDRESS                      01138000
DILBREA4 EQU *                                                          01139000
         LTR   R15,R15            ANY ERRORS?                           01140000
         BNZ   DILBERR            YES - TRANSLATE ERRORS                01141000
         GET   RPL=(6)                                                  01142000
         LA    R4,L'SAREPKEY+1     INITIALIZE RECORD LENGTH   2602105   01143000
         STCM  R4,15,DICMRECL     TO THE FIXED LENGTH                   01144000
         MVC   2(L'SAREPKYM,R5),SAREPKEY                      2602105   01145000
         LTR   R15,R15            ANY ERRORS?                           01146000
         BNZ   DILBRE4A           YES, IT MAY BE OK                     01147000
* DETERMINE IF IT IS A DUPLICATE RECORD                                 01148000
         CLC   DICMCMD,=C'INSERT' IZT AN INSERT                         01149000
         BNE   DILBREA6                                                 01150000
         CLC   SAREPNDX,=F'0'      IZT INDEX 0                2602105   01151000
         BNE   DILBREA6                                                 01152000
         MVI   SAREPKEY,C' '       DESTROY KEY                2602105   01153000
         B     DUPREC             GO REPORT A DUPLICATE RECORD          01154000
DILBRE4A EQU *                                                          01155000
         CLI   DILBFUNC,C'W'                                            01156000
         BE    DILBREA5           YES, IGNORE THE ERROR                 01157000
         B     NOTFND             NO, GO PROCESS ERROR                  01158000
DILBREA6 EQU *                                                          01159000
         MVC   2(L'SAREPKYM,R5),SAREPKEY                      2602105   01160000
         LA    R7,DICMRECL        AREA TO RECEIVE DATA                  01161000
         LR    R11,R1                                                   01162000
         ICM   R1,15,DIMODPTR       GET W/A                             01163000
         SHOWCB RPL=(11),LENGTH=8,AREA=(7),FIELDS=(RECLEN,FDBK),       X01164000
               MF=(G,(1))                                               01165000
DILBREA5 EQU *                                                          01166000
         XR    R0,R0                                                    01167000
         ICM   R0,15,DICMRECL     GET RECORD LENGTH                     01168000
         STCM  R0,3,SAREPLEN      SAVE LENGTH IN RECORD BUFF  2602105   01169000
         MVI   DICBDMOD,C' '      RESET MODIFY FLAG                     01170000
         B     DILBRET0           RETURN TO CALLER                      01171000
         EJECT                                                          01172000
*********************************************************************** 01173000
*        THIS ROUTINE WILL FIND THE POSITION OF THE SPECIFIED         * 01174000
*        FIELD NUMBER IN THE LOCAL BUFFER.                            * 01175000
*        INPUT:  DIFLDNUM - THE FIELD NUMBER TO FIND                  * 01176000
*        OUTPUT: DILBPOS  - THE ADDRESS OF THE DATA                   * 01177000
*                DILBLEN  - THE LENGTH OF THE DATA                    * 01178000
*********************************************************************** 01179000
DILBFLD  EQU *                                                          01180000
         STM   R0,R15,DILBSAVE    SAVE ALL REGISTERS                    01181000
         L     R9,DILBADDR                                              01182000
         BAS   R10,DILBINIT                                   2024183   01183000
         ICM   R0,1,SAREP#FL      GET THE NUMBER FLDS IN BUFF 2602105   01184000
         STCM  R0,1,DILBFCNT      SAVE IN THE FIELD COUNTER             01185000
         LA    R7,SAREP#FL        POINT TO THE NUMBER OF FLDS 2602105   01186000
         ST    R7,DILBPOS         SAVE THE RECORD POSITION              01187000
         LA    R7,1(,R7)          POINT TO THE FIRST FIELD              01188000
         XR    R6,R6                                                    01189000
         ICM   R6,3,SAREPLEN      LENGTH IS RECORD LENGTH     2602105   01190000
         LA    R4,L'SAREPKEY                                  2602105   01191000
         SR    R6,R4              SUBTRACT OUT FIXED LENGTH             01192000
         STH   R6,DILBLEN                                               01193000
DILBFLD1 EQU *                                                          01194000
* DIFLDNUM CONTAINS THE FIELDS NUMBER REQUESTED                         01195000
* DIFLDNUM IS DECREMENTED AS EACH FIELD IS PASSED                       01196000
* IN THIS LOOP, R7 POINTS TO THE CURRENT POSITION                       01197000
*    IN THE LOCAL BUFFER                                                01198000
         CLI   DIFLDNUM,X'00'     IS THIS THE FIELD NUMBER              01199000
         BE    DILBFLD2           GOT IT, SO GET OUT                    01200000
         CLI   DILBFCNT,X'00'     ARE WE OUT OF FIELDS                  01201000
         BNE   DILBFLDP           NO, GO POINT TO THE DATA              01202000
         MVI   0(R7),X'00'        YES, ADD AN EMPTY FIELD               01203000
         MVI   DICBDMOD,C'M'      SET MODIFY FLAG                       01204000
         XR    R1,R1                                                    01205000
         ICM   R1,1,DILBFCNT      ADD 1 TO THE FIELD COUNTER            01206000
         LA    R1,1(,R1)                                                01207000
         STC   R1,DILBFCNT                                              01208000
         CH    R1,=H'255'         MORE THAN 255 FIELDS?                 01209000
         BH    DILBERR1           YES, GOTO ERROR                       01210000
         ICM   R1,1,SAREP#FL      ADD 1 TO THE NUMBER OF      2602105   01211000
         LA    R1,1(,R1)                DEFINITIONS                     01212000
         STC   R1,SAREP#FL                                    2602105   01213000
         CH    R1,=H'255'         MORE THAN 255 FIELDS?                 01214000
         BH    DILBERR1           YES, GOTO ERROR                       01215000
         ICM   R1,3,SAREPLEN      GET RECORD LENGTH           2602105   01216000
         LA    R1,1(,R1)          ADD 1 BYTE FOR 1 FIELD                01217000
         STCM  R1,3,SAREPLEN      SAVE LENGTH IN RECORD BUFF  2602105   01218000
DILBFLDP EQU *                                                          01219000
         LA    R6,1(R7)                                                 01220000
         ST    R6,DILBPOS         RECORD POS = CURRENT POS + 1          01221000
         XR    R1,R1                                                    01222000
         ICM   R1,1,0(R7)         LENGTH = * CURRENT POS                01223000
         STH   R1,DILBLEN                                               01224000
         LA    R7,1(R1,R7)        CURRENT POS += LENGTH + 1             01225000
         ICM   R1,1,DILBFCNT      FIELD COUNTER --                      01226000
         BCTR  R1,0                                                     01227000
         STC   R1,DILBFCNT                                              01228000
         ICM   R1,1,DIFLDNUM      FIELD NUMBER --                       01229000
         BCTR  R1,0                                                     01230000
         STC   R1,DIFLDNUM                                              01231000
         B     DILBFLD1           GO LOOP AGAIN                         01232000
*                                                                       01233000
* COME HERE IF THE FIELD NUMBER WAS FOUND                               01234000
*                                                                       01235000
DILBFLD2 EQU *                                                          01236000
         B     DILBRET0           RETURN TO CALLER                      01237000
*                                                                       01238000
* COME HERE IF THE NUMBER OF FIELDS GT 255                              01239000
*                                                                       01240000
DILBERR1 EQU *                                                          01241000
         LA    R15,336            MORE THAN 255 FIELDS                  01242000
         B     DILBERR            RETURN TO CALLER                      01243000
         EJECT                                                          01244000
*********************************************************************** 01245000
*        THIS ROUTINE WILL MOVE A FIELD FROM THE LOCAL BUFFER         * 01246000
*        TO THE USERS BUFFER.                                         * 01247000
*        INPUT:  DILBPOS  - THE ADDRESS OF THE DATA                   * 01248000
*                DILBLEN  - THE LENGTH OF THE DATA                    * 01249000
*                R5       - ADDRESS OF USERS BUFFER                   * 01250000
*                DIUSRLEN - THE LENGTH OF THE USERS DATA              * 01251000
*        OUTPUT: R5       - USERS BUFFER WITH DATA INSERTED           * 01252000
*********************************************************************** 01253000
DILBMVU  EQU *                                                          01254000
         STM   R0,R15,DILBSAVE    SAVE ALL REGISTERS                    01255000
         L     R9,DILBADDR                                              01256000
         BAS   R10,DILBINIT                                   2024183   01257000
         MVC   DIMVFRM,DILBPOS    LOAD THE FROM POSITION                01258000
         LA    R1,(L'SAREPLEN+L'SAREPKEY)(,R5) LOAD TO POSITN 2602105   01259000
         ST    R1,DIMVTO                                                01260000
        LH    R1,DILBLEN         MAKE THE USERS LENGTH                  01261000
        STH   R1,DIUSRLEN          EQUAL TO THE TARGET LENGTH           01262000
         LA    R15,L'SAREPKEY(,R1)                            2602105   01264000
         STCM  R15,3,0(R5)        SAVE NEW RECORD LENGTH                01265000
         ST    R1,DIMVLEN         SAVE THE LENGTH                       01271000
         MVC   DIMVBLK,=F'256'    SET UP BLOCK SIZE                     01272000
         BAS   R10,DSMOVE         TRUNCATE THE FIELD          2024183   01273000
* THE FOLLOWING STATEMENT(S) MODIFIED BY                      2024183   01274999
         BAS   R10,DILBCAL        CALCULATE THE LENGTH OF THE RECORD    01275000
         CLC   DICMCMD,=CL6'SELECT' RETURNING DATA TO USER?   2602105   01275050
         BNE   DILBRET0              N. NO AUDIT DATA RTRND   2602105   01275100
         CLI   DICMEXT,DICMBRWS   IS THIS A BROWSE REQUEST    2602105   01275150
         BE    DILBRET0              N. NO AUDIT DATA RTRND   2602105   01275200
         CLC   SAREPKYM,DICIKEY   IS THIS HIERARCHICAL ENTITY 2602105   01275250
         BE    DILBMVUR              Y. SKIP-HIER AUDIT LOGIC 2602105   01275300
         MVC   DICMEAUD,SAREPAUD     Y. RETURN AUDIT-ENTITY   2602105   01275350
         MVC   DICMHAUD,SAREPAUD        RETURN AUDIT-HDR      2602105   01275400
         MVC   DICMIAUD,=XL40'00'       CLEAR  AUDIT-ITEM     2602105   01275450
         B     DILBRET0           RETURN TO CALLER            2602105   01275500
DILBMVUR DS   0H                                              2602105   01275550
         MVC   DICMEAUD,DICIEAUD  RETURN ENTITY AUDIT DATA    2602105   01275600
         CLC   DICINDX,=F'0'      IS THIS ROOT DATA?          2602105   01275650
         BNE   DILBMVUI              N. SKIP TO AUDIT/ITEM    2602105   01275700
         MVC   DICMHAUD,SAREPAUD     Y. RTN AUDIT DATA/ROOT   2602105   01275750
         B     DILBRET0                 RETURN TO CALLER      2602105   01275800
DILBMVUI DS   0H                                              2602105   01275850
         MVC   DICMIAUD,SAREPAUD     RETURN AUDIT DATA/ITEM   2602105   01275900
         B     DILBRET0           RETURN TO CALLER                      01276000
         EJECT                                                          01277000
*********************************************************************** 01278000
*        THIS ROUTINE WILL MOVE A FIELD FROM THE USERS BUFFER         * 01279000
*        TO THE LOCAL BUFFER.                                         * 01280000
*        INPUT:  DILBPOS  - THE ADDRESS OF THE DATA                   * 01281000
*                DILBLEN  - THE LENGTH OF THE DATA                    * 01282000
*                R5       - ADDRESS OF USERS BUFFER                   * 01283000
*                DIUSRLEN - THE LENGTH OF THE USERS DATA              * 01284000
*        OUTPUT: DILBPOS  - LOCAL BUFFER WITH DATA INSERTED           * 01285000
*********************************************************************** 01286000
DILBMVL  EQU *                                                          01287000
         STM   R0,R15,DILBSAVE    SAVE ALL REGISTERS                    01288000
         L     R9,DILBADDR                                              01289000
         BAS   R10,DILBINIT                                   2024183   01290000
         MVI   DICBDMOD,C'M'      SET MODIFY FLAG                       01291000
         MVC   DIMVTO,DILBPOS     LOAD THE TO POSITION                  01292000
         LA    R1,(L'SAREPLEN+L'SAREPKEY)(,R5) LOAD FR POSITN 2602105   01292020
         ST    R1,DIMVFRM                                     2602105   01292040
         OC    DICMCAUD,DICMCAUD   IS SADSCMAA CURR AUDIT OK  2602105   01292060
         BZ    DILBMVLI              N. NO AUDIT UPDATE       2602105   01292080
         CLC   SAREPAUD,DICMCAUD   IS AUDIT DATA ALREADY SET  2602105   01292100
         BE    DILBMVLI              Y. NO INDEX AUDIT UPDATE 2602105   01292120
         MVC   SAREPAUD,DICMCAUD     N. SET AUDIT SEGMENT     2602105   01292140
         LA    R14,DICMWORK+4      R14 = A(ENTITY TYPE TABLE) 2602105   01292160
         LA    R15,4               R15 = MAX #ENTTYPETBLSLOTS 2602105   01292180
DILBMVET DS   0H                                              2602105   01292200
         CLC   SAREPTYP,DICMETYP(R14) IS THIS ENT TYPE DATA   2602105   01292220
         BE    DILBMVEF              Y. SKIP-LOAD FL/REC LVL  2602105   01292240
         LA    R14,DICMELEN(,R14)  R14 = A(ENTTYPE TBL-NEXT)  2602105   01292260
         BCT   R15,DILBMVET        LOOP TO FIND ENT TYPE      2602105   01292280
         B     DILBMVEI            IGNORE FL/REC LVL IF ENTNF 2602105   01292300
DILBMVEF DS   0H                                              2602105   01292320
         MVC   SAREPFLV(2),DICMEFRL(R14) SET FIL/REC LEVEL    2602105   01292340
DILBMVEI DS   0H                                              2602105   01292360
         MVC   DICIEASG,SAREPASG   SET INDEX AUDIT SEGMT      2602105   01292380
**************************************************************2602105** 01292400
*        CHECK IFLAG REQUIREMENTS                             2602105 * 01292420
**************************************************************2602105** 01292440
FLG88LVL EQU   X'4000'             INDEX FLAG: 88 LEVEL       2602105   01292460
FLGDAGNS EQU   X'8000'             INDEX FLAG: NESTED DAG     2602105   01292480
FLG88LOF EQU   X'FFFF'-FLG88LVL    MASK/COB 88 LVL: OFF       2602105   01292500
FLGDAGOF EQU   X'FFFF'-FLGDAGNS    MASK/NESTED DAG: OFF       2602105   01292520
         SPACE 1                                              2602105   01292540
DILBMVLI DS   0H                                              2602105   01292560
         L     R15,4(,R13)         R15  = A(DSDIENT SAVEAREA) 2602105   01292580
         L     R15,(12*4)(,R15)    R15  = A(CUR FLD TBL ENTRY)2602105   01292600
         CLC   DICM1TYP(3,R15),=CL3'DAG' DO WE HAVE DAG DATA  2602105   01292620
         BNE   DILBMVLS              N. SKIP DAG EDITS        2602105   01292640
         CLC   DICM1NAM(8,R15),=CL8'ENTNAME' IS THIS ENT NM   2602105   01292660
         BE    DILBDAGN              Y. GO CK FOR NESTED DAG  2602105   01292680
         CLC   DICM1NAM(8,R15),=CL8'ENTLVL' IS THIS COBOL LVL 2602105   01292700
         BNE   DILBMVLS              N. SKIP CK FOR 88 LEVELS 2602105   01292720
         L     R14,=A(FLG88LOF)    R14= FIELD IS ENTLVL       2602105   01292740
         CLC   0(2,R1),=CL2'88'    IS THIS AN 88 LEVEL        2602105   01292760
         BNE   DILBMVLS            SKIP OUT OF DAG EDITS      2602105   01292780
         L     R15,=A(FLG88LVL)    R15 = ITEM IS 88 LEVEL     2602105   01292800
         B     DILBIFST            SKIP TO SET INDEX FLAG     2602105   01292820
DILBDAGN DS   0H                                              2602105   01292840
         L     R14,=A(FLGDAGOF)    R14= FIELD IS ENTNAME      2602105   01292860
         CLI   0(R1),C'@'          IS THIS A NESTED DAGNAME   2602105   01292880
         BNE   DILBMVLS              N. SKIP OUT OF DAG EDITS 2602105   01292900
         L     R15,=A(FLGDAGNS)    R15 = ITEM IS NESTED DAGNM 2602105   01292920
DILBIFST DS   0H                                              2602105   01292940
         LA    R0,0                R0  = CLEARED              2602105   01292960
         ICM   R0,B'0011',DICIDNUM R0  = # INDEXES (RECORD)   2602105   01292980
         LA    R1,DICIDATA+2       R1  = A(INDEXES)           2602105   01293000
         ICM   R4,B'1111',SAREPNDX R4  = INDEX FOR CUR REC    2602105   01293020
DILBIFLO DS   0H                                              2602105   01293040
         ICM   R6,B'0011',0(R1)    R6  = INDEX W/FLAGS        2602105   01293060
         N     R6,=X'00000FFF'     R6  = INDEX W/O FLAGS      2602105   01293080
         CR    R4,R6               IS THIS INDEX FOR CUR REC  2602105   01293100
         BE    DILBIFLG              Y. SKIP TO SET FLAGS     2602105   01293120
         LA    R1,2(,R1)           R1  = A(NEXT INDEX)        2602105   01293140
         BCT   R0,DILBIFLO           N. LOOP TO LOCATE INDEX  2602105   01293160
         B     DILBMVLS            IGNORE IFLAGS IF INDEX ERR 2602105   01293180
DILBIFLG DS   0H                                              2602105   01293200
         ICM   R6,B'0011',0(R1)    R6  = INDEX W/FLAGS        2602105   01293220
         NR    R6,R14              CLEAR ENTNAME/ENTLVL FLAG  2602105   01293240
         OR    R6,R15              SET ENTNAME/ENTLVL FLAG    2602105   01293260
         STCM  R6,B'0011',0(R1)    UPDATE INDEX W/FLAGS       2602105   01293280
DILBMVLS DS   0H                                              2602105   01293300
         LH    R7,DIUSRLEN        GET THE USERS LENGTH OF THE DATA      01295000
         LA    R1,L'SAREPKEY(,R7)                             2602105   01296000
         STCM  R1,3,0(R5)         SAVE NEW RECORD LENGTH                01297000
         LA    R0,SAREP#FL                                    2602105   01298000
         L     R4,DILBPOS                                               01299000
         LH    R1,DILBLEN         GET THE TARGET LENGTH OF THE DATA     01300000
         CR    R0,R4              IZT MOVE THE WHOLE RECORD             01301000
         BE    DILBMVC1           YES, GO MOVE THE WHOLE RECORD         01302000
         CR    R1,R7              IS TARGET LEN EQ USERS LEN            01303000
         BE    DILBMVC1           YES, GO MOVE THE FIELD                01304000
         BL    DILBMVL1           IF LT, GO INSERT                      01305000
* IF RECORD LEN GT USERS LEN, DELETE BYTES                              01306000
* R1 = TARGET LEN,  R7 = SOURCE LEN, R4 = TARGET ADDR, R8 = SOURCE ADDR 01307000
         MVC   DIMVTO,DILBPOS     LOAD THE TO POSITION                  01308000
         SR    R1,R7              FIND THE DIFFERENCE                   01309000
         ICM   R7,3,SAREPLEN      MOVELEN=RECL-TRUNC+STRT-POS 2602105   01310000
         SR    R7,R1                                                    01311000
         STCM  R7,3,SAREPLEN        SAVE NEW RECORD LENGTH    2602105   01312000
         LA    R15,SAREPKEY                                   2602105   01313000
         AR    R7,R15               R7 NOW HAS END OF RECORD            01314000
         SR    R7,R4                R7 NOW HAS LENGTH TO MOVE           01315000
         LA    R15,0(R1,R4)       FROMPOS = POS + TRUNC                 01316000
         ST    R15,DIMVFRM                                              01317000
         ST    R7,DIMVLEN         SET THE LENGTH                        01318000
         MVC   DIMVBLK,=F'256'    SET BLOCK SIZE 256                    01319000
         BAS   R10,DSMOVE         TRUNCATE THE FIELD          2024183   01320000
         LH    R1,DIUSRLEN        GET THE USERS LENGTH OF THE DATA      01321000
         LA    R15,(L'SAREPLEN+L'SAREPKEY)(,R5) LOAD FR POS   2602105   01322000
         ST    R15,DIMVFRM                                              01323000
         L     R15,DILBPOS        LOAD THE  TO  POSITION                01324000
         BCTR  R15,0                                                    01325000
         STCM  R1,1,0(R15)        SAVE LENGTH IN LOCAL BUFFER           01326000
         LA    R15,1(R15)                                               01327000
         ST    R15,DIMVTO         SAVE THE  TO  POSITION                01328000
DILBMVC1 EQU   *                                                        01329000
         ST    R1,DIMVLEN         SAVE THE LENGTH                       01330000
         MVC   DIMVBLK,=F'256'    GET BLOCK SIZE 256                    01331000
         BAS   R10,DSMOVE         TRUNCATE THE FIELD          2024183   01332000
* THE FOLLOWING STATEMENT(S) MODIFIED BY                      2024183   01332999
         BAS   R10,DILBCAL        CALCULATE THE LENGTH OF THE RECORD    01333000
         B     DILBRET0           RETURN TO CALLER                      01334000
* IF RECORD LEN LT USERS LEN, INSERT BYTES                              01335000
* R1 = TARGET LEN,  R7 = SOURCE LEN, R4 = TARGET ADDR, R6 = SOURCE ADDR 01336000
DILBMVL1 EQU *                                                          01337000
         SR    R7,R1              FIND THE DIFFERENCE                   01338000
         ICM   R1,3,SAREPLEN      ADD PAD AMOUNT TO REC LEN   2602105   01339000
         AR    R1,R7                                                    01340000
         STCM  R1,3,SAREPLEN      SAVE NEW RECORD LENGTH      2602105   01341000
         SR    R1,R7              MOVELEN = RECL + START - POS          01342000
         LA    R6,SAREPKEY                                    2602105   01343000
         AR    R1,R6                                                    01344000
         LR    R6,R1              SAVE THE END OF RECORD                01345000
         L     R4,DILBPOS                                               01346000
         SR    R1,R4                                                    01347000
         LA    R4,0(R7,R6)        CALC NEW END OF RECORD                01348000
         ST    R1,DIMVLEN                                               01349000
         ST    R7,DIMVBLK                                               01350000
         ST    R4,DIMVTO                                                01351000
         ST    R6,DIMVFRM                                               01352000
         BAS   R10,DSMOVEB        INSERT CHARACTERS           2024183   01353000
         LA    R4,(L'SAREPLEN+L'SAREPKEY)(,R5) LOAD FR POSITN 2602105   01354000
         ST    R4,DIMVFRM                                               01355000
         L     R4,DILBPOS         LOAD THE  TO  POSITION                01356000
         ST    R4,DIMVTO                                                01357000
         BCTR  R4,0                                                     01358000
         XR    R1,R1                                                    01359000
         LH    R1,DIUSRLEN        GET THE USERS LENGTH OF THE DATA      01360000
         ST    R1,DIMVLEN                                               01361000
         STCM  R1,1,0(R4)         SAVE LENGTH IN LOCAL BUFFER           01362000
         B     DILBMVC1           DO THE MOVE                           01363000
         EJECT                                                          01364000
*********************************************************************** 01365000
*        THIS ROUTINE WILL CALCULATE THE LENGTH OF THE LOCAL          * 01366000
*        BUFFER.                                                      * 01367000
*********************************************************************** 01368000
DILBCAL  EQU *                                                          01369000
         STM   R0,R15,DIMVSAVE    SAVE ALL REGISTERS                    01370000
         LA    R5,SAREPVAR        LOAD STARTING POSITION+ KEY 2602105   01371000
         XR    R6,R6                                                    01372000
         IC    R6,SAREP#FL        NUMBER OF DATA FIELDS       2602105   01373000
DILBCAL1 EQU *                                                          01374000
         LTR   R6,R6              ARE WE OUT OF FIELDS                  01375000
         BE    DILBCAL2           YES, GET OUT                          01376000
         XR    R7,R7                                                    01377000
         IC    R7,0(,R5)          GET THE FIELD LENGTH                  01378000
         LA    R5,1(R7,R5)        ADD FIELD LENGTH TO ADDRESS + 1       01379000
         BCTR  R6,0               DECREMENT FIELD COUNTER               01380000
         B     DILBCAL1           LOOP BACK                             01381000
DILBCAL2 EQU *                                                          01382000
         LA    R6,SAREPKEY        LOAD STARTING POSITION      2602105   01383000
         SR    R5,R6              FIND THE LENGTH                       01384000
         C     R5,=A(SAREPBSZ)    IS THE LENGTH GT 4095       2602105   01385000
         BH    DILBCAL3           YES, GO DO ERROR                      01386000
         STCM  R5,3,SAREPLEN      STORE THE LENGTH            2602105   01387000
         LM    R0,R15,DIMVSAVE    RELOAD REGISTERS                      01388000
         BR    R10                                                      01389000
DILBCAL3 EQU *                                                          01390000
         LM    R0,R15,DIMVSAVE    RELOAD REGISTERS                      01391000
         LA    R15,337            RECORD GT 4096                        01392000
         B     DILBERR                                                  01393000
         EJECT                                                          01394000
*********************************************************************** 01395000
*        THIS ROUTINE WILL DELETE AN ENTIRE ENTITY                    * 01396000
*********************************************************************** 01397000
DILBDEL  EQU *                                                          01398000
         STM   R0,R15,DILBSAVE    SAVE ALL REGISTERS                    01399000
         L     R9,DILBADDR                                              01400000
         BAS   R10,DILBINIT                                   2024183   01401000
         BAS   R10,DILBIPT        GO WRITE THE INDEX          2024183   01402000
         BNZ   DILBERR                                                  01403000
         BAS   R10,DILBPUT        GO WRITE THE RECORD         2024183   01404000
         BNZ   DILBERR                                                  01405000
         MVC   SAREPKEY,=XL40'00'  MOVE LOW VALUES TO KEY     2602105   01406000
         MVC   SAREPKYM,DICMDAPL(R5)  MOVE FIRST PART OF KEY  2602105   01407000
         L     R4,=F'32767'                                             01408000
         ICM   R11,15,DIRPLD       GET RPL ADDRESS                      01409000
         ICM   R1,15,DIMODPTR                                           01410000
* THE FOLLOWING STATEMENT(S) MODIFIED BY                      2024183   01410999
         MODCB RPL=(11),OPTCD=(KGE,UPD),RECLEN=(4),AREA=(S,DICMSAVE),  X01411000
               ARG=(S,SAREPKEY),MF=(G,(1))                    2602105   01412000
         LR    R1,R11             SAVE RPL ADDRESS                      01413000
         LTR   R15,R15            ANY ERRORS?                           01414000
         BNZ   DILBERR            YES - TRANSLATE ERRORS                01415000
         XR    R6,R6              CLEAR DELETE COUNTER                  01416000
DILBDEL1 EQU *                                                          01417000
         ICM   R1,15,DIRPLD       GET RPL ADDRESS                       01418000
         GET   RPL=(1)                                                  01419000
         LTR   R15,R15            WAS IT FOUND                          01420000
         BNZ   DILBDEL3           NO, END OF DATA                       01421000
         L     R1,DICMSAVE                                              01422000
         MVC   SAREPKEY,0(R1)      COPY KEY THAT WAS FOUND    2602105   01423000
         CLC   SAREPKYM,DICMDAPL(R5)  DO THE KEYS MATCH       2602105   01424000
         BNE   DILBDEL3           NO, END OF DATA                       01425000
         ICM   R1,15,DIRPLD       GET RPL ADDRESS                       01426000
         ERASE RPL=(1)                                                  01427000
         LTR   R15,R15            WAS IT DELETED                        01428000
         BNZ   DILBDEL4           NO GO DO ERROR                        01429000
         ICM   R15,15,DICMNACC    GET NUMBER OF ACCESSES THIS REQUEST   01430000
         LA    R15,1(R15)         INCREMENT BY ONE                      01431000
         STCM  R15,15,DICMNACC    RESTORE NUMBER OF ACCESSES            01432000
DILBDEL2 EQU *                                                          01433000
         LA    R6,1(,R6)          BUMP THE DELETE COUNTER               01434000
         ICM   R15,15,SAREPNDX     BUMP THE INDEX             2602105   01435000
         LA    R15,1(,R15)                                              01436000
         STCM  R15,15,DICMKNDX                                          01437000
         B     DILBDEL1           GO DELETE NEXT                        01438000
DILBDEL3 EQU *                                                          01439000
         MVI   DICIDMOD,C' '      INDEX DOES NOT NEED WRITTEN           01440000
         MVI   DICBDMOD,C' '      RECORD DOES NOT NEED WRITTEN          01441000
         MVC   SAREPKEY,=XL40'00'  MOVE LOW VALUES TO KEY     2602105   01442000
         ICM   R1,15,DIRPLD       GET RPL ADDRESS                       01443000
         ENDREQ RPL=(1)                                                 01444000
         ICM   R11,15,DIRPLD       GET RPL ADDRESS                      01445000
         ICM   R1,15,DIMODPTR                                           01446000
         MODCB RPL=(11),OPTCD=(KEQ),MF=(G,(1))                2024183   01447000
         MVC   DICIKEY,=XL40'00'          CLEAR KEY           2602105   01448000
         LTR   R6,R6              WAS ENTITY THERE ?                    01449000
         BE    NOTFND             NO, GIVE A NOT FOUND ERROR            01450000
         B     DILBRET0                                                 01451000
DILBDEL4 EQU *                                                          01452000
         MVI   DICIDMOD,C' '      INDEX DOES NOT NEED WRITTEN           01453000
         MVI   DICBDMOD,C' '      RECORD DOES NOT NEED WRITTEN          01454000
         MVC   SAREPKEY,=XL40'00'  MOVE LOW VALUES TO KEY     2602105   01455000
         ICM   R1,15,DIRPLD       GET RPL ADDRESS                       01456000
         ENDREQ RPL=(1)                                                 01457000
         ICM   R11,15,DIRPLD       GET RPL ADDRESS                      01458000
         ICM   R1,15,DIMODPTR                                           01459000
         MODCB RPL=(11),OPTCD=(KEQ),MF=(G,(1))                2024183   01460000
         LR    R1,R11             SAVE RPL ADDRESS                      01461000
         MVC   DICIKEY,=XL40'00'          CLEAR KEY           2602015   01462000
         LA    R15,338                                                  01463000
         B     DILBERR                                                  01464000
         EJECT                                                          01465000
*********************************************************************** 01466000
*        THIS ROUTINE WILL DELETE THE CURRENT ITERATION               * 01467000
*********************************************************************** 01468000
DILBDL1  EQU *                                                          01469000
         STM   R0,R15,DILBSAVE    SAVE ALL REGISTERS                    01470000
         L     R9,DILBADDR                                              01471000
         BAS   R10,DILBINIT                                   2024183   01472000
         L     R4,=F'32767'                                             01473000
         ICM   R11,15,DIRPLD       GET RPL ADDRESS                      01474000
         ICM   R1,15,DIMODPTR                                           01475000
* THE FOLLOWING STATEMENT(S) MODIFIED BY                      2024183   01475999
         MODCB RPL=(11),OPTCD=(KEQ,UPD),RECLEN=(4),AREA=(S,DICMSAVE),  X01476000
               ARG=(S,SAREPKEY),MF=(G,(1))                    2602105   01477000
         LR    R1,R11             SAVE RPL ADDRESS                      01478000
         LTR   R15,R15            ANY ERRORS?                           01479000
         BNZ   DILBERR            YES - TRANSLATE ERRORS                01480000
         ICM   R1,15,DIRPLD       GET RPL ADDRESS                       01481000
         GET   RPL=(1)                                                  01482000
         LTR   R15,R15            WAS IT FOUND                          01483000
         BNZ   DILBDL11           NO, DELETE THE INDEX ANYWAY           01484000
         ICM   R1,15,DIRPLD       GET RPL ADDRESS                       01485000
         ERASE RPL=(1)                                                  01486000
         LTR   R15,R15            WAS IT DELETED                        01487000
         BNZ   DILBERR            NO, GO DO ERROR                       01488000
* NOW DELETE THE INDEX FROM THE RECORD                                  01489000
DILBDL11 EQU *                                                          01490000
         XR    R2,R2                                                    01491000
         ICM   R2,3,DICMDNDX+2(5) LOAD THE REQUESTED INDEX              01492000
         MVC   DIMVBLK,=F'256'    MOVE 256 BYTES AT A TIME              01493000
         LA    R1,DICIDATA                                              01494000
         SLL   R2,1               MULTIPLY BY 2                         01495000
         AR    R1,R2                                                    01496000
         ST    R1,DIMVTO          MOVE TO HERE                          01497000
         LA    R1,2(,R1)                                                01498000
         ST    R1,DIMVFRM         MOVE FROM HERE                        01499000
         XR    R1,R1                                                    01500000
         ICM   R1,3,DICIDNUM      GET HINDEX                            01501000
         XR    R2,R2                                                    01502000
         ICM   R2,3,DICMDNDX+2(5) LOAD THE REQUESTED INDEX              01503000
         SR    R1,R2              HOW MANY COME AFTER THIS              01504000
         SLL   R1,1               MULTILY RESULT BY 2                   01505000
         ST    R1,DIMVLEN         MOVE THIS LENGTH                      01506000
         XR    R1,R1                                                    01507000
         ICM   R1,3,DICIDNUM      DECREMENT HINDEX                      01508000
         BCTR  R1,0                                                     01509000
         STCM  R1,3,DICIDNUM                                            01510000
         BAS   R10,DSMOVE         DELETE THE INDEX            2024183   01511000
         MVI   DICIDMOD,C'M'      INDEX HAS BEEN MODIFIED               01512000
         MVI   DICBDMOD,C' '      RECORD DOES NOT NEED WRITTEN          01513000
         MVC   SAREPKEY,=XL40'00'  MOVE LOW VALUES TO KEY     2602105   01514000
         B     DILBRET0                                                 01515000
         EJECT                                                          01516000
*********************************************************************** 01517000
*        THIS ROUTINE WILL CHECK TO SEE IF THE LOCAL BUFFER           * 01518000
*        NEEDS TO BE WRITTEN, AND WRITE IT IF IT DOES.                * 01519000
*********************************************************************** 01520000
DILBPUT  EQU *                                                          01521000
         XR    R15,R15                                                  01522000
         STM   R0,R15,DILBSAV1    SAVE ALL REGISTERS                    01523000
         L     R9,DILBADDR                                              01524000
         BAS   R10,DILBINIT                                   2024183   01525000
DILBPUTA EQU *                                                          01526000
         CLI   DICBDMOD,C'M'      HAS THE LOCAL BUFFER BEEN MODIFIED    01527000
         BNE   DILBPUTX           NO, DO NOT WRITE OUT                  01528000
         L     R4,=F'32767'                                             01529000
         ICM   R11,15,DIRPLD       GET RPL ADDRESS                      01530000
         ICM   R1,15,DIMODPTR                                           01531000
* THE FOLLOWING STATEMENT(S) MODIFIED BY                      2024183   01531999
         MODCB RPL=(11),RECLEN=(4),AREA=(S,DICMSAVE),         2602105  X01532000
               ARG=(S,SAREPKEY),MF=(G,(1))                    2602105   01533000
         LR    R1,R11             SAVE RPL ADDRESS                      01534000
         LTR   R15,R15            ANY ERRORS?                           01535000
         BNZ   DILBERRP           YES - TRANSLATE ERRORS                01536000
         ICM   R1,15,DIRPLD       GET RPL ADDRESS                       01537000
         GET   RPL=(1)                                                  01538000
         LTR   R15,R15            WAS IT FOUND                          01539000
         BNZ   DILBPUT2           NO GO PUT IT                          01540000
         ICM   R1,15,DIRPLD       GET RPL ADDRESS                       01541000
         ERASE RPL=(1)                                                  01542000
         LTR   R15,R15            ANY ERRORS?                           01543000
         BNZ   DILBERRP           YES - TRANSLATE ERRORS                01544000
DILBPUT2 EQU *                                                          01545000
         XR    R4,R4                                                    01546000
         ICM   R4,3,SAREPLEN      LOAD THE LENGTH             2602105   01547000
         ICM   R11,15,DIMODPTR                                          01548000
* THE FOLLOWING STATEMENT(S) MODIFIED BY                      2024183   01548999
         MODCB RPL=(6),RECLEN=(4),OPTCD=(KEQ),                2602105  X01549000
               AREA=(S,SAREPKEY),ARG=(S,SAREPKEY),MF=(G,(11)) 2602105   01550000
         LR    R1,R11             SAVE RPL ADDRESS                      01551000
         LTR   R15,R15            ANY ERRORS?                           01552000
         BNZ   DILBERRP           YES - TRANSLATE ERRORS                01553000
         PUT   RPL=(6)                                                  01554000
         LTR   R15,R15            ANY ERRORS?                           01555000
         BNZ   DILBERRP           YES - TRANSLATE ERRORS                01556000
DILBPUT1 EQU *                                                          01557000
         MVI   DICBDMOD,C' '      RESET MODIFY FLAG                     01558000
         MVI   SAREPKEY,C' '       DESTROY KEY                2602105   01559000
         B     DILBRETP                                                 01560000
DILBPUTX EQU *                                                          01561000
         MVI   DICBDMOD,C' '      RESET MODIFY FLAG                     01562000
         MVI   SAREPKEY,C' '       DESTROY KEY                2602105   01563000
         B     DILBRETX                                                 01564000
         EJECT                                                          01565000
*********************************************************************** 01566000
*        THIS ROUTINE WILL CHECK TO SEE IF THE INDEX                  * 01567000
*        NEEDS TO BE WRITTEN, AND WRITE IT IF IT DOES.                * 01568000
*********************************************************************** 01569000
DILBIPT  EQU *                                                          01570000
         XR    R15,R15                                                  01571000
         STM   R0,R15,DILBSAV1    SAVE ALL REGISTERS                    01572000
         L     R9,DILBADDR                                              01573000
         BAS   R10,DILBINIT                                   2024183   01574000
DILBIPTA EQU *                                                          01576000
         CLI   DICIDMOD,C'M'      HAS THE INDEX BEEN MODIFIED 9913569   01576700
         BNE   DILBO600           NO, GO READ THE RIGHT ONE   9913569   01577400
         ICM   R11,15,DIRPLI       GET RPL ADDRESS            9913569   01578100
         ICM   R1,15,DIMODPTR                                 9913569   01578800
         LA    R4,DILOCAL          R4 = A(LOCAL DICT BUFFERAR)9913569   01579500
         A     R4,=A(DIINDXDL-DILOCAL) R4 = A(INDX DLET WKAR) 9913569   01580200
         MVC   0(L'DICIKEY,R4),DICIKEY INIT INDEX KEY         9913569   01580900
* THE FOLLOWING STATEMENT(S) MODIFIED BY                      2024183   01581599
         MODCB RPL=(11),MF=(G,(1)),                           9913569  X01581600
               ARG=(4),AREA=(4),AREALEN=4096,                 9913569  X01582300
               OPTCD=(KEQ,UPD,MVE),RECLEN=(S,DIMVLEN)         9913569   01583000
         LR    R1,R11             SAVE RPL ADDRESS            9913569   01583700
         LTR   R15,R15            ANY ERRORS                  9913569   01584400
         BNZ   DILBERRP             Y. EXIT W/ERRORS          9913569   01585100
DILBO100 DS   0H                                              9913569   01585800
         ICM   R1,B'1111',DIRPLI  R1 = A(SAREP INDEX RPL)     9913569   01586500
         GET   RPL=(1)            READ INDEX RECORD           9913569   01587200
         LTR   R15,R15            ANY VSAM ERRORS (NOT FOUND) 9913569   01587900
         BNZ   DILBO200             Y. IGNORE/GO WRITE INDEX  9913569   01588600
         CLC   DICIKEY(21),0(R4)  IS THIS INDEX/CURR ENTITY   9913569   01589300
         BNE   DILBO200             N. ERASE DONE/GO PUT INDX 9913569   01590000
         ICM   R1,B'1111',DIRPLI  R1 = A(SAREP INDEX RPL)     9913569   01590700
         ERASE RPL=(1)            ERASE CURRENT INDEX REC     9913569   01591400
         LTR   R15,R15            ANY ERRORS                  9913569   01592100
         BNZ   DILBERRP             Y. EXIT W/ERRORS          9913569   01592800
         ICM   R1,B'1111',(DICIKIDX-DICIKEY)(R4) INDEX IND/CUR9913569   01593500
         LA    R1,1(,R1)          R1 = INDEX INDEX/NEXT       9913569   01594200
         STCM  R1,B'1111',(DICIKIDX-DICIKEY)(R4) RESET INDEX  9913569   01594900
         B     DILBO100           LOOP TO DELETE INDEX RECS   9913569   01595600
DILBO200 DS   0H                                              9913569   01596300
*-------------------------------------------------------------9913569-* 01597000
* CALCULATE SIZE OF LOGICAL INDEX AND LOAD TO R2              9913569 * 01597700
*-------------------------------------------------------------9913569-* 01598400
         LA    R1,0                R1  = CLEARED              9913569   01599100
         ICM   R1,B'0011',DICIDNUM R1  = # LOGICAL INDEXES    9913569   01599800
         SLL   R1,1                R1  = L'LOGICAL INDEXES    9913569   01600500
         LA    R2,(DICIDATA+2-DICIKEY)(R1) R2=L'INDEX/LOGICAL 9913569   01601200
         LA    R4,DICIKEY          R4 = A(INDEX REC TO WRITE) 9913569   01601900
         L     R5,=F'4096'         INIT MAX L'INDEX REC/PHYS  9913569   01602600
         MVC   DIINDXRD,NULLS      INIT REPLACED DATA SAVEAREA9913569   01603300
DILBO300 DS   0H                                              9913569   01604000
         CR    R2,R5               MORE THAN 1 INDEX REC TO WR9913569   01604700
         BNL   DILBO400              Y. WRITE INDEX/LEN=4096  9913569   01605400
         LR    R5,R2                 N. RESET RECLEN=LAST INDX9913569   01606100
DILBO400 DS   0H                                              9913569   01606800
         ICM   R11,15,DIRPLI       R11= A(SAREP INDEX RPL)    9913569   01607500
         ICM   R1,15,DIMODPTR      R1 = A(MODCB WORK AREA)    9913569   01608200
* THE FOLLOWING STATEMENT(S) MODIFIED BY                      2024183   01608899
         MODCB RPL=(11),MF=(G,(1)),                           9913569  X01608900
               ARG=(4),AREA=(4),AREALEN=4096,                 9913569  X01609600
               OPTCD=(KEQ,NUP,MVE),RECLEN=(5)                 9913569   01610300
         LR    R1,R11             SAVE RPL ADDRESS            9913569   01611000
         LTR   R15,R15            ANY ERRORS                  9913569   01611700
         BNZ   DILBERRP           YES - TRANSLATE ERRORS      9913569   01612400
         ICM   R1,15,DIRPLI       GET RPL ADDRESS             9913569   01613100
         PUT   RPL=(1)                                        9913569   01613800
         LTR   R15,R15            ANY ERRORS                  9913569   01614500
         BNZ   DILBERRP           YES - TRANSLATE ERRORS      9913569   01615200
         CLC   DIINDXRD,NULLS      DID WE OVERLAY INDEX DATA  9913569   01615900
         BE    DILBO500              N. SKIP DATA RESTORE     9913569   01616600
         MVC   0(25,R4),DIINDXRD     Y. RESTORE DATA OVERLAID 9913569   01617300
DILBO500 DS   0H                                              9913569   01618000
         SR    R2,R5               IS THERE MORE INDEX DATA   9913569   01618700
         BNP   DILBO600              N. GO RETURN TO CALLER   9913569   01619400
         LA    R2,L'DICIKEY(,R2)   R2 = L'INDEX TO WR/LOGICAL 9913569   01620100
         MVC   DIINDXRD,(4096-25)(R4) SAVE DATA TO BE OVERLAID9913569   01620800
         MVC   (4096-25)(25,R4),0(R4) SET KEY FOR NXT INDEXREC9913569   01621500
         LA    R4,(4096-25)(,R4)   R4 = A(NEXT INDEX TO WRITE)9913569   01622200
         ICM   R1,B'1111',(DICIKIDX-DICIKEY)(R4) INDEX/PREV   9913569   01622900
         LA    R1,1(,R1)           R1 = INDEX INDEX/NEXT      9913569   01623600
         STCM  R1,B'1111',(DICIKIDX-DICIKEY)(R4) SET INDX/NEXT9913569   01624300
         B     DILBO300            GO WRITE NEXT INDEX RECORD 9913569   01625000
DILBO600 DS   0H                                              9913569   01625700
         MVI   DICIDMOD,C' '      RESET MODIFY FLAG                     01629000
         MVI   DICIKEY,C' '       DESTROY KEY                           01630000
         B     DILBRETX                                                 01631000
DILBERRP EQU *                                                          01632000
         STM   R0,R1,DILBSAV1     SAVE FIRST TWO                        01633000
         ST    R15,DILBSAV1+60    SAVE R15                              01634000
DILBRETP EQU *                                                          01635000
DILBRETX EQU *                                                          01636000
         LM    R0,R15,DILBSAV1    LOAD ALL REGISTERS                    01637000
         LTR   R15,R15            TEST RETURN CODE                      01638000
         BR    R10                LET CALLER HANDLE ERRORS              01639000
         EJECT                                                          01640000
*                                                                       01641000
* THIS ROUTINE WILL MOVE A BLOCK OF MEMORY                              01642000
*                                                                       01643000
* R1 - LENGTH                                                           01644000
* R2 - BLOCK                                                            01645000
* R5 - FROM ADDRESS                                                     01646000
* R6 - TO   ADDRESS                                                     01647000
* R7 - RETURN ADDRESS                                                   01648000
*                                                                       01649000
DSMOVE   EQU *                                                          01650000
         STM   R0,R15,DIMVSAVE    SAVE REGISTERS                        01651000
         L     R1,DIMVLEN                                               01652000
         L     R2,DIMVBLK                                               01653000
         L     R5,DIMVFRM                                               01654000
         L     R6,DIMVTO                                                01655000
DSMOVEL  EQU *                                                          01656000
         CR    R1,R2              IS THE LENGTH GT BLOCK                01657000
         BH    DSMOVE1            YES, GO MOVE ONE BLOCK                01658000
         LTR   R1,R1              IS THE LENGTH ZERO                    01659000
         BE    DSMOVEX            YES, GET OUT                          01660000
         LR    R2,R1                                                    01661000
DSMOVE1  EQU *                                                          01662000
         BCTR  R2,0               DECREMENT LENGTH                      01663000
         EX    R2,DSMOVE2         MOVE ONE BLOCK                        01664000
         LA    R2,1(R2)           INCREMENT LENGTH                      01665000
         AR    R5,R2              MOVE UP ONE BLOCK                     01666000
         AR    R6,R2                                                    01667000
         SR    R1,R2                                                    01668000
         B     DSMOVEL            GO LOOP AGAIN                         01669000
DSMOVE2  MVC   0(0,R6),0(R5)                                            01670000
DSMOVEX  LM    R0,R15,DIMVSAVE    RELOAD REGISTERS                      01671000
         BR    R10                                                      01672000
*                                                                       01673000
* THIS ROUTINE WILL MOVE A BLOCK OF MEMORY BACKWARDS                    01674000
*                                                                       01675000
* R1 - LENGTH                                                           01676000
* R2 - BLOCK                                                            01677000
* R5 - FROM ADDRESS                                                     01678000
* R6 - TO   ADDRESS                                                     01679000
* R7 - RETURN ADDRESS                                                   01680000
*                                                                       01681000
DSMOVEB  EQU *                                                          01682000
         STM   R0,R15,DIMVSAVE    SAVE REGISTERS                        01683000
         L     R1,DIMVLEN                                               01684000
         L     R2,DIMVBLK                                               01685000
         L     R5,DIMVFRM                                               01686000
         L     R6,DIMVTO                                                01687000
DSMOVEBL EQU *                                                          01688000
         CR    R1,R2              IS THE LENGTH GT BLOCK                01689000
         BH    DSMOVEB1           YES, GO MOVE ONE BLOCK                01690000
         LTR   R1,R1              IS THE LENGTH ZERO                    01691000
         BE    DSMOVEX            YES, GET OUT                          01692000
         LR    R2,R1                                                    01693000
DSMOVEB1 EQU *                                                          01694000
         SR    R5,R2              MOVE BACK ONE BLOCK                   01695000
         SR    R6,R2                                                    01696000
         SR    R1,R2                                                    01697000
         BCTR  R2,0               DECREMENT LENGTH                      01698000
         EX    R2,DSMOVE2         MOVE ONE BLOCK                        01699000
         LA    R2,1(R2)           INCREMENT LENGTH                      01700000
         B     DSMOVEBL           GO LOOP AGAIN                         01701000
         TITLE 'DSLOCAL  - LITERAL POOL'                                01702000
NULLS    DC    XL80'00'                                       9913569   01702100
         LTORG                                                          01703000
         DROP                                                           01704000
*                                                                       01705000
         EJECT                                                          01706000
         PRINT GEN                                                      01707000
         COPY  SADSPARA                                                 01708000
         COPY  SADSDEFA                                                 01709000
         COPY  SADSENTA                                                 01710000
         COPY  SADSMSGA                                                 01711000
SADSNUCB CSECT                                                          01712000
         LTORG                                                          01713000
         DS    0D                                                       01714000
SITMSTMP DC    CL64'SADSNUCB  -----TSD-             05/17/00  13.09.17' 01715000
*        THIS PROGRAM CONTAINS TRADE SECRETS THAT BELONG                01715001
*        TO ALLTEL INFORMATION SERVICES, INC. AND IS                    01715002
*        LICENSED BY AN AGREEMENT.  ANY UNAUTHORIZED ACCESS,            01715003
*        USE, DUPLICATION, OR DISCLOSURE IS UNLAWFUL.                   01715004
*        COPYRIGHT ALLTEL INFORMATION SERVICES, INC.                    01715005
*        2000, ALL RIGHTS RESERVED.                                     01715006
         END                                                            01716000
