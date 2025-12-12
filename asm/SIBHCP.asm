*ASM XOPTS(NOEPILOG)                                          2024183   00000080
*     * 802262 12/11/20 PROYECTO UPGRADE GN172 EDVR
         LCLB  &CICS                                          2024183   00000090
         AIF   ('&SYSPARM' NE 'CICS').GEN1                    2021627   00000092
&CICS    SETB  1                                              2021627   00000094
         TITLE 'SIBHCP  - COMPRESS BALANCE HISTORY REC  '     2021627   00000096
         SIRENT                                               2024422   00000104
FIBHCP   DFHEIENT CODEREG=(12),DATAREG=(13),EIBREG=(11)       2024183   00000106
FIBHCP   AMODE 31                                             2024183   00000108
FIBHCP   RMODE ANY                                            2024183   00000110
         SIEQREG                                                        00000112
         L     R1,DFHEICAP         LOAD ADDR OF COMMON AREA             00000114
         B     *+24                                                     00000116
         DC    CL20'FIBHCP  (SIBHCP  )'                                 00000118
         AGO   .GEN2                                                    00000120
.GEN1    ANOP                                                           00000122
         TITLE 'SIBHCP  - COMPRESS BALANCE HISTORY REC  '               00000124
SIBHCP   START                                                          00000126
         SIBASE BASEREG=12,RWNUM=0001,RWSUB=000                         00000128
.GEN2    ANOP                                                           00000130
*---------------------------------------------------------------------* 00000132
*                   ** PROGRAM DESCRIPTION **                         * 00000134
*                 COMPRESS BALANCE HISTORY REC                        * 00000136
*                                                                     * 00000138
*---------------------------------------------------------------------* 00000140
*                  ** HISTORY OF REVISIONS **                         * 00000142
* DESCRIPTION                                                 CHNGID  * 00000144
* __________________________________________________________  _______ * 00000146
*                                                                     * 00000148
* 12/06/01 MAKE PUNCH CONDITIONAL                             2024422   00000986
* 06/29/01 CHANGES FOR REENTRANT                              2021627 * 00000988
* 01/03/00 RELINK FOR 31-BIT PROCESS                          2024183 * 00000990
* 10/09/97 INITIAL DISTRIBUTION.                              9913615 * 00000992
*                                                                     * 00000994
*---------------------------------------------------------------------* 00000996
         EJECT                                                          00000998
*********************************************************************** 00017000
*                                                                     * 00018000
*        THIS ROUTINE TAKES A RECORD AND CREATES A NEW VARIABLE       * 00019000
*  LENGTH RECORD FROM IT.  THE NEW RECORD HAS MULTIPLE OCCURRENCES    * 00020000
*  OF BYTES FROM ONE OCCURRENCE TO THE NEXT REPLACED BY X'FFLL'       * 00021000
*  WHERE LL IS THE NUMBER OF BYTES.  THE PROGRAM RETURNS              * 00022000
*  A COMPRESSED RECORD WITH CORRECT RECORD LENGTH.  IF ANY ERRORS     * 00023000
*  OCCUR THE RECORD IS RETURNED IN ITS ORIGINAL FORM.  THE PROGRAM    * 00024000
*  ASSUMES A RECORD WITH FIXED DATA FOLLOWED BY OCCURRENCES OF DATA.  * 00025000
*  IN LOOKING FOR DUPLICATE DATA, THE PROGRAM COMPARES THE DATA IN    * 00026000
*  THE INPUT RECORD TO DATA IN THE LATEST OCCURRENCE.  THIS IS DONE   * 00027000
*  BY MOVING NON-DUPLICATED DATA TO A WORK AREA USED IN THE           * 00028000
*  COMPARISONS.  THIS WORK AREA IS 255 BYTES WHICH LIMITS THE LENGTH  * 00029000
*  OF EACH OCCURRENCE TO 255 BYTES.                                   * 00030000
*                                                                     * 00031000
*  IF THE RECORD IS SUCCESSFULLY COMPRESSED, 6 BYTES ARE ADDED TO THE * 00032000
*  END OF THE RECORD CONTAINING THE FACTORS USED IN COMPRESSION.      * 00033000
*  THE FORMAT OF THE 6 BYES IS X'AABBBBCCCCFF' WHERE                  * 00034000
*           AA   IS THE LENGTH OF EACH OCCURRENCE                     * 00035000
*           BBBB IS THE LENGTH OF THE FIXED AREA                      * 00036000
*           CCCC IS THE LENGTH OF THE ORIGINAL RECORD                 * 00037000
*           FF   IS A BYTE OF X'FF' INDICATING A COMPRESSED RECORD    * 00038000
*                                                                     * 00039000
*  IF THERE IS AN ERROR IN THE CALLING PARAMETERS MAKING COMPRESSION  * 00040000
*  IMPOSSIBLE, THE RECORD IS RETURNED IN ITS ORIGINAL FORM BUT WITH   * 00041000
*  BINARY ZERO IN THE OUTPUT RECORD LENGTH.  IF THE I/O MODULE TRIES  * 00042000
*  TO WRITE THIS RECORD IT WILL GET AN I/O ERROR.                     * 00043000
*                                                                     * 00044000
*        THE PROGRAM EXPECTS THE FOLLOWING PARAMETERS:                * 00045000
*                                                                     * 00046000
*              A) WORKING-STORAGE-REC  PIC X(  ). (INCLUDES LENGTH)   * 00047000
*              B) FIXED-AREA-LENGTH    PIC S9999 COMP SYNC VALUE +  . * 00048000
*              C) MAX-NUM-OF-OCCURS    PIC S9999 COMP SYNC VALUE +  . * 00049000
*              D) COMPRESSED-RECORD    PIC X(  ). (WITH LENGTH)       * 00050000
*                                                                     * 00051000
*        REGISTER USAGE                                               * 00052000
*                                                                     * 00053000
*              0                                                      * 00054000
*              1    POINTS TO USERS ADCON TABLE                       * 00055000
*              2    ADDRESS OF WORKING STORAGE RECORD                 * 00056000
*              3    ADDRESS OF FIXED AREA LENGTH                      * 00057000
*              4    ADDRESS OF NUMBER OF OCCURS                       * 00058000
*              5    ADDRESS OF CURRENT DATA FROM OCCURRENCES          * 00059000
*              6    WORK REGISTER - POSITION IN INPUT RECORD          * 00060000
*              7    WORK REGISTER - POSITION IN OUTPUT RECORD         * 00061000
*              8                                                      * 00062000
*              9                                                      * 00063000
*              10                                                     * 00064000
*              11                                                     * 00065000
*              12   BASE REGISTER                                     * 00066000
*              13   ADDRESS OF USERS REGISTER SAVE AREA               * 00067000
*              14   RETURN ADDRESS TO USERS PROGRAM                   * 00068000
*              15   ENTRY POINT FOR THIS ROUTINE                      * 00069000
*                                                                     * 00070000
*********************************************************************** 00071000
         EJECT                                                          00072000
         LM    R2,R5,0(R1)         LOAD USER PARAMETERS                 00073000
         ST    R5,IOADDR           ADDR OF OUTPUT RECORD                00074000
         NI    IOADDR,X'7F'        RESET H.O. BIT FROM LAST ADDR LIST   00075000
*    CALCULATE THE LENGTH OF EACH OCCURRENCE                            00076000
         MVC   NUMOCCUR,0(R4)      SAVE NUMBER OF OCCURRENCES           00077000
         LH    R11,0(R2)           RECORD LENGTH FROM WORKG STORAGE REC 00078000
         STH   R11,SVLENG          SAVE RECORD LENGTH                   00079000
         SH    R11,=H'4'           REDUCE REC LENGTH BY LENGTH FIELD    00080000
         SH    R11,0(R3)        REDUCE BY LENGTH OF FIXED AREA OF REC   00081000
         SR    R10,R10             SET EVEN REG OF PAIR TO ZERO         00082000
         LH    R9,NUMOCCUR         LOAD DIVISOR - NUMBER OF OCCURRENCES 00083000
         LTR   R9,R9               TEST DIVISOR FOR ZERO                00084000
         BZ    NONEDEL             ALLOW 0 OCCURRENCES                  00085000
         DR    R10,R9         DIVIDE NUM OCCUR INTO LENG OF OCCUR AREA  00086000
         LTR   R10,R10        TEST REMAINDER FOR ZERO                   00087000
         BNZ   ERROR             BR NOT ZERO TO ERROR                   00088000
         LTR   R11,R11        TEST SEGMENT LENGTH FOR ZERO              00088100
         BZ    ERROR             BR ZERO TO ERROR                       00088200
         CH    R11,=H'255'         MAX AREA FOR SAVING OCCUR IS 255     00089000
         BH    ERROR               BR IF CANNOT PROCESS                 00090000
         STH   R11,OCCURLNG        SAVE LENGTH OF EACH OCCURRENCE       00091000
*    SET UP REGISTERS FOR POINTING TO DATA IN RECORDS                   00092000
         LR    R6,R2               LOAD ADDR OF INPUT REC IN R6         00093000
         LA    R6,4(R6)            BUMP PAST RECORD LENGTH              00094000
         L     R7,IOADDR           LOAD ADDR OF OUTPUT REC IN R7        00095000
         LA    R7,4(R7)            BUMP PAST OUTPUT REC LENGTH          00096000
*    MOVE FIXED PORTION OF RECORD TO OUTPUT AREA                        00097000
         LR    R8,R7               SET WITH ADDRESS OF 'TO' AREA        00098000
         LH    R9,0(R3)            SET LENGTH OF FIXED AREA             00099000
         LR    R10,R6              SET R10 WITH ADDRESS OF 'FROM' AREA  00100000
         LR    R11,R9              SET LENGTH OF FIXED DATA             00101000
         MVCL  R8,R10              MAKE MOVE OF FIXED AREA              00102000
*   SET POINTERS TO POS IN RECORD PAST THE FIXED DATA                   00103000
*    (AFTER MCVL THE REGISTERS SHOULD BE AT THE CORRECT LOCATION)       00104000
         LR    R6,R10              INPUT RECORD                         00105000
         LR    R7,R8               OUTPUT RECORD                        00106000
*   CALCULATE THE ADDRESS OF THE END OF THE INPUT RECORD                00107000
         LR    R8,R2               ADDR OF BEGINNING OF RECORD          00108000
         AH    R8,SVLENG           ADD RECORD LENGTH                    00109000
         BCTR  R8,0                REDUCE BY ONE                        00110000
         ST    R8,ENDADDR          SAVE END ADDRESS                     00111000
*   NOW MOVE THE 1ST OCCURRENCE FROM THE INPUT TO OUTPUT                00112000
         IC    R9,OCCURLNG+1       MOVE LENGTH OF OCCURRENCE            00113000
         SH    R9,=H'1'            MAKE REL TO 0 FOR MOVE CHAR          00114000
         EX    R9,EXECMVC7         MOVE 1ST OCCURRENCE TO OUTPUT        00115000
*   ALSO MOVE 1ST OCCUR TO SAVE DATA AREA                               00116000
         LA    R5,CURRDATA                                    2021627   00117000
         EX    R9,EXECMVC5         MOVE 1ST OCCURRENCE TO SAVE AREA     00118000
         LR    R10,R5              CALC END OF SAVE DATA AREA           00119000
         AH    R10,OCCURLNG        BEGIN ADDR + LENGTH                  00120000
         BCTR  R10,0               MINUS 1                              00121000
         ST    R10,ENDCURR         SAVE THIS ADDRESS                    00122000
*   INCREASE THE POINTERS PAST THE 1ST OCCURRENCE                       00123000
         AH    R6,OCCURLNG         INPUT RECORD                         00124000
         AH    R7,OCCURLNG         OUTPUT RECORD                        00125000
*                                                                       00126000
*                                                                       00127000
PROCLOOP EQU   *                                                        00128000
*   SEE IF THE END OF INPUT HAS BEEN REACHED                            00129000
         C     R6,ENDADDR          CURRENT POSITION VS END OF RECORD    00130000
         BH    PROCEND             BR IF END OF RECORD                  00131000
*   CHECK THIS CHARACTER FOR HEX FF - THERE SHOULD BE NONE IN THE       00132000
*                                   COMPRESSED PART OF THE RECORD       00133000
         CLI   0(R6),X'FF'         IS THIS CHAR HEX FF                  00134000
         BE    NONEDEL             BR TO PROCESS                        00135000
         C     R5,ENDCURR          IS THIS THE END OF AN OCCURRENCE     00136000
         BH    RESETCUR            BR YES                               00137000
         CLC   0(1,R6),0(R5)       COMPARE INPUT BYTE TO SAVED DATA     00138000
         BE    HEXFF               BR IF EQUAL                          00139000
MOVE1CHR EQU   *                                                        00140000
         MVC   0(1,R7),0(R6)       MOVE CHARACTER TO OUTPUT RECORD      00141000
         MVC   0(1,R5),0(R6)       MOVE CHARACTER TO SAVE CURR DATA     00142000
         LA    R6,1(R6)            NEXT POS IN INPUT                    00143000
         LA    R7,1(R7)            NEXT POS IN OUTPUT                   00144000
         LA    R5,1(R5)            NEXT POS IN DATA SAVE AREA           00145000
         B     PROCLOOP                                                 00146000
*    END OF OCCURRENCE - RESET THE POINTER                              00147000
RESETCUR EQU   *                                                        00148000
         LA    R5,CURRDATA         SET POINTER                2021627   00149000
         B     PROCLOOP                                                 00150000
*                                                                       00151000
*                                                                       00152000
*    PROCESS IDENTICAL DATA BETWEEN OCCURRENCES                         00153000
HEXFF    EQU   *                                                        00154000
*   1ST CHECK TO SEE IF THERE ARE 3 BYTES LEFT IN THE OCCURRENCE        00155000
         LR    R8,R5               ADDR OF POSITION IN SAVED DATA       00156000
         LA    R8,2(R8)            ADD 2 MORE                           00157000
         C     R8,ENDCURR          BE SURE CHAR NOT PAST END OF OCCUR   00158000
         BH    MOVE1CHR            BR IF END OF OCCURRENCE              00159000
         LH    R10,=H'1'           SET COUNTER TO 1                     00160000
         LR    R9,R6               LOAD WORK REG R9 WITH ADDR OF INPUT  00161000
         LR    R8,R5               LOAD WORK REG R8 WITH ADDR OF SAVED  00162000
HEXFFNXT EQU   *                                                        00163000
         LA    R8,1(R8)            NEXT POS IN SAVED DATA               00164000
         LA    R9,1(R9)            NEXT POS IN INPUT                    00165000
         C     R8,ENDCURR          CURRENT POS VS END OF SAVED DATA     00166000
         BH    HEXFFMVE            BR IF END OF DATA                    00167000
         CLC   0(1,R8),0(R9)       COMPARE INPUT TO SAVED DATA          00168000
         BE    HEXFFCNT            BR TO COUNT NUMBER OF EQUAL CHAR     00169000
*    THIS IS THE END OF STRING OF DUPLICATE DATA                        00170000
*    SEE HOW MANY CHARACTERS ARE DUPLICATED- MUST BE MORE THAN 2        00171000
HEXFFMVE EQU   *                                                        00172000
         CH    R10,=H'2'                                                00173000
         BNH   MOVE1CHR            IF NOT GT 2 GO MOVE SINGLE CHAR      00174000
         MVI   0(R7),X'FF'         STORE REPEAT CHAR IN OUTPUT          00175000
         STC   R10,1(R7)       STORE NUMBER OF DUPL CHAR IN NEXT POS    00176000
         LA    R7,2(R7)            BUMP TO NEXT POS IN OUTPUT           00177000
         LR    R6,R9               SET POSITION IN INPUT RECORD         00178000
         LR    R5,R8               SET POSITION IN SAVED DATA AREA      00179000
         B     PROCLOOP                                                 00180000
HEXFFCNT EQU   *                                                        00181000
         LA    R10,1(R10)          ADD 1 TO COUNT                       00182000
         B     HEXFFNXT                                                 00183000
*                                                                       00184000
PROCEND  EQU   *                                                        00185000
*    END OF INPUT RECORD - CALC OUTPUT RECORD LENGTH                    00186000
         LR    R9,R7               R7 IS POS PAST THE LAST CHAR OF REC  00187000
         S     R9,IOADDR           SUBT BEGINNING ADDR FOR LENGTH       00188000
         LA    R9,6(R9)   ADD LENGTH OF COMPRESSION FACTORS AT END REC  00189000
         L     R5,IOADDR                                                00190000
         STH   R9,0(R5)            PUT LENGTH IN OUTPUT RECORD          00191000
         MVI   2(R5),X'00'         ZERO REST OF LENGTH FIELD            00192000
         MVI   3(R5),X'00'                                              00193000
*   NOW SEE IF OUTPUT RECORD IS SHORTER THAN INPUT                      00194000
         CLC   0(2,R5),SVLENG      COMP OUTPUT LNG VS INPUT LENGTH      00195000
         BNL   NONEDEL             BR IF OUTPUT NOT SHORTER             00196000
*   STORE COMPRESSION FACTORS AT END OF RECORD                          00197000
         MVC   0(1,R7),OCCURLNG+1  R7 IS END OF REC - MOVE OCCUR LNG    00198000
         MVC   1(2,R7),0(R3)       MOVE LENGTH OF FIXED AREA OF REC     00199000
         MVC   3(2,R7),0(R2)       MOVE LENGTH OF ORIGINAL RECORD       00200000
         MVI   5(R7),X'FF'         MOVE COMPRESSION FLAG TO END         00201000
         B     GOBACK                                                   00202000
*                                                                       00203000
*   IF NOT, USE THE INPUT RECORD AS OUTPUT                              00204000
NONEDEL  EQU   *                                                        00205000
         LH    R9,SVLENG           LENGTH OF INPUT RECORD               00206000
         L     R8,IOADDR           SET WITH ADDRESS OF 'TO' AREA        00207000
         LR    R10,R2              SET R10 WITH ADDRESS OF 'FROM' AREA  00208000
         LR    R11,R9              SET LENGTH OF RECORD                 00209000
         MVCL  R8,R10              MOVE INPUT RECORD TO OUTPUT          00210000
         B     GOBACK                                                   00211000
*                                                                       00212000
*  ERROR IN CALL PARAMETERS- RETURN RECORD AND PUT ZERO IN RECORD LNG   00213000
ERROR    EQU   *                                                        00214000
         LH    R9,SVLENG           LENGTH OF INPUT RECORD               00215000
         L     R8,IOADDR           SET WITH ADDRESS OF 'TO' AREA        00216000
         LR    R10,R2              SET R10 WITH ADDRESS OF 'FROM' AREA  00217000
         LR    R11,R9              SET LENGTH OF RECORD                 00218000
         MVCL  R8,R10              MOVE INPUT RECORD TO OUTPUT          00219000
         L     R8,IOADDR           ADDRESS THE 1ST OF OUTPUT RECORD     00220000
         MVC   0(2,R8),=X'0000'    SET LENGTH TO ZERO                   00221000
*                                                                       00222000
GOBACK   EQU   *                                                        00223000
         AIF   (&CICS).CICSRET                                          00224000
         SIRETRN                   RETURN                               00225000
         AGO   .DATA                                                    00226000
.CICSRET ANOP                                                           00227000
         EXEC  CICS RETURN                                              00228000
         LTORG                                                          00229000
.DATA    ANOP                                                           00230000
         SPACE 3                                                        00231000
EXECMVC5 MVC   0(0,R5),0(R6)  MOVE FROM INPUT RECORD TO R5 LOC          00232000
EXECMVC7 MVC   0(0,R7),0(R6)  MOVE FROM INPUT RECORD TO R7 LOC          00233000
         AIF   ('&SYSPARM' NE 'CICS').STGBAT                  2021627   00233005
* THE FOLLOWING STATEMENT(S) MODIFIED BY                      2021627   00233009
DFHEISTG DSECT                                                          00233010
* THE FOLLOWING STATEMENT(S) MODIFIED BY                      2021627   00233014
.STGBAT  ANOP                                                           00233015
IOADDR   DS    F                   ADDR OF OUTPUT RECORD                00234000
ENDADDR  DS    F                   END OF INPUT RECORD                  00235000
ENDCURR  DS    F                   END OF CURRENT OCCURRENCE DATA       00236000
SVLENG   DS    H            INPUT RECORD LENGTH                         00237000
NUMOCCUR DS    H            NUMBER OF OCCURRENCES IN RECORD             00238000
OCCURLNG DS    H            LENGTH OF EACH OCCURRENCE                   00239000
CURRDATA DS    255C   WORK AREA CONTAINING THE MOST RECENT DATA IN      00240000
*                     AN OCCURRENCE                                     00241000
         AIF   ('&SYSPARM' NE 'CICS').DTSBAT                            00244985
FIBHCP   CSECT                                                          00244986
         AGO   .DTSMRG                                                  00244987
.DTSBAT  ANOP                                                           00244988
SIBHCP   CSECT                                                          00244989
.DTSMRG  ANOP                                                           00244990
         LTORG                                                          00244991
         DS    0D                                                       00244992
SITMSTMP DC    CL64'SIBHCP    -----TSD-             12/06/01  14.25.00' 00244993
*        THIS PROGRAM CONTAINS TRADE SECRETS THAT BELONG                00244994
*        TO ALLTEL INFORMATION SERVICES, INC. AND IS                    00244995
*        LICENSED BY AN AGREEMENT.  ANY UNAUTHORIZED ACCESS,            00244996
*        USE, DUPLICATION, OR DISCLOSURE IS UNLAWFUL.                   00244997
*        COPYRIGHT ALLTEL INFORMATION SERVICES, INC.                    00244998
*        2001, ALL RIGHTS RESERVED.                                     00244999
         END                                                            00245000
