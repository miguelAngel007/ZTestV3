*     * 802262 12/11/20 PROYECTO UPGRADE GN172 EDVR
         TITLE 'SISSLOP - LOOP OPEN/CLOSE/WRITE ROUTINE '               00000100
*                                                                     * 00000200
*********************************************************************** 00000300
*                                                                     * 00000400
*    TITLE : SISSLOP - LOOP OPEN/CLOSE/WRITE ROUTINE                  * 00000500
*                                                                     * 00000600
*    FUNCTION :                                                       * 00000700
*       THIS PROGRAM IS USED TO PROCESS MULTIPLE WRITES TO SEGMENTED  * 00000800
*       FILES WITHOUT REQUIRING APPLICATIONS TO MAKE MULTIPLE         * 00000900
*       I-O SUBROUTINE CALLS.                                         * 00001000
*                                                                     * 00001100
*       ONLY OPEN/CLOSE/ OR WRITE REQUEST IS VALID IN THIS PROGRAM.   * 00001200
*                                                                     * 00001300
*       THIS PROGRAM EXPECTS THE FOLLOWING INPUT:                     * 00001400
*                                                                     * 00001500
*         1. STACK TABLE (2 BYTE ENTRIES) OF THE FILE SUFFIXES        * 00001600
*            REQUIRED (AS RETURNED FROM SISSLKP OR SISSBLD).          * 00001700
*            THIS STACK MUST CONTAIN LOW-VALUES (X'00) AFTER THE      * 00001800
*            ACTIVE ENTRIES.                                          * 00001900
*                                                                     * 00002000
*         2. I-O-BASE COPYBOOK FILLED IN EXCEPT FOR THE               * 00002100
*            I-O-BASE-SEG-NO WHICH WILL BE UPDATED AS REQUIRED BY     * 00002200
*            THE SISSLOP PROGRAM.                                     * 00002300
*                                                                     * 00002400
*         3. IO-CONTROL-AREA MUST BE SET UP WITH THE TYPE I/O REQUEST.* 00002500
*            CURRENTLY, ONLY O/C/W WILL BE SUPPORTED.                 * 00002600
*                                                                     * 00002700
*         4. I-O-RETURN-AREA MUST BE SET UP WITH THE RECORD TO BE     * 00002800
*            WRITTEN.                                                 * 00002900
*********************************************************************** 00003000
*  NEW PROGRAM FOR FILE SEGMENTATION                                  * 00003100
*                                                                     * 00003200
*-----------------------------------------------------------* 0525104   00003202
*                  ** HISTORY OF REVISIONS **               * 0525104   00003204
*                                                           * 0525104   00003206
* DESCRIPTION                                       CHNGID  * 0525104   00003208
* ________________________________________________  _______ * 0525104   00003210
* 09/15/05 ACCEPTS AND PROCESS THE EMPTY SEGMENTS    GN5311 * 0625311   00003288
* 10/12/04 INCREASED THE MAXIMUM NUMBER OF SSR       GN5104 * 0525104   00003290
* SEGMENTS TO 200.                                   GN5104 * 0525104   00003292
*                                                           * 0525104   00003294
*-----------------------------------------------------------* 0525104   00003296
*********************************************************************** 00003300
*                                                                     * 00003400
         COPY SIOPTNS                                                   00003500
         TITLE 'SISSLOP - DSECT TABLES'                                 00003600
*                                                                     * 00003700
*********************************************************************** 00003800
*                                                                     * 00003900
* * * *                   DSECT TABLES                          * * * * 00004000
*                                                                     * 00004100
*********************************************************************** 00004200
*                                                                     * 00004300
*      -----------------------------------------------                  00004400
*      ¦ CALLING PARAMETERS FOR OPEN/CLOSE/WRITE LOOP¦                  00004500
*      -----------------------------------------------                  00004600
         SPACE 3                                                        00004700
         COPY SIDSSIOB                                                  00004800
*** RETURN CODES FOLLOW FOR THIS MODULE ***                             00004900
SCBRTNRM EQU   C'0'      NORMAL RETURN                                  00005000
SCBRTMPT EQU   C'5'      EMPTY TABLE PASSED                             00005100
SCBRTFTL EQU   C'9'      FATAL ERROR                                    00005200
         SPACE 3                                                        00005300
         COPY SIDSCNTL   STANDARD SI I-O PARAMETER LIST                 00005400
IOREQLEN EQU  L'IOOPER+L'IOACCESS                                       00005500
         SPACE 3                                                        00005600
RETAREA  DSECT                                                          00005700
         DS    CL4000    RETURN AREA FOR RECORDS                        00005800
         SPACE 3                                                        00005900
*** LIST OF SEGMENT/GROUP NOS REQUIRED (BUILT BY SISSBLD OR SISSLKP)    00006000
LOPRESP  DSECT                                                          00006100
LOPSTKTB DS    201CL2     STACK TABLE ENTRIES                 0525104   00006200
LOPSTKLN EQU  *-LOPSTKTB  LENGTH OF STACK TABLE                         00006300
         TITLE 'SISSLOP - PROGRAM ENTRY POINT AND GENERAL HOUSEKEEPING' 00006400
*                                                                     * 00006500
*********************************************************************** 00006600
*                                                                     * 00006700
* * * *        PROGRAM ENTRY POINT AND GENERAL HOUSEKEEPING     * * * * 00006800
*                                                                     * 00006900
*********************************************************************** 00007000
*                                                                     * 00007100
****  REGISTER USEAGE *****                                             00007200
*** R0 - COUNTER FOR LOOP CONTROL                                       00007300
*** R1 - PARAMETER PASSING                                              00007400
*** R3 - LKPRESP - (STACK TABLE LIST OF SEGMENTS/GROUPS)                00007500
*** R4 - FILE NAME/PROTOTYPE INFORMATION)                               00007600
*** R5 - SI S-O CONTROL AREA (TO CALL SIIORTN)                          00007700
*** R6 - RECORD AREA - MUST BE SET UP FOR WRITE                         00007800
*** R7 - WORK REGISTER                                                  00007900
*** R12 - BASE REGISTER                                                 00008000
*** R14 - BRANCH AND LINK REGISTER                                      00008100
*** R15 - WORK REGISTER                                                 00008200
         SPACE 3                                                        00008300
SISSLOP  CSECT                                                          00008400
         SIBASE BASEREG=(BASEREG),ID=SISSLOP,RWNUM=0022,RWSUB=000       00008500
         STCM R1,B'1111',ORIG1    SAVE ORIGINAL CONTENTS OF R1          00008600
         LM    R3,R6,0(R1)        LOAD ADDRESSABILITY TO INPUT PARMS    00008700
         USING LOPRESP,R3         ADDRESSABILITY TO SEG/GRP NBR TBL     00008800
         USING SCBASE,R4          ADDRESSABILITY TO FILE/PROTOTYPE AREA 00008900
         USING SIDSCNTL,R5        ADDRESSABILITY TO I-O CONTROL BLOCK   00009000
         USING RETAREA,R6         ADDRESSABILITY TO RECORD AREA (MUST   00009100
**                                  BE INITIALIZED FOR A WRITE REQUEST) 00009200
         TITLE 'SISSLOP - MAINLINE PROCESSING'                          00009300
*                                                                     * 00009400
*********************************************************************** 00009500
*                                                                     * 00009600
* * * *                  MAINLINE PROCESSING                    * * * * 00009700
*                                                                     * 00009800
*********************************************************************** 00009900
*                                                                     * 00010000
         LA    WORKREG,LOPSTKTB   PICK UP STACK ENTRY FOR LOOP INIT     00010100
         LA    CTRREG,LOPSTKLN    PICK UP MAX STACK LENGTH              00010200
         SRL   CTRREG,1           DIVIDE BY 2 FOR NUMBER OF ENTRIES     00010300
         MVI   SCBRTCD,SCBRTMPT   INITIALIZE RETURN CODE TO EMPTY STACK 00010400
LOPTHRU  DS    0H                 MAINLINE LOOP                         00010500
         CLC   0(L'LOPSTKTB,WORKREG),ZEROS IS IT LOW VALUES?            00010600
         BE    RETURN             YES, ALL FINISHED                     00010700
         MVI   SCBRTCD,SCBRTNRM   IF ALL CALLS SUCCESSSFUL, INDICATE    00010800
         MVC   SCBSGNO(L'LOPSTKTB),0(WORKREG) MOVE SEG NBR FROM STK     00010900
         BAS   RETRG,SISSRTN                                  2024183   00011000
         CLC   IORETCOD,ZEROS      GOOD RETURN                          00011100
*** ALLOW EMPTY FILE IF ITS OPEN OUTPUT REQUEST ***                     00011200
         BE    CONTLOOP            YES, KEEP GOING                      00011300
         CLC   IORETCOD,=H'12'     OTHERWISE, IS IT EMPTY FILE?         00011400
         BE    CONTLOOP            YES, KEEP GOING            0625311   00011500
         CLC   IOOPER(IOREQLEN),=CL2'OL' OTHERWISE, IS IT OPEN FOR LOAD 00011600
         BE    CONTLOOP            YES THEN IT'S VALID                  00011700
         CLC   IOOPER(IOREQLEN),=CL2'OO'  IS IT OPEN FOR OUTPUT?        00011800
         BE    CONTLOOP            YES THEN IT'S VALID                  00011900
         CLC   IOOPER(IOREQLEN),=CL2'PL'  IS IT ESDS OPEN FOR LOAD?     00011910
         BE    CONTLOOP            YES THEN IT'S VALID                  00011920
         CLC   IOOPER(IOREQLEN),=CL2'PO'  IS IT ESDS OPEN FOR OUTPUT?   00011930
         BNE   ERRRETRN            NO - THEN IT'S AN ERROR              00011940
CONTLOOP DS    0H                                                       00012000
         LA    WORKREG,L'LOPSTKTB(WORKREG)  POINT TO NEXT ENTRY IN STK  00012100
         BCT   CTRREG,LOPTHRU      LOOP UNTIL ALL ENTRIES               00012200
RETURN   DS    0H                  NORMAL RETURN                        00012300
         SIRETRN RC=(15)           RETURN TO CALLER                     00012400
         TITLE 'SISSLOP - ERROR ROUTINES'                               00012500
*                                                                     * 00012600
*********************************************************************** 00012700
*                                                                     * 00012800
* * * *                ERROR ROUTINES                           * * * * 00012900
*                                                                     * 00013000
*********************************************************************** 00013100
*                                                                     * 00013200
ERRRETRN DS    0H                  IO ERROR ROUTINE                     00013300
         MVI   SCBRTCD,SCBRTFTL    INDICATE FATAL ERROR                 00013400
         B     RETURN              RETURN TO CALLER                     00013500
         TITLE 'SISSLOP - MISCELLANEOUS ROUTINES'                       00013600
*                                                                     * 00013700
*********************************************************************** 00013800
*                                                                     * 00013900
* * * *              MISCELLANEOUS ROUTINES                     * * * * 00014000
*                                                                     * 00014100
*********************************************************************** 00014200
*                                                                     * 00014300
*        ----------------------------                                   00014400
*        CALL SEGMENTATION I/O DRIVER                                   00014500
*        ----------------------------                                   00014600
         SPACE 3                                                        00014700
SISSRTN  DS    0H                  CALL SISSRTN SEGMENTATION IO DRIVER  00014800
*   RETRG HAS RETURN ADDRESS AFTER I/O RETURN                           00014900
         ICM   R15,X'F',SISSADCN   GET I/O DRIVER ADDRESS               00015000
         BNZ   SISSAFLD           GOT IT, DONT NEED TO RELOAD 2024183   00015100
         LOAD  EP=SISSRTN         LOAD THE IO ROUTINE DRIVER            00015200
         ST    R0,SISSADCN        STORE THE ADDRESS HERE                00015300
         LR    R15,R0              PUT ADDRESS HERE                     00015400
SISSAFLD DS    0H                                                       00015500
         L     R1,ORIG1            PICK UP ORIGINAL PARM LIST PTR       00015600
         LA    R1,L'ORIG1(R1)      ONLY 1ST PARM (STACK) IS UNNEEDED    00015700
         CALL  (15)                CALL SISSRTN                         00015800
         BR    RETRG               RETURN TO CALLING ROUTINE            00015900
         TITLE 'SISSLOP-STATIC STORAGE+LITERALS,EQUATES,AND CONSTANTS'  00016000
*                                                                     * 00016100
*********************************************************************** 00016200
*                                                                     * 00016300
* * * *    STATIC STORAGE + LITERALS, EQUATES, AND CONSTANTS    * * * * 00016400
*                                                                     * 00016500
*********************************************************************** 00016600
*                                                                     * 00016700
*** STATIC STORAGE ***                                                  00016800
WKSTGSEG DC    CL12'**WKSTGSEG**'                                       00016900
PGMID    DC    CL8'SISSLOP'                                             00017000
WORKREG  EQU   7                                                        00017100
CTRREG   EQU   8                  USED FOR LOOP CONTROL                 00017200
BASEREG  EQU   12                 BASE REGISTER                         00017300
RETRG    EQU   9                  RETURN REGISTER                       00017400
ZEROS    DC    H'0'                                                     00017500
SISSADCN DC    A(0)                PLACE HOLDER FOR SISSRTN ADDRESS     00017600
ORIG1    DC    A(0)                                                     00017700
         TITLE 'SISSLOP - GENERATED LITERALS, EQUATES, AND CONSTANTS'   00017800
*                                                                     * 00017900
*********************************************************************** 00018000
*                                                                     * 00018100
* * * *          GENERATED LITERALS, EQUATES, AND CONSTANTS     * * * * 00018200
*                                                                     * 00018300
*********************************************************************** 00018400
*                                                                     * 00018500
         LTORG                                                          00018600
SISSLOP  CSECT                                                          00018696
         LTORG                                                          00018697
         DS    0D                                                       00018698
SITMSTMP DC    CL64'SISSLOP   -----TSD-             09/26/05  00.03.42' 00018699
*        THIS PROGRAM CONTAINS TRADE SECRETS THAT BELONG                00018700
*        TO FIDELITY INFORMATION SERVICES AND IS                        00018701
*        LICENSED BY AN AGREEMENT.  ANY UNAUTHORIZED ACCESS,            00018702
*        USE, DUPLICATION, OR DISCLOSURE IS UNLAWFUL.                   00018703
*        COPYRIGHT FIDELITY INFORMATION SERVICES                        00018704
*        2005, ALL RIGHTS RESERVED.                                     00018705
         END                                                            00018900
