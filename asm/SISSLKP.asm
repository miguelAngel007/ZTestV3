*     * 802262 12/11/20 PROYECTO UPGRADE GN172 EDVR
         TITLE 'SISSLKP - S S R  INCORE TABLE LOOKUP MODULE'            00000100
*---------------------------------------------------------------------* 00000200
*                   ** PROGRAM DESCRIPTION **                         * 00000300
*                                                                     * 00000400
*    TITLE : SISSLKP - SSR  INCORE TABLE LOOKUP MODULE                * 00000500
*                                                                     * 00000600
*    FUNCTION :                                                       * 00000700
*       THIS PROGRAM IS USED TO SEARCH THE INCORE TABLE BUILT BY THE  * 00000800
*       SISSBLD PROGRAM.  IT PROCESSES THE FOLLOWING TYPES OF         * 00000900
*       REQUESTS:                                                     * 00001000
*       1 - FULL SSR RECORD LOOKUP (LKPREQ2 = 0 OR SPACES)            * 00001100
*               BASED ON CONTROLS PASSED IN LKPTCTLS, THE 50 BYTE SSR * 00001200
*               BATCH RECORD IS RETURNED AT LKPRESP.                  * 00001300
*       2 - GROUP/SEGMENT LIST LOOKUP VIA CONTROLS (LKPREQ2=1,2,3,4)  * 00001400
*               BASED ON THE LEVEL OF CONTROLS AND THE VALUE OF THOSE * 00001500
*               CONTROLS PASSED IN LKPTCTLS, A LIST IS RETURNED AT    * 00001600
*               LKPRESP DEPENDING ON LKPREQ1 (0=SEGMENTS, 1=MASTER    * 00001700
*               GROUPS, 2=CONTROL GROUPS, 3=ALPHA GROUPS).            * 00001800
*       3 - GROUP LIST LOOKUP VIA SEGMENT NUMBER (LKPREQ2=9 AND THE   * 00001900
*               FIRST 2 CHARACTERS OF LKPTCTLS = 00-99), A LIST IS    * 00002000
*               RETURNED AT LKPRESP OF ALL GROUPS REFERENCED          * 00002100
*               ON SSR RECORDS THAT PERTAIN TO THAT SEGMENT NUMBER    * 00002200
*               DEPENDING ON LKPREQ1 (1=MASTER, 2=CONTROL, 3=ALPHA).  * 00002300
*       4 - LIST ALL GROUPS/SEGMENT NUMBERS IN THE PASSED SSR TABLE   * 00002400
*               (LKPREQ2=9 AND THE FIRST 2 CHARACTERS OF LKPTCTLS=**).* 00002500
*                A LIST IS RETURNED AT LKPRESP DEPENDING ON LKPREQ1   * 00002600
*                (0=SEGMENTS, 1=MASTER GROUPS, 2=CONTROL GROUPS,      * 00002700
*                3=ALPHA GROUPS).                                     * 00002800
*       5 - GROUP/SEGMENT LIST LOOKUP VIA CONTROLS AND BATCH SEGMENT  * 00002900
*               (LKPREQ2=5,6,7,8) BASED ON THE LEVEL OF CONTROLS      * 00003000
*               PASSED IN LKPTCTLS AND THE BATCH SEGMENT PASSED IN    * 00003100
*               LKPTBSEG, A LIST IS RETURNED AT LKPRESP DEPENDING ON  * 00003200
*               LKPREQ1 (0=SEGMENTS, 1=MASTER GROUPS, 2=CONTROL       * 00003300
*               GROUPS, 3=ALPHA GROUPS).                              * 00003400
*                                                                     * 00003500
*       A FOURTH PARAMETER CAN BE PASSED TO THIS MODULE.      0525104 * 00003510
*       IF THE FOURTH PARAMETER IS NOT PRESENT, THE MAXIMUM   0525104 * 00003512
*       NUMBER OF SEGMENTS IS SET TO 25. IF THE FOURTH        0525104 * 00003514
*       PARAMETER IS PRESENT THE MAXIMUM NUMBER OF SEGMENTS   0525104 * 00003516
*       IS SET TO 200.                                        0525104 * 00003518
*********************************************************************** 00003600
*  NEW PROGRAM FOR FILE SEGMENTATION                                  * 00003700
*                                                                     * 00003800
*********************************************************************** 00003900
*                                                                     * 00004000
*---------------------------------------------------------------------* 00004005
         EJECT                                                          00004010
*---------------------------------------------------------------------* 00004015
*                  ** HISTORY OF REVISIONS **                         * 00004020
*                                                                     * 00004025
* DESCRIPTION                                                 CHNGID  * 00004030
* __________________________________________________________  _______ * 00004035
* 02/03/05 CONSIDER LOW-VALUES WHEN DETERMINING KEY LENGTH    0535152   00004073
* 10/12/04 INCREASED THE MAXIMUM NUMBER OF SSR SEGMENTS TO    0525104 * 00004075
* 200.                                                        0525104 * 00004076
* 08/31/00 FIX BINSRCH IF >=22 SEGS & ARG = BETW N-1 & N SEG  2024226 * 00004077
* 08/28/00 ACCT LEVEL SEGMENTATION                            2023258 * 00004078
* 03/07/00 CHANGED BAL TO BAS                                 2024183 * 00004079
* 07/20/94 TO ALLOW ALPHANUMERIC FILE GROUPS                  2602266 * 00004080
*                                                                     * 00004085
*---------------------------------------------------------------------* 00004090
         EJECT                                                          00004095
         COPY SIOPTNS                                                   00004100
         TITLE 'SISSLKP - DSECTS'                                       00004200
*                                                                     * 00004300
*********************************************************************** 00004400
         COPY SIDSSLKP                                                  00004500
         SPACE 3                                                        00004600
         COPY SIDSSBLD             SSR TABLE AREA BUILT BY SISSBLD      00004700
         TITLE 'SISSLKP - PROGRAM ENTRY POINT AND GENERAL HOUSEKEEPING' 00004800
*                                                                     * 00004900
*********************************************************************** 00005000
*                                                                     * 00005100
* * * *        PROGRAM ENTRY POINT AND GENERAL HOUSEKEEPING     * * * * 00005200
*                                                                     * 00005300
*********************************************************************** 00005400
*                                                                     * 00005500
****  REGISTER USEAGE *****                                             00005600
*** R0 - COUNTER FOR LOOP CONTROL                                       00005700
*** R1 - PARAMETER PASSING                                              00005800
*** R3 - APPLICATION REQUEST                                            00005900
*** R4 - SSR TABLE ADDRESS (AREA INITILAIZED BY SISSBLD)                00006000
*** R5 - RESPONSE AREA ADDRESS                                          00006100
*** R6 - SSR DETAIL ENTRY PTR                                           00006200
*** R7 - WORK REGISTER                                                  00006300
*** R8 - WORK REGISTER                                                  00006400
*** R12 - BASE REGISTER                                                 00006500
*** R14 - BRANCH AND LINK REGISTER                                      00006600
*** R15 - WORK REGISTER                                                 00006700
         SPACE 3                                                        00006800
SISSLKP  CSECT                                                          00006900
         SIBASE BASEREG=(BASEREG),ID=SISSLKP,RWNUM=0022,RWSUB=000       00007000
         STCM R1,B'1111',ORIG1    SAVE ORIGINAL CONTENTS OF R1          00007100
         LM    R3,R5,0(R1)        LOAD ADDRESSABILITY TO INPUT PARMS    00007200
         USING LKPREQ,R3          ADDRESSABILITY TO REQUEST PARAMETERS  00007300
         USING SIBLTBL,R4         ADDRESSABILITY TO SSR TABLE           00007400
         USING LKPRESP,R5         ADDRESSABILITY TO RESPONSE TABLE      00007500
         STM   R6,R9,SAVEREGS     BACKUP REGISTERS            0525104   00007625
         LA    R6,LKPRESP         LOAD ADD OF RESPONSE TABLE  0525104   00007630
         LA    R7,LKPSTKLN        LOAD LEN OF RESPONSE TABLE  0525104   00007635
         LA    R9,1               CLEAR THE REG               0525104   00007640
         LA    R8,ZEROS2          MOVE FROM LOW VALUES        0525104   00007645
         MVCL  R6,R8              ZERO OUT RESPONSE TABLE     0525104   00007650
         LM    R6,R9,SAVEREGS     RESTORE REGISTERS           0525104   00007655
         MVI   LKRETCOD,LKPNRMRT  INITIALIZE RETURN CODE TO ZERO        00007700
         TM    8(R1),X'80'        3 PARAMETERS ONLY?          0525104   00007710
         BO    SETLMT             YES, GO SET LIMITS          0525104   00007712
         ZAP   MAXSEGCT,=P'200'   SET SEGMENT LIMIT           0525104   00007714
         B     INITA              CONTINUE                    0525104   00007716
SETLMT   DS    0H                                             0525104   00007718
         ZAP   MAXSEGCT,=P'25'    SET SEGMENT LIMIT           0525104   00007720
INITA    DS    0H                                             0525104   00007722
*** INITIALIZE DYNAMIC STORAGE TO INSURE REUSEABILITY ***               00007800
         XC    DWORD(LENADCNS),DWORD      ZERO OUT THESE AREAS          00007900
         MVI   ENDTBFLG,ENDTBOFF          INTITALIZE FIELD              00008000
         STM   R6,R9,SAVEREGS     BACKUP REGISTERS            0525104   00008125
         LA    R6,UNIQTBL         LOAD ADDRESS OF UNIQTBL     0525104   00008130
         LA    R7,TOTULEN         LOAD LENGTH  OF UNIQTBL     0525104   00008135
         LA    R9,1               CLEAR THE REGT AREA         0525104   00008140
         LA    R8,ZEROS2          MOVE FROM LOW VALUES        0525104   00008145
         MVCL  R6,R8              ZERO OUT FOR EACH EXEC      0525104   00008150
         LM    R6,R9,SAVEREGS     RESTORE REGISTERS           0525104   00008155
         ZAP   UNIQTOT,PKED0      ZERO OUT FIELD FOR LOOP               00008200
         TITLE 'SISSLKP  -  VALIDATE THE CALLING PARAMETERS'            00008300
         SPACE 3                                                        00008400
*** VALIDATE ACTION FLAG 1 *****                                        00008500
         SPACE 3                                                        00008600
         CLI   LKPREQ1,LKPSEG     Q. IS IT LOWER THAN 0                 00008700
         BL    ERRACT1            A. YES, ITS AN ERROR                  00008800
         CLI   LKPREQ1,LKPALP     Q. IS IT HIGHER THAN 3                00008900
         BH    ERRACT1            A. YES, ITS AN ERROR                  00009000
         CP    SIBLSNOE,PKED0     Q. HAS SSR TABLE BEEN INITIALIZED?    00009100
         BNH   ERREMPTY           A. NO, GIVE RETURN CODE AND GET OUT   00009200
         ZAP   DWORD,SIBLSNOE     A. Y,GET TOT ENTRIES TO GET TBL END   00009300
         SP    DWORD,PKED1         DECREMENT TO PT TO LAST ENTRY        00009400
         MP    DWORD,SIBLSLN       MULTIPLY TO CALCULATE DISPLACEMENT   00009500
         CVB   R15,DWORD           CONVERT TO BINARY TO CALCULATE       00009600
         LA    SSRDETRG,SIBLTBDE(R15) POINT TO HIGH ENTRY OF TABLE      00009700
         ST    SSRDETRG,LASTSSR                                         00009800
         SPACE 3                                                        00009900
*** VALIDATE ACTION FLAG 2 *****                                        00010000
         SPACE 3                                                        00010100
         CLI   LKPREQ2,LKPACCT    Q. IS IT ACCOUNT LOOKUP?    2511795   00010110
         BE    CHKSRCH1           A. Y,SRCH FOR SSR SINGLE REC2511795   00010120
         CLI   LKPREQ2,LKPDET1    Q. IS IT SINGLE RECORD LOOKUP?        00010200
         BE    CHKSRCH1           A. Y, GO SEARCH FOR SSR SINGLE RECORD 00010300
         CLI   LKPREQ2,LKPDET2    Q. IS THIS ALSO SINGLE RECORD LOOKUP  00010400
         BE    CHKSRCH1           A. YES, SEARCH FOR SSR SINGLE RECORD  00010500
         BL    ERRACT2            LOWER THAN ZERO AND NOT SPACES, ERROR 00010600
         CLI   LKPREQ2,LKPCTL4    IS IT SINGLE RECORD LOOKUP (LEVEL 4?) 00010700
         BE    CHKSRCH1           YES, GO SEARCH FOR SSR SINGLE RECORD  00010800
         BL    MULTSRCH           REQ 1, 2, 3 CAN RETURN MULTIPLE ROWS  00010900
         MVI   CTLSGFLG,C'1'      TURN ON CTL/SEQ LOOKUP FLAG           00011000
         CLI   LKPREQ2,LKPC4SG    SINGLE REC LOOKUP (CTL4/SEG)?         00011100
         BE    CHKSRCH1           YES, GO SEARCH FOR SSR SINGLE         00011200
         BL    MULTSRCH           REQ 5,6,7 CAN RET MULTIPLE ROWS       00011300
         CLI   LKPREQ2,LKPALL     IS THIS A SEARCH BASED ON SEG #       00011400
         BNE   ERRACT2            NO, IT'S INVALID FLAGS                00011500
         TITLE 'SISSLKP  -  HANDLE ALL REQUEST'                         00011600
         SPACE 3                                                        00011700
********************************************************************    00011800
*** PROCESS REQUEST FOR SERIAL SEARCH OF TABLE FOR ALL SEGS/GRPS ***    00011900
*** (LKPREQ2=9).  IF SEGMENTS ARE REQUESTED (LKPREQ1=0), ALL     ***    00012000
*** SEGMENTS MUST BE REQUESTED (LKPTBSEG='**').  IF GROUPS ARE   ***    00012100
*** REQUESTED, LOOKUP CAN BE FOR ALL GROUPS IN THE SSR TABLE     ***    00012200
*** (LKPTBSEG=**) OR IT CAN BE BASED ON A SPECIFIC SEGMENT NUMBER **    00012300
*** (LKPTBSEG=##).                                                **    00012400
********************************************************************    00012500
         SPACE 3                                                        00012600
*** FIRST VALIDATE FOR ILLOGICAL REQUEST ***                            00012700
         MVI   CTLSGFLG,C'0'      TURN OFF CTL/SEQ LOOKUP FLAG          00012800
         CLI   LKPREQ1,LKPSEG     IS IT A LOOKUP BY SEGMENT             00012900
         BH    VALIDALL           NO, THEN ANY REQUEST IS VALID         00013000
         CLC   LKPTBSEG,LKPALLCT  LOOKUP BY SEGMNT, ONLY NO             00013100
         BNE   ERRACT3              SEARCH IS VALID                     00013200
VALIDALL DS    0H                                                       00013300
*** SEARCH IS FOR ALL GROUPS BASED ON SEGMENT NUMBER PASSED ***         00013400
         LA    SSRDETRG,SIBLTBDE    PICK UP FIRST SSR DETAIL ENTRY      00013500
         USING SIBLDET,SSRDETRG     GET ADDRESSABILITY TO DETAILS       00013600
         ZAP   DWORD,SIBLSNOE       PICK UP NBR OF ENTRIES IN DETAILS   00013700
         CVB   R0,DWORD             CONVERT TO BINARY FOR LOOPCTL       00013800
ALLLOOP  DS    0H                   THIS ONLY PROCESSES SRCH BY SEGMENT 00013900
         LA    RETRG,ALLNXTNT       RETURN PT FROM LKPLVL               00014000
         CLC   LKPTBSEG,LKPALLCT    FULL SEG/GRP                        00014100
         BE    LKPLVL               YES, SELECT GROUP #                 00014200
         CLC   SIBLDSEG,LKPTBSEG   IS THIS DESIRED SEGMENT?             00014300
         BE    LKPLVL               YES, GO PROCESS                     00014400
ALLNXTNT DS    0H                                                       00014500
         LA    SSRDETRG,SIBLDLEN(SSRDETRG)                              00014600
         BCT   R0,ALLLOOP           INSPECT NEXT TABLE ENTRY            00014700
         TITLE 'SISSLKP  -  BUILD RESPONSE STACK FOR CALLER'            00014800
*****************************************************************       00014900
*** BUILD THE RESPONSE ***                                              00015000
*  THIS ROUTINE IS CALLED FOR LKPREQ2=1,2,3 AND 9,  WHERE MULTIPLE      00015100
*  CAN BE RETURNED.  THE UNIQTBL IS SEARCHED FOR INITIALIZED            00015200
*  FIELDS AND THOSE FIELDS ARE MOVED INTO THE RESPONSE AREA.            00015300
*****************************************************************       00015400
BLDSTK   DS    0H                                                       00015500
*** BUILD STACK RESPONSE RECORD                                         00015600
         LA    WORKREG,UNIQENT      POINT TO CURRENT UNIQUE TABLE       00015700
         LA    R0,MAXUNIQ           TOTAL POSSIBLE ENTRIES IN UNIQTBL   00015800
         CP    UNIQTOT,MAXSEGCT     SEGMENT COUNT WITHIN DESIGN LIMIT?  00015900
         BH    MAXSEG               NO RETURN BAD RESPONSE TO USER      00016000
         CLI   CTLSGFLG,CTLSGON     COMPARE USING CTLS AND SEG?         00016100
         BNE   STACKCHK             NO, CONTINUE EDITS                  00016200
         CP    UNIQTOT,PKED1        DID AN ENTRY MATCH CRITERIA?        00016300
         BL    NOMATCH              NO, TELL THE CALLER                 00016400
STACKCHK DS    0H                                                       00016500
         CP    UNIQTOT,PKED1        AT LEAST ONE ENTRY RETURNED?        00016600
         BL    NORSERR              NO, TELL THE CALLER                 00016700
         LA    R15,LKPSTKTB         POINT TO STACK TABLE                00016800
         CLC   UNIQSPAC,ZEROS2      IS SPACES AN ENTRY?                 00016900
         BE    STACKLP              NO JUST DO NORMAL PROCESSING        00017000
*** FOR A SEGMENT NUMBER REQUEST, DONT RETURN SPACES ***                00017100
         CLI   LKPREQ1,LKPSEG       IS THIS A SEGMENT LOOKUP REQUEST    00017200
         BE    STACKLP              YES, DONT INCLUDE SPACES            00017300
         CLI   LKPREQ1,LKPSEG1       DITTO                              00017400
         BE    STACKLP                                                  00017500
         MVC   0(L'UNIQSPAC,R15),UNIQSPAC    SPACES, MOVE TO STK        00017600
         LA    R15,L'LKPSTKTB(R15)   POINT TO NEXT ENTRY IN STACK       00017700
STACKLP  DS    0H                                                       00017800
         CLC   0(L'UNIQENT,WORKREG),ZEROS2 IS IT INITIALIZED? 2602266   00017900
         BE    ENDSTACK                 NO, RETURN            2602266   00018000
         MVC   0(L'LKPSTKTB,R15),0(WORKREG) SEGMENT IS USED - MOVE IT   00018100
         LA    R15,L'LKPSTKTB(R15)     POINT TO NEXT STACK AREA         00018200
         LA    WORKREG,L'UNIQENT(WORKREG) POINT TO NEXT UNIQTBL ENTRY   00018400
         BCT   R0,STACKLP                                               00018500
ENDSTACK DS    0H                                             2602266   00018550
          SPACE 3                                                       00018600
RETURN   SIRETRN RC=(15)         RETURN TO CALLING PGM                  00018700
         TITLE 'SISSLKP  -  RETURN A SINGLE ENTRY RTN'                  00018800
*************************************************************           00018900
** THIS ROUTINE RETURNS ONE AND ONLY ONE ENTRY BASED ON THE             00019000
** CONTROLS PASSED IN THE SEARCH ARGUMENT. IF THE SSR TABLE             00019100
** CONTAINS 12 ENTRIES OR MORE, A BINARY SEARCH IS PERFORMED.           00019200
** OTHERWISE, A SERIAL SEARCH IS DONE.  THE RESPONSE TO THIS            00019300
** CALL IS A SINGLE ELEMENT (2 CHARACTERS) FOR A LKPREQ2=4,             00019400
** WHILE IT IS A FULL SSR BATCH ENTRY (50 CHAR) WHEN THE                00019500
** LKPREQ2=0 OR SPACE.                                                  00019600
*************************************************************           00019700
CHKSRCH1 DS    0H        ROUTINE TO SEARCH BASED ON CONTROLS            00019800
         LA    SSRDETRG,SIBLTBDE   POINT TO BEGINNING OF TABLE          00019900
         USING SIBLDET,SSRDETRG    GIVE ADDRESSABILITY TO DETAIL RCD    00020000
         LA    R9,28               SET TO MAX LENGTH          2023258   00020010
         CLI   LKPREQ2,LKPACCT     IS THIS ACCT LVL LOOKUP?   2023258   00020020
         BNE   GOTLEN              NO                         2023258   00020030
         LA    R8,LKPTCTLS+L'LKPTCTLS-1  POINT TO END OF KEY  2023258   00020040
LENLOOP  DS    0H                  BUMP BACKWARDS THRU KEY    2023258   00020045
         CLI   0(R8),C' '          REACHED A NON-BLANK CHAR   2023258   00020050
         BNE   CHKLOW              CHK LOW-VALUES             0535152   00020060
         B     LENLOOP1            LOOK AT NEXT POSITION      0535152   00020064
CHKLOW   EQU   *                                              0535152   00020066
         CLI   0(R8),X'00'                                    0535152   00020068
         BNE   GOTLEN              YES, STOP HERE             0535152   00020070
LENLOOP1 EQU   *                                              0535152   00020072
         BCTR  R8,0                LOOK AT NEXT POSITION      0535152   00020074
         BCT   R9,LENLOOP          LOOP BACK UP               0535152   00020076
GOTLEN   DS    0H                                             2023258   00020085
         BCTR  R9,0                DECREMENT ONE FOR EX       2023258   00020090
         EX    R9,COMPSSR          CMP ARGUMENT VS FIRST ENTRY2023258   00020100
         BH    CKHITBL             ARGUMENT HIGH, BYPASS NEXT CODE      00020200
         BE    CHKRETNT            EQUAL, FOUND THE DESIRED RCD         00020300
*** OTHERWISE, ENTRY IS TOO LOW    - SET RETURN CODE ***                00020400
         MVI   LKRETCOD,LKPLOWRT   SET ERROR CODE                       00020500
         B     CHKRETNT            AND RETURN FIRST ENTRY               00020600
CKHITBL  DS    0H                  ARGUMENT GREATER THAN 1ST TBL ENTRY  00020700
         L     SSRDETRG,LASTSSR    GET ADDRESS OF LAST ENTRY            00020800
         EX    R9,COMPSSR          CMP ARG WITH LAST TBL ENTRY2023258   00020900
         BE    CHKRETNT            RETURN THIS ENTRY IF EXACT HIT       00021000
         BL    CKSELSCH            ARGUMENT LOW, SELECT TYPE SEARCH     00021100
         B     CHKRETNT              AND RETURN HIGHEST ENTRY           00021200
CKSELSCH DS    0H                                                       00021300
         LA    SSRDETRG,SIBLTBDE   RESET TO BEGINNING OF TABLE          00021400
         CP    SIBLSNOE,BINTHRSH   SHOULD A BINARY SEARCH BE PERFORMED? 00021500
         BNH   SERSR14             NO, GO PERFORM SERIAL SEARCH         00021600
         TITLE 'SISSLKP  -  BINARY SEARCH ROUTINE'                      00021700
         SPACE 3                                                        00021800
*** BINARY SEARCH ROUTINE USES THE MIDPOINTS STORED IN THE SSR TABLE ** 00021900
         SPACE 3                                                        00022000
         LA    WORKREG,SIBLSMP1    POINT TO FIRST BINARY SEARCH DISPL.  00022100
         AH    SSRDETRG,0(WORKREG) POINT TO ENTRY IN MIDDLE             00022200
         XC    FDBNTRY,FDBNTRY     ZERO OUT FOUND ADDRESS               00022300
         LA    R0,SIBLDLEN         GET BINARY VALUE OF SSR REC LEN      00022400
         STH   R0,NTRYLEN          SAVE FOR COMPARE                     00022500
BINLOOP  DS    0H                                                       00022600
         EX    R9,COMPSSR           CMP CURR MIDPT TO REQUEST 2023258   00022700
         BE    BINFOUND             YES, EXIT ROUTINE                   00022800
         BH    BINHI                IF SEARCHARG IS HI, TRY ANOTHER     00022900
*** OTHERWISE, SEARCH ENTRY IS LOWER THAN TABLE ENTRY, TRY LOW END      00023000
BINLO    DS    0H                                             2024226   00023010
         CLC   0(L'SIBLSMP1,WORKREG),NTRYLEN LENGTH DOWN TO ONE ENTRY?  00023100
         BNH   BINEND               YES, GET PREVIOUS ENTRY, IF ANY     00023200
         LA    WORKREG,L'SIBLSMP1(WORKREG)   GET NEXT MIDPT IN TABLE    00023300
         SH    SSRDETRG,0(WORKREG)  SUBTRACT TO HIGHER ENTRIES          00023400
         B     BINLOOP              REITIERATE TO COMPARE               00023500
BINHI    DS    0H                                                       00023600
         ST    SSRDETRG,FDBNTRY     SAVE IN CASE THIS IS IT             00023700
         CLC   0(L'SIBLSMP1,WORKREG),NTRYLEN LENGTH DOWN TO ONE ENTRY?  00023800
         BNH   BINEND               YES, GET PREVIOUS ENTRY, IF ANY     00023900
         LA    WORKREG,L'SIBLSMP1(WORKREG)   GET NEXT MIDPT IN TABLE    00024000
         AH    SSRDETRG,0(WORKREG)  ADD TO HIGHER ENTRIES     2024226   00024100
         C     SSRDETRG,LASTSSR     ARE WE BEYOND SSR TABLE   2024226   00024110
         BH    BINLO                  Y. BACK UP TO PREV MIDPT2024226   00024120
         B     BINLOOP              REITIERATE TO COMPARE               00024200
BINFOUND DS    0H                   FOUND ENTRY, CONTINUE               00024300
         ST    SSRDETRG,FDBNTRY     EQUAL COMPARE, SEARCH CAN COMPLETE  00024400
BINEND   DS    0H                   END OF SEARCH REACHED, TAKE CLOSEST 00024500
         ICM   SSRDETRG,X'F',FDBNTRY  WAS ANY ENTRY LOWER OR EQUAL      00024600
         BP    CHKRETNT             YES, USE AS CURRENT ENTRY           00024700
*** OTHERWISE, WE DID NOT GET A MATCH ***                               00024800
         MVI   LKRETCOD,LKPNRMRT    INDICATE THAT ENTRY IS TOO LOW      00024900
         LA    SSRDETRG,SIBLTBDE    POINT TO FIRST ENTRY                00025000
         B     CHKRETNT             PASS IT TO USER                     00025100
COMPSSR  CLC   LKPTCTLS(0),SIBLDCTL COMPARE PASSED KEY TO SSR 2023258   00025110
         TITLE 'SISSLKP  -  SERIAL SEARCH ROUTINES'                     00025200
         SPACE 2                                                        00025300
*** SERIAL SEARCH LOGIC FOR SINGLE ENTRY (USES FULL CONTROLS) ***       00025400
         SPACE 2                                                        00025500
SERSR14  DS    0H                                                       00025600
         LA    R15,L'LKPTCTLS      COMPARE IS ON FULL LENGTH            00025700
         BCTR  R15,0               DECREMENT BY 1                       00025800
         CLI   LKPREQ2,LKPACCT     ACCT LEVEL SEGMENTATION    2023258   00025810
         BNE   SERSRLEN            NO, R15 IS GOOD            2023258   00025820
         LR    R15,R9              SET R15 WITH EXACT KEY LEN 2023258   00025830
SERSRLEN DS    0H                                             2023258   00025840
         LA    R8,L'LKPTBSEG       COMPARE LENGTH OF SEGMENT            00025900
         BCTR  R8,0                DECREMENT BY 1                       00026000
         BAS   RETRG,SERSRCH       CALL SERIAL SEARCH ROUTINE 2024183   00026100
*** ENTRY FOUND THAT IS TO BE RETURNED ***                              00026200
CHKRETNT DS    0H                                                       00026300
         CLI   LKPREQ2,LKPDET1     IS ENTIRE SSR RECORD TO BE RETURNED  00026400
         BE    RETSSR              YES, JUST MOVE IN SSR RECORD         00026500
         CLI   LKPREQ2,LKPDET2     ALSO MEANS ENTIRE SSR IS REQUESTED   00026600
         BE    RETSSR                                                   00026700
         BAS   RETRG,LKPLVL        YES, GET THE GROUP         2024183   00026800
         CLI   CTLSGFLG,CTLSGON    COMPARE USING CTLS AND SEG?          00026900
         BNE   CHKRETCT            NO, CONTINUE EDITS                   00027000
         CP    UNIQTOT,PKED1       DID AN ENTRY MATCH CRITERIA?         00027100
         BL    NOMATCH             NO, TELL THE CALLER                  00027200
CHKRETCT DS    0H                                                       00027300
         MVC   LKPSTKTB,CURRENT  MOVE IN VALUE RETURNED BY LKPLVL       00027400
         B     RETURN                                                   00027500
RETSSR   DS    0H                                                       00027600
         MVC   LKPRESP(SIBLDLEN),SIBLDET OTHERWISE, RETURN ENTIRE SSR   00027700
         B     RETURN                                                   00027800
         SPACE 3                                                        00027900
         TITLE 'SISSLKP - SEARCH LOGIC FOR MULTIPLE ENTRIES '           00028000
         SPACE 3                                                        00028100
MULTSRCH DS    0H      SEARCH FOR ALL HITS BASED ON A PARTIAL KEY       00028200
         CLI   LKPREQ2,LKPCTL1    IS IT LOOKUP BY CONTROL 1 ONLY        00028300
         BNE   TRYCTL2            NO, TRY CONTROL 2                     00028400
         LA    R15,L'LKPTCTL1     GET LENGTH FOR COMPARE                00028500
         B     MULTSTUP           SETUP FOR LOOP                        00028600
TRYCTL2  DS    0H                                                       00028700
         CLI   LKPREQ2,LKPCTL2    LOOKUP BY CONTORL 2?                  00028800
         BNE   TRYCTL3            NO, TRY FOR CTL 3                     00028900
         LA    R15,L'LKPTCTL2     LENGTH FOR COMPARE                    00029000
         B     MULTSTUP           SETUP FOR LOOP                        00029100
TRYCTL3  DS    0H                                                       00029200
         CLI   LKPREQ2,LKPCTL3    LOOKUP BY CONTORL 3?                  00029300
         BNE   TRYC1SG            NO, TRY FOR CTL 1 AND SEG             00029400
         LA    R15,L'LKPTCTL3     GET LENGTH FOR COMPARE                00029500
         B     MULTSTUP           SETUP FOR LOOP                        00029600
TRYC1SG  DS    0H                                                       00029700
         CLI   LKPREQ2,LKPC1SG    LOOKUP BY CTL 1 AND SEG?              00029800
         BNE   TRYC2SG            NO, TRY FOR CTL 2 AND SEG             00029900
         LA    R15,L'LKPTCTL1     GET LENGTH FOR COMPARE                00030000
         B     MULTSTUP           SETUP FOR LOOP                        00030100
TRYC2SG  DS    0H                                                       00030200
         CLI   LKPREQ2,LKPC2SG    LOOKUP BY CTL 2 AND SEG?              00030300
         BNE   TRYC3SG            NO, TRY FOR CTL 3 AND SEG             00030400
         LA    R15,L'LKPTCTL2     LENGTH FOR COMPARE                    00030500
         B     MULTSTUP           SETUP FOR LOOP                        00030600
TRYC3SG  DS    0H                 IF NONE OF ABOVE, MUST BE CTL 3       00030700
         LA    R15,L'LKPTCTL3     GET LENGTH FOR COMPARE                00030800
MULTSTUP DS    0H                SETUP FOR COMPARE LOOP                 00030900
         LA    SSRDETRG,SIBLTBDE  POINT TO FIRST ENTRY                  00031000
         BCTR  R15,0               DECREMENT BY 1 FOR EX INSTRUCTION    00031100
         STH   R15,MULTLEN         SAVE MULTIPLE LENGTH ACROSS SUBRTNS  00031200
         LA    R8,L'LKPTBSEG       COMPARE LENGTH OF SEGMENT            00031300
         BCTR  R8,0                DECREMENT BY 1                       00031400
*** CHECK FOR REQUEST LOWER THAN TABLE ENTRIES ***                      00031500
         EX    R15,EXCOMP          COMPARE TO LOW END OF THE TABLE      00031600
         BNH   MUTSTHI             HIGH, TEST HIGH END OF TABLE         00031700
         MVI   LKRETCOD,LKPLOWRT   SET ERROR CODE                       00031800
         MVI   ENDTBFLG,ENDTBON    SET END OF TABLE TO PREVENT MORE     00031900
         B     MULTFND             GO RETURN FIRST ENTRY                00032000
MUTSTHI  DS    0H                  TEST FOR EQUAL OR HI                 00032100
         BE    MULTFND             FOR EQUAL TO 1ST ENTRY, CONTINUE     00032200
         L     SSRDETRG,LASTSSR    POINT TO LAST ENTRY                  00032300
         EX    R15,EXCOMP          COMPARE FOR HIGH END RANGE           00032400
         BNL   MLTLPBEG            NOT HIGH, GO TO LOOP                 00032500
         MVI   ENDTBFLG,ENDTBON    SET END OF TABLE TO PREVENT MORE     00032600
         B     MULTFND             RETURN LAST ENTRY                    00032700
MLTLPBEG DS    0H                  OTHERWISE, ENTRY IS WITHIN TABLE     00032800
         LA    SSRDETRG,SIBLTBDE   POINT TO FIRST TABLE ENTRY           00032900
MULTLOOP DS    0H                                                       00033000
         LH    R15,MULTLEN        GET LENGTH FOR COMPARE                00033100
         BAS   RETRG,SERSRCH      CALL SERIAL SEARCH          2024183   00033200
         CLI   ENDTBFLG,ENDTBON   IS IT END OF TABLE?                   00033300
         BNE   MULTFND            NO, FOUND AN ENTRY                    00033400
         CP    UNIQTOT,PKED1      WAS ONE ENTRY FOUND YET?              00033500
         BNL   MULTEND            YES, ALL FINISHED                     00033600
MULTFND  DS    0H                 FOUND ENTRY, TURN ON FLAG             00033700
         BAS   RETRG,LKPLVL       WITH CORRECT VALUE          2024183   00033800
         CLI   PREVFLG,PREVON     WAS PREVIOUS ENTRY INSPECTED ONCE     00033900
         BE    TRYMLTND           YES, DONT DO AGAIN                    00034000
         MVI   PREVFLG,PREVON     OTHERWISE SET FLAG FOR NEXT TIME      00034100
         OC    PREVSSR,PREVSSR    IS THERE A PREVIOUS SSR?              00034200
         BZ    TRYMLTND           NO, DONT TRY ANY PREVIOUS             00034300
         C     SSRDETRG,PREVSSR   RETURNED ENTRY EQUAL PREVIOUS ENTRY?  00034400
         BE    TRYMLTND           YES, ALREADY USED THIS ENTRY - BYPASS 00034500
*** SHOULD ONLY USE ENTRY THAT FITS CONTROLS ***                        00034600
         PACK  DWORD,SIBLDCTL     PACK THE SSR PASSED                   00034700
         CLI   DWORD+7,PKEDSP     IS SPACES AT END OF SSR RCD           00034800
         BNE   SSRNUM             NO, CAN PROCEED WITH ARITHMETIC       00034900
         MVI   DWORD+7,PKED0I     OTHERWISE, OVERLAY WITH PKED ZERO     00035000
SSRNUM   DS    0H                                                       00035100
         SP    DWORD,PKED1        DECREMENT BY 1                        00035200
         UNPK  PRVCTLS,DWORD      RESTORE INTO TEMPORARY AREA           00035300
         OI    PRVCTLS+L'PRVCTLS-1,NUMDISP MAKE DISPLAYABLE             00035400
         LH    R15,MULTLEN        PICK UP LENGTH FOR COMPARE            00035500
         EX    R15,EXCOMPT        NOW CHECK IF NEED PREVIOUS ENTRY      00035600
         BL    TRYMLTND           NO, PREVIOUS SSR IS LOWER             00035700
         LR    R0,SSRDETRG        OTHERWISE, SAVE CURRENT SSR PTR       00035800
         L     SSRDETRG,PREVSSR   PICK UP PREVIOUS ENTRY FROM SERSRCH   00035900
         BAS   RETRG,LKPLVL       RETURN THIS ENTRY           2024183   00036000
         LR    SSRDETRG,R0        RESTORE CURRENT ENTRY PTR             00036100
TRYMLTND DS    0H                 NOW CHECK FOR END OF TABLE            00036200
         CLI   ENDTBFLG,ENDTBON   IS IT REALLY THE END OF TABLE         00036300
         BNE   MULTLOOP           NO, CONTINUE LOOP                     00036400
MULTEND  DS    0H                                                       00036500
         B     BLDSTK             FORMAT RESPONSE BASED ON FLAG TABLE   00036600
EXCOMPT  CLC   PRVCTLS(1),LKPTCTLS COMPARE SSR VS REQUEST (EXECUTED)    00036700
         TITLE 'SISSLKP - SEARCH SEARCH SUBROUTINE '                    00036800
SERSRCH  DS    0H                                                       00036900
**************************************************************          00037000
*** SERSRCH IS A SUBROUTINE TO PERFORM SERIAL SEARCHES.   ***           00037100
*** INPUT: SSRDETRG POINTS TO CURRENT SSR TABLE ENTRY     ***           00037200
***        RETRG CONTAINS RTURN ADDRESS                   ***           00037300
***        R15 CONTAINS LENGTH FOR COMPARE (ALREADY       ***           00037400
***            DECREMENTED)                               ***           00037500
***        LASTSSR CONTAINS ADDRESS OF LAST ACTIVE ENTRY  ***           00037600
***            IN THE SSR TABLE                           ***           00037700
***        R8 CONTAINS LENGTH FOR SEGMENT COMPARE         ***           00037800
***            (ALREADY DECREMENTED)                      ***           00037900
*** OUTPUT: NEW SSRDETRG FOR THE CLOSEST COMPARE          ***           00038000
***         ENDTBLFG SET TO ONE IF END OF TABLE           ***           00038100
***                                                       ***           00038200
**************************************************************          00038300
         SPACE 3                                                        00038400
         MVI   ENDTBFLG,ENDTBOFF    INITIALIZE END FLAG TO ZERO         00038500
SERLOOP  DS    0H                                                       00038600
         ST    SSRDETRG,PREVSSR     SAVE CURRENT ENTRY, IN CASE         00038700
         LA    SSRDETRG,SIBLDLEN(SSRDETRG) GET NEXT ENTRY               00038800
         C     SSRDETRG,LASTSSR     IS THIS LAST SSR ADDRESS            00038900
         BNH   SERCOMP              NO, GO DO COMPARE                   00039000
         B     ENDSRCH              IF PAST END OF TABLE DONT COMPARE   00039100
SERCOMP  DS    0H                                                       00039200
         EX    R15,EXCOMP           COMPARE SSR ENTRY AGAINST CONTROLS  00039300
         BL    SERLOOP              IF SSR HIGH, REPEAT LOOP            00039400
         BE    EXITSRCH             EQUAL HIT, RETURN    2511795        00039500
ENDSRCH  DS    0H                                                       00039600
         MVI   ENDTBFLG,ENDTBON     BEYOND RANGE OF ARGUMENT - INDICATE 00039700
         L     SSRDETRG,PREVSSR     IF SSR HIGH, WANT PREVIOUS ENTRY    00039800
EXITSRCH DS    0H                                                       00040600
         BR    RETRG                                                    00040700
EXCOMP   CLC   SIBLDCTL(1),LKPTCTLS COMPARE SSR VS REQUEST (EXECUTED)   00040800
EXCOMPSG CLC   SIBLDSEG(1),LKPTBSEG COMPARE SSR VS REQUEST              00040900
         TITLE 'SISSLKP  - LOCATE PROPER ELEMENT'                       00041000
********************************************************************    00041100
* LKPLVL - LOOKUP TYPE OF DATA TO RETURN (SEGMENT, MASTER GROUP,   *    00041200
*          CONTROL GROUP, ALPHA GROUP)                             *    00041300
*  INPUT: RETRG   - SET TO RETURN POINT                            *    00041400
*         LKPREQ1 - FROM CALLER INDICATES TYPE OF DATA             *    00041500
*         SSRDETRG- POINTS TO CURRENT SSR TABLE ENTRY              *    00041600
*  OUTPUT: UNIQTBL- UPDATED WITH NUMBER TO BE RETURNED AT RELATIVE *    00041700
*                   ADDRESS IN UNIQUE TABLE                        *    00041800
*          CURRENT - CURRENT VALUE STORED IN UNIQUE TABLE               00041900
*  WORKREG IS DESTROYED IN THIS ROUTINE                            *    00042000
********************************************************************    00042100
LKPLVL   DS    0H                                                       00042200
         CLI   CTLSGFLG,CTLSGON     COMPARE USING CTLS AND SEG?         00042300
         BNE   TRYSEGL              NO, CONTINUE PROCESSING             00042400
         EX    R8,EXCOMPSG          COMPARE SEGMENT NUMBER              00042500
         BE    TRYSEGL              EQUAL, CONTINUE                     00042600
         B     UPDURET              DONT RETURN A CNTL GROUP            00042700
TRYSEGL  DS    0H                                                       00042800
         CLI   LKPREQ1,LKPSEG       IS REQUEST FOR SEGMENT LIST         00042900
         BNE   TRYMGRP                                                  00043000
         LA    WORKREG,SIBLDSEG     YES, POINT TO SEGMENT NBR           00043100
*** FOR SEGMENT NUMBER OF SPACES, DONT RETURN ***                       00043200
         CLC   0(L'SIBLDSEG,WORKREG),SPACES2  IS IT SPACES              00043300
         BE    UPDURET               YES, DONT RETURN FOR SEGMENT NBRS  00043400
         B     UPDUNIQ                OTHERWISE, INSERT INTO UNIQUE TBL 00043500
TRYMGRP  DS    0H                                                       00043600
         CLI   LKPREQ1,LKPMST       IS IT REQUEST FOR MASTER GRP #      00043700
         BNE   TRYCGRP                                                  00043800
         LA    WORKREG,SIBLDMGR     YES, POINT TO MASTER GRP #          00043900
         B     UPDUNIQ                                                  00044000
TRYCGRP  DS    0H                                                       00044100
         CLI   LKPREQ1,LKPCTL       IS IT REQUEST FOR CONTROL GRP #     00044200
         BNE   ISALPHA              NO, IT MUST BE AN ALPHA GRP REQ     00044300
         LA    WORKREG,SIBLDCGR     PICK UP CONTROL GROUP PTR           00044400
         B     UPDUNIQ                                                  00044500
ISALPHA  DS    0H                                                       00044600
         LA    WORKREG,SIBLDAGR     POINT TO ALPHA GROUP                00044700
UPDUNIQ  DS    0H                   ROUTINE TO UPDATE FLAG TABLE        00044800
         MVC   CURRENT(L'CURRENT),0(WORKREG) RETURN CURRENT GRP/SEG     00044900
         CLC   0(L'CURRENT,WORKREG),SPACES2  IS NUMBER SPACES           00045000
         BE    UPDSPAC                                        2602266   00045100
         CP    UNIQTOT,MAXSEGCT    SEG CT WITHIN DESIGN LIMIT 2602266   00045200
         BH    UPDURET                NO, DONT UPDATE         2602266   00045300
         LA    R15,UNIQENT            ADDRESS TABLE           2602266   00045400
GETUDISP DS    0H                                                       00045500
         CLC   0(L'UNIQENT,R15),0(WORKREG) ENTRY IN TABLE?    2602266   00045550
         BE    UPDURET                YES, DONT REUPDATE      2602266   00045600
         CLC   0(L'UNIQENT,R15),ZEROS2     OPEN SLOT IN TABLE 2602266   00045650
         BE    STUNIQ                 YES, STORE IN TABLE     2602266   00045700
         LA    R15,L'UNIQENT(R15)     BUMP TO NEXT            2602266   00045720
         B     GETUDISP               CONTINUE SEARCH         2602266   00045730
UPDSPAC  EQU   *                                              2602266   00045750
         LA    R15,UNIQSPAC           ADDRESS SPACE ENTRY     2602266   00045800
         CLC   0(L'UNIQENT,R15),ZEROS2     OPEN SLOT IN TABLE 2602266   00045850
         BNE   UPDURET                NO, DONT REUPDATE       2602266   00045900
STUNIQ   EQU   *                                              2602266   00045950
         MVC   0(L'UNIQENT,R15),0(WORKREG)    STORE INTO UNIQUE TABLE   00046000
         AP    UNIQTOT,PKED1          INCREMENT ENTRY NBR               00046100
UPDURET  DS    0H                                                       00046200
         BR    RETRG                  RETURN TO CALLER                  00046300
         TITLE 'SISSLKP - ERROR ROUITNES'                               00046400
*                                                                     * 00046500
*********************************************************************** 00046600
*                                                                     * 00046700
* * * *                ERROR ROUTINES                           * * * * 00046800
*                                                                     * 00046900
*********************************************************************** 00047000
*                                                                     * 00047100
ERRACT1  DS    0H             INVALID ACTION FLAG 1                     00047200
         MVI   LKRETCOD,LKPINVRT  INDICATE TO CALLER                    00047300
         B     RETURN                                                   00047400
ERRACT2  DS    0H             INVALID ACTION FLAG 1                     00047500
         MVI   LKRETCOD,LKPINVRT  INDICATE TO CALLER                    00047600
         B     RETURN                                                   00047700
ERRACT3  DS    0H             INVALID ACTION FLAG 1                     00047800
         MVI   LKRETCOD,LKPINVRT  INDICATE TO CALLER                    00047900
         B     RETURN                                                   00048000
ERREMPTY DS    0H              EMPTY TABLE ERROR                        00048100
         MVI   LKRETCOD,LKPMPTRT   INDICATE TO CALLER                   00048200
         B     RETURN                                                   00048300
NORSERR  DS    0H              EMPTY RESPONSE TABLE - NOTHING FOUND     00048400
         MVI   LKRETCOD,LKPMPTRT   INDICATE TO CALLER                   00048500
         B     RETURN                                                   00048600
MAXSEG   DS    0H                                                       00048700
         MVI   LKRETCOD,LKPMAXRT TOO MANY ENTRIES IN RESPONSE           00048800
         B     RETURN                                                   00048900
NOMATCH  DS    0H                                                       00049000
         MVI   LKRETCOD,LKPNOMAT NO MATCH ON CTLS/BATCH SEG COMBINATION 00049100
         B     RETURN                                                   00049200
         TITLE 'SISSLKP-STATIC STORAGE,LITERALS,EQUATES,AND CONSTANTS'  00049300
*                                                                     * 00049400
*********************************************************************** 00049500
*                                                                     * 00049600
* * * *    STATIC STORAGE + LITERALS, EQUATES, AND CONSTANTS    * * * * 00049700
*                                                                     * 00049800
*********************************************************************** 00049900
*                                                                     * 00050000
         DS    0D                                                       00050100
WKSTGSEG DC    CL12'**WKSTGSEG**'                                       00050200
PGMID    DC    CL8'SISSLKP'                                             00050300
SSRDETRG EQU   6                  SSR DETAIL REGISTER                   00050400
WORKREG  EQU   7                                                        00050500
BASEREG  EQU   12                 BASE REGISTER                         00050600
RETRG    EQU   14                 RETURN REGISTER                       00050700
         SPACE 2                                                        00050800
BINTHRSH DC    PL2'12'             THRESHOLD FOR DOING BINARY SEARCH    00051000
LKPALLCT DC    CL2'**'              ALL SEGMENT/GROUP NBRS ARE DESIRED  00051100
PKED1    DC    PL1'1'                                                   00051200
PKED0    DC    PL1'0'                                                   00051300
SPACES2  DC    CL2'  '                                                  00051400
ZEROS2   DC    XL2'0000'                                                00051500
PKED0I   EQU   X'0F'                                                    00051600
PKEDSP   EQU   X'04'                                                    00051700
NUMDISP  EQU   X'0F'                                                    00051800
         TITLE 'SISSRTN - GENERATED LITERALS, EQUATES, AND CONSTANTS'   00051900
*                                                                     * 00052000
*********************************************************************** 00052100
*                                                                     * 00052200
* * * *          GENERATED LITERALS, EQUATES, AND CONSTANTS     * * * * 00052300
*                                                                     * 00052400
*********************************************************************** 00052500
*                                                                     * 00052600
         LTORG                                                          00052700
         TITLE 'SISSGNX'  - DYNAMIC STORAGE'                            00052800
*                                                                     * 00052900
*********************************************************************** 00053000
*                                                                     * 00053100
* * * *                  DYNAMIC STORAGE                        * * * * 00053200
*                                                                     * 00053300
*********************************************************************** 00053400
*                                                                     * 00053500
*        DYNAMIC SECTION OF DATA - GETMAIN AREA FOR REENTRANCY          00053600
DWORD    DC    D'0'          AREA FOR BINARY CONVERSIONS                00053700
CURRENT  DC    XL2'0000'     CURRENT GROUP/SEGEMENT NUMBER - LKPLVL     00053800
ENDTBFLG DC    X'00'         END TABLE INDICATOR (SERSRCH)              00053900
ENDTBOFF EQU   C'0'                       TURN OFF END INDICATOR        00054000
ENDTBON  EQU   C'1'                       INDICATE END OF TABLE         00054100
PREVFLG  DC    X'00'         MULTIPLE SEARCH - FIRST TIME FLAG          00054200
PREVON   EQU   C'1'                 PREVIOUS FLAG SET ON                00054300
CTLSGFLG DC    X'00'         CONTROLS AND SEGMENT LOOKUP FLAG           00054400
CTLSGON  EQU   C'1'                 USE CTLS/SEG TO LOOKUP              00054500
ORIG1    DC    A(0)          ADDRESS OF PREVIOUS SSR ENTRY              00054600
SAVEREGS  DC    4F'0'       REGISTER SAVEAREA                 0525104   00054650
PREVSSR  DC    A(0)          ADDRESS OF PREVIOUS SSR ENTRY              00054700
LASTSSR  DC    A(0)          ADDRESS OF LAST ENTRY IN SSR TABLE         00054800
FDBNTRY  DC    A(0)          ADDRESS OF MOST RECENT ENTRY IN SSR TBL    00054900
NTRYLEN  DC    H'0'          BINARY VALUE OF SSR REC LENGTH             00055000
MULTLEN  DC    H'0'          LENGTH FOR LEVEL COMPARE                   00055100
PRVCTLS  DC    XL14'00'      AREA TO DETERMINE IF PREVIOUS SSR NEEDED   00055200
LENADCNS EQU   *-DWORD       USED FOR INTIALIZATION                     00055300
MAXSEGCT DC    PL2'0'        MAX NO SEGMENTS FOR THE STACK    0525104   00055310
*** TABLE FOR UNIQUE NUMBER CALCULATIONS ***                            00055400
UNIQTBL  DS    0C                                                       00055500
UNIQTOT  DC    PL2'0'      TOTAL UNIQUE ENTRIES                         00055600
UNIQENT  DC    201XL2'0000'  201 ENTRIES                      0525104   00055700
UNIQSPAC DC    XL2'00'       SPECIAL ENTRY FOR SPACES(NOT NUMERIC)      00055800
TOTULEN  EQU   *-UNIQTBL     TOTAL TABLE LENGTH FOR CLEARING OUT        00055900
MAXUNIQ  EQU   201           TOTAL ENTRIES IN UNIQUE TBL      0525104   00056000
*** END OF UNIQUE TABLE                                                 00056100
SISSLKP  CSECT                                                          00056200
         LTORG                                                          00056300
         DS    0D                                                       00056400
SITMSTMP DC    CL64'SISSLKP   -----TSD-             02/16/05  15.57.42' 00056500
*        THIS PROGRAM CONTAINS TRADE SECRETS THAT BELONG                00056501
*        TO FIDELITY INFORMATION SERVICES AND IS                        00056502
*        LICENSED BY AN AGREEMENT.  ANY UNAUTHORIZED ACCESS,            00056503
*        USE, DUPLICATION, OR DISCLOSURE IS UNLAWFUL.                   00056504
*        COPYRIGHT FIDELITY INFORMATION SERVICES                        00056505
*        2005, ALL RIGHTS RESERVED.                                     00056506
         END                                                            00056600
