*     * 802262 12/11/20 PROYECTO UPGRADE GN172 EDVR
         TITLE 'SISPOOL SUBROUTINE'                                     00000100
*---------------------------------------------------------------------* 00000110
*                  ** HISTORY OF REVISIONS **                         * 00000120
*                                                                     * 00000130
* DESCRIPTION                                                 CHNGID  * 00000140
* __________________________________________________________  _______ * 00000150
* 12/24/09 HYPERLINK NOTIFICATION CONTINUATION                1035884 * 00000168
* 06/31/08 CORRECT VSE ADDRESSABILITY ERRORS                  0915739   00000169
* 06/24/05 ADD CODE TO SUPPORT HTML PROCESSING                0615297 * 00000170
* 05/17/04 MODIFY CALL FOR COBOL/VSE                          0444987   00000172
* 06/24/03 CORRECT ABEND WITH ONLY 3 PARAMETERS PASSED        0354531   00000174
* 02/06/03 BYPASS $ IN XEROX CONTROL INE                      0334692 * 00000175
* 11/27/01 ADDED PROCESSING OF HEADING LANG IN CASE HEADINGS  2024378 * 00000176
*          ARE DIFERENT LANG THAN DETAIL                      2024378 * 00000177
* 08/09/01 ADDED A CHECK FOR THE $ PRINT INDICATOR BEFORE     2024346 * 00000178
*          BLANKING THE $ OUT OF THE PRINT LINE               2024346 * 00000179
* 03/02/00 CHANGED BAL TO BAS                                 2024183 * 00000180
* 12/08/99 REPLACED LOOPS TO FIND ' ' & '$' WITH TR INST.     2024207 * 00000181
* 11/30/99 CHG ACCESS POSITION OF DECIMAL                     2024087 * 00000183
* 12/16/98 FIX PRINT WHEN DBCS IS IN 1ST LINE                 9914008 * 00000185
* 12/10/97 SIOPTNS GLOBALIZATION - SASIOPTE ADDED             9913483 * 00000187
* 04/29/97 MODIFIED TO USE GNCALL                             9913437 * 00000189
* 01/15/98 HANDLE ONLY 2 PARAMETERS WHEN CLOSING              9913703 * 00000190
* 09/25/97 FIXED OC1 IN SISPOOL FOR SIBLSTAT                  9912009 * 00000191
* 05/01/97 ADDED FLOATING DECIMAL CAPABILITY                  9913481 * 00000193
*          ADDED MULTI-LANG PRINTING CAPABILITY USING TRC     9913481 * 00000195
* 06/30/95 TOOK OUT CHECK FOR &PRTXLT                         2602409 * 00000197
*---------------------------------------------------------------------* 00000198
         EJECT                                                          00000199
         COPY  SIOPTNS                                                  00000200
SISPOOL  START                                                          00000300
         SIBASE BASEREG=(12,7)                                0915739   00000400
         EJECT                                                          00000500
*********************************************************************** 00000600
*        INITIALIZATION AND ENTRY SETUP                               * 00000700
*********************************************************************** 00000800
         LM    R2,R6,0(R1)        LOAD ADDRESSES OF PARMS     0615297   00000900
         STCM  R1,15,SAVER1       SAVE PARM ADDRESS           9913483   00000910
*        R2    STANDARD HEADING AREA                                    00001000
         USING STHDAREA,R2                                              00001100
*        R3    SPOOL CONTROL AREA                                       00001200
         USING LSSPLAR,R3                                               00001300
*        R4    PRINT LINE                                               00001400
         USING LSPRT,R4                                                 00001500
*        R5    SPOOL CONTROL AREA EXTENDED                    9913481   00001510
         USING SPLCTLEX,R5                                    9913481   00001520
         ST    R5,SVSPLLEX        SAVE EXTENDED ADDRESS       1035884   00001522
         ST    R6,SVHTMLEX                                    0615297   00001525
*        R8    WORKING STORAGE SPOOL CONTROL SAVE AREA        9913481   00001600
         USING WSSPLWK,R8                                     9913481   00001700
OPTE100  EQU   *                                              9913483   00001705
         MVC   SVRPTBRK,LSRPTBRK  SAVE RPT BREAK FLAG         0615297   00001707
         LA    R6,OPTEREC         GET SASIOPTE REC ADDR       9913483   00001710
         USING SASIOPTE,R6                                    9913483   00001715
         CLC   SIOPESN,=CL8'SASIOPTE'  RECORD PRESENT?        9913483   00001720
         BE    OPTE110            YES, GO PROCESS             9913483   00001725
         MVC   SIOPESN,=CL8'SASIOPTE'  SET FOR READ           9913483   00001730
         MVC   SIOPEC1,=X'FFFF'    SET CTL1 DFLT              9913483   00001735
         MVI   SIOPESK,C' '        CLEAR SEARCH KEY           9913483   00001740
         MVC   SIOPESK+1(L'SIOPESK-1),SIOPESK   ''            9913483   00001745
         SIPLIST PARMLIST,(OPTEREC)                           0444987   00001747
         ICM   R15,15,SIPBCDAD     SIPBCDIO ADDR              0444987   00001749
         BNZ   OPTE105                                        0444987   00001750
         SACALL SIPBCDIO,TYPE=LOAD                            0444987   00001753
         ST    R1,SIPBCDAD         SAVE ADDR                  0444987   00001756
         LR    R15,R1              LOAD ENTRY POINT           0444987   00001759
OPTE105 EQU    *                   GET SASIOPTE RECORD        0444987   00001762
         LA    R1,PARMLIST                                    0444987   00001765
         BASSM R14,R15             CALL ROUTINE               0444987   00001767
         MVC   WMLTLANG,MLTLANG                               9913483   00001770
         MVC   WFLOATDC,FLOATDC                               9913483   00001775
         MVC   WDIVIDER,DIVIDER                               9913483   00001780
OPTE110  EQU   *                                              9913483   00001785
         DROP  R6                                             9913483   00001790
         ICM   R1,15,SAVER1       LOAD PARM ADDRESS           9913483   00001795
         LA    R4,0(0,R4)         CLEAR HI ORDER BYTE OF R4             00001800
         TM    LSEXTFL,X'E0'      EXTENDED PROCESSING INDICATED         00001900
         BO    A0010              YES - DONT FIX LENGTH                 00002000
         MVC   LSEXTLN,H133       FORCE LENGTH TO 133                   00002100
         CLC   LSEXTFL,=C'L'      IS LANGUAGE PRESENT IN EXT  9913481   00002105
         BE    A0009              YES, DON'T SET LENGTH       9913481   00002110
         B     A0010              BRANCH AROUND LENGTH CHANGE 9913481   00002115
A0009    EQU   *                                              9913481   00002120
         TM    4(R1),X'80'        ARE THERE ONLY 2 PARAMETERS?9913703   00002122
         BO    A0010              YES, EXT AREA NOT PASSED    9913703   00002124
         TM    8(R1),X'80'        IS THIS THE LAST PARAMETER? 9913481   00002125
         BO    A0010              YES, EXT AREA NOT PASSED    9913481   00002130
         MVC   SAVEDEC,DFLTDEC    MOVE IN DEFAULT DECIMALS    2024207   00002131
         MVC   SAVECURR,DFLTCURR  MOVE IN DEFAULT CURRENCY    2024207   00002132
         CLC   WFLOATDC,=C'Y'                                 2024207   00002133
         BNE   A0009C                                         2024207   00002134
         MVC   SAVEDEC,SPLDEC     MOVE DECIMALS TO SAVE AREA  2024207   00002135
         MVC   SAVECURR,SPLCURR   MOVE CURRENCY TO SAVE AREA  2024207   00002136
A0009C   EQU   *                                              2024207   00002137
         MVC   SAVEDLNG,SPLLANG   YES, MOVE DEF LANG TO EXT   2024378   00002138
         MVC   SAVEHLNG,SPLHLNG   MOVE HDR LANG TO EXT        2024378   00002139
         CLC   SPLHLNG,=C'  '     IS HDR LANG SPACES IN EXT   2024378   00002140
         BNE   A0009D             NO DONT DEFAULT TO DETAIL   2024378   00002141
         MVC   SAVEHLNG,SPLLANG   YES, USE DETAIL LANG        2024378   00002142
A0009D   EQU   *                                              2024378   00002143
         CLC   SPLLANG,=C'  '     IS LANGUAGE SPACES IN EXT   2024378   00002144
         BE    A0010              YES, DON'T CHANGE LENGTH    9913481   00002145
         CLC   LSEXTLN,=X'0085'   IS LENGTH 133?              9913481   00002150
         BNE   A0010              NO                          9913481   00002155
         CLC   WMLTLANG,=C'Y'     MULTI-LANGUAGE?             2024378   00002157
         BNE   A0010              NO BYPASS LENGTH CHANGE     2024378   00002158
         MVC   LSEXTLN,H134       FORCE LENGTH TO 134         9913481   00002160
A0010    EQU   *                                                        00002200
         MVC   SAVEPRTD,LSPRTD    SAVE PRINT $ INDICATOR      2024346   00002210
         MVC   WKPRTID,LSPRTID    SAVE PRINT FILE ID                    00002300
         TR    WKPRTID,CTLINDEX   GET CTL TABLE INDEX                   00002400
         SR    R6,R6              CLEAR R6                              00002500
         IC    R6,WKPRTID         LOAD INDEX                            00002600
         MH    R6,H96             MULTIPLY BY LENGTH OF EACH CTL AREA   00002700
         LA    R8,CTLWORK(R6)     POINT TO CORRECT CTL AREA   9913481   00002800
         CLI   WSSPLCD,X'00'      IS AREA INITIALIZED                   00002900
         BNE   B0010              YES                                   00003000
         MVC   WSSPLAR,LSSPLAR    SAVE SPOOL CONTROL AREA               00003100
         EJECT                                                          00003200
*********************************************************************** 00003300
*        CHECK FOR CONTROL BREAKS FOR REPORT RECAP AND BILLING FILES  * 00003400
*********************************************************************** 00003500
B0010    EQU   *                                                        00003600
         CLI   LSSPLCD,C'9'       CLOSE FILES CODE                      00003700
         BE    G0010              YES                                   00003800
         CLC   LSAPPL,=CL2'FM'    IS APPLICATION CODE FM                00003900
         BE    C0010              YES                                   00004000
         CLC   LSAPPL,=CL2'AP'    IS APPLICATION CODE AP                00004100
         BE    C0010              YES                                   00004200
         CLI   LSRPTBRK,C'X'      FORCE REPORT BREAK                    00004300
         BE    B0090              YES                                   00004400
         CLC   WSRPTCTL,LSRPTCTL  ANY BREAK AT ALL                      00004500
         BE    B0100              NO - DONT CHECK ANY MORE              00004600
         CLI   WSRCPLVL,C'1'      CHK FOR CTL1 ONLY RECAP BRK           00004700
         BNE   B0020              NO                                    00004800
         CLC   WSRPCTL1,LSRPCTL1  CTL1 BREAK                            00004900
         BNE   B0090              YES                                   00005000
B0020    EQU   *                                                        00005100
         CLI   WSRCPLVL,C'2'      CHK FOR CTL1 OR CTL2 RECAP BRK        00005200
         BNE   B0030              NO                                    00005300
         CLC   WSRPCTL1(5),LSRPCTL1  CTL1 OR CTL2 BREAK                 00005400
         BNE   B0090              YES                                   00005500
B0030    EQU   *                                                        00005600
         CLI   WSRCPLVL,C'3'      CHK FOR CTL1, CTL2 OR CTL3 RECAP BRK  00005700
         BNE   B0040              NO                                    00005800
         CLC   WSRPCTL1(8),LSRPCTL1  CTL1, CTL2, OR CTL3 BREAK          00005900
         BNE   B0090              YES                                   00006000
B0040    EQU   *                                                        00006100
         CLI   WSRCPLVL,C'4'      CHK FOR CTL4 RECAP BRK                00006200
         BNE   B0100              NO                                    00006300
         CLC   WSRPCTL4,LSRPCTL4  CTL4 BREAK                            00006400
         BE    B0100              NO                                    00006500
B0090    EQU   *                                                        00006600
         BAS   R10,Z0150          WRITE REPORT RECAP RECORD   2024183   00006700
B0100    EQU   *                                                        00006800
         CLI   LSBCRBRK,C'X'      FORCE BCR BREAK                       00006900
         BE    B0190              YES                                   00007000
         CLC   WSBCRCTL,LSBCRCTL  BCR BREAK                             00007100
         BE    D0010              NO                                    00007200
B0190    EQU   *                                                        00007300
         BAS   R10,Z0160     WRITE BILLING STATISTICS RECORD  2024183   00007400
         B     D0010                                                    00007500
         EJECT                                                          00007600
*********************************************************************** 00007700
*        CHECK FOR CONTROL BREAKS FOR REPORT RECAP AND BILLING FILES  * 00007800
*        FMS ONLY                                                     * 00007900
*********************************************************************** 00008000
C0010    EQU   *                  FMS APPLICATION CODE                  00008100
         CLI   LSRPTBRK,C'X'      FORCE REPORT BREAK                    00008200
         BE    C0090              YES                                   00008300
         CLI   WSRCPLVL,C'1'      CHK FOR CCR RECAP BRK                 00008400
         BNE   C0100              NO                                    00008500
         CLC   WSRPCCR,LSRPCCR    CCR BREAK                             00008600
         BE    C0100              NO                                    00008700
C0090    EQU   *                                                        00008800
         BAS   R10,Z0150          WRITE REPORT RECAP RECORD   2024183   00008900
C0100    EQU   *                                                        00009000
         CLI   LSBCRBRK,C'X'      FORCE BCR BREAK                       00009100
         BE    C0190              YES                                   00009200
         CLC   WSBCCCR,LSBCCCR    CCR BREAK                             00009300
         BE    D0010              NO                                    00009400
C0190    EQU   *                                                        00009500
         BAS   R10,Z0160    WRITE BILLING STATISTICS RECORD   2024183   00009600
         EJECT                                                          00009700
*********************************************************************** 00009800
*        PRODUCE HEADINGS                                             * 00009900
*********************************************************************** 00010000
D0010    EQU   *                                                        00010100
         MVC   WSRPTNO,LSRPTNO    MOVE REPORT NO.                       00010200
         CLI   STDHDBRK,C' '      DO-NOT-PRODUCE-HEADINGS               00010300
         BE    F0010              YES - GO TO DETAIL OUTPUT             00010400
         CP    STDLNCTR,STDFULPG  END OF PAGE                           00010500
         BL    F0010              NO - GO TO DETAIL OUTPUT              00010600
         MVC   CTLSAVE,LSSPLAR    SAVE SPOOL CONTROL AREA               00010700
         ICM   R14,12,LSEXTLN     PICK UP LINE LENGTH         0915739   00010710
         SRL   R14,16                                         0915739   00010720
         BCTR  R14,0                                          0915739   00010730
         EX    R14,SAVLINE        SAVE CURRENT DETAIL LINE    0915739   00010740
         MVC   LSLNTYPE,=CL2'HP'  SET LINE TYPE AND SUBTYPE             00010900
         CLC   LSEXTFL,=C'L'      IS LANGUAGE PRESENT IN EXT  9913481   00010910
         BE    D0015              YES, DON'T SET LENGTH       9913481   00010920
         MVC   LSEXTLN,H133       SET LINE LENGTH                       00011000
D0015    EQU   *                                              9913481   00011010
         MVI   LSPRTCTL,C'P'      SET TOP-OF-PAGE CTL CHARACTER         00011100
         MVI   LSPRTLNE,C' '      CLEAR REMAINDER OF PRINT LINE         00011200
         MVC   LSPRTLNE+1(L'LSPRTLNE-1),LSPRTLNE                        00011300
         MVC   SAVELANG,SAVEHLNG  MOVE HEADING LANG TO SAVE   2024378   00011390
         BAS   R10,Z0010          CALL OUTPUT MODULES         2024183   00011400
         MVC   SVRPTBRK,LSRPTBRK  SAVE RPT BRK FLAG           0615297   00011410
         MVC   STDHD1PG,=X'402020202120'                                00011500
         ED    STDHD1PG,STDPGCTR  PUT PAGE NO. IN HEADING               00011600
         CLI   STDHDBRK,C'S'      SUB-HEADINGS ONLY            SI0507   00011700
         BE    E0010              YES - GO TO SECONDARY HEADINGSI0507   00011800
         MVC   SAVELANG,SAVEHLNG  MOVE HEADING LANG TO SAVE   2024378   00011890
         MVC   LSPRTARA,STDHD1    MOVE HEADING 1 TO PRINT AREA          00011900
         BAS   R10,Z0010          CALL OUTPUT MODULES         2024183   00012000
         MVC   SAVELANG,SAVEHLNG  MOVE HEADING LANG TO SAVE   2024378   00012090
         MVC   LSPRTARA,STDHD2    MOVE HEADING 2 TO PRINT AREA          00012100
         BAS   R10,Z0010          CALL OUTPUT MODULES         2024183   00012200
         MVC   SAVELANG,SAVEHLNG  MOVE HEADING LANG TO SAVE   2024378   00012290
         MVC   LSPRTARA,STDHD3    MOVE HEADING 3 TO PRINT AREA          00012300
         BAS   R10,Z0010          CALL OUTPUT MODULES         2024183   00012400
         MVC   SAVELANG,SAVEHLNG  MOVE HEADING LANG TO SAVE   2024378   00012490
         MVC   LSPRTARA,STDHD4    MOVE HEADING 4 TO PRINT AREA          00012500
         BAS   R10,Z0010          CALL OUTPUT MODULES         2024183   00012600
         EJECT                                                          00012700
*********************************************************************** 00012800
*        PRODUCE SECONDARY HEADINGS                                   * 00012900
*********************************************************************** 00013000
E0010    EQU   *                                                        00013100
         ZAP   WKNOSEC,STDSCNO    SET NO. OF SECONDARY HEADINGS         00013200
         BZ    E0030              NO SECONDARIES                        00013300
         MVI   LSLNSTYP,C'S'      SET SUBTYPE VALUE                     00013400
         ZAP   WKSECSUB,STDSCSTR  SAVE STARTING SECONDARY HEADING NO.   00013500
         ZAP   DWORD,STDSCSTR                                           00013600
         CVB   R6,DWORD                                                 00013700
         BCTR  R6,0                                                     00013800
         MH    R6,H133                                                  00013900
         LA    R6,STDSCHD(R6)     POINT TO FIRST SEC. HDNG TO PRT       00014000
         CLC   LSEXTFL,=C'L'      IS LANGUAGE PRESENT IN EXT  9913481   00014010
         BE    E0020              YES, DON'T SET LENGTH       9913481   00014020
         MVC   LSEXTLN,H133       SET LINE LENGTH                       00014100
E0020    EQU   *                                                        00014200
         MVC   SAVELANG,SAVEHLNG  MOVE HEADING LANG TO SAVE   2024378   00014290
         MVC   LSPRTARA,0(R6)     MOVE SEC. HDNG TO PRINT AREA          00014300
         BAS   R10,Z0010          CALL OUTPUT MODULES         2024183   00014400
         SP    WKNOSEC,=P'1'                                            00014500
         AP    WKSECSUB,=P'1'                                           00014600
         LA    R6,133(0,R6)       BUMP TO NEXT SECONDARY HDNG           00014700
         CP    WKNOSEC,=P'0'      FINISHED NO. OF LINES                 00014800
         BNH   E0030              YES                                   00014900
         CP    WKSECSUB,=P'11'    SUBSCRIPT UNDER 11                    00015000
         BL    E0020              YES - LOOP BACK                       00015100
E0030    EQU   *                                                        00015200
         MVI   STDSCBRK,C' '      SET FORCE SECONDARY FLAG OFF          00015300
         MVC   SAVELNSQ,LSLNSEQ   SAVE LINE SEQ                         00015400
         MVC   LSSPLAR(L'CTLSAVE),CTLSAVE    RESTORE SPOOL CONTROL AREA 00015500
         MVC   LSLNSEQ,SAVELNSQ   RESTORE LINE SEQ                      00015600
         ICM   R14,12,LSEXTLN     GET LINE LENGTH             0915739   00015700
         SRL   R14,16                                         0915739   00015800
         BCTR  R14,0                                          0915739   00015900
         EX    R14,RSTLINE        RESTORE ORIGINAL PRINT LINE 0915739   00016000
         B     F0010                                                    00016100
RSTLINE  MVC   LSPRTARA(0),LINESAVE                           2500295   00016200
         EJECT                                                          00016300
*********************************************************************** 00016400
*        PRODUCE DETAIL OUTPUT                                        * 00016500
*********************************************************************** 00016600
F0010    EQU   *                                                        00016700
         CLI   STDSCBRK,C'X'      FORCE SECONDARY HEADINGS              00016800
         BNE   F0020              NO - PRINT DETAIL                     00016900
         MVC   CTLSAVE,LSSPLAR    SAVE SPOOL CONTROL AREA               00017000
         ICM   R14,12,LSEXTLN     GET LINE LENGTH             0915739   00017010
         SRL   R14,16                                         0915739   00017020
         BCTR  R14,0                                          0915739   00017030
         EX    R14,SAVLINE        SAVE CURRENT DETAIL LINE    0915739   00017040
         MVI   LSLNTYPE,C'H'      SET LINE TYPE                         00017200
         B     E0010              GO DO SECONDARY HEADINGS              00017300
F0020    EQU   *                                                        00017400
         MVC   SAVELANG,SAVEDLNG  MOVE DETAIL  LANG TO SAVE   2024378   00017490
         MVI   LSLNTYPE,C'D'      SET DETAIL LINE TYPE                  00017500
         BAS   R10,Z0010          CALL OUTPUT MODULES         2024183   00017600
         B     F0030                                          2500295   00017610
SAVLINE  MVC   LINESAVE(0),LSPRTARA                           2500295   00017620
         SPACE 3                                                        00017700
*********************************************************************** 00017800
*        RETURN TO CALLING PROGRAM                                    * 00017900
*********************************************************************** 00018000
F0030    EQU   *                                                        00018100
         SIRETRN RC=0                                                   00018200
         EJECT                                                          00018300
*********************************************************************** 00018400
*        CLOSE FILES ROUTINE                                          * 00018500
*********************************************************************** 00018600
G0010    EQU   *                                                        00018700
         LA    R8,CTLWORK         POINT TO START OF WORK AREA 9913481   00018800
         LA    R9,7               SET LOOP LIMIT                        00018900
G0020    EQU   *                                                        00019000
         CLI   WSSPLCD,X'00'      WAS AREA USED                         00019100
         BE    G0060              NO                                    00019200
         MVI   WSSPLCD,C'1'       FORCE CODE TO NOT CLOSE               00019300
         CP    WSRPTLN,=P'0'      REPORT LINE COUNT ZERO                00019400
         BNE   G0030              NO - PUT OUT RECORD                   00019500
         CP    WSRPTPG,=P'0'      REPORT PAGE COUNT ZERO                00019600
         BE    G0040              YES - CHK BILLING COUNTS              00019700
G0030    EQU   *                                                        00019800
         BAS   R10,Z0150          CALL SIREPRCP               2024183   00019900
G0040    EQU   *                                                        00020000
         CP    WSBLSLN,=P'0'      BILLING LINE COUNT ZERO               00020100
         BNE   G0050              NO - PUT OUT RECORD                   00020200
         CP    WSBLSPG,=P'0'      BILLING PAGE COUNT ZERO               00020300
         BE    G0060              YES - GET NEXT WORK AREA              00020400
G0050    EQU   *                                                        00020500
         BAS   R10,Z0160          CALL SIBLSTAT               2024183   00020600
G0060    EQU   *                                                        00020700
         LA    R8,CTLWORKL(0,R8)                              9913481   00020800
         BCT   R9,G0020                                                 00020900
         LA    R8,CTLWORK         POINT TO START OF WORK AREA 9913481   00021000
         MVI   WSSPLCD,C'9'       FORCE CLOSE CODE                      00021100
         MVI   LSPRTCTL,C'9'      FORCE CLOSE CODE                      00021200
* * * * IF HTML WRITTEN, MAKE CALL TO ADD END TAGS * * * *    0615297   00021210
         CLI   WKHTML,C'Y'        HTML FLAG ON?               0615297   00021215
         BNE   G0065              N-CONTINUE                  0615297   00021220
         BAS   R10,Z0500          CALL SIHTML                 0615297   00021225
         MVI   WKHTML,C' '        TURN OFF HTML FLAG          0615297   00021230
G0065    EQU   *                                              0615297   00021235
         TM    ROUTEFLG,X'01'     DID I EVER CALL SIPRINT               00021300
         BZ    *+8                NO - DONT CALL SIPRINT TO CLOSE       00021400
         BAS   R10,Z0110          CALL SIPRINT                2024183   00021500
         TM    ROUTEFLG,X'02'     DID I EVER CALL SIFICHE               00021600
         BZ    *+8                NO - DONT CALL SIFICHE TO CLOSE       00021700
         BAS   R10,Z0120          CALL SIFICHE                2024183   00021800
         TM    ROUTEFLG,X'04'     DID I EVER CALL SIXMIT                00021900
         BZ    *+8                NO - DONT CALL SIXMIT TO CLOSE        00022000
         BAS   R10,Z0130          CALL SIXMIT                 2024183   00022100
         TM    ROUTEFLG,X'08'     DID I EVER CALL SINQR                 00022200
         BZ    *+8                NO - DONT CALL SINQR TO CLOSE         00022300
         BAS   R10,Z0140          CALL SINQR                  2024183   00022400
         TM    BLRCPFLG,X'02'     DID I EVER CALL SIREPRCP              00022500
         BZ    *+8                NO - DONT CALL SIREPRCP TO CLOSE      00022600
         BAS   R10,Z0150          CALL SIREPRCP               2024183   00022700
         TM    BLRCPFLG,X'01'     DID I EVER CALL SIBLSTAT              00022800
         BZ    *+8                NO - DONT CALL SIBLSTAT TO CLOSE      00022900
         BAS   R10,Z0160          CALL SIBLSTAT               2024183   00023000
         B     F0030                                                    00023100
         EJECT                                                          00023200
*********************************************************************** 00023300
*        OUTPUT ROUTING ROUTINE                                       * 00023400
*                                                                     * 00023500
* OUTPUT              LSSPLCD ROUTING CODES                           * 00023600
* THE FOLLOWING STATEMENT(S) MODIFIED BY                      0615297   00023699
* FILE  1 2 3 4 5 6 7 8 A B C D E F G H I J K L M N O P Q R S T U V W * 00023700
* ----  --------------------------------------------------------------* 00023800
* PRINT X   X   X   X   X   X   X   X   X   X   X   X   X   X   X   X * 00023900
*                                                                     * 00024000
* FICHE   X X     X X     X X     X X     X X     X X     X X     X X * 00024100
*                                                                     * 00024200
* XMIT        X X X X         X X X X         X X X X         X X X X * 00024300
*                                                                     * 00024400
* NQR                 X X X X X X X X                 X X X X X X X X * 00024500
*                                                                     * 00024600
* HTML                                X X X X X X X X X X X X X X X X * 00024610
*                                                                     * 00024620
*********************************************************************** 00024700
Z0010    EQU   *                                                        00024800
         ST    R10,SAVERTN                                              00024900
         AP    LSLNSEQ,=P'1'      ADD 1 TO LINE SEQUENCE NUMBER         00026000
         AP    WSRPTLN,=P'1'      ADD 1 TO REPORT RECAP LINE COUNTER    00026100
         AP    WSBLSLN,=P'1'      ADD 1 TO BILL STAT LINE COUNTER       00026200
         TM    LSPRTCTL,X'C0'     IF PRT CTL IN TABLE                   00026300
         BO    Z0015                  GO TO TRANSLATE                   00026400
         MVI   LSPRTCTL,C'1'      FORCE PRT CTL TO 1                    00026500
Z0015    EQU   *                                                        00026600
         TR    LSPRTCTL,VALIDPRT-X'C0'  ENSURE VALID PRINT CONTROL      00026700
         MVC   WKPRTCTL,LSPRTCTL  SAVE PRINT CONTROL CHARACTER          00026800
         TR    WKPRTCTL,CTLLNNO-X'C0'   CONVERT TO NO. LINES TO ADD     00026900
         AP    STDLNCTR,WKPRTCTL  ADD NO. LINES TO LINE COUNTER         00027000
         CLI   WKPRTCTL,X'9C'     IS IT A NEW PAGE                      00027100
         BNE   Z0020              NO                                    00027200
         ZAP   STDLNCTR,=P'0'     ZERO LINE COUNTER                     00027300
         AP    STDPGCTR,=P'1'     ADD 1 TO PAGE COUNTER                 00027400
         AP    WSRPTPG,=P'1'      ADD 1 TO REPORT RECAP PAGE COUNTER    00027500
         AP    WSBLSPG,=P'1'      ADD 1 TO BILL STAT PAGE COUNTER       00027600
Z0020    EQU   *                                                        00027700
         LH    R14,H133           FORCE LINE LENGTH           0915739   00027710
         SH    R14,H2             SUBTRACT 2 FOR EXECUTE      0915739   00027715
         EX    R14,TRLINE         TRANSLATE OUT LOWVALUES     0915739   00027720
         ICM   R15,15,=V(FIXTRC)  CALL FIX TRC ROUTINE        2024207   00027725
         BASR  R14,R15                                        2024207   00027730
         MVC   PRTLNLNG,LSEXTLN   SAVE PRINT LINE LENGTH      2024207   00027735
         CLI   SAVEDBLG,C'Y'      IS THIS A DBCS LANG?        2024207   00027740
         BE    Z0022              YES, GO PROCESS DBCS LINES  2024207   00027745
         B     Z0025              NO, GO FIX FLOAT DECIMAL    2024207   00027755
Z0022    EQU   *                                              9913481   00027760
         ICM   R15,15,=V(FIXDBCS) CALL PRINT DBCS RECORD      9913481   00027765
         BASR  R14,R15                                        2024207   00027770
Z0025    EQU   *                                              9913481   00027775
         ICM   R15,15,=V(FIXDEC)  CALL FIX FLOAT DECIMAL      9913481   00027780
         BASR  R14,R15                                        2024207   00027785
         CLI   LSSPLCD,C'0'       NIBBLE TESTABLE?            0615297   00027800
         BH    Z0030              YES                         0615297   00027802
         CLI   LSSPLCD,C'H'       NIBBLE TESTABLE?            0615297   00027805
         BL    Z0030              YES                         0615297   00027810
         CLI   LSSPLCD,C'I'       PRINT AND HTML?             0615297   00027815
         BE    Z0035              YES                         0615297   00027820
         CLI   LSSPLCD,C'K'       PRINT AND HTML?             0615297   00027825
         BE    Z0035              YES                         0615297   00027830
         CLI   LSSPLCD,C'M'       PRINT AND HTML?             0615297   00027835
         BE    Z0035              YES                         0615297   00027840
         CLI   LSSPLCD,C'O'       PRINT AND HTML?             0615297   00027845
         BE    Z0035              YES                         0615297   00027850
         CLI   LSSPLCD,C'Q'       PRINT AND HTML?             0615297   00027855
         BE    Z0035              YES                         0615297   00027860
         CLI   LSSPLCD,C'S'       PRINT AND HTML?             0615297   00027865
         BE    Z0035              YES                         0615297   00027870
         CLI   LSSPLCD,C'U'       PRINT AND HTML?             0615297   00027875
         BE    Z0035              YES                         0615297   00027880
         CLI   LSSPLCD,C'W'       PRINT AND HTML?             0615297   00027885
         BE    Z0035              YES                         0615297   00027890
         B     Z0040              NO                          0615297   00027895
Z0030    EQU   *                                              0615297   00027900
         TM    LSSPLCD,X'01'      DO I PRINT                  0615297   00027905
         BZ    Z0040              NO                          0615297   00027910
Z0035    EQU   *                                              0615297   00027915
         BAS   R10,Z0110          CALL SIPRINT                2024183   00028000
Z0040    EQU   *                                              0615297   00028100
         CLI   LSPRTCTL,C'7'      DO I PRINT ONLY                       00028200
         BL    Z0045              NO                          0615297   00028300
         CLI   LSPRTCTL,C'9'      DO I PRINT ONLY                       00028400
         BL    Z0100              YES                         0615297   00028500
Z0045    EQU   *                                              0615297   00028600
         CLI   LSSPLCD,C'0'       NIBBLE TESTABLE?            0615297   00028602
         BH    Z0050              YES                         0615297   00028605
         CLI   LSSPLCD,C'H'       NIBBLE TESTABLE?            0615297   00028607
         BL    Z0050              YES                         0615297   00028610
         CLI   LSSPLCD,C'J'       FICHE AND HTML?             0615297   00028615
         BE    Z0055              YES                         0615297   00028620
         CLI   LSSPLCD,C'K'       FICHE AND HTML?             0615297   00028625
         BE    Z0055              YES                         0615297   00028630
         CLI   LSSPLCD,C'N'       FICHE AND HTML?             0615297   00028635
         BE    Z0055              YES                         0615297   00028640
         CLI   LSSPLCD,C'O'       FICHE AND HTML?             0615297   00028645
         BE    Z0055              YES                         0615297   00028650
         CLI   LSSPLCD,C'R'       FICHE AND HTML?             0615297   00028655
         BE    Z0055              YES                         0615297   00028660
         CLI   LSSPLCD,C'S'       FICHE AND HTML?             0615297   00028665
         BE    Z0055              YES                         0615297   00028670
         CLI   LSSPLCD,C'V'       FICHE AND HTML?             0615297   00028675
         BE    Z0055              YES                         0615297   00028680
         CLI   LSSPLCD,C'W'       FICHE AND HTML?             0615297   00028685
         BE    Z0055              YES                         0615297   00028690
         B     Z0060              NO                          0615297   00028695
Z0050    EQU   *                                              0615297   00028700
         TM    LSSPLCD,X'02'      DO I FICHE                            00028900
         BZ    Z0060              NO                          0615297   00028910
Z0055    EQU   *                                              0615297   00029000
         BAS   R10,Z0120          CALL SIFICHE                2024183   00029100
Z0060    EQU   *                                              0615297   00029200
         CLI   LSSPLCD,C'0'       NIBBLE TESTABLE?            0615297   00029202
         BH    Z0065              YES                         0615297   00029205
         CLI   LSSPLCD,C'H'       NIBBLE TESTABLE?            0615297   00029207
         BL    Z0065              YES                         0615297   00029210
         CLI   LSSPLCD,C'L'       TRANSMIT AND HTML?          0615297   00029215
         BE    Z0070              YES                         0615297   00029220
         CLI   LSSPLCD,C'M'       TRANSMIT AND HTML?          0615297   00029225
         BE    Z0070              YES                         0615297   00029230
         CLI   LSSPLCD,C'N'       TRANSMIT AND HTML?          0615297   00029235
         BE    Z0070              YES                         0615297   00029240
         CLI   LSSPLCD,C'O'       TRANSMIT AND HTML?          0615297   00029245
         BE    Z0070              YES                         0615297   00029250
         CLI   LSSPLCD,C'T'       TRANSMIT AND HTML?          0615297   00029255
         BE    Z0070              YES                         0615297   00029260
         CLI   LSSPLCD,C'U'       TRANSMIT AND HTML?          0615297   00029265
         BE    Z0070              YES                         0615297   00029270
         CLI   LSSPLCD,C'V'       TRANSMIT AND HTML?          0615297   00029275
         BE    Z0070              YES                         0615297   00029280
         CLI   LSSPLCD,C'W'       TRANSMIT AND HTML?          0615297   00029285
         BE    Z0070              YES                         0615297   00029290
         B     Z0080              NO                          0615297   00029295
Z0065    EQU   *                                              0615297   00029298
         TM    LSSPLCD,X'04'      DO I TRANSMIT                         00029300
         BZ    Z0080              NO                          0615297   00029305
Z0070    EQU   *                                              0615297   00029310
         BAS   R10,Z0130          CALL SIXMIT                 0615297   00029315
Z0080    EQU   *                                              0615297   00029320
         CLI   LSSPLCD,C'8'       EQUAL 8?                    1035884   00029325
         BE    Z0088              Y - GO INQ                  1035884   00029330
         CLI   LSSPLCD,C'W'       GREATER THAN W?             1035884   00029335
         BH    Z0100              Y - NO INQ OR HTML          1035884   00029340
         CLI   LSSPLCD,C'A'       LESS THAN A?                1035884   00029345
         BL    Z0090              Y - NO INQ                  1035884   00029350
         CLI   LSSPLCD,C'H'       LESS THAN H?                1035884   00029355
         BL    Z0088              Y - GO INQ                  1035884   00029360
         CLI   LSSPLCD,C'P'       LESS THAN P?                1035884   00029365
         BL    Z0090              Y - NO INQ                  1035884   00029370
Z0088    EQU   *                                              0615297   00029435
         BAS   R10,Z0140          CALL SINQR                  0615297   00029440
         EJECT                                                0615297   00029445
Z0090    EQU   *                                              0615297   00029450
         CLI   LSSPLCD,C'W'       SPOOL CODE GREATER THAN W   0615297   00029455
         BH    Z0100              YES - NO HTML               0615297   00029460
         CLI   LSSPLCD,C'H'       SPOOL CODE LESS THAN H      0615297   00029465
         BL    Z0100              NO - NO HTML                0615297   00029470
Z0095    EQU   *                                              0615297   00029475
         BAS   R10,Z0500          CALL SIHTML                 0615297   00030000
         SPACE 3                                                        00030600
*********************************************************************** 00030700
*        CLEAR INPUT PRINT LINE AND RETURN                            * 00030800
*********************************************************************** 00030900
Z0100    EQU   *                                              0615297   00031000
         LH    R14,PRTLNLNG       GET LINE LENGTH             0915739   00031100
         BCTR  R14,0              SUBTRACT 2 FOR EXECUTE      0915739   00031200
         BCTR  R14,0                                          0915739   00031300
         MVI   LSPRTCTL,C' '                                            00031400
         EX    R14,CLRLINE        CLEAR PRINT LINE            0915739   00031500
Z0105    EQU   *                                              0615297   00031600
         L     R10,SAVERTN                                              00031700
         BR    R10                                                      00031800
CLRLINE  MVC   LSPRTLNE(0),LSPRTCTL                                     00031900
TRLINE   TR    LSPRTLNE(0),TRSPACES TRANSLATE OUT LOWVALUES   2602409   00032100
         EJECT                                                          00032300
*********************************************************************** 00032400
*        CALL SIPRINT ROUTINE                                         * 00032500
*********************************************************************** 00032600
Z0110    EQU   *                                                        00032700
         NOP   Z0111              BRANCH AFTER SIPRINT LOAD             00032900
         OI    *-3,X'F0'          CHANGE NOP TO BRANCH                  00033000
         AIF   (&VSE).Z0111                                   9913437   00033050
         LOAD  EP=SIPRINT         LOAD SIPRINT                          00033100
         ST    R0,APRINT              AND SAVE ENTRY ADDR               00033200
         AGO   .Z0111M                                        9913437   00033300
.Z0111   ANOP                                                           00033400
         GNCALL SIPRINT,TYPE=LOAD,WORKREG=R1                  9913437   00033410
         ST    R1,APRINT          AND SAVE ENTRY ADDRESS      9913437   00033420
* THE FOLLOWING STATEMENT(S) MODIFIED BY                      9913437   00033429
.Z0111M  ANOP                                                           00033430
Z0111    EQU   *                                              9913437   00033440
         OI    ROUTEFLG,X'01'     TURN ON PRINT FLAG                    00033500
         ST    R4,PARM1           BUILD SIPRINT PARAMETER LIST          00033600
         ST    R3,PARM2                                                 00033700
         OI    PARM2,X'80'                                              00033800
         LA    R1,PARM1           POINT TO PARAMETER LIST               00033900
         L     R15,APRINT         GET ADDR OF SIPRINT                   00034000
         BASR  R14,R15            LINK TO IT                  2024207   00034100
         BR    R10                                                      00034200
         SPACE 3                                                        00034300
*********************************************************************** 00034400
*        CALL SIFICHE ROUTINE                                         * 00034500
*********************************************************************** 00034600
Z0120    EQU   *                  CALL SIFICHE                          00034700
         NOP   Z0121              BRANCH AFTER SIFICHE LOAD             00034900
         OI    *-3,X'F0'          CHANGE NOP TO BRANCH                  00035000
         AIF   (&VSE).Z0121                                   9913437   00035050
         LOAD  EP=SIFICHE         LOAD SIFICHE                          00035100
         ST    R0,AFICHE              AND SAVE ENTRY ADDR               00035200
         AGO   .Z0121M                                        9913437   00035300
.Z0121   ANOP                                                           00035400
         GNCALL SIFICHE,TYPE=LOAD,WORKREG=R1 LOAD SIFICHE     9913437   00035410
         ST    R1,AFICHE          AND SAVE ENTRY ADDR         9913437   00035420
* THE FOLLOWING STATEMENT(S) MODIFIED BY                      9913437   00035429
.Z0121M  ANOP                                                           00035430
Z0121    EQU   *                                              9913437   00035440
         OI    ROUTEFLG,X'02'     TURN ON FICHE FLAG                    00035500
         ST    R3,PARM1           BUILD SIFICHE PARAMETER LIST          00035600
         ST    R4,PARM2                                                 00035700
         OI    PARM2,X'80'                                              00035800
         LA    R1,PARM1           POINT TO PARAMETER LIST               00035900
         L     R15,AFICHE         GET ADDR OF SIFICHE                   00036000
         BASR  R14,R15            LINK TO IT                  2024207   00036100
         BR    R10                                                      00036200
         SPACE 3                                                        00036300
*********************************************************************** 00036400
*        CALL SIXMIT ROUTINE                                          * 00036500
*********************************************************************** 00036600
Z0130    EQU   *                  CALL SIXMIT                           00036700
         NOP   Z0131              BRANCH AFTER SIXMIT LOAD              00036900
         OI    *-3,X'F0'          CHANGE NOP TO BRANCH                  00037000
         AIF   (&VSE).Z0131                                   9913437   00037050
         LOAD  EP=SIXMIT          LOAD SIXMIT                           00037100
         ST    R0,AXMIT               AND SAVE ENTRY ADDR               00037200
         AGO   .Z0131M                                        9913437   00037300
.Z0131   ANOP                                                           00037400
         GNCALL SIXMIT,TYPE=LOAD,WORKREG=R1  LOAD SIXMIT      9913437   00037410
         ST    R1,AXMIT           AND SAVE ENTRY ADDR         9913437   00037420
* THE FOLLOWING STATEMENT(S) MODIFIED BY                      9913437   00037429
.Z0131M  ANOP                                                           00037430
Z0131    EQU   *                                              9913437   00037440
         OI    ROUTEFLG,X'04'     TURN ON XMIT FLAG                     00037500
         ST    R3,PARM1           BUILD SIXMIT PARAMETER LIST           00037600
         ST    R4,PARM2                                                 00037700
         OI    PARM2,X'80'                                              00037800
         LA    R1,PARM1           POINT TO PARAMETER LIST               00037900
         L     R15,AXMIT          GET ADDR OF SIXMIT                    00038000
         BASR  R14,R15            LINK TO IT                  2024207   00038100
         BR    R10                                                      00038200
         SPACE 3                                                        00038300
*********************************************************************** 00038400
*        CALL SINQR ROUTINE                                           * 00038500
*********************************************************************** 00038600
Z0140    EQU   *                  CALL SINQR                            00038700
         NOP   Z0141              BRANCH AFTER SINQR LOAD               00038900
         OI    *-3,X'F0'          CHANGE NOP TO BRANCH                  00039000
         AIF   (&VSE).Z0141                                   9913437   00039050
         LOAD  EP=SINQR           LOAD SINQR                            00039100
         ST    R0,ANQR                AND SAVE ENTRY ADDR               00039200
         AGO   .Z0141M                                        9913437   00039300
.Z0141   ANOP                                                           00039400
         GNCALL SINQR,TYPE=LOAD,WORKREG=R1   LOAD SINQR       9913437   00039410
         ST    R1,ANQR            AND SAVE ENTRY ADDR         9913437   00039420
* THE FOLLOWING STATEMENT(S) MODIFIED BY                      9913437   00039429
.Z0141M  ANOP                                                           00039430
Z0141    EQU   *                                              9913437   00039440
         OI    ROUTEFLG,X'08'     TURN ON NQR FLAG                      00039500
         ST    R2,PARM1           BUILD SINQR PARAMETER LIST            00039600
         ST    R3,PARM2                                                 00039700
         ST    R4,PARM3                                                 00039800
         OI    PARM3,X'80'                                              00039900
         LA    R1,PARM1           POINT TO PARAMETER LIST               00040000
         L     R15,ANQR           GET ADDR OF SINQR                     00040100
         BASR  R14,R15            LINK TO IT                  2024207   00040200
         BR    R10                                                      00040300
         EJECT                                                          00040400
*********************************************************************** 00040500
*        CALL SIREPRCP ROUTINE                                        * 00040600
*********************************************************************** 00040700
Z0150    EQU   *                  CALL SIREPRCP                         00040800
         CLI   WSSPLCD,C'9'       ARE WE CLOSING FILES                  00040900
         BE    Z0151E             YES                         9912009   00041000
         CLI   WSBLSTSW,C'2'      DO I WANT REPORT RECAP                00041100
         BL    Z0155              NO                                    00041200
         CLI   WSBLSTSW,C'3'      DO I WANT REPORT RECAP                00041300
         BH    Z0155              NO                                    00041400
Z0151E   EQU   *                                              9912009   00041410
         NOP   Z0151              BRANCH AFTER SIREPRCP LOAD            00041600
         OI    *-3,X'F0'          CHANGE NOP TO BRANCH                  00041700
         AIF   (&VSE).Z0151                                   9913437   00041750
         LOAD  EP=SIREPRCP        LOAD SIREPRCP                         00041800
         ST    R0,AREPRCP             AND SAVE ENTRY ADDR               00041900
         AGO   .Z0151M                                        9913437   00041950
.Z0151   ANOP                                                           00042000
         GNCALL SIREPRCP,TYPE=LOAD,WORKREG=R1 LOAD SIREPRCP   9913437   00042010
         ST    R1,AREPRCP          AND SAVE ENTRY ADDR        9913437   00042020
* THE FOLLOWING STATEMENT(S) MODIFIED BY                      9913437   00042029
.Z0151M  ANOP                                                           00042030
Z0151    EQU   *                                                        00042100
         OI    BLRCPFLG,X'02'     SET SIREPRCP CALLED FLAG              00042200
         ST    R8,PARM1           BUILD SIREPRCP PARM LIST    9913481   00042300
         LA    R1,WSRPTLN                                               00042400
         ST    R1,PARM2                                                 00042500
         LA    R1,WSRPTPG                                               00042600
         ST    R1,PARM3                                                 00042700
         OI    PARM3,X'80'                                              00042800
         LA    R1,PARM1           POINT TO PARAMETER LIST               00042900
         L     R15,AREPRCP        GET ADDR OF SIREPRCP                  00043000
         BASR  R14,R15            LINK TO IT                  2024207   00043100
Z0155    EQU   *                                                        00043200
         CLI   WSSPLCD,C'9'       ARE WE CLOSING FILES                  00043300
         BER   R10                YES - RETURN                          00043400
         MVI   LSRPTBRK,C' '      CLEAR REPORT BREAK SWITCH             00043500
         ZAP   WSRPTLN,=P'0'      CLEAR REPORT LINE COUNTER             00043600
         ZAP   WSRPTPG,=P'0'      CLEAR REPORT PAGE COUNTER             00043700
         MVC   WSRPTCTL,LSRPTCTL  SAVE REPORT CONTROL AREA              00043800
         BR    R10                                                      00043900
         SPACE 3                                                        00044000
*********************************************************************** 00044100
*        CALL SIBLSTAT ROUTINE                                        * 00044200
*********************************************************************** 00044300
Z0160    EQU   *                  CALL SIBLSTAT                         00044400
         CLC   LSAPPL,=CL2'FM'    IS APPLICATION CODE FM                00044500
         BE    Z0160A             YES                                   00044600
         CLC   LSAPPL,=CL2'AP'    IS APPLICATION CODE AP                00044700
         BNE   Z0161              NO                                    00044800
Z0160A   EQU   *                                                        00044900
         MVI   WSBCRC,C'0'        ZERO RCCR AREA                        00045000
         MVC   WSBCRC+1(17),WSBCRC                                      00045100
Z0161    EQU   *                                                        00045200
         CLI   WSSPLCD,C'9'       ARE WE CLOSING FILES                  00045300
         BE    Z0161E             YES                         9912009   00045400
         CLI   WSBLSTSW,C'1'      DO I WANT BILLING STATISTICS          00045500
         BL    Z0165              NO                                    00045600
         CLI   WSBLSTSW,C'2'      DO I WANT BILLING STATISTICS          00045700
         BH    Z0165              NO                                    00045800
Z0161E   EQU   *                                              9912009   00045810
         NOP   Z0162              BRANCH AFTER SIBLSTAT LOAD            00046000
         OI    *-3,X'F0'          CHANGE NOP TO BRANCH                  00046100
         AIF   (&VSE).Z0162                                   9913437   00046150
         LOAD  EP=SIBLSTAT        LOAD SIBLSTAT                         00046200
         ST    R0,ABLSTAT             AND SAVE ENTRY ADDR               00046300
         AGO   .Z0162M                                        9913437   00046350
.Z0162   ANOP                                                           00046400
         GNCALL SIBLSTAT,TYPE=LOAD,WORKREG=R1 LOAD SIBLSTAT   9913437   00046410
         ST    R1,ABLSTAT          AND SAVE ENTRY ADDR        9913437   00046420
* THE FOLLOWING STATEMENT(S) MODIFIED BY                      9913437   00046429
.Z0162M  ANOP                                                           00046430
Z0162    EQU   *                                                        00046500
         OI    BLRCPFLG,X'01'     SET SIBLSTAT CALLED FLAG              00046600
         ST    R8,PARM1           BUILD SIBLSTAT PARM LIST    9913481   00046700
         LA    R1,WSBLSLN                                               00046800
         ST    R1,PARM2                                                 00046900
         LA    R1,WSBLSPG                                               00047000
         ST    R1,PARM3                                                 00047100
         OI    PARM3,X'80'                                              00047200
         LA    R1,PARM1           POINT TO PARAMETER LIST               00047300
         L     R15,ABLSTAT        GET ADDR OF SIBLSTAT                  00047400
         BASR  R14,R15            LINK TO IT                  2024207   00047500
Z0165    EQU   *                                                        00047600
         CLI   WSSPLCD,C'9'       ARE WE CLOSING FILES                  00047700
         BER   R10                YES - RETURN                          00047800
         MVI   LSBCRBRK,C' '      CLEAR BCR BREAK SWITCH                00047900
         ZAP   WSBLSLN,=P'0'      CLEAR BILLING LINE COUNTER            00048000
         ZAP   WSBLSPG,=P'0'      CLEAR BILLING PAGE COUNTER            00048100
         MVC   WSSPLAR,LSSPLAR    SAVE SPOOL CONTROL AREA               00048200
         BR    R10                                                      00048300
         EJECT                                                          00048400
************************************************************* 0615297   00048402
*        CALL SIHTML ROUTINE                                * 0615297   00048404
************************************************************* 0615297   00048406
Z0500    EQU   *                  CALL SIHTML                 0615297   00048408
         NOP   Z0501              BRANCH AFTER SIHTML LOAD    0615297   00048410
         OI    *-3,X'F0'          CHANGE NOP TO BRANCH        0615297   00048412
         AIF   (&VSE).Z0501                                   0615297   00048414
         LOAD  EP=SIHTML          LOAD SIHTML                 0615297   00048416
         ST    R0,AHTML               AND SAVE ENTRY ADDR     0615297   00048418
         AGO   .Z0501M                                        0615297   00048420
.Z0501   ANOP                                                 0615297   00048422
         GNCALL SIHTML,TYPE=LOAD,WORKREG=R1 LOAD SIHTML       0615297   00048424
         ST    R1,AHTML           AND SAVE ENTRY ADDR         0615297   00048426
.Z0501M  ANOP                                                 0615297   00048430
Z0501    EQU   *                                              0615297   00048432
         MVI   WKHTML,C'Y'        TURN ON HTML FLAG           0615297   00048440
         MVC   SVRPTBK2,LSRPTBRK  SAVE RPT BRK FLG            0615297   00048445
         MVC   LSRPTBRK,SVRPTBRK  SET INITIAL RPT BRK FLG     0615297   00048450
         ST    R3,PARM1           BUILD SIHTML PARAMETER LIST 0615297   00048455
         ST    R4,PARM2                                       0615297   00048460
         ST    R2,PARM3                                       0615297   00048465
         MVC   PARM4,SVHTMLEX                                 0615297   00048467
         MVC   PARM5,SVSPLLEX                                 1035884   00048470
         OI    PARM5,X'80'                                    1035884   00048472
         LA    R1,PARM1           POINT TO PARAMETER LIST     0615297   00048475
         L     R15,AHTML          GET ADDR OF SIHTML          0615297   00048480
         BASR  R14,R15            LINK TO IT                  0615297   00048485
         MVC   LSRPTBRK,SVRPTBK2  RESTORE RPT BREAK FLAG      0615297   00048490
         BR    R10                                            0615297   00048495
         SPACE 3                                              0615297   00048497
*********************************************************************** 00048500
*        WORK AREAS AND TABLES                                        * 00048600
*********************************************************************** 00048700
DWORD    DC    D'0'                                                     00048800
SAVERTN  DC    F'0'                                                     00048900
PARM1    DC    F'0'                                                     00049000
PARM2    DC    F'0'                                                     00049100
PARM3    DC    F'0'                                                     00049200
PARM4    DC    F'0'                                           0615297   00049210
PARM5    DC    F'0'                                           1035884   00049212
APRINT   DC    F'0'                                                     00049400
AFICHE   DC    F'0'                                                     00049500
AHTML    DC    F'0'                                           0615297   00049510
AXMIT    DC    F'0'                                                     00049600
ANQR     DC    F'0'                                                     00049700
AREPRCP  DC    F'0'                                                     00049800
ABLSTAT  DC    F'0'                                                     00049900
PRTLNLNG DC    H'0'                                                     00050900
H2       DC    H'2'                                                     00051000
H96      DC    H'96'                                                    00051100
H133     DC    H'133'                                                   00051200
H134     DC    H'134'                                         9913481   00051210
SVL1REGS DS    15F                     LV 1 CSECT REGS        9913481   00051215
SVL2REGS DS    15F                     LV 2 CSECT REGS        9913481   00051220
DBCSLNG  DS    F                                              9913481   00051225
SVHTMLEX DS    F                                              0615297   00051227
SVSTRFLD DS    F                       SAVE FIELD START ADDR  9913481   00051230
SVSPLLEX DS    F                                              1035884   00051232
SVLENFLD DS    F                       SAVE FIELD LENGTH      9913481   00051235
SAVER1   DS    F                       SAVE PARM ADDRESS      9913483   00051237
LANGADDR DC    A(LANGTBL)      ADDRESS OF LANGUAGE TABLE      9913481   00051240
SAVEPRTD DS    CL1             SAVE PRT $ INDICATOR           2024346   00051242
SAVEDBLG DC    CL1'N'                                         9913481   00051245
SAVEHLNG DC    CL2'  '                                        2024378   00051246
SAVEDLNG DC    CL2'  '                                        2024378   00051248
SAVELANG DC    CL2'  '                                        9913481   00051250
SAVEDEC  DS    CL1                                            2024207   00051251
SAVECURR DS    CL3                                            2024207   00051252
DFLTDEC  DC    CL1'2'                                         2024207   00051253
DFLTCURR DC    CL3'   '                                       2024207   00051254
WKFDSWC  DS    PL1                REQ'D DEC/DIV MOVE COUNTER  9913481   00051255
SVFDSWC  DS    PL1                REQ'D DEC/DIV MV CTR SAVER  9913481   00051260
WMLTLANG DS    CL1                SASIOPTE MLTLANG FIELD      9913483   00051262
WFLOATDC DS    CL1                SASIOPTE FLOATDC FIELD      9913483   00051264
WDIVIDER DS    CL1                SASIOPTE DIVIDER FIELD      9913483   00051266
WKNOSEC  DC    PL2'0'                                                   00051300
WKSECSUB DC    PL2'0'                                                   00051400
WKPRTCTL DC    CL1' '                                                   00051500
WKPRTID  DC    CL1' '                                                   00051600
WKHTML   DS    CL1' '                                         0615297   00051605
WKHYPER  DS    CL1' '                                         0615297   00051610
SVRPTBRK DS    CL1' '                                         0615297   00051615
SVRPTBK2 DS    CL1' '                                         0615297   00051620
ROUTEFLG DC    B'0000&NQR&XMIT&FICHE&PRINT' SET FLAGS FOR NULL FILES    00051700
BLRCPFLG DC    B'000000&REPRCP&BLSTAT' SET FLAGS FOR NULL FILES         00051800
CTLSAVE  DC    CL77' '                                                  00051900
LINESAVE DC    CL205' '                                                 00052000
SAVELNSQ DC    PL4'0'                                                   00052100
SIPBCDAD DC    F'0'               BATCH I/O RTN ADDR          0444987   00052105
SIPBCDIO DC    CL8'SIPBCDIO'      BATCH SASIOPTE I/O RTN      9913483   00052110
PARMLIST DS    2F                 BATCH SIPBCDIO LIST         9913483   00052115
OPTEREC  DS    CL300              SASIOPTE RECORD             9913483   00052120
         DS    0D                                                       00052200
VALIDPRT DC    CL16'1ABCDEFGHI111111'                                   00052300
         DC    CL16'1JKLMNOPQR111111'                                   00052400
         DC    CL16'11STUVWXYZ111111'                                   00052500
         DC    CL16'0123111789111119'                                   00052600
         DS    0D                                                       00052700
CTLLNNO  DC    XL16'1C9C1C1C1C1C1C1C1C1C1C1C1C1C1C1C'                   00052800
         DC    XL16'1C1C1C1C1C2C3C9C1C1C1C1C1C1C1C1C'                   00052900
         DC    XL16'1C1C1C1C1C1C1C1C1C1C1C1C1C1C1C1C'                   00053000
         DC    XL16'0C1C2C3C1C1C1C0C0C0C1C1C1C1C1C1C'                   00053100
         DS    0D                                                       00053300
***** USED IN CONVERTING LOW-VALUES TO SPACES    ****         2602409   00053302
TRSPACES DC    X'400102030405060708090A0B0C0D0E0F'     00-0F  2602409   00053304
         DC    X'101112131415161718191A1B1C1D1E1F'     10-1F  2602409   00053306
         DC    X'202122232425262728292A2B2C2D2E2F'     20-2F  2602409   00053308
         DC    X'303132333435363738393A3B3C3D3E3F'     30-3F  2602409   00053310
         DC    X'404142434445464748494A4B4C4D4E4F'     40-4F  2602409   00053312
         DC    X'505152535455565758595A5B5C5D5E5F'     50-5F  2602409   00053314
         DC    X'606162636465666768696A6B6C6D6E6F'     60-6F  2602409   00053316
         DC    X'707172737475767778797A7B7C7D7E7F'     70-7F  2602409   00053318
         DC    X'808182838485868788898A8B8C8D8E8F'     80-8F  2602409   00053320
         DC    X'909192939495969798999A9B9C9D9E9F'     90-9F  2602409   00053322
         DC    X'A0A1A2A3A4A5A6A7A8A9AAABACADAEAF'     A0-AF  2602409   00053324
         DC    X'B0B1B2B3B4B5B6B7B8B9BABBBCBDBEBF'     B0-BF  2602409   00053326
         DC    X'C0C1C2C3C4C5C6C7C8C9CACBCCCDCECF'     C0-CF  2602409   00053328
         DC    X'D0D1D2D3D4D5D6D7D8D9DADBDCDDDEDF'     D0-DF  2602409   00053330
         DC    X'E0E1E2E3E4E5E6E7E8E9EAEBECEDEEEF'     E0-EF  2602409   00053332
         DC    X'F0F1F2F3F4F5F6F7F8F9FAFBFCFDFEFF'     F0-FF  2602409   00053334
         SPACE                                                          00053600
         LTORG                                                          00053700
         DS    0D                                                       00053800
CTLINDEX DC    12XL16'00'                                               00053900
         AIF   (NOT &VSE).MVS1                                          00054000
         DC    XL16'00000102030405060708000000000000'                   00054100
*                     A B C D E F G H I                                 00054200
         DC    XL16'00090A0B0C0000000000000000000000'                   00054300
*                     J K L M                                           00054400
         DC    2XL16'00'                                                00054500
         AGO   .INDXEND                                                 00054600
.MVS1    ANOP                                                           00054700
         DC    XL16'00000102030405060708000000000000'                   00054800
*                     A B C D E F G H I                                 00054900
         DC    XL16'00090A0B0C0D0E0F1011000000000000'                   00055000
*                     J K L M N O P Q R                                 00055100
         DC    XL16'00001213141516171819000000000000'                   00055200
*                       S T U V W X Y Z                                 00055300
         DC    XL16'00'                                                 00055400
.INDXEND ANOP                                                           00055500
         DS    0D                                          LSPRTID      00055600
CTLWORK  DC    XL77'00',PL3'0',PL4'0',PL3'0',PL4'0',XL5'00'   A         00055700
         DC    XL77'00',PL3'0',PL4'0',PL3'0',PL4'0',XL5'00'   B         00055800
         DC    XL77'00',PL3'0',PL4'0',PL3'0',PL4'0',XL5'00'   C         00055900
         DC    XL77'00',PL3'0',PL4'0',PL3'0',PL4'0',XL5'00'   D         00056000
         DC    XL77'00',PL3'0',PL4'0',PL3'0',PL4'0',XL5'00'   E         00056100
         DC    XL77'00',PL3'0',PL4'0',PL3'0',PL4'0',XL5'00'   F         00056200
         DC    XL77'00',PL3'0',PL4'0',PL3'0',PL4'0',XL5'00'   G         00056300
         DC    XL77'00',PL3'0',PL4'0',PL3'0',PL4'0',XL5'00'   H         00056400
         DC    XL77'00',PL3'0',PL4'0',PL3'0',PL4'0',XL5'00'   I         00056500
         DC    XL77'00',PL3'0',PL4'0',PL3'0',PL4'0',XL5'00'   J         00056600
         DC    XL77'00',PL3'0',PL4'0',PL3'0',PL4'0',XL5'00'   K         00056700
         DC    XL77'00',PL3'0',PL4'0',PL3'0',PL4'0',XL5'00'   L         00056800
         DC    XL77'00',PL3'0',PL4'0',PL3'0',PL4'0',XL5'00'   M         00056900
         AIF   (&VSE).VSEESA5                                           00057000
         DC    XL77'00',PL3'0',PL4'0',PL3'0',PL4'0',XL5'00'   N         00057100
         DC    XL77'00',PL3'0',PL4'0',PL3'0',PL4'0',XL5'00'   O         00057200
         DC    XL77'00',PL3'0',PL4'0',PL3'0',PL4'0',XL5'00'   P         00057300
         DC    XL77'00',PL3'0',PL4'0',PL3'0',PL4'0',XL5'00'   Q         00057400
         DC    XL77'00',PL3'0',PL4'0',PL3'0',PL4'0',XL5'00'   R         00057500
         DC    XL77'00',PL3'0',PL4'0',PL3'0',PL4'0',XL5'00'   S         00057600
         DC    XL77'00',PL3'0',PL4'0',PL3'0',PL4'0',XL5'00'   T         00057700
         DC    XL77'00',PL3'0',PL4'0',PL3'0',PL4'0',XL5'00'   U         00057800
         DC    XL77'00',PL3'0',PL4'0',PL3'0',PL4'0',XL5'00'   V         00057900
         DC    XL77'00',PL3'0',PL4'0',PL3'0',PL4'0',XL5'00'   W         00058000
         DC    XL77'00',PL3'0',PL4'0',PL3'0',PL4'0',XL5'00'   X         00058100
         DC    XL77'00',PL3'0',PL4'0',PL3'0',PL4'0',XL5'00'   Y         00058200
         DC    XL77'00',PL3'0',PL4'0',PL3'0',PL4'0',XL5'00'   Z         00058300
.VSEESA5 ANOP                                                           00058400
CTLWORKL EQU   96                                                       00058500
         SPACE 2                                              9913481   00058510
         DS    0D                                             9913481   00058515
         COPY  SIDTLTRC                                       9913481   00058520
         EJECT                                                          00058600
*********************************************************************** 00058700
*        DSECTS                                                       * 00058800
*********************************************************************** 00058900
         SPACE                                                          00059000
LSPRT    DSECT                                                          00059100
LSPRTARA DS    0CL133                                                   00059200
LSPRTCTL DS    CL1                                                      00059300
LSPRTLNE DS    CL132                                                    00059400
LSPRTEXT DS    CL72                                                     00059500
         DS    CL1                                            9913481   00059510
         SPACE 3                                                        00059600
LSSPLAR  DSECT                                                          00059700
LSSPLCD  DS    CL1           SPOOL ROUTING CODE                         00059800
LSFORM   DS    CL1           SPOOL FORM CODE                            00059900
LSPROG   DS    0CL8          PROGRAM ID                                 00060000
LSAPPL   DS    CL2           APPLICATION CODE                           00060100
LSMAIN   DS    CL2           MAINLINE PROGRAM NUMBER                    00060200
LSUSER   DS    CL4           REPORT PROGRAM NAME                        00060300
LSRPTNO  DS    CL3           REPORT NUMBER                              00060400
LSBCRBRK DS    CL1           BCR BREAK SWITCH                           00060500
LSBCRCTL DS    0CL14         BCR CONTROL AREA                           00060600
LSBCCTL1 DS    CL2           BCR BANK NO                                00060700
LSBCCTL2 DS    CL3           BCR CONTROL2                               00060800
LSBCCTL3 DS    CL3           BCR CONTROL3                               00060900
LSBCCTL4 DS    CL4           BCR CONTROL4                               00061000
         DS    CL10                                                     00061100
         ORG   LSBCRCTL                                                 00061200
LSBCCCR  DS    CL4           FMS CCR NO                                 00061300
LSBCRC   DS    9CL2          FMS RC NO                                  00061400
         ORG                                                            00061500
LSRPTBRK DS    CL1           REPORT BREAK SWITCH                        00061600
LSRPTCTL DS    0CL14         REPORT CONTROL AREA                        00061700
LSRPCTL1 DS    CL2           REPORT BANK NO                             00061800
LSRPCTL2 DS    CL3           REPORT CONTROL2                            00061900
LSRPCTL3 DS    CL3           REPORT CONTROL3                            00062000
LSRPCTL4 DS    CL4           REPORT CONTROL4                            00062100
         DS    CL10                                                     00062200
         ORG   LSRPTCTL                                                 00062300
LSRPCCR  DS    CL4           REPORT CCR NO                              00062400
LSRPRC   DS    9CL2          REPORT RC NO                               00062500
         ORG                                                            00062600
LSBLSTSW DS    CL1           BILLING STATUS CODE                        00062700
LSLNSEQ  DS    PL4           LINE SEQUENCE NUMBER                       00062800
LSLNTYPE DS    CL1           LINE TYPE CODE                             00062900
LSLNSTYP DS    CL1           LINE SUBTYPE CODE                          00063000
LSRCPLVL DS    CL1           RECAP LEVEL                                00063100
LSPRTID  DS    CL1           PRINT FILE ID                              00063200
LSEXTFL  DS    CL1           EXTENDED PRINT PROCESSING FLAG             00063300
LSEXTLN  DS    BL2           EXTENDED PROCESSING LINE LENGTH            00063400
LSPRTD   DS    CL1           PRINT DOLLAR INDICATOR           2024346   00063500
         DS    CL5                                                      00063600
         EJECT                                                          00063700
WSSPLWK  DSECT                                                          00063800
WSSPLAR  DS    0CL77                                                    00063900
WSSPLCD  DS    CL1           SPOOL ROUTING CODE                         00064000
WSFORM   DS    CL1           SPOOL FORM CODE                            00064100
WSPROG   DS    0CL8          PROGRAM ID                                 00064200
WSAPPL   DS    CL2           APPLICATION CODE                           00064300
WSMAIN   DS    CL2           MAINLINE PROGRAM NUMBER                    00064400
WSUSER   DS    CL4           REPORT PROGRAM NAME                        00064500
WSRPTNO  DS    CL3           REPORT NUMBER                              00064600
WSBCRBRK DS    CL1           BCR BREAK SWITCH                           00064700
WSBCRCTL DS    0CL14         BCR CONTROL AREA                           00064800
WSBCCTL1 DS    CL2           BCR BANK NO                                00064900
WSBCCTL2 DS    CL3           BCR CONTROL2                               00065000
WSBCCTL3 DS    CL3           BCR CONTROL3                               00065100
WSBCCTL4 DS    CL4           BCR CONTROL4                               00065200
         DS    CL10                                                     00065300
         ORG   WSBCRCTL                                                 00065400
WSBCCCR  DS    CL4           FMS CCR NO                                 00065500
WSBCRC   DS    9CL2          FMS RC NO                                  00065600
         ORG                                                            00065700
WSRPTBRK DS    CL1           REPORT BREAK SWITCH                        00065800
WSRPTCTL DS    0CL14         REPORT CONTROL AREA                        00065900
WSRPCTL1 DS    CL2           REPORT BANK NO                             00066000
WSRPCTL2 DS    CL3           REPORT CONTROL2                            00066100
WSRPCTL3 DS    CL3           REPORT CONTROL3                            00066200
WSRPCTL4 DS    CL4           REPORT CONTROL4                            00066300
         DS    CL10                                                     00066400
         ORG   WSRPTCTL                                                 00066500
WSRPCCR  DS    CL4           REPORT CCR NO                              00066600
WSRPRC   DS    9CL2          REPORT RC NO                               00066700
         ORG                                                            00066800
WSBLSTSW DS    CL1           BILLING STATUS CODE                        00066900
WSLNSEQ  DS    PL4           LINE SEQUENCE NUMBER                       00067000
WSLNTYPE DS    CL1           LINE TYPE CODE                             00067100
WSLNSTYP DS    CL1           LINE SUBTYPE CODE                          00067200
WSRCPLVL DS    CL1           RECAP LEVEL                                00067300
WSPRTID  DS    CL1           PRINT FILE ID                              00067400
WSEXTFL  DS    CL1           EXTENDED PRINT PROCESSING FLAG             00067500
WSEXTLN  DS    BL2           EXTENDED PROCESSING LINE LENGTH            00067600
WSPRTD   DS    CL1           PRINT DOLLAR INDICATOR           2024346   00067610
         DS    CL5                                            2024346   00067700
WSBLSPG  DS    PL3           BILLING STATISTICS PAGE COUNTER            00067800
WSBLSLN  DS    PL4           BILLING STATISTICS LINE COUNTER            00067900
WSRPTPG  DS    PL3           REPORT RECAP PAGE COUNTER                  00068000
WSRPTLN  DS    PL4           REPORT RECAP LINE COUNTER                  00068100
         DS    CL5                                                      00068200
         EJECT                                                          00068300
LGTBAREA DSECT                                                9913481   00068310
LGTBLANG DS    CL2                                            9913481   00068315
LGTBDBLG DS    CL1                                            9913481   00068320
LGTBTRC  DS    XL1                                            9913481   00068325
LGTBDESC DS    CL16                                           9913481   00068330
LANGLENG EQU   *-LGTBAREA                                     9913481   00068335
         COPY  SIDSSPEX                                       9913481   00068340
         COPY  SASIOPEA                                       9913483   00068345
STHDAREA DSECT               STANDARD HEADINGS AREA                     00068400
STDHD1   DS    0CL133                                                   00068500
         DS    CL127                                                    00068600
STDHD1PG DS    CL6                                                      00068700
STDHD2   DS    CL133                                                    00068800
STDHD3   DS    CL133                                                    00068900
STDHD4   DS    CL133                                                    00069000
STDFULPG DS    PL2           NO. LINES PER PAGE                         00069100
STDPGCTR DS    PL3           PAGE COUNTER                               00069200
STDLNCTR DS    PL4           LINE COUNTER                               00069300
STDHDBRK DS    CL1           FORCE HEADINGS FLAG                        00069400
STDSCBRK DS    CL1           FORCE SECONDARY HEADINGS FLAG              00069500
STDSCSTR DS    PL2           SECONDARY HEADINGS START LINE              00069600
STDSCNO  DS    PL2           NUMBER OF SECONDARY HEADING LINES          00069700
         DS    CL10                                                     00069800
STDSCHD  DS    10CL133                                                  00069900
************************************************************* 9913481 * 00070000
*        CALL FIX TRC ROUTINE FOR LANGUAGE                    9913481 * 00070100
************************************************************* 9913481 * 00070200
FIXTRC   CSECT                                                9913481   00070300
         STM   R14,R12,SVL1REGS    SAVE CALLERS REGISTERS     9913481   00070500
         USING *-4,R7             ADDRESS CSECT               9913481   00070600
         LR    R7,R15             LOAD CSECT BASE ADDRESS     9913481   00070700
         CLC   WMLTLANG,=C'Y'     MULTI-LANGUAGE?             9913483   00070710
         BNE   Z0180              NO GO TO Z0180              9913483   00070720
         CLC   LSEXTFL,=C'T'      IS THIS AN RECORD WITH TRC? 9913481   00070800
         BE    Z0180              YES, RETURN                 9913481   00070900
         CLC   LSEXTFL,=C'X'      IS THIS AN EXT RECORD?      9913481   00071000
         BE    Z0180              YES, RETURN                 9913481   00071100
         CLC   LSEXTFL,=C'L'      IS THIS AN RECORD WITH TRC? 9913481   00071110
         BNE   Z0180              YES, RETURN                 9913481   00071120
         L     R9,LANGADDR        GET ADDR OF LANG/TRC TABLE  9913481   00072100
         USING LGTBAREA,R9                                    9913481   00072200
Z0172    EQU   *                                              9913481   00072300
         MVC   LSEXTLN,H134       FORCE LNG 134 FOR SIPRINT   9913481   00072400
         MVC   EXPRTCTL(L'EXPRTCTL),0(R4)  MOVE PRINT CONTROL 9913481   00072500
         MVC   EXPRTLEN(L'EXPRTLNE),1(R4)  MOVE PRINTLINE     9913481   00072600
Z0173    EQU   *                                              9913481   00072700
         CLC   SAVELANG,=C'  '    IS THIS THE LANG SPACES     9913481   00072800
         BE    Z0176              YES                         9913481   00072900
         CLC   SAVELANG,LGTBLANG  IS THIS THE CORRECT LANG    9913481   00073000
         BE    Z0174              YES                         9913481   00073100
         LA    R9,LANGLENG(R9)    BUMP TABLE LENGTH           9913481   00073200
         CLI   0(R9),X'FF'        END OF TABLE                9913481   00073300
         BNE   Z0173              NO                          9913481   00073400
         B     Z0176              YES - MOVE DEFAULT TRC      9913481   00073500
Z0174    EQU   *                                              9913481   00073600
         MVC   EXPRTTRC,LGTBTRC   MOVE TRC TO OUTPUT LINE     9913481   00073700
         MVC   SAVEDBLG,LGTBDBLG  MOVE DBSC FLAG TO SAVE      9913481   00073800
         B     Z0178                                          9913481   00073900
Z0176    EQU   *                                              9913481   00074000
         MVC   EXPRTTRC,=X'00'    MOVE DEFAULT TRC CODE       9913481   00074100
Z0178    EQU   *                                              9913481   00074200
         MVC   LSPRTARA(134),EXPRTARA                         9913481   00074300
Z0180    EQU   *                                              9913481   00074400
         LM    R14,R12,SVL1REGS                               9913481   00074500
         BR    R14                                            9913481   00074700
EXPRTARA DS    0CL206                                         9913481   00074800
EXPRTCTL DS    CL1                                            9913481   00074900
EXPRTTRC DS    XL1                                            9913481   00075000
EXPRTLEN DS    0CL204                                         9913481   00075100
EXPRTLNE DS    CL132                                          9913481   00075200
EXPRTEXT DS    CL72                                           9913481   00075300
         LTORG                                                9913481   00075400
         DROP  R7                                             9913481   00075500
         SPACE 2                                              9913481   00075600
************************************************************* 9913481   00075700
*        FIXDBCS                                              9913481   00075800
************************************************************* 9913481   00075900
FIXDBCS  CSECT                                                9913481   00076000
         STM   R14,R12,SVL1REGS   SAVE CALLERS REGISTERS      9913481   00076100
         USING *-4,R7             ADDRESS CSECT               9913481   00076200
         LR    R7,R15             LOAD CSECT BASE ADDRESS     9913481   00076300
* LOCATE DBCS FIELDS FOR SO/SI PAIRS TO SPLIT PRINT LINE      9913481   00076400
* TO FIRST PRINT THE DBCS DATA AND THEN PRINT THE SBCS DATA   9913481   00076500
*                                                             9913481   00076600
         LA    R4,LSPRT           GET PRINT AREA ADDRESS      9913481   00076700
         LR    R8,R4              GET PRINT AREA ADDRESS      9913481   00076800
         LA    R8,2(R8)           BUMP PAST CC AND TRC        9913481   00076900
         LA    R6,132             GET PRINT LINE LENGTH       9913481   00077000
         ST    R8,DBCSADDR        SAVE FIELD ADDRESS          9913481   00077100
         BCTR  R6,0               SUBTRACT 1 FOR EXECUTE      9913481   00077200
         ST    R6,DBCSLNG         SAVE FIELD LENGTH           9913481   00077300
         LA    R5,0(R6,R8)        CALC LAST BYTE IN FLD ADDR  9913481   00077400
         MVC   ENGLAREA,0(R4)     MOVE PRT LINE TO DBCS LINE  9913481   00077500
         MVC   SAVETRC,1(R4)      SAVE TRC - MOVE IN DBCS PRT 9913481   00077600
         MVI   1(R4),X'00'        MOVE DEFAULT TRC            9913481   00077700
         MVC   DBCSSIAD,=X'00000000' CLEAR SI ADDRESS         9913481   00077800
         MVC   DBCSSOAD,=X'00000000' CLEAR SO ADDRESS         9913481   00077900
Z0192    EQU   *                                              2024378   00077910
         EX    R6,DBCSFISI        SEE IF THERE IS A 'SI' CHAR 9913481   00078000
         BZ    Z0194              EXIT IF NO 'SI' CHARACTER   9913481   00078100
*                                                             9913481   00078300
*                                                             9913481   00078400
         EX    R6,DBCSFISO        SEE IF THERE IS A 'SO' CHAR 9913481   00078500
         BZ    Z0194              EXIT IF NO 'SO' CHARACTER   9913481   00078600
         MVC   1(1,R4),SAVETRC    MOVE IN SAVED TRC           9913481   00078700
         MVI   ENGLAREA+1,X'00'   MOVE TRC TO ENGLISH LINE    9913481   00078800
         MVI   0(R4),X'F0'        MOVE CC TO DBCS LINE        9913481   00078900
         ST    R1,DBCSSOAD        SAVE ADDR OF 'SO' CHARACTER 9913481   00079000
         ST    R8,WORKADDR        SAVE ADDR IN WORKADDR       9913481   00079100
         S     R1,WORKADDR        GET LENGTH OF ENG/SO AREA   9913481   00079200
         ST    R1,SAVELNG         SAVE LENGTH                 9913481   00079300
         EX    R1,DBCSMVC         MOVE SPACES TO DBCS LINE    9913481   00079400
         L     R6,DBCSLNG         RESTORE LENGTH PRINT LINE   9913481   00079500
         EX    R6,DBCSFISI        SEE IF THERE IS A 'SI' CHAR 9913481   00079600
         CLC   DBCSLNG,SAVELNG    IS SAVELNG > DBCSLNG?       9913481   00079700
         BL    Z0194              YES, EXIT                   9913481   00079800
         S     R6,SAVELNG         SUBTRACT FROM TOTAL LENGTH  9913481   00079900
         ST    R6,SAVELNG         SAVE TEMP LENGTH            9913481   00080000
         BCTR  R6,0               SUBTRACT 1 MORE             9913481   00080100
         ST    R6,DBCSLNG         SAVE FIELD LENGTH           9913481   00080200
         L     R6,SAVELNG         RESTORE R6 LENGTH           9913481   00080300
         BZ    Z0192              LOOP IF NO 'SI' CHARACTER   9913481   00080400
         LR    R8,R1              PUT BEGIN OF SCAN IN R8     9913481   00080500
         LA    R8,1(R8)           BUMP UP 1 PAST SI OR 0F     9913481   00080600
         ST    R1,DBCSSIAD        SAVE ADDR OF 'SI' CHARACTER 9913481   00080700
         L     R9,DBCSSOAD        LOAD SO ADDRESS TO R9       9913481   00080800
         S     R9,DBCSADDR        SUBTRACT 3RD POS FROM SO    9913481   00080900
         LA    R11,ENGLAREA(R9)                               9913481   00081000
         LA    R11,2(R11)         BUMP UP 2 FOR CC AND TRC    9913481   00081100
         XR    R9,R9              CLEAR R9                    9913481   00081200
         L     R9,DBCSSIAD        LOAD SI ADDRESS TO R9       9913481   00081300
         S     R9,DBCSSOAD        SUBTRACT SO FROM SI FOR LNG 9913481   00081400
         ST    R9,SAVELNG         SAVE LENGTH                 9913481   00081500
         L     R6,DBCSLNG         LOAD REST OF RECORD LENGTH  9913481   00081600
         CLC   DBCSLNG,SAVELNG    IS SAVELNG > DBCSLNG?       9913481   00081700
         BNL   Z0193              YES, EXIT                   9913481   00081800
         EX    R9,ENGLMVC         MOVE SPACES TO DBCS IN ENG  9913481   00081900
         B     Z0194              EXIT                        9913481   00082000
Z0193    EQU   *                  YES, EXIT                   9913481   00082100
         S     R6,SAVELNG         SUBTRACT FROM REMAINDER     9913481   00082200
         ST    R6,DBCSLNG         SAVE FIELD LENGTH           9913481   00082300
         EX    R9,ENGLMVC         MOVE SPACES TO DBCS IN ENG  9913481   00082400
         B     Z0192              LOOP BACK FOR MORE SO/SI    9913481   00082500
Z0194    EQU   *                                              9913481   00082600
*                                                             9913481   00082700
* CHECK TO SEE IF SOSI CHARACTERS WERE FOUND                  9913481   00082800
* IF SO, MAKE SURE DBCS PRINT LINE IS IN DBCS FORMAT          9913481   00082900
*                                                             9913481   00083000
         CLC   DBCSSIAD,=X'00000000' SI CHAR LOCATED?         9913481   00083100
         BE    Z0270              NO                          9913481   00083200
         LR    R6,R5              PUT END OF LINE ADDR R6     9913481   00083300
         S     R6,DBCSSIAD        CALCULATE LENGTH TO END     9913481   00083400
         ST    R6,DBCSLNG         SAVE FIELD LENGTH           9913481   00083500
         CLC   DBCSLNG,=X'00000000' IS R6 ZERO?               9913481   00083600
         BE    Z0195              YES, GO TO Z0195            9913481   00083700
         L     R8,DBCSSIAD        PUT END OF DBCS IN R8       9913481   00083800
         LA    R8,1(R8)           BUMP UP 1 PAST SI OR 0F     9913481   00083900
         BCTR  R6,0               SUBTRACT 1 FOR EXECUTE      9913481   00084000
         EX    R6,DBCSMVC         MOVE SPACES END OF DBCS     9913481   00084100
Z0195    EQU   *                                              9913481   00084200
         XR    R8,R8              CLEAR R1                    9913481   00084300
         XR    R6,R6              CLEAR R6                    9913481   00084400
         L     R8,DBCSADDR        LOAD DBCS LINE +2           9913481   00084500
         LA    R6,132             GET PRINT LINE LENGTH       9913481   00084600
         BCTR  R6,0               SUBTRACT 1 FOR EXECUTE      9913481   00084700
         LA    R5,0(R6,R8)        CALC LAST BYTE IN FLD ADDR  9913481   00084800
Z0196    EQU   *                                              9913481   00084900
         EX    R6,DBCSNOSP        FIND OUR FIRST NON SPACE    9913481   00085000
         BZ    Z0204              GO PRINT LINE               9913481   00085100
         CLI   0(R1),X'0F'        IS THIS A SI CHARACTER?     9913481   00085200
         BNE   Z0197              NO, CONTINUE                9913481   00085300
         MVI   0(R1),C' '         SPACE OUT THE '0F' CHAR     9913481   00085400
         B     Z0196              CONTINUE LOOP               9913481   00085500
Z0197    EQU   *                                              9913481   00085600
         ST    R1,DBCSSOAD        SAVE ADDR NON SPACE CHAR    9913481   00085700
         L     R9,DBCSSOAD        GET ADDR OF NON SPACE CHAR  9913481   00085800
         SR    R9,R8              GET LENGTH OF SPACE AREA    9913481   00085900
         ST    R9,SAVELNG         PUT LENGTH IN SAVELNG       9913481   00086000
         TM    SAVELNG+3,X'01'    IS LENGTH ODD               9913481   00086100
         BNO   Z0202              NO, LENGTH IS EVEN          9913481   00086200
         BCTR  R1,0               BACK UP ONE BYTE            9913481   00086300
         ST    R1,DBCSSOAD        SAVE ADDRESS OF NON SPACE   9913481   00086400
Z0198    EQU   *                                              9913481   00086500
         MVC   0(1,R1),1(R1)      MOVE PRINTLINE LEFT 1 BYTE  9913481   00086600
         LA    R1,1(R1)           BUMP TO NEXT CHARACTER      9913481   00086700
         CLI   1(R1),X'0F'        IS THIS A SI CHARACTER?     9913481   00086800
         BNE   Z0198              NO, GO MOVE NEXT CHARACTER  9913481   00086900
         MVI   0(R1),C' '         SPACE OUT THE LAST NONSPACE 9913481   00087000
         ST    R1,DBCSADDR        NEXT SPACE TO START LOOP    9913481   00087100
         MVI   1(R1),C' '         SPACE OUT THE SI OR 0F CHAR 9913481   00087200
Z0200    EQU   *                                              9913481   00087300
         LR    R6,R5              GET END ADDRESS             9913481   00087400
         S     R6,DBCSADDR        CALC LENGTH OF SCAN         9913481   00087500
         L     R8,DBCSADDR        SET SCAN START ADDR         9913481   00087600
         EX    R6,DBCSFISI        FIND THE NEXT SI CHARACTER  9913481   00087700
         BZ    Z0204              EXIT IF END OF PRINTLINE    9913481   00087800
         LR    R6,R5              GET END ADDRESS             9913481   00087900
         SR    R6,R8              CALC LENGTH OF SCAN         9913481   00088000
         B     Z0196              LOOP TO FIX NEXT AREA       9913481   00088100
Z0202    EQU   *                                              9913481   00088200
         LR    R6,R5              GET END ADDRESS             9913481   00088300
         S     R6,DBCSADDR        CALC LENGTH OF SCAN         9913481   00088400
         L     R8,DBCSADDR        SET SCAN START ADDR         9913481   00088500
         EX    R6,DBCSFISI        FIND THE NEXT SI CHARACTER  9913481   00088600
         BZ    Z0204              EXIT IF END OF PRINTLINE    9913481   00088700
         MVI   0(R1),C' '         SPACE OUT THE SI OR 0F CHAR 9913481   00088800
         ST    R1,DBCSADDR        NEXT SPACE TO START LOOP    9913481   00088900
         LR    R6,R5              GET END ADDRESS             9913481   00089000
         SR    R6,R8              CALC LENGTH OF SCAN         9913481   00089100
         B     Z0200              LOOP TO FIX NEXT AREA       9913481   00089200
Z0204    EQU   *                                              9913481   00089300
         L     R2,SVL1REGS+16     RESTORE R2                  9913481   00089400
         TM    LSSPLCD,X'01'      DO I PRINT                  9913481   00089500
         BZ    Z0230              NO                          9913481   00089600
         BAS   R10,Z0110          CALL SIPRINT                2024183   00089700
Z0230    EQU   *                                              9913481   00089800
         CLI   LSPRTCTL,C'7'      DO I PRINT ONLY             9913481   00089900
         BL    Z0235              NO                          9913481   00090000
         CLI   LSPRTCTL,C'9'      DO I PRINT ONLY             9913481   00090100
         BL    Z0260              YES                         9913481   00090200
Z0235    EQU   *                                              9913481   00090300
         TM    LSSPLCD,X'02'      DO I FICHE                  9913481   00090400
         BZ    Z0240              NO                          9913481   00090500
         BAS   R10,Z0120          CALL SIFICHE                2024183   00090600
Z0240    EQU   *                                              9913481   00090700
         TM    LSSPLCD,X'04'      DO I TRANSMIT               9913481   00090800
         BZ    Z0250              NO                          9913481   00090900
         BAS   R10,Z0130          CALL SIXMIT                 2024183   00091000
         EJECT                                                9913481   00091100
Z0250    EQU   *                                              9913481   00091200
         TM    LSSPLCD,X'08'      DO I INQR                   9913481   00091300
         BO    Z0251              YES                         9913481   00091400
         CLI   LSSPLCD,C'G'       SPOOL CODE GREATER THAN G   9913481   00091500
         BH    Z0260              YES - NO INQR               9913481   00091600
         CLI   LSSPLCD,C'A'       SPOOL CODE LESS THAN A      9913481   00091700
         BL    Z0260              YES - NO INQR               9913481   00091800
Z0251    EQU   *                                              9913481   00091900
         BAS   R10,Z0140          CALL SINQR                  2024183   00092000
Z0260    EQU   *                                              9913481   00092100
         MVC   LSPRTARA(L'ENGLAREA),ENGLAREA                  9913481   00092200
Z0270    DS    0H                                             9913481   00092300
         LM    R14,R12,SVL1REGS                               9913481   00092400
         BR    R14                                            9913481   00092500
DBCSMVC  MVC   0(0,R8),SPACES                                 9913481   00092600
ENGLMVC  MVC   0(0,R11),SPACES                                9913481   00092700
DBCSFISO TRT   0(0,R8),SOTABLE                                9913481   00092800
DBCSFISI TRT   0(0,R8),SITABLE                                9913481   00092900
DBCSNOSP TRT   0(0,R8),NOSPTAB                                9913481   00093000
         DS    0D                                             9913481   00093100
SOTABLE  DC    256X'00'                                       9913481   00093200
         ORG   SOTABLE+X'0E'                                  9913481   00093300
         DC    X'0E'                                          9913481   00093400
         ORG                                                            00093500
SITABLE  DC    256X'00'                                       9913481   00093600
         ORG   SITABLE+X'0F'                                  9913481   00093700
         DC    X'0F'                                          9913481   00093800
         ORG                                                            00093900
NOSPTAB  DC    256X'01'                                       9913481   00094000
         ORG   NOSPTAB+X'40'                                  9913481   00094100
         DC    X'00'                                          9913481   00094200
         ORG                                                            00094300
SPACES   DC    CL134' '                                       9913481   00094400
ENGLAREA DS    CL134                                          9913481   00094500
SAVELNG  DS    F                                              9913481   00094600
SAVETRC  DS    CL1                                            9913481   00094700
DBCSADDR DS    F                                              9913481   00094800
DBCSSOAD DS    F                                              9913481   00094900
DBCSSIAD DS    F                                              9913481   00095000
WORKADDR DS    F                                              9913481   00095100
         LTORG                                                9913481   00095200
         DROP  R7                                             9913481   00095300
         EJECT                                                9913481   00095400
*-----------------------------------------------------------* 9913481   00095500
*                                                           * 9913481   00095600
*        FLOATING DECIMAL SUBROUTINE                        * 9913481   00095700
*                                                           * 9913481   00095800
*-----------------------------------------------------------* 9913481   00095900
FIXDEC   CSECT                                                9913481   00096000
         STM   R14,R12,SVL1REGS   SAVE CALLERS REGS           9913481   00096100
         USING *-4,R7                                         9913481   00096200
         LR    R7,R15                                         9913481   00096300
Z0300    LR    R8,R4              A'PRINT LINE                9913481   00096400
         ST    R8,WK@REG8         STORE A'PRINT LINE          9913481   00096500
         CLI   WFLOATDC,C'Y'      FLOATING DEC ENV?           9913483   00096710
         BE    Z0300A             YES, GO SHIFT DECIMAL       9913481   00096720
         CLI   WDIVIDER,C','      SWAP DEC/DIV ENV?           9913483   00096730
         BE    Z0336              NO, REMOVE $ & QUIT S/R     9913481   00096740
Z0300A   EQU *          GO HERE IF FLOATING DEC ENV OR        9913481   00096750
*                       IF COMMA IS DECIMAL ENV               9913481   00096760
         LA    R6,134             L'PRINT LINE+1              9913481   00097600
         AR    R6,R4              R4 = A'START OF LINE        9913481   00097700
         LR    R9,R6              R9 = A'END OF LINE+1        9913481   00097800
         CLI   WFLOATDC,C'Y'      FLOATING DEC ENV?           9913483   00097810
         BNE   Z0304              NO, GO TO Z0304             9913483   00097815
         CLI   SAVEDEC,X'00'      CURR # DECIMALS LOWVAL?     2024207   00097900
         BE    Z0301              YES, SEARCH FOR CURR.       9913481   00098000
         CLI   SAVEDEC,C' '       CURR # DECIMALS BLANK?      2024207   00098100
         BNE   Z0304              NO, USE VALUE SENT          9913481   00098200
Z0301    EQU *                                                9913481   00098300
         LA    R10,SINXDSCT       ADDR CURRENCY CODE TBL      9913481   00098400
Z0302    EQU   *                                              9913481   00098500
         CLI   0(R10),X'FF'       AT END OF TABLE?            9913481   00098600
         BE    Z0304              YES, NOTFND & CONTINUE.     9913481   00098700
         CLC   4(3,R10),SAVECURR  CUR CODE MATCH FOUND?       2024207   00098800
         BE    Z0303              YES, JUMP OUT OF LOOP.      9913481   00098900
         LA    R10,NXDSLENG(R10)  BUMP TO NEXT TBL ITEM       9913481   00099000
         B     Z0302              GO BACK AROUND              9913481   00099100
Z0303    EQU   *                                              9913481   00099200
         MVC   SAVEDEC,12(R10)    RETRV CURR # DEC FROM TAB   2024207   00099350
         L     R1,=A(SAVER1)      ADDRESS PARAMETER ADDRESS   0354531   00099355
         L     R1,0(R1)           ADDRESS PARAMETERS          0354531   00099360
         TM    4(R1),X'80'        ONLY 2 PARAMETERS?          0354531   00099365
         BO    Z0304              NO SPEX AREA - SKIP         0354531   00099370
         TM    8(R1),X'80'        ONLY 3 PARAMETERS?          0354531   00099380
         BO    Z0304              NO SPEX AREA - SKIP         0354531   00099385
         MVC   SPLDEC,12(R10)     RETRV CURR # DEC FROM TAB   0354531   00099390
Z0304    EQU   *                                              9913481   00099400
         ZAP   SVFDSWC,=P'0'      INIT # SHIFTS NEEDED        9913481   00099410
         ZAP   WKFDSWC,=P'0'      INIT # SHIFTS NEEDED        9913481   00099420
         TM    SAVEDEC,X'F0'      # DEC TO SHIFT NUMERIC?     2024207   00099500
         BNO   Z0305              NO, SKIP THE CALC DEC       9913481   00099600
*                                                             9913481   00100100
*        CALCULATE THE NUMBER OF DECIMAL SHIFTS NEEDED        9913481   00100200
*        CURR EQUAL 3 DECIMAL (3 - 2 = +1)  SHIFT DEC LEFT    9913481   00100300
*        CURR EQUAL 2 DECIMAL (2 - 2 =  0)  SKIP DEC SHIFTING 9913481   00100400
*        CURR EQUAL 1 DECIMAL (1 - 2 = -1)  SHIFT DEC RIGHT   9913481   00100500
*        CURR EQUAL 0 DECIMAL (0 - 2 = -2)  SHIFT DEC RIGHT   9913481   00100600
*                                                             9913481   00100700
         PACK  WK@DWRD,SAVEDEC    PACK CURR # DEC             2024207   00100800
         SP    WK@DWRD,=P'2'      SUBTRACT U.S. DOLLAR BASE 2 9913481   00100900
         MVC   SVFDSWC,WK@DWRD+7 CONVERT TO PACKED DECIMAL    9913481   00101000
         MVC   WKFDSWC,SVFDSWC    SAVE THIS 'MOVE' COUNTER    9913481   00101100
*                                                             9913481   00101200
*        SCAN FOR FIRST $ SIGN                                9913481   00101300
*                                                             9913481   00101400
Z0305    EQU   *                                              9913481   00101500
         CR    R4,R9              REACHED THE END YET?        2024207   00101520
         BNL   Z0337              YES, NOTHING MORE TO FIND   2024207   00101540
         CLC   1(6,R4),=C'$DJDE$' IS IT XEROX CTL LINE?       0334692   00101542
         BE    Z0337              YES, DON'T REMOVE $         0334692   00101544
         CLC   2(5,R4),=C'$DJDE'  IS IT XEROX CTL LINE?       0334692   00101546
         BE    Z0337              YES, DON'T REMOVE $         0334692   00101548
         LR    R6,R9              LOAD END OF THE LINE        2024207   00101560
         SR    R6,R4              SUB CURRENT PTR TO GET LEN  2024207   00101580
         BCTR  R6,0               SUB 1 FROM LENGTH FOR EX    2024207   00101600
         EX    R6,LOOKDOLL        LOOK FOR $                  2024207   00101620
         BZ    Z0337              DID NOT FIND $, C'EST FINI  2024207   00101640
         LR    R4,R1              LOAD ADDR OF $              2024207   00101660
         BL    Z0306              FOUND $ GO FLOAT DECIMAL    2024207   00101680
         MVI   0(R4),C' '         CLEAR LAST BYTE WITH $      2024207   00101700
         B     Z0337              C'EST FINI                  2024207   00101720
LOOKDOLL TRT   0(0,R4),DOLLARTR   LOOK FOR $                  2024207   00101740
*                                                             2024207   00101760
DOLLARTR DC    256X'00'           00-FF                       2024207   00101780
         ORG   DOLLARTR+X'5B'     5B-5B                       2024207   00101800
         DC    X'5B'              $                           2024207   00101820
         ORG                                                            00101840
*                                                             2024207   00101860
Z0306    EQU   *                                              9913481   00102200
         MVI   0(R4),C' '         BLANK OUT THE $             9913481   00102300
         XR    R6,R6              R6 = A'DECIMAL IN FIGURE    9913481   00102400
         STCM  R4,15,SVSTRFLD                                 9913481   00102410
*                                                             9913481   00102500
*        $ SIGN FOUND                                         9913481   00102600
*        SCAN FOR FIRST 0-9 OR DECIMAL SYMBOL                 9913481   00102700
*                                                             9913481   00102800
Z0307    EQU   *                                              9913481   00102900
         LA    R4,1(R4)           NO, BUMP TO NEXT BYTE       9913481   00103000
         CR    R4,R9              REACH END YET?              9913481   00103100
         BH    Z0336              YES, NO NUMERIC OR DEC FND  9913481   00103200
         CLI   0(R4),C'.'         IS IT  A DECIMAL?           9913481   00103300
         BE    Z0308              YES, GO STORE ADDR          9913481   00103400
         CLI   0(R4),C'0'         IS IT NUMERIC?              9913481   00103500
         BL    Z0307              NO, KEEP LOOKING            9913481   00103600
         CLI   0(R4),C'9'         IS IT NUMERIC?              9913481   00103700
         BNH   Z0309              YES, FOUND FIRST DIGIT      9913481   00103800
         B     Z0307              NO, KEEP LOOKING            9913481   00103900
Z0308    EQU   *                                              9913481   00104000
         LR    R11,R4             R11 = A'FIRST POSITION      9913481   00104100
         LR    R6,R4              R6  = A'DECIMAL             9913481   00104200
         B     Z0310                                          9913481   00104300
Z0309    EQU   *                                              9913481   00104400
         LR    R11,R4             R11 = A'FIRST DIGIT         9913481   00104500
*                                                             9913481   00104600
*        SCAN FOR THE END OF THE FIGURE                       9913481   00104700
*                                                             9913481   00104800
Z0310    EQU   *                                              9913481   00104900
         LA    R4,1(R4)           BUMP TO NEXT BYTE           9913481   00105000
         CR    R4,R9              REACH END YET?              9913481   00105100
         BH    Z0312              YES, CHECK IF DEC FND       9913481   00105200
         CLI   0(R4),C'.'         THIS IS A DECIMAL?          9913481   00105300
         BE    Z0311              YES, KEEP THIS ADDR.        9913481   00105400
         CLI   0(R4),C','         THIS IS A DIVIDER?          9913481   00105500
         BE    Z0310              YES, IGNORE & CONTINUE.     9913481   00105600
         CLI   0(R4),C'0'         THIS BYTE < X'F0'?          9913481   00105700
         BL    Z0312              YES, STOP LOOKING.          9913481   00105800
         CLI   0(R4),C'9'         THIS BYTE > X'F9'?          9913481   00105900
         BH    Z0312              YES, STOP LOOKING.'         9913481   00106000
         B     Z0310              FOUND NUMERIC, KEEP LOOKING 9913481   00106100
Z0311    EQU   *                                              9913481   00106200
         LR    R6,R4              R6 = A'DECIMAL              9913481   00106300
         B     Z0310              GO BACK AROUND              9913481   00106400
Z0312    EQU   *                                              9913481   00106500
         ST    R4,WK@REG4         STORE A'END OF FLD          9913481   00106600
         LR    R10,R4             R10 EQUAL END OF FLD        9913481   00106610
         S     R10,SVSTRFLD       SUBTRACT START OF FLD       9913481   00106620
         ST    R10,SVLENFLD       TO GET LENGTH OF FLD        9913481   00106630
         LTR   R6,R6              ANY DECIMAL POINT FOUND?    9913481   00106700
         BNZ   Z0323              YES, SKIP WHOLE $ PROC.     9913481   00106800
*                                                             9913481   00106900
*        PROCESS WHOLE DOLLAR FIGURES                         9913481   00107000
*                                                             9913481   00107100
Z0313    EQU   *                                              9913481   00107200
         CLI   SAVEDEC,C'0'       0 DEC PLACES REQ'D?         2024207   00107300
         BE    Z0314              YES, SHIFT UP & ADD '**'.   9913481   00107400
         CLI   SAVEDEC,C'1'       1 DEC PLACE REQ'D?          2024207   00107500
         BE    Z0317              YES, SHIFT UP & ADD '*'.    9913481   00107600
         CLI   SAVEDEC,C'3'       3 DEC PLACE REQ'D?          2024207   00107700
         BE    Z0320              YES, FLOAT AS 1 DEC PLACE,  9913481   00107800
         B     Z0330              ELSE, PROCESS NEXT FIGURE.  9913481   00107900
*                                                             9913481   00108000
*        0 DECIMAL FOR WHOLE $ = ADD '**' AT END OF FIGURE    9913481   00108100
*                                                             9913481   00108200
Z0314    EQU   *                                              9913481   00108300
         BCTR  R11,0                                          9913481   00108400
         BCTR  R11,0              BACK UP 2 FOR A'1ST DIGIT   9913481   00108500
         ST    R11,WK@REG11       STORE NEW A'1ST DIGIT       9913481   00108600
         BCTR  R4,0                                           9913481   00108700
         BCTR  R4,0               BACK UP 2 FOR A'LAST DIG.   9913481   00108800
Z0315    EQU   *                                              9913481   00108900
         LR    R6,R4              LOAD FIELD END POINT        2024207   00109000
         SR    R6,R11             SUB BEGINING TO CALC LENGTH 2024207   00109100
         BCTR  R6,0               SUB 1 FOR EXECUTE           2024207   00109200
         EX    R6,SHIFT2          EXECUTE FIELD SHIFT         2024207   00109300
Z0316    EQU   *                                              9913481   00109400
         MVC   0(2,R4),=C'**'     ADD '**' AT END OF FIGURE   2024207   00109500
         L     R11,WK@REG11       RESTORE NEW A'1ST DIGIT     9913481   00109600
         L     R6,WK@REG4         R6 = A'END OF FIGURE        9913481   00109700
         B     Z0323              GO TO SWPDC000 RTN          9913481   00109800
SHIFT2   MVC   0(0,R11),2(R11)                                2024207   00109850
*                                                             9913481   00109900
*        1 DECIMAL FOR WHOLE $ = ADD '*' AT END OF FIGURE     9913481   00110000
*                                                             9913481   00110100
Z0317    EQU   *                                              9913481   00110200
         BCTR  R11,0              BACK UP 1 FOR A'1ST DIGIT   9913481   00110300
         ST    R11,WK@REG11       STORE NEW A'1ST DIGIT       9913481   00110400
         BCTR  R4,0               BACK UP 1 FOR A'LAST DIG.   9913481   00110500
Z0318    EQU   *                                              9913481   00110600
         LR    R6,R4              LOAD FIELD END POINT        2024207   00110700
         SR    R6,R11             SUB BEGINING TO CALC LENGTH 2024207   00110800
         BCTR  R6,0               SUB 1 FOR EXECUTE           2024207   00110900
         EX    R6,SHIFT1          EXECUTE FIELD SHIFT         2024207   00111000
Z0319    EQU   *                                              9913481   00111100
         MVI   0(R4),C'*'         ADD '**' AT END OF FIGURE   2024207   00111200
         L     R11,WK@REG11       RESTORE NEW A'1ST DIGIT     9913481   00111300
         L     R6,WK@REG4         R6 = A'END OF FIGURE        9913481   00111400
         B     Z0323              GO TO SWPDC000 RTN          9913481   00111500
SHIFT1   MVC   0(0,R11),1(R11)                                2024207   00111550
*                                                             9913481   00111600
*        3 DECIMALS FOR WHOLE $ = SHIFT DECIMAL POINT 1 LEFT  9913481   00111700
*                                                             9913481   00111800
Z0320    EQU   *                                              9913481   00111900
         BCTR  R11,0              BACK UP 1 FOR A'1ST DIGIT   9913481   00112000
         ST    R11,WK@REG11       STORE NEW A'1ST DIGIT       9913481   00112100
         BCTR  R4,0               BACK UP 1 FOR A'LAST DIG.   9913481   00112200
         BCTR  R4,0               BACK UP 1 MORE FOR A'DECIML 9913481   00112300
Z0321    EQU   *                                              9913481   00112400
         LR    R6,R4              LOAD FIELD END POINT        2024207   00112500
         SR    R6,R11             SUB BEGINING TO CALC LENGTH 2024207   00112600
         BCTR  R6,0               SUB 1 FOR EXECUTE           2024207   00112700
         EX    R6,SHIFT1          EXECUTE FIELD SHIFT         2024207   00112800
Z0322    EQU   *                                              9913481   00112900
         MVI   0(R4),C'.'         PLACE DECIMAL IN FIGURE     2024207   00113000
         LR    R6,R4              R6 = A'DECIMAL POINT        2024207   00113100
         BCTR  R6,0               BACK UP ONE,IGNORE THIS DEC 9913481   00113200
         L     R11,WK@REG11       RESTORE NEW A'1ST DIGIT     9913481   00113300
         B     Z0323              GO TO SWPDC000 RTN          9913481   00113400
*                                                             9913481   00113500
*        'FLOAT' DECIMAL POINT LOGIC                          9913481   00113600
*                                                             9913481   00113700
Z0323    EQU   *                                              9913481   00113800
         CP    SVFDSWC,=PL1'0'    SHIFTS TO BE MADE?          9913481   00113810
         BE    Z0330              NO, CHK SWAP DEC/DIV        9913481   00113820
         LR    R10,R6              SET R10 TO WHERE DEC PT IS 9913481   00113900
         ICM   R15,15,=V(SWPDC000) CALL DEC SWAP ROUTINE      9913481   00114000
         BASR  R14,R15                                        2024207   00114100
*                                                             9913481   00114102
*        SWAP DECIMAL/DIVIDER IF NEEDED                       9913481   00114104
*                                                             9913481   00114106
Z0330    EQU   *                                              9913481   00114108
         CLI   WDIVIDER,C','                                  9913483   00114110
         BE    Z0335                                          9913481   00114112
         ICM   R15,15,=V(DECDIV00) CALL DEC SWAP ROUTINE      9913481   00114114
         BASR  R14,R15                                        2024207   00114116
*                                                             9913481   00114200
*        BUMP TO NEXT FLOATING DECIMAL FIELD ON PRINT LINE    9913481   00114300
*                                                             9913481   00114400
Z0335    EQU   *                                              9913481   00114500
         L     R4,WK@REG4         RESTORE A'END OF LAST FLD   9913481   00114600
         B     Z0305              GO BACK & SCAN FOR NEXT $   9913481   00114700
Z0336    EQU   *                                              9913481   00114900
         CLI   SAVEPRTD,C'Y'      DO WE LEAVE $               2024346   00114910
         BE    Z0337              YES, SKIP THE REMOVAL OF $  2024346   00114920
         L     R8,WK@REG8         RESTORE A'START OF PRT LINE 9913481   00115000
         CLC   1(6,R8),=C'$DJDE$' IS IT XEROX CTL LINE?       0334692   00115010
         BE    Z0337              YES-DON'T REMOVE $          0334692   00115012
         CLC   2(5,R8),=C'$DJDE'  IS IT XEROX CTL LINE?       0334692   00115014
         BE    Z0337              YES-DON'T REMOVE $          0334692   00115016
         TR    0(132,R8),TRDOLLAR REMOVE ALL $ FROM PRT LINE  9913481   00115100
Z0337    EQU   *                                              9913481   00115200
         LM    R14,R12,SVL1REGS   RESTORE CALLER REGS         9913481   00115300
         BR    R14                RETURN TO CALLER            9913481   00115400
*                                                             9913481   00115500
*        END OF FLOATING DECIMAL SUBROUNTINE                  9913481   00115600
*                                                             9913481   00115700
WK@REG8  DS    F                  R8  START OF PRINT LINE     9913481   00115800
WK@REG4  DS    F                  R4  CUR POS IN PRT LINE     9913481   00116000
WK@REG11 DS    F                  R11 1ST POS IN FIGURE       9913481   00116100
WK@DWRD  DS    D                  WORK FIELD FOR CVB          9913481   00116200
TRDOLLAR DC    X'000102030405060708090A0B0C0D0E0F'     00-0F  9913481   00116300
         DC    X'101112131415161718191A1B1C1D1E1F'     10-1F  9913481   00116400
         DC    X'202122232425262728292A2B2C2D2E2F'     20-2F  9913481   00116500
         DC    X'303132333435363738393A3B3C3D3E3F'     30-3F  9913481   00116600
         DC    X'404142434445464748494A4B4C4D4E4F'     40-4F  9913481   00116700
         DC    X'505152535455565758595A405C5D5E5F'     50-5F  9913481   00116800
         DC    X'606162636465666768696A6B6C6D6E6F'     60-6F  9913481   00116900
         DC    X'707172737475767778797A7B7C7D7E7F'     70-7F  9913481   00117000
         DC    X'808182838485868788898A8B8C8D8E8F'     80-8F  9913481   00117100
         DC    X'909192939495969798999A9B9C9D9E9F'     90-9F  9913481   00117200
         DC    X'A0A1A2A3A4A5A6A7A8A9AAABACADAEAF'     A0-AF  9913481   00117300
         DC    X'B0B1B2B3B4B5B6B7B8B9BABBBCBDBEBF'     B0-BF  9913481   00117400
         DC    X'C0C1C2C3C4C5C6C7C8C9CACBCCCDCECF'     C0-CF  9913481   00117500
         DC    X'D0D1D2D3D4D5D6D7D8D9DADBDCDDDEDF'     D0-DF  9913481   00117600
         DC    X'E0E1E2E3E4E5E6E7E8E9EAEBECEDEEEF'     E0-EF  9913481   00117700
         DC    X'F0F1F2F3F4F5F6F7F8F9FAFBFCFDFEFF'     F0-FF  9913481   00117800
         LTORG                                                9913481   00117900
         COPY  SINXDSCT                                       9913481   00118000
         DROP  R7                                             9913481   00118200
         EJECT                                                9913481   00118300
         TITLE 'SWPDC000 - SWAP DECIMAL IN FIELD'             9913481   00118400
**************************************************************9913481   00118500
*               SWAP DECIMAL ROUTINE                         *9913481   00118600
* THIS RTN SHIFTS THE DECIMAL AND COMMAS IN THE FIELD PASSED *9913481   00118700
* TO ALIGN WITH THE CURRENCY NEEDED.                         *9913481   00118800
*                                                            *9913481   00118900
* R11 POINTS TO THE FRONT OF THE FIELD                       *9913481   00119000
* R10 POINTS TO THE BACK OF THE FIELD                        *9913481   00119100
* WKFDSWC  - THIS FIELD HOLDS THE NUMBER OF SWAPS NEEDED     *9913481   00119200
*                                                            *9913481   00119300
* EXAMPLE :                                                  *9913481   00119400
*               0 DEC       1 DEC          3 DEC             *9913481   00119500
* FIELD SENT  = 1234.56     1234.56        1234.56           *9913481   00119600
* ADJUSTED TO = 123456.     12345.6        123.456           *9913481   00119700
*                                                            *9913481   00119800
**************************************************************9913481   00119900
SWPDC000 CSECT                                                9913481   00120000
         STM   R14,R12,SVL2REGS   SAVE ALL REGS               9913481   00120100
         USING SWPDC000,R7                                    9913481   00120200
         LR    R7,R15             SET ADDRESSABILITY          9913481   00120300
*                                                             9913481   00120400
         MVC   WKFDSWC,SVFDSWC    RESET NUM OF SHIFTS         9913481   00120500
SWPDC010 EQU   *                  LOOP TO FIND DECIMAL        9913481   00120600
         CLI   0(R10),C'.'        HAVE WE FOUND THE DECIMAL?  9913481   00120700
         BE    SWPDC020           YES, GO SHIFT IT            9913481   00120800
         CLI   0(R10),C','        HAVE WE FOUND THE DIVIDER?  9913481   00120900
         BE    SWPDC070           YES, GO SHIFT IT            9913481   00121000
         BCTR  R10,0              NO, BUMP BACK ONE           9913481   00121100
         CR    R10,R11            REACH START YET?            9913481   00121200
         BNL   SWPDC010           NO, KEEP LOOKING            9913481   00121300
         B     SWPDC900           YES, DID NOT FIND DECIMAL   9913481   00121400
*                                 *---------------------*     9913481   00121500
*                                 *  SHIFT DECIMAL LOOP *     9913481   00121600
*                                 *---------------------*     9913481   00121700
SWPDC020 EQU   *                                              9913481   00121800
         CP    WKFDSWC,=PL1'0'    DIRECTION OF SHIFT?         9913481   00121900
         BH    SWPDC030           POSITIVE, GO SHIFT DEC LEFT 9913481   00122000
         BZ    SWPDC050           NO MORE - CONTINUE ON TO COM9913481   00122100
         MVC   0(1,R10),1(R10)    NEGATIVE - OVERLAY DEC WITH 9913481   00122200
         MVI   1(R10),C'.'        ...MOVE DEC TO RIGHT        9913481   00122300
         AP    WKFDSWC,=PL1'1'    DECREM. NUM OF SHIFTS       9913481   00122400
         LA    R10,1(R10)         ADDR NEXT DIGIT IN FLD      9913481   00122500
         B     SWPDC020           GO BACK FOR NEXT SHIFT      9913481   00122600
SWPDC030 EQU   *                  POSITIVE, SHIFT DEC TO LEFT 9913481   00122700
         BCTR  R10,0              ADDR PREV DIGIT IN FLD      9913481   00122800
         CLI   0(R10),C' '        IS THIS A SPACE?            9913481   00122900
         BNE   SWPDC040           NO, CONTINUE                9913481   00123000
         MVI   0(R10),C'0'        YES, CHANGE TO ZERO.        9913481   00123100
SWPDC040 EQU   *                                              9913481   00123200
         MVC   1(1,R10),0(R10)    OVERLAY DEC WITH DIGIT      9913481   00123300
         MVI   0(R10),C'.'        ...MOVE DEC TO LEFT         9913481   00123400
         SP    WKFDSWC,=PL1'1'    DECREM. NUM OF SHIFTS       9913481   00123500
         B     SWPDC020           GO BACK FOR NEXT SHIFT      9913481   00123600
*                                 *--------------------*      9913481   00123700
*                                 *  SHIFT COMMAS LOOP *      9913481   00123800
*                                 *--------------------*      9913481   00123900
SWPDC050 EQU   *                                              9913481   00124000
         MVC   WKFDSWC,SVFDSWC       RESET SHIFTS NEEDED      9913481   00124100
SWPDC060 EQU   *                                              9913481   00124200
         BCTR  R10,0                   R10 BACK 1             9913481   00124300
         CR    R10,R11                 AT THE BEGINNING?      9913481   00124400
         BL    SWPDC090                YES.. WE ARE FINISHED  9913481   00124500
         CLI   0(R10),C','             IS IT COMMA?           9913481   00124600
         BE    SWPDC070                YES.. GO SHIFT IT      9913481   00124700
         CLI   0(R10),C' '             IS IT A SPACE?         9913481   00124800
         BNE   SWPDC060                NO.. KEEP LOOKING      9913481   00124900
         B     SWPDC090                YES.. WE ARE FINISHED  9913481   00125000
SWPDC070 EQU   *                       FOUND COMMA            9913481   00125100
         CP    WKFDSWC,=PL1'0'         DIRECTION OF SHIFT?    9913481   00125200
         BH    SWPDC080                POSITIVE/SHIFT COMMA L 9913481   00125300
         BZ    SWPDC050                NO MORE - LOOK FOR NXT 9913481   00125400
         MVC   0(1,R10),1(R10)         SHIFT # AND COMMA      9913481   00125500
         MVI   1(R10),C','             SHIFT COMMA RIGHT      9913481   00125600
         AP    WKFDSWC,=PL1'1'         DECREM. NUM OF SHIFTS  9913481   00125700
         LA    R10,1(R10)              ADDR NEXT DIGIT IN FLD 9913481   00125800
         B     SWPDC070                GO BACK FOR NEXT SHIFT 9913481   00125900
SWPDC080 EQU   *                       FOUND COMMA...         9913481   00126000
         BCTR  R10,0                   ADDR PREV DIGIT IN FLD 9913481   00126100
         MVC   1(1,R10),0(R10)         SHIFT # AND COMMA      9913481   00126200
         MVI   0(R10),C','             SHIFT COMMA LEFT       9913481   00126300
         SP    WKFDSWC,=PL1'1'         DECREM. NUM OF SHIFTS  9913481   00126400
         B     SWPDC070                GO BACK FOR NEXT SHIFT 9913481   00126500
*                                 *--------------------*      9913481   00126600
*                                 *  BLANK OUT LEADING *      9913481   00126700
*                                 *  COMMAS AND ZEROS  *      9913481   00126800
*                                 *--------------------*      9913481   00126900
SWPDC090 EQU   *                                              9913481   00127000
         CLI   1(R10),C','             LEADING COMMA TO REMV? 9913481   00127100
         BNE   SWPDC100                NO, CONTINUE           9913481   00127200
         MVI   1(R10),C' '             YES, BLANK IT OUT      9913481   00127300
SWPDC100 EQU   *                                              9913481   00127400
         CP    SVFDSWC,=PL1'0'         IS IT 0 DEC OR 1 DEC?  9913481   00127700
         BNL   SWPDC900                ...NO, THEN FINISHED   9913481   00127800
         MVC   WKFDSWC,SVFDSWC         ...YES, RESET # SHIFTS 9913481   00127900
SWPDC110 EQU   *                                              9913481   00128000
         CLI   1(R10),C'0'             FIRST SIG. DIG. IS 0?  9913481   00128100
         BE    SWPDC120                ...YES, GO BLANK IT OUT9913481   00128200
         CLI   1(R10),C' '             IS IT A BLANK?         9913481   00128300
         BNE   SWPDC900                ...NO,  FINISHED.      9913481   00128400
         LA    R10,1(R10)              ...YES, NEXT DIGIT     9913481   00128500
         B     SWPDC110                CHECK NEXT DIGIT       9913481   00128600
SWPDC120 EQU   *                       HAVE A ZERO            9913481   00128700
         MVI   1(R10),C' '             BLANK OUT THIS ZERO    9913481   00128800
         AP    WKFDSWC,=PL1'1'         INCREM. LOOP COUNTER   9913481   00128900
         BZ    SWPDC900                FINISHED? GET OUT      9913481   00129000
         LA    R10,1(R10)              NO, LOOK AT NEXT BYTE  9913481   00129100
         CLI   1(R10),C'0'             FIRST SIG. FIG. IS 0?  9913481   00129200
         BNE   SWPDC900                ...NO, GET OUT         9913481   00129300
         CLI   2(R10),C'.'             IS IT '0.' ON ZERO DEC 9913481   00129400
         BE    SWPDC900                YES, LEAVE IT          9913481   00129500
         B     SWPDC120                NO, GO BACK TO BLANK IT9913481   00129600
SWPDC900 EQU   *                                              9913481   00129700
         LM    R14,R12,SVL2REGS                               9913481   00129800
         BR    R14                     RETURN                 9913481   00129900
         LTORG                                                9913481   00130000
         EJECT                                                9913481   00130010
         TITLE 'DECDIV00 - SWAP DECIMAL/DIVIDER ROUTINE'      9913481   00130020
**************************************************************9913481   00130030
*               SWAP DECIMAL/DIVIDER ROUTINE                 *9913481   00130040
* THIS ROUTINE WILL SWAP THE PERIOD AND COMMAS IN THE OUTPUT *9913481   00130050
* FOR CLIENTS THAT USE THE COMMA AS DECIMAL AND PERIOD AS    *9913481   00130060
* DIVIDER SO THAT PROPER EDITING IS DONE.                    *9913481   00130070
* THE OUTPUT IS ADJUSTED FROM:                               *9913481   00130080
* 99999,999,999.99                                           *9913481   00130090
* TO:                                                        *9913481   00130100
* 99999.999.999,99                                           *9913481   00130110
*                                                            *9913481   00130120
**************************************************************9913481   00130130
DECDIV00 CSECT                                                9913481   00130140
         STM   R14,R12,SVL2REGS   SAVE ALL REGS               9913481   00130150
         USING DECDIV00,R7                                    9913481   00130160
         LR    R7,R15             SET ADDRESSABILITY          9913481   00130170
*                                                             9913481   00130180
         XR    R6,R6                                          9913481   00130190
         XR    R10,R10                                        9913481   00130200
         ICM   R6,15,SVSTRFLD     GET FIELD ADDRESS           9913481   00130210
         BZ    DECDIV99           ZERO? GET OUT               9913481   00130220
         ICM   R10,15,SVLENFLD    GET FIELD LENGTH            9913481   00130230
         BZ    DECDIV99           ZERO? GETOUT                9913481   00130240
         BCTR  R10,0              DECREMENT FOR EXECUTE INSTR 9913481   00130250
         EX    R10,TRDECDIV       HAVE WE FOUND THE DECIMAL?  9913481   00130260
*                                                             9913481   00130270
DECDIV99 EQU   *                                              9913481   00130280
         LM    R14,R12,SVL2REGS                               9913481   00130290
         BR    R14                     RETURN                 9913481   00130300
*                                                             9913481   00130310
TRDECDIV TR    0(0,R6),DECDIVTB                               9913481   00130320
*                                                             9913481   00130330
*   TABLE OF ALL HEX CHARACTERS WITH 4B CHANGED TO 6B AND     9913481   00130340
*                                    6B CHANGED TO 4B         9913481   00130350
DECDIVTB DC    X'000102030405060708090A0B0C0D0E0F'     00-0F  9913481   00130360
         DC    X'101112131415161718191A1B1C1D1E1F'     10-1F  9913481   00130370
         DC    X'202122232425262728292A2B2C2D2E2F'     20-2F  9913481   00130380
         DC    X'303132333435363738393A3B3C3D3E3F'     30-3F  9913481   00130390
         DC    X'404142434445464748494A6B4C4D4E4F'     40-4F  9913481   00130400
         DC    X'505152535455565758595A5B5C5D5E5F'     50-5F  9913481   00130410
         DC    X'606162636465666768696A4B6C6D6E6F'     60-6F  9913481   00130420
         DC    X'707172737475767778797A7B7C7D7E7F'     70-7F  9913481   00130430
         DC    X'808182838485868788898A8B8C8D8E8F'     80-8F  9913481   00130440
         DC    X'909192939495969798999A9B9C9D9E9F'     90-9F  9913481   00130450
         DC    X'A0A1A2A3A4A5A6A7A8A9AAABACADAEAF'     A0-AF  9913481   00130460
         DC    X'B0B1B2B3B4B5B6B7B8B9BABBBCBDBEBF'     B0-BF  9913481   00130470
         DC    X'C0C1C2C3C4C5C6C7C8C9CACBCCCDCECF'     C0-CF  9913481   00130480
         DC    X'D0D1D2D3D4D5D6D7D8D9DADBDCDDDEDF'     D0-DF  9913481   00130490
         DC    X'E0E1E2E3E4E5E6E7E8E9EAEBECEDEEEF'     E0-EF  9913481   00130500
         DC    X'F0F1F2F3F4F5F6F7F8F9FAFBFCFDFEFF'     F0-FF  9913481   00130510
         LTORG                                                9913481   00130520
         EJECT                                                9913481   00130530
SISPOOL  CSECT                                                          00170190
         LTORG                                                          00170191
         DS    0D                                                       00170192
SITMSTMP DC    CL64'SISPOOL   -----TSD-             04/30/10  08.42.12' 00170193
*        THIS PROGRAM CONTAINS TRADE SECRETS THAT BELONG                00170194
*        TO FIDELITY INFORMATION SERVICES AND IS                        00170195
*        LICENSED BY AN AGREEMENT.  ANY UNAUTHORIZED ACCESS,            00170196
*        USE, DUPLICATION, OR DISCLOSURE IS UNLAWFUL.                   00170197
*        COPYRIGHT FIDELITY INFORMATION SERVICES                        00170198
*        2010, ALL RIGHTS RESERVED.                                     00170199
         END                                                            00170200
