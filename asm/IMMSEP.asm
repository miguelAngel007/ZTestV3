*     * FO5238 * 06/26/11 PROYECTO REBORN
*        PRINT NOGEN                                                    00000100
*---------------------------------------------------------------------- 00000200
*        THIS PHASE WILL EXPAND THE IMPACS MASTER RECORD                00000300
*        INTO THE FULL 16630 BYTE SIZE.  THIS WILL BE DONE BY 0930011   00000400
*        CHECKING EACH TRAILER OCCURS FIELD AND EXPANDING IF            00000500
*        PRESENT.  THE TRAILERS WILL BE EXPANDED IN THE MASTER          00000600
*        AREA PASSED IN PARAMETER ONE FORM THE MAINLINE PROGRAM.        00000700
*                                                                       00000800
*                                                                       00000900
*            PARAMETERS ARE PASSED:  PARAMETER 1 - R8                   00001000
*                                    PARAMETER 2 - R3                   00001100
*                                                                       00001200
*        PARAMETER 1 WILL BE THE ADDRESS OF THE RETURN FILE AREA.       00001300
*        PARAMETER 2 WILL BE THE ADDRESS OF THE CONDENSED MASTER.       00001400
*                                                                       00001500
*                                                                       00001600
*---------------------------------------------------------------------- 00001700
*     *     PROGRAMS USING THIS ROUTINE ARE:                    *       00001800
*     *                                                         *       00001900
*     *                  IMACTMA IM31 FIIMLOAD                  *       00002000
*     *                                                         *       00002100
*     * ****                                                *****       00002200
*---------------------------------------------------------------------- 00002300
*                                                                       00002400
*        THIS ROUTINE EXPANDS THE IMPACS MASTER.                        00002500
*                                                                       00002600
*        THE MASTER MAY ONLY BE EXPANDED INTO ANOTHER AREA,             00002700
*                      NEVER THE SAME AREA.                             00002800
*                                                                       00002900
*        THIS IS THE SOURCE FOR BATCH, FIAS AND TS EXPANSION.           00003000
*        BASED ON THE PRESENCE OR ABSENCE OF THE TS 'EYECATCHER'        00003100
*        A DETERMINATION IS MADE WHETHER THE CALL CAME FROM TS OR       00003200
*        FIAS.  IF FIAS, THE ADDRESS OF THE COMPRESSED MASTER IS        00003300
*        AT OFFSET 69 OF THE COMMON AREA.  IF TS, THE ADDRESS OF        00003400
*        THE COMPRESSED MASTER IS THE FIRST 4 BYTES OF THE COMMON       00003500
*        AREA.                                                          00003600
*                                                                       00003700
*           ONLINE FIAS:                                                00003800
*                  EXEC CICS LINK PROGRAM ('FIIMMSEP')                  00003900
*                                 COMMAREA (WS-COMMON-AREA)             00004000
*                                 LENGTH (WS-COMMON-AREA-LENGTH)        00004100
*                                 END-EXEC.                             00004200
*                                                                       00004300
*           BATCH:                                                      00004400
*                  CALL 'IMMSEP'   USING MASTER-AREA           00901360 00004500
*                                        COMP-MASTER-AREA.              00004600
*                                                                       00004700
*        PARAMETER 1 IS THE ADDRESS OF THE EXPANDED MASTER.             00004800
*        PARAMETER 2 IS THE ADDRESS OF THE CONDENSED MASTER.            00004900
*                                                                       00005000
*                  REVISED                                    0266888   00005005
* 05/23/02  LE ENABLED, REENTRANT                             0266888   00005095
* 28/04/11  APPLY FIX FOR RSEC IM0187                         ~~~0187   00005096
*---------------------------------------------------------------------- 00005100
         SIRENT                                                         00005200
         COPY  SIOPTNS                                        0266888   00005202
         LCLB  &CICS                                          0266888   00005205
         SIEQREG                                              0266888   00005300
         AIF   ('&SYSPARM' NE 'CICS').BTCHDTA                 0266888   00005400
DFHEISTG DSECT                                                          00005500
         AGO   .DATAMRG                                       0266888   00005600
.BTCHDTA ANOP                                                           00005700
         PRINT ON,GEN                                         0266888   00005800
         CEECAA                                                         00005900
         EJECT                                                0266888   00006000
         CEEDSA                                                         00006100
         EJECT                                                0266888   00006200
.DATAMRG ANOP                                                           00006300
UDSABEG  DS    0D                  BEGIN OF USER DSA          0266888   00006400
SAVEAREA DS    9D                                             0266888   00006500
DBLWORD  DS    D                                              0266888   00006600
PGMPLIST DS    F                   ORIGINAL R1 SAVE AREA      0266888   00006700
FWORD    DS    F                 FULL WORD FOR GETMAIN        0266888   00006800
SAVE13   DS    F                                              0266888   00006900
RETADDR  DS    F                                              0266888   00007000
FULLWORD DS    F                                              0266888   00007005
LENGTH   DS    F                                              0266888   00007010
RECLENGH DS    0F                                             0266888   00007015
BINZEROS DS    H                                              0266888   00007020
RECLNG   DS    H                                              0266888   00007025
HALFWORD DS    H                                              0266888   00007030
HWORD    DS    H                                              0266888   00007032
         COPY  TSTS2WMS                                       0266888   00007035
*                                                             0266888   00007040
WSEYECAT DS    CL8                                            0266888   00007042
         COPY  TSTS2ECC                                       0266888   00007045
#UDSALEN EQU   *-UDSABEG           LENGTH OF DSA              0266888   00007050
         AIF   ('&SYSPARM' NE 'CICS').GEN1                    0266888   00007055
&CICS    SETB  1                                              0266888   00007060
         TITLE 'FIIMMSEP - IM MASTER RECORD EXPANSION ROUTINE'          00007100
*                                                                       00007200
         AIF   (&CICS).ONL3                                             00007300
         AGO   .GEN1                                                    00007400
.ONL3    ANOP                                                           00007500
*                                                                       00007600
         EJECT                                                          00008200
FIIMMSEP DFHEIENT CODEREG=(R12),DATAREG=(R13,R9),EIBREG=(R2)            00008300
FIIMMSEP AMODE 31                                             9915833 / 00008335
FIIMMSEP RMODE ANY                                            9915833 / 00008340
         L     R3,DFHEICAP         LOAD ADDR OF COMMON AREA             00008400
         CLC   TSEYECAT,0(R3)      IS TS EYE CATCHER PRESENT            00008500
         BE    TSSTART             YES - GO TO TS START UP              00008600
         SPACE 2                                                        00008700
FISTART  EQU   *                   FIAS START UP                        00008800
         C     R3,=F'80000000'     WAS THERE ANY RECORD PASSED?         00008900
         BE    EXIT                  NO- GET OUT                        00009000
         LA    R3,69(R3)           GET ADDRESS OF COMPRESSED MASTER     00009100
         L     R4,0(R3)            SAVE COMPRESSED MASTER ADDRESS       00009200
         XC    FWORD,FWORD         CLEAR FULL WORD            0262563   00009210
         MVC   FWORD+2(2),MSTLNG   MOVE LENGTH TO FULL WORD   0262563   00009220
* THE FOLLOWING STATEMENT(S) MODIFIED BY                      0262563   00009299
         EXEC  CICS GETMAIN FLENGTH (FWORD) SET (R8) INITIMG (X'00')    00009300
         ST    R8,0(R3)            SAVE EXPANDED MASTER ADDRESS         00009400
         LR    R3,R4               POINT TO COMPRESSED MASTER           00009500
         B     FIDONE                                                   00009600
         SPACE 2                                                        00009700
TSSTART  EQU   *                   TS START UP                          00009800
         EXEC CICS HANDLE CONDITION ERROR (ECICS000)                    00009900
         MVC   WMSMGPID,=C'FIIMMSEP' PROGRAM NAME FOR ERRORS            00010000
         MVC   WSEYECAT,TSEYECAT   SET WORK TS EYECATCHER               00010100
         CLC   EIBCALEN,=H'0000'   COMMAREA LENGTH?                     00010200
         BE    ERRCOMM                  NO, ERROR EXIT                  00010300
         MVC   TSECC,0(R3)         MOVE COMMAREA TO W/S                 00010400
         CLC   ECCRECAD,=X'00000000' RECORD ADDRESS?                    00010500
         BE    ERRMAST                  NO, ERROR EXIT                  00010600
         CLC   ECCMAXLG,MSTLNG     EQUAL TO MASTER LENGTH?              00010700
         BNE   ERRLEN                   NO, ERROR EXIT                  00010800
         CLC   ECCEXPAD,=X'00000000' GETMAIN DONE ALREADY?              00010900
         BE    TSGETM                   NO, GO TO TSGETM                00011000
         SR    R6,R6               ZERO FROM LENGTH & PAD               00011100
         LH    R2,MSTLNG           LOAD TO   LENGTH                     00011200
         L     R8,ECCEXPAD         LOAD TO   ADDR                       00011300
         L     R3,ECCRECAD         LOAD FROM ADDR                       00011400
         B     FIDONE                                                   00011500
TSGETM   EQU   *                                                        00011600
         L     R4,ECCRECAD         LOAD FROM ADDR                       00011700
         LR    R7,R4               SAVE FROM ADDR                       00011800
         XC    FWORD,FWORD         CLEAR FULL WORD            0262563   00011850
         MVC   FWORD+2(2),ECCMAXLG MOVE MAX LEN FOR GETMAIN   0262563   00011900
* THE FOLLOWING STATEMENT(S) MODIFIED BY                      0262563   00011999
         EXEC  CICS  GETMAIN FLENGTH (FWORD) SET (R8) INITIMG (X'00')   00012000
         ST    R8,ECCEXPAD         STORE TO   ADDR                      00012100
         ST    R8,16(R3)           STORE TO   ADDR                      00012200
         MVC   20(2,R3),ECCMAXLG   STORE TO   LENGTH                    00012300
         L     R3,ECCRECAD         LOAD FROM ADDR                       00012400
FIDONE   EQU   *                                                        00012500
         AGO   .GEN2                                                    00012600
         SPACE 2                                                        00012700
.GEN1    ANOP                                                           00012800
#DSALEN  EQU   *-CEEDSA            LENGTH OF DSA              0266888   00012805
         SPACE 1                                                        00012900
IMMSEP CEEENTRY ,                                                      X00012905
               PPA=PPA,                                                X00012910
               AUTO=#DSALEN,                                           X00012915
               MAIN=NO,                                                X00012920
               NAB=NO,                                                 X00012925
               PARMREG=1,                                              X00012930
               BASE=9                                                   00012935
         XR    R15,R15                                        0266888   00012940
         B     INIT000                                        0266888   00012945
         USING CEECAA,R12                                     0266888   00012950
* ------------------------------------------------------------------- * 00012955
*                  PROLOG AREA                                        * 00012960
* ------------------------------------------------------------------- * 00012965
PPA      CEEPPA ,                                                      X00012970
               LIBRARY=NO,                                             X00013000
               PPA2=YES,                                               X00013005
               EXTPROC=YES,                                            X00013010
               TSTAMP=YES,                                             X00013015
               PEP=YES,                                                X00013145
               INSTOP=YES,                                             X00013150
               EPNAME=IMMSEP,                                          X00013155
               VER=01,                                                 X00013160
               REL=01,                                                 X00013165
               MOD=00,                                                 X00013170
               DSA=YES                                                  00013175
         SPACE 2                                              0266888   00013180
         LTORG                                                0266888   00013185
INIT000  EQU   *                                              0266888   00013190
         ST    R1,PGMPLIST                                    0266888   00013195
         L     R8,0(1)             STORE PARAMETER 1 ADDRESS            00013200
         L     R3,4(1)             STORE PARAMETER 2 ADDRESS            00013300
.GEN2    ANOP                                                           00013400
         USING MASTEROT,R8,R11,R15,R14  SET UP DUMMY SECTION  9915845   00013500
         USING MASTERIN,R3         SET UP DUMMY SECTION                 00013600
         LA    R11,2048(R8)        EST ADDRESSABILITY         9915845   00013700
         LA    R11,2048(R11)       SECTION FOR MASTER OUT     9915845   00013800
         LA    R15,2048(R11)       ESTABLISH ADDRESSABILITY   9915845   00013830
         LA    R15,2048(R15)       SECTION FOR MASTER OUT     9915845   00013860
         LA    R14,2048(R15)       ESTABLISH ADDRESSABILITY   9915845   00013870
         LA    R14,2048(R14)       SECTION FOR MASTER OUT     9915845   00013880
         SR    R7,R7               ESTABLISH REGISTERS TO BE USED TO    00013900
         SR    R10,R10             POINT TO SEGMENTS IN I/O FILES       00014000
* ** CHECK FOR A BATCH HEADER RECORD  ********************              00014100
         CLI   26(R3),C'0'         CHECK FOR BANK HEADER       1004875  00014200
         BNE   MOVEMAST            NO, GO MOVE MASTER RECORD            00014250
         MVC   OFIXED(84),0(R3)    MOVE BANK HEADER                     00014300
         B     EXIT                                                     00014350
         SPACE 2                                                        00014400
*---------------------------------------------------------------------- 00014600
*        MOVED FIXED PORTION OF RECORD TO OUTPUT AREA                   00014700
*---------------------------------------------------------------------- 00014800
MOVEMAST EQU *                                                          00014900
         LR    R4,R8               SET R4 WITH ADDR OF 'TO'   9915845   00014920
         LA    R5,3000             SET LENGTH OF FIXED DATA   9915845   00015000
         LR    R6,R3               SET R6 WITH ADDRESS OF 'FROM' AREA   00015100
         LA    R7,3000             SET LENGTH OF FIXED DATA   9915845   00015200
         MVCL  R4,R6               MAKE MOVE OF FIXED AREA              00015300
*---------------------------------------------------------------------- 00015400
*        CLEAR TRAILER AREA                                    1105504  00015500
*---------------------------------------------------------------------- 00015600
         LA    R5,4000                 MOVE LENGTH TO MOVE     1004875  00017300
         L     R7,BLANK                SET UP FILL CHARACTER   1105504  00017400
         MVCL  R4,R6                   MOVE FROM R6 TO R4      1004875  00017500
         LA    R5,4000                 MOVE LENGTH TO MOVE     1105504  00017600
         L     R7,BLANK                SET UP FILL CHARACTER  9915845   00017700
         MVCL  R4,R6                   MOVE FROM R6 TO R4      1105504  00017800
         LA    R5,3000                 MOVE LENGTH TO MOVE    9915845   00018000
         L     R7,BLANK                SET UP FILL CHARACTER   1105504  00018100
         MVCL  R4,R6                   MOVE FROM R6 TO R4      1105504  00018200
         LA    R5,2626                 MOVE LENGTH TO MOVE    0930011   00018210
* THE FOLLOWING STATEMENT(S) MODIFIED BY                      0617360   00018219
         L     R7,BLANK                SET UP FILL CHARACTER   1105504  00018220
* THE FOLLOWING STATEMENT(S) MODIFIED BY                      0617360   00018229
         MVCL  R4,R6                   MOVE FROM R6 TO R4      1105504  00018230
*---------------------------------------------------------------------- 00018300
*        EXPAND NAME AND ADDRESS SECTION                                00018400
*---------------------------------------------------------------------- 00018500
NASTART  EQU   *                   FIAS/BATCH NAME/ADDR PROC            00018600
         LA    R4,3000(R3)         ESTABLISH POINTER FOR INPUT9915845   00018700
         CP    INADDTR,=P'0'       IS THERE A NAME ADDRESS TRAILER      00018800
         BE    DOSOCSEC            NONE - GO EXPAND OTHER TRAILERS      00018900
         MVC   ONADD(3),INADD      MOVE NBR OF LINES & CTRY 1 9915845   00019000
         LA    R4,3(R4)            TO OUTPUT - BUMP 3 BYTES   9915845   00019100
         AH    R10,=H'3'           KEEP COUNT ON BYTES MOVED  9915845   00019200
         LA    R5,ONADD+3          POINT TO OUTPUT FOR N/A 1  9915845   00019300
         ZAP   DBLWORD,INADD(1)    LOAD NUMBER OF LINES ON ADDRESS ONE  00019400
         MVC   HALFWORD,=H'42'     SET UP LENGTH OF ONE OCCURS          00019500
         BAL   6,EXPAND            GO EXPAND NAME ADDRESS TRAILER ONE   00019600
         CP    INADDTR,=P'2'       IS THERE A NAME ADDRESS TRAILER TWO  00019700
         BNE   DOSOCSEC            NONE - GO EXPAND OTHER TRAILERS      00019800
         MVC   ONADD+389(3),0(R4)  MOVE NBR OF LINES & CTRY 2 9915845   00019900
         LA    R4,3(R4)            TRL TWO TO OUTPUT - BUMP 3 9915845   00020000
         AH    R10,=H'3'           KEEP COUNT ON BYTES MOVED  9915845   00020100
         LA    R5,ONADD+392        POINT TO NAME ADDESSS TRLR 9915845   00020200
         ZAP   DBLWORD,ONADD+389(1)  LD NBR OF LINES IN ADDR  9915845   00020300
         MVC   HALFWORD,=H'42'     SET UP LENGTH ON ONE OCCURS          00020400
         BAL   6,EXPAND            GO EXPAND NAME ADDRESS TRAILER TWO   00020500
         B     DOSOCSEC            BRANCH TO EXPAND REMAINING TRAILERS  00020600
*---------------------------------------------------------------------- 00020700
*        EXPAND REMAINING TRAILERS                                      00020800
*---------------------------------------------------------------------- 00020900
DOSOCSEC CLI   ISOCSECT,C'0'       IS SOCIAL SECURITY TRAILER PRESENT   00021000
         BE    A1                  NO  -  BRANCH AROUND                 00021100
         MVC   OSOCSEC,0(R4)       MOVE SOCIAL SECURITY TRAILER         00021200
         LA    R10,135(R10)        ADD TO RECORD LENGTH       9915845   00021300
         LA    R4,135(R4)          POINT TO NEXT TRAILER      9915845   00021400
         SPACE 1                                                        00021500
A1       CLI   ILOANTR,C'0'        IS LOAN TRAILER PRESENT    9915845   00022800
         BE    A2                  NO, BRANCH                 9915845   00022900
         LA    R5,OLOAN                                                 00023000
         PACK  DBLWORD,ILOANTR                                          00023100
         MVC   HALFWORD,=H'1500'                              9915845   00023200
         BAL   R6,EXPAND           GO EXPAND LOAN TRAILER               00023300
         SPACE 1                                                        00023400
A2       CLI   IMKTTR,C'0'         IS MARKETING TRLR PRESENT  9915845   00023500
         BE    A3                  NO, BRANCH                 9915845   00023600
         LA    R5,OMKT                                                  00023700
         PACK  DBLWORD,IMKTTR                                           00023800
         MVC   HALFWORD,=H'360'                               9915845   00023900
         BAL   R6,EXPAND           GO EXPAND MARKETING TRAILER          00024000
         SPACE 1                                                        00024100
A3       CP    ITRNAFFT,=P'0'      IS TRANSFER TRLR PRESENT   9915845   00024200
         BE    A4                  NO, BRANCH                 9915845   00024300
         LA    R5,OTRNAFF                                               00024400
         ZAP   DBLWORD,ITRNAFFT                                         00024500
         MVC   HALFWORD,=H'85'                                9915845   00024600
         BAL   R6,EXPAND           GO EXPAND TRANSFER/AFFILATE TRAILER  00024700
         SPACE 1                                                        00024800
A4       CLI   IODNSFTR,C'0'       IS OD/NSF TRAILER PRESENT  9915845   00024900
         BE    A5                  NO, BRANCH                 9915845   00025000
         MVC   OODNSF,0(R4)        MOVE OD/NSF TRAILER                  00025100
         LA    R10,130(R10)        ADD TO RECORD LENGTH       9915845   00025200
         LA    R4,130(R4)          POINT TO NEXT TRAILER      9915845   00025300
         SPACE 1                                                        00025400
A5       CLI   ISVTR,C'0'          IS SAVINGS TRAILER PRESENT 9915845   00025500
         BE    A6                  NO, BRANCH                 9915845   00025600
         LA    R5,OSV                                                   00025700
         PACK  DBLWORD,ISVTR                                            00025800
         MVC   HALFWORD,=H'660'                               9915845   00025900
         BAL   R6,EXPAND           GO EXPAND SAVINGS TRAILER            00026000
         SPACE 1                                                        00026100
A6       CLI   IFLTR,C'0'                                     9915845   00026200
         BE    A7                                             9915845   00026300
         MVC   OFL,0(R4)           MOVE FLOAT TRAILER                   00026400
         LA    R10,56(R10)         ADD TO RECORD LENGTH       9915845   00026500
         LA    R4,56(R4)           POINT TO NEXT TRAILER      9915845   00026600
         SPACE 1                                                        00026700
A7       CLI   IUNCLTR,C'0'        IS UNCOLLECTED TRLR PRESENT9915845   00026800
         BE    A8                  NO, BRANCH                 9915845   00026900
         MVC   OUNCL,0(R4)         MOVE UNCOLLECTED TRAILER             00027000
         LA    R10,96(R10)         ADD TO RECORD LENGTH       9915845   00027100
         LA    R4,96(R4)           POINT TO NEXT TRAILER IN   9915845   00027200
         SPACE 1                                                        00027300
A8       CP    ICMBTR,=P'0'        IS COMBINED TRLR PRESENT   9915845   00027400
         BE    A9                  NO, BRANCH                 9915845   00027500
         LA    R5,OCMB                                                  00027600
         ZAP   DBLWORD,ICMBTR                                           00027700
         MVC   HALFWORD,=H'46'                                          00027800
         BAL   R6,EXPAND           GO EXPAND COMBINED TRAILER           00027900
         SPACE 1                                                        00028000
A9       CLI   ITGTR,C'0'          IS TARGET TRAILER PRESENT  9915845   00028100
         BE    A10                 NO, BRANCH                 9915845   00028200
         MVC   OTG,0(R4)           MOVE TARGET TRAILER                  00028300
         LA    R10,125(R10)        ADD TO RECORD LENGTH       9915845   00028400
         LA    R4,125(R4)          POINT TO NEXT TRAILER      9915845   00028500
         SPACE 1                                                        00028600
A10      CLI   ILMTTR,C'0'         IS LIMIT TRAILER PRESENT   9915845   00028700
         BE    A11                 NO, BRANCH                 9915845   00028800
         MVC   OLMT,0(R4)          MOVE LIMIT TRAILER                   00028900
         LA    R10,75(R10)         ADD TO RECORD LENGTH       9915845   00029000
         LA    R4,75(R4)           POINT TO NEXT TRAILER      9915845   00029100
         SPACE 1                                                        00029200
A11      CLI   ITAXTR,C'0'         IS TAX TRAILER PRESENT     9915845   00029300
         BE    A12                 NO, BRANCH                 9915845   00029400
         MVC   OTAX,0(R4)          MOVE TAX TRAILER                     00029500
         LA    R10,120(R10)        ADD TO RECORD LENGTH       9915845   00029600
         LA    R4,120(R4)          POINT TO NEXT TRAILER      9915845   00029700
         SPACE 1                                                        00029800
A12      CLI   IODACRTR,C'0'       IS OD/ACCRUAL TRLR PRESENT 9915845   00029900
         BE    A13                 NO, BRANCH                 9915845   00030000
         LA    R5,OODACR                                      9915845   00030010
         PACK  DBLWORD,IODACRTR                               9915845   00030100
         MVC   HALFWORD,=H'425'                               9915845   00030200
         BAL   R6,EXPAND           GO EXPAND OD ACCR TRAILER  9915845   00030300
         SPACE 1                                                        00030400
A13      CLI   IINFOTR,C'0'        IS INFO TRAILER PRESENT    9915845   00030500
         BE    A14                 NO, BRANCH                 9915845   00030600
         MVC   OINFO,0(R4)         MOVE INFORMATION TRAILER             00030700
         LA    R10,60(R10)         ADD TO RECORD LENGTH                 00030800
         LA    R4,60(R4)           PT TO NXT TRLR IN INPUT AREA         00030900
         SPACE 1                                                        00031700
A14      CLI   IDEPTR,C'0'         IS DEPOSIT TRAILER PRESENT 9915845   00031800
         BE    A15                 NO, BRANCH                 9915845   00031900
         MVC   ODEP,0(R4)          MOVE DEPOSIT TRAILER                 00032000
         LA    R10,130(R10)        ADD TO RECORD LENGTH       9915845   00032100
         LA    R4,130(R4)          POINT TO NEXT TRAILER      9915845   00032200
         SPACE 1                                                        00032300
A15      CLI   IKITETR,C'0'        IS KITING TRAILER PRESENT  9915845   00032400
         BE    A16                 NO, BRANCH                 9915845   00032500
         LA    R5,OKITE                                       9915845   00032510
         PACK  DBLWORD,IKITETR                                9915845   00032600
         MVC   HALFWORD,=H'275'                               9915845   00032700
         BAL   R6,EXPAND           GO EXPAND KITING TRAILER   9915845   00032800
         SPACE 1                                                        00032900
A16      CLI   IEFATR,C'0'         IS EFA TRAILER PRESENT?    9915845   00033000
         BE    A17                 NO, BRANCH                 9915845   00033100
         MVC   OEFA,0(R4)          MOVE EFA TRAILER                     00033200
         LA    R10,133(R10)        ADD TO RECORD LENGTH       9915845   00033300
         LA    R4,133(R4)          POINT TO NEXT TRAILER INPUT          00033400
         SPACE 1                                                        00033500
A17      CLI   ICASHTR,C'0'        IS CASH TRAILER PRESENT?   9915845   00033600
         BE    A18                 NO, BRANCH                 9915845   00033700
         MVC   OCASH,0(R4)         MOVE CASH AVL TRAILER                00033800
         LA    R10,204(R10)        ADD TO RECORD LENGTH       9915845   00033900
         LA    R4,204(R4)          POINT TO NEXT TRAILER INPUT9915845   00034000
         SPACE 1                                                        00034100
A18      CLI   IINVTR,C'0'         IS INVESTMENT TRLR PRESENT?9915845   00034110
         BE    A19                 NO, BRANCH                 9915845   00034115
         MVC   OINV,0(R4)          MOVE INVESTMENT TRAILER     1004554  00034120
         LA    R10,120(R10)        ADD TO RECORD LENGTH       9915845   00034125
         LA    R4,120(R4)          POINT TO NEXT TRAILER INPUT9915845   00034130
         SPACE 1                                               1004875  00034132
A19      CLI   IRATETR,C'0'        IS RATE TRLR PRESENT?      9915845   00034134
         BE    A27                 NO, BRANCH                 2012254   00034136
         MVC   ORATEFL,0(R4)       MOVE RATE TRAILER FLAGS     1004875  00034138
         LA    R10,7(R10)          ADD TO RECORD LENGTH       2012254   00034140
         LA    R4,7(R4)            POINT TO NEXT TRAILER INPUT2012254   00034142
         SPACE 1                                              9915845   00034144
A20      CLI   ORATEDDA,C'N'       IS DDA RATE TRLR PRESENT?  9915845   00034146
         BE    A21                 NO, BRANCH                 9915845   00034148
         LA    R5,ORDDA                                       9915845   00034150
         PACK  DBLWORD,=C'1'       ONE DDA RATE TRAILER       9915845   00034152
         MVC   HALFWORD,=H'421'                               9915845   00034154
         BAL   R6,EXPAND           GO EXPAND COMBINED TRAILER 9915845   00034156
         SPACE 1                                              9915845   00034158
A21      CLI   ORATESAV,C'N'       IS SAV RATE TRLR PRESENT?  9915845   00034160
         BE    A22                 NO, BRANCH                 9915845   00034162
         MVC   ORSAV,0(R4)         MOVE SAV RATE TRAILER FLAG 2012254   00034163
         LA    R10,43(R10)         ADD TO RECORD LENGTH       2012254   00034164
         LA    R4,43(R4)           POINT TO NEXT TRAILER INPUT2012254   00034165
         SPACE 1                                              2012254   00034166
A22      CLI   ORATEOD,C'N'        IS OD RATE TRLR PRESENT?   2012254   00034167
         BE    A23                 NO, BRANCH                 2012254   00034168
         LA    R5,OROD                                        2012254   00034169
         PACK  DBLWORD,=C'1'       ONE OD RATE TRAILER        2012254   00034170
         MVC   HALFWORD,=H'421'                               2012254   00034171
         BAL   R6,EXPAND           GO EXPAND OD RATE TRAILER  2012254   00034172
         SPACE 1                                              2012254   00034173
A23      CLI   ORATELN,C'N'        IS LN RATE TRLR PRESENT?   2012254   00034174
         BE    A24                 NO, BRANCH                 2012254   00034175
         LA    R5,ORLN                                        2012254   00034176
         PACK  DBLWORD,=C'1'       ONE LOAN RATE TRAILER      2012254   00034178
         MVC   HALFWORD,=H'288'                               2012254   00034179
         BAL   R6,EXPAND           GO EXPAND LOAN RATE TRLR   2012254   00034180
         SPACE 1                                              2012254   00034181
A24      CLI   ORATETFD,C'N'       IS FED TAX RATE TRLR?      2012254   00034182
         BE    A25                 NO, BRANCH                 2012254   00034183
         MVC   ORFEDTX,0(R4)       MOVE FED TAX RATE TRLR     2012254   00034184
         LA    R10,118(R10)        ADD TO RECORD LENGTH       2012254   00034185
         LA    R4,118(R4)          POINT TO NEXT TRAILER INPUT2012254   00034186
         SPACE 1                                              2012254   00034187
A25      CLI   ORATETST,C'N'       IS STATE TAX RATE TRLR?    2012254   00034188
         BE    A26                 NO, BRANCH                 2012254   00034189
         MVC   ORSTTX,0(R4)        MOVE STATE TAX RATE TRLR   2012254   00034190
         LA    R10,118(R10)        ADD TO RECORD LENGTH       2012254   00034191
         LA    R4,118(R4)          POINT TO NEXT TRAILER INPUT2012254   00034192
         SPACE 1                                              2012254   00034193
A26      CLI   ORATETLC,C'N'       IS LOCAL TAX RATE TRLR?    2012254   00034194
         BE    A27                 NO, BRANCH                 2012254   00034195
         MVC   ORLOCTX,0(R4)       MOVE LOCAL TAX RATE TRLR   2012254   00034196
         LA    R10,118(R10)        ADD TO RECORD LENGTH       2012254   00034197
         LA    R4,118(R4)          POINT TO NEXT TRAILER INPUT2012254   00034198
         SPACE 1                                              2012254   00034199
A27      CLI   IEXTTR,C'0'         IS EXT SC TRAILER PRESENT  2012254   00034200
         BE    A28                 NO, BRANCH                 2012254   00034202
         LA    R5,OEXTSC                                      9915845   00034204
         PACK  DBLWORD,IEXTTR                                 9915845   00034206
         MVC   HALFWORD,=H'2000'                              9915845   00034208
         BAL   R6,EXPAND           GO EXPAND EXT TRAILER      9915845   00034210
         SPACE 1                                              9915845   00034212
A28      CLI   IAYTR,C'0'          IS ALT YR TRLR PRESENT?    2012254   00034214
         BE    A29                 NO, BRANCH                 2012254   00034215
         MVC   OALTYR,0(R4)        MOVE ALT YR TRLR           9915845   00034216
         LA    R10,240(R10)        ADD TO RECORD LENGTH       9915845   00034217
         LA    R4,240(R4)          POINT TO NEXT TRAILER      9915845   00034218
         SPACE 1                                              9915845   00034219
A29      CLI   IPLTR,C'0'          IS PLAN TRLR PRESENT?      0617360   00034220
         BE    A30                 NO, BRANCH                 0617360   00034221
         LA    R5,OPLN             POSITION TO OUTPUT TRLR    0617360   00034222
         PACK  DBLWORD,=C'1'       SET NUMBER OF SEGMENTS     0617360   00034223
         MVC   HALFWORD,=H'626'    SET LGTH FIXED SEG (626)   0617360   00034224
         BAL   R6,EXPAND           GO EXPAND                  0617360   00034225
         CP    OPLN+624(2),=PL1'+1' ANY TRAILER OCCURS?       0617360   00034226
         BL    A30                 NO, GO TO DO AFF           0617360   00034227
         LA    R5,OPLN+626         POINT TO TRAILER OCCURS    0617360   00034228
         ZAP   DBLWORD,OPLN+624(2)  MV NO OF OCCURS           0617360   00034229
         MVC   HALFWORD,=H'15'     SET LENGTH OF AN OCCURS    0617360   00034230
         BAL   R6,EXPAND           GO EXPAND                  0617360   00034231
         SPACE 1                                              0617360   00034232
A30      CLI   IDTCTTR,C'0'        DATA CENTER TRLR PRESENT?  0617360   00034233
         BE    A31                 NO, BRANCH                 0617360   00034234
         LA    R5,ODTCT                                       0617360   00034235
         ZAP   DBLWORD,0(2,R4)     DETERMINE LGTH DC TRLR     0617360   00034236
         ST    R6,RETADDR          SAVE RETURN ADDR           0617360   00034237
         CVB   R6,DBLWORD                                     0617360   00034238
         ST    R6,FULLWORD                                    0617360   00034239
         L     R6,RETADDR          LOAD RETURN ADDR           9915845   00034240
         MVC   HALFWORD,FULLWORD+2                            9915845   00034241
         PACK  DBLWORD,IDTCTTR                                9915845   00034243
         BAL   R6,EXPAND           GO EXPAND DC TRLR          9915845   00034245
         SPACE 1                                              9915845   00034246
*-------------------------------------------------------------9915845   00034248
*        CHECK EXPANDED LENGTH TO MAKE SURE EXPANSION WAS NOT 9915845   00034300
*        GREATER THAN LENGTH PASSED INDICATED IT SHOULD BE.   9915845   00034400
*-------------------------------------------------------------9915845   00034500
* THE FOLLOWING STATEMENT(S) MODIFIED BY                      0617360   00034599
A31      AH    R10,=H'3000'        ADD IN FIXED PORTION OF REC2012254   00034600
         MVC   FULLWORD,0(R8)                                 9915845   00034700
         MVC   RECLNG,FULLWORD                                          00034800
         MVC   BINZEROS,=X'0000'                                        00034900
         L     R5,RECLENGH                                    0266888   00035000
* THE FOLLOWING STATEMENT(S) MODIFIED BY                      0266888   00035099
         CR    R5,R10              CHECK LENGTH IF OK GO TO EXIT        00035100
         BE    EXIT                                                     00035200
         B     ERR                 FORCE DUMP                           00035300
         SPACE 1                                                        00035400
EXIT     EQU   *                                                        00035500
         AIF   (&CICS).CICSRET                                          00035600
         XR    R15,R15                                        ~~~0187   00035610
         CEETERM RC=(15)           RETURN                     0266888   00035700
         SPACE 3                                                        00035800
         AGO   .DATA                                                    00035900
.CICSRET ANOP                                                           00036000
*---------------------------------------------------------------------- 00036100
*        THE NAME AND ADDRESS AREAS MUST BE EXPANDED FOR TS,            00036200
*        BASED ON THE PRESENCE OR ABSENCE OF THE TS 'EYECATCHER'        00036300
*        A DETERMINATION IS MADE WHETHER THE CALL CAME FROM TS OR       00036400
*        FIAS. IF COMING FROM TS THE NAME AND ADDRESS AREA MUST BE      00036500
*        FULLY EXPANDED FOR COMPATIBILITY WITH DATA DICTIONARY.         00036600
*---------------------------------------------------------------------- 00036700
*                                                                       00036800
         CLC   TSEYECAT,WSEYECAT   IS TS EYE CATCHER PRESENT            00036900
         MVC   WSEYECAT,SPACES     CLEAR WORK EYE CATCHER               00037000
         BNE   NAEND               NO - BYPASS TS NAME/ADDR PROCESSING  00037100
         SPACE 1                                                        00037200
*---------------------------------------------------------------------- 00037300
*        THE FOLLOWING NAME/ADDRESS EXPAND CODE IS USED ONLY BY TS      00037400
*---------------------------------------------------------------------- 00037500
*                                                                       00037600
*---------------------------------------------------------------------- 00037700
*        CLEAR OUTPUT NAME AND ADDRESS AREA                             00037800
*---------------------------------------------------------------------- 00037900
         MVI   ONADD,X'00'         MOVE BINARY ZEROS TO N/A             00038000
         MVC   ONADD+1(249),ONADD  OUTPUT AREA                          00038100
         MVC   ONADD+250(250),ONADD                                     00038200
         MVC   ONADD+500(250),ONADD                           9915845   00038300
         MVC   ONADD+750(29),ONADD                            9915845   00038310
TSNADDR  EQU   *                                                        00038400
         SPACE 1                                                        00038500
         LA    R7,2                NUMBER OF TIMES TO LOOP              00038600
         LA    R4,3000(R3)         ESTABLISH POINTER FOR INPUT9915845   00038700
         CP    ONADDTR,=P'0'       IS THERE A NAME ADDRESS TRAILER      00038800
         BE    NAEND               NONE - GO EXPAND OTHER TR            00038900
         MVC   ONADD(3),0(R4)      MOVE # OF LINES IN N/A 1   9915845   00039000
         LA    R4,3(R4)            TO OUTPUT - BUMP INPUT PTR 9915845   00039100
         LA    R9,ONADD+3          POINT TO OUTPUT FOR N/A    9915845   00039200
         USING NAAREA,R5           SET BAR FOR NEW NA AREA              00039300
         LA    R5,ONADD+3          POINT TO OUTPUT FOR N/A    9915845   00039400
         ZAP   DBLWORD,ONADD(1)    LOAD NUMBER OF LINES ON ADDRESS ONE  00039500
NACHK    EQU   *                                                        00039600
         CVB   R6,DBLWORD          SET NO OF N/A LINES AS CNTR          00039700
NALINE   EQU   *                                                        00039800
         CLI   0(R4),C'1'          Q.  NAME LINE                        00039900
         BE    NAMOVE              ... YES, MOVE NAME LINE              00040000
         CLI   0(R4),C'2'          Q.  ADDRESS LINE                     00040100
         BNE   NACTYST             ... NO, CHECK FOR CTY/STATE          00040200
NAMOVE   EQU   *                                                        00040300
         MVC   0(L'NALINE1,R9),0(R4)   MOVE N/A LINE                    00040400
         LA    R4,L'NALINE1(R4)    POINT TO NEXT N/A LINE               00040500
         LA    R9,L'NALINE1(R9)    POINT TO NEXT OUTPUT LINE            00040600
         BCT   R6,NALINE           LOOP UNTIL END OF N/A OR             00040700
         B     NABMP1              END OF N/A TRAILER                   00040800
NACTYST  EQU   *                                                        00040900
         CLI   0(R4),C'3'          Q.  NAME LINE                        00041000
         BNE   NASPECI             ... NO, CHECK FOR SPEC INST          00041100
         MVC   NALINE6,0(R4)       MOVE CITY/STATE LINE       9915868   00041200
         B     NABMP               BUMP TO NEXT LINE                    00041300
NASPECI  EQU   *                                                        00041400
         CLI   0(R4),C'4'          Q.  SPECIAL INSTRUCTION              00041500
         BNE   NAMISC              ... NO, CHECK FOR MISC INFO          00041600
         MVC   NALINE7,0(R4)       MOVE SPECIAL INST LINE     9915868   00041700
         B     NABMP               BUMP TO NEXT LINE                    00041800
NAMISC   EQU   *                                                        00041900
         MVC   NALINE8,0(R4)       MOVE MISC LINE             9915868   00042000
         B     NABMP               BUMP TO NEXT LINE                    00042100
NABMP    EQU   *                                                        00042200
         LA    R4,L'NALINE1(R4)    POINT TO NEXT N/A LINE               00042300
         BCT   R6,NACTYST          LOOP UNTIL END OF N/A                00042400
NABMP1   BCT   R7,NACKTR2          LOOP FOR 2ND TRAILER                 00042500
         B     NAEND               END OF ALL TRAILERS                  00042600
NACKTR2  EQU   *                                                        00042700
         CP    ONADDTR,=P'2'       Q.  SECOND N/A TRAILER               00042800
         BNE   NAEND               ...NO, END EXPAND                    00042900
NABMPA   EQU   *                                                        00043000
         MVC   ONADD+389(3),0(R4)  MOVE NUMBER OF LINES IN N/A9915845   00043100
         LA    R4,3(R4)            TO OUTPUT - BUMP INPUT PTR 9915845   00043200
         LA    R9,ONADD+392        POINT TO N/A TRAILER 2     9915845   00043300
         LA    R5,ONADD+392        POINT TO N/A TRAILER 2     9915845   00043400
         ZAP   DBLWORD,ONADD+389(1) LOAD # OF LINE IN ADDR 2  9915845   00043500
         B     NACHK               PROCESS TRAILER 2                    00043600
         DROP  R5                  DROP R5                              00043700
NAEND    EQU   *                                                        00043800
*---------------------------------------------------------------------- 00043900
*        CICS RETURN                                                    00044000
*---------------------------------------------------------------------- 00044100
         EXEC  CICS RETURN                                              00044200
*                                                                       00044300
ERRCOMM  EQU   *                   NO COMMAREA ADDRESS                  00044400
         MVC   WMSAPPL(6),=C'TS0316'                                    00044500
         B     ERROR                                                    00044600
ERRMAST  EQU   *                   NO MASTER ADDRESS                    00044700
         MVC   WMSAPPL(6),=C'TS0318'                                    00044800
         B     ERROR                                                    00044900
ERRLEN   EQU   *                   INCORRECT MAX EXPAND LENGTH          00045000
         MVC   WMSAPPL(6),=C'TS0317'                                    00045100
ERROR    EQU   *                                                        00045200
         MVI   WMSREQCD,C'1'                                            00045300
         MVC   WMSANCA,ECCNCADR                                         00045400
         BAL   R15,ERRTN000                                             00045500
         BAL   R15,FATAL000                                             00045600
         B     ABEND000                                                 00045700
*                                                                       00045800
TSEYECAT DC    CL8'** ECC**'                                            00046000
MSTLNG   DC    H'16630'          ++MAXIMUM RECORD LENGTH      0930011   00046200
SPACES   DC    40C' '                                                   00046300
*                                                                       00046400
         COPY TSTS4PMS                                                  00046500
*                                                                       00046600
         COPY TSTS4PCM                                                  00046700
*                                                                       00046800
.DATA    ANOP                                                           00046900
         SPACE 2                                                        00047000
*---------------------------------------------------------------------- 00047100
*      THIS ROUTINE WILL EXPAND THE TRAILER AREA PASSED INTO THE        00047200
*      WORK AREA FOR THAT APPROPRIATE TRAILER.                          00047300
*      DBLWORD--NUMBER OF OCCURRENCES OF TRAILER                        00047400
*      HALFWORD--LENGTH OF EACH OCCURRENCE                              00047500
*      LENGTH--MAXIMUM LENGTH OF ALL OCCURRENCES                        00047600
*---------------------------------------------------------------------- 00047700
         SPACE 1                                                        00047800
EXPAND   EQU   *                                                        00047900
         ST    R6,RETADDR          SAVE RETURN ADDR                     00048000
         CVB   R7,DBLWORD          CONVERT THE TRAILER OCCURS TO BINARY 00048100
         MH    R7,HALFWORD         MULTIPLY BY OCCURS SEGMENT LENGTH    00048200
         LR    R6,R7               SET R6 TO SAME LENGTH AS R7          00048300
         C     R6,=F'257'          IS RESULT LESS THAN ONE MOVE IN LGTH 00048400
         BL    MOV1                YES--GO MAKE ONE MOVE                00048500
         C     R6,=F'513'          IS RESULT LESS THAN THR MOVES IN LGH 00048600
         BL    MOV2                YES--TWO MOVES REQUIRED              00048700
         C     R6,=F'769'          IS RESULT LESS THAN FOUR MOVES       00048800
         BL    MOV3                YES--THREE MOVES REQUIRED            00048900
         C     R6,=F'1025'         IS RESULT LESS THAN FIVE MOVES       00049000
         BL    MOV4                YES--FOUR MOVES REQUIRED             00049100
         C     R6,=F'1281'         IS RESULT LESS THAN 6 MOVES9915845   00049110
         BL    MOV5                YES--FIVE MOVES REQUIRED   9915845   00049130
         C     R6,=F'1537'         IS RESULT LESS THAN 7 MOVES9915845   00049140
         BL    MOV6                YES--SIX MOVES REQUIRED    9915845   00049150
         C     R6,=F'1793'         IS RESULT LESS THAN 8 MOVES9915845   00049160
         BL    MOV7                YES--SEVEN MOVES REQUIRED  9915845   00049170
         C     R6,=F'2049'         IS RESULT LESS THAN 9 MOVES9915845   00049180
         BL    MOV8                YES--EIGHT MOVES REQUIRED  9915845   00049190
         B     MOV9                GO MAKE NINE MOVES         9915845   00049200
MOV1     SH    R6,=H'1'                                                 00049300
         EX    R6,MOVE             ONLY ONE MOVE NECESSARY - GO MOVE    00049400
         AR    R10,R7                                                   00049500
         AR    R4,R7               INCREASE RECORD COUNT BY SEG LENGTH  00049600
GETOUT   EQU   *                   LOCATION IN MASTER FOR OUTPUT TRAILR 00049700
         L     R6,RETADDR          LOAD RETURN ADDR                     00049800
         BR    R6                                                       00049900
         SPACE 2                                                        00050000
*---------------------------------------------------------------------- 00050100
*        THE MOVE LENGTH CALCULATED MUST                                00050200
*        ALWAYS BE ONE LESS THAN ACTUAL LENGTH                          00050300
*        SO ADJUSTMENT HAS BEEN FORCED IN MOV                           00050400
*        ROUTINES.                                                      00050500
*---------------------------------------------------------------------- 00050600
         SPACE 1                                                        00050700
MOV9     EQU   *                                              9915845   00050710
         S     R6,=F'2049'         SET UP R6 WITH PROPER LGTH 9915845   00050720
         EX    R6,MOVE9            GO MAKE NINTH MOVE         9915845   00050730
         LA    R6,2048                                        9915845   00050740
MOV8     S     R6,=F'1793'         SET UP R6 WITH PROPER LGTH 9915845   00050750
         EX    R6,MOVE8            GO MAKE EIGHTH MOVE        9915845   00050760
         LA    R6,1792                                        9915845   00050770
MOV7     S     R6,=F'1537'         SET UP R6 WITH PROPER LGTH 9915845   00050780
         EX    R6,MOVE7            GO MAKE SEVENTH MOVE       9915845   00050790
         LA    R6,1536                                        9915845   00050800
MOV6     S     R6,=F'1281'         SET UP R6 WITH PROPER LGTH 9915845   00050810
         EX    R6,MOVE6            GO MAKE SIXTH MOVE         9915845   00050820
         LA    R6,1280                                        9915845   00050830
MOV5     S     R6,=F'1025'         SET UP R6 WITH PROPER LGTH 9915845   00050900
         EX    R6,MOVE5            GO MAKE FIFTH MOVE                   00051000
         LA    R6,1024                                                  00051100
MOV4     S     R6,=F'769'          SET UP R6 WITH PROPER LENGTH         00051200
         EX    R6,MOVE4            GO MAKE FOURTH MOVE                  00051300
         LA    R6,768                                                   00051400
MOV3     S     R6,=F'513'          SET UP PROPER LENGTH                 00051500
         EX    R6,MOVE3            GO MAKE THIRD MOVE                   00051600
         LA    R6,512                                                   00051700
MOV2     S     R6,=F'257'          SET UP PROPER LENGTH                 00051800
         EX    R6,MOVE2            GO MAKE SECOND MOVE                  00051900
         LA    R6,256                                                   00052000
         B     MOV1                                                     00052100
         SPACE 1                                                        00052200
MOVE9    MVC   2048(0,R5),2048(R4)                            9915845   00052210
MOVE8    MVC   1792(0,R5),1792(R4)                            9915845   00052300
MOVE7    MVC   1536(0,R5),1536(R4)                            9915845   00052400
MOVE6    MVC   1280(0,R5),1280(R4)                            9915845   00052410
MOVE5    MVC   1024(0,R5),1024(R4)                            9915845   00052420
MOVE4    MVC   768(0,R5),768(R4)   THESE INSTRUCTIONS EXECUTE 9915845   00052430
MOVE3    MVC   512(0,R5),512(R4)   WITH AN EX COMMAND, LENGTH IS        00052500
MOVE2    MVC   256(0,R5),256(R4)   SUPPLIED IN REGISTER R6.             00052600
MOVE     MVC   0(0,R5),0(R4)                                            00052700
         SPACE 1                                                        00052800
ERR      DC    H'0'                                                     00052900
         DC    C'DUMPED BECAUSE EXPANDED LENGTH OVER RECORD LGTH'       00053000
         SPACE 1                                                        00053100
         LTORG                                                          00054200
         DC    X'00'                                                    00054300
         SPACE 1                                                        00054400
* THE FOLLOWING STATEMENT(S) MODIFIED BY                      0266888   00054404
BLANK    DC    X'40000000'                                     1105504  00054405
MASTEROT DSECT                     MASTER AREA (DUMMY SECTION)          00054500
OFIXED   DS    CL2960              FIXED SEGMENT              9915845   00054600
ONADDTR  DS    CL1                 TRAILER  - NAME ADDRESS              00054700
OSOCSECT DS    CL1                 POINTERS   SOCIAL SECURITY           00054800
OLOANTR  DS    CL1                            LOAN                      00055000
OMKTTR   DS    CL1                            MARKETING                 00055100
OTRNAFFT DS    CL2                            XFER-AFFL       9915845   00055200
OODNSFTR DS    CL1                            OD-NSF                    00055300
OSVTR    DS    CL1                            SAVINGS                   00055400
OFLTR    DS    CL1                            FLOAT                     00055500
OUNCLTR  DS    CL1                            UNCOLLECTED               00055600
OCMBTR   DS    CL2                            COMBINED STATEMENT        00055700
OTGTR    DS    CL1                            TARGET                    00055800
OLMTTR   DS    CL1                            LIMIT                     00055900
OTAXTR   DS    CL1                            TAX                       00056000
OODACRTR DS    CL1                            OD ACCRUAL                00056100
OINFOTR  DS    CL1                            INFORMATION               00056200
ODEPTR   DS    CL1                            DEPOSIT TRAILER           00056400
OKITETR  DS    CL1                            KITING TRAILER            00056500
OEFATR   DS    CL1                            EFA TRAILER               00056600
OCASHTR  DS    CL1                            CASH AV TRAILER           00056700
OINVTR   DS    CL1                            INVESTMENT TRLR  1004554  00056710
ORATETR  DS    CL1                            RATE TRAILER     1004875  00056720
OEXTTR   DS    CL1                            EXT SC TRLR     9915845   00056725
* THE FOLLOWING STATEMENT(S) MODIFIED BY                      0617360   00056726
OAYTR    DS    CL1                            ALT YEAR TRAILER9915845   00056727
OPLTR    DS    CL1                            PLAN TRLR       0617360   00056730
         DS    CL11                           FILLER          0617360   00056800
ODTCTTR  DS    CL1                            DATA CENTER     9915845   00056810
         DS    CL2                            FILLER          9915845   00056820
ONADD    DS    CL778               TRAILER -  N/A SEGMENT     9915845   00056900
OSOCSEC  DS    CL135               SEGMENTS   SOCIAL SECURITY 9915845   00057000
OLOAN    DS    CL1500                         LOAN            9915845   00057200
OMKT     DS    CL360                          MARKETING       9915845   00057300
OTRNAFF  DS    CL765                          XFER-ALLF       9915845   00057400
OODNSF   DS    CL130                          OD-NSF          9915845   00057500
OSV      DS    CL660                          SAVINGS         9915845   00057600
OFL      DS    CL56                           FLOAT           9915845   00057700
OUNCL    DS    CL96                           UNCOLLECTED     9915845   00057800
OCMB     DS    CL690                          COMBINED STMT   9915845   00057900
OTG      DS    CL125                          TARGET          9915845   00058000
OLMT     DS    CL75                           LIMIT           9915845   00058100
OTAX     DS    CL120                          TAX             9915845   00058200
OODACR   DS    CL425                          OD ACCRUAL      9915845   00058300
OINFO    DS    CL60                           INFORMATION               00058400
ODEP     DS    CL130                          DEPOSIT TRAILER 9915845   00058600
OKITE    DS    CL275                          KITING TRAILER  9915845   00058700
OEFA     DS    CL133                          EFA TRAILER     9915845   00058800
OCASH    DS    CL204                          CASH AVL TRAILER9915845   00058900
OINV     DS    CL120                          INVESTMENT TRLR 9915845   00058910
ORATEFL  DS    0CL7                           RATE FLAGS      2012254   00058915
ORATEDDA DS    CL1                            DDA RATE FLAG    1004875  00058920
ORATESAV DS    CL1                            SAV RATE FLAG    1004875  00058925
ORATEOD  DS    CL1                            OD RATE FLAG     1004875  00058930
ORATELN  DS    CL1                            LN RATE FLAG     1004875  00058935
ORATETFD DS    CL1                            FED TAX RT FLAG 2012254   00058936
ORATETST DS    CL1                            ST TAX RT FLAG  2012254   00058937
ORATETLC DS    CL1                            LOC TAX RT FLAG 2012254   00058938
ORDDA    DS    CL421                          DDA RATE TRAILER9915845   00058940
ORSAV    DS    CL43                           SAV RATE TRAILER9915845   00058945
OROD     DS    CL421                          OD RATE TRAILER 9915845   00058950
ORLN     DS    CL288                          LN RATE TRAILER 9915845   00058955
ORFEDTX  DS    CL118                          FED TAX RT TRLR 2012254   00058956
ORSTTX   DS    CL118                          ST TAX RT TRLR  2012254   00058957
ORLOCTX  DS    CL118                          LOC TAX RT TRLR 2012254   00058958
OEXTSC   DS    CL2000                         EXT SC TRLR     9915845   00058960
OALTYR   DS    CL240                          ALT YR TRAILER  9915845   00058965
OFILL    DS    CL143                          FILLER          2012254   00059000
OPLN     DS    CL2126                         PLAN TRLR       0617360   00059002
ODTCT    DS    CL750                          DATA CENTER     0930011   00059010
MASTERIN DSECT                     MASTER AREA (DUMMY SECTION)          00059100
IFIXED   DS    CL2960              FIXED SEGMENT              9915845   00059200
INADDTR  DS    CL1                 TRAILER  - NAME ADDRESS              00059300
ISOCSECT DS    CL1                 POINTERS   SOCIAL SECURITY           00059400
ILOANTR  DS    CL1                            LOAN                      00059600
IMKTTR   DS    CL1                            MARKETING                 00059700
ITRNAFFT DS    CL2                            XFER-AFFL       9915845   00059800
IODNSFTR DS    CL1                            OD-NSF                    00059900
ISVTR    DS    CL1                            SAVINGS                   00060000
IFLTR    DS    CL1                            FLOAT                     00060100
IUNCLTR  DS    CL1                            UNCOLLECTED               00060200
ICMBTR   DS    CL2                            COMBINED STATEMENT        00060300
ITGTR    DS    CL1                            TARGET                    00060400
ILMTTR   DS    CL1                            LIMIT                     00060500
ITAXTR   DS    CL1                            TAX                       00060600
IODACRTR DS    CL1                            OD ACCRUAL                00060700
IINFOTR  DS    CL1                            INFORMATION               00060800
IDEPTR   DS    CL1                            DEPOSIT TRAILER           00061000
IKITETR  DS    CL1                            KITING TRAILER            00061100
IEFATR   DS    CL1                            EFA TRAILER               00061200
ICASHTR  DS    CL1                            CASH AVAILABLE            00061300
IINVTR   DS    CL1                            INVESTMENT TRLR  1004554  00061310
IRATETR  DS    CL1                            RATE TRAILER     1004875  00061320
IEXTTR   DS    CL1                            EXT SC TRLR     9915845   00061325
IAYTR    DS    CL1                            ALT YR TRLR     9915845   00061330
IPLTR    DS    CL1                            PLAN TRLR       0617360   00061332
         DS    CL11                           FILLER          0617360   00061400
IDTCTTR  DS    CL1                            DATA CENTER     9915845   00061410
         DS    CL2                            FILLER          9915845   00061420
INADD    DS    CL778               TRAILER -  N/A SEGMENT     9915845   00061500
ISOCSEC  DS    CL135               SEGMENTS   SOCIAL SECURITY 9915845   00061600
ILOAN    DS    CL1500                         LOAN            9915845   00061800
IMKT     DS    CL360                          MARKETING       9915845   00061900
ITRNAFF  DS    CL765                          XFER-AFFL       9915845   00062000
IODNSF   DS    CL130                          OD-NSF          9915845   00062100
ISV      DS    CL660                          SAVINGS         9915845   00062200
IFL      DS    CL56                           FLOAT           9915845   00062300
IUNCL    DS    CL96                           UNCOLLECTED     9915845   00062400
ICMB     DS    CL690                          COMB STMT       9915845   00062500
ITG      DS    CL125                          TARGET          9915845   00062600
ILMT     DS    CL75                           LIMIT           9915845   00062700
ITAX     DS    CL120                          TAX             9915845   00062800
IODACR   DS    CL425                          OD ACCRUAL      9915845   00062900
IINFO    DS    CL60                           INFORMATION               00063000
IDEP     DS    CL130                          DEPOSIT TRAILER 9915845   00063200
IKITE    DS    CL275                          KITING TRAILER  9915845   00063300
IEFA     DS    CL133                          EFA TRAILER     9915845   00063400
ICASH    DS    CL204                          CASH AVAILABLE  9915845   00063500
IINV     DS    CL120                          INVESTMENT TRLR  1004554  00063510
IRATEFL  DS    0CL7                           RATE FLAGS      2012254   00063515
IRATEDDA DS    CL1                            DDA RATE FLAG    1004875  00063520
IRATESAV DS    CL1                            SAV RATE FLAG    1004875  00063525
IRATEOD  DS    CL1                            OD RATE FLAG     1004875  00063530
IRATELN  DS    CL1                            LN RATE FLAG     1004875  00063535
IRATETFD DS    CL1                            FED TAX RT FLAG 2012254   00063536
IRATETST DS    CL1                            ST TAX RT FLAG  2012254   00063537
IRATETLC DS    CL1                            LOC TAX RT FLAG 2012254   00063538
IRDDA    DS    CL421                          DDA RATE TRAILER9915845   00063540
IRSAV    DS    CL43                           SAV RATE TRAILER9915845   00063545
IROD     DS    CL421                          OD RATE TRAILER 9915845   00063550
IRLN     DS    CL288                          LN RATE TRAILER 9915845   00063555
IRFEDTX  DS    CL118                          FED TAX RT TRLR 2012254   00063556
IRSTTX   DS    CL118                          ST TAX RT TRLR  2012254   00063557
IRLOCTX  DS    CL118                          LOC TAX RT TRLR 2012254   00063558
IEXTSC   DS    CL2000                         EXT SC TRLR     9915845   00063570
IALTYR   DS    CL240                          ALT YR TRLR     9915845   00063580
IFILL    DS    CL143                          FILLER          2012254   00063600
IPLN     DS    CL2126                         PLAN TRLR       0617360   00063605
IDTCT    DS    CL750                          DATA CENTER     0930011   00063610
*                                                                       00063700
*----------------------------------------------------------------*      00063800
         SPACE 2                                                        00063900
         AIF   (&CICS).ONL4                                             00064000
         AGO   .COM4                                                    00064100
.ONL4    ANOP                                                           00064200
NAAREA   DSECT                NAME / ADDRESS DSECT                      00064300
NAAREA1  DS    0CL386                                         9915845   00064400
NALINE1  DS    CL42                                                     00064500
NALINE2  DS    CL42                                                     00064600
NALINE3  DS    CL42                                                     00064700
NALINE4  DS    CL42                                                     00064800
NALINE5  DS    CL42                                                     00064900
NALINE6  DS    CL42                                                     00065000
NALINE7  DS    CL42                                                     00065100
NALINE8  DS    CL42                                           9715504   00065110
NAFILL   DS    CL50                                           9915845   00065120
*                                                                       00065200
         COPY TSTS2NCA                                                  00065300
*                                                                       00065400
         COPY TSTS2TSC                                                  00065500
*                                                                       00065600
         COPY TSTS2TTR                                                  00065700
*                                                                       00065800
.COM4    ANOP                                                           00065900
         AIF   ('&SYSPARM' NE 'CICS').DTSBAT                            00066185
FIIMMSEP CSECT                                                          00066186
         AGO   .DTSMRG                                                  00066187
.DTSBAT  ANOP                                                           00066188
IMMSEP   CSECT                                                          00066189
.DTSMRG  ANOP                                                           00066190
         LTORG                                                          00066191
         DS    0D                                                       00066192
SITMSTMP DC    CL64'IMMSEP    -----TSD-             06/29/09  09.00.35' 00066193
*        THIS PROGRAM CONTAINS TRADE SECRETS THAT BELONG                00066194
*        TO FIDELITY INFORMATION SERVICES AND IS                        00066195
*        LICENSED BY AN AGREEMENT.  ANY UNAUTHORIZED ACCESS,            00066196
*        USE, DUPLICATION, OR DISCLOSURE IS UNLAWFUL.                   00066197
*        COPYRIGHT FIDELITY INFORMATION SERVICES                        00066198
*        2009, ALL RIGHTS RESERVED.                                     00066199
         END                                                            00066200
