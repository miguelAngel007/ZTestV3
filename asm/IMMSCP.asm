*     * FO5238 * 06/26/11 PROYECTO REBORN
*        PRINT NOGEN                                                    00000100
*-----------------------------------------------------------------*     00000200
*        THIS PHASE COMPRESSES THE IMPACS MASTER FILE.                  00000300
*                                                                       00000400
*            PARAMETER IS PASSED:  PARAMETER 1 = R3                     00000500
*                                                                       00000600
*                                                                       00000700
*        PARAMETER 1 WILL BE THE ADDRESS OF THE MASTER FILE.            00000800
*                                                                       00000900
*                                                                       00001000
*-----------------------------------------------------------------      00001100
*           PROGRAMS USING THIS I/O ROUTINE ARE:                        00001200
*                                                                       00001300
*                         IMACTMA                                       00001400
*                  CALL 'IMMSCP' USING IM-MASTER-RECORD.                00001500
*                                                                       00001600
*                     REVISED                                 0266888   00001605
*  05/23/02  LE ENABLED, REETRANT                        ~~~6888        00001610
*-----------------------------------------------------------------      00001700
         SIRENT                                                         00001800
         LCLB  &CICS                                          0266888   00001805
         SIEQREG                                              0266888   00001810
         AIF   ('&SYSPARM' NE 'CICS').BTCHDTA                 0266888   00001815
DFHEISTG DSECT                                                          00001820
         AGO   .DATAMRG                                       0266888   00001825
.BTCHDTA ANOP                                                           00001830
         PRINT ON,GEN                                         0266888   00001835
         CEECAA                                                         00001840
         EJECT                                                0266888   00001845
         CEEDSA                                                         00001850
         EJECT                                                0266888   00001855
* THE FOLLOWING STATEMENT(S) MODIFIED BY                      0266888   00001859
.DATAMRG ANOP                                                           00001860
UDSABEG  DS    0D                  BEGIN OF USER DSA          0266888   00001865
SAVEAREA DS    9D                                             0266888   00001870
DBLWORD  DS    D                                              0266888   00001875
PGMPLIST DS    F                   ORIGINAL R1 SAVE AREA      0266888   00001880
SAVE13   DS    F                                              0266888   00001885
FULLWORD DS    F                                              0266888   00001890
CMPMST   DS    F                   SAVED ADDRESS OF COMPRESSED MASTER   00001895
EXPMST   DS    F                   SAVED ADDRESS OF EXPANDED MASTER     00001900
STORE1   DS    F                                              0266888   00001905
STORE2   DS    F                                              0266888   00001910
STORE3   DS    F                                              0266888   00001915
LENGTH   DS    H                                              0266888   00001920
RECLENGH DS    0F                                             0266888   00001925
RECLNG   DS    H                                              0266888   00001930
BINZEROS DS    H                                              0266888   00001935
HALFWORD DS    H                                              0266888   00001940
HWORD    DS    H                                              0266888   00001945
RATEFLGS DS    0CL7                                           0266888   00001950
RATEFDDA DS    CL1                                            0266888   00001955
RATEFSAV DS    CL1                                            0266888   00001960
RATEFOD  DS    CL1                                            0266888   00001965
RATEFLN  DS    CL1                                            0266888   00001970
RATETFD  DS    CL1                                            0266888   00001975
RATETST  DS    CL1                                            0266888   00001980
RATETLC  DS    CL1                                            0266888   00001985
*                                                             0266888   00001990
         COPY  TSTS2WMS                                       0266888   00001995
*                                                             0266888   00002000
         COPY  TSTS2ECC                                       0266888   00003600
         EJECT                                                0266888   00003700
#UDSALEN EQU   *-UDSABEG           LENGTH OF DSA              0266888   00003800
         AIF   ('&SYSPARM' NE 'CICS').GEN1                    0266888   00003900
&CICS    SETB  1                                              0266888   00004000
* THE FOLLOWING STATEMENT(S) MODIFIED BY                      0266888   00004099
         TITLE 'FIIMSPEP- IM MASTER RECORD COMPRESSION ROUTINE'         00004100
FIIMMSCP DFHEIENT CODEREG=(R12),DATAREG=(R13,R9),EIBREG=(R2)  0266888   00004200
FIIMMSCP AMODE 31                                             0266888   00004300
FIIMMSCP RMODE ANY                                            0266888   00004400
         L     R3,DFHEICAP         LOAD ADDR OF COMMON AREA             00004500
         CLC   TSEYECAT,0(R3)      IS TS EYE CATCHER PRESENT            00004600
         BE    TSSTART             YES - GO TO TS START UP              00004700
*                                                                       00004800
FISTART  EQU   *                                                        00004900
         C     R3,=F'80000000'     WAS THERE ANY RECORD PASSED?         00005000
         BE    EXIT                  NO- GET OUT                        00005100
         ST    R3,CMPMST           SAVE ADDRESS OF COMPRESSED MASTER    00005200
         ST    R3,EXPMST           SAVE ADDRESS OF EXPANDED MASTER      00005300
*                                    ONLINE- COMPRESSED MASTER AND      00005400
*                                    EXPANDED MASTER OCCUPY SAME SPACE  00005500
         XC    2(2,R3),2(R3)       CLEAR SECOND HALF OF LENGTH FIELD    00005600
         B     FIDONE                                                   00005700
         SPACE 2                                                        00005800
TSSTART  EQU   *                   TS START UP                          00005900
         EXEC CICS HANDLE CONDITION ERROR (ECICS000)                    00006000
         MVC   WMSMGPID,=C'FIIMMSCP' PROGRAM NAME FOR ERRORS            00006100
         CLC   EIBCALEN,=H'0000'   COMMAREA LENGTH?                     00006200
         BE    ERRCOMM                  NO, ERROR EXIT                  00006300
         MVC   TSECC,0(R3)         MOVE COMMAREA TO W/S                 00006400
         CLC   ECCRECAD,=X'00000000' IS THERE A RECORD ADDRESS?         00006500
         BE    ERRMAST                  NO, ERROR EXIT                  00006600
         L     R3,ECCRECAD         POINT TO EXPANDED MASTER             00006700
         ST    R3,CMPMST           SAVE ADDRESS OF COMPRESSED MASTER    00006800
         ST    R3,EXPMST           SAVE ADDRESS OF EXPANDED MASTER      00006900
*                                    ONLINE- COMPRESSED MASTER AND      00007000
*                                    EXPANDED MASTER OCCUPY SAME SPACE  00007100
         XC    2(2,R3),2(R3)       CLEAR SECOND HALF OF LENGTH FIELD    00007200
FIDONE   EQU   *                                                        00007300
         AGO   .GEN2                                                    00007400
         SPACE 2                                                        00007500
.GEN1    ANOP                                                           00007600
#DSALEN  EQU   *-CEEDSA            LENGTH OF DSA              0266888   00007605
         TITLE 'IMMSCP   MASTER RECORD COMPRESSION ROUTINE'    1004554  00007700
         SPACE 1                                                        00007800
* THE FOLLOWING STATEMENT(S) MODIFIED BY                      0266888   00007804
IMMSCP CEEENTRY ,                                                      X00007805
               PPA=PPA,                                                X00007810
               AUTO=#DSALEN,                                           X00007815
               MAIN=NO,                                                X00007820
               NAB=NO,                                                 X00007825
               PARMREG=1,                                              X00007830
               BASE=4                                                   00007835
         XR    R15,R15                                        0266888   00007840
         B     INIT000                                        0266888   00007845
         USING CEECAA,R12                                     0266888   00007850
* ------------------------------------------------------------------- * 00007855
*                  PROLOG AREA                                        * 00007860
* ------------------------------------------------------------------- * 00007865
PPA      CEEPPA ,                                                      X00007870
               LIBRARY=NO,                                             X00007900
               PPA2=YES,                                               X00007905
               EXTPROC=YES,                                            X00007910
               TSTAMP=YES,                                             X00007915
               PEP=YES,                                                X00007920
               INSTOP=YES,                                             X00007925
               EPNAME=IMMSCP,                                          X00007930
               VER=01,                                                 X00007935
               REL=01,                                                 X00007940
               MOD=00,                                                 X00007945
               DSA=YES                                                  00007950
         SPACE 2                                              0266888   00007955
         LTORG                                                0266888   00007960
INIT000  EQU   *                                              0266888   00008000
         ST    R1,PGMPLIST                                    0266888   00008005
         L     R3,0(1)             STORE PARAMETER ADDRESS (MASTER)     00008100
.GEN2    ANOP                                                           00008200
* ** CHECK FOR A BATCH HEADER RECORD  ********************    0547345   00008210
         CLI   26(R3),C'0'         CHECK FOR BANK HEADER      0547345   00008220
         BE    EXIT                YES,RETURN TO CALLER       0547345   00008230
         USING MASTERIN,R3                                    9915845   00008300
         SR    R7,R7               R7 WILL BE USED TO ACCUM RECORD LGTH 00008400
         LH    R7,=H'3000'         INITIALIZE FIXED LGTH      9915845   00008500
         LA    R10,INADD           POINT INPUT REGISTER TO 1ST TRAILER  00008600
         LA    R9,INADD            POINT OUTPUT REGISTER TO 1ST TRAILER 00008700
*                                  R7   REGISTER WILL BE INCREASED      00008800
*                                  BY APPROPRIATE TRAILER LENGTH- AT    00008900
*                                  END OF THIS ROUTINE IT WILL CONTAIN  00009000
*                                  THE TRUE LENGTH OF THE RECORD.       00009100
*                                                                       00009200
*                                                                       00009300
*-----------------------------------------------------------------      00009400
*        CONDSE NAME ADDRESS SEGMENT ROUTINE                            00009500
*        THIS ROUTINE EXPECTS THAT THE TRAILER FLAG CONTAINS A VALUE    00009600
*        OF PACKED 0, 1, OR 2 INDICATING THE NUMBER OF TRAILERS         00009700
*        PRESENT.  THE FIRST BYTE AND THE 296TH BYTE OF THE EXPANDED TR 00009800
*        WILL CONTAIN VALUES OF 1 THROUGH SEVEN (NUMBER OF LINES) FOR   00009900
*        FIRST OR SECOND TRAILER THIS ROUTINE MUST SQUEEZE THE BLANK    00010000
*        LINES OUT OF THE TRAILER AND COUNT THE NUMBER OF GOOD LINES.   00010100
*-----------------------------------------------------------------      00010200
*                                                                       00010300
         CP   INADDTR,=P'0'        IS THERE A NAME ADDRESS TRAILER      00010400
         BE   NONA                 NO  -  GO EXPAND SOC SEC TRAILER     00010500
*-----------------------------------------------------------------      00010600
*                                  COUNT NUMBER OF LINES PRESENT        00010700
*        NAME ADDRESS TRAILER ONE                                       00010800
*        CONDENSE LINES TOGETHER IN INPUT AREA                          00010900
*-----------------------------------------------------------------      00011000
         MVC   INADD(1),=P'0'      START TRAILER LINE COUNT WITH ZERO   00011100
         LA    R11,8               INITILIAZE NUMBER OF POSSIBL1105504  00011200
         LA    R2,INADD+3          POINT TO BEGIN MOVE AREA   9915845   00011300
         LR    R6,R2               ESTABLISH OUTPUT POINTER             00011400
COMPARE1 CLC   2(40,R2),SPACES     CHECK FOR BLANK LINE                 00011500
         BE    COMPAR1A            IF NOT GO BUMP POINTERS              00011600
         CLC   2(40,R2),BIZERO     CHECK FOR BINARY ZEROES              00011700
         BNE   NOTSPAC1            IF NOT GO BUMP POINTERS              00011800
COMPAR1A LA    R2,42(R2)           BUMP PAST BLANK LINE                 00011900
         BCT   R11,COMPARE1        HAVE ALL LINES BEEN LOOKED AT        00012000
         B     MOVNATR1            YES  -  GO MAKE NA TRAILER ONE MOVE  00012100
NOTSPAC1 CR    R2,R6               IS INPUT AND OUTPUT PTR STILL SAME   00012200
         BE    NOSQUEZ1            IF SO, NO SQUEEZE NEEDED             00012300
         MVC   0(42,R6),0(R2)      IF NOT, MOVE LINES TOGETHER          00012400
NOSQUEZ1 LA    R2,42(R2)           BUMP TO NEXT INPUT LINE              00012500
         LA    R6,42(R6)           BUMP TO NEXT OUTPUT LINE             00012600
         AP    INADD(1),=P'1'      ADD TO TRAILER ONE LINE COUNT        00012700
         BCT   R11,COMPARE1        HAVE ALL LINES BEEN LOOKED AT        00012800
*                                  IF NOT, GO TO COMPARE 1              00012900
MOVNATR1 BAS   R2,MOVNADD          GO CONDENSE NA TRAILER ONE 0262563   00013000
         CP    INADDTR,=P'2'       IS THERE A NAME ADDRESS TRAILER TWO  00013100
         BNE   NONA2               NO  -  GO EXPAND SOC SEC TRAILER     00013200
*-----------------------------------------------------------------      00013300
*                                  COUNT NUMBER OF LINES PRESENT        00013400
*        NAME ADDRESS TRAILER TWO                                       00013500
*        CONDENSE LINES TOGETHER IN INPUT AREA                          00013600
*-----------------------------------------------------------------      00013700
         MVC   INADD+389(1),=P'0'  START TRLR LINE CNT WITH 0 9915845   00013800
         LA    R11,8               INITILIAZE NBR OF LINES    9915845   00013900
         LA    R2,INADD+392        POINT TO BEGIN MOVE AREA   9915845   00014000
         LR    R6,R2               ESTABLISH OUTPUT POINTER             00014100
COMPARE2 CLC   2(40,R2),SPACES     CHECK FOR BLANK LINE                 00014200
         BE    COMPAR2A            IF NOT GO BUMP POINTERS              00014300
         CLC   2(40,R2),BIZERO     CHECK FOR BINARY ZEROES              00014400
         BNE   NOTSPAC2            IF NOT GO BUMP POINTERS              00014500
COMPAR2A LA    R2,42(R2)           BUMP PAST BLANK LINE                 00014600
         BCT   R11,COMPARE2        HAVE ALL LINES BEEN LOOKED AT        00014700
         B     MOVNATR2            YES  -  GO MAKE NA TRAILER TWO MOVE  00014800
NOTSPAC2 CR    R2,R6               IS INPUT AND OUTPUT PTR STILL SAME   00014900
         BE    NOSQUEZ2            IF SO, NO SQUEEQE NEEDED             00015000
         MVC   0(42,R6),0(R2)      IF NOT, MOVE LINES TOGETHER          00015100
NOSQUEZ2 LA    R2,42(R2)           BUMP TO NEXT INPUT LINE              00015200
         LA    R6,42(R6)           BUMP TO NEXT OUTPUT LINE             00015300
         AP    INADD+389(1),=P'1'  ADD TO TRLR 2 LINE CNT     9915845   00015400
         BCT   R11,COMPARE2        HAVE ALL LINES BEEN LOOKED AT        00015500
*                                  IF NOT, GO TO COMPARE 2              00015600
MOVNATR2 BAS   R2,MOVNADD          GO EXPAND NA TRAILER TWO   0262563   00015700
         B     DOSOCSEC                                                 00015800
         SPACE 1                                                        00015900
MOVNADD  MVC   0(3,R9),0(R10)      MOVE ADDR LINE CTR/CNTRY   9915845   00016000
         LA    R9,3(R9)            BUMP TO NEXT OUTPUT POS    9915845   00016100
         AH    R7,=H'3'            ADD TO RECORD LENGTH       9915845   00016200
         ZAP   DBLWORD,0(1,R10)    MOVE NUMBER OF OCCURRENCES           00016300
         LA    R10,3(R10)          BUMP TO NEXT INPUT POSITON 9915845   00016400
         MVC   HALFWORD,=H'42'     LENGTH OF ONE OCCURRENCE             00016500
         MVC   LENGTH,=H'386'      MAXIMUM LGTH ALL OCCURS    9915845   00016600
         BAS   R8,COMPRESS         GO COMPRESS                0262563   00016700
         LA    R10,386(R10)        BUMP INPUT PTR TO NEXT     9915845   00016800
         BR    R2                                                       00016900
         SPACE 1                                                        00017000
*-----------------------------------------------------------------      00017100
*        CONDENSE REMAINING TRAILERS                                    00017200
*-----------------------------------------------------------------      00017300
NONA     LA    R10,778(R10)        BUMP INPUT PTR TO SOC SEC  9915845   00017400
         B     DOSOCSEC                                                 00017500
NONA2    LA    R10,389(R10)        BUMP INPUT PTR TO SOC SEC  9915845   00017600
         SPACE 1                                                        00017700
*-----------------------------------------------------------------      00017800
***      SOCIAL SECURITY TRAILER     ***                                00017900
*-----------------------------------------------------------------      00018000
         SPACE 1                                                        00018100
DOSOCSEC CLI   ISOCSECT,C'0'         IS SOCIAL SECURITY TRAILER PREST   00018200
         BE    A1                    NO  -  BRANCH AROUND               00018300
         MVC   0(135,R9),0(R10)      MOVE SOC SEC TRLR        9915845   00018400
         LA    R9,135(R9)            BUMP TO NEXT OUTPUT POS  9915845   00018500
         LA    R7,135(R7)            ADD TO LENGTH OF RECORD  9915845   00018600
A1       LA    R10,135(R10)          BUMP TO NEXT INPUT TRLR  9915845   00018700
         SPACE 1                                                        00018800
*-----------------------------------------------------------------      00020100
***      LOAN TRAILER                ***                                00020200
*-----------------------------------------------------------------      00020300
         SPACE 1                                                        00020400
         CLI   ILOANTR,C'0'          IS LOAN TRAILER PRESENT            00020500
         BE    A2                                             9915845   00020600
         LA    R5,1500               ESTABLISH LENGTH OF TRLR 9915845   00020700
         MVC   0(256,R9),0(R10)      CONDENSE LOAN TRAILER              00020800
         MVC   256(256,R9),256(R10)                                     00020900
         MVC   512(256,R9),512(R10)                                     00021000
         MVC   768(256,R9),768(R10)                                     00021100
         MVC   1024(256,R9),1024(R10)                         9915845   00021200
         MVC   1280(220,R9),1280(R10)                         9915845   00021210
         AR    R7,R5                 INCREASE RECORD COUNT              00021300
         AR    R9,R5                 INCREASE POINTER TO NEXT OUTPUT    00021400
A2       LA    R10,1500(R10)         BUMP TO NEXT INPUT TRLR  9915845   00021500
         SPACE 1                                                        00021600
*-----------------------------------------------------------------      00021700
***      MARKETING TRAILER           ***                                00021800
*-----------------------------------------------------------------      00021900
         SPACE 1                                                        00022000
         CLI   IMKTTR,C'0'           IS MARKETING TRAILER PRESENT       00022100
         BE    A3                    NO                       9915845   00022200
         LA    R5,360                ESTABLISH LENGTH OF TRLR 9915845   00022300
         MVC   0(256,R9),0(R10)      CONDENSE MARKETING TRAILER         00022400
         MVC   256(104,R9),256(R10)                           9915845   00022500
         AR    R7,R5                 INCREASE RECORD COUNT              00022600
         AR    R9,R5                 INCREASE POINTER TO NEXT OUTPUT    00022700
A3       LA    R10,360(R10)          BUMP TO NEXT INPUT TRLR  9915845   00022800
         SPACE 1                                                        00022900
*-----------------------------------------------------------------      00023000
***      TRANSFER TRAILER            ***                                00023100
*-----------------------------------------------------------------      00023200
         SPACE 1                                                        00023300
         CP    ITRNAFFT,=P'0'        IS TRANSFER TRAILER PRESENT        00023400
         BE    A4                    NO                       9915845   00023500
         ZAP   DBLWORD,ITRNAFFT                                         00023600
         MVC   HALFWORD,=H'85'                                9915845   00023700
         MVC   LENGTH,=H'765'                                 9915845   00023800
         BAS   R8,COMPRESS           CNDSE TRANS/AFFILATE TRR 0262563   00023900
A4       LA    R10,765(R10)          BUMP TO NEXT INPUT TRLR  9915845   00024000
         SPACE 1                                                        00024100
*-----------------------------------------------------------------      00024200
***      OD/NSF TRAILER              ***                                00024300
*-----------------------------------------------------------------      00024400
         SPACE 1                                                        00024500
         CLI   IODNSFTR,C'0'         IS OD/NSF TRAILER PRESENT          00024600
         BE    A5                    NO                       9915845   00024700
         MVC   0(130,R9),0(R10)      MOVE OD/NSF TRAILER      9915845   00024800
         LA    R9,130(R9)            BUMP TO NEXT OUTPUT POS  9915845   00024900
         LA    R7,130(R7)            ADD TO LENGTH OF RECORD  9915845   00025000
A5       LA    R10,130(R10)          BUMP TO NEXT INPUT TRLR  9915845   00025100
         SPACE 1                                                        00025200
*-----------------------------------------------------------------      00025300
***      SAVINGS TRAILER             ***                                00025400
*-----------------------------------------------------------------      00025500
         SPACE 1                                                        00025600
         CLI   ISVTR,C'0'            IS SAVINGS TRAILER PRESENT         00025700
         BE    A6                    NO                       9915845   00025800
         LA    R5,660                ESTABLISH LENGTH OF TRLR 9915845   00025900
         MVC   0(256,R9),0(R10)      CONDENSE SAVINGS TRAILER           00026000
         MVC   256(256,R9),256(R10)  CONDENSE SAVINGS TRAILER 9915845   00026010
         MVC   512(148,R9),512(R10)                           9915845   00026100
         AR    R7,R5                 INCREASE RECORD COUNT              00026200
         AR    R9,R5                 INCREASE POINTER TO NEXT OUTPUT    00026300
A6       LA    R10,660(R10)          BUMP TO NEXT INPUT TRLR  9915845   00026400
         SPACE 1                                                        00026500
*-----------------------------------------------------------------      00026600
***      FLOAT TRAILER               ***                                00026700
*-----------------------------------------------------------------      00026800
         SPACE 1                                                        00026900
         CLI   IFLTR,C'0'            IS FLOAT TRAILER PRESENT           00027000
         BE    A7                    NO                       9915845   00027100
         MVC   0(56,R9),0(R10)       MOVE FLOAT TRAILER       9915845   00027200
         LA    R9,56(R9)             BUMP TO NEXT OUTPUT POS  9915845   00027300
         LA    R7,56(R7)             ADD TO LENGTH OF RECORD  9915845   00027400
A7       LA    R10,56(R10)           BUMP TO NEXT INPUT TRLR  9915845   00027500
         SPACE 1                                                        00027600
*-----------------------------------------------------------------      00027700
***      UNCOLLECTED TRAILER         ***                                00027800
*-----------------------------------------------------------------      00027900
         SPACE 1                                                        00028000
         CLI   IUNCLTR,C'0'          IS UNCOLL TRAILER PRESENT          00028100
         BE    A8                    NO                       9915845   00028200
         MVC   0(96,R9),0(R10)       MOVE UNCOLLECTED TRAILER 9915845   00028300
         LA    R9,96(R9)             BUMP TO NEXT OUTPUT POS  9915845   00028400
         LA    R7,96(R7)             ADD TO LENGTH OF RECORD  9915845   00028500
A8       LA    R10,96(R10)           BUMP TO NEXT INPUT TRLR  9915845   00028600
         SPACE 1                                                        00028700
*-----------------------------------------------------------------      00028800
***      COMBINED TRAILER            ***                                00028900
*-----------------------------------------------------------------      00029000
         SPACE 1                                                        00029100
         CP    ICMBTR,=P'0'          IS COMBINED TRAILER PRESENT        00029200
         BE    A9                    NO                       9915845   00029300
         ZAP   DBLWORD,ICMBTR                                           00029400
         MVC   HALFWORD,=H'46'                                          00029500
         MVC   LENGTH,=H'690'                                           00029600
         BAS   R8,COMPRESS           CNDSE COMBINED TRAILER   0262563   00029700
A9       LA    R10,690(R10)          BUMP TO NEXT INPUT TRLR  9915845   00029800
         SPACE 1                                                        00029900
*-----------------------------------------------------------------      00030000
***      TARGET TRAILER              ***                                00030100
*-----------------------------------------------------------------      00030200
         SPACE 1                                                        00030300
         CLI   ITGTR,C'0'            IS TARGET TRAILER PRESENT          00030400
         BE    A10                   NO                       9915845   00030500
         MVC   0(125,R9),0(R10)      MOVE TARGET TRAILER      9915845   00030600
         LA    R9,125(R9)            BUMP TO NEXT OUTPUT POS  9915845   00030700
         LA    R7,125(R7)            ADD TO LENGTH OF RECORD  9915845   00030800
A10      LA    R10,125(R10)          BUMP TO NEXT INPUT TRLR  9915845   00030900
         SPACE 1                                                        00031000
*-----------------------------------------------------------------      00031100
***      LIMIT TRAILER               ***                                00031200
*-----------------------------------------------------------------      00031300
         SPACE 1                                                        00031400
         CLI   ILMTTR,C'0'           IS LIMIT TRAILER PRESENT           00031500
         BE    A11                   NO                       9915845   00031600
         MVC   0(75,R9),0(R10)       MOVE LIMIT TRAILER       9915845   00031700
         LA    R9,75(R9)             BUMP TO NEXT OUTPUT POS  9915845   00031800
         LA    R7,75(R7)             ADD TO LENGTH OF RECORD  9915845   00031900
A11      LA    R10,75(R10)           BUMP TO NEXT INPUT TRLR  9915845   00032000
         SPACE 1                                                        00032100
*-----------------------------------------------------------------      00032200
***      TAX TRAILER                 ***                                00032300
*-----------------------------------------------------------------      00032400
         SPACE 1                                                        00032500
         CLI   ITAXTR,C'0'           IS TAX TRAILER PRESENT             00032600
         BE    A12                   NO                       9915845   00032700
         MVC   0(120,R9),0(R10)      MOVE TAX TRAILER         9915845   00032800
         LA    R9,120(R9)            BUMP TO NEXT OUTPUT POS  9915845   00032900
         LA    R7,120(R7)            ADD TO LENGTH OF RECORD  9915845   00033000
A12      LA    R10,120(R10)          BUMP TO NEXT INPUT TRLR  9915845   00033100
         SPACE 1                                                        00033200
*-----------------------------------------------------------------      00033300
***      OD ACCRUAL TRAILER          ***                                00033400
*-----------------------------------------------------------------      00033500
         SPACE 1                                                        00033600
         CLI   IODACRTR,C'0'         IS OD ACCRUAL TRAILER PRESENT      00033700
         BE    A13                   NO                       9915845   00033800
         LA    R5,425                ESTABLISH LENGTH OF TRLR 9915845   00033900
         MVC   0(256,R9),0(R10)      CONDENSE OD ACCR TRAILER 9915845   00034000
         MVC   256(169,R9),256(R10)  CONDENSE OD ACCR TRAILER 9915845   00034100
         AR    R7,R5                 INCREASE RECORD COUNT    9915845   00034110
         AR    R9,R5                 INCR PTR TO NEXT OUTPUT  9915845   00034120
A13      LA    R10,425(R10)          BUMP TO NEXT INPUT TRLR  9915845   00034200
         SPACE 1                                                        00034300
*-----------------------------------------------------------------      00034400
***      INFORMATION TRAILER         ***                                00034500
*-----------------------------------------------------------------      00034600
         SPACE 1                                                        00034700
         CLI   IINFOTR,C'0'          IS INFORMATION TRAILER PRESENT     00034800
         BE    A14                   NO                       9915845   00034900
         MVC   0(60,R9),0(R10)       MOVE INFORMATION TRAILER           00035000
         LA    R9,60(R9)             BUMP TO NEXT OUTPUT POS.           00035100
         LA    R7,60(R7)             ADD TO LENGTH OF RECORD            00035200
A14      LA    R10,60(R10)           BUMP TO NEXT INPUT TRLR  9915845   00035300
         SPACE 1                                                        00035400
*-----------------------------------------------------------------      00035500
***      DEPOSIT TRAILER             ***                                00036900
*-----------------------------------------------------------------      00037000
         SPACE 1                                                        00037100
         CLI   IDEPTR,C'0'           IS DEPOSIT TRAILER PRESENT         00037200
         BE    A15                   NO                       9915845   00037300
         MVC   0(130,R9),0(R10)      MOVE DEPOSIT TRAILER     9915845   00037400
         LA    R9,130(R9)            BUMP TO NEXT OUTPUT POS  9915845   00037500
         LA    R7,130(R7)            ADD TO LENGTH OF RECORD  9915845   00037600
A15      LA    R10,130(R10)          BUMP TO NEXT INPUT TRLR  9915845   00037700
         SPACE 1                                                        00037800
*-----------------------------------------------------------------      00037900
***      KITING SUSPECT TRAILER      ***                                00038000
*-----------------------------------------------------------------      00038100
         SPACE 1                                                        00038200
         CLI   IKITETR,C'0'          IS KITING SUSPECT TRAILER          00038300
         BE    A16                   NO                       9915845   00038400
         LA    R5,275                ESTABLISH LENGTH OF TRLR 9915845   00038500
         MVC   0(256,R9),0(R10)      CONDENSE DEPOSIT TRLR    9915845   00038600
         MVC   256(19,R9),256(R10)   CONDENSE DEPOSIT TRLR    9915845   00038700
         AR    R7,R5                 INCREASE RECORD COUNT    9915845   00038710
         AR    R9,R5                 INCR PTR TO NEXT OUTPUT  9915845   00038720
A16      LA    R10,275(R10)          BUMP TO NEXT INPUT TRLR  9915845   00038800
         SPACE 1                                                        00038900
*-----------------------------------------------------------------      00039000
***      EXPEDITED FUNDS AVAILABILITY (EFA) TRAILER ****                00039100
*-----------------------------------------------------------------      00039200
         SPACE 1                                                        00039300
         CLI   IEFATR,C'0'           IS EFA TRAILER PRESENT?            00039400
         BE    A17                   NO                       9915845   00039500
         MVC   0(133,R9),0(R10)      MOVE EFA TRAILER         9915845   00039600
         LA    R9,133(R9)            BUMP TO NEXT OUTPUT POS. 9915845   00039700
         LA    R7,133(R7)            ADD TO LENGTH OF RECORD  9915845   00039800
A17      LA    R10,133(R10)          BUMP TO NEXT INPUT TRLR  9915845   00039900
         SPACE 1                                                        00040000
*-----------------------------------------------------------------      00040100
***      CASH AVAILIBILITY TRAILER ****                                 00040200
*-----------------------------------------------------------------      00040300
         SPACE 1                                                        00040400
         CLI   ICASHTR,C'0'          IS CASH TRAILER PRESENT?           00040500
         BE    A18                   NO                       9915845   00040600
         MVC   0(204,R9),0(R10)      MOVE CASH AVL TRAILER    9915845   00040700
         LA    R9,204(R9)            BUMP TO NEXT OUTPUT POS. 9915845   00040800
         LA    R7,204(R7)            ADD TO LENGTH OF RECORD  9915845   00040900
A18      LA    R10,204(R10)          BUMP TO NEXT INPUT TRLR  9915845   00041000
         SPACE 1                                               1004554  00041010
*--------------------------------------------------------------1004554  00041020
***      INVESTMENT TRAILER ****                               1004554  00041030
*--------------------------------------------------------------1004554  00041040
         SPACE 1                                               1004554  00041050
         CLI   IINVTR,C'0'           IS INVEST TRLR PRESENT?   1004554  00041060
         BE    A19                   NO                       9915845   00041070
         MVC   0(120,R9),0(R10)      MOVE INVESTMENT TRLR     9915845   00041080
         LA    R9,120(R9)            BUMP TO NEXT OUTPUT POS. 9915845   00041090
         LA    R7,120(R7)            ADD TO LENGTH OF RECORD  9915845   00041100
A19      LA    R10,120(R10)          BUMP TO NEXT INPUT TRLR  9915845   00041110
         SPACE 1                                               1004554  00041120
*--------------------------------------------------------------1004875  00041122
***      RATE TRAILER                ***                       1004875  00041124
*--------------------------------------------------------------1004875  00041126
         SPACE 1                                               1004875  00041128
         CLI   IRATETR,C'0'          IS RATE TRAILER PRESENT   1004875  00041130
         BNE   A20                   NO                       9915845   00041132
         LA    R10,1534(R10)         BUMP TO NEXT INPUT TRLR  2012254   00041133
         B     A28                                            2012254   00041134
A20      MVC   RATEFLGS,0(10)        SAVE RATE FLAGS          9915845   00041135
         MVC   0(7,R9),0(R10)        MOVE RATE FLAGS          2012254   00041136
         LA    R9,7(R9)              BUMP TO NEXT OUTPUT POS. 2012254   00041137
         LA    R7,7(R7)              ADD TO LENGTH OF RECORD  2012254   00041138
         LA    R10,7(R10)            BUMP TO NEXT INPUT TRLR  2012254   00041139
         CLI   RATEFDDA,C'N'         IS DDA RATE TRLR PRESENT  1004875  00041140
         BE    A21                   NO                       9915845   00041141
         LA    R5,421                ESTABLISH LENGTH OF TRLR 9915845   00041142
         MVC   0(256,R9),0(R10)      CONDENSE DDA RATE TRAILER 1004875  00041144
         MVC   256(165,R9),256(R10)                           9915845   00041146
         AR    R7,R5                 INCREASE RECORD COUNT     1004875  00041148
         AR    R9,R5                 INC POINTER TO NEXT OUTPUT1004875  00041150
A21      LA    R10,421(R10)          BUMP TO NEXT INPUT TRLR  2012254   00041151
         CLI   RATEFSAV,C'N'         IS SAV RATE TRLR PRESENT 2012254   00041152
         BE    A22                   NO                       2012254   00041153
         MVC   0(43,R9),0(R10)       MOVE SAV RATE TRLR       2012254   00041154
         LA    R9,43(R9)             BUMP TO NEXT OUTPUT POS. 2012254   00041155
         LA    R7,43(R7)             ADD TO LENGTH OF RECORD  2012254   00041156
A22      LA    R10,43(R10)           BUMP TO NEXT INPUT TRLR  2012254   00041157
         CLI   RATEFOD,C'N'          IS OD RATE TRLR PRESENT  2012254   00041158
         BE    A23                   NO                       2012254   00041159
         LA    R5,421                ESTABLISH LENGTH OF TRLR 2012254   00041160
         MVC   0(256,R9),0(R10)      CONDENSE OD RATE TRLR    2012254   00041161
         MVC   256(165,R9),256(R10)                           2012254   00041162
         AR    R7,R5                 INCREASE RECORD COUNT    2012254   00041163
         AR    R9,R5                 INCR PTR TO NEXT OUTPUT  2012254   00041164
A23      LA    R10,421(R10)          BUMP TO NEXT INPUT TRLR  2012254   00041165
         CLI   RATEFLN,C'N'          IS LN RATE TRLR PRESENT  2012254   00041166
         BE    A24                   NO                       2012254   00041167
         LA    R5,288                ESTABLISH LENGTH OF TRLR 2012254   00041168
         MVC   0(256,R9),0(R10)      CONDENSE LN RATE TRLR    2012254   00041169
         MVC   256(32,R9),256(R10)                            2012254   00041170
         AR    R7,R5                 INCREASE RECORD COUNT    2012254   00041171
         AR    R9,R5                 INCR PTR TO NEXT OUTPUT  2012254   00041172
A24      LA    R10,288(R10)          BUMP TO NEXT INPUT TRLR  2012254   00041173
         CLI   RATETFD,C'N'          IS FED TAX RATE TRLR     2012254   00041174
         BE    A25                   NO                       2012254   00041175
         MVC   0(118,R9),0(R10)      MOVE SAV RATE TRLR       2012254   00041176
         LA    R9,118(R9)            BUMP TO NEXT OUTPUT POS  2012254   00041177
         LA    R7,118(R7)            ADD TO LENGTH OF RECORD  2012254   00041178
A25      LA    R10,118(R10)          BUMP TO NEXT INPUT TRLR  2012254   00041179
         CLI   RATETST,C'N'          IS ST TAX RATE TRLR      2012254   00041180
         BE    A26                   NO                       2012254   00041181
         MVC   0(118,R9),0(R10)      MOVE SAV RATE TRLR       2012254   00041182
         LA    R9,118(R9)            BUMP TO NEXT OUTPUT POS  2012254   00041183
         LA    R7,118(R7)            ADD TO LENGTH OF RECORD  2012254   00041184
A26      LA    R10,118(R10)          BUMP TO NEXT INPUT TRLR  2012254   00041185
         CLI   RATETLC,C'N'          IS LOC TAX RATE TRLR     2012254   00041186
         BE    A27                   NO                       2012254   00041187
         MVC   0(118,R9),0(R10)      MOVE SAV RATE TRLR       2012254   00041188
         LA    R9,118(R9)            BUMP TO NEXT OUTPUT POS  2012254   00041189
         LA    R7,118(R7)            ADD TO LENGTH OF RECORD  2012254   00041190
A27      LA    R10,118(R10)          BUMP TO NEXT INPUT TRLR  2012254   00041191
         SPACE 1                                              2012254   00041192
*------------------------------------------------------------ 2012254   00041193
***   EXT SERVICE CHARGE TRAILER     ***                       1105504  00041194
*--------------------------------------------------------------1105504  00041196
         SPACE 1                                               1105504  00041198
A28      CLI   IEXTTR,C'0'           IS EXT SC TRLR PRESENT?  2012254   00041202
         BE    A29                   NO                       2012254   00041204
         LA    R5,2000               ESTABLISH LENGTH OF TRLR 9915845   00041206
         MVC   0(256,R9),0(R10)      CONDENSE EXT TRLR         1105504  00041208
         MVC   256(256,R9),256(R10)                            1105504  00041210
         MVC   512(256,R9),512(R10)                            1105504  00041212
         MVC   768(256,R9),768(R10)                            1105504  00041214
         MVC   1024(256,R9),1024(R10)                          1105504  00041216
         MVC   1280(256,R9),1280(R10)                          1105504  00041218
         MVC   1536(256,R9),1536(R10)                         9915845   00041220
         MVC   1792(208,R9),1792(R10)                         9915845   00041222
         AR    R7,R5                 INCREASE RECORD COUNT    9915845   00041223
         AR    R9,R5                 INCREASE PTR TO NEXT     9915845   00041224
A29      LA    R10,2000(R10)         BUMP TO NEXT INPUT TRLR  2012254   00041225
         SPACE 1                                              9915845   00041226
*-----------------------------------------------------------  9915845   00041228
***      ALTERNATE YEAR-END TRAILER  ***                      9915845   00041230
*-----------------------------------------------------------  9915845   00041240
         SPACE 1                                              9915845   00041250
         CLI   IAYTR,C'0'            IS ALT YR TRLR PRESENT?  0617360   00041256
         BE    A30                   NO                       0617360   00041258
         MVC   0(240,R9),0(R10)      MOVE ALT YR TRLR         0617360   00041260
         LA    R9,240(R9)            BUMP TO NEXT OUTPUT POS. 0617360   00041261
         LA    R7,240(R7)            ADD TO LENGTH OF RECORD  0617360   00041262
A30      LA    R10,240(R10)          BUMP TO NEXT INPUT TRLR  0617360   00041263
         SPACE 1                                              0617360   00041264
*------------------------------------------------------------ 0617360   00041265
***      FILLER                      ***                      0617360   00041266
*------------------------------------------------------------ 0617360   00041267
         SPACE 1                                              0617360   00041268
A31      LA    R10,143(R10)          BUMP TO NEXT INPUT TRLR  0617360   00041269
         SPACE 1                                              0617360   00041270
*-----------------------------------------------------------  0617360   00041271
***      PLAN TRAILER   ***                                   0617360   00041272
*-----------------------------------------------------------  0617360   00041273
         SPACE 1                                              0617360   00041274
         CLI   IPLTR,C'0'            IS PLAN TRLR PRESENT?    0617360   00041275
         BE    A32                   NO                       0617360   00041276
         LA    R5,2126               ESTABLISH LENGTH OF TRLR 0617360   00041277
         MVC   0(256,R9),0(R10)      CONDENSE PLN TRAILER     0617360   00041278
         MVC   256(256,R9),256(R10)                           0617360   00041279
         MVC   512(114,R9),512(R10)  MOVE FIX AREA (626)      0617360   00041280
         ZAP   DBLWORD,624(2,R10)    SET NO OF OCCURS F/TRALR 0617360   00041281
         LA    R9,626(R9)            BUMP TO OUTPUT POSITION  0617360   00041282
         LA    R7,626(R7)            ADD TO LENGTH OF RECORD  0617360   00041283
         CP    DBLWORD,=P'0'         ANY OCCURRENCES?         0617360   00041284
         BE    A32                   NO SEGMENTS              0617360   00041285
         LA    R10,626(R10)          ADD TO LENGTH OF RECORD  0617360   00041286
         MVC   HALFWORD,=H'15'       SET SEGMENT LENGTH       0617360   00041287
         MVC   LENGTH,=H'1500'       SET TOTAL LENGTH         0617360   00041288
         BAS   R8,COMPRESS           GO CONDENSE PLN SEGMENTS 0617360   00041289
         LA    R10,1500(R10)         BUMP TO NEXT INPUT TRLR  1020121   00041290
         B     A32A                  SKIP FULL BUMP           1020121   00041291
A32      LA    R10,2126(R10)         BUMP TO NEXT INPUT TRLR  1020121   00041292
         SPACE 1                                              1020121   00041293
*------------------------------------------------------------ 1020121   00041294
***      DATA CENTER TRAILER         ***                      1020121   00041295
*------------------------------------------------------------ 9915845   00041296
         SPACE 1                                              9915845   00041298
A32A     CLI   IDTCTTR,C'0'          IS DC TRLR PRESENT?      1020121   00041300
         BE    A33                   NO                       0617360   00041302
         ZAP   DBLWORD,0(2,R10)      DETERMINE DC TRLR LGTH   9915845   00041304
         CVB   R5,DBLWORD                                     9915845   00041306
         ST    R5,STORE3                                      9915845   00041308
         BAS   R8,THRMOVES           GO CONDENSE TRAILER      0930011   00041310
A33      LA    R10,750(R10)          BUMP TO NEXT INPUT TRLR  0930011   00041312
         SPACE 1                                              9915845   00041316
         ST    R7,FULLWORD                                    9915845   00041318
         MVC   RECLNG,FULLWORD+2     MOVE RECORD OUTPUT LGTH  9915845   00041320
         MVC   BINZEROS,=X'0000'     PLUG HEX ZEROS TO OUTPUT LENGTH    00041400
         MVC   0(4,R3),RECLENGH      MOVE TRUE LENGTH TO AREA PASSED    00041500
EXIT     EQU   *                                                        00041600
         AIF   (&CICS).CICSRET                                          00041700
         CEETERM RC=(15)           RETURN                     0266888   00041800
         SPACE 2                                                        00041900
         AGO   .DATA                                                    00042000
.CICSRET ANOP                                                           00042100
         L     R5,DFHEICAP           LOAD ADDR OF COMMON AREA           00042200
         MVC   20(2,R5),RECLENGH     MOVE LENGTH TO COMM AREA           00042300
         EXEC  CICS RETURN                                              00042400
*                                                                       00042500
ERRCOMM  EQU   *                     NO COMMAREA ADDRESS                00042600
         MVC   WMSAPPL(6),=C'TS0316'                                    00042700
         B     ERROR                                                    00042800
ERRMAST  EQU   *                     NO MASTER ADDRESS                  00042900
         MVC   WMSAPPL(6),=C'TS0318'                                    00043000
         B     ERROR                                                    00043100
ERROR    EQU   *                                                        00043200
         MVI   WMSREQCD,C'1'                                            00043300
         MVC   WMSANCA,ECCNCADR                                         00043400
         BAS   R15,ERRTN000                                   0262563   00043500
         BAS   R15,FATAL000                                   0262563   00043600
         B     ABEND000                                                 00043700
*                                                                       00043800
TSEYECAT DC    C'** ECC**'                                              00043900
*                                                                       00044100
         COPY TSTS4PMS                                                  00044200
*                                                                       00044300
         COPY TSTS4PCM                                                  00044400
*                                                                       00044500
         EJECT                                                          00044600
.DATA    ANOP                                                           00044700
         SPACE 3                                                        00044800
*-----------------------------------------------------------------      00044900
*        THIS ROUTINES PRIMARY FUNCTION IS TO MOVE THE VARIABLE         00045000
*   TRAILERS FROM IMPACS   MASTER FIXED AREA TO BE STACKED ONE BEHIND   00045100
*   THE OTHER .  ALL DATA COMPRESSION WILL BE DONE WITHIN THE RECORD    00045200
*   ITSELF, THEREFORE IT IS MANDATORY THAT EACH TRAILER BE HANDLED      00045300
*   IN SEQUENCE AS IT OCCURS WITHIN THE RECORD.  VARIABLE DATA          00045400
*   EXPECTED IS:                                                        00045500
*      DBLWORD--NUMBER OF OCCURRENCES OF TRAILER                        00045600
*      HALFWORD--LENGTH OF EACH OCCURRENCE                              00045700
*      LENGTH--MAXIMUM LENGTH OF ALL OCCURRENCES                        00045800
*-----------------------------------------------------------------      00045900
         SPACE 1                                                        00046000
COMPRESS CVB   R5,DBLWORD          CONVERT THE TRAILER OCCURS TO BINARY 00046100
         MH    R5,HALFWORD         MULTIPLY BY OCCURS SEGMENT LENGTH    00046200
         CH    R5,LENGTH           MAKE SURE RESULT IS NOT GREATER      00046300
         BH    ERRLENGH            THAN MAX ALLOWED--IF SO ERROR        00046400
         ST    R5,STORE3                                                00046500
         C     R5,=F'257'          IS RESULT LESS THAN TWO MOVE IN LGTH 00046600
         BL    MOV1                YES--GO MAKE ONE MOVE                00046700
         C     R5,=F'513'          IS RESULT LESS THAN THR MOVES IN LGH 00046800
         BL    TWOMOVES            YES--TWO MOVES REQUIRED              00046900
         C     R5,=F'769'          IS RESULT LESS THAN FOUR MOVES       00047000
         BL    THRMOVES            YES--THREE MOVES REQUIRED            00047100
         C     R5,=F'1025'         IS RESULT LESS FIVE MOVES  9915845   00047102
         BL    FOURMOVE            YES--FOUR MOVES REQUIRED   9915845   00047104
         C     R5,=F'1281'         IS RESULT LESS SIX MOVES   9915845   00047106
         BL    FIVEMOVE            YES--FIVE MOVES REQUIRED   9915845   00047108
         C     R5,=F'1537'         IS RESULT LESS SEVEN MOVES 9915845   00047110
         BL    SIXMOVE             YES--SIX MOVES REQUIRED    9915845   00047112
         C     R5,=F'1793'         IS RESULT LESS EIGHT MOVES 9915845   00047114
         BL    SEVEMOVE            YES--SEVEN MOVES REQUIRED  9915845   00047116
         B     EIGHMOVE            GO MAKE EIGHT MOVES        9915845   00047118
         SPACE 1                                              9915845   00047120
TWOMOVES MVC   0(256,R9),0(R10)    TWO MOVES TO MOVE TRLR     9915845   00047122
         SH    R5,=H'257'          MOVE FIRST 256 BYTES       9915845   00047124
         EX    R5,MOVE2            MOVE REMAINDER             9915845   00047126
         B     ADDCOUNT            GO BUMP RECORD POSITIONS   9915845   00047128
MOVE2    MVC   256(0,R9),256(R10)                             9915845   00047130
         SPACE 1                                              9915845   00047132
THRMOVES MVC   0(256,R9),0(R10)    THREE MOVES REQUIRED       9915845   00047134
         MVC   256(256,R9),256(R10)   MOVE FIRST 512 BYTES    9915845   00047136
         SH    R5,=H'513'          UP TO MOVE REMAINDER       9915845   00047138
         EX    R5,MOVE3            MOVE REMAINDER             9915845   00047140
         B     ADDCOUNT            GO BUMP RECORD POSITIONS   9915845   00047142
MOVE3    MVC   512(0,R9),512(R10)                             9915845   00047144
         SPACE 1                                              9915845   00047146
FOURMOVE MVC   0(256,R9),0(R10)    FOUR MOVES REQUIRED        9915845   00047148
         MVC   256(256,R9),256(R10)   MOVE FIRST 768 BYTES    9915845   00047150
         MVC   512(256,R9),512(R10)   MOVE REMAINDER          9915845   00047152
         SH    R5,=H'769'                                     9915845   00047154
         EX    R5,MOVE4            MOVE REMAINDER             9915845   00047156
         B     ADDCOUNT            GO BUMP RECORD POSITIONS   9915845   00047158
MOVE4    MVC   768(0,R9),768(R10)                             9915845   00047160
         SPACE 1                                              9915845   00047162
FIVEMOVE MVC   0(256,R9),0(R10)    FIVE MOVES REQUIRED        9915845   00047164
         MVC   256(256,R9),256(R10)      MOVE 1ST 1024 BYTES  9915845   00047166
         MVC   512(256,R9),512(R10)      UP TO MOVE REMAINDER 9915845   00047168
         MVC   768(256,R9),768(R10)      UP TO MOVE REMAINDER 9915845   00047170
         SH    R5,=H'1025'                                    9915845   00047172
         EX    R5,MOVE5            MOVE REMAINDER             9915845   00047174
         B     ADDCOUNT            GO BUMP RECORD POSITIONS   9915845   00047180
MOVE5    MVC   1024(0,R9),1024(R10)                           9915845   00047190
         SPACE 1                                              9915845   00047200
SIXMOVE  MVC   0(256,R9),0(R10)    SIX MOVES REQUIRED         9915845   00047210
         MVC   256(256,R9),256(R10)    MOVE 1ST 1280 BYTES    9915845   00047220
         MVC   512(256,R9),512(R10)      UP TO MOVE REMAINDER 9915845   00047230
         MVC   768(256,R9),768(R10)      UP TO MOVE REMAINDER 9915845   00047240
         MVC   1024(256,R9),1024(R10)    UP TO MOVE REMAINDER 9915845   00047250
         SH    R5,=H'1281'                                    9915845   00047260
         EX    R5,MOVE6            MOVE REMAINDER             9915845   00047270
         B     ADDCOUNT            GO BUMP RECORD POSITIONS   9915845   00047280
MOVE6    MVC   1280(0,R9),1280(R10)                           9915845   00047290
         SPACE 1                                                        00047300
SEVEMOVE MVC   0(256,R9),0(R10)    SEVEN MOVES REQUIRED       9915845   00047310
         MVC   256(256,R9),256(R10)      MOVE 1ST 1536 BYTES  9915845   00047320
         MVC   512(256,R9),512(R10)      UP TO MOVE REMAINDER 9915845   00047400
         MVC   768(256,R9),768(R10)      UP TO MOVE REMAINDER 9915845   00047500
         MVC   1024(256,R9),1024(R10)    UP TO MOVE REMAINDER 9915845   00047600
         MVC   1280(256,R9),1280(R10)    UP TO MOVE REMAINDER 9915845   00047700
         SH    R5,=H'1537'                                    9915845   00047800
         EX    R5,MOVE7            MOVE REMAINDER             9915845   00047900
         B     ADDCOUNT            GO BUMP RECORD POSITIONS   9915845   00048000
MOVE7    MVC   1536(0,R9),1536(R10)                           9915845   00048100
         SPACE 1                                              9915845   00048200
EIGHMOVE MVC   0(256,R9),0(R10)    SEVEN MOVES REQUIRED       9915845   00048300
         MVC   256(256,R9),256(R10)      MOVE 1ST 1792 BYTES  9915845   00048400
         MVC   512(256,R9),512(R10)      UP TO MOVE REMAINDER 9915845   00048500
         MVC   768(256,R9),768(R10)      UP TO MOVE REMAINDER 9915845   00048600
         MVC   1024(256,R9),1024(R10)    UP TO MOVE REMAINDER 9915845   00048700
         MVC   1280(256,R9),1280(R10)    UP TO MOVE REMAINDER 9915845   00048800
         MVC   1536(256,R9),1536(R10)    UP TO MOVE REMAINDER 9915845   00048900
         SH    R5,=H'1793'                                    9915845   00049000
         EX    R5,MOVE7            MOVE REMAINDER             9915845   00049100
         B     ADDCOUNT            GO BUMP RECORD POSITIONS             00049200
MOVE8    MVC   1792(0,R9),1792(R10)                           9915845   00049300
         SPACE 1                                                        00049400
MOV1     SH    R5,=H'1'                                                 00049500
         EX    R5,MOVE             ONLY ONE MOVE NECESSARY - GO MOVE    00049600
ADDCOUNT EQU   *                                                        00049700
         L     R5,STORE3           RESTORE LENGTH OF TRAILER            00049800
         AR    R7,R5               INCREASE RECORD COUNT BY SEG LENGTH  00049900
         AR    R9,R5               INCREASE POINTER TO NEXT AVAIABLE    00050000
         BR    R8                  LOCATION IN MASTER FOR OUTPUT TRAILR 00050100
MOVE     MVC   0(0,R9),0(R10)                                           00050200
         SPACE 2                                                        00050300
ERRLENGH DC    H'0'                ERROR                                00050400
         DC    C'DUMPED BECAUSE SEGMENT OVER LIMIT ALLOWED'             00050500
         SPACE 2                                                        00050600
SPACES   DC    CL40' '                                                  00052100
BIZERO   DC    40X'00'             BINARY ZEROES                        00052200
         LTORG                                                          00052300
MASTERIN DSECT                     MASTER AREA (DUMMY SECTION)          00052400
IFIXED   DS    CL2960              FIXED SEGMENT              9915845   00052500
INADDTR  DS    CL1                 TRAILER  - NAME ADDRESS              00052600
ISOCSECT DS    CL1                 POINTERS   SOCIAL SECURITY           00052700
ILOANTR  DS    CL1                            LOAN                      00052900
IMKTTR   DS    CL1                            MARKETING                 00053000
ITRNAFFT DS    CL2                            XFER AFFILIATE  9915845   00053100
IODNSFTR DS    CL1                            OD-NSF                    00053200
ISVTR    DS    CL1                            SAVINGS                   00053300
IFLTR    DS    CL1                            FLOAT                     00053400
IUNCLTR  DS    CL1                            UNCOLLECTED               00053500
ICMBTR   DS    CL2                            COMBINED STATEMENT        00053600
ITGTR    DS    CL1                            TARGET                    00053700
ILMTTR   DS    CL1                            LIMIT                     00053800
ITAXTR   DS    CL1                            TAX                       00053900
IODACRTR DS    CL1                            OD ACCRUAL                00054000
IINFOTR  DS    CL1                            INFORMATION               00054100
IDEPTR   DS    CL1                            DEPOSIT TRAILER           00054300
IKITETR  DS    CL1                            KITING SUSPECT TR         00054400
IEFATR   DS    CL1                            EFA TRAILER               00054500
ICASHTR  DS    CL1                            CASH AVL TRAILER          00054600
IINVTR   DS    CL1                            INVESTMENT TRLR  1004554  00054610
IRATETR  DS    CL1                            RATE TRAILER     1004875  00054620
IEXTTR   DS    CL1                            EXT             9915845   00054625
IAYTR    DS    CL1                            ALT YR TRAILER  9915845   00054630
IPLTR    DS    CL1                            PLAN TRAILER    0617360   00054635
         DS    CL11                           FILLER          0617360   00054640
IDTCTTR  DS    CL1                            DATA CENTER     9915845   00054650
         DS    CL2                            FILLER          9915845   00054700
INADD    DS    CL778               TRAILER -  N/A SEGMENT     9915845   00054800
ISOCSEC  DS    CL135               SEGMENTS   SOCIAL SECURITY 9915845   00054900
ILOAN    DS    CL1500                         LOAN            9915845   00055100
IMKT     DS    CL360                          MARKETING       9915845   00055200
ITRNAFF  DS    CL765                          XFER-AFFL       9915845   00055300
IODNSF   DS    CL130                          OD-NSF          9915845   00055400
ISV      DS    CL660                          SAVINGS         9915845   00055500
IFL      DS    CL56                           FLOAT           9915845   00055600
IUNCL    DS    CL96                           UNCOLLECTED     9915845   00055700
ICMB     DS    CL690                          COMBINED STATEMENT        00055800
ITG      DS    CL125                          TARGET          9915845   00055900
ILMT     DS    CL75                           LIMIT           9915845   00056000
ITAX     DS    CL120                          TAX             9915845   00056100
IODACR   DS    CL425                          OD ACCRUAL      9915845   00056200
IINFO    DS    CL60                           INFORMATION               00056300
IDEP     DS    CL130                          DEPOSIT TRAILER 9915845   00056500
IKITE    DS    CL275                          KITING SUSPECT  9915845   00056600
IEFA     DS    CL133                          EFA TRAILER     9915845   00056700
ICASH    DS    CL204                          CASH AVL TRLR   9915845   00056800
IINVEST  DS    CL120                          INVESTMENT TRLR 9915845   00056810
IRATEDDA DS    CL1                            DDA RATE TRLR    1004875  00056820
IRATESAV DS    CL1                            SAV RATE TRLR    1004875  00056830
IRATEOD  DS    CL1                            OD RATE TRLR     1004875  00056840
IRATELN  DS    CL1                            LN RATE TRLR     1004875  00056850
IRATETFD DS    CL1                            FED TAX RT TRLR 2012254   00056852
IRATETST DS    CL1                            ST TAX RT TRLR  2012254   00056854
IRATETLC DS    CL1                            LOC TAX RT TRLR 2012254   00056856
IRATE    DS    CL1534                         RATE TRLR INFO  2012254   00056860
IEXTSC   DS    CL2000                         EXT SC TRLR     9915845   00056870
IALTYR   DS    CL240                          ALT YR TRLR     9915845   00056880
IFILLR   DS    CL143                          FILLER          2012254   00056900
IPLN     DS    CL2126                         PLAN TRLR       0617360   00056905
IDTCT    DS    CL750                          DATA CENTER     0930011   00056910
         AIF   (&CICS).ONL4                                             00057400
.BAT4    ANOP                                                           00057500
*                                                                       00057800
         AGO   .COM4                                                    00057900
.ONL4    ANOP                                                           00058000
         COPY TSTS2NCA                                                  00058100
*                                                                       00058200
         COPY TSTS2TSC                                                  00058300
*                                                                       00058400
         COPY TSTS2TTR                                                  00058500
*                                                                       00058600
.COM4    ANOP                                                           00058700
         AIF   ('&SYSPARM' NE 'CICS').DTSBAT                            00058785
FIIMMSCP CSECT                                                          00058786
         AGO   .DTSMRG                                                  00058787
.DTSBAT  ANOP                                                           00058788
IMMSCP   CSECT                                                          00058789
.DTSMRG  ANOP                                                           00058790
         LTORG                                                          00058791
         DS    0D                                                       00058792
SITMSTMP DC    CL64'IMMSCP    -----TSD-             12/10/09  15.41.30' 00058793
*        THIS PROGRAM CONTAINS TRADE SECRETS THAT BELONG                00058794
*        TO FIDELITY INFORMATION SERVICES AND IS                        00058795
*        LICENSED BY AN AGREEMENT.  ANY UNAUTHORIZED ACCESS,            00058796
*        USE, DUPLICATION, OR DISCLOSURE IS UNLAWFUL.                   00058797
*        COPYRIGHT FIDELITY INFORMATION SERVICES                        00058798
*        2009, ALL RIGHTS RESERVED.                                     00058799
         END                                                            00058800
