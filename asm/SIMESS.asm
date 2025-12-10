*     * 802262 12/11/20 PROYECTO UPGRADE GN172 EDVR
         TITLE 'SIMESS SUBROUTINE FOR OS/MVS'                           00000002
*---------------------------------------------------------------------* 00000005
*                   ** PROGRAM DESCRIPTION **                         * 00000010
*                                                                     * 00000015
* SIMESS IS THE MESSAGE PRINT SUBROUTINE                              * 00000020
*                                                                     * 00000040
*---------------------------------------------------------------------* 00000045
         EJECT                                                          00000050
*---------------------------------------------------------------------* 00000055
*                  ** HISTORY OF REVISIONS **                         * 00000060
*                                                                     * 00000065
* DESCRIPTION                                                 CHNGID  * 00000070
* __________________________________________________________  _______ * 00000075
* 01/23/14 CLOSE PRINT BEFORE CALLING SICAN                   1436560   00000079
* 04/25/2006 INCLUDED NUMERIC CHECK FOR SECONDARY ERROR CODE  0725446   00000080
* 02/14/2000 REPLACED ALL BALR/BAL WITH BASR/BAS              2024207 * 00000171
* 02/02/00 MODIFY TO WORK WITH 31-BIT STORAGE                 2024183 * 00000173
* 07/25/95 MODIFY TO PRINT HIGHEST CONDITION CODE ON          2602755 * 00000175
* SECONDARY MESSAGES.                                         2602755 * 00000180
*                                                                     * 00000185
* 07/19/96 MODIFIED TO ALLOW SIMESS-MESS-NO TO BE USED AS A   2022121 * 00000186
* WAY OF OVERRIDING THE OVERRIDE MESSAGES.                    2022121 * 00000187
* LINES:  00001510, 00001520, 00024610                        2022121 * 00000188
*---------------------------------------------------------------------* 00000190
         EJECT                                                          00000195
SIMESS   START                                                          00000200
         SIBASE BASEREG=(12),RWNUM=0020,RWSUB=002,            2024183  X00000300
               SETMODE=3124                                   2024183   00000350
         EJECT                                                          00000400
*********************************************************************** 00000500
*        INITIALIZATION AND ENTRY SETUP                               * 00000600
*********************************************************************** 00000700
         ICM   R2,15,=X'7FFFFFFF'  Q. WHAT ADDR               2024183   00000705
         LA    R2,0(,R2)           ..MODE ARE                 2024183   00000710
         C     R2,=X'7FFFFFFF'     ..WE IN                    2024183   00000715
         BE    INIT0110            A. RUNNING IN AMODE(31)    2024183   00000720
         NI    12(R13),X'7F'       SET BIT OF R14 IN RSA OFF  2024183   00000725
         SIMODE 31                 ENABLE 31 BIT ADDR MODE    2024183   00000730
         B     EXEC                                           2024183   00000735
INIT0110 DS    0H                                             2024183   00000740
         OI    12(R13),X'80'       SET R14 HIGH BIT ON        2024183   00000745
         SPACE 1                                              2024183   00000750
EXEC     DS    0H                                             2024183   00000755
         L     R2,0(0,R1)          LOAD ADDRESSES OF PARAMETERS         00000800
*        R2    SIMESS PARAMETER AREA                                    00000900
         USING MSGAREA,R2                                               00001000
*        R6    ERROR TABLE ENTRY                                        00001100
         USING ERTBAREA,R6                                              00001200
A0010    B     INIT                INITIALIZE AND NOP THIS              00001300
         CLI   MESSPGM,X'FF'       END-OF-JOB CALL                      00001400
         BE    Z0010               YES                                  00001500
         CLI   MESSERNO,X'FF'      RESET OVERRIDE TABLE?      2022121   00001510
         BE    INIT0005            YES                        2022121   00001520
A0020    EQU   *                                                        00001600
         CP    LINECTR,MAXLINES    END OF PAGE                          00001700
         BL    A0030               NO                                   00001800
         BAS   R10,PRTHDNG         PRINT HEADINGS             2024207   00001900
A0030    EQU   *                                                        00002000
         LA    R6,SIDTERTB         GET ADDR OF ERROR TABLE              00002100
         SR    R7,R7                                                    00002200
A0040    EQU   *                                                        00002300
         IC    R7,0(0,R6)          GET ENTRY LENGTH                     00002400
         CLC   ERTBERNO,MESSERNO   IS THIS THE CORRECT MSG NO           00002500
         BE    B0010               YES                                  00002600
         LA    R6,1(R7,R6)         BUMP TO NEXT MSG                     00002700
         CLI   0(R6),X'FF'         END OF TABLE                         00002800
         BNE   A0040               NO                                   00002900
         B     C0010               YES - CHK DEFAULT                    00003000
         EJECT                                                          00003100
*********************************************************************** 00003200
*        PROCESS MESSAGE FROM ERROR TABLE                             * 00003300
*********************************************************************** 00003400
B0010    EQU   *                                                        00003500
         CLI   ERTBERCC,C' '       IS CONDITION CODE SPACE              00003600
         BE    RETURN              YES - MESSAGE IS IGNORED             00003700
         MVC   DETPGMID,MESSPGM    MOVE PROGRAM ID                      00003800
         MVC   DETERNO,ERTBERNO    MOVE ERROR NUMBER                    00003900
         MVC   CSLERNO,ERTBERNO                                         00004000
         MVC   CSLABND,ERTBABND    MOVE ABEND CODE                      00004100
         MVC   DETERCC,ERTBERCC    MOVE CONDITION CODE                  00004200
         SH    R7,=H'9'            GET LENGTH FOR MOVE                  00004300
         EX    R7,CHKSPACE         IS MESSAGE ALL SPACES                00004400
         BE    B0040               YES - USE OPTIONAL MSG               00004500
         EX    R7,MVCDETMS         MOVE ERROR MESSAGE TO DETAIL LINE    00004600
         EX    R7,MVCCSLMS         MOVE ERROR MESSAGE TO CONSOLE LINE   00004700
         CLI   ERTBSEND,C'C'       DOES THIS MSG GO TO CONSOLE          00004800
         BNE   B0020               NO                                   00004900
         BAS   R10,CALLCSL                                    2024207   00005000
B0020    EQU   *                                                        00005100
         MVI   DETPCTL,C'0'        SPACE 2 AND WRITE CODE               00005200
         AP    LINECTR,=P'2'       ADD 2 TO LINE COUNTER                00005300
         BAS   R10,PUTPRT          PRINT MESSAGE              2024207   00006100
B0040    EQU   *                                                        00006400
         CLI   MESSMSG,C' '        IS OPTIONAL MSG PRESENT              00006500
         BNE   B0050               YES                                  00006600
         CLC   MESSMSG+1(L'MESSMSG-1),MESSMSG                           00006700
         BE    B0060               NO                                   00006800
B0050    EQU   *                                                        00006900
         MVC   DETERMSG,MESSMSG    MOVE OPTIONAL MESSAGE                00007000
         MVC   CSLERMSG,MESSMSG                                         00007100
         MVI   DETPCTL,C' '        SPACE 1 AND WRITE CODE               00007200
         AP    LINECTR,=P'1'       ADD 1 TO LINE COUNTER                00007300
         BAS   R10,PUTPRT          PRINT MESSAGE              2024207   00007400
         CLI   ERTBSEND,C'C'       DOES THIS MSG GO TO CONSOLE          00007500
         BNE   B0060               NO                                   00007600
         BAS   R10,CALLCSL                                    2024207   00007700
B0060    EQU   *                                                        00007800
         AP    ERRCNT,=P'1'        ADD 1 TO ERROR COUNT                 00007900
         CLC   WKHICC,ERTBERCC     IS NEW COND CODE HIGHER THAN LAST    00008000
         BNL   B0070               NO                                   00008100
         MVC   WKHICC,ERTBERCC     MOVE NEW COND CODE TO SAVE AREA      00008200
B0070    EQU   *                                                        00008300
         CLI   ERTBDASH,C'-'       SECONDARY CODE PRESENT?    2602755   00008310
         BNE   B0072               NO                         2602755   00008320
         CLC   WKHISC,ERTBERSC     IS NEW SECONDARY CODE HIGH 2602755   00008330
         BNL   B0072               NO                         2602755   00008340
         MVC   WKHISC,ERTBERSC     SAVE SECONDARY CODE        2602755   00008350
B0072    EQU   *                                              2602755   00008360
         CLI   ERTBABND,C' '       DO WE ABEND                          00008400
         BE    RETURN              NO - RETURN                          00008500
         PACK  DWORD,MESSERNO      GET MSG NO. AS ABEND CODE            00008600
         CVB   R0,DWORD                                                 00008700
         N     R0,MAXCODE          FORCE CODE TO 4095 OR LESS           00008800
         CLI   ERTBABND,C'D'       CANCEL WITH DUMP                     00008900
         BNE   B0080               YES                                  00009000
         LNR   R0,R0                                                    00009100
B0080    EQU   *                                                        00009200
         STH   R0,SICANAB          SAVE ABEND CODE                      00009300
         B     CALLCAN                                                  00009400
         SPACE 3                                                        00009500
CHKSPACE CLC   ERTBMSG(0),DETERMSG                                      00009600
MVCDETMS MVC   DETERMSG(0),ERTBMSG MOVE ERROR MESSAGE                   00009700
MVCCSLMS MVC   CSLERMSG(0),ERTBMSG                                      00009800
         EJECT                                                          00009900
*********************************************************************** 00010000
*        PROCESS OPTIONAL MSG ONLY OR NO MSG AT ALL                   * 00010100
*********************************************************************** 00010200
C0010    EQU   *                                                        00010300
         MVC   DETPGMID,MESSPGM    MOVE PROGRAM ID                      00010400
         MVC   DETERNO,MESSERNO    MOVE ERROR NUMBER                    00010500
         MVC   CSLERNO,MESSERNO                                         00010600
         LH    R1,MESSERCC         GET OPTIONAL CONDITION CODE          00010700
         CVD   R1,DWORD            CONVERT IT TO EXT DECIMAL            00010800
         UNPK  WKERCC,DWORD                                             00010900
         OI    WKERCC+3,X'F0'                                           00011000
         MVC   DETERCC,WKERCCRC    MOVE CONDITION CODE                  00011100
         CLI   MESSMSG,C' '        IS OPTIONAL MSG PRESENT              00011200
         BNE   C0020               YES                                  00011300
         CLC   MESSMSG+1(L'MESSMSG-1),MESSMSG                           00011400
         BNE   C0020               YES                                  00011500
         MVC   DETERMSG(L'DEFMSG),DEFMSG MOVE DEFAULT ERROR MSG         00011600
         MVC   CSLERMSG(L'DEFMSG),DEFMSG MOVE DEFAULT ERROR MSG         00011700
         B     C0030                                                    00011800
C0020    EQU   *                                                        00011900
         MVC   DETERMSG,MESSMSG    MOVE OPTIONAL MSG                    00012000
         MVC   CSLERMSG,MESSMSG    MOVE OPTIONAL MSG                    00012100
C0030    EQU   *                                                        00012200
         CLI   WKERCCCS,C'0'       DOES THIS MSG GO TO CONSOLE          00012300
         BE    C0040               NO                                   00012400
         MVI   CSLABND,C' '        CLEAR CONSOLE ABEND CODE             00012500
         CLI   WKERCCAB,C'1'       IS ABEND CODE A 1                    00012600
         BNE   *+8                 NO                                   00012700
         MVI   CSLABND,C'C'        SET CONSOLE ABEND CODE TO C          00012800
         CLI   WKERCCAB,C'2'       IS ABEND CODE A 2                    00012900
         BNE   *+8                 NO                                   00013000
         MVI   CSLABND,C'D'        SET CONSOLE ABEND CODE TO D          00013100
         BAS   R10,CALLCSL                                    2024207   00013200
C0040    EQU   *                                                        00013300
         MVI   DETPCTL,C'0'        SPACE 2 AND WRITE CODE               00013400
         AP    LINECTR,=P'2'       ADD 2 TO LINE COUNTER                00013500
         BAS   R10,PUTPRT          PRINT MESSAGE              2024207   00013600
         AP    ERRCNT,=P'1'        ADD 1 TO ERROR COUNT                 00013700
         CLC   WKHICC,WKERCCRC     IS NEW COND CODE HIGHER THAN LAST    00013800
         BNL   C0050               NO                                   00013900
         MVC   WKHICC,WKERCCRC     MOVE NEW COND CODE TO SAVE AREA      00014000
C0050    EQU   *                                                        00014100
         CLI   MESSDASH,C'-'       SECONDARY CODE PRESENT     2602755   00014110
         BNE   C0055               NO                         2602755   00014120
         CLC   WKHISC,MESSERSC     NEW CODE HIGH?             2602755   00014130
         BNL   C0055               NO                         2602755   00014140
         MVN   NUMWORK,MESSERSC    MOVE ERSC NUMERICS TO WK   0725446   00014142
         CLC   NUMWORK,MESSERSC    IS FIELD NUMERIC           0725446   00014144
         BNE   C0054               IF NOT MOVE ZERO           0725446   00014146
         MVC   WKHISC,MESSERSC     SAVE SECONDARY CODE        2602755   00014150
         B     C0055                                          0725446   00014154
C0054    MVC   WKHISC,=C'00'                                  0725446   00014158
C0055    EQU   *                                              2602755   00014160
         CLI   WKERCCAB,C'0'       DO WE ABEND                          00014200
         BNE   C0060               YES                                  00014300
         XC    MESSERCC,MESSERCC   CLEAR RETURN CODE                    00014400
         B     RETURN                                                   00014500
C0060    EQU   *                                                        00014600
         PACK  DWORD,MESSERNO      GET MSG NO. AS ABEND CODE            00014700
         CVB   R0,DWORD                                                 00014800
         N     R0,MAXCODE          FORCE CODE TO 4095 OR LESS           00014900
         CLI   WKERCCAB,C'2'       CANCEL WITH DUMP                     00015000
         BNE   C0070               YES                                  00015100
         LNR   R0,R0                                                    00015200
C0070    EQU   *                                                        00015300
         STH   R0,SICANAB          SAVE ABEND CODE                      00015400
         B     CALLCAN             CANCEL PROGRAM W/O DUMP              00015500
         EJECT                                                          00015600
*********************************************************************** 00015700
*        END OF JOB ROUTINE                                           * 00015800
*********************************************************************** 00015900
Z0010    EQU   *                                                        00016000
         CLI   PROCFLAG,C'1'       ANY PROCESSING OCCURRED              00016100
         BNE   Z0040               NO                                   00016200
         CP    LINECTR,MAXLINES    END OF PAGE                          00016300
         BL    Z0030               NO                                   00016400
         BAS   R10,PRTHDNG         PRINT HEADINGS             2024207   00016500
Z0030    EQU   *                                                        00016600
         PACK  DWORD,WKHICC        RETURN HI COND CODE                  00016700
         CVB   R1,DWORD                                                 00016800
         STH   R1,MESSERCC                                              00016900
         ED    TOTERR,ERRCNT       MOVE TOTAL NO. MSGS TO PRT           00017000
         ED    TOTHICC,DWORD+4     MOVE HI CC TO PRT                    00017100
         PACK  DWORD,WKHISC        RETURN HI SECONDARY CODE   2602755   00017110
         CVB   R1,DWORD                                       2602755   00017200
         ED    TOTHISC,DWORD+4     MOVE HI CC TO PRT          2602755   00017300
         MVC   IOA1(L'HDNG4),HDNG4 MOVE HEADING 4 TO I/O AREA           00017400
         BAS   R10,PUTPRT                                     2024207   00017500
         MVC   IOA1(L'TOTLINE1),TOTLINE1 MOVE MSG TO I/O AREA           00017600
         BAS   R10,PUTPRT                                     2024207   00017700
         MVC   IOA1(L'TOTLINE2),TOTLINE2 MOVE MSG TO I/O AREA           00017800
         BAS   R10,PUTPRT                                     2024207   00017900
         MVC   IOA1(L'TOTLINE3),TOTLINE3 MOVE MSG TO I/O AREA 2602755   00017910
         BAS   R10,PUTPRT                                     2024207   00017920
         CLOSE PRTFILE,MODE=31                                2024183   00018000
         MVC   A0010(4),POSTCLOS   DISABLE THIS PROGRAM.                00018010
Z0040    EQU   *                                                        00018100
         B     RETURN                                                   00018200
         EJECT                                                          00018300
*********************************************************************** 00018400
*        RETURN AND MISC CALLED ROUTINES                              * 00018500
*********************************************************************** 00018600
RETURN   EQU   *                                                        00018700
         SIRETRN                                                        00018800
         SPACE 3                                                        00018900
POSTCLOS B     RETURN                                                   00018910
         SPACE 3                                                        00018920
PUTPRT   EQU   *                                                        00019000
         NOP   PUTPRT10            CHG TO BR AFTER FILE OPEN            00019100
         OI    *-3,X'F0'           CHG NOP TO BRANCH                    00019200
         DEVTYPE PRTFILE+40,DEVTWORK SEE IF DD STMT FOR SIMESS IN JCL   00019210
         LTR   R15,R15             WAS DD STMT PRESENT                  00019220
         BZ    OPENPRT             YES - OPEN PRINT FILE                00019230
         MVC   PRTFILE+40(8),=CL8'SYSOUT' CHANGE DDNAME TO SYSOUT       00019240
OPENPRT  EQU   *                                                        00019250
         OPEN  (PRTFILE,OUTPUT),MODE=31  OPEN PRINT FILE      2024183   00019300
         MVI   PROCFLAG,C'1'       SET PROCESSING FLAG                  00019400
PUTPRT10 EQU   *                                                        00019500
         PUT   PRTFILE,IOA1                                             00019600
         MVI   IOA1+1,C' '         CLEAR I/O AREA BUFFER                00019700
         MVC   IOA1+2(L'IOA1-2),IOA1+1                                  00019800
         BR    R10                                                      00019900
         SPACE 3                                                        00020000
CALLCSL  EQU   *                                                        00020100
         WTO   MF=(E,WTOMSG)                                            00020200
         MVI   CSLERMSG,C' '       CLEAR CONSOLE MESSAGE LINE           00020300
         MVC   CSLERMSG+1(L'CSLERMSG-1),CSLERMSG                        00020400
         BR    R10                 RETURN                               00020500
         EJECT                                                          00020600
CALLCAN  EQU   *                                                        00020700
         CLOSE PRTFILE,MODE=31                                1436560   00020710
         LOAD  EP=SICAN            LOAD SICAN                           00020800
         LA    R1,SICANAB          GET ADDR OF ABEND CODE               00020900
         ST    R1,FWORD            STORE IT                             00021000
         OI    FWORD,X'80'         SET END-OF-LIST FLAG                 00021100
         LA    R1,FWORD            GET PARAMETER LIST ADDR              00021200
         LR    R15,R0              GET ENTRY ADDR                       00021300
         BASR  R14,R15             CALL SICAN                 2024207   00021400
         DC    X'0000'             CANCEL (SHOULDNT GET HERE)           00021500
         SPACE 3                                                        00021600
PRTHDNG  EQU   *                                                        00021700
         ST    R10,SAVER10         SAVE RETURN REGISTER                 00021800
         AP    PAGECTR,=P'1'       ADD TO PAGE COUNTER                  00021900
         MVC   H1PAGE,=XL6'402020202120'                                00022000
         ED    H1PAGE,PAGECTR                                           00022100
         MVC   IOA1(L'HDNG1),HDNG1 MOVE HEADING 1 TO I/O AREA           00022400
         BAS   R10,PUTPRT                                     2024207   00022500
         MVC   IOA1(L'HDNG2),HDNG2 MOVE HEADING 2 TO I/O AREA           00022600
         BAS   R10,PUTPRT                                     2024207   00022700
         MVC   IOA1(L'HDNG3),HDNG3 MOVE HEADING 3 TO I/O AREA           00022800
         BAS   R10,PUTPRT                                     2024207   00022900
         ZAP   LINECTR,=P'4'       SET LINE COUNTER                     00023000
         L     R10,SAVER10         GET RETURN REGISTER                  00023100
         BR    R10                 RETURN                               00023200
         EJECT                                                          00023300
*********************************************************************** 00023400
*        INITIALIZATION                                               * 00023500
*********************************************************************** 00023600
INIT     EQU   *                                                        00023700
         NI    A0010+1,X'0F'       NOP THE INITIAL BRANCH               00023800
         LOAD  EP=SIGETLNE         LOAD SIGETLNE                        00023900
         LR    R15,R0              GET ENTRY ADDR                       00024000
         LA    R1,MAXLINES         GET MAXLINES ADDR                    00024100
         ST    R1,FWORD            SAVE IT                              00024200
         LA    R1,FWORD            GET PARAMETER ADDR                   00024300
         BASR  R14,R15         CALL SIGETLNE TO GET LINE COUNT2024207   00024400
         DELETE EP=SIGETLNE        DONT NEED SIGETLNE ANY MORE HERE     00024500
         MVC   H1PGMID,MESSPGM     MOVE PROGRAM NAME TO HEADING         00024600
INIT0005 EQU   *   RESET THE MESS TABLE FROM OVERRIDES        2022121   00024610
         XC    MESSERCC,MESSERCC   ZERO RETURN CODE                     00024700
         LA    R4,MESSMSG          POINT TO OVERRIDE AREA               00024800
         LA    R5,11               SET NO. OF OVERRIDES                 00024900
INIT0010 EQU   *                                                        00025000
         CLC   0(4,R4),=CL4' '     IS THIS ENTRY SPACES                 00025100
         BE    RETURN              YES - RETURN                         00025200
         LA    R6,SIDTERTB         GET ADDR OF ERROR TABLE              00025300
         SR    R7,R7               CLEAR REG 7                          00025400
INIT0020 EQU   *                                                        00025500
         CLC   ERTBERNO,0(R4)      IS THIS THE CORRECT MSG NO           00025600
         BE    INIT0030            YES                                  00025700
         IC    R7,0(0,R6)          GET ENTRY LENGTH                     00025800
         LA    R6,1(R7,R6)         BUMP TO NEXT MSG                     00025900
         CLI   0(R6),X'FF'         END OF TABLE                         00026000
         BNE   INIT0020            NO                                   00026100
         MVI   MESSERCC+1,X'08'    SET RETURN CODE                      00026200
         B     INIT0090            YES - GET NEXT OVERRIDE ADDR         00026300
INIT0030 EQU   *                                                        00026400
         CLC   4(2,R4),DETERMSG    IS RC SPACES                         00026500
         BE    INIT0031            YES - TAKE IT (MSG IGNORED)          00026600
         MVN   NUMWORK,4(R4)       MOVE RC NUMERICS TO WORK AREA        00026700
         CLC   NUMWORK,4(R4)       IS FIELD NUMERIC                     00026800
         BE    INIT0031            YES - TAKE IT                        00026900
         BAS   R10,SETRC4          SET RETURN CODE = 4        2024207   00027000
         B     INIT0040            CHECK NEXT FIELD                     00027100
INIT0031 EQU   *                                                        00027200
         MVC   ERTBERCC,4(R4)      MOVE NEW CONDITION CODE TO TABLE     00027300
INIT0040 EQU   *                                                        00027400
         CLI   6(R4),C' '          IS SEND CODE A SPACE                 00027500
         BE    INIT0041            YES - TAKE IT                        00027600
         CLI   6(R4),C'C'          IS SEND CODE A C                     00027700
         BE    INIT0041            YES - TAKE IT                        00027800
         BAS   R10,SETRC4          SET RETURN CODE = 4        2024207   00027900
         B     INIT0050            CHECK NEXT FIELD                     00028000
INIT0041 EQU   *                                                        00028100
         MVC   ERTBSEND,6(R4)      MOVE NEW SEND CODE TO TABLE          00028200
INIT0050 EQU   *                                                        00028300
         CLI   7(R4),C' '          IS ABEND CODE A SPACE                00028400
         BE    INIT0051            YES - TAKE IT                        00028500
         CLI   7(R4),C'A'          IS ABEND CODE AN A                   00028600
         BE    INIT0051            YES - TAKE IT                        00028700
         CLI   7(R4),C'D'          IS ABEND CODE A D                    00028800
         BE    INIT0051            YES - TAKE IT                        00028900
         BAS   R10,SETRC4          SET RETURN CODE = 4        2024207   00029000
         B     INIT0090            GET NEXT OVERRIDE FIELD              00029100
INIT0051 EQU   *                                                        00029200
         MVC   ERTBABND,7(R4)      MOVE NEW ABEND CODE TO TABLE         00029300
INIT0090 EQU   *                                                        00029400
         LA    R4,8(0,R4)          BUMP TO NEXT OVERRIDE FIELD          00029500
         BCT   R5,INIT0010         LOOP BACK                            00029600
         B     RETURN                                                   00029700
SETRC4   EQU   *                                                        00029800
         CLI   MESSERCC+1,X'04'    IS RC ALREADY HIGHER THAN 4          00029900
         BHR   R10                 YES - RETURN                         00030000
         MVI   MESSERCC+1,X'04'    SET RC TO 4                          00030100
         BR    R10                 RETURN                               00030200
         EJECT                                                          00030300
*********************************************************************** 00030400
*        WORK AREAS                                                   * 00030500
*********************************************************************** 00030600
DWORD    DC    D'0'                                                     00030700
DEVTWORK DC    D'0'                                                     00030710
FWORD    DC    F'0'                                                     00030800
ACLRDWR  DC    F'0'                                                     00030900
SAVER10  DC    F'0'                                                     00031000
MAXCODE  DC    F'4095'                                                  00031100
SICANAB  DC    H'0'                                                     00031200
NUMWORK  DC    CL2'00'                                                  00031300
MAXLINES DC    PL2'0'                                                   00031400
LINECTR  DC    PL2'999'                                                 00031500
PAGECTR  DC    PL3'0'                                                   00031600
ERRCNT   DC    PL4'0'                                                   00031700
WKHICC   DC    CL2'00'                                                  00031800
WKHISC   DC    CL2'00'                                        2602755   00031810
PROCFLAG DC    CL1'0'                                                   00031900
WKERCC   DS    0CL4                                                     00032000
WKERCCCS DC    CL1' '                                                   00032100
WKERCCAB DC    CL1' '                                                   00032200
WKERCCRC DC    CL2' '                                                   00032300
         SPACE 3                                                        00032400
         LTORG                                                          00032500
         SPACE 3                                                        00032600
WTOMSG   WTO   '1234567890123456789012345678901234567890123456789012345X00032700
               6789012345678901234567890',                             X00032800
               ROUTCDE=(2),DESC=(7),MF=L                                00032900
         ORG   WTOMSG+4                                                 00033000
         DC    CL3'SIE'                                                 00033100
CSLERNO  DC    CL4' '                                                   00033200
CSLABND  DC    CL1' '                                                   00033300
         DC    CL1'-'                                                   00033400
CSLERMSG DC    CL71' '                                                  00033500
         ORG                                                            00033600
         EJECT                                                          00033700
*********************************************************************** 00033800
*        HEADINGS AND PRINT LINES                                     * 00033900
*********************************************************************** 00034000
HDNG1    DS    0CL67                                                    00034100
         DC    CL1'1'                                                   00034200
         DC    CL8'PROGRAM '                                            00034300
H1PGMID  DC    CL8' '                                                   00034400
         DC    CL40' ERROR MESSAGES'                                    00034500
         DC    CL4'PAGE'                                                00034600
H1PAGE   DC    CL6' '                                                   00034700
*                                                                       00034800
HDNG2    DS    0CL35                                                    00034900
         DC    CL1'0'                                                   00035000
         DC    CL34'PGM.      ERNO  CC  ERROR MESSAGES'                 00035100
*                                                                       00035200
HDNG3    DS    0CL121                                                   00035300
         DC    CL1' '                                                   00035400
         DC    CL20'********  ****  **'                                 00035500
         DC    100CL1'*'                                                00035600
*                                                                       00035700
HDNG4    DS    0CL32                                                    00035800
         DC    CL1'0'                                                   00035900
         DC    CL31'          END OF ERROR MESSAGES'                    00036000
*                                                                       00036100
TOTLINE1 DS    0CL48                                                    00036200
         DC    CL1'0'                                                   00036300
         DC    CL37'          TOTAL MESSAGES REPORTED ARE'              00036400
TOTERR   DC    XL10'40206B2020206B202120'                               00036500
*                                                                       00036600
TOTLINE2 DS    0CL48                                                    00036700
         DC    CL1'0'                                                   00036800
         DC    CL37'          HIGHEST CONDITION CODE IS  '              00036900
TOTHICC  DC    XL10'40206B2020206B212020'                               00037000
*                                                                       00037100
TOTLINE3 DS    0CL48                                          2602755   00037110
         DC    CL1'0'                                         2602755   00037120
         DC    CL37'          MAXIMUM SEVERITY CODE IS   '    2602755   00037130
TOTHISC  DC    XL10'40206B2020206B212020'                     2602755   00037140
*                                                                       00037150
DEFMSG   DC    CL49'ERROR MESSAGE NOT FOUND IN SI ERROR MESSAGE TABLE'  00037200
*                                                                       00037300
IOA1     DS    0CL121              PRINT FILE I/O AREA                  00037400
DETPCTL  DC    CL1' '                                                   00037500
DETPGMID DC    CL8' '                                                   00037600
         DC    CL2' '                                                   00037700
DETERNO  DC    CL4' '                                                   00037800
         DC    CL2' '                                                   00037900
DETERCC  DC    CL2' '                                                   00038000
         DC    CL2' '                                                   00038100
DETERMSG DC    CL100' '                                                 00038200
         EJECT                                                          00038300
*********************************************************************** 00038400
*        PRINT DCB                                                    * 00038500
*********************************************************************** 00038600
* THE FOLLOWING STATEMENT(S) MODIFIED BY                      2024183   00038699
PRTFILE  DCB   DCBE=PRTFIL2,LRECL=121,DSORG=PS,MACRF=PM,      2024183  X00038700
               RECFM=FBA,DDNAME=SIMESS                        2024183   00038710
PRTFIL2  DCBE  RMODE31=BUFF                                   2024183   00038720
         EJECT                                                          00038800
*********************************************************************** 00038900
*        STANDARD ERROR MESSAGE TABLE                                 * 00039000
*********************************************************************** 00039100
         COPY  SIDTERTB                                                 00039200
         EJECT                                                          00039300
*********************************************************************** 00039400
*        DSECTS                                                       * 00039500
*********************************************************************** 00039600
MSGAREA  DSECT                                                          00039700
MESSPGM  DS   CL8                                                       00039800
MESSERNO DS   CL4                                                       00039900
MESSERCC DS   XL2                                                       00040000
MESSMSG  DS   CL100                                                     00040100
         ORG   MESSMSG                                        2602755   00040110
         DS    CL15                                           2602755   00040120
MESSDASH DS    CL1                                            2602755   00040130
MESSERSC DS    CL2                                            2602755   00040140
         SPACE 3                                                        00040200
ERTBAREA DSECT                                                          00040300
ERTBLNG  DS   CL1                                                       00040400
ERTBERNO DS   CL4                                                       00040500
ERTBERCC DS   CL2                                                       00040600
ERTBSEND DS   CL1                                                       00040700
ERTBABND DS   CL1                                                       00040800
ERTBMSG  DS   CL100                                                     00040900
         ORG   ERTBMSG                                        2602755   00040910
         DS    CL15                                           2602755   00040920
ERTBDASH DS    CL1                                            2602755   00040930
ERTBERSC DS    CL2                                            2602755   00040940
SIMESS   CSECT                                                          00040996
         LTORG                                                          00040997
         DS    0D                                                       00040998
SITMSTMP DC    CL64'SIMESS    -----TSD-             02/14/14  12.08.16' 00040999
*        THIS PROGRAM CONTAINS TRADE SECRETS THAT BELONG                00041000
*        TO FIDELITY INFORMATION SERVICES AND IS                        00041001
*        LICENSED BY AN AGREEMENT.  ANY UNAUTHORIZED ACCESS,            00041002
*        USE, DUPLICATION, OR DISCLOSURE IS UNLAWFUL.                   00041003
*        COPYRIGHT FIDELITY INFORMATION SERVICES                        00041004
*        2014, ALL RIGHTS RESERVED.                                     00041005
*  00000186 00000187 00000188 00001510 00001520 00024610       9712121  00041006
         END                                                            00041200
