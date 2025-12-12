*     * 802262 12/11/20 PROYECTO UPGRADE GN172 EDVR
         TITLE 'BINARY SEARCH ROUTINE FOR LARGE SEQUENTIAL TABLES'      00000010
*---------------------------------------------------------------------* 00000020
*                   ** PROGRAM DESCRIPTION **                         * 00000030
*                                                                     * 00000040
* SIBINSRH IS THE BINARY SEARCH ROUTINE FOR LARGE SEQUENTIAL TABLES   * 00000050
*                                                                     * 00000060
* THIS ROUTINE RECEIVES TWO PARAMETERS. THE FIRST IS THE ADDRESS OF   * 00000070
* THE TABLE TO BE SEARCHED IN THE FORMAT SHOWN BELOW (OR BEGINNING    * 00000080
* FACTORS FOR THE DESIRED TABLE IF MULTIPLE TABLES IN A MODULE). THE  * 00000090
* SECOND IS A WORK AREA IN THE FORMAT DEFINED BELOW. THE ACTUAL SEARCH* 00000100
* FACTORS ARE CALCULATED THE FIRST TIME THE MODULE IS CALLED AND      * 00000110
* STORED BACK WHERE THE FACTORS WERE PASSED. THE CONSTANT WORD OF     * 00000120
* ZERO INDICATES THE NEED TO CALCULATE THESE SEARCH FACTORS.          * 00000130
*                                                                     * 00000140
*---------------------------------------------------------------------* 00000150
         EJECT                                                9913437   00000160
*---------------------------------------------------------------------* 00000170
*                  ** HISTORY OF REVISIONS **                         * 00000180
*                                                                     * 00000190
* DESCRIPTION                                                 CHNGID  * 00000200
* __________________________________________________________  _______ * 00000210
* 03/14/00 31-BIT                                             2024183 * 00001910
* 02/04/98 MODIFY FOR COBOL/VS AND COBOL/VSE                  9913741 * 00001912
* 04/02/97 MODIFY FOR 31-BIT                                  9913437 * 00001914
*                                                                     * 00001916
*---------------------------------------------------------------------* 00001918
         EJECT                                                9913437   00001920
SIBINSRH START 0                                                        00002000
SIBINSRH AMODE 31                                             2024183   00002100
SIBINSRH RMODE ANY                                            2024183   00002200
*                                                                       00012000
WORKAREA DSECT      **** DEFINE CALLING PROGRAM WORK AREA ****          00013000
WACODE   DS    CL1       ONE BYTE RETURN CODE                           00014000
*                          0-NOT FOUND IN TABLE                         00015000
*                          1-FOUND IN TABLE                             00016000
         DS    CL3       FILLER                                         00017000
WAENTRY  DS    CL256     UP TO 256 BYTES CORRESPONDING TO A TABLE ENTRY 00018000
*                             WHEN CALLED, THE SEARCH ARGUMENT WILL BE  00019000
*                             STORED HERE- UPON RETURN, THE TABLE ENTRY 00020000
*                             FOUND WILL BE STORED HERE IF CODE IS 1.   00021000
*                                                                       00022000
TABLE    DSECT      **** DEFINE FORMAT OF TABLE PHASE                   00023000
TBSTART  DS    A(TBENT1-TBSTART)   DISPLACEMENT TO FIRST TABLE ENTRY    00024000
TBSIZE   DS    A(TBEND-TBENT1)     SIZE OF TABLE IN BYTES               00025000
TBENTRY  DS    A(TBENT2-TBENT1)    SIZE OF EACH TABLE ENTRY             00026000
TBCMPR   DS    A(8)                NO OF BYTES TO COMPARE               00027000
         DS    F'0'                CONSTANT FULLWORD ZERO               00028000
*                                                                       00029000
* NOTE - THE ABOVE FIELDS ARE MODIFIED BY THE ROUTINE AFTER THE TABLE   00030000
* IS LOADED. ANYTHING BETWEEN THE ABOVE AND THE FOLLOWING ENTRIES ARE   00031000
* IGNORED. THE ABOVE MUST NOT BE ALTERED AFTER THE INITIAL CALL.        00032000
*                                                                       00033000
TBENT1   EQU   *                                                        00034000
         DS    CL20      FIRST TABLE ENTRY                              00035000
TBENT2   EQU   *                                                        00036000
         DS    CL20      SECOND TABLE ENTRY                             00037000
         DS    2000CL20  X NUMBER ADDITIONAL ENTRIES                    00038000
TBEND    EQU   *         END OF TABLE ENTRIES                           00039000
*                                                                       00040000
* NOTE - THE PRECEEDING SAMPLE WOULD CAUSE A TABLE OF 2002 TWENTY BYTE  00041000
*        ENTRIES TO BE SEARCHED ON THE FIRST 8 BYTES.                   00042000
*                                                                       00043000
SIBINSRH CSECT                                                          00044000
R1       EQU   1                                                        00045000
R2       EQU   2                                                        00046000
R3       EQU   3                                                        00047000
R4       EQU   4                                                        00048000
R5       EQU   5                                                        00049000
R6       EQU   6                                                        00050000
R7       EQU   7                                                        00051000
R8       EQU   8                                                        00052000
R9       EQU   9                                                        00053000
R10      EQU   10                                                       00054000
R11      EQU   11                                                       00055000
R12      EQU   12                                                       00056000
R13      EQU   13                                                       00057000
R14      EQU   14                                                       00058000
R15      EQU   15                                                       00059000
         USING *,R15                                                    00060000
         STM   R14,R12,12(R13)                                          00061000
         B     16(R15)                                                  00062000
         DC    CL8'SIBINSRH'                                            00063000
         LR    R12,R13                                                  00064000
         LA    R13,SAVEAREA                                             00065000
         ST    R12,SAVEAREA+4                                           00066000
         ST    R13,8(0,R12)                                             00067000
         LM    R2,R3,0(R1)                                              00068000
         USING TABLE,R2                                                 00069000
         USING WORKAREA,R3                                              00070000
         MVI   WACODE,C'0'         CLEAR RETURN CODE                    00071000
         LA    R4,SRCHLO           ADDR FOR LOW AND                     00072000
         LA    R5,SRCHHI                HIGH COMPARE                    00073000
         LM    R8,R12,TBSTART      LOAD BINARY SEARCH FACTORS           00074000
         LTR   R12,R12             HAVE THEY BEEN INITIALIZED YET       00075000
         BNZR  R4                  YES - GO START SEARCH LOW            00076000
*                                                                       00077000
* CALC BINARY SEARCH FACTORS FOR TABLE FROM PREDEFINED INFO             00078000
*                                                                       00079000
         AR    R8,R2          CALC BEGINNING TABLE ADDR                 00080000
         LR    R14,R10        CALC MAXIMUM BINARY SEARCH SIZE WHICH     00081000
TBLSET1  SLL   R14,1           * EXCEEDS ACTUAL TABLE                   00082000
         CR    R14,R9          *                                        00083000
         BNH   TBLSET1         *                                        00084000
         AR    R9,R8          CALC ADDR OF ACTUAL TABLE END             00085000
         LR    R12,R14        CALC FIRST SEARCH ADJUSTMENT TO GET TO    00086000
         SRL   R12,1          * BINARY SEARCH CENTER OF ACTUAL TABLE    00087000
         SR    R14,R10        CALC BEGIN ADDR TO ADJUST (BINARY SEARCH  00088000
         AR    R8,R14         * CENTER OF NEXT LARGER TABLE SIZE)       00089000
         BCTR  R11,0          SUBTRACT 1 FOR LENGTH CODE OF COMPARE     00090000
         STM   R8,R12,TBSTART SAVE CALCULATED SEARCH FACTORS            00091000
*                                                                       00092000
* BINARY SEARCH ROUTINE (FIRST ADJUSTMENT IS DOWN TO BEGIN SEARCH)      00093000
*                                                                       00094000
SRCHLO   SR    R8,R12              ADJUST SEARCH ADDR DOWN              00095000
         B     SEARCH                                                   00096000
SRCHHI   AR    R8,R12              ADJUST SEARCH ADDR UP                00097000
*                                                                       00098000
SEARCH   CR    R12,R10        IF SEARCH ADJUSTMENT LESS THAN ONE ENTRY  00099000
         BL    SRCHEXIT        * SIZE, SEARCH IS EXAUSTED               00100000
         SRL   R12,1          HALVE ADJUSTMENT FOR NEXT ADJUST          00101000
         CR    R8,R9          IF SEARCH ADDR IS BEYOND ACTUAL TABLE END 00102000
         BNLR  R4              * GO TO ADJUST DOWN                      00103000
         EX    R11,CLCINST    COMPARE SEARCH ARGUMENT TO TABLE          00104000
         BLR   R4             LOW-GO TO ADJUST DOWN                     00105000
         BHR   R5             HIGH-GO TO ADJUST UP                      00106000
*                                                                       00107000
* EQUAL COMPARE * SET CODE AND STORE TABLE ENTRY                        00108000
*                                                                       00109000
         MVI   WACODE,C'1'         SET CODE TO FOUND                    00110000
         BCTR  R10,0               MOVE TABLE ENTRY TO WORK             00111000
         EX    R10,MVCINST         *                                    00112000
SRCHEXIT EQU   *                   RESTORE CALLING REGISTERS            00113000
         L     R13,SAVEAREA+4                                           00114000
         LM    R14,R12,12(R13)                                          00115000
         SR    R15,R15                                                  00116000
         BSM   0,R14                                          2024183   00117000
*                                                                       00118000
* CONSTANTS AND WORKAREAS                                               00119000
*                                                                       00120000
CLCINST  CLC   WAENTRY(0),0(R8)    *EXECUTED*                           00121000
MVCINST  MVC   WAENTRY(0),0(R8)    *EXECUTED*                           00122000
SAVEAREA DC    18F'0'                                                   00123000
*                                                                       00124000
         LTORG                                                          00125000
SIBINSRH CSECT                                                          00125990
         LTORG                                                          00125991
         DS    0D                                                       00125992
SITMSTMP DC    CL64'SIBINSRH  -----TSD-             04/21/00  14.36.42' 00125993
*        THIS PROGRAM CONTAINS TRADE SECRETS THAT BELONG                00125994
*        TO ALLTEL INFORMATION SERVICES, INC. AND IS                    00125995
*        LICENSED BY AN AGREEMENT.  ANY UNAUTHORIZED ACCESS,            00125996
*        USE, DUPLICATION, OR DISCLOSURE IS UNLAWFUL.                   00125997
*        COPYRIGHT ALLTEL INFORMATION SERVICES, INC.                    00125998
*        2000, ALL RIGHTS RESERVED.                                     00125999
         END                                                            00126000
