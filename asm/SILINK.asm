*     * 802262 12/11/20 PROYECTO UPGRADE GN172 EDVR
         TITLE 'SILINK SUBROUTINE FOR MVS'                              00001000
************************************************************* 2024183   00001100
*          ***  REVISION HISTORY  ***                       * 2024183   00001200
*                                                           * 2024183   00001300
* DATE      DESCRIPTION                             CHG ID  * 2024183   00001400
* ________  _____________________________________   _______ * 2024183   00001500
* 03/01/00  ENABEL FOR 31-BIT ADDRESSING            ~~~4183 * 2024183   00001898
************************************************************* 2024183   00001900
SILINK   START                                                          00009000
         SIBASE BASEREG=(12),RWNUM=025,RWSUB=000,SETMODE=3124 2024183   00017000
         LR    R2,R1              SAVE R1                               00037000
         L     R3,0(0,R1)         LOAD ADDR OF SUBPROG NAME             00038000
         LA    R4,LCLTBL          POINT TO LOCAL TABLE                  00039000
         USING PGMTBL,R4                                                00040000
         USING PGMENTRY,R5                                              00041000
A0010    EQU   *                                                        00042000
         LA    R5,PGMOFS(0,R4)    POINT TO PROGRAM ENTRY                00043000
         LA    R6,10              SET LOOP COUNT                        00044000
A0020    EQU   *                                                        00045000
         CLC   PGMNAME,0(R3)      CHK FOR PROGRAM IN TABLE              00046000
         BE    A0100              FOUND - CALL IT                       00047000
         CLI   PGMNAME,X'00'      IS THIS SLOT OPEN                     00048000
         BE    A0050              YES - FILL IT IN                      00049000
         LA    R5,PGMLNG(0,R5)    BUMP TO NEXT ENTRY ADDR               00050000
         BCT   R6,A0020           LOOP BACK                             00051000
         LR    R0,R4              SAVE R4                               00052000
         ICM   R4,B'1111',NEXTTBL LOAD NEXT TABLE POINTER               00053000
         BNZ   A0010              LOOP BACK IF NOT ZEROS                00054000
*        ALL TABLES ARE FULL - ALLOCATE AND CHAIN IN A NEW TABLE        00055000
         LR    R4,R0              RESTORE R4                            00056000
         GETMAIN R,LV=128,LOC=ANY GET ANOTHER 128 BYTES       2024183   00057000
         ST    R1,NEXTTBL         SAVE POINTER TO NEW TABLE             00058000
         LR    R4,R1              POINT R4 TO NEW TABLE                 00059000
         XC    0(128,R4),0(R4)    CLEAR TABLE                           00060000
         LA    R5,PGMOFS(0,R4)    POINT TO PROGRAM ENTRY                00061000
A0050    EQU   *                                                        00062000
         LOAD  EPLOC=(3)          LOAD ROUTINE                          00063000
         MVC   PGMNAME,0(R3)      PUT NAME IN TABLE                     00064000
         ST    R0,PGMADDR         SAVE ENTRY ADDR                       00065000
         TR    ENTCNT,ADDONE      ADD ONE TO NO. ENTRIES IN TABLE       00075000
A0100    EQU   *                                                        00076000
         ICM   R15,15,PGMADDR     Q. CALLEE AMODE 31          2500295   00077000
         LA    R1,4(0,R2)         POINT TO SECOND PARAMETER IN LIST     00089000
         BASSM R14,R15            LINK TO ROUTINE             2024183   00090000
         SIRETRN RC=0             RETURN TO CALLING PROGRAM             00105000
         EJECT                                                          00106000
ADDONE   DC    XL12'0102030405060708090A0B0C'                           00111000
PGMPTR   DC    A(LCLTBL)          POINTER TO NEXT FREE TABLE ENTRY      00112000
LCLTBL   DC    XL128'00'          LOCAL PROGRAM TABLE                   00113000
         LTORG                                                          00114000
         SPACE 3                                                        00115000
PGMTBL   DSECT                                                          00116000
NEXTTBL  DC    F'0'                                                     00117000
ENTCNT   DC    X'00'              NO. OF ENTRIES IN THIS TABLE          00118000
         DC    XL3'00'                                                  00119000
PGMOFS   EQU   *-PGMTBL                                                 00120000
PGMENTRY DSECT                                                          00121000
PGMNAME  DC    XL8'00'                                                  00122000
PGMADDR  DC    XL4'00'                                                  00123000
PGMLNG   EQU   *-PGMENTRY                                               00124000
SILINK   CSECT                                                          00124996
         LTORG                                                          00124997
         DS    0D                                                       00124998
SITMSTMP DC    CL64'SILINK    -----TSD-             08/18/00  11.31.47' 00124999
*        THIS PROGRAM CONTAINS TRADE SECRETS THAT BELONG                00125000
*        TO ALLTEL INFORMATION SERVICES, INC. AND IS                    00125001
*        LICENSED BY AN AGREEMENT.  ANY UNAUTHORIZED ACCESS,            00125002
*        USE, DUPLICATION, OR DISCLOSURE IS UNLAWFUL.                   00125003
*        COPYRIGHT ALLTEL INFORMATION SERVICES, INC.                    00125004
*        2000, ALL RIGHTS RESERVED.                                     00125005
         END                                                            00125200
