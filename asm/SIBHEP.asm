*ASM XOPTS(NOEPILOG)                                          2024448   00000040
*     * 802262 12/11/20 PROYECTO UPGRADE GN172 EDVR
         SIRENT                                               2024448   00000042
         LCLB  &CICS                                          2024448   00000044
         SIEQREG                                              2024448   00000046
         AIF   ('&SYSPARM' NE 'CICS').BTCHDTA                 2024448   00000050
* THE FOLLOWING STATEMENT(S) MODIFIED BY                      2024448   00000055
DFHEISTG DSECT                                                          00000056
         AGO   .DATAMRG                                       2024448   00000058
* THE FOLLOWING STATEMENT(S) MODIFIED BY                      2024448   00000059
.BTCHDTA ANOP                                                           00000060
         PRINT ON,GEN                                         2024448   00000062
* THE FOLLOWING STATEMENT(S) MODIFIED BY                      2024448   00000063
         CEECAA                                                         00000064
         EJECT                                                2024448   00000066
* THE FOLLOWING STATEMENT(S) MODIFIED BY                      2024448   00000067
         CEEDSA                                                         00000068
         EJECT                                                2024448   00000070
* THE FOLLOWING STATEMENT(S) MODIFIED BY                      2024448   00000071
.DATAMRG ANOP                                                           00000072
UDSABEG  DS    0D                  BEGIN OF USER DSA          2024448   00000074
PGMPLIST DS    F                   ORIGINAL R1 SAVE AREA      2024448   00000076
IOADDR   DS    F                   ADDR OF OUTPUT RECORD      2024448   00000078
ENDADDR  DS    F                   END OF INPUT RECORD        2024448   00000080
* THE FOLLOWING STATEMENT(S) MODIFIED BY                      2024448   00000081
ENDCURR  DS    F                   END OF CURRENT OCCURRENCE DATA       00000082
SVLENG   DS    H            INPUT RECORD LENGTH               2024448   00000084
OCCURLNG DS    H            LENGTH OF EACH OCCURRENCE         2024448   00000086
FIXARLNG DS    H            LENGTH OF FIXED AREA              2024448   00000088
* THE FOLLOWING STATEMENT(S) MODIFIED BY                      2024448   00000089
ORIGRLNG DS    H            LENGTH OF ORIGINAL RECORD BEFORE COMPRESSN  00000090
#UDSALEN EQU   *-UDSABEG           LENGTH OF DSA              2024448   00000092
         AIF   ('&SYSPARM' NE 'CICS').GEN1                    2024448   00000094
&CICS    SETB  1                                              2021627   00000100
         TITLE 'SIBHEP  - EXPAND THE BALANCE HISTORY REC'     2021627   00000102
FIBHEP   DFHEIENT CODEREG=(12),DATAREG=(13),EIBREG=(11)       2024183   00000106
FIBHEP   AMODE 31                                             2024183   00000108
FIBHEP   RMODE ANY                                            2024183   00000110
         L     R1,DFHEICAP         LOAD ADDR OF COMMON AREA             00000114
         B     *+24                                                     00000116
         DC    CL20'FIBHEP  (SIBHEP  )'                                 00000118
         AGO   .GEN2                                                    00000120
.GEN1    ANOP                                                           00000122
         TITLE 'SIBHEP  - EXPAND THE BALANCE HISTORY REC'               00000124
#DSALEN  EQU   *-CEEDSA            LENGTH OF DSA              2024448   00000146
* THE FOLLOWING STATEMENT(S) MODIFIED BY                      2024448   00000147
SIBHEP CEEENTRY ,                                                      X00000148
               PPA=PPA,                                                X00000150
               AUTO=#DSALEN,                                           X00000152
               MAIN=NO,                                                X00000154
               NAB=NO,                                                 X00000156
               PARMREG=1,                                              X00000158
               BASE=4                                                   00000160
         XR    R15,R15                                        2024448   00000162
         B     INIT000                                        2024448   00000164
         USING CEECAA,R12                                     2024448   00000166
* ---------------------------------------------------------- *2024448   00000168
*                  PROLOG AREA                               *2024448   00000170
* ---------------------------------------------------------- *2024448   00000172
* THE FOLLOWING STATEMENT(S) MODIFIED BY                      2024448   00000173
PPA      CEEPPA ,                                                      X00000174
               LIBRARY=NO,                                             X00000176
               PPA2=YES,                                               X00000178
               EXTPROC=YES,                                            X00000180
               TSTAMP=YES,                                             X00000182
               PEP=YES,                                                X00000184
               INSTOP=YES,                                             X00000186
               EPNAME=SIBHEP,                                          X00000188
               VER=01,                                                 X00000190
               REL=01,                                                 X00000192
               MOD=00,                                                 X00000194
               DSA=YES                                                  00000196
         SPACE 2                                              2024448   00000198
         LTORG                                                2024448   00000200
INIT000  EQU   *                                              2024448   00000202
         ST    R1,PGMPLIST                                    2024448   00000204
* THE FOLLOWING STATEMENT(S) MODIFIED BY                      2024448   00000229
.GEN2    ANOP                                                           00000230
*---------------------------------------------------------------------* 00000232
*                   ** PROGRAM DESCRIPTION **                         * 00000234
*                 EXPAND THE BALANCE HISTORY REC                      * 00000236
*                                                                     * 00000238
*---------------------------------------------------------------------* 00000240
*                  ** HISTORY OF REVISIONS **                         * 00000242
* DESCRIPTION                                                 CHNGID  * 00000244
* __________________________________________________________  _______ * 00000246
*                                                                     * 00000248
* 04/05/02 LE ENABLED                                         2024448   00000982
* 12/06/01 MAKE PUNCH CONDITIONAL                             2024422   00000984
* 06/29/01 CHANGES FOR REENTRANT                              2021627 * 00000986
* 01/03/00 ADD 31-BIT PROCESSING                              2024183 * 00000988
* 05/04/98 CORRECT TRANS/ISO ABEND TO USE LENGTH OF RECORD    9913824 * 00000990
* 10/09/97 INITIAL DISTRIBUTION.                              9913615 * 00000992
*                                                                     * 00000994
*---------------------------------------------------------------------* 00000996
         EJECT                                                          00000998
*********************************************************************** 00017000
*                                                                     * 00018000
*        THIS ROUTINE TAKES A RECORD THAT WAS COMPRESSED BY THE       * 00019000
*  ROUTINE SIBHCP AND EXPANDS IT.  DUPLICATE DATA FROM ONE OCCURRENCE * 00020000
*  TO ANOTHER IS INDICATED BY THE SEQUENCE X'FFLL' WHERE LL IS        * 00021000
*  THE NUMBER OF BYTES DELETED FROM THE RECORD.  THE PROGRAM RETURNS  * 00022000
*  AN EXPANDED RECORD IN THE WORKING STORAGE AREA.  IF THE LENGTH     * 00023000
*  OF THE COMPRESSED RECORD EQUALS THE EXPANDED RECORD THE PROGRAM    * 00024000
*  DOES NOT TRY TO EXPAND IT.  THE EXPAND AND COMPRESS PROGRAMS       * 00025000
*  ASSUME A RECORD WITH FIXED DATA FOLLOWED BY OCCURRENCES OF DATA.   * 00026000
*                                                                     * 00027000
*  IF THE RECORD IS COMPRESSED, 6 BYTES WERE ADDED TO THE             * 00028000
*  END OF THE RECORD CONTAINING THE FACTORS USED IN COMPRESSION.      * 00029000
*  THE FORMAT OF THE 6 BYES IS X'AABBBBCCCCFF' WHERE                  * 00030000
*           AA   IS THE LENGTH OF EACH OCCURRENCE                     * 00031000
*           BBBB IS THE LENGTH OF THE FIXED AREA                      * 00032000
*           CCCC IS THE LENGTH OF THE ORIGINAL RECORD                 * 00033000
*           FF   IS A BYTE OF X'FF' INDICATING A COMPRESSED RECORD    * 00034000
*                                                                     * 00035000
*  IF THE PARAMETERS TO THE PROGRAM ARE INCORRECT, THE PROGRAM WILL   * 00036000
*  RETURN LOW VALUES IN THE LENGTH FIELD OF THE WORKING STORAGE       * 00037000
*  RECORD AND THE RECORD WILL LOOK LIKE THE COMPRESSED RECORD.        * 00038000
*                                                                     * 00039000
*        THE PROGRAM EXPECTS THE FOLLOWING PARAMETERS:                * 00040000
*                                                                     * 00041000
*              A) WORKING-STORAGE-REC  PIC X(  ). (INCLUDES LENGTH)   * 00042000
*              B) COMPRESSED-RECORD    PIC X(  ). (WITH LENGTH)       * 00043000
*                                                                     * 00044000
*        REGISTER USAGE                                               * 00045000
*                                                                     * 00046000
*              0                                                      * 00047000
*              1    POINTS TO USERS ADCON TABLE                       * 00048000
*              2    ADDRESS OF WORKING STORAGE RECORD                 * 00049000
*              3                                                      * 00050000
*              4                                                      * 00051000
*              5                                                      * 00052000
*              6    WORK REGISTER - POSITION IN INPUT RECORD          * 00053000
*              7    WORK REGISTER - POSITION IN OUTPUT RECORD         * 00054000
*              8                                                      * 00055000
*              9                                                      * 00056000
*              10                                                     * 00057000
*              11                                                     * 00058000
*              12   BASE REGISTER                                     * 00059000
*              13   ADDRESS OF USERS REGISTER SAVE AREA               * 00060000
*              14   RETURN ADDRESS TO USERS PROGRAM                   * 00061000
*              15   ENTRY POINT FOR THIS ROUTINE                      * 00062000
*                                                                     * 00063000
*********************************************************************** 00064000
         EJECT                                                          00065000
         LM    R2,R3,0(R1)         LOAD USER PARAMETERS                 00066000
         ST    R3,IOADDR           ADDR OF COMPRESSED RECORD            00067000
         NI    IOADDR,X'7F'        RESET H.O. BIT FROM LAST ADDR LIST   00068000
         CLC   0(2,R3),0(R2)       COMPARE REC LNG OF THE 2 RECORDS     00069000
         BH    ERROR               COMPRESSED REC GR THAN WS AREA       00070000
         BE    NOEXPAND            EQUAL - DO NOT EXPAND                00071000
*   FIND THE COMPRESSION FACTORS AT THE END OF THE COMPRESSED RECORD    00072000
         L     R5,IOADDR           ADDR OF COMPRESSED RECORD            00073000
         AH    R5,0(R3)            ADD RECORD LENGTH                    00074000
         SH    R5,=H'6'            BACK UP TO LAST 6 BYTES OF REC       00075000
         CLI   5(R5),X'FF'         IS LAST BYTE A COMPRESS INDICATOR    00076000
         BNE   NOEXPAND            NO - DO NOT EXPAND                   00077000
         SR    R9,R9               CLEAR R9                             00078000
         IC    R9,0(R5)            OBTAIN LENGTH OF OCCURRENCE          00079000
         STH   R9,OCCURLNG         SAVE LENGTH OF OCCURRENCE            00080000
         MVC   FIXARLNG,1(R5)      LENGTH OF FIXED AREA                 00081000
         MVC   ORIGRLNG,3(R5)      LENGTH OF ORIG REC BEFORE COMPRESSN  00082000
*   SAVE LENGTH OF WORKING STORAGE AREA                                 00083000
         LH    R11,0(R2)           RECORD LENGTH FROM WORKG STORAGE REC 00084000
         STH   R11,SVLENG          SAVE RECORD LENGTH                   00085000
*   CHECK OUT THE VALIDITY OF THE COMPRESSION FACTORS                   00086000
         LH    R11,ORIGRLNG        LENGTH OF ORIG RECORD                00087000
         SH    R11,=H'4'           REDUCE REC LNG BY LENGTH FIELD       00088000
         SH    R11,FIXARLNG        SUBTR FIXED AREA                     00089000
         SR    R10,R10             SET EVEN REG OF PAIR TO ZERO         00090000
         LH    R9,OCCURLNG         LOAD DIVISOR - OCCURRENCE LENGTH     00091000
         LTR   R9,R9               TEST DIVISOR FOR ZERO                00092000
         BZ    ERROR               BR ERROR                             00093000
         DR    R10,R9         DIVIDE OCCUR LNG INTO LENG OF OCCUR AREA  00094000
         LTR   R10,R10        TEST REMAINDER FOR ZERO                   00095000
         BNZ   ERROR             BR NOT ZERO TO ERROR                   00096000
*    SET UP REGISTERS FOR POINTING TO DATA IN RECORDS                   00097000
         L     R6,IOADDR           LOAD ADDR OF COMPR REC IN R6         00098000
         LA    R6,4(R6)            BUMP PAST RECORD LENGTH              00099000
         LR    R7,R2               LOAD ADDR OF WORK ST REC IN R7       00100000
         LA    R7,4(R7)            BUMP PAST OUTPUT REC LENGTH          00101000
*    MOVE FIXED PORTION OF RECORD TO OUTPUT AREA                        00102000
         LR    R8,R7               SET WITH ADDRESS OF 'TO' AREA        00103000
         LH    R9,FIXARLNG         SET LENGTH OF FIXED AREA             00104000
         LR    R10,R6              SET R10 WITH ADDRESS OF 'FROM' AREA  00105000
         LR    R11,R9              SET LENGTH OF FIXED DATA             00106000
         MVCL  R8,R10              MAKE MOVE OF FIXED AREA              00107000
*   SET POINTERS TO POS IN RECORD PAST THE FIXED DATA                   00108000
*    (AFTER MCVL THE REGISTERS SHOULD BE AT THE CORRECT LOCATION)       00109000
         LR    R6,R10              INPUT RECORD                         00110000
         LR    R7,R8               OUTPUT RECORD                        00111000
*   CALCULATE THE ADDRESS OF THE END OF THE INPUT RECORD                00112000
         L     R5,IOADDR           ADDR OF BEGINNING OF RECORD          00113000
         L     R8,IOADDR           ADDR OF BEGINNING OF RECORD          00114000
         AH    R8,0(R5)            ADD RECORD LENGTH                    00115000
         SH    R8,=H'7'      REDUCE BY COMPRESS FACTORS (6) + 1         00116000
         ST    R8,ENDADDR          SAVE END ADDRESS                     00117000
*   NOW MOVE THE 1ST OCCURRENCE FROM THE INPUT TO OUTPUT                00118000
         IC    R9,OCCURLNG+1       MOVE LENGTH OF OCCURRENCE            00119000
         SH    R9,=H'1'            MAKE REL TO 0 FOR MOVE CHAR          00120000
         EX    R9,EXECMVC7         MOVE 1ST OCCURRENCE TO OUTPUT        00121000
*   INCREASE THE POINTERS PAST THE 1ST OCCURRENCE                       00122000
         AH    R6,OCCURLNG         INPUT RECORD                         00123000
         AH    R7,OCCURLNG         OUTPUT RECORD                        00124000
*                                                                       00125000
*                                                                       00126000
PROCLOOP EQU   *                                                        00127000
*   SEE IF THE END OF INPUT HAS BEEN REACHED                            00128000
         C     R6,ENDADDR          CURRENT POSITION VS END OF RECORD    00129000
         BH    PROCEND             BR IF END OF RECORD                  00130000
*   CHECK THIS CHARACTER FOR HEX FF - THIS INDICATES THAT DUPLICATE     00131000
*                                   DATA HAS BEEN DELETED               00132000
         CLI   0(R6),X'FF'         IS THIS CHAR HEX FF                  00133000
         BE    HEXFF               BR TO PROCESS                        00134000
MOVE1CHR EQU   *                                                        00135000
         MVC   0(1,R7),0(R6)       MOVE CHARACTER TO OUTPUT RECORD      00136000
         LA    R6,1(R6)            NEXT POS IN INPUT                    00137000
         LA    R7,1(R7)            NEXT POS IN OUTPUT                   00138000
         B     PROCLOOP                                                 00139000
*                                                                       00140000
*                                                                       00141000
*    PROCESS IDENTICAL DATA BETWEEN OCCURRENCES                         00142000
HEXFF    EQU   *                                                        00143000
         LR    R8,R7          LOAD WORK REG R8 WITH ADDR OF OUT POS     00144000
         SH    R8,OCCURLNG    POINT BACK TO THE PREV OCCURRENCE IN OUT  00145000
         SR    R9,R9               CLEAR R9                             00146000
         IC    R9,1(R6)            GET COUNT OF CHAR DELETED IN R9      00147000
         BCTR  R9,0                MAKE REL TO 0                        00148000
         EX    R9,EXECMVC  MOVE DELETED DATA FROM LAST OCCUR TO THIS 1  00149000
         IC    R9,1(R6)            GET COUNT OF CHAR DELETED IN R9      00150000
         AR    R7,R9       BUMP POS IN OUTPUT BY THE NUM BYTES MOVED    00151000
         LA    R6,2(R6)    POINT TO CHAR IN INPUT PAST THE FFLL SEQUEN  00152000
         B     PROCLOOP                                                 00153000
*                                                                       00154000
PROCEND  EQU   *                                                        00155000
*    END OF INPUT RECORD - SHOULD BE LIKE IT WAS BEFORE COMPRESSION     00156000
*    CALC THE WORKING STORAGE RECORD LENGTH                             00157000
         LR    R9,R7               R7 IS POS PAST THE LAST CHAR OF REC  00158000
         SR    R9,R2               SUBT BEGINNING ADDR OF RECORD        00159000
*    RECORD LENGTH SHOULD EQUAL THE ORIGINAL RECORD LENGTH              00160000
         CH    R9,ORIGRLNG         COMPARE W/S LNG TO ORIG LNG          00161000
         BNE   ERROR                                                    00162000
         STH   R9,0(R2)            PUT LENGTH IN WORK STORAGE RECORD    00163000
         B     GOBACK                                                   00164000
*                                                                       00165000
*    USE THE INPUT RECORD AS OUTPUT                                     00166000
NOEXPAND EQU   *                                                        00167000
         LR    R8,R2               SET WITH ADDRESS OF 'TO' AREA        00168000
         LH    R9,0(R3)            LENGTH OF INPUT RECORD     9913824   00169000
         L     R10,IOADDR          SET R10 WITH ADDRESS OF 'FROM' AREA  00170000
         LR    R11,R9              SET LENGTH OF MOVE                   00171000
         MVCL  R8,R10              MOVE INPUT RECORD TO OUTPUT          00172000
         B     GOBACK                                                   00173000
*                                                                       00174000
*    IF THERE IS AN ERROR IN PARAMETERS, PUT INPUT RECORD IN WORKING    00175000
*       STORAGE AREA AND SET WORKING STORAGE RECORD LENGTH TO ZERO      00176000
ERROR    EQU   *                                                        00177000
         LR    R8,R2               SET WITH ADDRESS OF 'TO' AREA        00178000
         LH    R9,0(R3)            LENGTH OF INPUT RECORD     9913824   00179000
         L     R10,IOADDR          SET R10 WITH ADDRESS OF 'FROM' AREA  00180000
         LR    R11,R9              SET LENGTH OF RECORD                 00181000
         MVCL  R8,R10              MOVE INPUT RECORD TO OUTPUT          00182000
         MVC   0(2,R2),=X'0000'    SET LENGTH OF RECORD TO ZERO         00183000
*                                                                       00184000
GOBACK   EQU   *                                                        00185000
         AIF   (&CICS).CICSRET                                          00186000
         CEETERM RC=(15)           RETURN                     2024448   00186005
         AGO   .DATA                                                    00188000
.CICSRET ANOP                                                           00189000
         EXEC  CICS RETURN                                              00190000
         LTORG                                                          00191000
.DATA    ANOP                                                           00192000
         SPACE 3                                                        00193000
EXECMVC  MVC   0(0,R7),0(R8)  MOVE FROM R8 LOC TO OUTPUT RECORD         00194000
EXECMVC7 MVC   0(0,R7),0(R6)  MOVE FROM INPUT RECORD TO R7 LOC          00195000
         AIF   ('&SYSPARM' NE 'CICS').DTSBAT                            00205985
FIBHEP   CSECT                                                          00205986
         AGO   .DTSMRG                                                  00205987
.DTSBAT  ANOP                                                           00205988
SIBHEP   CSECT                                                          00205989
.DTSMRG  ANOP                                                           00205990
         LTORG                                                          00205991
         DS    0D                                                       00205992
SITMSTMP DC    CL64'SIBHEP    -----TSD-             06/26/02  07.22.54' 00205993
*        THIS PROGRAM CONTAINS TRADE SECRETS THAT BELONG                00205994
*        TO ALLTEL INFORMATION SERVICES, INC. AND IS                    00205995
*        LICENSED BY AN AGREEMENT.  ANY UNAUTHORIZED ACCESS,            00205996
*        USE, DUPLICATION, OR DISCLOSURE IS UNLAWFUL.                   00205997
*        COPYRIGHT ALLTEL INFORMATION SERVICES, INC.                    00205998
*        2002, ALL RIGHTS RESERVED.                                     00205999
         END                                                            00206000
