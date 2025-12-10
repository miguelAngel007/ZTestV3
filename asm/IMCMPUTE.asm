*ASM XOPTS(NOEPILOG)                                                    00000001
         SIRENT                                                         00000004
*     * FO5238 * 06/26/11 PROYECTO REBORN
*---------------------------------------------------------------------* 00000006
*                   ** PROGRAM DESCRIPTION **                         * 00000008
* PROGRAM IMCMPUTE - VARIABLE LENGTH ADD DECIMAL                      * 00000012
*                                                                     * 00000014
*---------------------------------------------------------------------* 00000016
         EJECT                                                          00000092
         LCLB  &CICS                                                    00000094
         SIEQREG                                                        00000096
         AIF   ('&SYSPARM' NE 'CICS').BTCHDTA                           00000098
DFHEISTG DSECT                                                          00000100
         AGO   .DATAMRG                                                 00000102
.BTCHDTA ANOP                                                           00000104
         PRINT ON,GEN                                                   00000106
         CEECAA                                                         00000108
         EJECT                                                          00000110
         CEEDSA                                                         00000112
         EJECT                                                          00000114
.DATAMRG ANOP                                                           00000116
UDSABEG  DS    0D                  BEGIN OF USER DSA                    00000118
PGMPLIST DS    F                   ORIGINAL R1 SAVE AREA                00000120
#UDSALEN EQU   *-UDSABEG           LENGTH OF DSA                        00000122
         AIF   ('&SYSPARM' NE 'CICS').GEN1                              00000124
&CICS    SETB  1                                                        00000126
         TITLE 'FIIMCMPT - IM VARIABLE LENGTH ADD DECIMAL ROUTINE'      00000128
FIIMCMPT DFHEIENT CODEREG=(12),DATAREG=(13),EIBREG=(11)                 00000130
FIIMCMPT AMODE 31                                                       00000137
FIIMCMPT RMODE ANY                                                      00000138
         L     R1,DFHEICAP         LOAD ADDR OF COMMON AREA             00000145
         B     *+24                                                     00000146
         DC    CL20'FIIMCMPT(IMCMPUTE)'                                 00000150
         AGO   .GEN2                                                    00000155
* THE FOLLOWING STATEMENT(S) MODIFIED BY                                00000159
.GEN1    ANOP                                                           00000160
#DSALEN  EQU   *-CEEDSA            LENGTH OF DSA                        00000170
         TITLE 'IMCMPUTE - IM VARIABLE LENGTH ADD DECIMAL ROUTINE'      00000190
IMCMPUTE CEEENTRY ,                                                    X00000195
               PPA=PPA,                                                X00000200
               AUTO=#DSALEN,                                           X00000205
               MAIN=NO,                                                X00000210
               NAB=NO,                                                 X00000215
               PARMREG=1,                                              X00000220
               BASE=9                                                   00000225
         XR    R15,R15                                                  00000230
         B     INIT000                                                  00000235
         USING CEECAA,R12                                               00000240
* ------------------------------------------------------------------- * 00000245
*                  PROLOG AREA                                        * 00000250
* ------------------------------------------------------------------- * 00000255
PPA      CEEPPA ,                                                      X00000260
               LIBRARY=NO,                                             X00000265
               PPA2=YES,                                               X00000270
               EXTPROC=YES,                                            X00000275
               TSTAMP=YES,                                             X00000280
               PEP=YES,                                                X00000285
               INSTOP=YES,                                             X00000290
               EPNAME=IMCMPUTE,                                        X00000295
               VER=01,                                                 X00000300
               REL=01,                                                 X00000305
               MOD=00,                                                 X00000310
               DSA=YES                                                  00000315
         SPACE 2                                                        00000320
         LTORG                                                          00000325
INIT000  EQU   *                                                        00000330
         ST    R1,PGMPLIST                                              00000335
.GEN2    ANOP                                                           00000400
BEGIN    LM    2,5,0(1)       GET PARAMETERS FROM REG1                  00000500
         LH    6,0(4)         GET TO FIELD LENGTH                       00000600
         LR    7,6            MOVE TO LENGTH TO FROM-LENGTH             00000700
         LTR   4,4            CHECK FOR FROM-LENGTH                     00000800
         BM    *+8            BRANCH AROUND FROM-LENGTH                 00000900
         LH    7,0(5)         GET FROM-LENGTH                           00001000
         BCTR  7,0            SUBTRACT 1 FROM FROM-LENGTH               00001100
         BCTR  6,0            SUBTRACT 1 FROM TO-LENGTH                 00001200
         SLA   6,4            SHIFT LEFT 4 BITS                         00001300
         OR    6,7            OR LENGTHS INTO 1 BYTE OF REG 6           00001400
         EX    6,ADD          EXECUTE ADD INSTRUCTIONS                  00001500
ENDOP    EQU   *                                                        00001510
         AIF   (&CICS).CICSRET                                          00001520
         CEETERM RC=(15)           RETURN                               00001600
         SPACE 2                                                        00001700
         AGO   .DATA                                                    00001710
.CICSRET ANOP                                                           00001720
         EXEC  CICS RETURN                                              00001730
.DATA    ANOP                                                           00001740
ADD      AP   0(0,2),0(0,3)                                             00001800
         LTORG                                                          00001810
         AIF   ('&SYSPARM' NE 'CICS').DTSBAT                            00001885
FIIMCMPT CSECT                                                          00001886
         AGO   .DTSMRG                                                  00001887
.DTSBAT  ANOP                                                           00001888
IMCMPUTE CSECT                                                          00001889
.DTSMRG  ANOP                                                           00001890
         LTORG                                                          00001891
         DS    0D                                                       00001892
SITMSTMP DC    CL64'IMCMPUTE  -----TSD-             01/31/07  13.21.09' 00001893
*        THIS PROGRAM CONTAINS TRADE SECRETS THAT BELONG                00001894
*        TO FIDELITY INFORMATION SERVICES AND IS                        00001895
*        LICENSED BY AN AGREEMENT.  ANY UNAUTHORIZED ACCESS,            00001896
*        USE, DUPLICATION, OR DISCLOSURE IS UNLAWFUL.                   00001897
*        COPYRIGHT FIDELITY INFORMATION SERVICES                        00001898
*        2007, ALL RIGHTS RESERVED.                                     00001899
         END                                                            00001900
