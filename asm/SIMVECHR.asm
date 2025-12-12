*ASM XOPTS(NOEPILOG)                                                    00000100
*     * 802262 12/11/20 PROYECTO UPGRADE GN172 EDVR
**********************************************************************  00000120
*                      HISTORY OF REVISIONS                          *  00000121
*DESCRIPTION                                                  CHG ID *  00000122
*--------------------------------------------------------------------*  00000123
* 06/21/11 ADDED SIRENT FOR THREADSAFE COMPLIANCE             1216075   00000181
* 03/01/00 ENABLE FOR 31-BIT ADDRESSING                       2024183*  00000182
* 02/04/98 MODIFIED FOR COBOL/VS AND COBOL/VSE                9913741 * 00000183
* 08/11/97 SET RC FOR COBOL/VSE.                              9913437 * 00000185
*05/15/96  ADDED CAPABILITY FOR 31-BIT ADDRESSING             9913165*  00000187
*                                                                    *  00000188
**********************************************************************  00000189
         LCLB  &CICS                                                    00000200
         AIF   ('&SYSPARM' NE 'CICS').GEN1                              00000300
         SIRENT                        ;                      1216075   00000305
&CICS    SETB  1                                                        00000400
         TITLE 'FIMVECHR - VARIABLE-LENGTH MOVE ROUTINE'                00000500
FIMVECHR DFHEIENT CODEREG=(12),DATAREG=(13),EIBREG=(11)                 00000600
FIMVECHR AMODE 31                                             9913165 / 00000635
FIMVECHR RMODE ANY                                            9913165 / 00000640
         SIEQREG                                                        00000700
         L     R1,DFHEICAP         LOAD ADDR OF COMMON AREA             00000800
         B     *+24                                                     00000900
         DC    CL20'FIMVECHR(SIMVECHR)'                                 00001000
         AGO   .GEN2                                                    00001100
.GEN1    ANOP                                                           00001200
         TITLE 'SIMVECHR - VARIABLE-LENGTH MOVE ROUTINE'                00001300
SIMVECHR START                                                          00001400
         SIBASE BASEREG=12                                              00001500
.GEN2    ANOP                                                           00001600
         L     2,0(0,1)                 GET TO FIELD ADDR               00001700
         L     4,4(0,1)                 GET FROM FIELD ADDR             00001800
         L     3,8(0,1)                 GET LENGTH FIELD ADDR           00001900
         SR    5,5                      CLEAR REG 5                     00002000
         ICM   5,B'0011',0(3)           GET LENGTH                      00002100
         BZ    ENDOP                    EXIT IF ZERO                    00002200
         LR    3,5                      GET LENGTH                      00002300
         MVCL  2,4                      MOVE                            00002400
         BNO   ENDOP                    EXIT IF MOVE OK - NO OVERLAP    00002500
         SRA   5,8                      DIVIDE LENGTH BY 256            00002600
         LTR   5,5                      CHK FOR NOT GT 256              00002700
         BZ    LESS256                                                  00002800
         BM    ENDOP                                                    00002900
         LA    6,256                    SET CONSTANT 256 IN REG 6       00003000
MOVE1    MVC   0(256,2),0(4)            MOVE 256 BYTES                  00003100
         SR    3,6                      SUBTRACT 256 FROM LENGTH        00003200
         AR    2,6                      ADD 256 TO TO-FIELD ADDR        00003300
         AR    4,6                      ADD 256 TO FROM-FIELD ADDR      00003400
         BCT   5,MOVE1                                                  00003500
LESS256  BCTR  3,0                      SUBTRACT 1 FROM REMAINING LNG   00003600
         LTR   3,3                      CHK FOR LNG NEGATIVE            00003700
         BM    ENDOP                    GO TO ENDOP IF NO REMAINDER     00003800
         EX    3,MVCINST                EXECUTE MOVE INSTRUCTION        00003900
ENDOP    EQU   *                                                        00004000
         AIF   (&CICS).CICSRET                                          00004100
         SIRETRN RC=0              RETURN                     9913437   00004200
         AGO   .DATA                                                    00004300
.CICSRET ANOP                                                           00004400
         EXEC  CICS RETURN                                              00004500
         LTORG                                                          00004600
.DATA    ANOP                                                           00004700
MVCINST  MVC   0(0,2),0(4)              MOVE REMAINDER                  00004800
         AIF   ('&SYSPARM' NE 'CICS').DTSBAT                            00004885
FIMVECHR CSECT                                                          00004886
         AGO   .DTSMRG                                                  00004887
.DTSBAT  ANOP                                                           00004888
SIMVECHR CSECT                                                          00004889
.DTSMRG  ANOP                                                           00004890
         LTORG                                                          00004891
         DS    0D                                                       00004892
SITMSTMP DC    CL64'SIMVECHR  -----TSD-             10/24/11  09.49.18' 00004893
*        THIS PROGRAM CONTAINS TRADE SECRETS THAT BELONG                00004894
*        TO FIDELITY INFORMATION SERVICES AND IS                        00004895
*        LICENSED BY AN AGREEMENT.  ANY UNAUTHORIZED ACCESS,            00004896
*        USE, DUPLICATION, OR DISCLOSURE IS UNLAWFUL.                   00004897
*        COPYRIGHT FIDELITY INFORMATION SERVICES                        00004898
*        2011, ALL RIGHTS RESERVED.                                     00004899
         END                                                            00004900
