*     * FO5238 * 06/26/11 PROYECTO REBORN
IMACTEDT START 0                                                        00000100
         SIBASE BASEREG=(12),ID=IMACTEDT                        OS50001 00000200
* THE FOLLOWING STATEMENT(S) MODIFIED BY                      0417078   00000299
         LM    3,6,0(1)            LOAD ADDRESSES OF PARAMETERS         00000300
* THE FOLLOWING STATEMENT(S) MODIFIED BY                      0417078   00000309
         CLI   0(6),C'2'           IS THIS A 12 DIGIT ACCOUNT NUMBER    00000310
         BE    ACCT12                                         0417078   00000320
         PACK  ACCTPK,4(10,3)      PACK ACCOUNT NUMBER        0417078   00000400
         LA    7,ACCTPK+1                                     0417078   00000500
         B     LOADACCT                                       0417078   00000510
ACCT12   PACK  ACCTPK,2(12,3)      PACK ACCOUNT NUMBER        0417078   00000520
         LA    7,ACCTPK                                       0417078   00000530
LOADACCT MVC   EDITACT+5(1),0(5)   STORE DISPLACEMENT         0417078   00000600
         MVC   ACTEDIT+1(13),0(4)                                       00000700
EDITACT  ED    ACTEDIT,0(7)                                   0417078   00000800
         MVC   0(13,4),ACTEDIT+1   SEND BACK EDITED ACCT NO             00000900
         SIRETRN RC=0                                           OS50001 00001000
ACCTPK   DC    PL7'0'                                         0417078   00001100
ACTEDIT  DC    CL14' '                                                  00001200
IMACTEDT CSECT                                                          00001201
         LTORG                                                          00001202
         DS    0D                                                       00001203
SITMSTMP DC    CL64'IMACTEDT  -----TSD-             05/23/03  08.48.48' 00001204
*        THIS PROGRAM CONTAINS TRADE SECRETS THAT BELONG                00001205
*        TO ALLTEL INFORMATION SERVICES, INC. AND IS                    00001206
*        LICENSED BY AN AGREEMENT.  ANY UNAUTHORIZED ACCESS,            00001207
*        USE, DUPLICATION, OR DISCLOSURE IS UNLAWFUL.                   00001208
*        COPYRIGHT ALLTEL INFORMATION SERVICES, INC.                    00001209
*        2003, ALL RIGHTS RESERVED.                                     00001210
         END                                                            00001300
