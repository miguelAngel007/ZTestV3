*     * FO5238 * 06/26/11 PROYECTO REBORN
* THE FOLLOWING STATEMENT(S) MODIFIED BY                      9915845   00000099
IMGLTTS  SIIOMOD                                                       X00000100
               TYPE=S,                                                 X00000200
               RECFORM=VARBLK,                                         X00000300
               INBLK=27990,                                            X00000400
               OUTBLK=27990,                                           X00000500
               OUTBLK8=27998,                                          X00000600
               RECSIZE=1254,                                           X00000700
               TINSYS=SYS007,                                          X00000800
               TOUTSYS=SYS008,                                         X00000900
               TINREW=UNLOAD,                                          X00001000
               TOUTREW=UNLOAD                                           00001100
IMGLTTS  CSECT                                                          00001196
         LTORG                                                          00001197
         DS    0D                                                       00001198
SITMSTMP DC    CL64'IMGLTTS   -----TSD-             09/23/97  15.50.57' 00001199
*        THIS PROGRAM CONTAINS TRADE SECRETS THAT BELONG                00001200
*        TO ALLTEL INFORMATION SERVICES, INC. AND IS                    00001201
*        LICENSED BY AN AGREEMENT.  ANY UNAUTHORIZED ACCESS,            00001202
*        USE, DUPLICATION, OR DISCLOSURE IS UNLAWFUL.                   00001203
*        COPYRIGHT ALLTEL INFORMATION SERVICES, INC.                    00001204
*        1997, ALL RIGHTS RESERVED.                                     00001205
         END                                                            00001400
