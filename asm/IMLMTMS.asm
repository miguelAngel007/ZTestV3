*     * FO5238 * 06/26/11 PROYECTO REBORN
IMLMTMS  SIIOMOD                                                       X00001000
               TYPE=S,                                                 X00002000
               RECFORM=VARBLK,                                         X00003000
               INBLK=27990,                                            X00004000
               OUTBLK=27990,                                           X00005000
               OUTBLK8=27998,                                          X00006000
               RECSIZE=625,                                            X00007000
               TINSYS=SYS007,                                          X00008000
               TOUTSYS=SYS008,                                         X00009000
               TINREW=UNLOAD,                                          X00010000
               TOUTREW=UNLOAD                                           00011000
IMLMTMS  CSECT                                                          00012190
         LTORG                                                          00012191
         DS    0D                                                       00012192
SITMSTMP DC    CL64'IMLMTMS   -----TSD-             03/09/98  11.10.27' 00012193
*        THIS PROGRAM CONTAINS TRADE SECRETS THAT BELONG                00012194
*        TO ALLTEL INFORMATION SERVICES, INC. AND IS                    00012195
*        LICENSED BY AN AGREEMENT.  ANY UNAUTHORIZED ACCESS,            00012196
*        USE, DUPLICATION, OR DISCLOSURE IS UNLAWFUL.                   00012197
*        COPYRIGHT ALLTEL INFORMATION SERVICES, INC.                    00012198
*        1998, ALL RIGHTS RESERVED.                                     00012199
         END                                                            00012200
