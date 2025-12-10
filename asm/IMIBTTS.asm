*     * FO5238 * 06/26/11 PROYECTO REBORN
IMIBTTS  SIIOMOD                                                       X00001000
               TYPE=S,                                                 X00002000
               RECFORM=VARBLK,                                         X00003000
               INBLK=27990,                                            X00004000
               OUTBLK=27990,                                           X00005000
               OUTBLK8=27998,                                          X00006000
               RECSIZE=15331,                                          X00007000
               TINSYS=SYS007,                                          X00008000
               TOUTSYS=SYS008,                                         X00009000
               TINREW=UNLOAD,                                          X00010000
               TOUTREW=UNLOAD                                           00011000
IMIBTTS  CSECT                                                          00011990
         LTORG                                                          00011991
         DS    0D                                                       00011992
SITMSTMP DC    CL64'IMIBTTS   -----TSD-             10/01/97  23.56.15' 00011993
*        THIS PROGRAM CONTAINS TRADE SECRETS THAT BELONG                00011994
*        TO ALLTEL INFORMATION SERVICES, INC. AND IS                    00011995
*        LICENSED BY AN AGREEMENT.  ANY UNAUTHORIZED ACCESS,            00011996
*        USE, DUPLICATION, OR DISCLOSURE IS UNLAWFUL.                   00011997
*        COPYRIGHT ALLTEL INFORMATION SERVICES, INC.                    00011998
*        1997, ALL RIGHTS RESERVED.                                     00011999
         END                                                            00012000
