*     * FO5238 * 06/26/11 PROYECTO REBORN
IMACMTS  SIIOMOD                                                       X00001001
               TYPE=S,                                                 X00002000
               RECFORM=FIXBLK,                                         X00003000
               INBLK=23200,                                            X00004001
               OUTBLK=23200,                                           X00005001
               OUTBLK8=23208,                                          X00006001
               RECSIZE=400,                                            X00007001
               TINSYS=SYS007,                                          X00008000
               TOUTSYS=SYS008,                                         X00009000
               TINREW=UNLOAD,                                          X00010000
               TOUTREW=UNLOAD                                           00011000
IMACMTS  CSECT                                                          00011990
         LTORG                                                          00011991
         DS    0D                                                       00011992
SITMSTMP DC    CL64'IMACMTS   -----TSD-             05/28/97  10.52.53' 00011993
*        THIS PROGRAM CONTAINS TRADE SECRETS THAT BELONG                00011994
*        TO ALLTEL INFORMATION SERVICES, INC. AND IS                    00011995
*        LICENSED BY AN AGREEMENT.  ANY UNAUTHORIZED ACCESS,            00011996
*        USE, DUPLICATION, OR DISCLOSURE IS UNLAWFUL.                   00011997
*        COPYRIGHT ALLTEL INFORMATION SERVICES, INC.                    00011998
*        1997, ALL RIGHTS RESERVED.                                     00011999
         END                                                            00012000
