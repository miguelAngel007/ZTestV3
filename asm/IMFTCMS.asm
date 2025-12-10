*     * FO5238 * 06/26/11 PROYECTO REBORN
* THE FOLLOWING STATEMENT(S) MODIFIED BY                      9915845   00000999
IMFTCMS  SIIOMOD                                                       X00001000
               TYPE=S,                                                 X00002000
               RECFORM=FIXBLK,                                         X00003000
               INBLK=27900,                                            X00004000
               OUTBLK=27900,                                           X00005000
               OUTBLK8=27908,                                          X00006000
               RECSIZE=150,                                            X00007000
               TINSYS=SYS007,                                          X00008000
               TOUTSYS=SYS008,                                         X00009000
               TINREW=UNLOAD,                                          X00010000
               TOUTREW=UNLOAD                                           00011000
IMFTCMS  CSECT                                                          00011990
         LTORG                                                          00011991
         DS    0D                                                       00011992
SITMSTMP DC    CL64'IMFTCMS   -----TSD-             09/23/97  15.43.31' 00011993
*        THIS PROGRAM CONTAINS TRADE SECRETS THAT BELONG                00011994
*        TO ALLTEL INFORMATION SERVICES, INC. AND IS                    00011995
*        LICENSED BY AN AGREEMENT.  ANY UNAUTHORIZED ACCESS,            00011996
*        USE, DUPLICATION, OR DISCLOSURE IS UNLAWFUL.                   00011997
*        COPYRIGHT ALLTEL INFORMATION SERVICES, INC.                    00011998
*        1997, ALL RIGHTS RESERVED.                                     00011999
         END                                                            00012000
