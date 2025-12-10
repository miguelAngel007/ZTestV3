*     * FO5238 * 06/26/11 PROYECTO REBORN
* THE FOLLOWING STATEMENT(S) MODIFIED BY                      0927857   00000999
IMACDTS  SIIOMOD                                                       X00001000
               TYPE=S,                                                 X00002000
               RECFORM=FIXBLK,                                         X00003000
               INBLK=27984,                                            X00004000
               OUTBLK=27984,                                           X00005000
               OUTBLK8=27992,                                 0927857  X00006000
               RECSIZE=53,                                             X00007000
               TINSYS=SYS007,                                          X00008000
               TOUTSYS=SYS008,                                         X00009000
               TINREW=UNLOAD,                                          X00010000
               TOUTREW=UNLOAD                                           00011000
IMACDTS  CSECT                                                          00011991
         LTORG                                                          00011992
         DS    0D                                                       00011993
SITMSTMP DC    CL64'IMACDTS   -----TSD-             08/13/08  07.22.20' 00011994
*        THIS PROGRAM CONTAINS TRADE SECRETS THAT BELONG                00011995
*        TO FIDELITY INFORMATION SERVICES AND IS                        00011996
*        LICENSED BY AN AGREEMENT.  ANY UNAUTHORIZED ACCESS,            00011997
*        USE, DUPLICATION, OR DISCLOSURE IS UNLAWFUL.                   00011998
*        COPYRIGHT FIDELITY INFORMATION SERVICES                        00011999
*        2008, ALL RIGHTS RESERVED.                                     00012000
         END                                                            00012001
