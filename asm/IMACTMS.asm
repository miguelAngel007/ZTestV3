*     * FO5238 * 06/26/11 PROYECTO REBORN
* THE FOLLOWING STATEMENT(S) MODIFIED BY                      0930011   00000999
IMACTMS  SIIOMOD                                                       X00001000
               TYPE=S,                                                 X00002000
               RECFORM=VARBLK,                                         X00003000
               INBLK=27990,                                   9915845  X00004000
               OUTBLK=27990,                                  9915845  X00004790
               OUTBLK8=27998,                                 9915845  X00005580
               RECSIZE=16630,                                 9915845  X00006370
               TINSYS=SYS007,                                          X00008000
               TOUTSYS=SYS008,                                         X00009000
               TINREW=UNLOAD,                                          X00010000
               TOUTREW=UNLOAD                                           00011000
IMACTMS  CSECT                                                          00011996
         LTORG                                                          00011997
         DS    0D                                                       00011998
SITMSTMP DC    CL64'IMACTMS   -----TSD-             06/29/09  08.59.33' 00011999
*        THIS PROGRAM CONTAINS TRADE SECRETS THAT BELONG                00012000
*        TO FIDELITY INFORMATION SERVICES AND IS                        00012001
*        LICENSED BY AN AGREEMENT.  ANY UNAUTHORIZED ACCESS,            00012002
*        USE, DUPLICATION, OR DISCLOSURE IS UNLAWFUL.                   00012003
*        COPYRIGHT FIDELITY INFORMATION SERVICES                        00012004
*        2009, ALL RIGHTS RESERVED.                                     00012005
         END                                                            00012200
