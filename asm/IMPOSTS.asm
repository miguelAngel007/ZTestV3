*     * FO5238 * 06/26/11 PROYECTO REBORN
* THE FOLLOWING STATEMENT(S) MODIFIED BY                      9915239   00000999
IMPOSTS  SIIOMOD                                                       X00001000
               TYPE=S,                                                 X00002000
               RECFORM=VARBLK,                                         X00003000
               INBLK=27990,                                     1105504X00004000
               OUTBLK=27990,                                    1105504X00005000
               OUTBLK8=27998,                                   1105504X00006000
               RECSIZE=3093,                                    1105504X00007000
               TINSYS=SYS007,                                          X00008000
               TOUTSYS=SYS008,                                         X00009000
               TINREW=UNLOAD,                                          X00010000
               TOUTREW=UNLOAD                                           00011000
IMPOSTS  CSECT                                                          00011996
         LTORG                                                          00011997
         DS    0D                                                       00011998
SITMSTMP DC    CL64'IMPOSTS   -----TSD-             04/01/99  12.01.54' 00011999
*        THIS PROGRAM CONTAINS TRADE SECRETS THAT BELONG                00012000
*        TO ALLTEL INFORMATION SERVICES, INC. AND IS                    00012001
*        LICENSED BY AN AGREEMENT.  ANY UNAUTHORIZED ACCESS,            00012002
*        USE, DUPLICATION, OR DISCLOSURE IS UNLAWFUL.                   00012003
*        COPYRIGHT ALLTEL INFORMATION SERVICES, INC.                    00012004
*        1999, ALL RIGHTS RESERVED.                                     00012005
         END                                                            00012200
