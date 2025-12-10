*     * FO5238 * 06/26/11 PROYECTO REBORN
* THE FOLLOWING STATEMENT(S) MODIFIED BY                      0326947   00000999
IMTAGTS  SIIOMOD                                                       X00001000
               TYPE=S,                                                 X00002000
               RECFORM=VARBLK,                                         X00003000
               INBLK=27990,                                            X00004000
               OUTBLK=27990,                                           X00005000
               OUTBLK8=27998,                                          X00006000
               RECSIZE=3051,                                           X00007000
               TINSYS=SYS007,                                          X00008000
               TOUTSYS=SYS008,                                         X00009000
               TINREW=UNLOAD,                                          X00010000
               TOUTREW=UNLOAD                                           00011000
IMTAGTS  CSECT                                                          00011001
         LTORG                                                          00011002
         DS    0D                                                       00011003
SITMSTMP DC    CL64'IMTAGTS   -----TSD-             11/12/02  12.59.26' 00011004
*        THIS PROGRAM CONTAINS TRADE SECRETS THAT BELONG                00011005
*        TO ALLTEL INFORMATION SERVICES, INC. AND IS                    00011006
*        LICENSED BY AN AGREEMENT.  ANY UNAUTHORIZED ACCESS,            00011007
*        USE, DUPLICATION, OR DISCLOSURE IS UNLAWFUL.                   00011008
*        COPYRIGHT ALLTEL INFORMATION SERVICES, INC.                    00011009
*        2002, ALL RIGHTS RESERVED.                                     00011010
         END                                                            00012000
