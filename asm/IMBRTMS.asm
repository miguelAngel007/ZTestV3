*     * FO5238 * 06/26/11 PROYECTO REBORN
* THE FOLLOWING STATEMENT(S) MODIFIED BY                      0266830   00009999
IMBRTMS  SIIOMOD                                                       X00010000
               TYPE=S,                                                 X00020000
               RECFORM=FIXBLK,                                         X00030000
               INBLK=30008,                                            X00040000
               OUTBLK=30008,                                           X00050000
               OUTBLK8=30016,                                          X00060000
               RECSIZE=30008,                                          X00070005
               TINSYS=SYS007,                                          X00080000
               TOUTSYS=SYS008,                                         X00090000
               TINREW=UNLOAD,                                          X00100000
               TOUTREW=UNLOAD                                           00110000
IMBRTMS  CSECT                                                          00120001
         LTORG                                                          00130001
         DS    0D                                                       00140002
IMBRTMS  CSECT                                                          00149993
         LTORG                                                          00149994
         DS    0D                                                       00149995
SITMSTMP DC    CL64'IMBRTMS   -----TSD-             02/26/02  08.56.01' 00149996
*        THIS PROGRAM CONTAINS TRADE SECRETS THAT BELONG                00149997
*        TO ALLTEL INFORMATION SERVICES, INC. AND IS                    00149998
*        LICENSED BY AN AGREEMENT.  ANY UNAUTHORIZED ACCESS,            00149999
*        USE, DUPLICATION, OR DISCLOSURE IS UNLAWFUL.                   00150000
*        COPYRIGHT ALLTEL INFORMATION SERVICES, INC.                    00150001
*        2002, ALL RIGHTS RESERVED.                                     00150002
         END                                                            00150003
