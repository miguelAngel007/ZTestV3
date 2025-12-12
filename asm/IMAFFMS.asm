*     * FO5238 * 06/26/11 PROYECTO REBORN
IMAFFMS  SIIOMOD                                                       X00001000
               TYPE=S,                                                 X00002000
               RECFORM=VARBLK,                                         X00003000
               INBLK=27990,                                            X00004000
               OUTBLK=27990,                                           X00005000
               OUTBLK8=27998,                                          X00006000
               RECSIZE=850,                                            X00007000
               TINSYS=SYS007,                                          X00008000
               TOUTSYS=SYS008,                                         X00009000
               TINREW=UNLOAD,                                          X00010000
               TOUTREW=UNLOAD                                           00011000
IMAFFMS  CSECT                                                          00012185
         LTORG                                                          00012186
         DS    0D                                                       00012187
SITMSTMP DC    CL64'IMAFFMS   -----TSD-             11/03/08  11.05.11' 00012188
*        THIS PROGRAM CONTAINS TRADE SECRETS THAT BELONG                00012189
*        TO FIDELITY INFORMATION SERVICES AND IS                        00012190
*        LICENSED BY AN AGREEMENT.  ANY UNAUTHORIZED ACCESS,            00012191
*        USE, DUPLICATION, OR DISCLOSURE IS UNLAWFUL.                   00012192
*        COPYRIGHT FIDELITY INFORMATION SERVICES                        00012193
*        2008, ALL RIGHTS RESERVED.                                     00012194
         END                                                            00012200
