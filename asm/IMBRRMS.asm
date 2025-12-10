*     * FO5238 * 06/26/11 PROYECTO REBORN
*********************************************************************** 00010000
* THIS IS THE NEW I/O MODULE TO BE USED FOR THE BALANCE REPORTING     * 00020000
* FILE USED FOR ENCODE+.                                              * 00030000
*********************************************************************** 00040000
* THE FOLLOWING STATEMENT(S) MODIFIED BY                      0927857   00049999
IMBRRMS  SIIOMOD                                                       X00050000
               TYPE=S,                                                 X00060000
               RECFORM=FIXBLK,                                         X00070000
               INBLK=2950,                                    0927857  X00080000
               OUTBLK=2950,                                   0927857  X00090000
               OUTBLK8=2958,                                  0927857  X00100000
               RECSIZE=295,                                            X00110000
               TINREW=UNLOAD,                                 0927857  X00120000
               TOUTREW=UNLOAD,                                0927857  X00130000
               TINSYS=SYS007,                                 0927857  X00140000
               TOUTSYS=SYS008                                 0927857   00150000
IMBRRMS  CSECT                                                          00159990
         LTORG                                                          00159991
         DS    0D                                                       00159992
SITMSTMP DC    CL64'IMBRRMS   -----TSD-             08/13/08  07.22.37' 00159993
*        THIS PROGRAM CONTAINS TRADE SECRETS THAT BELONG                00159994
*        TO FIDELITY INFORMATION SERVICES AND IS                        00159995
*        LICENSED BY AN AGREEMENT.  ANY UNAUTHORIZED ACCESS,            00159996
*        USE, DUPLICATION, OR DISCLOSURE IS UNLAWFUL.                   00159997
*        COPYRIGHT FIDELITY INFORMATION SERVICES                        00159998
*        2008, ALL RIGHTS RESERVED.                                     00159999
         END                                                            00160000
