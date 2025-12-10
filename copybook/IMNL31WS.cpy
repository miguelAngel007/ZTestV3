*     * FO5238*12/05/11 JCTE PROYECTO UPGRADE SYSTEMAT
000100*--------------------------------------------------------------*
000200*     IMNL31WS                                                 *
000300*     INTL -   WORKING STORAGE FIELDS FOR INTERNATIONAL CODE   *
000400*--------------------------------------------------------------*
000500*--------------------------------------------------------------*
000600*     WORK FIELDS FOR INTERFACING WITH NX AND CL TO OPEN FILES *
000700*     FOR FORIEGN CURRENCY EXCHANGE                            *
000800*--------------------------------------------------------------*
000900
001000 01  SCNX-PHASE                      PIC X(8)    VALUE 'IMSCNXCL'.
001100
001200 01  OPEN-FLAGS.
001300     03  NX-OPEN                     PIC X       VALUE 'N'.
001400     03  CL-OPEN                     PIC X       VALUE 'N'.
