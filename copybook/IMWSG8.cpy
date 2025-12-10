*     * FO5238*12/05/11 JCTE PROYECTO UPGRADE SYSTEMAT
000100*--------------------------------------------------------------*
000200* THIS COPYBOOK CONTAINS THE FIELDS USED BY IM22G8 TO GENERATE *
000300* A EDITED PLAN KEY FROM AN EXISTING PLAN KEY.                 *
000400*--------------------------------------------------------------*
000500 01  WS-G8-FIELDS.
000600     03  WS-G8-PLN-TYPE               PIC XX.
000700     03  WS-G8-PLAN-KEY.
000800         05  WS-G8-PLN-PLAN-ID        PIC X(12).
000900         05  WS-G8-PLN-SEQ-NO         PIC XXX.
001000         05  WS-G8-EMP-PLAN-ID        PIC X(12).
001100     03  WS-G8-EDITED-PLAN-KEY        PIC X(35).
