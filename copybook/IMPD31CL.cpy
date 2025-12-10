*     * FO5238*12/05/11 JCTE PROYECTO UPGRADE SYSTEMAT
000100*----------------------------------------------------------------*
000200*    INITIALIZE INDEPENDENT INTEREST CYCLE FIELDS                *
000300*----------------------------------------------------------------*
000400*
000500     MOVE SPACE TO INT-CYCLE
000600                   INT-DAY
000700                   INT-CYCLE-DAYX
000800                   INT-PAY-TODAY
000900                   INT-INCR
001000                   INT-MONTH1
001100                   INT-CYCLE-DAYX
001200                   OD-CYCLE-DAYX.
001300
001400     MOVE '0'   TO INT-PAY-TODAY
001410                   IOD-PAY-TODAY
001420                   SAV-PAY-TODAY
001500                   OD-CHG-TODAY.
001600
