*     * FO5238*12/05/11 JCTE PROYECTO UPGRADE SYSTEMAT
000100*----------------------------------------------------------------*
000200*    WORKING-STORAGE COPYBOOK FOR REGULATION DD LEAP YEAR        *
000300*    PROCESSING REQUIREMENTS.                                    *
000400*----------------------------------------------------------------*
000500 01  LEAP-YEAR-WORK-AREA.
000600     03  WK-OPEN-DATE.
000700         05  WK-OPEN-MO          PIC XX.
000800         05  WK-OPEN-DA          PIC XX.
000900         05  WK-OPEN-YR          PIC XX.
001000     03  WK-LEAP-DATE.
001100         05  WK-LEAP-MO          PIC XX          VALUE '02'.
001200         05  WK-LEAP-DA          PIC XX          VALUE '29'.
001300         05  WK-LEAP-YR          PIC XX.
001400     03  WK-FLAG                 PIC X.
001500     03  WK-FORMAT               PIC XX          VALUE '11'.
001600     03  WK-RETURN-FLG           PIC X.
001700     03  WK-LEAP-YEAR            PIC X.
