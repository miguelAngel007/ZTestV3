*     * FO5238*12/05/11 JCTE PROYECTO UPGRADE SYSTEMAT
000100*----------------------------------------------------------------*
000200*         IM31 USER PROCEDURE COPYBOOK USED IN PAR C2100         *
000300*----------------------------------------------------------------*
000400     MOVE WBC-CAPTURE-MO TO WRK-DC-DSC-MO.                        IMIB004
000500     MOVE WBC-CAPTURE-DA TO WRK-DC-DSC-DA.                        IMIB004
000600     MOVE WBC-CAPTURE-YR TO WRK-DC-DSC-YY.                        IMIB004
000700     IF  WBC-CAPTURE-YR GREATER THAN '50'                         IMIB004
000800         MOVE '19' TO WRK-DC-DSC-CC                               IMIB004
000900     ELSE                                                         IMIB004
001000         MOVE '20' TO WRK-DC-DSC-CC.                              IMIB004
