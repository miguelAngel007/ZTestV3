*     * FO5238*12/05/11 JCTE PROYECTO UPGRADE SYSTEMAT
000100*----------------------------------------------------------------*
000200*    PROCEDURE DIVISION COPYBOOK FOR REGULATION DD LEAP YEAR     *
000300*    PROCESSING REQUIREMENTS.                                    *
000400*----------------------------------------------------------------*
000500 CHECK-366.
000600     MOVE '0' TO WK-FLAG
000700                 WK-RETURN-FLG.
000800     IF  WK-LEAP-YEAR NOT EQUAL '1'
000900         GO TO C366-EXIT.
001000     MOVE '1' TO WK-RETURN-FLG.
001100     IF  WMS-TIS-CONSUMER-FLAG EQUAL 'N'
001200         GO TO C366-EXIT.
001300     CALL 'SICOMPDT' USING WK-OPEN-DATE
001400                           WK-LEAP-DATE
001500                           WK-FLAG
001600                           WK-FORMAT.
001700     IF  WK-FLAG EQUAL 'H'
001800         MOVE '0' TO WK-RETURN-FLG.
001900
002000 C366-EXIT.
002100     EXIT.
