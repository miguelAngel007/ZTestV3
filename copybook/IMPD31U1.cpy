*     * 410223*10/06/24 JCTE METODOLOGIA SPLIT EN SALDO COMPROMISO     *
*     * 203917*01/09/22 JCTE INT ACREEDOR CTAS REMUNERADAS MES VIGENTE..
*     * FO5238*12/05/11 JCTE PROYECTO UPGRADE SYSTEMAT
000100*----------------------------------------------------------------*
000200*         IM31 PROCEDURE COPYBOOK - INSERT IN PAR A0001          *
000300*----------------------------------------------------------------*
000400******************************************************************IMIB056
000500*    CALL RATE ACCESS. (OPEN ( IB-SW = 0 )                        IMIB056
000600******************************************************************IMIB056
000700     MOVE 'IM31IBAC'  TO IB-RATE-PGM.                             IMIB056
000800     MOVE 0           TO IB-SW.                                   IMIB056
000900     CALL 'SILINK' USING IB-RATE-PGM                              IMIB056
001000                         IB-SW                                    IMIB056
001100                         IB-CONTROL-KEY                           IMIB056
001200                         IB-RATE-FIELDS.                          IMIB056
001300     IF  IB-RET-CODE NOT EQUAL 0000                               IMIB056
001400         MOVE IB-RATE-PGM  TO SIMESS-PROGRAM                      IMIB056
001500         MOVE 505 TO SIMESS-MESS-NO                               IMIB056
001600         MOVE 'OPEN FAILED TARIF' TO SIMESS-OPTIONAL-MESSAGE      IMIB056
001700         CALL 'SIMESS' USING SIMESS-AREA                          IMIB056
001800         GO TO Z9900.                                             IMIB056
001900******************************************************************IMIBXXX
002000*    CALL RUTINA INACTIVAD (OPEN ( IB-SW = 0 )                    IMIBXXX
002100******************************************************************IMIBXXX
002200     MOVE 'IM31IBST'  TO IB-RATE-PGM.                             IMIBXXX
002300     MOVE 0           TO IB-SW.                                   IMIBXXX
002400     CALL 'SILINK' USING IB-RATE-PGM                              IMIBXXX
002500                         IB-SW                                    IMIBXXX
002600                         IB-CONTROLS-ST                           IMIBXXX
002700                         IB-OUTPUT-ST.                            IMIBXXX
002800     IF  IB-RETURN-CODE-ST NOT EQUAL 0000                         IMIBXXX
002900         MOVE IB-RATE-PGM  TO SIMESS-PROGRAM                      IMIBXXX
003000         MOVE 505 TO SIMESS-MESS-NO                               IMIBXXX
003100         MOVE 'OPEN FAILED IMSTP' TO SIMESS-OPTIONAL-MESSAGE      IMIBXXX
003200         CALL 'SIMESS' USING SIMESS-AREA                          IMIBXXX
003300         GO TO Z9900.                                             IMIBXXX
