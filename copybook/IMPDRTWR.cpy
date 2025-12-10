*     * FO5238*12/05/11 JCTE PROYECTO UPGRADE SYSTEMAT
000100*
000200*       READ RATE WORK FILE, LOAD TO WORKING STORAGE.
000300*
000400 READ-RATE-WORK.
000500     MOVE 'K'                    TO I-O-CONTROL-OPERATOR.
000600     MOVE 'I'                    TO I-O-CONTROL-ACCESS.
000700     MOVE WBC1-CONTROL-1         TO RWF-CTL1.                     0266741
000710     MOVE WBC-CURR-CODE          TO RWF-CURRENCY.                 0266741
000800     MOVE 'I'                    TO RWF-RATE-TYPE.
000810     MOVE +1                     TO RWF-SEQ.                      2012795
000900
001000 RATE-WORK-CALL.
001100     CALL 'IMRTEWV'           USING I-O-CONTROL-AREA
001200                                    RATE-WORK-FILE.
001300     IF  NOT I-O-88-NORMAL-RET
001400         MOVE HIGH-VALUES        TO RWF-RATE-INFO.
001500 RATE-CALL-END.
001600     EXIT.
001700
001800 RATE-CALL-CONT.
001900     MOVE RWF-RATE-INFO       TO WS-RWF-DDA-INFO-DATA (RWF-SEQ).  2012795
001910     ADD +1                   TO RWF-SEQ.                         2012795
001920     IF  RWF-SEQ    GREATER THAN +10                              2012795
001930         MOVE +1              TO RWF-SEQ                          2012795
001940         GO TO RATE-CALL-L                                        2012795
001950     ELSE                                                         2012795
001960         PERFORM RATE-WORK-CALL THRU RATE-CALL-END                2012795
001970         GO TO RATE-CALL-CONT.                                    2012795
001980 RATE-CALL-L.                                                     2012795
002000     MOVE 'L'                 TO RWF-RATE-TYPE.                   2012795
002100     PERFORM RATE-WORK-CALL THRU RATE-CALL-END.
002200     MOVE RWF-RATE-INFO       TO WS-RWF-LN-INFO-DATA (RWF-SEQ).   2012795
002210     ADD +1                   TO RWF-SEQ.                         2012795
002220     IF  RWF-SEQ    GREATER THAN +10                              2012795
002230         MOVE +1              TO RWF-SEQ                          2012795
002240         GO TO RATE-CALL-O                                        2012795
002250     ELSE                                                         2012795
002260         GO TO RATE-CALL-L.                                       2012795
002270 RATE-CALL-O.                                                     2012795
002300     MOVE 'O'                    TO RWF-RATE-TYPE.
002400     PERFORM RATE-WORK-CALL THRU RATE-CALL-END.
002500     MOVE RWF-RATE-INFO       TO WS-RWF-OD-INFO-DATA (RWF-SEQ).   2012795
002510     ADD +1                   TO RWF-SEQ.                         2012795
002520     IF  RWF-SEQ    GREATER THAN +10                              2012795
002530         MOVE +1              TO RWF-SEQ                          2012795
002540         GO TO RATE-CALL-P                                        2012795
002550     ELSE                                                         2012795
002560         GO TO RATE-CALL-O.                                       2012795
002570 RATE-CALL-P.                                                     2012795
002600     MOVE 'P'                    TO RWF-RATE-TYPE.
002700     PERFORM RATE-WORK-CALL THRU RATE-CALL-END.
002800     MOVE RWF-RATE-INFO       TO WS-RWF-PRM-INFO-DATA (RWF-SEQ).  2012795
002810     ADD +1                   TO RWF-SEQ.                         2012795
002820     IF  RWF-SEQ    GREATER THAN +10                              2012795
002830         MOVE +1              TO RWF-SEQ                          2012795
002840         GO TO RATE-CALL-S                                        2012795
002850     ELSE                                                         2012795
002860         GO TO RATE-CALL-P.                                       2012795
002870 RATE-CALL-S.                                                     2012795
002900     MOVE 'S'                    TO RWF-RATE-TYPE.
003000     PERFORM RATE-WORK-CALL THRU RATE-CALL-END.
003100     MOVE RWF-RATE-INFO       TO WS-RWF-SPL-INFO-DATA (RWF-SEQ).  2012795
003110     ADD +1                   TO RWF-SEQ.                         2012795
003120     IF  RWF-SEQ    GREATER THAN +10                              2012795
003130         MOVE +1              TO RWF-SEQ                          2012795
003140         GO TO RATE-CALL-T                                        2012795
003150     ELSE                                                         2012795
003160         GO TO RATE-CALL-S.                                       2012795
003170 RATE-CALL-T.                                                     2012795
003200     MOVE 'T'                    TO RWF-RATE-TYPE.
003300     PERFORM RATE-WORK-CALL THRU RATE-CALL-END.
003400     MOVE RWF-RATE-INFO       TO WS-RWF-TIER-INFO-DATA (RWF-SEQ). 2012795
003410     ADD +1                   TO RWF-SEQ.                         2012795
003420     IF  RWF-SEQ    GREATER THAN +10                              2012795
003430         MOVE +1              TO RWF-SEQ                          2012795
003440         GO TO RATE-CALL-X                                        2012254
003450     ELSE                                                         2012795
003460         GO TO RATE-CALL-T.                                       2012795
003463 RATE-CALL-X.                                                     2012254
003466     MOVE 'X'                    TO RWF-RATE-TYPE.                2012254
003469     PERFORM RATE-WORK-CALL THRU RATE-CALL-END.                   2012254
003472     MOVE RWF-RATE-INFO       TO WS-RWF-TAX-INFO-DATA (RWF-SEQ).  2012254
003475     ADD +1                   TO RWF-SEQ.                         2012254
003478     IF  RWF-SEQ    GREATER THAN +10                              2012254
003481         MOVE +1              TO RWF-SEQ                          2012254
003484         GO TO RATE-WORK-END                                      2012254
003487     ELSE                                                         2012254
003490         GO TO RATE-CALL-X.                                       2012254
003500 RATE-WORK-END.
003600     EXIT.
