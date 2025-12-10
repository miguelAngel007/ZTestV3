*     * FO5238*12/05/11 JCTE PROYECTO UPGRADE SYSTEMAT
000100******************************************************************
000200***  THE FOLLOWING CODE IS TO CALL A PHASE TO OPEN/CLOSE RELATION-
000300***  SHIP PRICING NX FILES FOR MULTI-CURRENCY PROCESSING
000400******************************************************************
000500
000600     IF WBC-NX-INSTALLED EQUAL '1'                                0266741
000700         IF  (NX-OPEN EQUAL 'N')
000800         AND (WBC-SC-INSTALLED EQUAL 'B' OR 'R')                  0266741
000900             CALL 'SILINK' USING SCNX-PHASE
001000                                 OPEN-FLAGS.
001100
