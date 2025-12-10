*     * FO5238*12/05/11 JCTE PROYECTO UPGRADE SYSTEMAT
000100******************************************************************
000200***  THE FOLLOWING CODE IS TO CALL A PHASE TO OPEN/CLOSE RELATION-
000300***  SHIP PRICING NX FILES FOR MULTI-CURRENCY PROCESSING
000400******************************************************************
000500
000600     IF  NX-OPEN EQUAL 'Y'
000700         CALL 'SILINK' USING SCNX-PHASE
000800                             OPEN-FLAGS.
000900
