*     * FO5238*12/05/11 JCTE PROYECTO UPGRADE SYSTEMAT
000100 PL-CD-EDITS.
000200*
000300     IF  WK-PE-PL-CD EQUAL '  '
000400         GO TO PL-CD2-EDITS.
000500*
000600     SET PL-CODE-IND TO +1.
000700     SEARCH PL-CODE-NAME
000800     WHEN (WK-PE-PL-CD     EQUAL PL-CODE1 (PL-CODE-IND))
000900          GO TO PL-CD2-EDITS.
001000*
001100*    *-----------------------------*
001200*    *    INVALID PLAN CODE IM2504 *                              0937740
001300*    *-----------------------------*
001400     MOVE +10 TO WK-PE-ERR-CD (PE-IDX)
001410     MOVE '1' TO WK-PE-ERR-PC (PE-IDX)                            0937740
001500     SET PE-IDX UP BY 1.
001600*
001700 PL-CD2-EDITS.
001800*
001900     IF  WK-PE-PL-CD2 EQUAL '  '
002000         IF  PE-IDX EQUAL 1                                       0937740
002010             GO TO PL-CD-IM29-VALIDATE                            0937740
002020         ELSE                                                     0937740
002030             GO TO PL-CD-EDITS-EXIT.                              0937740
002100*
002110* PL-CD > 49 CONTRIBUTION                                         0937740
002200     IF  (WK-PE-PL-CD GREATER THAN '49')
002300     AND (WK-PE-PL-CD2   NOT EQUAL SPACES)
002400*        *********************************
002500*        * CANNOT HAVE SECONDARY PL CODE *
002600*        * ON A CONTRIBUTION IM2505      *                        0937740
002700*        *********************************
002800         MOVE +12 TO WK-PE-ERR-CD (PE-IDX)
002810         MOVE '2' TO WK-PE-ERR-PC (PE-IDX)                        0937740
002900         SET PE-IDX UP BY 1
003000         GO TO PL-CD-EDITS-EXIT.                                  0937740
003100*
003200     SET PL-CODE-IND  TO +1.
003300     SEARCH PL-CODE-NAME
003400       WHEN (WK-PE-PL-CD2 EQUAL PL-CODE2 (PL-CODE-IND))           0937740
003410        AND (PE-IDX       EQUAL 1)                                0937740
003500            GO TO PL-CD-COMB-EDITS                                0937740
003510       WHEN (WK-PE-PL-CD2 EQUAL PL-CODE2 (PL-CODE-IND))           0937740
003520        AND (PE-IDX   NOT EQUAL 1)                                0937740
003530            GO TO PL-CD-EDITS-EXIT.                               0937740
003600*
003700*    *-------------------------------*                            0937740
003800*    * INVALID SECONDARY PL CD IM2516*                            0937740
003900*    *-------------------------------*                            0937740
004000     MOVE +6  TO WK-PE-ERR-CD (PE-IDX).                           0937740
004010     MOVE '2' TO WK-PE-ERR-PC (PE-IDX).                           0937740
004100     SET PE-IDX UP BY 1.
004200     GO TO PL-CD-EDITS-EXIT.                                      0937740
004300*
004400 PL-CD-COMB-EDITS.
004500*
004600     SET PL-CODE-IND  TO +1.
004700     SEARCH PL-CODE-NAME
004800       WHEN WK-PE-PL-CODES EQUAL PL-COMBINED-CODES (PL-CODE-IND)
004900            GO TO PL-CD-IM29-VALIDATE.                            0937740
005000*
005100*    *---------------------------------*
005200*    * INVALID COMBINATION OF PL CODES *
005210*    * IM2517                          *                          0937740
005300*    *---------------------------------*
005400     MOVE +18 TO WK-PE-ERR-CD (PE-IDX).                           0937740
005410     MOVE '1' TO WK-PE-ERR-PC (PE-IDX).                           0937740
005500     SET PE-IDX UP BY 1.
005600*
005700 PL-CD-IM29-VALIDATE.                                             0937740
005800*
005900     IF  WK-PE-PGM NOT EQUAL 'IM29'                               0937740
006000         GO TO PL-CD-IM2I-VALIDATE.                               0937740
006100*
006110* PL-CD > 49 CONTRIBUTION                                         0937740
006200     IF  (WK-PE-PL-CD GREATER THAN '49')                          0937740
006300     AND (WK-PE-ADJ-TYPE     EQUAL '1' OR '2')                    0937740
006800*        *-----------------------------*                          0937740
006900*        * INV PL CD FOR TAX ADJ IM2503*                          0937740
007000*        *-----------------------------*                          0937740
007100         MOVE +9 TO WK-PE-ERR-CD (PE-IDX)                         0937740
007110         MOVE '1' TO WK-PE-ERR-PC (PE-IDX)                        0937740
007200         SET PE-IDX UP BY 1.                                      0937740
007300*                                                                 0937740
007310* PL-CD SPACES INVALID                                            0937740
007325     IF  WK-PE-PL-CD EQUAL '  '                                   0937740
007330*        *----------------------------*                           0937740
007345*        * INVALID PLAN CODE - IM2504 *                           0937740
007350*        *----------------------------*                           0937740
007365         MOVE +10 TO WK-PE-ERR-CD (PE-IDX)                        0937740
007370         MOVE '1' TO WK-PE-ERR-PC (PE-IDX)                        0937740
007385         SET PE-IDX UP BY 1                                       0937740
007390         GO TO PL-CD-EDITS-EXIT.                                  0937740
007400*                                                                 0937740
007500     GO TO PL-CD-TRCD-EDITS.                                      0937740
008400*
008410 PL-CD-IM2I-VALIDATE.                                             0937740
008420*                                                                 0937740
008430     IF  WK-PE-PGM NOT EQUAL 'IM2I'                               0937740
008440         GO TO PL-CD-IM60-VALIDATE.                               0937740
008450     GO TO PL-CD-TRCD-EDITS.                                      0937740
008500*                                                                 0937740
008510 PL-CD-IM60-VALIDATE.                                             0937740
008520*                                                                 0937740
008530     IF  WK-PE-PGM NOT EQUAL 'IM60'                               0937740
008540         GO TO PL-CD-IM80-VALIDATE.                               0937740
008550     GO TO PL-CD-TRCD-EDITS.                                      0937740
008600*
008615 PL-CD-IM80-VALIDATE.                                             0937740
008620*                                                                 0937740
008625     IF  WK-PE-PGM NOT EQUAL 'IM80'                               0937740
008630         GO TO PL-CD-TRCD-EDITS.                                  0937740
008635*                                                                 0937740
008640* PL-CD 00 - NON-REPORTABLE 50 - NON-REPORTABLE                   0937740
008645     IF  WK-PE-PL-CD EQUAL '00' OR '50' OR '60'                   0937740
008650*        *----------------------------*                           0937740
008655*        * INVALID PLAN CODE - IM2504 *                           0937740
008660*        *----------------------------*                           0937740
008665         MOVE +10 TO WK-PE-ERR-CD (PE-IDX)                        0937740
008670         MOVE '1' TO WK-PE-ERR-PC (PE-IDX)                        0937740
008675         SET PE-IDX UP BY 1                                       0937740
008680         GO TO PL-CD-EDITS-EXIT.                                  0937740
008685     GO TO PL-CD-TRCD-EDITS.                                      0937740
008700*                                                                 0937740
008705 PL-CD-TRCD-EDITS.                                                0937740
008710*                                                                 0937740
008715*  WK-PE-TR-TYPE                                                  0937740
008720*  1-DEPOSIT 3-CHECK 5-LOAN CREDIT 7-SAVINGS TRLR CR              0937740
008725*  2-CREDIT  4-DEBIT 6-LOAN DEBIT  8-SAVINGS TRLR DB              0937740
008730*  WK-PE-OPT-2      4 STATE TAX DEBIT                             0937740
008735*  WK-PE-OPT-6      1 FORCE PLAN DEPOSIT                          0937740
008740*  WK-PE-OPT-9      1 NONRPT PLN SAVINGS TRLR DEBIT               0937740
008745*  WK-PE-OPT-9      2 NONRPT PLN SAVINGS TRLR CREDIT              0937740
008750*  WK-PE-OPT-11     2 FED TAX CREDIT                              0937740
008755*  WK-PE-OPT-12     1 FORCE PLAN CREDIT                           0937740
008760*  WK-PE-OPT-12     2 NONRPT PLN CREDIT                           0937740
008765*  WK-PE-OPT-13     1 FED TAX DEBIT                               0937740
008770*  WK-PE-OPT-16     1 STATE TAX CREDIT                            0937740
008775*  WK-PE-OPT-18     1 NONRPT PLN DEBIT                            0937740
008780*                                                                 0937740
008800     IF  WK-PE-TR-INFO NOT EQUAL 'Y'                              0937740
008810         GO TO PL-CD-MAST-EDITS.                                  0937740
008900*
008910* TR-TYPE 4 DEBIT OPT-18 NONRPT PLN                               0937740
008920* TR-TYPE 2 CREDIT OPT-12 NONRPT PLN                              0937740
009000     IF  (((WK-PE-TR-TYPE   EQUAL '4') AND                        0937740
009010           (WK-PE-OPT-18    EQUAL '1' OR '3' OR '5' OR '7'))      0937740
009020     OR   ((WK-PE-TR-TYPE   EQUAL '2') AND                        0937740
009100           (WK-PE-OPT-12    EQUAL '2' OR '3' OR '6' OR '7')))     0937740
009200         IF  WK-PE-PL-CD GREATER THAN SPACES                      0937740
009300*            *-----------------------------*                      0937740
009400*            * PL CD INVALID WITH TRAN OPT *                      0937740
009410*            * IM2508                      *                      0937740
009500*            *-----------------------------*                      0937740
009600             MOVE +11 TO WK-PE-ERR-CD (PE-IDX)                    0937740
009610             MOVE '1' TO WK-PE-ERR-PC (PE-IDX)                    0937740
009700             SET PE-IDX UP BY 1.                                  0937740
009800*
009810* TR-TYPE 2 CREDIT OPT-11 FED TAX OPT-16 STATE TAX                0937740
009820* OPT-12 NONRPT PLN  PL-CD > 49 OR SPACE CONTRIBUTION             0937740
009830     IF  (WK-PE-TR-TYPE     EQUAL '2')                            0937740
009840     AND ((WK-PE-OPT-11     EQUAL '2' OR '3' OR '6' OR '7')       0937740
009850     OR  (WK-PE-OPT-16      EQUAL '1' OR '3' OR '5' OR '7'))      0937740
009860     AND ((WK-PE-PL-CD GREATER THAN '49')                         0937740
009870     OR  (WK-PE-PL-CD EQUAL '  ')                                 0937740
009880     OR  (WK-PE-OPT-12      EQUAL '2' OR '3' OR '6' OR '7'))      0937740
009890*        *-------------------------------------*                  0937740
009900*        * INVALID COMBINATION CREDIT - IM2549 *                  0937740
009910*        *-------------------------------------*                  0937740
009920* IF TRANSACTION ONLY EDIT, ERROR IF A PL-CD WAS ENTERED          0937740
009930         IF  ((WK-PE-MASTER EQUAL 'Y')                            0937740
009940         OR  (WK-PE-PL-CD GREATER THAN SPACES))                   0937740
009950             MOVE +19 TO WK-PE-ERR-CD (PE-IDX)                    0937740
009960             MOVE '1' TO WK-PE-ERR-PC (PE-IDX)                    0937740
009970             SET PE-IDX UP BY 1.                                  0937740
010000*                                                                 0937740
010010* TR-TYPE 4 DEBIT OPT-13 FED TAX OPT-2 STATE TAX                  0937740
010020* OPT-18 NONRPT PLN  PL-CD > '49' CONTRIBUTION                    0937740
010030     IF  (WK-PE-TR-TYPE     EQUAL '4')                            0937740
010040     AND ((WK-PE-OPT-13     EQUAL '1' OR '3' OR '5' OR '7')       0937740
010050     OR  (WK-PE-OPT-2       EQUAL '4' OR '5' OR '6' OR '7'))      0937740
010060     AND ((WK-PE-PL-CD GREATER THAN '49')                         0937740
010070     OR  (WK-PE-OPT-18      EQUAL '1' OR '3' OR '5' OR '7'))      0937740
010080*        *------------------------------------*                   0937740
010090*        * INVALID COMBINATION DEBIT - IM2550 *                   0937740
010100*        *------------------------------------*                   0937740
010110* IF TRANSACTION ONLY EDIT, ERROR IF A PL-CD WAS ENTERED          0937740
010120         IF  ((WK-PE-MASTER EQUAL 'Y')                            0937740
010130         OR  (WK-PE-PL-CD GREATER THAN SPACES))                   0937740
010140             MOVE +20 TO WK-PE-ERR-CD (PE-IDX)                    0937740
010150             MOVE '1' TO WK-PE-ERR-PC (PE-IDX)                    0937740
010160             SET PE-IDX UP BY 1.                                  0937740
010200*                                                                 0937740
010210* WHEN ADDITIONAL PLAN TYPES ARE VALID IN THE FUTURE              0937740
010220*  THE FOLLOWING IF QUALIFIER SHOULD BE INCLUDED                  0937740
010230* IF WK-PE-PL-TYPE 10 HSA EDIT                                    0937740
010240* TR-TYPE 2 CREDIT OPT-12 NONRPT PLN                              0937740
010250     IF  (WK-PE-TR-TYPE      EQUAL '2')                           0937740
010260     AND (WK-PE-OPT-12       EQUAL '2' OR '3' OR '6' OR '7')      0937740
010270         IF  WK-PE-PL-CD NOT EQUAL SPACES                         0937740
010280*            *-----------------------------*                      0937740
010300*            * PL CD INVALID WITH TRAN OPT *
010310*            * IM2508                      *                      0937740
010400*            *-----------------------------*
010500             MOVE +11 TO WK-PE-ERR-CD (PE-IDX)
010510             MOVE '1' TO WK-PE-ERR-PC (PE-IDX)                    0937740
010600             SET PE-IDX UP BY 1
010700         ELSE
010800             MOVE '50' TO WK-PE-PL-CD.
010900*
010910* IF WK-PE-PL-TYPE 10 HSA EDIT                                    0937740
010920* TR-TYPE 4 DEBIT OPT-18 NONRPT PLN                               0937740
011000     IF  (WK-PE-TR-TYPE      EQUAL '4')
011100     AND (WK-PE-OPT-18       EQUAL '1' OR '3' OR '5' OR '7')
011200         IF  WK-PE-PL-CD NOT EQUAL SPACES
011300*            *-----------------------------*
011400*            * PL CD NOT VALID WITH TR OPT *
011410*            * IM2508                      *                      0937740
011500*            *-----------------------------*
011600             MOVE +11 TO WK-PE-ERR-CD (PE-IDX)
011610             MOVE '1' TO WK-PE-ERR-PC (PE-IDX)                    0937740
011700             SET PE-IDX UP BY 1
011800         ELSE
011900             MOVE '00' TO WK-PE-PL-CD.
012000*
012010* IF WK-PE-PL-TYPE 10 HSA EDIT                                    0937740
012020* TR-TYPE 7 SAVINGS TRLR CREDIT OPT-9 NONRPT PLN (FUTURE USE)     0937740
012030* SAVINGS TRAILER IS NOT VALID WITH PLAN TRAILER AT THIS TIME     0937740
012100     IF  (WK-PE-TR-TYPE      EQUAL '7')
012200     AND (WK-PE-OPT-9        EQUAL '2' OR '3' OR '6' OR '7')
012300         IF  WK-PE-PL-CD NOT EQUAL SPACES
012400*            *-----------------------------*
012500*            * PL CD INVALID WITH TRAN OPT *
012510*            * IM2508                      *                      0937740
012600*            *-----------------------------*
012700             MOVE +11 TO WK-PE-ERR-CD (PE-IDX)
012710             MOVE '1' TO WK-PE-ERR-PC (PE-IDX)                    0937740
012800             SET PE-IDX UP BY 1
012900         ELSE
013000             MOVE '50' TO WK-PE-PL-CD.
013100*
013110* IF WK-PE-PL-TYPE 10 HSA EDIT                                    0937740
013120* TR-TYPE 8 SAVINGS TRLR DEBIT OPT-9 NONRPT PLN (FUTURE USE)      0937740
013130* SAVINGS TRAILER IS NOT VALID WITH PLAN TRAILER AT THIS TIME     0937740
013200     IF  (WK-PE-TR-TYPE      EQUAL '8')
013300     AND (WK-PE-OPT-9        EQUAL '1' OR '3' OR '5' OR '7')
013400         IF  WK-PE-PL-CD NOT EQUAL SPACES
013500*            *-----------------------------*
013600*            * PL CD INVALID WITH TRAN OPT *
013610*            * IM2508                      *                      0937740
013700*            *-----------------------------*
013800             MOVE +11 TO WK-PE-ERR-CD (PE-IDX)
013810             MOVE '1' TO WK-PE-ERR-PC (PE-IDX)                    0937740
013900             SET PE-IDX UP BY 1
014000         ELSE
014100             MOVE '00' TO WK-PE-PL-CD.
014200*
014210* IF WK-PE-PL-TYPE 10 HSA EDIT                                    0937740
014220* TR-TYPE 1-DEPOSIT 2-CREDIT                                      0937740
014230*         3-CHECK   4-DEBIT                                       0937740
014240* PL-CD 21 - HSA NORMAL DISTRIBUTION                              0937740
014250* PL-CD 71 - HSA NORMAL CONTRIBUTION                              0937740
014300     IF  WK-PE-PL-CD EQUAL SPACES
014400         IF  (WK-PE-TR-TYPE  EQUAL '1' OR '2')
014500             MOVE '71' TO WK-PE-PL-CD
014600         ELSE
014700         IF  (WK-PE-TR-TYPE  EQUAL '3' OR '4')
014800             MOVE '21' TO WK-PE-PL-CD.
014900*
015000 PL-CD-MAST-EDITS.
015100*
015200     IF  WK-PE-MASTER NOT EQUAL 'Y'
015300         GO TO PL-CD-EDITS-EXIT.
015400*
015410 PL-CD-MAST-EDIT-TYPE-10.                                         0937740
015420*                                                                 0937740
015500     IF  WK-PE-TR-INFO NOT EQUAL 'Y'
015600         GO TO PL-CD-MAST-ONLY-TYPE-10.                           0937740
015610*                                                                 0937740
015620* TRANSACTION EDITS WITH MASTER FIELDS                            0937740
015630* NOTE: ST INCLUDES SIMILAR EDITS IN PROCESS MODULES              0937740
015700*
015710* TR-TYPE 1 DEPOSIT OPT-6 FORCE PLAN                              0937740
015720* VALID-CONT - 1 NO                                               0937740
015730* PL-CD 71 - HSA NORMAL PL-CD 79 - HSA EMPLOYER CONTRIBUTION      0937740
015800     IF  (WK-PE-TR-TYPE       EQUAL '1')
015900     AND (WK-PE-VALID-CONT    EQUAL '1')
016000         IF  (WK-PE-PL-CD     EQUAL '71' OR '79')
016100             IF  (WK-PE-OPT-6 EQUAL '1' OR '3' OR '5' OR '7')
016200                 NEXT SENTENCE
016300             ELSE
016400*                *-----------------------------*
016500*                *   INVALID FOR CONTR IM2507  *                  0937740
016600*                *-----------------------------*
016700                 MOVE +8 TO WK-PE-ERR-CD (PE-IDX)
016710                 MOVE '1' TO WK-PE-ERR-PC (PE-IDX)                0937740
016800                 SET PE-IDX UP BY 1.
016900*
016910* TR-TYPE 2 CREDIT OPT-12 FORCE PLAN                              0937740
016920* VALID-CONT - 1 NO                                               0937740
016930* PL-CD 71 - HSA NORMAL PL-CD 79 - HSA EMPLOYER CONTRIBUTION      0937740
017000     IF  (WK-PE-TR-TYPE        EQUAL '2')
017100     AND (WK-PE-VALID-CONT     EQUAL '1')
017200         IF  (WK-PE-PL-CD      EQUAL '71' OR '79')
017300             IF  (WK-PE-OPT-12 EQUAL '1' OR '3' OR '5' OR '7')
017400                 NEXT SENTENCE
017500             ELSE
017600*                *-----------------------------*
017700*                *   INVALID FOR CONTR IM2507  *                  0937740
017800*                *-----------------------------*
017900                 MOVE +8 TO WK-PE-ERR-CD (PE-IDX)
017910                 MOVE '1' TO WK-PE-ERR-PC (PE-IDX)                0937740
018000                 SET PE-IDX UP BY 1.
018100*
031100 PL-CD-MAST-ONLY-TYPE-10.                                         0937740
031200*                                                                 0937740
031300     IF  (WK-PE-PL-CD EQUAL '  ' OR '00' OR '21' OR '22' OR '23'  0937740
031400                         OR '24' OR '27' OR '35' OR '36' OR '37'  0937740
031500                         OR '38' OR '50' OR '71' OR '72' OR '77'  0937740
031600                         OR '78' OR '79')                         0937740
031700         NEXT SENTENCE                                            0937740
031800     ELSE                                                         0937740
031900*        *----------------------------------*                     0937740
032000*        * PL CODE INVALID FOR HSA - IM2521 *                     0937740
032100*        *----------------------------------*                     0937740
032200         MOVE +13 TO WK-PE-ERR-CD (PE-IDX)                        0937740
032300         MOVE '1' TO WK-PE-ERR-PC (PE-IDX)                        0937740
032400         SET PE-IDX UP BY 1                                       0937740
032500         GO TO PL-CD-EDITS-EXIT.                                  0937740
032600*                                                                 0937740
032700******************************************************************0937740
032800* EDITS AGAINST THE MASTER FIELDS EXCEPT PLAN TYPE ARE NOT        0937740
032900* DONE WHEN PERFORMING AN ADJUSTMENT ON THE IM29 OR IM60 SCREEN   0937740
033000******************************************************************0937740
033100     IF  WK-PE-PGM EQUAL 'IM29' OR 'IM60'                         0937740
033200         GO TO PL-TYPE-10-CD2                                     0937740
033300     ELSE                                                         0937740
033400     IF  WK-PE-PGM     EQUAL 'IM2I'                               0937740
033500     AND WK-PE-PL-CD NOT EQUAL ' '                                0937740
033600*        *-------------------------------*                        0937740
033700*        * INT DIST TYP INV HSA - IM2546 *                        0937740
033800*        *-------------------------------*                        0937740
033900         MOVE +14 TO WK-PE-ERR-CD (PE-IDX)                        0937740
034000         MOVE '1' TO WK-PE-ERR-PC (PE-IDX)                        0937740
034100         SET PE-IDX UP BY 1                                       0937740
034200         GO TO PL-CD-EDITS-EXIT.                                  0937740
034300*                                                                 0937740
034400 PL-TYPE-10-CONT.                                                 0937740
034500* WK-PE-PART-EXC-FLG                                              0937740
034600* 0  - NO EXCEPTION                                               0937740
034700* 1  - DISABLED                                                   0937740
034800* 2  - DECEASED                                                   0937740
034900*                                                                 0937740
035000*                                                                 0937740
035100     IF  (WK-PE-PL-CD            EQUAL '24')                      0937740
035200     AND (WK-PE-PART-EXC-FLG NOT EQUAL '1')                       0937740
035300*        *------------------------------*                         0937740
035400*        * IF PL DIST CODE EQ 24, PART  *                         0937740
035500*        * EXC FLAG MUST EQ 1 - IM2529  *                         0937740
035600*        *------------------------------*                         0937740
035700         MOVE +1  TO WK-PE-ERR-CD (PE-IDX)                        0937740
035800         MOVE '1' TO WK-PE-ERR-PC (PE-IDX)                        0937740
035900         SET PE-IDX UP BY 1                                       0937740
036000         GO TO PL-CD-EDITS-EXIT.                                  0937740
036100*                                                                 0937740
036200     IF  (WK-PE-PART-EXC-FLG EQUAL '1')                           0937740
036300     AND (WK-PE-PL-CD        EQUAL '21' OR '35' OR '36')          0937740
036400*        *-----------------------------------------*              0937740
036500*        * PLAN ACCOUNT OWNER IS DISABLED - IM2509 *              0937740
036600*        *-----------------------------------------*              0937740
036700         MOVE +2  TO WK-PE-ERR-CD (PE-IDX)                        0937740
036800         MOVE '1' TO WK-PE-ERR-PC (PE-IDX)                        0937740
036900         SET PE-IDX UP BY 1                                       0937740
037000         GO TO PL-CD-EDITS-EXIT.                                  0937740
037100*                                                                 0937740
037200* WK-PE-BENE-FLG                                                  0937740
037300* E - ESTATE   (EST)                                              0937740
037400* N - NOT A BENEFICIARY   (NO)                                    0937740
037500* S - SPOUSE   (SPO)                                              0937740
037600* Y - NON-SPOUSE/NON-ESTATE/OTHER OR UNKNOWN TYPE   (OTH)         0937740
037700*                                                                 0937740
037800     IF  ((WK-PE-PART-EXC-FLG   EQUAL '2')                        0937740
037900     OR  (WK-PE-BENE-FLG    NOT EQUAL 'N'))                       0937740
038000     AND (WK-PE-PL-CD           EQUAL '22' OR '23' OR '27')       0937740
038100         IF  WK-PE-PART-EXC-FLG EQUAL '2'                         0937740
038200*            *-----------------------------------------*          0937740
038300*            * PLAN ACCOUNT OWNER IS DECEASED - IM2547 *          0937740
038400*            *-----------------------------------------*          0937740
038500             MOVE +15 TO WK-PE-ERR-CD (PE-IDX)                    0937740
038600             MOVE '1' TO WK-PE-ERR-PC (PE-IDX)                    0937740
038700             SET PE-IDX UP BY 1                                   0937740
038800             GO TO PL-CD-EDITS-EXIT                               0937740
038900         ELSE                                                     0937740
039000*            *--------------------------------------------*       0937740
039100*            * PLAN ACCOUNT OWNER IS BENEFICIARY - IM2537 *       0937740
039200*            *--------------------------------------------*       0937740
039300             MOVE +4  TO WK-PE-ERR-CD (PE-IDX)                    0937740
039400             MOVE '1' TO WK-PE-ERR-PC (PE-IDX)                    0937740
039500             SET PE-IDX UP BY 1                                   0937740
039600             GO TO PL-CD-EDITS-EXIT.                              0937740
039700*                                                                 0937740
039800     IF  (WK-PE-PL-CD         EQUAL '  ' OR '21' OR '35' OR '36') 0937740
039900     AND ((WK-PE-BENE-FLG NOT EQUAL 'N')                          0937740
040000     OR  (WK-PE-PART-EXC-FLG  EQUAL '2'))                         0937740
040100     AND (WK-PE-DEATH-DT      EQUAL SPACES OR ZEROES)             0937740
040200*        *--------------------------------------*                 0937740
040300*        * DEATH DATE REQUIRED FOR HSA DCD/BENE *                 0937740
040400*        * TO TAKE DEATH DISTRIBUTIONS - IM2536 *                 0937740
040500*        *--------------------------------------*                 0937740
040600         MOVE +3  TO WK-PE-ERR-CD (PE-IDX)                        0937740
040700         MOVE '1' TO WK-PE-ERR-PC (PE-IDX)                        0937740
040800         SET PE-IDX UP BY 1                                       0937740
040900         GO TO PL-CD-EDITS-EXIT.                                  0937740
041000*                                                                 0937740
041100     IF  (WK-PE-PL-CD               EQUAL '  ' OR '21')           0937740
041200     AND ((WK-PE-BENE-FLG       NOT EQUAL 'N')                    0937740
041300     OR  (WK-PE-PART-EXC-FLG        EQUAL '2'))                   0937740
041400         IF  (WK-PE-DEATH-YR GREATER THAN ZERO)                   0937740
041500         AND (WK-PE-DEATH-YR    LESS THAN WK-PE-RUN-YEAR)         0937740
041600             IF  (WK-PE-BENE-FLG    EQUAL 'S')                    0937740
041700                 NEXT SENTENCE                                    0937740
041800             ELSE                                                 0937740
041900*                *------------------------*                       0937740
042000*                * INVLD PL CODE FOR BENE *                       0937740
042100*                * FLG/PART EXC - IM2540  *                       0937740
042200*                *------------------------*                       0937740
042300                 MOVE +7  TO WK-PE-ERR-CD (PE-IDX)                0937740
042400                 MOVE '1' TO WK-PE-ERR-PC (PE-IDX)                0937740
042500                 SET PE-IDX UP BY 1                               0937740
042600         ELSE                                                     0937740
042700*            *-----------------------------------*                0937740
042800*            * PL CD/DEATH YEAR INVALID - IM2510 *                0937740
042900*            *-----------------------------------*                0937740
043000             MOVE +5  TO WK-PE-ERR-CD (PE-IDX)                    0937740
043100             MOVE '1' TO WK-PE-ERR-PC (PE-IDX)                    0937740
043200             SET PE-IDX UP BY 1.                                  0937740
043300*                                                                 0937740
043400     IF  (WK-PE-PL-CD            EQUAL '35' OR '36')              0937740
043500     AND (WK-PE-PART-EXC-FLG NOT EQUAL '2')                       0937740
043600     AND (WK-PE-BENE-FLG         EQUAL 'N')                       0937740
043700*        *-----------------------------------------*              0937740
043800*        * IF PL DIST CODE EQ 35/36; PART EXC FLAG *              0937740
043900*        * MUST EQ 2 OR ACCT A BENE ACCT - IM2548  *              0937740
044000*        *-----------------------------------------*              0937740
044100         MOVE +16 TO WK-PE-ERR-CD (PE-IDX)                        0937740
044200         MOVE '1' TO WK-PE-ERR-PC (PE-IDX)                        0937740
044300         SET PE-IDX UP BY 1                                       0937740
044400         GO TO PL-CD-EDITS-EXIT.                                  0937740
044500*                                                                 0937740
044600     IF  (WK-PE-PL-CD                EQUAL '35')                  0937740
044700         IF  (WK-PE-DEATH-YR  GREATER THAN ZERO)                  0937740
044800         AND (WK-PE-DEATH-YR         EQUAL WK-PE-RUN-YEAR)        0937740
044900             NEXT SENTENCE                                        0937740
045000         ELSE                                                     0937740
045100             IF  WK-PE-BENE-FLG EQUAL 'E'                         0937740
045200                 NEXT SENTENCE                                    0937740
045300             ELSE                                                 0937740
045400*                *------------------------*                       0937740
045500*                * INVLD PL CODE FOR BENE *                       0937740
045600*                * FLG/PART EXC - IM2540  *                       0937740
045700*                *------------------------*                       0937740
045800                 MOVE +7  TO WK-PE-ERR-CD (PE-IDX)                0937740
045900                 MOVE '1' TO WK-PE-ERR-PC (PE-IDX)                0937740
046000                 SET PE-IDX UP BY 1.                              0937740
046100*                                                                 0937740
046200     IF  (WK-PE-PL-CD               EQUAL '36')                   0937740
046300     AND ((WK-PE-BENE-FLG       NOT EQUAL 'N')                    0937740
046400     OR  (WK-PE-PART-EXC-FLG        EQUAL '2'))                   0937740
046500         IF  (WK-PE-DEATH-YR GREATER THAN ZERO)                   0937740
046600         AND (WK-PE-DEATH-YR    LESS THAN WK-PE-RUN-YEAR)         0937740
046700             IF  (WK-PE-BENE-FLG    EQUAL 'Y')                    0937740
046800                 NEXT SENTENCE                                    0937740
046900             ELSE                                                 0937740
047000*                *------------------------*                       0937740
047100*                * INVLD PL CODE FOR BENE *                       0937740
047200*                * FLG/PART EXC - IM2540  *                       0937740
047300*                *------------------------*                       0937740
047400                 MOVE +7  TO WK-PE-ERR-CD (PE-IDX)                0937740
047500                 MOVE '1' TO WK-PE-ERR-PC (PE-IDX)                0937740
047600                 SET PE-IDX UP BY 1                               0937740
047700         ELSE                                                     0937740
047800*            *-----------------------------------*                0937740
047900*            * PL CD/DEATH YEAR INVALID - IM2510 *                0937740
048000*            *-----------------------------------*                0937740
048100             MOVE +5  TO WK-PE-ERR-CD (PE-IDX)                    0937740
048200             MOVE '1' TO WK-PE-ERR-PC (PE-IDX)                    0937740
048300             SET PE-IDX UP BY 1.                                  0937740
048400*                                                                 0937740
048500 PL-TYPE-10-CD2.                                                  0937740
048600*                                                                 0937740
048700     IF  WK-PE-PL-CD2 EQUAL '  '                                  0937740
048800         GO TO PL-CD-EDITS-EXIT.                                  0937740
048900*                                                                 0937740
049000*    *---------------------------------------------*              0937740
049100*    * INVALID CODE AND TYPE COMBINATIONS - IM2515 *              0937740
049200*    *---------------------------------------------*              0937740
049300     MOVE +17 TO WK-PE-ERR-CD (PE-IDX).                           0937740
049400     MOVE '2' TO WK-PE-ERR-PC (PE-IDX).                           0937740
049500     SET PE-IDX UP BY 1.                                          0937740
049600*                                                                 0937740
049700     GO TO PL-CD-EDITS-EXIT.                                      0937740
049800*                                                                 0937740
090000 PL-CD-EDITS-EXIT.                                                0937740
090100     EXIT.                                                        0937740
