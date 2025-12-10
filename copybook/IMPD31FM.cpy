*     * FO5238*12/05/11 JCTE PROYECTO UPGRADE SYSTEMAT
000100*----------------------------------------------------------------*
000200*                                                                *
000300*         THIS USER EXIT WILL CALCULATE THE FAIR MARKET VALUE    *
000400*         OF A PLAN ACCOUNT.                                     *
000500*         THE FAIR MARKET VALUE WILL BE CALCULATED USING THE     *
000600*         FOLLOWING FORMULA:                                     *
000700*                                                                *
000800*         FAIR MARKET VALUE =  CURRENT BALANCE                   *
000900*                           +  ACCRUED INTEREST                  *
001000*                                                                *
001100*          THIS CALCULATION WILL ONLY BE PERFORMED ON THE LAST   *
001200*          PROCESSING DAY OF THE CALENDAR YEAR.  AT THIS TIME    *
001300*          THE YEAR END ACCRUAL AMOUNT WILL BE SET TO BE EQUAL   *
001400*          TO THE ACCRUED INTEREST ON THAT DATE.                 *
001500*                                                                *
001600*----------------------------------------------------------------*
001700 CALC-FAIR-MARKET-VALUE.
001800     MOVE WMS-IOD-CYC-ACC-INT TO HOLD-INTEREST.
001900     IF  WMS-SAVINGS-TRLR EQUAL '1'
002000         ADD WMS-ACCR-INT TO HOLD-INTEREST.
002100     COMPUTE WMS-PLN-TRLR-YR-END-ACCR-CUR ROUNDED
002200                                = HOLD-INTEREST.
002300     COMPUTE WMS-PLN-TRLR-FAIR-MKT-VAL-CUR ROUNDED
002400                EQUAL  WMS-CURR-BAL
002500                     + WMS-PLN-TRLR-YR-END-ACCR-CUR.
002600 CFMV-EXIT.
002700     EXIT.
002800
