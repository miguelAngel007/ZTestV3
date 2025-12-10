*     * FO5238*12/05/11 JCTE PROYECTO UPGRADE SYSTEMAT
000100*----------------------------------------------------------------*
000200*    DAILY MONETARY ACTIVITY FILE FOR AUTOBORROW AND COMMERCIAL  *
000300*    SWEEPS.                                                     *
000400*----------------------------------------------------------------*
000500 01  IM-ACM-ABM-MONETARY.
000600     03  IM-ACD-BANK-ID          PIC 9(04)       COMP.
000700     03  IM-ACD-ACCT-NO          PIC S9(15)      COMP-3.
000800     03  IM-ACD-COST-CENT        PIC 9(08).
000900     03  IM-ACD-DDA-SAV-BAL      PIC S9(11)V99   COMP-3.
001000     03  IM-ACD-TRNSF-AMT        PIC S9(11)V99   COMP-3.
001100     03  IM-ACD-POST-DATE        PIC S9(07)      COMP-3.
001200     03  IM-ACD-TRAN-CODE        PIC X(01).
001300     03  IM-ACD-DDA-BAL          PIC S9(11)V99   COMP-3.
001400     03  IM-ACD-FUND-NUMB        PIC S9(15)      COMP-3.
001500     03  IM-ACD-MTH-END-IND      PIC X(01).
