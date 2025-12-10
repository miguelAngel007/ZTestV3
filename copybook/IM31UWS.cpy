*     * 512823*01/07/25 JCTE SPLIT CON RANGOS EN DIFERENTES TASAS.    **
*     * 410223*10/06/24 JCTE METODOLOGIA SPLIT EN SALDO COMPROMISO.   **
*     * 203917*30/12/22 JCTE INT.ACREEDOR CTA.REMUNERADAS. MES VIGENTE..
*     * FO5238*12/05/11 JCTE PROYECTO UPGRADE SYSTEMAT
000100*----------------------------------------------------------------*
000200*         IM31 USER WORKING STORAGE COPYBOOK                     *
000300*----------------------------------------------------------------*
000400 01  IB-RATE-PGM                     PIC X(8) VALUE SPACES.       IMIB056
000500 01  IB-SW                           PIC 9    VALUE 0.            IMIB056
000600     88  IB-OPEN                              VALUE 0.            IMIB056
000700     88  IB-READ                              VALUE 1.            IMIB056
000800     88  IB-CLOSE                             VALUE 2.            IMIB056
000900 01  IB-CONTROL-KEY.                                              IMIB056
001000     03  IB-CONTROLS                 PIC X(12)       VALUE SPACES.IMIB056
001100     03  IB-ACCT-NO                  PIC X(10)       VALUE SPACES.IMIB056
001110     03  IB-ACCRUAL-DATE             PIC X(08)       VALUE SPACES.203917
001200     03  IB-CUST-NO                  PIC X(12)       VALUE SPACES.IMIB056
001201     03  IB-TIPO-TASA                PIC 9(01)       VALUE ZEROES.512823
001202     03  IB-TIPO-SALDO               PIC 9(01)       VALUE ZEROES.410223
001203     03  IB-ACCRUAL-BALANCE          PIC S9(13)V99 COMP-3 VALUE 0.410223
001300 01  IB-RATE-FIELDS.                                              IMIB056
001400     03  IB-RET-CODE      PIC 9(4)             VALUE ZEROES.      IMIB056
001500     03  IB-DAILY-RATE    PIC SVPP9(15) COMP-3 OCCURS 9 TIMES.    IMIB056
001600     03  IB-LIMIT         PIC S9(13)    COMP-3 OCCURS 8 TIMES.    IMIB056
001700 01  IB-CONTROLS-ST.                                              IMIBXXX
001800     05  IB-CONTROL-KEY-ST           PIC  X(22).                  IMIBXXX
001900     05  IB-ACCT-TYPE-ST             PIC  X(03).                  IMIBXXX
002000     05  IB-CURR-BAL-ST              PIC S9(13)V99   COMP-3.      IMIBXXX
002100 01  IB-OUTPUT-ST.                                                IMIBXXX
002200     05  IB-RETURN-CODE-ST           PIC  9(04).                  IMIBXXX
002300     05  IB-INGRESO-ST               PIC  X(01).                  IMIBXXX
002400         88 IB-SI-INGRESO-ST         VALUE '1'.                   IMIBXXX
002500         88 IB-NO-INGRESO-ST         VALUE '2'.                   IMIBXXX
002600 01  WRK-DC-DSC-DATE.                                             IMIB004
002700     03  WRK-DC-DSC-MO               PIC  XX.                     IMIB004
002800     03  WRK-DC-DSC-DA               PIC  XX.                     IMIB004
002900     03  WRK-DC-DSC-CC               PIC  XX.                     IMIB004
003000     03  WRK-DC-DSC-YY               PIC  XX.                     IMIB004
004100     SKIP3
