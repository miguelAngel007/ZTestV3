*     * 802262 12/11/20 PROYECTO UPGRADE GN172 EDVR
000010 01  DATE-AREA.
000015   03  DATE-AREA-6.                                               2602675
000020     04  DT-LOW-DATE.                                             2602675
000030         05  DT-L-MO     PICTURE XX.
000040         05  DT-L-DA     PICTURE XX.
000050         05  DT-L-YR     PICTURE XX.
000060     04  FILLER REDEFINES DT-LOW-DATE.                            2602675
000070         05  DT-L-MO9    PICTURE 99.
000080         05  DT-L-DA9    PICTURE 99.
000090         05  DT-L-YR9    PICTURE 99.
000095     04  DT-LOW-DATEY REDEFINES DT-LOW-DATE      PICTURE 9(6).    2602675
000100     04  DT-HIGH-DATE.                                            2602675
000110         05  DT-H-MO     PICTURE XX.
000120         05  DT-H-DA     PICTURE XX.
000130         05  DT-H-YR     PICTURE XX.
000140     04  FILLER REDEFINES DT-HIGH-DATE.                           2602675
000150         05  DT-H-MO9    PICTURE 99.
000160         05  DT-H-DA9    PICTURE 99.
000170         05  DT-H-YR9    PICTURE 99.
000175     04  DT-HIGH-DATEY REDEFINES DT-HIGH-DATE    PICTURE 9(6).    2602675
000180     04  FILLER REDEFINES DT-HIGH-DATE.                           2602675
000190         05  DATE-FLAG   PICTURE X.
000200         05  FILLER      PICTURE X(5).
000210     04  FILLER REDEFINES DT-HIGH-DATE.                           2602675
000220         05  DT-JUL-YR   PICTURE S999    COMPUTATIONAL-3.
000230         05  DT-JUL-DA   PICTURE S999    COMPUTATIONAL-3.
000240         05  FILLER      PICTURE XX.
000250     04  RET-DAYS        PICTURE S9(5)   COMPUTATIONAL-3.         2602675
000260   03  DATE-AREA-8 REDEFINES DATE-AREA-6.                         2602675
000270     04  DT-LOW8-DATE.                                            2602675
000280         05  DT-L8-MO     PICTURE XX.                             2602675
000290         05  DT-L8-DA     PICTURE XX.                             2602675
000300         05  DT-L8-CC     PICTURE XX.                             2602675
000310         05  DT-L8-YR     PICTURE XX.                             2602675
000320     04  FILLER REDEFINES DT-LOW8-DATE.                           2602675
000330         05  DT-L8-MO9    PICTURE 99.                             2602675
000340         05  DT-L8-DA9    PICTURE 99.                             2602675
000350         05  DT-L8-CC9    PICTURE 99.                             2602675
000360         05  DT-L8-YR9    PICTURE 99.                             2602675
000370     04  DT-LOW8-DATEY REDEFINES DT-LOW8-DATE      PICTURE 9(8).  2602675
000380     04  DT-JUL8-YR       PICTURE S9(5) COMPUTATIONAL-3.          2602675
000390     04  FILLER REDEFINES DT-JUL8-YR.                             2602675
000400         05  DATE8-FLAG  PICTURE X.                               2602675
000410         05  FILLER      PICTURE X(2).                            2602675
000420     04  DT-JUL8-DA       PICTURE S9(3) COMPUTATIONAL-3.          2602675
000430     04  FILLER           PICTURE X(2).                           2602675
000440   03  BASE-YEAR       PICTURE 9(2)    VALUE 50.                  2602675
000450   03  DATE-FORMAT     PICTURE X       VALUE '6'.                 2602675
