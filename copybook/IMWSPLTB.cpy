*     * FO5238*12/05/11 JCTE PROYECTO UPGRADE SYSTEMAT
000100 01  PL-CODE-TABLE.
000200     03  PL-CODE-TAB.
000300         05  FILLER      PIC XXXX    VALUE '00  '.
000400         05  FILLER      PIC X(18)   VALUE 'NON-REPORTABLE    '.
000500         05  FILLER      PIC X(18)   VALUE '                  '.
000600         05  FILLER      PIC XXXX    VALUE '21  '.
000700         05  FILLER      PIC X(18)   VALUE 'HSA NORMAL/DEATH  '.  0727606
000800         05  FILLER      PIC X(18)   VALUE '                  '.
000900         05  FILLER      PIC XXXX    VALUE '22  '.
001000         05  FILLER      PIC X(18)   VALUE 'HSA EXC CONTR     '.
001100         05  FILLER      PIC X(18)   VALUE '                  '.
001200         05  FILLER      PIC XXXX    VALUE '23  '.
001300         05  FILLER      PIC X(18)   VALUE 'HSA EXC CONTR EARN'.
001400         05  FILLER      PIC X(18)   VALUE '                  '.
001500         05  FILLER      PIC XXXX    VALUE '24  '.
001600         05  FILLER      PIC X(18)   VALUE 'HSA DISABILITY    '.
001700         05  FILLER      PIC X(18)   VALUE '                  '.
001800         05  FILLER      PIC XXXX    VALUE '27  '.
001900         05  FILLER      PIC X(18)   VALUE 'HSA PROHIBITED    '.
002000         05  FILLER      PIC X(18)   VALUE '                  '.
002100         05  FILLER      PIC XXXX    VALUE '35  '.
002200         05  FILLER      PIC X(18)   VALUE 'HSA DEATH         '.  0727606
002300         05  FILLER      PIC X(18)   VALUE '                  '.
002400         05  FILLER      PIC XXXX    VALUE '36  '.
002500         05  FILLER      PIC X(18)   VALUE 'HSA DEATH - OTHER '.
002600         05  FILLER      PIC X(18)   VALUE '                  '.
002700         05  FILLER      PIC XXXX    VALUE '37  '.
002800         05  FILLER      PIC X(18)   VALUE 'HSA EXT TRANSFER  '.
002900         05  FILLER      PIC X(18)   VALUE '                  '.
003000         05  FILLER      PIC XXXX    VALUE '38  '.
003100         05  FILLER      PIC X(18)   VALUE 'HSA INT TRANSFER  '.
003200         05  FILLER      PIC X(18)   VALUE '                  '.
003300         05  FILLER      PIC XXXX    VALUE '50  '.
003400         05  FILLER      PIC X(18)   VALUE 'NON-REPORTABLE    '.
003500         05  FILLER      PIC X(18)   VALUE '                  '.
003600         05  FILLER      PIC XXXX    VALUE '71  '.
003700         05  FILLER      PIC X(18)   VALUE 'HSA NORMAL        '.
003800         05  FILLER      PIC X(18)   VALUE '                  '.
003900         05  FILLER      PIC XXXX    VALUE '72  '.
004000         05  FILLER      PIC X(18)   VALUE 'HSA ROLLOVER      '.
004100         05  FILLER      PIC X(18)   VALUE '                  '.
004200         05  FILLER      PIC XXXX    VALUE '77  '.
004300         05  FILLER      PIC X(18)   VALUE 'HSA EXT TRANSFER  '.
004400         05  FILLER      PIC X(18)   VALUE '                  '.
004500         05  FILLER      PIC XXXX    VALUE '78  '.
004600         05  FILLER      PIC X(18)   VALUE 'HSA INT TRANSFER  '.
004700         05  FILLER      PIC X(18)   VALUE '                  '.
004800         05  FILLER      PIC XXXX    VALUE '79  '.
004900         05  FILLER      PIC X(18)   VALUE 'HSA EMPLOYER      '.  0727606
005000         05  FILLER      PIC X(18)   VALUE '                  '.
005100     03  PL-CODE-NAME   REDEFINES PL-CODE-TAB
005200                                                 OCCURS 16 TIMES
005300                                                 INDEXED BY
005400                                                 PL-CODE-IND.
005500         05  PL-COMBINED-CODES.
005600             07  PL-CODE1   PIC XX.
005700             07  PL-CODE2   PIC XX.
005800         05  PL-NAME.
005900             07  PL-NAME1   PIC X(18).
006000             07  PL-NAME2   PIC X(18).
006100     EJECT
