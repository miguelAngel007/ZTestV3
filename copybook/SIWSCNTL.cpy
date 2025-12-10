*     * 802262 12/11/20 PROYECTO UPGRADE GN172 EDVR
000001*--------------------------------------------------------------*  9910919
000002*              ** COPYBOOK DESCRIPTION **                      *  9910919
000003*         I/O SUBROUTINE CONTROL AREA COPYBOOK                 *  9910919
000004*                                                              *  9910919
000005*--------------------------------------------------------------*  9910919
000006*               ** HISTORY OF REVISIONS **                     *  9910919
000007* DESCRIPTION                                           CHNGID *  9910919
000008* ____________________________________________________  _______*  9910919
000009*                                                              *  9910919
000094* 07/19/99 MODIFIED XRBA PROCESSING                     ~~~4102*  9910919
000096* 05/23/97 MODIFIED FOR 24X7                            ~~~0919*  9910919
000097*                                                              *  9910919
000098*--------------------------------------------------------------*  9910919
000099*                                                                 9910919
000100 01  I-O-CONTROL-AREA.
000200     03  I-O-CONTROL-OPERATOR    PIC X.
000300         88  I-O-88-OPEN-SEQ     VALUE 'O' 'P'.
000400         88  I-O-88-OPEN-KSDS    VALUE 'O'.
000500         88  I-O-88-OPEN-ESDS    VALUE 'P'.
000510         88  I-O-88-OPEN-HRBA    VALUE 'H'.                       9910919
000600         88  I-O-88-CLOSE        VALUE 'E'.
000700         88  I-O-88-START-SEQ    VALUE 'S'.
000800         88  I-O-88-START-SEQKEQ VALUE 'S'.
000900         88  I-O-88-START-SEQKGE VALUE 'T'.
001000         88  I-O-88-SEQ-READ     VALUE 'R'.
001100         88  I-O-88-KEYED-DIRECT VALUE 'K'.
001200         88  I-O-88-RBA-DIRECT   VALUE 'K'.
001300         88  I-O-88-REWRITE      VALUE 'W'.
001400         88  I-O-88-INSERT       VALUE 'A'.
001500         88  I-O-88-RBA-DIRRET   VALUE 'B'.
001600         88  I-O-88-DELETE       VALUE 'D'.
001700         88  I-O-88-REREAD       VALUE 'N'.
001800         88  I-O-88-LOAD         VALUE 'L'.
001900     03  I-O-CONTROL-ACCESS      PIC X.
002000         88  I-O-88-INPUT        VALUE 'I'.
002100         88  I-O-88-OUTPUT       VALUE 'O'.
002200         88  I-O-88-UPDATE       VALUE 'U'.
002300         88  I-O-88-LOAD-MODE    VALUE 'L'.
002400     03  I-O-RETURN-CODE         PIC S9(4) COMP.
002500         88  I-O-88-NORMAL-RET   VALUE +0000.
002600         88  I-O-88-END-OF-FILE  VALUE +0004.
002700         88  I-O-88-NOT-FOUND    VALUE +0008.
002800         88  I-O-88-FILE-EMPTY   VALUE +0012.
002900         88  I-O-88-ERROR        VALUE +0901 THRU +0999.
002910     03  I-O-SEND-CODE   REDEFINES  I-O-RETURN-CODE                ~~~4102
002920                                    PIC S9(4) COMP.                ~~~4102
002930         88  I-O-88-XRBA         VALUE -0001.                      ~~~4102
002940     03  I-O-RECORD-XRBAX        PIC X(8).                         ~~~4102
002950     03  I-O-RECORD-XRBA REDEFINES I-O-RECORD-XRBAX                ~~~4102
002960                                 PIC S9(16) COMP.                  ~~~4102
002970     03  FILLER REDEFINES I-O-RECORD-XRBA.                         ~~~4102
003000         05  I-O-RECORD-RBA      PIC X(4).
003100         05  I-O-BDAM-RELATIVE-BLOCK REDEFINES I-O-RECORD-RBA
003200                                 PIC S9(8) COMP.
003300         05  FILLER              PIC X(4).                         ~~~4102
