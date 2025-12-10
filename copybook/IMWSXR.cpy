*     * FO5238*12/05/11 JCTE PROYECTO UPGRADE SYSTEMAT
000100*----------------------------------------------------------------*
000200*    IMPACS GENERIC REGION FIELD BUILD WORKING-STORAGE           *
000300*    SECTION COPYBOOK                                            *
000400*                                                                *
000500*    YOU CAN ADD REDEFINES OF THE RATE AND SERVICE CHARGE        *
000600*    REGIONS TO THIS COPYBOOK TO BUILD USER-DEFINED REGION       *
000700*    FIELDS.                                                     *
000800*                                                                *
000900*    USE THIS COPYBOOK WITH THE IMPDXR COPYBOOK.                 *
001000*----------------------------------------------------------------*
001100 01  WS-BCR-INFO.
001200     03  WS-CTL2-FLAG            PIC X.
001300     03  WS-CONTROL-2            PIC XXX.
001400     03  WS-CTL3-FLAG            PIC X.
001500     03  WS-CONTROL-3            PIC XXX.
001600     03  WS-RATE-REGION-KEY.
001700         05  WS-RT-REG-FLD1      PIC X(4).
001800         05  WS-RT-REG-FLD2      PIC X(4).
001900         05  WS-RT-REG-FLD3      PIC X(4).
002000     03  WS-SVCH-REGION-KEY.
002100         05  WS-SC-REG-FLD1      PIC X(4).
002200         05  WS-SC-REG-FLD2      PIC X(4).
002300         05  WS-SC-REG-FLD3      PIC X(4).
002400
002500 01  WS-REGION-INFO.
002600     03  WS-MST-START            PIC S9999   COMP SYNC.
002700     03  WS-MST-LENGTH           PIC S9999   COMP SYNC.
002800     03  WS-KEY-START            PIC S9999   COMP SYNC.
002900     03  WS-KEY-LENGTH           PIC S9999   COMP SYNC.
003000     03  WS-FLD-NBR              PIC X(4).
003100     03  WS-FLD-NBR9 REDEFINES WS-FLD-NBR PIC 9(4).
003200     03  WS-CTL1-START           PIC 9(5)    VALUE 1.
003300     03  WS-CTL1-LENGTH          PIC 99      VALUE 2.
003400     03  WS-CTL2-START           PIC 9(5)    VALUE 3.
003500     03  WS-CTL2-LENGTH          PIC 99      VALUE 3.
003600     03  WS-CTL3-START           PIC 9(5)    VALUE 6.
003700     03  WS-CTL3-LENGTH          PIC 99      VALUE 3.
003800     03  WS-CTL4-START           PIC 9(5)    VALUE 9.
003900     03  WS-CTL4-LENGTH          PIC 99      VALUE 4.
004000     03  WS-REGION-KEY           PIC X(10)   VALUE ZEROES.
004100     03  FILLER REDEFINES WS-REGION-KEY.
004200         05  WS-BCR-CTL2         PIC X(3).
004300         05  WS-BCR-CTL3         PIC X(3).
004400         05  WS-REGION-SPACES    PIC X(4).
004500     03  WS-REGION-BYTE REDEFINES WS-REGION-KEY
004600                                 PIC X       OCCURS 10 TIMES.
004700
