*     * 802262 12/11/20 PROYECTO UPGRADE GN172 EDVR
000100***************************************************************** 260
000200*  DICTIONARY COMMON AREA                                       * 260
000300*                                                               * 260
000400*                    HISTORY OF REVISIONS                         260
000410* DESCRIPTION                                           CHNGID  * 260
000500* ----------------------------------------------------  --------* 260
000596* 05/31/95 CORRECT THE PLACEMENT OF PHYSICAL KEY,       2602105 * 2602105
000597*          OPERATOR ID, AND AUDIT DATA.                 2602105 * 2602105
000598* 11/01/94 MODIFY AUDIT SAVE AREAS FOR CONTROL ID       2602105 * 2602105
000600* 03/01/92 ADD OPERATOR ID, AUDIT DATA, & PHYSICAL KEY  0300290 * 2600290
000700*                                                               * 260
000800***************************************************************** 260
000900 01  DICCAREA.                                                    260
001000     03  FILLER                            PIC X(432).            260
001100     SKIP1                                                        260
001200     03  DI-DICTIONARY-DDNAME              PIC X(08).             260
001300     03  DI-DICTIONARY-AUXILIARY           PIC X(08).             260
001400     03  DI-COMMAND                        PIC X(06).             260
001500     03  DI-DICTIONARY-MORE                PIC X.                 260
001600     03  DI-DICTIONARY-STATUS              PIC X.                 260
001700     03  DI-EXT-FLAGS                      PIC X(01).             260
001800     03  FILLER                            PIC X(03).             260
001900     03  DI-NUMBER-ACCESSES                PIC 9(08) COMP.        260
002000     SKIP1                                                        260
002100     03  DI-DICTIONARY-KEY.                                       260
002200         04  DI-DICTIONARY-KEY-CONTROL.                           260
002300             05  DI-DICTIONARY-APPL        PIC X(02).             260
002400             05  DI-DICTIONARY-FORMAT      PIC X(03).             260
002500             05  DI-DICTIONARY-ALIAS.                             260
002600                 07  DI-DICTIONARY-NAME    PIC X(08).             260
002700                 07  DI-DICTIONARY-FIELD-DATA.                    260
002800                     09  DI-DICTIONARY-FIELD PIC X(08).           260
002900                     09  DI-DICTIONARY-OCCUR PIC 9(08) COMP.      260
003000     03  DI-PREVIOUS-KEY.                                         260
003100         04  DI-PREVIOUS-KEY-CONTROL.                             260
003200             05  DI-PREVIOUS-APPL          PIC X(02).             260
003300             05  DI-PREVIOUS-FORMAT        PIC X(03).             260
003400             05  DI-PREVIOUS-NAME          PIC X(08).             260
003500         04  DI-PREVIOUS-FIELD-DATA.                              260
003600             05  DI-PREVIOUS-FIELD         PIC X(08).             260
003700             05  DI-PREVIOUS-OCCUR         PIC 9(08) COMP.        260
003800     SKIP1                                                        260
003900     03  DI-CURRENT-APPLICATION            PIC X(02).             260
004000     03  DI-CURRENT-FORMAT                 PIC X(03).             260
004100     03  DI-CURRENT-ENTITY                 PIC X(20).             260
004200     03  DI-CURRENT-NAME                   PIC X(20).             260
004210     03  FILLER                            PIC X(23).             2602105
004300     03  DI-PHYSICAL-INDEX-DATA.                                  260
004400         05  DI-INDEX-FLAG-OFF             PIC X.
004500         05  DI-INDEX-FLAG-ON              PIC X.
004600         05  DI-PHYSICAL-INDEX             PIC X(2).
004700     03  FILLER                            PIC X(98).             2602105
004900     03  ALIGNMENT                         PIC X(4).              2602105
005000     SKIP3                                                        260
005100***************  THIS AREA CLEARED UPON EACH COMMAND ************ 260
005200     03  DI-CONTROL-AREA.                                         260
005300         05  DI-KEY-LENGTH                 PIC 9(04) COMP.        260
005400         05  DI-KEY-SAVE                   PIC X(25).             260
005500         05  DI-PARS-FLAG                  PIC X.                 260
005600         05  DI-DEFS-FLAG                  PIC X.                 260
005700         05  DI-ENT-FLAG                   PIC X.                 260
005800         05  DI-EXT-FLAG                   PIC X.                 260
005900         05  DI-VAL-FLAG                   PIC X.                 260
006000         05  DI-STATUS-AREA.                                      260
006100             07  DI-WORK-AREA              PIC X(16).             260
006200             07  DI-LIT-START              PIC 9(08) COMP.        260
006300             07  DI-BUF-START              PIC 9(08) COMP.        260
006400             07  DI-ENTITY-ADDRESS         PIC 9(08) COMP.        260
006500             07  DI-FIELD-ADDRESS          PIC 9(08) COMP.        260
006600             07  DI-RECORD-LENGTH          PIC 9(08) COMP.        260
006700             07  DI-SEVERITY               PIC 9(04) COMP.        260
006800             07  DI-STATUS                 PIC 9(04) COMP.        260
006900             07  DI-ERROR-MESSAGE          PIC X(40).             260
007000         05  FILLER                        PIC X(144).            260
007100***************************************************************** 260
007200     SKIP2                                                        260
007210     03  DI-OPERATOR-ID                    PIC X(08).             2602105
007300     03  DI-AUDIT-DATA-SAVE-AREA.                                 260
007400         05  DI-ENT-CURR-MNTE-DATA.                               2602105
007500             07  DI-ENT-CURR-MNTE-YYMMDD   PIC 9(6) COMP-3.       2602105
007600             07  DI-ENT-CURR-MNTE-HHMMSS   PIC 9(6) COMP-3.       2602105
007700             07  DI-ENT-CURR-MNTE-OPER     PIC X(8).              2602105
007800             07  DI-ENT-CURR-MNTE-CHG-ID   PIC X(8).              2602105
007900         05  DI-ENT-LAST-MNTE-DATA.                               2602105
008000             07  DI-ENT-LAST-MNTE-YYMMDD   PIC 9(6) COMP-3.       2602105
008100             07  DI-ENT-LAST-MNTE-HHMMSS   PIC 9(6) COMP-3.       2602105
008200             07  DI-ENT-LAST-MNTE-OPER     PIC X(8).              2602105
008300             07  DI-ENT-LAST-MNTE-CHG-ID   PIC X(8).              2602105
008400         05  DI-HDR-LAST-MNTE-DATA.                               2602105
008500             07  DI-HDR-LAST-MNTE-YYMMDD   PIC 9(6) COMP-3.       2602105
008600             07  DI-HDR-LAST-MNTE-HHMMSS   PIC 9(6) COMP-3.       2602105
008700             07  DI-HDR-LAST-MNTE-OPER     PIC X(8).              2602105
008800             07  DI-HDR-LAST-MNTE-CHG-ID   PIC X(8).              2602105
008900         05  DI-ITM-LAST-MNTE-DATA.                               2602105
009000             07  DI-ITM-LAST-MNTE-YYMMDD   PIC 9(6) COMP-3.       2602105
009100             07  DI-ITM-LAST-MNTE-HHMMSS   PIC 9(6) COMP-3.       2602105
009200             07  DI-ITM-LAST-MNTE-OPER     PIC X(8).              2602105
009300             07  DI-ITM-LAST-MNTE-CHG-ID   PIC X(8).              2602105
009400     SKIP2                                                        260
009500     03  DI-DEFINITION-AREA                PIC X(3152).           26002105
009600     03  DI-DEFINITION-RECORD REDEFINES DI-DEFINITION-AREA.       260
009700         05  DI-FIELD-TABLE-START          PIC 9(08) COMP.        260
009800         05  DI-ENTITY-TABLE OCCURS 4.                            260
009900             07  DI-ENTITY-FLAG            PIC X(01).             260
010000             07  DI-ENTITY-APPL            PIC X(02).             260
010100             07  DI-ENTITY-TYPE            PIC X(03).             260
010200             07  DI-ENTITY-NAME            PIC X(08).             260
010300             07  DI-ENTITY-FIELD           PIC X(08).             260
010400             07  DI-ENTITY-INDEX           PIC 9(08) COMP.        260
010500             07  DI-ENTITY-ALIAS           PIC X(20).             260
010600         05  DI-FIELD-TABLE-AREA OCCURS 55.                       260
010700             07  DI-FIELD-ENTRY-TYPE       PIC X(01).             260
010800             07  DI-FIELD-FLAG             PIC X(01).             260
010900             07  DI-FIELD-1-TYPE           PIC X(03).             260
011000             07  DI-FIELD-1-NAME           PIC X(08).             260
011100             07  DI-FIELD-1-LENGTH         PIC 9(04) COMP.        260
011200             07  DI-FIELD-1-ATTR           PIC X(02).             260
011300             07  DI-FIELD-1-IND            PIC X.                 260
011400             07  DI-FIELD-1-INDEX          PIC 9(08) COMP.        260
011500             07  DI-FIELD-1-COND           PIC X.                 260
011600             07  DI-FIELD-2-TYPE           PIC X(03).             260
011700             07  DI-FIELD-2-NAME           PIC X(08).             260
011800             07  DI-FIELD-2-LENGTH         PIC 9(04) COMP.        260
011900             07  DI-FIELD-2-ATTR           PIC X(02).             260
012000             07  DI-FIELD-2-IND            PIC X.                 260
012100             07  DI-FIELD-2-INDEX          PIC 9(08) COMP.        260
012200             07  DI-FIELD-CONN             PIC X.                 260
012300             07  DI-FIELD-TEST-COND        PIC X.                 260
012400         05  FILLER                        PIC X(417).            2602105
