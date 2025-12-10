*     * FO5238*12/05/11 JCTE PROYECTO UPGRADE SYSTEMAT
000100*--------------------------------------------------------------*
000200*    EXTERNAL INVESTMENT WORK FIELDS FOR IM31                  *
000300*--------------------------------------------------------------*
000400 01  WXI-EXTERNAL-INVEST-WORK.
000500     05  WXI-SAME-DAY                PIC X   VALUE '0'.
000600     05  WXI-NEXT-DAY                PIC X   VALUE '0'.
000700     05  WXI-PROD-ALLOW              PIC X   VALUE '0'.
000800     05  WXI-EXCLUDE-BAL             PIC X   VALUE '0'.
000900     05  WXI-DR-CR                   PIC X   VALUE ' '.
001000     05  WXI-LINK-IND-IN             PIC X   VALUE ' '.
001100     05  WXI-LINK-IND-OUT            PIC X   VALUE ' '.
001200     05  WXI-XINV-BAL-IN     COMP-3  PIC S9(13)V99   VALUE +0.
001300     05  WXI-XINV-BALANCE    COMP-3  PIC S9(13)V99   VALUE +0.
001400     05  WXI-FILE-MAINT.
001500         10  WXI-FM-NEW              PIC X(35)       VALUE SPACES.
001600         10  FILLER REDEFINES WXI-FM-NEW.
001700             15  WXI-FM-NEW-PROD     PIC X(03).
001800             15  FILLER              PIC X(32).
001900         10  FILLER REDEFINES WXI-FM-NEW.
002000             15  WXI-FM-NEW-FUND     PIC X(04).
002100             15  FILLER              PIC X(31).
002200         10  WXI-FM-FIELD            PIC X(04)       VALUE SPACES.
002300         10  WXI-FM-MAINT            PIC X(11)       VALUE SPACES.
002400         10  WXI-FM-REQ-BY           PIC X(03)       VALUE SPACES.
002500         10  WXI-FM-DESC             PIC X(17)       VALUE SPACES.
002600         10  WXI-FM-OLD              PIC X(35)       VALUE SPACES.
002602         10  WXI-FM-IM2E-TYPE        PIC X           VALUE '0'.   0817665
002700     05  WXI-EX-INFO.
002800         10  WXI-RECNO               PIC XX          VALUE SPACES.
002900         10  WXI-EXCODE.
003000             15  WXI-EXCODE-1        PIC XX          VALUE SPACES.
003100             15  WXI-EXCODE-2        PIC XX          VALUE SPACES.
003200         10  FILLER                  PIC X(12)       VALUE SPACES.0326947
003300         10  WXI-REJPROG             PIC XX          VALUE SPACES.
003400         10  WXI-REJRESN             PIC XXX         VALUE SPACES.0717565
003500*
