*     * 802262 12/11/20 PROYECTO UPGRADE GN172 EDVR
000010*--------------------------------------------------------------*  9913481
000020*               ** HISTORY OF REVISIONS **                     *  9913481
000030*                                                              *  9913481
000038* 12/08/97 SIOPTNS GLOBALIZATION - REMOVE VERSION &    ~~~3483 *  9913483
000039*          FORMAT                                      ~~~3483 *  9913483
000040* 05/01/97 ADDED CODES 12, 13, 38, 39                   GN3481 *  9913481
000050*--------------------------------------------------------------*  9913481
000100 01  WS-FORMAT-DATE-AREA.
000200     03  WS-FORMAT-DATE-CODES.
000300         05  U4MD                PIC XX      VALUE '01'.
000400         05  U4MDS               PIC XX      VALUE '02'.
000500         05  U4MY                PIC XX      VALUE '03'.
000600         05  U4MYS               PIC XX      VALUE '04'.
000700         05  U6                  PIC XX      VALUE '05'.
000800         05  U6S                 PIC XX      VALUE '06'.
000900         05  U8                  PIC XX      VALUE '07'.
001000         05  U8S                 PIC XX      VALUE '08'.
001100         05  UYMD                PIC XX      VALUE '09'.
001200         05  U6-TO-8S            PIC XX      VALUE '10'.
001210         05  U8-TO-SCYMD         PIC XX      VALUE '11'.          210
001212         05  U6-TO-CYMD          PIC XX      VALUE '12'.          9913481
001214         05  U6S-TO-CYMD         PIC XX      VALUE '13'.          9913481
001300         05  S4MD                PIC XX      VALUE '20'.
001400         05  S4MDS               PIC XX      VALUE '21'.
001500         05  S4MY                PIC XX      VALUE '22'.
001600         05  S4MYS               PIC XX      VALUE '23'.
001700         05  S6                  PIC XX      VALUE '24'.
001800         05  S6S                 PIC XX      VALUE '25'.          2602420
001900         05  S8                  PIC XX      VALUE '26'.          2602420
002000         05  S8S                 PIC XX      VALUE '27'.          2602420
002100         05  S6-TO-YMD           PIC XX      VALUE '28'.          2602420
002200         05  S6-TO-8S            PIC XX      VALUE '29'.          2602420
002300         05  SYMD-TO-8S          PIC XX      VALUE '30'.
002400         05  SYMDS-TO-8S         PIC XX      VALUE '31'.
002500         05  SYMD-TO-6           PIC XX      VALUE '32'.
002510         05  SCYMD-TO-8S         PIC XX      VALUE '33'.          SI020
002520         05  SCYMD-TO-10S        PIC XX      VALUE '34'.          SI020
002530         05  SCYMD-TO-4MD        PIC XX      VALUE '35'.          SI020
002540         05  SCYMD-TO-6          PIC XX      VALUE '36'.          SI020
002550         05  SMDCY-TO-10S        PIC XX      VALUE '37'.          2500156
002552         05  CYMD-TO-6           PIC XX      VALUE '38'.          9913481
002554         05  CYMD-TO-8S          PIC XX      VALUE '39'.          9913481
002600         05  SCYMD-TO-8          PIC XX      VALUE '98'.
002700         05  DELIM               PIC XX      VALUE '99'.
