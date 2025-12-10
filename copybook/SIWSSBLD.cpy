*     * 802262 12/11/20 PROYECTO UPGRADE GN172 EDVR
000100*********************************************************
000200*** BEGINNING OF SIWSSBLD COPYBOOK                    ***
000300***                                                   ***
000400*** INTERFACE AREA FOR SIWSSBLD TO BUILD IN-CORE COPY ***
000500*** OF THE SSR FILE                                   ***
000600*********************************************************
000601*--------------------------------------------------------------** 0525104
000602*                  ** HISTORY OF REVISIONS **                  ** 0525104
000603*                                                              ** 0525104
000604* DESCRIPTION                                          CHNGID  ** 0525104
000605* ____________________________________________________ _______ ** 0525104
000696* 10/12/04 INCREASED SSR SEGMENTS FROM 25 TO 200.       GN5104 ** 0525104
000697* FROM 26 TO 201(200) AND THE TABLE ENTRIES             GN5104 ** 0525104
000698* (SIWSBL-DETAIL-RECORD) FROM 199 TO 299.               GN5104 ** 0525104
000699*--------------------------------------------------------------** 0525104
000700 01  SIWSBL-APPL-PARMS.
000800     03  SIWSBL-APPL-ID                PIC X(2).
000900     03  SIWSBL-FILE-NAME              PIC X(7).
001000     03  SIWSBL-RETURN-CODE            PIC X(1).
001001         88  SIWSBL-88-NORMAL-RET      VALUE '0'.
001002         88  SIWSBL-88-TBLEMPTY        VALUE '1'.
001003         88  SIWSBL-88-SSRMAX          VALUE '2'.
001004         88  SIWSBL-88-SEGMAX          VALUE '3'.
001005         88  SIWSBL-88-INVALID-SEG     VALUE '4'.
001006         88  SIWSBK-88-OPENERR         VALUE '5'.
001007         88  SIWSBK-88-IOERR           VALUE '9'.
001100*
001200 01  SIWSBL-TABLE-AREA.
001300     03  SIWSBL-SUMMARY-RECORD.
001301         05  SIWSBL-NO-ENTRIES         PIC 9(2) COMP-3 VALUE 0.
001500         05  SIWSBL-TABLE-LENGTH       PIC 9(2) COMP-3.
001600         05  SIWSBL-SEG-COUNT          PIC 9(2) COMP-3.
001700         05  SIWSBL-MGRP-COUNT         PIC 9(2) COMP-3.
001800         05  SIWSBL-CGRP-COUNT         PIC 9(2) COMP-3.
001900         05  SIWSBL-AGRP-COUNT         PIC 9(2) COMP-3.
002000         05  FILLER                    PIC X(18).
002100         05  SIWSBL-MID-POINT-BIN-TAB  PIC X(20).
002200         05  FILLER REDEFINES SIWSBL-MID-POINT-BIN-TAB.
002300             07  SIWSBL-MID-POINT-1    PIC 9(2) COMP.
002400             07  SIWSBL-MID-POINT-2    PIC 9(2) COMP.
002500             07  SIWSBL-MID-POINT-3    PIC 9(2) COMP.
002600             07  SIWSBL-MID-POINT-4    PIC 9(2) COMP.
002700             07  SIWSBL-MID-POINT-5    PIC 9(2) COMP.
002800             07  SIWSBL-MID-POINT-6    PIC 9(2) COMP.
002900             07  SIWSBL-MID-POINT-7    PIC 9(2) COMP.
003000             07  SIWSBL-MID-POINT-8    PIC 9(2) COMP.
003100             07  SIWSBL-MID-POINT-9    PIC 9(2) COMP.
003200             07  SIWSBL-MID-POINT-10   PIC 9(2) COMP.
003300     03  SIWSBL-DETAIL-TABLE.
003310*-------------------------------------------------------------
003320* THIS AREA IS THE IN-CORE TABLE OF SSR ENTRIES,
003330* THE LAYOUT IS THE APPL ID, CTLS, AND SEGMENT NOS. FROM THE
003340* SSR RECORD, NOT THE WHOLE SSR RECORD.
003350*-------------------------------------------------------------
003400         05  SIWSBL-DETAIL-RECORD      OCCURS 299 TIMES.          0525104
003500             07  SIWSBL-APPLICATION-ID PIC X(2).
003600             07  SIWSBL-CONTROLS.                                 2601795
003610                 09  SIWSBL-CONTROL1234 PIC X(14).                2601795
003620                 09  SIWSBL-ACCOUNT     PIC X(14).                2601795
003700             07  SIWSBL-SEG-NUMBER     PIC X(2).                  2602266
003800             07  SIWSBL-MGRP-NO        PIC X(2).                  2602266
003900             07  SIWSBL-CGRP-NO        PIC X(2).                  2602266
004000             07  SIWSBL-AGRP-NO        PIC X(2).                  2602266
004100             07  FILLER                PIC X(12).                 2601795
004101***
004102 01  SIWSBL-STACK-TABLE.
004103     03  SIWSBL-STACK-NO OCCURS 201 TIMES PIC X(2).               0525104
004200*** END OF SIWSSBLD COPYBOOK ***
