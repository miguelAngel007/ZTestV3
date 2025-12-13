*     * 802262 12/11/20 PROYECTO UPGRADE GN172 EDVR
000100***************************************************************** 9913569
000200*                                                               * 9913569
000300*    SADSMCXB COMMUNICATION AREA.                               * 9913569
000400*                                                               * 9913569
000500*    DATA FORMAT VARIES, BASED ON COMMAND:                      * 9913569
000600*                                                               * 9913569
000700*      EXPAND/COMPRESS                                          * 9913569
000800*        BATCH - RETURNS DATA THRU SEG-AD (STRING OF BIN SEARCH * 9913569
000900*          DISPLACEMENTS, IN SEQUENCE RELATIVE TO DSM SEGMENTS.)* 9913569
001000*        ONLINE- RETURNS DATA THRU RTN-CODE (PROGRAM RTN CODE)  * 9913569
001100*                                                               * 9913569
001200*      SEARCH - DATA PERTAINING TO ONE SEGMENT                  * 9913569
001300*        MOVE MODE - RETURNS DATA THRU RESULT (SEG SEARCH CHAIN)* 9913569
001400*                                                               * 9913569
001500*       -IF INDEX IS ZERO AND ELEMENT-ID IS NOT LOW-VALUES,     * 9913569
001600*        SADXMCXB WILL RETURN DATA FOR SPECIFIED ELEMENT-ID.    * 9913569
001700*                                                               * 9913569
001800*       -IF INDEX IS ZERO AND ELEMENT-ID IS LOW-VALUES,         * 9913569
001900*        SADXMCXB WILL RETURN DATA FOR 1ST SEGMENT IN DSM       * 9913569
002000*        HAVING VALID ELEMENT-ID.                               * 9913569
002100*                                                               * 9913569
002200*       -IF INDEX IS NOT ZERO,                                  * 9913569
002300*        SADXMCXB WILL RETURN DATA FOR NEXT SEQUENTIAL SEGMENT  * 9913569
002400*        HAVING VALID ELEMENT-ID.                               * 9913569
002500*                                                               * 9913569
002600*       -INDEX OF HIGHEST SEGMENT EXAMINED WILL BE RETURNED IN  * 9913569
002700*        HIGH INDEX FIELD.  SEARCH WILL FAIL WHEN HINDEX EQUALS * 9913569
002800*        HIGHEST INDEX IN DSM & SEGMENT HAS NO VALID ELEMENT ID.* 9913569
002900*                                                               * 9913569
003000*       -NUMBER OF NODES/ELEMENT-ID WILL BE RETURNED IN NODES.  * 9913569
003100*        THIS TELLS YOU HOW MANY COMPARES WERE MADE TO FIND     * 9913569
003200*        THE SEGMENT.                                           * 9913569
003300*                                                               * 9913569
003400*       -RETURN CODE 00 INDICATES SEGMENT WAS FOUND.            * 9913569
003500*        RETURN CODE 08 INDICATES SEGMENT WAS NOT FOUND.        * 9913569
003600*                                                               * 9913569
003700***************************************************************** 9913569
003710*                    HISTORY OF REVISIONS                       * 9913569
003720* DESCRIPTION                                           CHNGID  * 9913569
003730* ----------------------------------------------------  ------- * 9913569
003798* 11/21/97 BIG DAG SUPPORT                              ~~~3569 * 9913569
003800***************************************************************** 9913569
003900 01  SADSM-DATA.
004000     05  SADSM-DATA-HDR.
004100         10  SADSM-COMMAND                 PIC X(6).
004200             88  SADSM-COMMAND-COMPRS        VALUE 'COMPRS'.
004300             88  SADSM-COMMAND-EXPAND        VALUE 'EXPAND'.
004400             88  SADSM-COMMAND-LOCATE        VALUE 'LOCATE'.
004500             88  SADSM-COMMAND-SEARCH        VALUE 'SEARCH'.
004600         10  SADSM-RTN-PROG-NAME           PIC X(8).
004700         10  SADSM-RTN-CODE                PIC 9(4) COMP.
004800             88  SADSM-RTN-OK                VALUE 00.
004900             88  SADSM-RTN-ELE-NF            VALUE 08.
005000             88  SADSM-RTN-COMPEX-ERR        VALUE 12.
005100     05  SADSM-DATA-VAR                    PIC X(20000).          9913569
005200     05  SADSM-DATA-SEARCH REDEFINES SADSM-DATA-VAR.
005300         10  SADSM-INDEX                   PIC 9(5) COMP.
005400         10  SADSM-HINDEX                  PIC 9(5) COMP.
005500         10  SADSM-NODES                   PIC 9(2) COMP.
005600         10  SADSM-SEGMENT.
005700             15  SADSM-LOWER               PIC X(2).
005800             15  SADSM-LOWER-9 REDEFINES
005900                                 SADSM-LOWER   PIC 9(4) COMP.
006000             15  SADSM-HIGHER              PIC X(2).
006100             15  SADSM-HIGHER-9 REDEFINES
006200                                 SADSM-HIGHER   PIC 9(4) COMP.
006300             15  SADSM-ELEMENT-ID          PIC X(8).
006400             15  SADSM-LEVEL-NUMBER        PIC X(1).
006500             15  SADSM-STORAGE-TYPE        PIC X(1).
006600             15  SADSM-EDIT-FLAG           PIC X(1).
006700             15  SADSM-FIELD-LENGTH        PIC 9(4) COMP.
006800             15  SADSM-FIELD-DISP          PIC X(2).
006900             15  SADSM-FIELD-FORMAT        PIC X(1).
007000             15  SADSM-NUMBER-DECIMALS     PIC X(1).
007100             15  SADSM-SECURITY-INQ        PIC X(1).
007200             15  SADSM-SECURITY-UPD        PIC X(1).
007300             15  SADSM-NUMBER-OCCURS       PIC X(1).
007400             15  FILLER                    PIC X(2).
007500             15  SADSM-OCCURS-DATA         PIC X(4)  OCCURS 3.
007600         10  SADSM-RESULT OCCURS 100.                             9913569
007700             15  SADSM-NODE-ID             PIC X(8).
007800             15  SADSM-NODE-DISP           PIC 9(4) COMP.
007900             15  SADSM-NODE-DISP-X REDEFINES
008000                   SADSM-NODE-DISP           PIC X(2).
008100         10  FILLER                        PIC X(18952).          9913569
008200     05  SADSM-DATA-COMPEX REDEFINES SADSM-DATA-VAR.
008300         10  SADSM-SEG-AD                  PIC X(2) OCCURS 10000. 9913569
