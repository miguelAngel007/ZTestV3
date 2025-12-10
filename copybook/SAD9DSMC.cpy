*     * 802262 12/11/20 PROYECTO UPGRADE GN172 EDVR
000100******************************************************************
000200*                                                                *
000300*  SAD9DSMC: AUXILIARY DICTIONARY DATA STORAGE MAP (DSM)         *
000400*                                                                *
000500* IN ORDER TO PROVIDE DOWNWARD COMPATABILITY PRIOR TO SA0004,    *
000600* AUXILIARY DICTIONARY INTERFACE MODULES WILL PROCESS REQUESTS   *
000700* SPECIFIED WITH EITHER LOGICAL KEY OR PHYSICAL KEY.  MODULES    *
000800* CODED BEFORE SA0004 USED LOGICAL KEY.  LOGICAL KEY STRUCTURE:  *
000900*                                                                *
001000*   +0(03) AUXILIARY ENTITY TYPE (LITERAL 'DSM')                 *
001100*   +3(08) AUXILIARY ENTITY NAME                                 *
001200*  +11(14) SLACK BYTES (BLANKS)                                  *
001300*                                                                *
001400* MODULES WHICH READ/WRITE AUXILIARY ENTITIES DIRECTLY FROM THE  *
001500* AUXILIARY DICTIONARY MUST USE THE AUXILIARY PHYSICAL KEY.      *
001600* PHYSICAL KEY STRUCTURE:                                        *
001700*                                                                *
001800*   +0(02) APPLICATION CODE                                      *
001900*   +2(03) REPOSITORY ENTITY TYPE                                *
002000*   +5(08) ENTITY NAME                                           *
002100*  +13(03) AUXILIARY ENTITY TYPE                                 *
002200*  +16(05) SLACK BYTES (BLANKS)                                  *
002300*  +21(02) SLACK BYTES (NULL OR LOW-VALUES)                      *
002400*  +23(01) AUXILIARY SEGMENT NUMBER                              *
002500*  +24(01) AUXILIARY SEGMENT PAGE NUMBER                         *
002600*                                                                *
002700******************************************************************
002800* AUX-OCCURS-DATA REFLECTS UP TO 3 LEVELS OF OCCURS DATA,        *
002900* INCLUDING LENGTH & NUMBER OCCURENCES FOR EACH OCCURS LEVEL.    *
003000*                                                                *
003100* FOR EXAMPLE, FOR COBOL DESCRIPTION...                          *
003200*                                                                *
003300* 01  DATA.                                                      *
003400*     05  DATA-A                                OCCURS 3.        *
003500*         10  DATA-B-1        PIC  X(4).                         *
003600*         10  DATA-B-2                          OCCURS 2.        *
003700*             15  DATA-C-1    PIC  X(1).                         *
003800*             15  DATA-C-2    PIC  9(3) COMP-3  OCCURS 4.        *
003900*     05  DATA-D              PIC  X(5).                         *
004000*                                                                *
004100* THE FOLLOWING OCCURS DATA WOULD APPLY...                       *
004200*                                                                *
004300*  LV COBNAME DISP LNTH OCCURS <OCRS LVL1> <OCRS LVL2> <OCRS LVL3>
004400*                       LEVELS  LNTH OCRS   LNTH OCRS   LNTH OCRS*
004500*  01 DATA      00   00    0                                     *
004600*  05 DATA-A    00   00    1     22    3                         *
004700*  10 DATA-B1   00   04    1     22    3                         *
004800*  10 DATA-B2   04   00    2     22    3      9    2             *
004900*  15 DATA-C1   04   01    2     22    3      9    2             *
005000*  15 DATA-C2   05   02    3     22    3      9    2      2    4 *
005100*  05 DATA-D    66   05    0                                     *
005200*                                                                *
005300*  THIS MAP MAY BE USED IN CONJUNCTION WITH SPECIFIED SUBSCRIPTS *
005400*  TO LOCATE GIVEN DATA.  NOTE THESE EXAMPLE CALCULATIONS:       *
005500*                                                                *
005600* DATA-A  (3    )  =  0 + (3-1)(22)                       =  44  *
005700* DATA-B1 (1 2  )  =  0 + (1-1)(22) + (2-1)(9)            =   9  *
005800* DATA-B2 (3 1  )  =  4 + (3-1)(22) + (1-1)(9)            =  48  *
005900* DATA-C1 (2 3  )  =  4 + (2-1)(22) + (3-1)(9)            =  44  *
006000* DATA-C2 (1 2 3)  =  5 + (1-1)(22) + (2-1)(9) + (3-1)(2) =  18  *
006100*                                                                *
006200******************************************************************
006300*        HISTORY OF REVISIONS                                    *
006400*                                                                *
006500* RELEASE# RSEC#  DESCRIPTION OF MODIFICATION                    *
006600* -------- ------ -----------------------------------------------*
006693* 10/18/16 GN6914 ADDED GROUP FIELDS TO THE AUX TABLE    GN6914  *1726914
006694* 07/11/06 GN5441 ADD SENSITIVE DATA CTR/SENS DATA       GN5441  *0715441
006696* 09/25/97 INCR AUX-DSM-ENT-LENGTH FROM HWORD TO FWORD  ~~~3569  *9913569
006698* 05/01/97 TRANSLATION ENABLE                           ~~~3481  *9913569
006700* SA0004   SA0290 REMOVE TBL LEN, INCLUDE KEY & DNM POINTER      *
006800*                                                                *
006900******************************************************************
007000
007100 01  AUXILIARY-TABLE-AREA.
007200     03  AUX-TABLE-HEADER.
007300         05  AUX-DSM-PHYS-KEY.
007400             10  AUX-DSM-PHYS-APPL   PIC  X(02).
007500             10  AUX-DSM-PHYS-REPTYP PIC  X(03).
007600             10  AUX-DSM-PHYS-NAME   PIC  X(08).
007700             10  AUX-DSM-PHYS-AUXTYP PIC  X(08).
007800             10  AUX-DSM-PHYS-INDEX.
007900                 15  FILLER          PIC  X(01).
008000                 15  AUX-DSM-PHYS-SEGNO PIC  X(01).
008100                 15  FILLER          PIC  X(01).
008200                 15  AUX-DSM-PHYS-PAGNO PIC  X(01).
008300         05  AUX-TABLE-KEY REDEFINES AUX-DSM-PHYS-KEY.
008400             10  AUX-TABLE-TYPE      PIC  X(03).
008500             10  AUX-TABLE-NAME      PIC  X(22).
008600         05  AUX-TABLE-AUDIT-DATA.
008700             10  AUX-LAST-MNTE-DATE  PIC S9(6)  COMP-3.
008800             10  AUX-LAST-MNTE-TIME  PIC S9(6)  COMP-3.
008900             10  AUX-LAST-MNTE-OPER  PIC  X(8).
009000             10  AUX-LAST-MNTE-CHGID PIC  X(8).
009100             10  AUX-DSM-ENT-LENGTH  PIC  9(5)  COMP.             9913569
009200             10  AUX-DSM-FILE-LEVEL  PIC  X(1).
009300             10  AUX-DSM-REC-LEVEL   PIC  X(1).
009400             10  FILLER              PIC  X(2).                   9913569
009500         05  AUX-NUMBER-FIELDS       PIC  9(04) COMP.
009600         05  AUX-DATAGROUP-LENGTH    PIC  X(02).
009700         05  AUX-DATAGROUP-FORMAT    PIC  X(01).
009800             88  AUX-DATAGROUP-FORMAT-FIXED      VALUE 'F'.
009900             88  AUX-DATAGROUP-FORMAT-VARIABLE   VALUE 'V'.
010000         05  AUX-DATAGROUP-TYPECODE  PIC  X(01).
010100             88  AUX-DATAGROUP-TYPECODE-DATABAS  VALUE 'D'.
010200             88  AUX-DATAGROUP-TYPECODE-PARMS    VALUE 'P'.
010300             88  AUX-DATAGROUP-TYPECODE-WORK     VALUE 'W'.
010400         05  AUX-DATAGROUP-DATABASE  PIC  X(08).
010500         05  AUX-THRESHHOLD-LIMIT    PIC  9(04) COMP.
010600         05  AUX-ALLOW-CRITERIA-EDIT PIC  X(01).
010700         05  AUX-KEY-ITEM-NUMBER     PIC  9(04) COMP.
010800         05  AUX-DNM-NUMBER-SEGMENTS PIC  X(01).                  0715441
010900         05  AUX-DNM-TOTAL-LENGTH    PIC  9(08) COMP.
010910         05  AUX-SENS-CTR            PIC  9(04) COMP.             0715441
011000         05  FILLER                  PIC  X(13).                  0715441
011100         05  AUX-BINSRCH-START       PIC  9(4)  COMP.
011200         05  AUX-BINSRCH-START-X
011300             REDEFINES AUX-BINSRCH-START PIC X(2).
011400     03  AUX-TABLE-ENTRY             OCCURS 10000 TIMES.          1726914
011410         05  AUX-BASE-ENTRY.                                      1726914
011500             10  AUX-BINSRCH-LOWER   PIC  9(4)  COMP.             1726914
011600             10  AUX-BINSRCH-LOWER-X                              1726914
011700                   REDEFINES AUX-BINSRCH-LOWER                    1726914
011710                                     PIC  X(2).                   1726914
011800             10  AUX-BINSRCH-HIGHER  PIC  9(4)  COMP.             1726914
011900             10  AUX-BINSRCH-HIGHER-X                             1726914
012000                   REDEFINES AUX-BINSRCH-HIGHER                   1726914
012010                                     PIC  X(2).                   1726914
012100             10  AUX-ELEMENT-ID      PIC  X(08).                  1726914
012200             10  AUX-LEVEL-NUMBER    PIC  X(01).                  1726914
012300             10  AUX-STORAGE-TYPE    PIC  X(01).                  1726914
012400                 88  AUX-STG-TYPE-VALUES   VALUES  'A' 'B' 'C' 'N'1726914
012410                                                   'P' 'I'.       1726914
012500                 88  AUX-STG-TYPE-ALFA     VALUE   'A'.           1726914
012600                 88  AUX-STG-TYPE-BINARY   VALUE   'B'.           1726914
012700                 88  AUX-STG-TYPE-ALFANUM  VALUE   'C'.           1726914
012710                 88  AUX-STG-TYPE-IDEO     VALUE   'I'.           1726914
012800                 88  AUX-STG-TYPE-NUMERIC  VALUE   'N'.           1726914
012900                 88  AUX-STG-TYPE-PACKED   VALUE   'P'.           1726914
013000             10  AUX-EDIT-FLAG       PIC  X(01).                  1726914
013100             10  AUX-FIELD-LENGTH    PIC  9(04) COMP.             1726914
013200             10  AUX-FIELD-DISP      PIC  X(02).                  1726914
013300             10  AUX-FIELD-FORMAT    PIC  X(01).                  1726914
013400             10  AUX-NUMBER-DECIMALS PIC  X(01).                  1726914
013500             10  AUX-SECURITY-INQ    PIC  X(01).                  1726914
013600             10  AUX-SECURITY-UPD    PIC  X(01).                  1726914
013700             10  AUX-NUMBER-OCCURS   PIC  X(01).                  1726914
013800             10  AUX-DNM-POINTER     PIC  9(04)  COMP.            1726914
013900             10  AUX-DNM-POINTER-X REDEFINES                      1726914
014000                 AUX-DNM-POINTER     PIC  X(02).                  1726914
014100         05  AUX-OCCURS.                                          1726914
014110             10  AUX-OCCURS-DATA     OCCURS 3 TIMES.              1726914
014200                 15  AUX-GROUP-DISP  PIC  X(02).                  1726914
014300                 15  AUX-MAX-OCCURS  PIC  9(04) COMP.             1726914