*ASM XOPTS(NOEPILOG)                                                    00001000
*     * 802262 12/11/20 PROYECTO UPGRADE GN172 EDVR
*---------------------------------------------------------------------* 00002000
*                   ** PROGRAM DESCRIPTION **                         * 00003000
*                                                                     * 00004000
*                                                                    *  00009000
* SADSMCXB/SADSMCXC                                                  *  00010000
*                                                                    *  00011000
* CALLING PROGRAM WILL PASS AN ADDRESS LIST AS FOLLOWS:              *  00012000
*   +0(4)      4K COMMUNICATION AREA    (MAPPED BY SADSMCMA)  9913569*  00013000
*   +4(4)> >= 64K BUFFER AREA FOR DSM   (MAPPED BY SAD8DSMA)  9913569*  00014000
*                                                                    *  00015000
*   NOTE: MINIMUM LENGTH COMMUNICATION AREA REQUIRED FOR ONLINE      *  00016000
*   INDICATED BY SADSMMLL FOR LOCATE MODE AND BY SADSMMLM FOR MOVE   *  00017000
*   MODE. (SEE SADSMCMA)                                             *  00018000
*                                                                    *  00019000
* COMMAREA FUNCTION CODES:  (SEE SADSMCMA/SADSMCOM)                  *  00020000
*   1) EXPAND - EXPAND COMPRESSED DSM TO FIXED FORMAT                *  00021000
*   2) COMPRS - COMPRESS DSM AND BUILD BINARY SEARCH TREE            *  00022000
*   3) LOCATE - LOCATE GIVEN ELEMENT ID VIA BINARY TREE/LOCATE MODE  *  00023000
*   4) SEARCH - LOCATE GIVEN ELEMENT ID VIA BINARY TREE/MOVE MODE    *  00024000
*                                                                    *  00025000
* COMMAREA RETURN CODE SET AS FOLLOWS:  (SEE SADSMCMA/SADSMPRC)      *  00026000
*   00 - FUNCTION SUCCESSFUL                                         *  00027000
*   08 - SEARCH UNSUCCESSFUL                                         *  00028000
*   12 - COMPEX UNSUCCESSFUL (COMPRESS/EXPAND)                       *  00029000
*                                                                    *  00030000
* BUFFER AREA CONTAINS DSM WITH FIXED LENGTH HDR & VAR LENGTH SEGMTS.*  00031000
*                                                                    *  00032000
* IN COMPRESS FUNCTION, THE BINARY SEARCH TREE IS BUILT.      9913569*  00032200
* RELATIVE DISPLACEMENT FROM BINARY SEARCH BASE ADDRESS IS    9913569*  00032400
* CALCULATED FOR EACH ITEM ITERATION AND RETURNED TO BATCH    9913569*  00032600
* CALLER.  RELATIVE DISPLACEMENTS FROM BINARY SEARCH BASE     9913569*  00032800
* ADDRESS ARE SET IN EACH ITEM ITERATION FOR LOWER AND HIGHER 9913569*  00033000
* ITEMS.  COMPARISON OF ELEMENT-IDS ARE USED FOR COMPARES.    9913569*  00033200
* END-OF-TREE IS INDICATED BY NULL (HEX ZEROS) LOWER/HIGHER   9913569*  00033400
* POINTERS.                                                   9913569*  00033600
*                                                             9913569*  00033800
* THE BINARY SEARCH TREE FOR 'BIG DAGS,' WHERE THERE ARE MORE 9913569*  00034000
* THAN 1721 ITEMS IN THE DSM WILL BE BUILT IN SEGMENTS OF     9913569*  00034200
* 1721 ITEMS.  FOR THE 1ST SEGMENT, THE BINARY SEARCH BASE    9913569*  00034400
* ADDRESS IS 2 BYTES PRIOR TO 1ST-ITEM DATA, AND THE BINARY   9913569*  00034600
* SEARCH STARTER ITEM OFFSET WILL BE STORED AT THE BINARY     9913569*  00034800
* SEARCH BASE ADDRESS.  THUS, 1ST-ITEM OFFSET IF 2.  (THIS    9913569*  00035000
* WAS THE DSM BINARY SEARCH ORIGINAL DESIGN & IMPLEMENTATION.)9913569*  00035200
*                                                             9913569*  00035400
* FOR SEGMENTS 2-N (NEW WITH GN3569), THE BIN SEARCH STARTING 9913569*  00035600
* ITEM OFFSET IS STORED WITH THE 1ST AVAILABLE ITEM THAT IS   9913569*  00035800
* END-OF-TREE ON BOTH LOWER AND HIGHER POINTERS--WHERE BOTH   9913569*  00036000
* LOWER AND HIGHER POINTERS ARE NULL (HEX ZEROS).  WHERE      9913569*  00036200
* BINARY SEARCH STARTING ITEM OFFSET IS STORED IN SEGS 2-N,   9913569*  00036400
* THE LOWER POINTER (1ST HALFWORD) IS OVERLAID WITH X'FFF0'   9913569*  00036600
* AND THE HIGHER POINTER (2ND HALFWORD) IS OVERLAID WITH THE  9913569*  00036800
* BINARY SEARCH STARTING ITEM OFFSET.                         9913569*  00037000
*                                                             9913569*  00037200
* FOR BIG DAGS (>1721 ITEMS), OFFSET FROM CUR SEGMENT ITEM 1  9913569*  00037400
* TO NEXT SEGMENT ITEM 1 WILL BE STORED IN 1ST ITEM OF CUR SEG9913569*  00037600
* WHERE BOTH LOWER AND HIGHER POINTERS ARE NULL (HEX ZEROS)-- 9913569*  00037800
* THAT IS, END-OF-TREE ON BOTH LOWER AND HIGHER POINTERS.     9913569*  00038000
* WHERE NEXT SEGMENT ITEM1 OFFSET IS STORED, THE LOWER POINTER9913569*  00038200
* (1ST HALFWORD) IS OVERLAID WITH X'FFF1' AND THE HIGHER      9913569*  00038400
* POINTER (2ND HALFWORD) IS OVERLAID WITH THE NEXT SEGMENT    9913569*  00038600
* ITEM 1 OFFSET FROM CURRENT SEGMENT ITEM 1.                  9913569*  00038800
*                                                             9913569*  00039000
* NOTE THAT BINARY SEARCH STARTER ITEM DATA IS LOADED BEFORE  9913569*  00039200
* NEXT SEGMENT ITEM 1 OFFSET, SO IF BOTH ARE LOADED IN A GIVEN9913569*  00039400
* SEGMENT, THE BINARY SEARCH STARTER ITEM WILL BE FOUND FIRST.9913569*  00039600
*                                                             9913569*  00039800
* SO, AT BINARY SEARCH TIME SADSMGET WILL DO A BINARY SEARCH  9913569*  00040000
* ON ONE 1721-ITEM SEGMENT AT A TIME.  SADSMGET WILL CONSIDER 9913569*  00040200
* ALL ITEMS TO BE END-OF-TREE, LOWER AND HIGHER, WHERE THE    9913569*  00040400
* LOWER POINTER STARTS WITH X'FFF*'.                          9913569*  00040600
*                                                                    *  00041000
* FOR BOTH COMPRESS AND EXPAND, SOURCE AND RESULT DSM WILL BE        *  00042000
* CONTAINED IN BUFFER PROVIDED BY CALLING PROGRAM.  COMPRESS  9913569*  00043000
* FUNCTION UPDATES IN-PLACE.  EXPAND FUNCTION USES AN INTERMEDIATE   *  00044000
* WORKAREA.                                                          *  00045000
*                                                                    *  00046000
* FOR SEARCH FUNCTION, SOURCE DSM (COMPRESSED) IS CONTAINED IN9913569*  00047000
* BUFFER PROVIDED BY CALLING PROGRAM.                                *  00048000
*                                                                     * 00048020
*---------------------------------------------------------------------* 00048025
*                  ** HISTORY OF REVISIONS **                         * 00048030
*                                                                     * 00048035
* DESCRIPTION                                                 CHNGID  * 00048040
* __________________________________________________________  _______ * 00048045
*                                                                     * 00048050
* 06/21/11 ADDED SIRENT FOR THREADSAFE COMPLIANCE             1216075   00050095
* 03/14/00 31-BIT                                             2024183 * 00050096
* 06/18/97 MODIFY TO COBOL/VSE                                9913437 * 00050098
*                                                                     * 00050100
*---------------------------------------------------------------------* 00050200
         EJECT                                                9913437   00050300
         GBLB  &CICS                                          9913437   00050400
         GBLC  &ENV                                           9913437   00050500
         GBLC  &TBLSIZE                                       9913437   00050600
         COPY  SIOPTNS                                        9913437   00050700
&TBLSIZE SETC  '6400'                                         9913437   00050800
         EJECT                                                9913437   00050900
         AIF   ('&SYSPARM' NE 'CICS').GEN1                              00051000
         SIRENT                        ;                      1216075   00051005
&ENV     SETC  'C'                                                      00052000
&CICS    SETB  1                                                        00053000
         TITLE 'SADSMCXC-DICTIONARY DSM COMPRESS/EXPAND ROUTINE-ONLINE' 00059000
SADSMCXC DFHEIENT CODEREG=(12),DATAREG=(13),EIBREG=(11)                 00060000
SADSMCXC AMODE 31                      / SET ADDRESS MODE     0400474 / 00064000
SADSMCXC RMODE ANY                     / SET RESIDENCE MODE   0400474 / 00065000
         SIEQREG                                                        00067000
         L     R1,DFHEICAP         LOAD ADDR OF COMMON AREA             00068000
         B     *+24                                                     00069000
         DC    CL20'SADSMCXB(SADSMCXC)'                                 00070000
         AGO   .GEN2                                                    00071000
.GEN1    ANOP                                                           00072000
&ENV     SETC  'B'                                                      00073000
         TITLE 'SADSMCXB-DICTIONARY DSM COMPRESS/EXPAND ROUTINE-BATCH'  00074000
SADSMCXB START                                                          00075000
         SIBASE BASEREG=12,RWNUM=0981,RWSUB=000  ,            9913569   00076000
.GEN2    ANOP                                                           00077000
         EJECT                                                          00078000
*---------------------------------------------------------------------* 00079000
* CLEAR SERIALLY REUSABLE STORAGE                                     * 00080000
* LOAD DICTIONARY WORKAREA & DSM BASE REGS                            * 00081000
*---------------------------------------------------------------------* 00082000
LOADPARM DS    0H                                                       00083000
         LA    R2,REENTBEG             R2 = A(SERIALLY REUSABLE STG)    00084000
         LR    R4,R2                   R4 = A(SERIALLY REUSABLE STG)    00085000
         L     R3,=A(REENTEND-REENTBEG)  R3 = L'SER REUSABLE STG        00086000
         LA    R5,0                    R5 = PAD CHARACTER & 0 LENGTH    00087000
         MVCL  R2,R4                   CLEAR SERIALLY REUSABLE STG      00088000
         LM    R9,R10,0(R1)            R9-10 = PARMS(WORKAREA, BUFFER)  00089000
         STM   R9,R10,PARMINAD         SAVE  = PARMS(WORKAREA, BUFFER)  00090000
         USING SADSMCMA,R9             R9  = A(SADSM WORKAREA)          00091000
         USING SADDS,R10               R10 = A(BUFFER AREA/HDR)         00092000
         MVC   SADSMPNM,=CL8'SADSMCX&ENV' LOAD PROG NAME IN RTN AREA    00093000
         MVC   SADSMPRC,=HL2'0'        CLEAR RETURN CODE TO ZERO        00094000
         SPACE 3                                                        00095000
*---------------------------------------------------------------------* 00096000
*                                                                     * 00097000
* COMPRESS ROUTINE.                                                   * 00098000
* TRUNCATE EACH DSM SEGMENT TO FIXED PORTION +(#OCCURS * VARIABLE LEN)* 00099000
*                                                                     * 00100000
* (R8 IS USED TO LOAD SEGMENT DISPLACEMENT LIST IN BATCH ONLY)        * 00101000
*---------------------------------------------------------------------* 00102000
COMPRESS DS    0H                                                       00103000
         LA    R8,SADSMSDI             R8 = A(SEGMENT SEGMENT CHAIN)    00104000
         CLC   SADSMCOM,=CL6'COMPRS'   IS THIS COMPRESS FUNCTION        00105000
         BNE   EXPAND                    N. GO TO EXPAND ROUTINE        00106000
         LA    R2,SADDSSLN             R2 = DSM HEADER SEGMENT LENGTH   00107000
         ST    R2,DSMLEN               SAVE DSM HEADER SEGMENT LENGTH   00108000
         LA    R2,0                    CLEAR R2                         00109000
         ICM   R2,B'0011',SADDSNFL     R2 = TOTAL # SEGMENTS            00110000
         C     R2,=A(SADDSDLN/SADDSELX) DO WE HAVE BIG-DAG DSM9913569   00110100
         BL    COM050                    N. COMPRS/BTREE ONCE 9913569   00110200
         LR    R1,R2                   R1 = TOTAL # ITEMS     9913569   00110300
         L     R2,=A(SADDSDLN/SADDSELX) R2 = MAXITMS/SEG/DSM  9913569   00110400
         SR    R1,R2                   R1 = #ITEMS REMAIN TODO9913569   00110500
         ST    R1,#ITEMREM             SAVE # ITEMS REMAINING 9913569   00110600
COM050   DS   0H                                              9913569   00110700
         ST    R2,SORT#ENT             SAVE # ENTRIES IN TABLE          00111000
         MVC   SORTELEN,=H'10'         SET TABLE ENTRY LENGTH FOR SORT  00112000
         MVC   SORTKLEN,=H'8'          SET TABLE ENTRY KEYLEN FOR SORT  00113000
         LA    R3,SADDSSLN(,R10)       R3 = A(DSM SEGMENT)              00114000
         LA    R4,SEGTABLC             R4 = A(SEGMENT TABLE)            00115000
         LR    R7,R3                   R7 = A(SEGMENT SOURCE AREA)      00116000
         USING SADDSENT,R7             R7 = A(BUFFER AREA/VAR SEG)      00117000
         LA    R15,SADDSBST            R15 =A(BASE/SEG OFFSET)9913569   00117100
         ST    R15,@BTREBAS            SAVE A(BASE/SEG OFFSET)9913569   00117200
COM100   DS    0H                                                       00118000
         MVC   0(L'SEGNAME,R4),SADDSEID LOAD SEG TABLE W/ELEMENT NAME   00119000
         LR    R14,R3                  R14 = A(CURRENT SEGMENT)         00120000
         L     R15,@BTREBAS            R15 =A(BASE/SEG OFFSET)9913569   00121000
         SR    R14,R15                 GET NEW SEGMENT OFFSET           00122000
         STH   R14,L'SEGNAME(,R4)      LOAD SEG OFFSET TO SORT TABLE    00123000
         AIF   (&CICS).COM200                                           00124000
         STH   R14,0(,R8)              SAVE SEG OFFSET FOR CALLING PGM  00125000
         LA    R8,2(,R8)               R8 = A(NEXT SEGMENT OFFSET AREA) 00126000
.COM200  ANOP                                                           00127000
COM200   DS    0H                                                       00128000
         LA    R5,0                                                     00129000
         IC    R5,SADDSNOL             R5 = # OCCURS LEVELS             00130000
         LA    R14,SADDSGLN            R14= L'OCCURS ENTRY              00131000
         SRDL  R14,32                  R14/15 = L'OCCURS ENTRY          00132000
         MR    R14,R5                  R15 = L'OCCURS ENTRIES           00133000
         LA    R5,SADDSELN(,R15)       R5  = L'CURRENT SEGMENT          00134000
         L     R14,DSMLEN              R14 = DSM LENGTH                 00135000
         AR    R14,R5                  R14 = DSM LENGTH THRU CUR SEG    00136000
         ST    R14,DSMLEN              SAVE  DSM LENGTH THRU CUR SEG    00137000
         BCTR  R5,0                    R5  = CURRENT SEGMENT LENTH CODE 00138000
         EX    R5,MVCVAR               MOVE CUR SEG TO NEW LOCATION     00139000
         LA    R3,1(R5,R3)             R3 = A(NEXT SEGMENT TARGET AREA) 00140000
         LA    R4,L'SEGTABLC(,R4)      R4 = A(CUR SEGTABLC ENTRY)       00141000
         LA    R7,SADDSELX(,R7)        R7 = A(NEXT SEGMENT SOURCE AREA) 00142000
         BCT   R2,COM100               MOVE ALL SEGMENTS                00143000
         ST    R3,@DSMITMT             SV A(NXT DSMITEM/TARG) 9913569   00144000
         ST    R7,@DSMITMS             SV A(NXT DSMITEM/SOUR) 9913569   00144100
         MVC   SORTNAME,=CL8'SAASORT&ENV'  SET SORT PROGRAM NAME        00145000
         LA    R1,SORTNAME             GET  SORT PROGRAM NAME ADDRESS   00146000
         ST    R1,SORTNMAD             SAVE SORT PROGRAM NAME ADDRESS   00147000
         LA    R1,SORTGCA              R1 = A(SORT RETURN CODE AREA)    00148000
         LA    R2,SORTPARM             R2 = A(SORT PARAMETERS)          00149000
         LA    R3,SEGTABLC             R3 = A(SORT ENTRIES)             00150000
         STM   R1,R3,PARMLIST          SAVE SORT'S PARMLIST             00151000
         OI    PARMLIST+8,X'80'        FLAG LAST PARM                   00152000
         SPACE                                                          00153000
*---------------------------------------------------------------------* 00154000
* GO SORT BINARY TREE SEGMENT TABLE                                   * 00155000
*---------------------------------------------------------------------* 00156000
GOSORT   DS    0H                                                       00157000
         AIF   (&CICS).B002                                             00158000
         LA    R1,SILKPARM             R1 = A(SILINK PARMLIST)          00159000
         GNCALL SILINK                                        9913437   00160000
         AGO   .B002A                                                   00161000
.B002    ANOP                                                           00162000
         EXEC  CICS  SUSPEND                                            00163000
         EXEC  CICS  LINK PROGRAM(SORTNAME)                            X00164000
                     COMMAREA(PARMLIST) LENGTH(12)                      00165000
.B002A   ANOP                                                           00166000
SORTRTN  DS    0H                                                       00167000
         CLC   SORTRCOD,=F'0'          WAS SORT SUCCESSFUL              00168000
         BE    BINARYT                   Y. CONTINUE                    00169000
         MVC   SADSMPNM,=CL8'SAASORTB'   N. ABORT/SPECIFY SORT FAILURE  00170000
         MVC   SADSMPRC,SORTRCOD+2          LOAD SORT PROGRAM NAME      00171000
         B     EXIT                         ABORT                       00172000
         SPACE                                                          00173000
*---------------------------------------------------------------------* 00174000
* BUILD BINARY TREE OFFSETS                                           * 00175000
*---------------------------------------------------------------------* 00176000
BINARYT  DS    0H                                                       00177000
         LA    R0,0                R0 = ITEM # TREE POSITION ADJUSTMENT 00178000
         LA    R1,1                R1 = 'AND' ARGUMENT                  00179000
         ICM   R2,B'1111',SORT#ENT R2 = # DSM FIELDS (LJ)     9913569   00180000
         SRL   R2,1                R2 = #DSM FIELDS / 2       9913569   00181000
         L     R15,@BTREBAS        R14= A(BINARY TREE BASE)   9913569   00181100
         STCM  R2,B'0011',0(R15)   SV BTREE STARTER ITM# OFFST9913569   00182000
         LA    R14,L'SEGTABLC      R14= SEGMENT TABLE LENGTH            00183000
         LTR   R2,R2               DO WE HAVE ONLY 1 ITEM IN TABLE      00184000
         BZ    SETSTART              Y. GO SET START DISP & QUIT        00185000
         LA    R2,1(,R2)           R2 = BINARY SEARCH STARTER ITEM #    00186000
         LA    R3,1                R3 = TABLE ITEM NUMBER/SAVEREG       00187000
BLDTREE  DS    0H                                                       00188000
         LR    R4,R3               R4 = TABLE ITEM NUMBER/WORKREG       00189000
         SR    R4,R0               R4 = ADJUSTED ITEM NUMBER            00190000
         LA    R5,1                R5 = LEVEL ID                        00191000
         MVC   COFFSETS,=F'0'      CLEAR CHILD-OFFSET-SAVEAREA TO NULLS 00192000
GETLEVEL DS    0H                                                       00193000
         LR    R6,R4               R4 = TABLE ITEM NUMBER/ARGUMENT      00194000
         NR    R6,R1               HAVE WE FOUND LEVEL                  00195000
         BNZ   SAVHILVL              Y. GO CHECK HIGHWATER MARK SAVED   00196000
         SRL   R4,1                  N. SEARCH FOR LEVEL (POWER OF 2)   00197000
         SLL   R5,1                                                     00198000
         B     GETLEVEL                                                 00199000
SAVHILVL DS    0H                                                       00200000
         C     R5,HILVL            IS CUR LEVEL HIGHEST SO FAR          00201000
         BNH   GETCHILN              N. SKIP ITEM SAVE                  00202000
         ST    R3,HILVLITM           Y. SAVE ITEM NUMBER OF HIGHEST LVL 00203000
         ST    R5,HILVL                 SAVE CUR LVL AS HIGHWATER MARK  00204000
GETCHILN DS    0H                                                       00205000
         CR    R5,R1               DO WE HAVE A LEVEL 1 ITEM            00206000
         BNH   NEXTITEM              Y. LEAVE LEFTCHILD/RIGHTCHILD NULL 00207000
         SRL   R5,1                FIND R/L CHILD DISP FROM PARENT      00208000
         LR    R6,R3               R6 = CURRENT ITEM NUMBER             00209000
         SR    R6,R5               R6 = LEFT CHILD NUMBER               00210000
         BCTR  R6,0                R7 = LEFT CHILD OFFSET NUMBER        00211000
         SRDL  R6,32               R6/7=LEFT CHILD OFFSET NUMBER        00212000
         MR    R6,R14              R7 = A(LEFT CHILD SEGTBL OFFSET)     00213000
         LA    R6,SEGTABLC(R7)     R6 = A(LEFT CHILD SEGTBL ENTRY)      00214000
         MVC   CLOFFSET,L'SEGNAME(R6) SAVE LEFTCHILD OFFSET             00215000
         LA    R6,0(R5,R3)         R6 = RIGHT CHILD NUMBER              00216000
CKRCHILD DS    0H                                                       00217000
         CR    R6,R2               IS CALCULATED RT CHILD OUT OF BOUNDS 00218000
         BL    GETOFFST              N. SKIP RT CHILD ADJUSTMENT        00219000
         SRL   R5,1                  Y. ADJUST RIGHT CHILD DISPLACEMENT 00220000
         LTR   R5,R5               IS THERE A VALID RIGHT CHILD         00221000
         BZ    SETOFFST              N. LEAVE RIGHT CHILD NULL          00222000
         SR    R6,R5                 Y. ADJUST RT CHILD NUMBER          00223000
         B     CKRCHILD                 LOOP TO CK FOR GOOD ADJUSTMENT  00224000
GETOFFST DS    0H                                                       00225000
         BCTR  R6,0                R7 = RIGHT CHILD OFFSET NUMBER       00226000
         SRDL  R6,32               R6/7=RIGHT CHILD OFFSET NUMBER       00227000
         MR    R6,R14              R7 = A(RIGHT CHILD SEGTBL OFFSET)    00228000
         LA    R6,SEGTABLC(R7)     R6 = A(RIGHT CHILD SEGTBL ENTRY)     00229000
         MVC   CROFFSET,L'SEGNAME(R6) SAVE RIGHTCHILD OFFSET            00230000
SETOFFST DS    0H                                                       00231000
         LR    R6,R3               R6 = CURRENT ITEM NUMBER             00232000
         BCTR  R6,0                R6 = CURRENT ITEM OFFSET NUMBER      00233000
         SRDL  R6,32               R6/7=CURRENT ITEM OFFSET NUMBER      00234000
         MR    R6,R14              R7 = CURRENT ITEM # SEGTBL OFFSET    00235000
         LA    R6,SEGTABLC(R7)     R6 = A(CURRENT ITEM)                 00236000
         LA    R7,0                                                     00237000
         ICM   R7,B'0011',L'SEGNAME(R6) R7 = DSM SEGMENT OFFSET         00238000
         L     R15,@BTREBAS        R14= A(BINARY TREE BASE)   9913569   00238100
         LA    R7,0(R15,R7)        R7 =A(DSM CURRENT SEGMENT) 9913569   00239000
         MVC   0(4,R7),COFFSETS    LOAD LEFTCHILD/RIGHTCHILD OFFSETS    00240000
NEXTITEM DS    0H                                                       00241000
         LA    R3,1(,R3)           R3 = NEXT TABLE ITEM NUMBER/SAVEREG  00242000
         C     R3,SORT#ENT         HAVE WE LOADED ALLITM/SIDE29913569   00243000
         BH    BLDTRND2              Y. SKIP TO FINISH RIGHT SIDE       00244000
         CR    R3,R2               HAVE WE LOADED ALL ITEMS CUR SIDE    00245000
         BL    BLDTREE               N. LOOP TO LOAD ALL ITEMS/SIDE     00246000
         SPACE                                                          00247000
BLDTRND1 DS    0H               ** FINISH TREE LEFT SIDE                00248000
         L     R6,HILVLITM         R6 = LEFT CHILD NUMBER  (HI LVL)     00249000
         BCTR  R6,0                R6 = LEFT CHILD OFFSET# (HI LVL)     00250000
         SRDL  R6,32               R6/7=LEFT CHILD OFFSET# (HI LVL)     00251000
         MR    R6,R14              R7 = A(LEFT CHILD SEGTBL OFFSET)     00252000
         LA    R6,SEGTABLC(R7)     R6 = A(LEFT CHILD SEGTBL ENTRY)      00253000
         MVC   SLOFFSET,L'SEGNAME(R6) SAVE LEFTCHILD OFFSET (HI LVL)    00254000
         CLC   SORT#ENT,=F'2'      ARE ONLY 2 ITEMS IN TBL    9913569   00255000
         BNH   SETSTART              Y. GO SET START DISP & QUIT        00256000
         LR    R0,R2               R0 = TABLE ITEM # ADJUSTMENT         00257000
         L     R2,SORT#ENT         R2 = # DSM ITEMS BEING WKD 9913569   00259000
         LA    R2,1(,R2)           R2 = RIGHT BOUNDARY FOR ITEM LIST    00260000
         MVC   HILVL,=F'0'         CLEAR HIGH-LEVEL NUMBER              00261000
         MVC   HILVLITM,=F'0'      CLEAR HIGH-LEVEL ITEM NUMBER         00262000
         B     NEXTITEM            GO BUILD TREE SIDE 2                 00263000
         SPACE                                                          00264000
BLDTRND2 DS    0H               ** FINISH TREE RIGHT SIDE & CONNECT     00265000
         L     R6,HILVLITM         R6 = RIGHT CHILD NUMBER (HI LVL)     00266000
         LTR   R6,R6               IS THERE ONLY 1 ELEMENT/DSM          00267000
         BZ    EXIT                  Y. LEAVE CHILDREN OFFSETS NULL     00268000
         BCTR  R6,0                R6 = RIGHT CHILD OFFSET# (HI LVL)    00269000
         SRDL  R6,32               R6/7=RIGHT CHILD OFFSET# (HI LVL)    00270000
         MR    R6,R14              R7 = A(RIGHT CHILD SEGTBL OFFSET)    00271000
         LA    R6,SEGTABLC(R7)     R6 = A(RIGHT CHILD SEGTBL ENTRY)     00272000
         MVC   SROFFSET,L'SEGNAME(R6) SAVE RIGHTCHILD OFFSET (HI LVL)   00273000
SETSTART DS    0H                                                       00274000
         LA    R6,0                CLEAR R6                             00275000
         LA    R7,0                CLEAR R7                   9913569   00275100
         L     R15,@BTREBAS        R15= A(BINARY TREE BASE)   9913569   00275200
         ICM   R7,B'0011',0(R15)   R6/R7 = STARTER ITEM #OFFST9913569   00275300
         MR    R6,R14              R7 = STARTER ITEM# OFFSET  9913569   00275400
         LA    R7,SEGTABLC(R7)     R7 = A(STARTER ITEM)       9913569   00275500
         ICM   R6,B'0011',L'SEGNAME(R7)  R6 = STRTR ITM OFFST 9913569   00275600
         LA    R7,0(R15,R6)        R7 =A(DSM STARTER SEGMENT) 9913569   00275700
         MVC   0(4,R7),SOFFSETS    LD LFCHILD/RTCHILD OFFSETS 9913569   00275800
*-------------------------------------------------------------9913569-* 00275900
* WE ARE WORKING DSM SEG 1 IF @BTREBAS (R15) EQUALS THE       9913569-* 00276000
* ADDRESS OF SADDSBST.                                        9913569-* 00276100
*                                                             9913569-* 00276200
* IF WE ARE WORKING ON DSM SEG 1, THE OFFSET TO THE BINARY    9913569-* 00276300
* SEARCH STARTER ELEMENT IS STORED IN SADDSBST, WHICH IS LAST 9913569-* 00276400
* 2 BYTES PRIOR TO ELEMENT ITERATIONS.                        9913569-* 00276500
*                                                             9913569-* 00276600
* IF WE ARE WORKING ON DSM SEGS 2-N, THE OFFSET TO THE BINARY 9913569-* 00276700
* SEARCH STARTER ELEMENT IS STORED IN THE BINARY SEARCH FIELDS9913569-* 00276800
* (1ST 4 BYTES) OF THE 1ST ELEMENT WHICH IS END-OF-TREE (HEX  9913569-* 00276900
* ZEROS) ON BOTH LOW AND HIGH SIDE.  IN THIS CASE (SEGS 2-N), 9913569-* 00277000
* THE LOW POINTER (1ST 2 BYTES) WILL BE X'FFF0' AND THE HIGH  9913569-* 00277100
* POINTER (2ND 2 BYTES) WILL BE THE STARTER ELEMENT OFFSET.   9913569-* 00277200
*                                                             9913569-* 00277300
* IF AN ADDITONAL DSM SEGMENT FOLLOWS THE CURRENT SEGMENT,    9913569-* 00277400
* THE OFFSET FROM CURRENT SEGMENT, ITEM 1, TO NEXT SEGMENT,   9913569-* 00277500
* ITEM 1, WILL BE STORED IN THE BINARY SEARCH FIELDS (1ST 4   9913569-* 00277600
* BYTES) OF THE 1ST END-OF-TREE ELEMENT (AFTER THE BTREE      9913569-* 00277700
* STARTER ITEM OFFSET IS STORED).  IN THIS CASE, THE LOWER    9913569-* 00277800
* POINTER (1ST 2 BYTES) WILL BE X'FFF1' AND THE HIGH POINTER  9913569-* 00277900
* (2ND 2 BYTES) WILL BE THE NEXT SEGMENT ITEM 1 OFFSET.       9913569-* 00278000
*                                                             9913569-* 00278100
*-------------------------------------------------------------9913569-* 00278200
         LA    R14,SADDSBST        R14 = A(BTREE BASE/SEG 1)  9913569   00278300
         CR    R14,R15             ARE WE WORKING DSM SEG 1   9913569   00278400
         BE    LOADBASE              Y. GO SET STRTR ITM OFFST9913569   00278500
         MVC   0(2,R15),OVRLDATA   RESTORE DATA OVERLAID/BST  9913569   00278600
         L     R0,SORT#ENT         R0  = # ITEMS THIS SEG     9913569   00278700
         LA    R14,0               R14 = CLEARED              9913569   00278800
         LA    R15,L'SADDSBST(,R15) R15 = A(1ST ITEM/THIS SEG)9913569   00278900
FINDBASE DS   0H                                              9913569   00279000
         CLC   0(4,R15),=F'0'      IS THIS ITEM END-OF-TREE   9913569   00279100
         BE    FLAGBASE              Y. GO SET STRTR ITM OFFST9913569   00279200
         ICM   R14,B'0001',(SADDSNOL-SADDSENT)(R15) R14=#OCCUR9913569   00279300
         SLL   R14,2               R14 = L'OCCURS DATA        9913569   00279400
         LA    R15,SADDSELN(R14,R15) R15 = A(NEXT DSM ITEM)   9913569   00279500
         BCT   R0,FINDBASE         LOOP TO FIND END-OF-TREE   9913569   00279600
         B     FUNCTERR            ERR/EXIT IF CANT FIND EOT  9913569   00279700
FLAGBASE DS   0H                                              9913569   00279800
         MVC   0(2,R15),=X'FFF0'   FLAG STARTER ITEM OFFSET   9913569   00279900
         LA    R15,2(,R15)         R15 = A(BASE DATA)         9913569   00280000
LOADBASE DS   0H                                              9913569   00280100
         STCM  R6,B'0011',0(R15)   SAVE BTREE STRTER ITM OFFST9913569   00280200
*-------------------------------------------------------------9913569-* 00280300
* DETERMINE WHETHER THERE ARE ANY MORE DSM SEGMENTS TO        9913569-* 00280400
* COMPRESS.  IF SO, SET UP REGS AND DATA AREAS, THEN LOOP     9913569-* 00280500
* BACK THROUGH.  IF NOT, LOAD TOTAL DSM LENGTH AND EXIT.      9913569-* 00280600
*-------------------------------------------------------------9913569-* 00280700
         ICM   R2,B'1111',#ITEMREM R2 = DSM ITEMS TO WORK     9913569   00280800
         BZ    COMEXIT             IF NO MORE ITEMS, EXIT     9913569   00280900
         MVC   #ITEMREM,=F'0'      CLR # ITEMS REMAINING      9913569   00281000
         C     R2,=A(SADDSDLN/SADDSELX) DO WE HAVE MORE 1 SEG 9913569   00281100
         BL    COMAGAIN                  N. COMPRS/BTREE ONCE 9913569   00281200
         LR    R1,R2               R1 = TOTAL # ITEMS REMAIN  9913569   00281300
         L     R2,=A(SADDSDLN/SADDSELX) R2 = MAXITMS/SEG/DSM  9913569   00281400
         SR    R1,R2               R1 = #ITEMS REMAIN TODO NXT9913569   00281500
         ST    R1,#ITEMREM         SAVE # ITEMS REMAINING NEXT9913569   00281600
COMAGAIN DS   0H                                              9913569   00281700
         L     R0,SORT#ENT         R0  = # ITEMS THIS SEG     9913569   00281800
         LA    R14,0               R14 = CLEARED              9913569   00281900
         L     R15,@BTREBAS        R15=A(REAL/PSEUDO SADDSBST)9913569   00282000
         LA    R15,2(,R15)         R15 = A(1ST ITEM/THIS SEG) 9913569   00282100
FINDNEXT DS   0H                                              9913569   00282200
         CLC   0(4,R15),=F'0'      IS THIS ITEM END-OF-TREE   9913569   00282300
         BE    FLAGNEXT              Y. SET A(PSEUDO-SADDSBST)9913569   00282400
         ICM   R14,B'0001',(SADDSNOL-SADDSENT)(R15) R14=#OCCUR9913569   00282500
         SLL   R14,2               R14 = L'OCCURS DATA        9913569   00282600
         LA    R15,SADDSELN(R14,R15) R15 = A(NEXT DSM ITEM)   9913569   00282700
         BCT   R0,FINDNEXT         LOOP TO FIND END-OF-TREE   9913569   00282800
         B     FUNCTERR            ERR/EXIT IF CANT FIND EOT  9913569   00282900
FLAGNEXT DS   0H                                              9913569   00283000
         MVC   0(2,R15),=X'FFF1'   FLAG NEXT SEG ITEM 1 OFFSET9913569   00283100
         LA    R15,2(,R15)         R15= A(NXT SEG ITM1 OFFST) 9913569   00283200
         ST    R2,SORT#ENT         SAVE # ENTRIES IN TABLE    9913569   00283300
         L     R3,@DSMITMT         R3 = A(NXT DSMITEM/TARGET) 9913569   00283400
         LA    R4,SEGTABLC         R4 = A(SEGMENT TABLE)      9913569   00283500
         L     R7,@DSMITMS         R7 = A(NXT DSMITEM/SOURCE) 9913569   00283600
         LR    R14,R3              R14 =A(NXT DSMITEM/TARGET) 9913569   00283700
         S     R14,=F'2'           R14 =A(BASE/SEG OFFSET)    9913569   00283800
         L     R1,@BTREBAS         R1=A(CUR RL/PSEUD SADDSBST)9913569   00283900
         ST    R14,@BTREBAS        SAVE A(NEW PSEUDO SADDSBST)9913569   00284000
         MVC   OVRLDATA,0(R14)     SAVE PSEUDO-SADDSBST OVRLAY9913569   00284100
         SR    R14,R1              R14 = OFFSET: NXT SEG ITM1 9913569   00284200
         STCM  R14,B'0011',0(R15)  SAVE OFFSET: NEXT SEG ITM1 9913569   00284300
         MVC   SOFFSETS,=F'0'      CLR STARTER-ITM OFST HOLDAR9913569   00284400
         MVC   HILVL,=F'0'         CLR BLDTREE HIGHEST LVL    9913569   00284410
         MVC   HILVLITM,=F'0'      CLR BLDTREE HIGHEST LVLITM#9913569   00284420
         B     COM100              LOOP TO COMPRESS NXT SEG   9913569   00284500
COMEXIT  DS   0H                                              9913569   00284600
         MVC   SADDSESL,DSMLEN     SET NEW DSM SEGMENT LENGTH 9913569   00284700
         B     EXIT                  Y. RETURN TO CALLING PROGRAM       00285000
         SPACE                                                          00286000
MVCVAR   MVC   0(0,R3),SADDSENT        MOVE FIXED LEN TO VAR LEN SEG    00287000
         SPACE                                                          00288000
         DROP  R7                                                       00289000
         EJECT                                                          00290000
*---------------------------------------------------------------------* 00291000
* EXPAND ROUTINE                                                      * 00292000
* LOAD SEGTABLE WITH ADDRESS OF EACH COMPRESSED SEGMENT.              * 00293000
* CALCULATE EXPANDED SEGMENT OFFSET, STARTING WITH THE LAST.          * 00294000
* MOVE COMPRESSED SEGMENT TO EXPANDED SEGMENT LOCATION, PAD WITH X'OO'* 00295000
* (USE TRANSIENT AREA TO AVOID DESTRUCTIVE OVERLAP).                  * 00296000
*---------------------------------------------------------------------* 00297000
EXPAND   DS    0H                                                       00298000
         CLC   SADSMCOM,=CL6'EXPAND'   IS THIS EXPAND FUNCTION          00299000
         BNE   SEARCH                    N. GO CK FOR OTHER LOGIC       00300000
         LA    R2,0                                                     00301000
         ICM   R2,B'0011',SADDSNFL     R2 = TOTAL # SEGMENTS            00302000
         LA    R3,SADDSSLN(,R10)       R3 = A(FIRST DSM SEGMENT)        00303000
         LA    R4,SEGTABLE             R4 = A(SEGMENT TABLE)            00304000
         LA    R5,SADDSGLN             R5 = L'OCCURS LEVEL DATA         00305000
EXP100   DS    0H                                                       00306000
         ST    R3,0(,R4)               LOAD SEG TABLE W/SEGMENT ADDRESS 00307000
         LR    R14,R3                  R14 = A(CURRENT SEGMENT)         00308000
         LA    R15,SADDSBST            R14 = A(BINSRCH OFFSET BASE)     00309000
         SR    R14,R15                 R14 = CURR SEGMENT OFFSET        00310000
         AIF   (&CICS).EXP150                                           00311000
         STH   R14,0(,R8)              SAVE SEG OFFSET FOR CALLING PGM  00312000
         LA    R8,2(,R8)               R8 = A(NEXT SEGMENT OFFSET AREA) 00313000
.EXP150  ANOP                                                           00314000
         LA    R14,0                   CLEAR R14                        00315000
         LA    R15,0                   CLEAR R15                        00316000
         IC    R15,(SADDSNOL-SADDSENT)(,R3) R14/R15 = # OCCURS LEVELS   00317000
         MR    R14,R5                  R15 = L'OCCURS DATA              00318000
         LA    R15,SADDSELN(,R15)      R15 = L'TOTAL SEGMENT            00319000
         ST    R15,L'SEGADDR(,R4)      SAVE TOTAL SEGMENT LENGTH        00320000
         LA    R3,0(R15,R3)            R3  = A(NEXT SEGMENT IN DSM)     00321000
         LA    R4,L'SEGADDR+L'SEGLEN(,R4)  R4 = A(NEXT SEG ADDR SLOT)   00322000
         BCT   R2,EXP100               FIND ALL SEGMENTS                00323000
         ST    R3,0(,R4)               LOAD SEG TABLE W/DSM END BYTE+1  00324000
         LA    R0,L'SEGADDR+L'SEGLEN   R0  = L' SEGTABLE ENTRY (ADDR)   00325000
         LA    R1,0(,R4)               R1 = A(A(DSM END BYTE + 1))      00326000
         LA    R2,0                                                     00327000
         ICM   R2,B'0011',SADDSNFL     R2 = TOTAL # SEGMENTS            00328000
         LR    R3,R2                   R3 = # SEGMENTS                  00329000
         LA    R14,0                                                    00330000
         LA    R15,SADDSELX            R14/15 = L'EXPANDED SEGMENT      00331000
         MR    R14,R3                  R15 = TOTAL LENGTH OF ALL SEGS   00332000
         LA    R14,SADDSSLN(,R15)      R14 = EXPANDED DSM SEG LENGTH    00333000
         STCM  R14,B'1111',SADDSESL    SV EXPANDED DSM SEG LEN9913569   00334000
         LA    R7,SADDSELX             R7 = L'TARGET SEGMENT (EXPANDED) 00335000
         SR    R14,R7                  R14 = OFFSET TO LAST SEGMENT     00336000
         LA    R3,SADDSKEY(R14)        R3 = A(CURR EXPANDED SEG TARGET) 00337000
EXP200   DS    0H                                                       00338000
         L     R4,0(,R1)               R4 = A(SOURCE SEGMENT END + 1)   00339000
         SR    R1,R0                   R1 = A(A(SOURCE SEGMENT))        00340000
         LM    R5,R6,0(R1)             R5/R6 = ADDR & L'SOURCE SEGMENT  00341000
         BCTR  R6,0                    R6 = SOURCE SEGMENT LENGTH CODE  00342000
         CR    R3,R4                   DOES TARGET OVERLAP SOURCE       00343000
         BH    EXP250                    N. BYPASS TRANSIENT AREA USE   00344000
         MVC   TRANAREA,0(R5)            Y. USE TRANSIENT AREA          00345000
         LA    R5,TRANAREA                  RESET SOURCE ADDR TO TRAN   00346000
EXP250   DS    0H                                                       00347000
         MVI   0(R3),0                                                  00348000
         MVC   1(SADDSELX-1,R3),0(R3)  CLEAR SEGMENT TARGET AREA        00349000
         EX    R6,MVCEXP               MOVE VAR LEN TO FIXED LEN SEG    00350000
         SR    R3,R7               R3 = A(NEXT TARGET SEGMENT)          00351000
         BCT   R2,EXP200           LOOP TO MOVE ALL SEGMENTS            00352000
         B     EXIT                QUIT WHEN FINISHED                   00353000
*                                                                       00354000
MVCEXP   MVC   0(0,R3),0(R5)       MOVE VAR LEN SEG TO FIXED AREA       00355000
         EJECT                                                          00356000
*---------------------------------------------------------------------* 00357000
* SEARCH/LOCATE ROUTINE                                               * 00358000
*                                                                     * 00359000
* IF INDEX & ELEMENT ARE NULL, REQUEST IS TO MAP BINARY SEARCH PATH   * 00360000
* FOR ALL ELEMENTS.  STARTING WITH INDEX=1, SUPPLY VALID ELEMENT IDS  * 00361000
* SUCCESSIVELY ON EACH CALL AND RETURN BINARY SEARCH PATH.            * 00362000
*                                                                     * 00363000
* IF INDEX IS ZERO AND ELEMENT ID IS SUPPLIED, REQUEST IS TO RETURN   * 00364000
* SEGMENT ADDRESS ONLY (LOCATE MODE) OR SEGMENT & SEARCH CHAIN DATA   * 00365000
* (MOVE MODE).                                                        * 00366000
*                                                                     * 00367000
*---------------------------------------------------------------------* 00368000
SEARCH   DS    0H                                                       00369000
         CLC   SADSMCOM,=CL6'SEARCH' IS THIS SEARCH FUNCTION            00370000
         BE    SEAR100                 Y. SKIP TO BINARY SEARCH         00371000
         CLC   SADSMCOM,=CL6'LOCATE' IS THIS SEARCH FUNCTION            00372000
         BNE   FUNCTERR                N. BRANCH TO FUNCTION ERR RTN    00373000
SEAR100  EQU   *                                                        00374000
         OC    SADSMIND,SADSMIND   IS INDEX ZERO                        00375000
         BZ    SEAR200               Y. GO CK FOR REQUESTED ELEMENT     00376000
         ICM   R1,B'1111',SADSMHIN   N. SET TO SEARCH FOR NEXT ELEMENT  00377000
         LA    R1,1(,R1)                                                00378000
         STCM  R1,B'1111',SADSMIND SAVE INDEX OF NEXT ELEMENT           00379000
         B     SEAR300                                                  00380000
SEAR200  DS    0H                                                       00381000
         CLI   SADSMELE,C' '       IS ELEMENT ID REQ PRESENT            00382000
         BH    SEAR600               Y. CONTINUE                        00383000
         LA    R1,1                  N. DEFAULT TO SEARCH FOR 1ST ELE   00384000
         STCM  R1,B'1111',SADSMIND SAVE INDEX OF NEXT ELEMENT           00385000
SEAR300  DS    0H                                                       00386000
*      GET ELE-ID FOR NEXT SEG, LOAD TO SADSMELE & KEEP HI-INDEX        00387000
         LA    R0,0                R0 = CLEARED               9913569   00387100
         ICM   R0,B'0011',SADDSNFL R0 = TOTAL # SEGMENTS                00388000
         LA    R2,1                R2 = HIGH-INDEX                      00389000
         LA    R3,SADDSSLN(,R10)   R3 = A(FIRST DSM SEGMENT)            00390000
         LA    R5,SADDSGLN         R5 = L'OCCURS LEVEL DATA             00391000
SEAR400  DS    0H                                                       00392000
         CR    R1,R2               HAVE WE REACHED DESIRED SEGMENT      00393000
         BH    SEAR500               N. SKIP TO NEXT SEGMENT            00394000
         MVC   SADSMELE,(SADDSEID-SADDSENT)(R3) LOAD SEARCH ARGUMENT    00395000
         STCM  R2,B'1111',SADSMHIN SAVE HIGH-INDEX                      00396000
*                                  IS ELEMENT ID NULL                   00397000
         OC  (SADDSEID-SADDSENT)(L'SADDSEID,R3),(SADDSEID-SADDSENT)(R3) 00398000
         BNZ   SEAR600               N. SKIP TO BEGIN BINARY SEARCH     00399000
         CR    R2,R0               HAVE WE SEARCHED ALL OF DSM9913569   00400000
         BNL   EXIT                  Y. SKIP OUT                        00401000
SEAR500  DS    0H                                                       00402000
         LA    R14,0               CLEAR R14                            00403000
         LA    R15,0               CLEAR R15                            00404000
         IC    R15,(SADDSNOL-SADDSENT)(R3) R14/R15 = # OCCURS LEVELS    00405000
         MR    R14,R5              R15 = L'OCCURS DATA                  00406000
         LA    R3,SADDSELN(R15,R3) R3  = A(NEXT SEGMENT IN DSM)         00407000
         LA    R2,1(,R2)           INCR HIGH-INDEX + 1                  00408000
         B     SEAR400             LOOP TO FIND CURR SEGMENT            00409000
         SPACE                                                          00410000
SEAR600  DS    0H                                                       00411000
         SADSMGET SADSMGWK,SADSMELE,SCHAIN=YES                  0400290 00412000
         STCM  R0,B'0011',SADSMNOD LOAD NUMBER NODES SEARCHED           00413000
         STCM  R1,B'1111',SADSMADD LOAD A(REQUESTED DSM ITEM)   0400290 00414000
         LTR   R15,R15             WAS THE SEARCH SUCCESSFUL            00415000
         BNZ   SEARERR                                                  00416000
         CLC   SADSMCOM,=CL6'LOCATE' IS THIS LOCATE MODE                00417000
         BE    EXIT                    Y. GO RETURN TO CALLER           00418000
         LA    R14,0               CLEAR R14                            00419000
         LA    R15,0               CLEAR R15                            00420000
         IC    R15,(SADDSNOL-SADDSENT)(,R3) R14/R15 = # OCCURS LEVELS   00421000
         LA    R0,SADDSGLN         R0 = L'OCCURS LEVEL DATA (1 SEG)     00422000
         MR    R14,R0              R15 = L'OCCURS DATA                  00423000
         LA    R15,SADDSELN(,R15)  R15 = L'TOTAL SEGMENT                00424000
         BCTR  R15,0               R15 = TOTAL SEGMENT CODE             00425000
         EX    R15,SEARMVC         MOVE SEGMENT TO FIXED LOCATION       00426000
         LA    R14,SADSMSCH        R14 = A(TARG:SADSMCMA SCH) 9913569   00427000
         LA    R15,L'SADSMSCH      R15 = L'TARG:SADSMCMA SCH) 9913569   00427020
         LA    R0,SEGTABLC         R0  = A(SOUR:SADSMGET WKAR)9913569   00427040
         LR    R1,R15              R1  = L'SOUR:SADSMGET WKAR 9913569   00427060
         MVCL  R14,R0              SET SRCH CHAIN IF MOVE MODE9913569   00427080
         B     EXIT                GO RETURN TO CALLER                  00428000
         SPACE                                                          00429000
         USING SADDSENT,R1         R1 = A(DSM SEGMENT VIA SADSMGET)     00430000
SEARMVC  MVC   SADSMSEG,SADDSENT   SET UP SEGMENT IF MOVE MODE          00431000
         DROP R1                                                        00432000
         SPACE                                                          00433000
SEARERR  DS    0H                                                       00434000
         LA    R15,8               LOAD RETURN CODE (NOT FOUND)         00435000
         STCM  R15,B'0011',SADSMPRC SET RETURN CODE (NOT FOUND)         00436000
         B     EXIT                GO RETURN TO CALLER                  00437000
         SPACE                                                          00438000
FUNCTERR DS    0H                                                       00439000
         LA    R15,12              LOAD RETURN CODE (FUNCTION ERROR)    00440000
         STCM  R15,B'0011',SADSMPRC SET RETURN CODE (FUNCTION ERROR)    00441000
         EJECT                                                          00442000
EXIT     DS    0H                                                       00443000
         AIF   (&CICS).CICSRET                                          00444000
         SIRETRN RC=0              RETURN                     9913437   00445000
         LTORG                                                          00446000
         AGO   .DATA                                                    00447000
.CICSRET ANOP                                                           00448000
         EXEC  CICS RETURN                                              00449000
         LTORG                                                          00450000
         DFHEISTG                                                       00451000
.DATA    ANOP                                                           00452000
REENTBEG EQU   *                                                        00453000
PARMINAD DS    2F                      ADDRESS LIST OF INPUT PARMS      00454000
DSMLEN   DS    F                       LENG OF DSM                      00455000
#ITEMREM DS    F                       # ITEMS REMAINING      9913569   00455100
@BTREBAS DS    A                       A(BINARY TREE BASE LOC)9913569   00455200
@DSMITMS DS    A                       A(DSM ITEM NEXT/SOURCE)9913569   00455300
@DSMITMT DS    A                       A(DSM ITEM NEXT/TARGET)9913569   00455400
HILVL    DS    F                       BLDTREE HIGHEST LEVEL            00456000
HILVLITM DS    F                       BLDTREE HIGHEST LEVEL ITEM #     00457000
OVRLDATA DS    CL2                     DATA OVERLD/PSEUDO-BST 9913569   00457100
TRANAREA DS    CL64                    EXPANSION TRANSIENT WORKAREA     00458000
*                                                                       00459000
SOFFSETS DS   0XL4                     BINSRCH OFFSETS/STARTER ITEM     00460000
SLOFFSET DS    HL2                     LEFT CHILD OFFSET                00461000
SROFFSET DS    HL2                     RIGHT CHILD OFFSET               00462000
COFFSETS DS   0XL4                     BINSRCH OFFSETS/CURRENT ITEM     00463000
CLOFFSET DS    HL2                     LEFT CHILD OFFSET                00464000
CROFFSET DS    HL2                     RIGHT CHILD OFFSET               00465000
*                                                                       00466000
SILKPARM DS    0F,0CL24                SILINK PARMS           9913569   00467000
SORTNMAD DS     A                      A(SORT PROGRAM NAME)             00468000
PARMLIST DS    3A                      SORT PARAMETER LIST              00469000
SORTNAME DS     CL8                    SORT PROGRAM NAME                00470000
*                                                                       00471000
SORTGCA  DS    0F                      SORT CONTROL WORK AREA (~GCA)    00472000
         DS    CL90                                                     00473000
SORTRCOD DS    F                       SORT RETURN CODE                 00474000
SORTECOD DS    F                       SORT ERROR  CODE                 00475000
*                                                                       00476000
SORTPARM DS    0F                      SORT CONTROL PARAMETERS          00477000
         DS    3F                                                       00478000
SORT#ENT DS    F                       SORT NUMBER ENTRIES              00479000
SORTELEN DS    H                       SORT ENTRY LENGTH                00480000
SORTKOFF DS    H                       SORT ENTRY KEY OFFSET            00481000
SORTKLEN DS    H                       SORT ENTRY KEY LENGTH            00482000
*                                                                       00483000
SADSMGWK DS    0D                  SADSMGET WORKAREA           0400290  00484000
SADSMGID DS    CL8                 SADSMGET SRCH ARG: ELE ID   0400290  00485000
SADSMITA DS    AL4                 A(REQUESTED DSM ITEM)       0400290  00486000
SADSM#NO DS    FL4                 # NODES SEARCHED            0400290  00487000
         DS   8FL4                 SADSMGET WORKAREA          9913569   00487100
SEGCHAIN DS    AL4                 A(SCHAIN DATA - NEXT SLOT) 9913569   00488000
SEGTABLC DS    &TBLSIZE.XL10           SEGMENT BINSRCH DATA TABLE       00489000
         ORG   SEGTABLC                                                 00490000
*                                    * TABLE AS USED BY COMPRESS        00491000
SEGNAME  DS    CL8                     ELEMENT ID                       00492000
SEGDISP  DS    AL2                     OFFSET FROM BEGINNING OF DSM     00493000
         ORG   SEGTABLC                                                 00494000
*                                    * TABLE AS USED BY EXPAND          00495000
SEGTABLE DS    &TBLSIZE.XL8            SEGMENT BINSRCH DATA TABLE       00496000
         ORG   SEGTABLE                                                 00497000
SEGADDR  DS    AL4                     SEGMENT ADDRESS                  00498000
SEGLEN   DS    F                       SEGMENT LENGTH                   00499000
         ORG                                                            00500000
REENTEND EQU   *                                                        00501000
         EJECT                                                          00502000
SADSMCMA SADSMCMA DSECT=YES                                             00503000
         EJECT                                                          00504000
         SAD9DSMA DSECT=YES                                             00505000
         AIF   ('&SYSPARM' NE 'CICS').DTSBAT                            00506185
SADSMCXC CSECT                                                          00506186
         AGO   .DTSMRG                                                  00506187
.DTSBAT  ANOP                                                           00506188
SADSMCXB CSECT                                                          00506189
.DTSMRG  ANOP                                                           00506190
         LTORG                                                          00506191
         DS    0D                                                       00506192
SITMSTMP DC    CL64'SADSMCXB  -----TSD-             10/24/11  09.49.06' 00506193
*        THIS PROGRAM CONTAINS TRADE SECRETS THAT BELONG                00506194
*        TO FIDELITY INFORMATION SERVICES AND IS                        00506195
*        LICENSED BY AN AGREEMENT.  ANY UNAUTHORIZED ACCESS,            00506196
*        USE, DUPLICATION, OR DISCLOSURE IS UNLAWFUL.                   00506197
*        COPYRIGHT FIDELITY INFORMATION SERVICES                        00506198
*        2011, ALL RIGHTS RESERVED.                                     00506199
         END                                                            00506200
