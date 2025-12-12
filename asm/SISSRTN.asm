*     * 802262 12/11/20 PROYECTO UPGRADE GN172 EDVR
         TITLE 'SISSRTN - FILE SEGMENTATION DRIVER MODULE'              00000100
*                                                                     * 00000200
************************************************************* 2024183   00000210
*             ***  REVISION HISTORY   ***                   * 2024183   00000220
*                                                           * 2024183   00000230
* DATE     DESCRIPTION                               CHG ID * 2024183   00000240
* -------- ----------------------------------------- -------* 2024183   00000250
* 05/17/05 CORRECTED THE ERROR IN CALCULATE OFFSET    GN5222* 0545222   00000294
* 10/12/04 INCREASED THE MAX NO OF SEGMENTS TO 200    GN5104* 0525104   00000296
* 03/07/00 ENABLE FOR 31-BIT ADDRESSING              ~~~4183* 2024183   00000298
*********************************************************************** 00000300
*                                                                     * 00000400
*    TITLE : SISSRTN - FILE SEGMENTATION DRIVER MODULE                * 00000500
*                                                                     * 00000600
*    FUNCTION :                                                       * 00000700
*       THIS PROGRAM IS USED TO DRIVE THE FILE SEGMENTATION FEATURE   * 00000800
*       OF SYSTEMATICS SOFTWARE.  IT PROVIDES A MEANS OF DUPLICATING  * 00000900
*       A PROTOTYPE STANDARD I/O MODULE AND A MEANS OF INTERFACING TO * 00001000
*       EACH ONE IT CREATES.                                          * 00001100
*                                                                     * 00001200
*********************************************************************** 00001300
*                                                                     * 00001400
         SPLEVEL TEST                                                   00001500
         TITLE 'SISSRTN - DSECT TABLES'                                 00001600
*                                                                     * 00001700
*********************************************************************** 00001800
*                                                                     * 00001900
* * * *                   DSECT TABLES                          * * * * 00002000
*                                                                     * 00002100
*********************************************************************** 00002200
*                                                                     * 00002300
*        ---------------------------                                    00002400
*        SEGMENT CONTROL BLOCK DSECT                                    00002500
*        ---------------------------                                    00002600
         SPACE 3                                                        00002700
         COPY  SIDSSIOB                                                 00002800
         SPACE 3                                                        00002900
*        -----------------------------                                  00003000
*        STANDARD SI I/O CONTROL DSECT                                  00003100
*        -----------------------------                                  00003200
         SPACE 3                                                        00003300
         COPY  SIDSCNTL                                                 00003400
         SPACE 3                                                        00003500
*        ----------------------------------                             00003600
*        SEGMENT PROTOTYPE NAME TABLE DSECT                             00003700
*        ----------------------------------                             00003800
         SPACE 3                                                        00003900
SPTDS    DSECT                                                          00004000
SPTNM    DS    CL08                NAME OF PROTOTYPE MODULE             00004100
SPTIOA   DS    XL04                POINTER TO I/O NAME TABLE FOR        00004200
*                                   THIS PROTOTYPE NAME                 00004300
SPTEP    DS    XL04                ENTRY POINT OF PROTOTYPE MODULE      00004400
SPTLSPTM DS    XL04                LENGTH OF PROTOTYPE IN DBLWORDS      00004500
SPTOADCN DS    XL04                OFFSET TO ADDRESS CONSTANTS (SIDSIO) 00004600
NESPTT   EQU   40                  NUMBER OF ENTRIES                    00004700
LSPTE    EQU   *-SPTNM             LENGTH OF TABLE ENTRY                00004800
LSPTT    EQU   NESPTT*LSPTE        LENGTH OF TABLE                      00004900
         SPACE 3                                                        00005000
*        ----------------------------                                   00005100
*        SEGMENT I/O NAME TABLE DSECT                                   00005200
*        ----------------------------                                   00005300
         SPACE 3                                                        00005400
SIODS    DSECT                                                          00005500
SIONM    DS    CL08                NAME OF I/O MODULE                   00005600
SIOBAA   DS    XL04                POINTER TO I/O MODULE BUILD AREA     00005700
SIOPTA   DS    XL04                POINTER TO PROTOTYPE TABLE ENTRY     00005800
SIORLF   DS    XL04                RELOCATION FACTOR +/-                00005900
*                                   THIS I/O NAME                       00006000
NEIOMT   EQU   201                 NUMBER OF ENTRIES          0525104   00006100
LSIOTE   EQU   *-SIODS             LENGTH OF TABLE ENTRY                00006200
LSIOTT   EQU   NEIOMT*LSIOTE       LENGTH OF TABLE                      00006300
         SPACE 3                                                        00006400
*        -----------------------------                                  00006500
*        INTERMEDIATE I/O MODULE DSECT                                  00006600
*        -----------------------------                                  00006700
         SPACE 3                                                        00006800
         SIIOMOD TYPE=DSECT                                             00006900
         SPACE 3                                                        00007000
*        ---------                                                      00007100
*        ACB DSECT                                                      00007200
*        ---------                                                      00007300
         SPACE 3                                                        00007400
         IFGACB AM=VSAM                                                 00007500
         SPACE 3                                                        00007600
*        ---------                                                      00007700
*        RPL DSECT                                                      00007800
*        ---------                                                      00007900
         SPACE 3                                                        00008000
         IFGRPL AM=VSAM                                                 00008100
         SPACE 3                                                        00008200
         TITLE 'SISSRTN - PROGRAM ENTRY POINT AND GENERAL HOUSEKEEPING' 00008300
*                                                                     * 00008400
*********************************************************************** 00008500
*                                                                     * 00008600
* * * *        PROGRAM ENTRY POINT AND GENERAL HOUSEKEEPING     * * * * 00008700
*                                                                     * 00008800
*********************************************************************** 00008900
*                                                                     * 00009000
SISSRTN  CSECT                                                          00009100
         SIBASE BASEREG=(SEGBASE),ID=SISSRTN,RWNUM=0022,      2024183  X00009200
               RWSUB=000,SETMODE=3124                         2024183   00009210
         USING SCBASE,R3           NOTIFY ASSEMBLER OF DSECT BASE REGS  00009300
         USING SIDSCNTL,R4         ...                                  00009400
         USING SPTDS,R10           ...                                  00009500
         USING SIODS,R11           ...                                  00009600
         STCM  R1,B'1111',ORIGR1  SAVE ORIGINAL R1 FOR LATER            00009700
         LM    R3,R5,0(R1)        LOAD PARAMETER ADDRESSES              00009800
*                                     REG 3 = SEGIO CTL BLOCK DSECT     00009900
*                                     REG 4 = I-O-CONTROL-AREA          00010000
*                                     REG 5 = I-O-RETURN-AREA           00010100
         TM    SEGSWTCH,SEGINITD   Q. INITIALIZATION DONE               00010200
         BO    SEGMAINL            A. YES GO TO MAINLINE PROCESSING     00010300
         B     SEGINIT             A. NO GO TO INITIALIZATION RTN       00010400
         TITLE 'SISSRTN - INITIALIZATION ROUTINE'                       00010500
*                                                                     * 00010600
*********************************************************************** 00010700
*                                                                     * 00010800
* * * *               INITIALIZATION ROUTINE                    * * * * 00010900
*                                                                     * 00011000
*********************************************************************** 00011100
*                                                                     * 00011200
SEGINIT  DS    0H                                                       00011300
         LA    R1,LSPTT           GET LENGTH OF SEG PROTOTYPE TABLE     00011400
         STCM  R1,B'1111',SEGGSAMT SAVE IN WORK AREA                    00011500
         BAS   LINKREG,SEGGSTG    GO GET STORAGE              2024183   00011600
         MVC   APROTT,SEGGSADR    SAVE ADDRESS OF ACQUIRED STORAGE      00011700
         ICM   R6,B'1111',SEGGSADR PICK UP STORAGE ADDRESS FOR INIT     00011800
         LA    R7,LSPTT           GET LENGTH OF I/O MODULE TABLE        00011900
         LA    R8,*               DUMMY ADDRESS FOR MOVE TO CLEAR       00012000
         ICM   R9,B'1111',=XL04'FF000000'  PAD CHAR AND ZERO LEN        00012100
         MVCL  R6,R8              CLEAR TABLE WITH X'FF'S               00012200
SEGIEND  DS    0H                                                       00012300
         OI    SEGSWTCH,SEGINITD  INDICATE INIT IS DONE                 00012400
         B     SEGMAINL           GO TO MAINLINE PROCESSING             00012500
         TITLE 'SISSRTN - MAINLINE PROCESSING'                          00012600
*                                                                     * 00012700
*********************************************************************** 00012800
*                                                                     * 00012900
* * * *                  MAINLINE PROCESSING                    * * * * 00013000
*                                                                     * 00013100
*********************************************************************** 00013200
*                                                                     * 00013300
SEGMAINL DS    0H                                                       00013400
         CLI   IOOPER,C'O'        Q. IF OPEN THE FILE?                  00013500
         BE    SEGOPEN            A.   YES, GO TO OPENFILE              00013600
         CLI   IOOPER,C'P'        Q. IF OPEN THE FILE?                  00013700
         BE    SEGOPEN            A.   YES, GO TO OPENFILE              00013800
         B     SEGPROC            A. NO, GO TO NORMAL PROCESSING        00013900
         SPACE 3                                                        00014000
*        --------------------------------                               00014100
*        NORMAL SEGMENT LOCATE PROCESSING                               00014200
*        --------------------------------                               00014300
         SPACE 3                                                        00014400
SEGPROC  DS    0H                                                       00014500
         BAS   LINKREG,SCANSPTT   SCAN THE PROTOTYPE TABLE    2024183   00014600
         LTR   R15,R15            Q. WAS NAME FOUND?                    00014700
         BNZ   SEGER01            A. NO, GO TO ERROR MESSAGE            00014800
         BAS   LINKREG,SCANSIOT   SCAN I/O MODULE NAME TABLE  2024183   00014900
         LTR   R15,R15            Q. WAS NAME FOUND?                    00015000
         BNZ   SEGER02            A. NO, GO TO ERROR MESSAGE            00015100
         B     SEGEXEC            A. YES, GO EXECUTE IT                 00015200
         SPACE 3                                                        00015300
*        ------------------------------                                 00015400
*        EXECUTE THE SEGMENT I/O MODULE                                 00015500
*        ------------------------------                                 00015600
         SPACE 3                                                        00015700
SEGEXEC  DS    0H                                                       00015800
         ICM   R15,B'1111',SIOBAA GET ADDRESS OF I/O MODULE             00015900
         ICM   R1,B'1111',ORIGR1  REFRESH R1 TO ORIGINAL STATE          00016000
         LA    R1,4(R1)           BUMP OVER 1ST POINTER                 00016100
         CALL  (15)               GO TO I/O MODULE                      00016200
         B     SEGRET             GO TO RETURN TO CALLER                00016300
SEGEREX  DS    0H                                                       00016400
         STH   R15,IORETCOD       R15 - SET STATUS                      00016500
         MVC   MESSPGM,PGMID      MOVE IN PROGRAM ID                    00016600
         ICM   R15,B'1111',SIMESSEP Q. IS SIMESS LOADED                 00016700
         BNZ   SEGER20            A. YES, GO EXECUTE IT                 00016800
         LOAD  EP=SIMESS          A. NO, LOAD SIMESS                    00016900
         STCM  R0,B'1111',SIMESSEP SAVE THE LOAD ADDRESS OF SIMESS      00017000
         LR    R15,R0             GET ADDRESS INTO BALR REG             00017100
SEGER20  DS    0H                                                       00017200
         CALL  (15),SIDSMESS,VL   GO EXECUTE SIMESS                     00017300
         MVI   MESSOPT,C' '       CLEAR OPTIONAL MESSAGE AREA           00017400
         MVC   MESSOPT+1(L'MESSOPT-1),MESSOPT                           00017500
         LH    R15,IORETCOD       R15 - SET STATUS                      00017600
SEGRET   SIRETRN RC=(15)          PASS CONTROL BACK TO CALLING PGM      00017700
         SPACE 3                                                        00017800
*        -----------------------                                        00017900
*        SEGMENT OPEN PROCESSING                                        00018000
*        -----------------------                                        00018100
         SPACE 3                                                        00018200
SEGOPEN  DS    0H                                                       00018300
         BAS   LINKREG,SCANSPTT   SCAN THE PROTOTYPE TABLE    2024183   00018400
         LTR   R15,R15            Q. WAS NAME FOUND?                    00018500
         BZ    SOP0060            A. YES, SEARCH IO MOD TABLE           00018600
         SPACE 3                                                        00018700
*        ---------------------------------                              00018800
*        ADD ENTRY TO PROTOTYPE NAME TABLE                              00018900
*        ---------------------------------                              00019000
         SPACE 3                                                        00019100
         ICM   R2,B'1111',CTPROTT Q. ROOM IN THE TABLE?                 00019200
         BZ    SEGER03            A. NO, ERROR MESSAGE - TABLE FULL     00019300
         ICM   R10,B'1111',TEPROTT A. YES, POINTER TO AVAILABLE ENTRY   00019400
         MVI   SPTNM,C' '         INITIALIZE NAME FIELD                 00019500
         MVC   SPTNM+1(L'SPTNM-1),SPTNM  ...                            00019600
         MVC   SPTNM(L'SCBPTNM),SCBPTNM SAVE PROTOTYPE NAME IN TABLE    00019700
         LOAD  EPLOC=SPTNM        LOAD THE MODULE REQUESTED             00019800
         STCM  R0,B'1111',SPTEP   SAVE ENTRY POINT                      00019900
         XC    SPTLSPTM,SPTLSPTM  ZERO LENGTH SAVE AREA                 00020000
         STCM  R1,B'0111',SPTLSPTM+1 SAVE LMOD LENGTH IN DBLWORDS       00020100
*                                                                       00020200
*              WE MUST LOCATE THE ADDRESS IN THE PROTOTYPE MODULE       00020300
*              OF THE ADDRESS CONSTANTS LIST.  THE ADDRESS WE ARE       00020400
*              LOOKING FOR SHOULD CONTAIN THE ADDRESS OF ITSELF.        00020500
*                           IE: AWKSTRG   DC   A(AWKSTRG)               00020600
*                                                                       00020700
         ICM   R1,B'1111',SPTEP   GET PROTOTYPE ENTRY POINT             00020800
         LA    R7,0(R1)           ADDRESS OF ENTRY POINT      0545222   00020810
         LA    R6,200             NUMBER OF FULLWORDS TO LOOK AT        00020900
SOP0020  DS    0H                                                       00021000
         C     R1,0(R1)           Q. ADDRESS OF ITSELF?  A(*)           00021100
         BE    SOP0040            A. Y, THEN GO SAVE IT                 00021200
         LA    R1,4(R1)           A. N, BUMP INDEX REG TO NEXT FULLWORD 00021300
         BCT   R6,SOP0020         Q. LAST FULLWORD TO CHECK?            00021400
*                                 A. N, GO CHECK THE NEXT WORD          00021500
         B     SEGER04            A. Y, GO TO ERROR                     00021600
SOP0040  DS    0H                                                       00021700
         SR    R1,R7              CALCULATE OFFSET            0545222   00021810
         STCM  R1,B'1111',SPTOADCN SAVE OFFSET TO THE ADDCONS (SIDSIO)  00021900
         LA    R1,LSIOTT          GET LENGTH OF IO MOD TABLE            00022000
         STCM  R1,B'1111',SEGGSAMT SAVE IN WORK AREA                    00022100
         BAS   LINKREG,SEGGSTG    GO GET STORAGE              2024183   00022200
         MVC   SPTIOA,SEGGSADR    SAVE ADDRESS OF ACQUIRED STORAGE      00022300
         ICM   R6,B'1111',SEGGSADR PICK UP STORAGE ADDRESS FOR INIT     00022400
         LA    R7,LSIOTT          GET LENGTH OF I/O MODULE TABLE        00022500
         LA    R8,*               DUMMY ADDRESS FOR MOVE TO CLEAR       00022600
         ICM   R9,B'1111',=XL04'FF000000'  PAD CHAR AND ZERO LEN        00022700
         MVCL  R6,R8              CLEAR TABLE WITH X'FF'S               00022800
SOP0060  DS    0H                                                       00022900
*                                                                       00023000
*    AT THIS POINT WE MUST SCAN ALL OF THE I/O MODULE TABLES CREATED    00023100
*    THUS FAR.  THIS IS TO CHECK FOR DUPLICATION OF I/O MODULES.  IE:   00023200
*    TO SEE IF ANOTHER PROTOTYPE MODULE WAS USED TO OPEN AN I/O         00023300
*    MODULE WITH THE SAME NAME WE ARE TRYING TO OPEN.                   00023400
*                                                                       00023500
*                                                                       00023600
*                                                                       00023700
         ICM   R10,B'1111',APROTT BEG ADDR OF PROTOTYPE TABLE           00023800
         LA    R6,NESPTT          GET NUMBER IN TABLE                   00023900
SOP0080  DS    0H                                                       00024000
         CLI   SPTNM,X'FF'        Q. IS THIS THE END OF THE TABLE?      00024100
         BE    SOP0120            A. YES, NO DUPLICATES,QUIT LOOKING    00024200
         CLC   SCBPTNM,SPTNM      Q. IS IT THE SELECTED ENTRY?          00024300
         BE    SOP0100            A. YES, BUMP OVER IT-NO NEED TO SRCH  00024400
         BAS   LINKREG,SCANSIOT   SCAN I/O MODULE NAME TABLE  2024183   00024500
         LTR   R15,R15            Q. WAS NAME FOUND?                    00024600
         BZ    SEGER06            A. YES, GO TO ERROR ROUTINE           00024700
SOP0100  DS    0H                                                       00024800
         LA    R10,LSPTE(R10)     A. NO, BUMP POINTER TO TABLE          00024900
         BCT   R6,SOP0080         Q. END OF TABLE?                      00025000
*                                 A. NO, RETURN TO CHECK NEXT ENTRY     00025100
*                                 A. YES, IT'S OK TO ADD THE I/O MODULE 00025200
*                                    THERE IS NOT A DUPLICATE ANYWHERE. 00025300
SOP0120  DS    0H                                                       00025400
*                                                                       00025500
*    WE HAVE NOW DETERMINED THAT THERE IS NOT A DUPLICATE I/O MODULE    00025600
*    ANYWHERE ELSE WITHIN THE I/O MODULE TABLES CREATED THUS FAR.       00025700
*    IT IS NOW OK TO ADD THE NEW I/O MODULE NAME TO THE I/O MODULE      00025800
*    TABLE.  WE WILL REFRESH THE SELECTED PROTOTYPE TABLE ENTRY TO      00025900
*    PROCEED.                                                           00026000
*                                                                       00026100
         ICM   R10,B'1111',TEPROTT REFRESH PROTOTYPE TABLE POINTER      00026200
         ICM   R11,B'1111',SPTIOA POINT TO I/O MODULE NAME TABLE        00026300
         BAS   LINKREG,SCANSIOT   SCAN I/O MODULE NAME TABLE  2024183   00026400
         LTR   R15,R15            Q. WAS NAME FOUND?                    00026500
         BZ    SEGEXEC            A. YES, GO EXECUTE IT                 00026600
         SPACE 3                                                        00026700
*        ----------------------------------                             00026800
*        ADD ENTRY TO I/O MODULE NAME TABLE                             00026900
*        ----------------------------------                             00027000
         SPACE 3                                                        00027100
         ICM   R2,B'1111',CTIOMOD Q. ROOM IN THE TABLE?                 00027200
         BZ    SEGER05            A. NO, ERROR MESSAGE - TABLE FULL     00027300
         ICM   R11,B'1111',TEIOMOD A. YES, POINTER TO AVAILABLE ENTRY   00027400
         MVI   SIONM,C' '         INITIALIZE NAME FIELD                 00027500
         MVC   SIONM+1(L'SIONM-1),SIONM  ...                            00027600
         MVC   SIONM(L'SCBIONM),SCBIONM SAVE NEW I/O NAME IN TABLE      00027700
         STCM  R10,B'1111',SIOPTA SAVE PROTOTYPE POINTER IN I/O TBL     00027800
         ICM   R7,B'1111',SPTLSPTM GET LENGTH OF I/O MODULE TABLE       00027900
*                                 ..THE LENGTH IS IN DOUBLE WORDS       00028000
         SLL   R7,3               MULTIPLY BY 8 FOR LENGTH IN BYTES     00028100
         STCM  R7,B'1111',SEGGSAMT STORE FOR GET STORAGE ROUTINE        00028200
         BAS   LINKREG,SEGGSTG    GO GET STORAGE              2024183   00028300
         MVC   SIOBAA,SEGGSADR    SAVE ADDRESS OF ACQUIRED STORAGE      00028400
         ICM   R6,B'1111',SEGGSADR PICK UP STORAGE ADDRESS FOR INIT     00028500
         ICM   R8,B'1111',SPTEP   POINT TO PROTOTYPE AREA               00028600
         LA    R6,0(R6)           CLEAR HIGH ORDER BIT        2024183   00028610
         LA    R8,0(R8)           CLEAR HIGH ORDER BIT        2024183   00028620
         SR    R6,R8              CALCULATE ADCON RELOCATION FACTOR     00028700
         STCM  R6,B'1111',SIORLF  STORE RELOCATION FACTOR               00028800
         ICM   R6,B'1111',SEGGSADR PICK UP STORAGE ADDRESS FOR INIT     00028900
         LR    R9,R7              GET LENGTH FOR FROM AREA              00029000
         MVCL  R6,R8              MOVE IN PROTOTYPE                     00029100
         BAS   LINKREG,RLADCONS   RELOCATE THE ADCONS NEW MOD 2024183   00029200
         B     SEGEXEC            GO EXECUTE IT                         00029300
         TITLE 'SISSRTN - MISCELLANEOUS ROUTINES'                       00029400
*                                                                     * 00029500
*********************************************************************** 00029600
*                                                                     * 00029700
* * * *              MISCELLANEOUS ROUTINES                     * * * * 00029800
*                                                                     * 00029900
*********************************************************************** 00030000
*                                                                     * 00030100
*        ---------------------------------                              00030200
*        SCAN SEGMENT PROTOTYPE NAME TABLE                              00030300
*        ---------------------------------                              00030400
         SPACE 3                                                        00030500
SCANSPTT DS    0H'0'                                                    00030600
         LA    R15,8              PRESET RC AS NOT FOUND                00030700
         ICM   R2,B'1111',CTPROTT GET PREVIOUS COUNTER ENTRIES LEFT     00030800
         ICM   R10,B'1111',TEPROTT Q. HAVE WE BEEN HERE BEFORE?         00030900
         BZ    SPROT020           A. NO, TO SET UP FOR REGULAR SCAN     00031000
         CLC   SCBPTNM,SPTNM      Q. SAME AS LAST TIME?                 00031100
         BE    SPROT060           A. YES, GO TO INDICATE FOUND          00031200
SPROT020 DS    0H                                                       00031300
         ICM   R10,B'1111',APROTT A. NO, BEG ADDR OF PROTOTYPE TABLE    00031400
         LA    R2,NESPTT          GET NUMBER IN TABLE                   00031500
SPROT040 DS    0H                                                       00031600
         STCM  R10,B'1111',TEPROTT SAVE PROTOTYPE TABLE ENTRY ADDRESS   00031700
         CLI   SPTNM,X'FF'        Q. IS THIS THE END OF THE TABLE?      00031800
         BE    SPROTEND           A. YES THEN GET OUT                   00031900
         CLC   SCBPTNM,SPTNM      Q. IS THIS OUR ENTRY?                 00032000
         BNE   SPROT080           A. NO, TO BUMP THE TABLE POINTER      00032100
SPROT060 DS    0H                                                       00032200
         SR    R15,R15            A. YES, ZERO RC - IND FOUND           00032300
         B     SPROTEND           GET OUT                               00032400
SPROT080 DS    0H                                                       00032500
         LA    R10,LSPTE(R10)     BUMP POINTER TO TABLE                 00032600
         BCT   R2,SPROT040        Q. END OF TABLE?                      00032700
*                                 A. NO, RETURN TO CHECK NEXT ENTRY     00032800
*                                 A. YES THEN GET OUT                   00032900
SPROTEND DS    0H                                                       00033000
         STCM  R2,B'1111',CTPROTT SAVE THIS COUNTER                     00033100
*    R10 CONTAINS THE ADDRESS OF THE TABLE ENTRY IF FOUND               00033200
*    R15 CONTAINS THE RETURN CODE FROM THIS SUB ROUTINE                 00033300
         BR    LINKREG            RETURN TO CALLER                      00033400
         SPACE 3                                                        00033500
*        ----------------------------------                             00033600
*        SCAN SEGMENT I/O MODULE NAME TABLE                             00033700
*        ----------------------------------                             00033800
         SPACE 3                                                        00033900
SCANSIOT DS    0H'0'                                                    00034000
         LA    R15,8              PRESET RC AS NOT FOUND                00034100
         ICM   R2,B'1111',CTIOMOD GET PREVIOUS COUNTER ENTRIES LEFT     00034200
         ICM   R11,B'1111',TEIOMOD Q. HAVE WE BEEN HERE BEFORE?         00034300
         BZ    SIOT020            A. NO, GO SET UP FOR REGULAR SCAN     00034400
         C     R10,SIOPTA         Q. SAME PROTOTYPE AS LAST TIME?       00034500
         BNE   SIOT020            A. NO, GO SET UP FOR REGULAR SCAN     00034600
         CLC   SCBIONM,SIONM      Q. SAME AS LAST TIME?                 00034700
         BE    SIOT060            A. YES, GO TO INDICATE FOUND          00034800
SIOT020  DS    0H                                                       00034900
         ICM   R11,B'1111',SPTIOA GET ADDRESS OF IOM TABLE              00035000
         LA    R2,NEIOMT          GET NUMBER IN TABLE                   00035100
SIOT040  DS  0H                                                         00035200
         STCM  R11,B'1111',TEIOMOD SAVE THIS ADDRESS                    00035300
         CLI   SIONM,X'FF'        Q. IS THIS THE END OF THE TABLE       00035400
         BE    SIOTEND            A. YES THEN GET OUT                   00035500
         CLC   SCBIONM,SIONM      Q. IS THIS OUR ENTRY?                 00035600
         BNE   SIOT080            A. NO, TO BUMP THE TABLE POINTER      00035700
SIOT060  DS    0H                                                       00035800
         SR    R15,R15            A. YES, IND FOUND                     00035900
         B     SIOTEND            GET OUT                               00036000
SIOT080  DS  0H                                                         00036100
         LA    R11,LSIOTE(R11)    BUMP POINTER TO TABLE                 00036200
         BCT   R2,SIOT040         Q. END OF TABLE?                      00036300
*                                 A. NO, RETURN TO CHECK NEXT ENTRY     00036400
*                                 A. YES THEN GET OUT                   00036500
SIOTEND  DS  0H                                                         00036600
         STCM  R2,B'1111',CTIOMOD SAVE THIS COUNTER                     00036700
*    R11 CONTAINS THE ADDRESS OF THE TABLE ENTRY IF FOUND               00036800
*    R15 CONTAINS THE RETURN CODE FROM THIS SUB ROUTINE                 00036900
         BR    LINKREG            RETURN TO CALLER                      00037000
         SPACE 3                                                        00037100
*        -----------                                                    00037200
*        GET STORAGE                                                    00037300
*        -----------                                                    00037400
         SPACE 3                                                        00037500
SEGGSTG  DS    0H                                                       00037600
         XC    SEGGSADR,SEGGSADR  CLEAR WORK AREA                       00037700
         ICM   R6,B'1111',SEGGSAMT Q. IS AMOUNT ZERO?                   00037800
         BZ    SEGGSEND           A. YES GET OUT                        00037900
*                                 A. NO GET THE STORAGE                 00038000
         GETMAIN                                                       X00038100
               R,                                                      X00038200
               LV=(R6),LOC=BELOW                              2024183   00038300
*                        UNCONDITIONAL REQUEST - MVS WILL ABEND IF      00038400
*                        STORAGE IS NOT AVAILABLE                       00038500
         ST    R1,SEGGSADR        SAVE ADDRESS OF STORAGE               00038600
SEGGSEND DS    0H                                                       00038700
         BR    LINKREG            RETURN TO MAINLINE                    00038800
         SPACE 3                                                        00038900
*        -----------------------------------------------                00039000
*        RELOCATE ADCONS IN THE NEWLY CREATED I/O MODULE                00039100
*        -----------------------------------------------                00039200
         SPACE 3                                                        00039300
RLADCONS DS    0H                                                       00039400
         ICM   R6,B'1111',SEGGSADR PICK UP STORAGE ADDRESS FOR INIT     00039500
         A     R6,SPTOADCN        ADD OFFSET TO ADDCONS (SIDSIO)        00039600
         USING SIDSIO,R6                                                00039700
         CLC   OPENSWTI(3),=C'NNN' Q. IS PROTOTYPE USABLE               00039800
         BNE   SEGER07            Q. N, GO TO ERROR ROUTINE             00039900
         MVC   IOINTID,SIONM      MOVE NAME TO THIS MODULE NAME         00040000
         LA    R1,AWKSTRG         RELOCATE ADCONS                       00040100
         ST    R1,AWKSTRG              FOR SISSRTN                      00040200
*                                                                       00040300
         CLI   MODTYPE,C'V'       Q. IS THIS A VSAM MODULE              00040400
         BE    RLA0020            Q. YES BUMP TO VSAM SECTION           00040500
         ICM   R1,B'1111',AINPDCB RELOCATE ADCON                        00040600
         A     R1,SIORLF          ADD RELOACTION FACTOR                 00040700
         STCM  R1,B'1111',AINPDCB STORE ADCON                           00040800
         ICM   R2,B'1111',0(R1)   ADDRESS DCBE                2024183   00040820
         A     R2,SIORLF          ADD RELOACTION FACTOR       2024183   00040830
         STCM  R2,B'1111',0(R1)   STORE UPDATED DCBE ADDRESS  2024183   00040840
         ICM   R1,B'1111',AOUTDCB RELOCATE ADCON                        00040900
         A     R1,SIORLF          ADD RELOACTION FACTOR                 00041000
         STCM  R1,B'1111',AOUTDCB STORE ADCON                           00041100
         ICM   R2,B'1111',0(R1)   ADDRESS DCBE                2024183   00041120
         A     R2,SIORLF          ADD RELOACTION FACTOR       2024183   00041130
         STCM  R2,B'1111',0(R1)   STORE UPDATED DCBE ADDRESS  2024183   00041140
         ICM   R1,B'1111',AUPDDCB RELOCATE ADCON                        00041200
         A     R1,SIORLF          ADD RELOACTION FACTOR                 00041300
         STCM  R1,B'1111',AUPDDCB STORE ADCON                           00041400
         ICM   R2,B'1111',0(R1)   ADDRESS DCBE                2024183   00041420
         A     R2,SIORLF          ADD RELOACTION FACTOR       2024183   00041430
         STCM  R2,B'1111',0(R1)   STORE UPDATED DCBE ADDRESS  2024183   00041440
*                                                                       00041500
         B     RLA0040                                                  00041600
RLA0020  DS    0H                                                       00041700
         ICM   R2,B'1111',AEXITLST RELOCATE ADCON                       00041800
         A     R2,SIORLF          ADD RELOACTION FACTOR                 00041900
         STCM  R2,B'1111',AEXITLST STORE ADCON                          00042000
*                                                                       00042100
         USING IFGRPL,R1                                                00042200
         USING IFGACB,R2                                                00042300
* INPUT RPL/ACB PROCESSING                                              00042400
         ICM   R1,B'1111',AINPRPL RELOCATE ADCON                        00042500
         A     R1,SIORLF          ADD RELOACTION FACTOR                 00042600
         STCM  R1,B'1111',AINPRPL STORE ADCON                           00042700
         ICM   R2,B'1111',AINPACB RELOCATE ADCON                        00042800
         A     R2,SIORLF          ADD RELOACTION FACTOR                 00042900
         STCM  R2,B'1111',AINPACB STORE ADCON                           00043000
         STCM  R2,B'1111',RPLDACB RELOCATE POINTER TO ACB WITHIN RPL    00043100
         MVC   ACBEXLST,AEXITLST  RELOCATE EXITLIST POINTER IN ACB      00043200
* OUTPUT RPL/ACB PROCESSING                                             00043300
         ICM   R1,B'1111',AOUTRPL RELOCATE ADCON                        00043400
         A     R1,SIORLF          ADD RELOACTION FACTOR                 00043500
         STCM  R1,B'1111',AOUTRPL STORE ADCON                           00043600
         ICM   R2,B'1111',AOUTACB RELOCATE ADCON                        00043700
         A     R2,SIORLF          ADD RELOACTION FACTOR                 00043800
         STCM  R2,B'1111',AOUTACB STORE ADCON                           00043900
         STCM  R2,B'1111',RPLDACB RELOCATE POINTER TO ACB WITHIN RPL    00044000
         MVC   ACBEXLST,AEXITLST  RELOCATE EXITLIST POINTER IN ACB      00044100
* UPDATE RPL/ACB PROCESSING     (UPDATE USES SAME RPL & ACB AS OUTPUT)  00044200
         MVC   AUPDRPL,AOUTRPL    OUTPUT/UPDATE ARE SAME                00044300
         MVC   AUPDACB,AOUTACB    OUTPUT/UPDATE ARE SAME                00044400
*                                                                       00044500
RLA0040  DS    0H                                                       00044600
         BR    LINKREG            RETURN TO MAINLINE                    00044700
         DROP  R1,R2,R6                                                 00044800
         TITLE 'SISSRTN - ERROR ROUTINES'                               00044900
*                                                                     * 00045000
*********************************************************************** 00045100
*                                                                     * 00045200
* * * *                ERROR ROUTINES                           * * * * 00045300
*                                                                     * 00045400
*********************************************************************** 00045500
*                                                                     * 00045600
*        -------------------                                            00045700
*        PROTOTYPE NOT FOUND                                            00045800
*        -------------------                                            00045900
         SPACE 3                                                        00046000
SEGER01  DS    0H                                                       00046100
         MVC   MESSNO,=CL04'0501' SET UP MESSAGE CODE                   00046200
         MVC   M01PTNM,SCBPTNM    MOVE PROTOTYPE NAME TO MESSAGE        00046300
         MVC   M01IONM,SCBIONM    MOVE I/O MOD NAME TO MESSAGE          00046400
         MVC   MESSOPT(MSG01L),MSG01 MOVE IN MESSAGE TEXT               00046500
         B     SEGEREX            GO TO ERROR EXIT ROUTINE              00046600
         SPACE 3                                                        00046700
*        --------------------                                           00046800
*        I/O MODULE NOT FOUND                                           00046900
*        --------------------                                           00047000
         SPACE 3                                                        00047100
SEGER02  DS    0H                                                       00047200
         MVC   MESSNO,=CL04'0501' SET UP MESSAGE CODE                   00047300
         MVC   M02PTNM,SCBPTNM    MOVE PROTOTYPE NAME TO MESSAGE        00047400
         MVC   M02IONM,SCBIONM    MOVE I/O MOD NAME TO MESSAGE          00047500
         MVC   MESSOPT(MSG02L),MSG02 MOVE IN MESSAGE TEXT               00047600
         B     SEGEREX            GO TO ERROR EXIT ROUTINE              00047700
         SPACE 3                                                        00047800
*        -----------------------                                        00047900
*        PROTOTYPE TABLE IS FULL                                        00048000
*        -----------------------                                        00048100
         SPACE 3                                                        00048200
SEGER03  DS    0H                                                       00048300
         MVC   MESSNO,=CL04'0501' SET UP MESSAGE CODE                   00048400
         MVC   MESSOPT(MSG03L),MSG03 MOVE IN MESSAGE TEXT               00048500
         B     SEGEREX            GO TO ERROR EXIT ROUTINE              00048600
         SPACE 3                                                        00048700
*        ----------------------------                                   00048800
*        PROTOTYPE IS NOT COMPATIABLE                                   00048900
*        ----------------------------                                   00049000
         SPACE 3                                                        00049100
SEGER04  DS    0H                                                       00049200
         MVC   MESSNO,=CL04'0501' SET UP MESSAGE CODE                   00049300
         MVC   M04PTNM,SCBPTNM    MOVE PROTOTYPE NAME TO MESSAGE        00049400
         MVC   MESSOPT(MSG04L),MSG04 MOVE IN MESSAGE TEXT               00049500
         B     SEGEREX            GO TO ERROR EXIT ROUTINE              00049600
         SPACE 3                                                        00049700
*        ------------------------                                       00049800
*        I/O MODULE TABLE IS FULL                                       00049900
*        ------------------------                                       00050000
         SPACE 3                                                        00050100
SEGER05  DS    0H                                                       00050200
         MVC   MESSNO,=CL04'0501' SET UP MESSAGE CODE                   00050300
         MVC   MESSOPT(MSG05L),MSG05 MOVE IN MESSAGE TEXT               00050400
         B     SEGEREX            GO TO ERROR EXIT ROUTINE              00050500
         SPACE 3                                                        00050600
*        -----------------------------------------------                00050700
*        DUPLICATE I/O MODULE IN ANOTHER PROTOTYPE TABLE                00050800
*        -----------------------------------------------                00050900
         SPACE 3                                                        00051000
SEGER06  DS    0H                                                       00051100
         MVC   MESSNO,=CL04'0501' SET UP MESSAGE CODE                   00051200
         MVC   M06PTNM2,SPTNM     MOVE IN PROTOTYPE NAME WITH DUP I/O   00051300
         MVC   M06IONM,SIONM      MOVE IN I/O NAME                      00051400
         ICM   R10,B'1111',TEPROTT REFRESH PROTOTYPE TABLE POINTER      00051500
         MVC   M06PTNM1,SPTNM     MOVE IN NEW PROTOTYPE NAME            00051600
         MVC   MESSOPT(MSG06L),MSG06 MOVE IN MESSAGE TEXT               00051700
         B     SEGEREX            GO TO ERROR EXIT ROUTINE              00051800
*        -------------------------------                                00051900
*        PROTOTYPE MODIFIED AND UNUSABLE                                00052000
*        -------------------------------                                00052100
         SPACE 3                                                        00052200
SEGER07  DS    0H                                                       00052300
         MVC   MESSNO,=CL04'0501' SET UP MESSAGE CODE                   00052400
         MVC   M07PTNM,SCBPTNM    MOVE PROTOTYPE NAME TO MESSAGE        00052500
         MVC   MESSOPT(MSG07L),MSG07 MOVE IN MESSAGE TEXT               00052600
         B     SEGEREX            GO TO ERROR EXIT ROUTINE              00052700
         TITLE 'SISSRTN - LITERALS, EQUATES, AND CONSTANTS'             00052800
*                                                                     * 00052900
*********************************************************************** 00053000
*                                                                     * 00053100
* * * *          LITERALS, EQUATES, AND CONSTANTS               * * * * 00053200
*                                                                     * 00053300
*********************************************************************** 00053400
*                                                                     * 00053500
WKSTGSEG DC    0D'0'                                                    00053600
         DC    CL12'**WKSTGSEG**'                                       00053700
         SPACE 3                                                        00053800
LINKREG  EQU   R8                  EQU FOR LINKAGE REGISTER             00053900
SEGBASE  EQU   R12                 EQU FOR BASE REGISTER                00054000
PGMID    DC    CL08'SISSRTN'       THIS MUST REMAIN UNCHANGED           00054100
APROTT   DC    F'0'                ADDRESS OF PROTOTYPE TABLE           00054200
AIOMOD   DC    F'0'                ADDRESS OF I/O MODULE TABLE          00054300
TEPROTT  DC    F'0'                ADDRESS OF PROTOTYPE TABLE ENTRY     00054400
TEIOMOD  DC    F'0'                ADDRESS OF I/O MODULE TABLE ENTRY    00054500
CTPROTT  DC    F'0'                COUNT REMAINDER IN PROTOTYPE TABLE   00054600
CTIOMOD  DC    F'0'                COUNT REMAINDER IN I/O MODULE TABLE  00054700
ORIGR1   DC    F'0'                                                     00054800
SEGGSADR DC    F'0'                ADDRESS OF ACQUIRED STORAGE -GET     00054900
SEGGSAMT DC    F'0'                AMOUNT OF ACQUIRED STORAGE -GET      00055000
LMODEP   DC    F'0'                ENTRY POINT OF LOADED MODULE         00055100
LMODLDW  DC    F'0'                LOADED MODULE LENGTH IN DBLWORDS     00055200
SIMESSEP DC    F'0'                ENTRY POINT OF SIMESS                00055300
*                                                                       00055400
SEGSWTCH DC    X'00'                                                    00055500
SEGINITD EQU   X'01'               INITIALIZATION DONE                  00055600
*                                                                       00055700
*                                                                       00055800
MSG01    DC    C'PROTOTYPE=('                                           00055900
M01PTNM  DC    CL(L'SCBPTNM)' '                                         00056000
         DC    C') FOR I/O MODULE=('                                    00056100
M01IONM  DC    CL(L'SCBIONM)' '                                         00056200
         DC    C') - WAS NOT SET UP USING AN OPEN'                      00056300
MSG01L   EQU   *-MSG01                                                  00056400
*                                                                       00056500
MSG02    DC    C'I/O MODULE=('                                          00056600
M02IONM  DC    CL(L'SCBIONM)' '                                         00056700
         DC    C') FOR PROTOTYPE=('                                     00056800
M02PTNM  DC    CL(L'SCBPTNM)' '                                         00056900
         DC    C') - WAS NOT SET UP USING AN OPEN'                      00057000
MSG02L   EQU   *-MSG02                                                  00057100
*                                                                       00057200
MSG03    DC    C'THE PROTOTYPE TABLE IS FULL-PROGRAM CHANGE REQUIRED'   00057300
MSG03L   EQU   *-MSG03                                                  00057400
*                                                                       00057500
MSG04    DC    C'PROTOTYPE=('                                           00057600
M04PTNM  DC    CL(L'SCBPTNM)' '                                         00057700
         DC    C') - IS NOT COMPATIABLE'                                00057800
MSG04L   EQU   *-MSG04                                                  00057900
*                                                                       00058000
MSG05    DC    C'THE I/O MODULE TABLE IS FULL-PROGRAM CHANGE REQUIRED'  00058100
MSG05L   EQU   *-MSG05                                                  00058200
*                                                                       00058300
MSG06    DC    C'I/O=('                                                 00058400
M06IONM  DC    CL(L'SCBIONM)' '                                         00058500
         DC    C') OF PROTOTYPE=('                                      00058600
M06PTNM1 DC    CL(L'SCBPTNM)' '                                         00058700
         DC    C') HAS DUPLICATE WITHIN PROTYPE=('                      00058800
M06PTNM2 DC    CL(L'SCBPTNM)' '                                         00058900
         DC    C')'                                                     00059000
MSG06L   EQU   *-MSG06                                                  00059100
*                                                                       00059200
MSG07    DC    C'PROTOTYPE=('                                           00059300
M07PTNM  DC    CL(L'SCBPTNM)' '                                         00059400
         DC    C') HAS BEEN MODIFIED AND IS UNUSABLE'                   00059500
MSG07L   EQU   *-MSG07                                                  00059600
*                                                                       00059700
*        -------------------                                            00059800
*        SIMESS CONTROL AREA                                            00059900
*        -------------------                                            00060000
         COPY  SIDSMESS                                                 00060100
SISSRTN  CSECT                                                          00060196
         LTORG                                                          00060197
         DS    0D                                                       00060198
SITMSTMP DC    CL64'SISSRTN   -----TSD-             05/17/05  01.33.58' 00060199
*        THIS PROGRAM CONTAINS TRADE SECRETS THAT BELONG                00060200
*        TO FIDELITY INFORMATION SERVICES AND IS                        00060201
*        LICENSED BY AN AGREEMENT.  ANY UNAUTHORIZED ACCESS,            00060202
*        USE, DUPLICATION, OR DISCLOSURE IS UNLAWFUL.                   00060203
*        COPYRIGHT FIDELITY INFORMATION SERVICES                        00060204
*        2005, ALL RIGHTS RESERVED.                                     00060205
         TITLE 'SISSRTN - GENERATED LITERALS, EQUATES, AND CONSTANTS'   00060300
         END                                                            00060400
*********************************************************************** 00060500
*                                                                     * 00060600
* * * *          GENERATED LITERALS, EQUATES, AND CONSTANTS     * * * * 00060700
*                                                                     * 00060800
*********************************************************************** 00060900
*                                                                     * 00061000
         LTORG                                                          00061100
SISSRTN  CSECT                                                          00061196
         LTORG                                                          00061197
         DS    0D                                                       00061198
SITMSTMP DC    CL64'SISSRTN   -----TSD-GN004        01/15/93  09.45.00' 00061199
         END                                                            00061200
