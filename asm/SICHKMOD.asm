*     * 802262 12/11/20 PROYECTO UPGRADE GN172 EDVR
         TITLE 'ROUTINE TO VERIFY LOAD MODULE IS IN A LOAD LIBRARY'     00000100
SICHKMOD START                                                          00000200
         SIBASE BASEREG=(12),RWNUM=0018,RWSUB=000                       00000300
         LM    R3,R4,0(R1)         GET MODULE NAME AND FLAG ADDR        00000400
         CLC   LASTNAME,0(R3)      CHK FOR SAME MODULE AS LAST CALL     00000500
         BE    EXIT                SAME AS LAST CALL                    00000600
         MVC   LASTNAME,0(R3)      SAVE MODULE NAME                     00000700
         LA    R5,TABLE            LOAD IN-CORE TABLE POINTERS          00000800
         LA    R6,TABLEND                                               00000900
         LA    R7,TBLENT                                                00001000
TABLOOP  EQU   *                                                        00001100
         CLI   0(R5),C' '          IS THIS AN EMPTY ENTRY               00001200
         BE    NOENTRY             YES-NO ENTRY IN TABLE YET            00001300
         CLC   LASTNAME,0(R5)      COMPARE THIS MODULE TO ENTRY IN TABL 00001400
         BE    HITIT               MATCHED-GO UPDATE FOUND/NOT FOUND    00001500
         LA    R5,9(0,R5)          NO-BUMP TO NEXT ENTRY                00001600
         BCT   R7,TABLOOP          LOOK SOME MORE                       00001700
NOENTRY  EQU   *                                                        00001800
         MVC   BLDLNAME,LASTNAME   MOVE PASSED MODULE NAME TO BE LOADED 00001900
         MVI   LASTCODE,C'1'       PRESET FLAG TO MODULE FOUND          00002000
         BLDL  0,BLDLLST           SEE IF MODULE CAN BE FOUND           00002100
         LTR   R15,R15             Q. WAS THE LOAD MODULE FOUND?        00002200
         BZ    PUTINTBL            A. YES                               00002300
         MVI   LASTCODE,C'0'       SET FLAG TO MODULE NOT FOUND         00002400
PUTINTBL EQU   *                                                        00002500
         CR    R5,R6               IS THERE STILL ROOM IN TABLE         00002600
         BNL   OVRLAY              NO-GO OVERLAY A 'NOT FOUND' ENTRY    00002700
         MVC   0(8,R5),LASTNAME    YES-SAVE CURRENT MODULE NAME         00002800
         MVC   8(1,R5),LASTCODE    SAVE CURRENT FLAG                    00002900
         B     EXIT                                                     00003000
OVRLAY   NOP   EXIT             BRANCH IF TABLE FULL OF 'FOUND' ENTRIES 00003100
         CLI   LASTCODE,C'1'       WAS MODULE FOUND                     00003200
         BNE   EXIT                NO-DON'T MESS ANY MORE               00003300
         LA    R5,TABLE            RESET TO FRONT OF TABLE              00003400
TABLOOPA EQU   *                                                        00003500
         CLI   8(R5),C'0'          IS THIS A 'NOT FOUND' ENTRY          00003600
         BE    OVRLAYIT            YES-GO RE-USE THIS ENTRY             00003700
         LA    R5,9(0,R5)          NO-BUMP TO NEXT ENTRY                00003800
         CR    R5,R6               IS TABLE EXHAUSTED                   00003900
         BL    TABLOOPA            NO-GO LOOK SOME MORE                 00004000
         OI    OVRLAY+1,X'F0'      YES-TURN ON BRANCH                   00004100
         B     EXIT                GO EXIT IN FRUSTRATION               00004200
OVRLAYIT EQU   *                                                        00004300
         MVC   0(8,R5),LASTNAME    UPDATE THIS ENTRY                    00004400
         MVC   8(1,R5),LASTCODE    SET FLAG                             00004500
         B     EXIT                                                     00004600
HITIT    EQU   *                                                        00004700
         MVC   LASTCODE,8(R5)      MOVE IN FOUND/NOT FOUND FLAG         00004800
EXIT     EQU   *                                                        00004900
         MVC   0(1,R4),LASTCODE                                         00005000
         SIRETRN                                                        00005100
         EJECT                                                          00005200
SPACES   DC    CL8' '                                                   00005300
LASTNAME DC    XL8'FFFFFFFFFFFFFFFF'                                    00005400
LASTCODE DC    CL1'1'                                                   00005500
BLDLLST  DS    0D                                                       00005600
         DC    X'0001000C'                                              00005700
BLDLNAME DC    CL8' '                                                   00005800
         DC    XL4'00'                                                  00005900
         LTORG                                                          00006000
TABLE    DC    200CL9' '                                                00006100
TABLEND  EQU   *                                                        00006200
TBLENT   EQU   (TABLEND-TABLE)/9                                        00006300
         END                                                            00006400
