*     * FO5238*12/05/11 JCTE PROYECTO UPGRADE SYSTEMAT
000100*--------------------------------------------------------------*
000200*    IMWSREGE IS FOR USE WITH PROCEDURE DIVISION COPYBOOKS     *
000300*    IMPDREGE AND IMPDREFF.                                    *
000400*--------------------------------------------------------------*
000500
000600 01  WS-REGE-FIELDS.
000700
000800*--------------------------------------------------------------*
000900*    FOR ONLINE, THE REG E BALANCE CONSISTS OF THE ONLINE      *
001000*    AVAILABLE BALANCE LESS THE OD LIMIT AMOUNT (IF INCLUDED). *
001100*--------------------------------------------------------------*
001200     05  WS-REGE-BAL             PIC S9(13)V99 COMP-3 VALUE +0.
001300
001400     05  WS-REGE-BALX            PIC ZZZZZZ,ZZ9.99-.
001500
001600*--------------------------------------------------------------*
001700*    INITIALIZE TO SPACE (ROUTINE HAS NOT BEEN CALLED)         *
001800*    VALUES IN USE BEFORE JULY 1 AND AFTER AUGUST 15, 2010     *
001900*    MUST ONLY BE CALCULATED ONE TIME                          *
002000*        X = REG E NOT IN EFFECT FOR ANY ACCOUNTS              *
002100*        A = REG E IN EFFECT FOR ALL ACCOUNTS                  *
002200*    VALUES IN USE ONLY BETWEEN JULY 1 AND AUGUST 15, 2010     *
002300*    MUST BE CALCULATED FOR EACH ACCOUNT                       *
002400*        Y = REG E IN EFFECT FOR THIS ACCOUNT                  *
002500*        N = REG E NOT IN EFFECT FOR THIS ACCOUNT              *
002600*--------------------------------------------------------------*
002700     05  WS-REGE-EFF             PIC X           VALUE ' '.
002800
002900*--------------------------------------------------------------*
003000*    EFFECTIVE DATE FOR NEW ACCOUNTS                           *
003100*    ACCOUNTS OPENED ON OR AFTER JULY 1, 2010                  *
003200*--------------------------------------------------------------*
003300*    05  WS-REGE-EFF-DT-NEW      PIC X(6)        VALUE '070110'.  1024493
003301     05  WS-REGE-EFF-DT-NEW      PIC X(6)        VALUE '010410'.  1024493
003400
003500*--------------------------------------------------------------*
003600*    EFFECTIVE DATE FOR EXISTING ACCOUNTS                      *
003700*    ACCOUNTS OPENED PRIOR TO JULY 1, 2010                     *
003800*--------------------------------------------------------------*
003900*    05  WS-REGE-EFF-DT-EXIST    PIC X(6)        VALUE '081510'.  1024493
003901     05  WS-REGE-EFF-DT-EXIST    PIC X(6)        VALUE '010410'.  1024493
004000
