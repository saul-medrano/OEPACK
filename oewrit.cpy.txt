! @#$^&* 137 23 392 0 0 0 0 0 0 0 0 0 8 *&^$#@ 
!!OEWRIT.CPY 8.0(26)
!!
!!                       V. I. C. S.
!!                    of Camp Verde, AZ
!!
!!Copyright (c) 1986,1987,1988,1989,1990,1991,1992,1993,1994,1995,1996,1997,
!!              1998,1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,
!!              2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021,
!!              2022 by
!!
!!      VERTICAL INTEGRATED COMPUTER SYSTEMS
!!            2933 W Middle Verde Rd
!!              Camp Verde, AZ  86322
!!               (928) 567-3727
!!
!!              Vertical Integrated Computer Systems, Camp Verde, AZ  USA
!!              All rights reserved.
!!
!!Post to inventory (OEPQTY and OEPEOD)
!!
!![1] 08/01/90 ESE      See OEPQTY[29] and OEPEOD.
!!
!![2] 08/18/90 ESE      Use TRXD instead of TRXP.
!!
!![3] 11/15/90 HLB      Commented out access to UKY.  Never set.
!!
!![4] 11/26/90 HLB      [3] is wrong.  It is used in OEPQTY
!!
!![5] 11/26/90 ESE      New QTY key logic added.
!!
!![6] 12/19/90 ESE      QTY key change removed, PHS key added.
!!
!![7] 01/16/91 ESE      Allow only sales to be posted to PHS.
!!
!![8] 01/18/91 ESE      Add net change totals to PHS.
!!
!![9] 01/22/91 ESE      Fixed PRH'INVENTORY to work if "O"
!!
!![10] 01/24/91 HLB     Changed sign in [8]
!!
!![11] 01/25/91 HLB     Transfers were not updating PHS
!!                      Also - only sales PHS not working correctly.
!!                      Basically, bunches of changes to UPDATE'PHS, 
!!                      undocumented.
!!
!![12] 02/04/91 HLB     Added all references to MIL file.
!!
!![13] 08/26/91 ESE     Use new VICQTY.SBR.
!!
!![14] ???
!!
!![15] 11/18/91 HLB     Interference with PROC'FTIME.  Caused problems
!!                      with return processing.
!!
!![16] 11/06/92 ESE     NEW:  Disable new VICQTY.SBR.
!!
!![17] 12/08/92 JMS     Add SRL keys when necessary.
!!
!![18] 12/17/92 ???     What is it!
!!
!![19] 03/03/93 ESE/HLB Re-wrote batch finding process to stop locking
!!                      problems.
!!
!![20] 04/07/93 ESE     NEW: do not change purchase date re: serial in X-fer.
!!
!![21] 04/21/93 HLB     NEW: Modified to use new costing macro
!!
!![22] 01/30/95 ESE     FIX: Kits very bad news - unpredictable at times.
!!
!![23] 03/08/95 HLB     FIX: When checking GLI batch, make sure that it
!!                      is not waiting for chaining repost.
!!
!!7.4   09/30/95 CAE    New version created
!!
!![24] 01/04/96 ESE     NEW: force UOM = 1 for T-Shirt.
!!
!![25] 07/08/96 ESE     FIX: not updating kits properly.
!!
!![26] 07/09/96 ESE     FIX: not updating kits properly.
!!
!!8.0  03/31/97 CAE     New version created
!!
!![27] 03/31/97 CAE     NEW: Changes for year 2000.
!!
!![28] 05/14/97 ESE     NEW: use ADDL
!!
!![29] 07/14/97  ESE   FIX: build kits put reasonable net/gross.
!!
LCL1    TRX'FTIME,B,1
\COPYF MEA IN MODE="PRI"
\COPYF  PHS IN
\COPYF  UOM IN MODE="PRI"
\COPYF  GLI IN
\COPYF  BCH IN
\COPYF  BCH IN BCH8 MODE="KEY"
\COPYF  TRX IN
\COPYF  BSC IN
\COPYF  MIL IN  
COPYF   "XXXFIL",NOAFR
!!
\COPYF  QTY IN QTY3 MODE="DTA"
\COPYF  PRH IN MODE="PRI"
\COPYF  APA IN MODE="PRI"
\COPYF  QTY IN MODE="PRI"
\COPYF  KTD IN MODE="PRI"
\COPYF  STX IN MODE="DTA"
\COPYF  CST IN MODE="PRI"                       !![11]
\COPYF  PRC IN MODE="PRI"                       !![11]
!!
\COPYF  HST AR
!!
COPY "CVTMAP"

MAP1 XDAYS,F
MAP1 MEAPTR,X,30

!!MISC MAPS

MAP1    JUNK'OVR1
        MAP2    FIRST'GLI(3),B,1
MAP1    JUNK'OVR2,@JUNK'OVR1
        MAP2    JUNK'FLD1,B,1,1
        MAP2    JUNK'FLD2,B,1,1
        MAP2    JUNK'FLD6,B,1,1

MAP1    JUNK'OVR3
        MAP2    SET'GLI'MODE(3),S,1
MAP1    JUNK'OVR4,@JUNK'OVR3
        MAP2    JUNK'FLD3,S,1,"S"
        MAP2    JUNK'FLD4,S,1,"T"
        MAP2    JUNK'FLD5,S,1,"O"

MAP1    SAV'LOC'NO,B,2
MAP1    FLAG,B,1
MAP1    LAST'FLAG,B,1
MAP1    SGLI'RCN(3),B,4
MAP1    SBCH'RCN(3),B,4
MAP1    TAX'PERC,B,5
MAP1    SHP'PRICE,B,5
MAP1    SHP'DISC,B,5
MAP1    AVAILABLE,F
MAP1    ARC'HDR,B,1
MAP1    DOKITS'COUNTER,B,5
MAP1    NEW'SHIPMENT,B,2
MAP1    ARCHIVE'LIN,B,1
MAP1    TQTY,B,5
MAP1    BQTY,B,5
MAP1    TQTY3,B,5
MAP1    SQTY,B,5
MAP1    SAV'TQTY,B,5
MAP1    SAV'BQTY,B,5
MAP1    PACTION'SQTY,B,5
MAP1    PACTION'BQTY,B,5
MAP1    CQTY,B,5
MAP1    SUB'FLAG,B,1
MAP1    CUTS,B,1
MAP1    SDATE,B,3
MAP1    BO'FLAG,B,1
MAP1    PROC'FTIME,B,1
MAP1    PROC'FTIME2,B,1         !![15]
MAP1    STQTY,F
LCL1    X,F
MAP1    XQ,F
MAP1    XL,F
MAP1    XX,F
MAP1    XH,F
MAP1    XP,F
MAP1    TC'YN,S,1

FIND'AND'WRITE'SLH:
        IF INC'SLD'DN = "Z" OR HDR'PROC >= 5 RETURN
        SLH'KCUS'NO = HDR'NAM'NO
        SLH'KSEC'NO = 0
        SLH'KTRK'NO = TRX'TRK'NO
        SLH'KUOM'NO = LIN'UOM'NO
        SLH'KLOC'NO = LIN'LOC'NO
        CALL PROCESS'SLH                !!BY PART, ONE LOCATION, ONE UOM
        SLH'KSEC'NO = PRH'SEC'NO
        SLH'KTRK'NO = 0 
        CALL PROCESS'SLH                !!BY PRODUCT LINE, ONE LOCATION
        SLH'KLOC'NO = LIN'LOC'NO
        SLH'KSEC'NO = 0
        CALL PROCESS'SLH                !!BY EVERYTHING, ONE LOCATION
RETURN

PROCESS'SLH:
        \VICS'RDLOCK SLH ACTION="ADD'SLH" V''ADDL="Y" !![28]
        \VICS'POST SLH ACTION="UPDATE'SLH"
RETURN

ADD'SLH:
        SLH'SPACE = SPACE
        SLH'CUS'NO = SLH'KCUS'NO                !Customer Number
        SLH'SEC'NO = SLH'KSEC'NO                !Product Line
        SLH'TRK'NO = SLH'KTRK'NO                !Part Number
        SLH'UOM'NO = SLH'KUOM'NO                !Unit Of Measure
        SLH'LOC'NO = SLH'KLOC'NO                !Location Number
!![28]ADD SLH FROM SLH'REC KEY SLH'KEY
        ADDL SLH FROM SLH'REC KEY SLH'KEY               !![28]
        \SETKEY SLH MODE="KY2"
        \ADDKEY SLH MODE="KY2"
RETURN

UPDATE'SLH:
        IF PRH'INTERFACE # 4 THEN
!![18] - used SAV'TQTY instead of TQTY below.
!!              \FROUND SLH'GROSS+(SAV'TQTY*LIN'PRICE)/PFACTOR/QFACTOR*100 SLH'GROSS
!!              \FROUND SLH'NET+(SAV'TQTY*(LIN'PRICE-LIN'DISC))/PFACTOR/QFACTOR*100 SLH'NET
                \FROUND SLH'GROSS+(SAV'TQTY*TRX'SGROSS)/PFACTOR/QFACTOR*100 SLH'GROSS
                \FROUND SLH'NET+(SAV'TQTY*TRX'SNET)/PFACTOR/QFACTOR*100 SLH'NET
        ELSE
                IF SLH'NEEDS'SUB = 1 THEN
                    \FROUND SLH'GROSS+((LIN'CALC'PRICE*LIN'SQ'FEET*PFACTOR)/QFACTOR/9)/PFACTOR SLH'GROSS
                    \FROUND SLH'NET+(((LIN'CALC'PRICE-LIN'CALC'DISC)*LIN'SQ'FEET*PFACTOR)/QFACTOR/9)/PFACTOR SLH'NET
                    SLH'NEEDS'SUB = 0
                ENDIF
        ENDIF
        \FROUND SLH'ITEMS+TQTY/QFACTOR SLH'ITEMS          !Number Of Items
RETURN

FIND'AND'WRITE'QTY:
        IF (HDR'BULK = "Y" AND QTY'BULK = "Y") OR IN2'WL'YN # "Y" OR &
                (SGN(NPQTY) = -1 AND ARC'INV'CR'YN(HDR'DOC'TYPE) = "Y") THEN
            NPQTY = 0
        ELSE
            NPQTY = TQTY
        ENDIF
        CASE
        IF HDR'PROC = 5 THEN
                BQTY = 0
                PROC'FTIME = 1
                XXX'KIT'BUILD = "Y"
                CALL WRITE'QTY
                PROC'FTIME = 2
                XXX'KIT'BUILD = "N"
!![18]          TQTY = -TQTY
                TQTY = -SAV'TQTY                                !![18]
                CALL WRITE'QTY
!![18]          TQTY = -TQTY
        ELSE
        IF HDR'PROC = 6 THEN
                PROC'FTIME = 1
                CALL WRITE'QTY
                PROC'FTIME = 2
                SAV'LOC'NO = LIN'LOC'NO
                LIN'LOC'NO = HDR'LOC'NO
!![18]          TQTY = -TQTY
                TQTY = -SAV'TQTY                        !![18]
!![18]          BQTY = -BQTY
                BQTY = -SAV'BQTY                        !![18]
                CALL WRITE'QTY
!![18]          TQTY = -TQTY
                LIN'LOC'NO = SAV'LOC'NO
        ELSE
!           IF SGN(NPQTY) = -1 AND ARC'INV'CR'YN(HDR'DOC'TYPE) = "Y" THEN
!               PROC'FTIME = 2
!           ENDIF
            CALL WRITE'QTY
        ENDCASE
        TQTY = SAV'TQTY                         !![18]
        BQTY = SAV'BQTY                         !![18]
RETURN

WRITE'QTY:
        IF XXX'KIT'BUILD = "Y" THEN        !!Not HDR'PROC = 5
                KIT'COST = 0
                KTD'TTL'PRICE = 0                                                                               !![29]
                KTD'TTL'QTY = 0                                                                         !![29]
                KTD'TTL'COUNT = 0                                                                               !![29]
                \DOKITS OE="Y" AVAILABLE="N" PACTION="PACTION"          &
                        OPR'ACTION="OPR'ACTION" TS'ATT'NO1="SUB'ATT'NO1"
                CALL FIND'PRH           !![22]
                IF DOKITS'ABORT = "L" OR DOKITS'ABORT = "P" OR          &
                                DOKITS'ABORT = "D" GOTO ABORT
        ELSE
                CALL FIND'UOM
                QTY'KLOC'NO = LIN'LOC'NO
                QTY'KTRK'NO = LIN'TRK'NO
                QTY'KATT'NO1 = SUB'ATT'NO1
                QTY'KATT'NO2 = SUB'ATT'NO2
                \VICS'RDLOCK QTY ACTION="ADD'QTY1" APID="IN" V''ADDL="Y"        !![28]
                IF UOM'DUNITS # 0 THEN
                        \FROUND (TQTY*UOM'NUNITS)/UOM'DUNITS TQTY
                        \FROUND (BQTY*UOM'NUNITS)/UOM'DUNITS BQTY
                ELSE
                        TQTY = 0
                        BQTY = 0
                ENDIF
                CALL UPDATE'QTY'REC
        ENDIF           
RETURN
 
MAP1 II,F
MAP1 QQ,F
 
OPR'ACTION:
        IF PRH'INVENTORY # "N" THEN
                \VICS'RDLOCK QTY ACTION="ADD'QTY1" APID="IN" V''ADDL="Y" !![28]
        ENDIF
RETURN

MAP1 COST'QTY,B,5
MAP1 KIT'COST,B,5

PACTION:
        IF PRH'INVENTORY # "N" THEN             !![26]
                PACTION'SQTY = TQTY
                PACTION'BQTY = BQTY
                II = DOKITS'THIS'MULT/(10^XXX'QFRACTION)
                \FROUND TQTY*II TQTY
                \FROUND BQTY*II BQTY
                CALL UPDATE'QTY'REC
                WRK'AMT = DOKITS'THIS'MULT/(10^XXX'QFRACTION)
                IF HDR'PROC = 5 OR HDR'PROC = 6 THEN
                    CALL DO'SETCST
                    KIT'COST = KIT'COST + (COST'QTY * WRK'AMT)
                ENDIF
                IF HDR'PROC = 5 OR XXX'KIT'BUILD = "Y" THEN
                    PRC'KTRK'NO = PRH'TRK'NO
                    PRC'KUOM'NO = KTD'UOM'NO
                    CALL KTD'USE'PRC
                    KTD'TTL'PRICE = KTD'TTL'PRICE+(WRK'AMT*PRC'AMT/10^PRH'PFRACTION)
                    KTD'TTL'QTY = KTD'TTL'QTY + WRK'AMT
                    KTD'TTL'COUNT = KTD'TTL'COUNT + 1
                ENDIF
                TQTY = PACTION'SQTY
                BQTY = PACTION'BQTY
        ENDIF
RETURN

DO'SETCST:
    \SETCST INC'CST'DSP PRE="QTY" COST="COST'QTY"
RETURN

ADD'QTY1:
CIF V''VICQTY="Y" THEN
        IF PROC'FTIME # 2 AND INC'VICQTY'YN # "Y" THEN
                ENTRY = "BAD QTY REC: "+QTY'KLOC'NO+"-"+HDR'DOC'TYPE+"-"+HDR'NO+"-"+SUB'LIN'NO+"-"+SUB'NO
                CALL DO'ERRL
        ENDIF
CELSE
        IF PROC'FTIME # 2 THEN
                ENTRY = "BAD QTY REC: "+QTY'KLOC'NO+"-"+HDR'DOC'TYPE+"-"+HDR'NO+"-"+SUB'LIN'NO+"-"+SUB'NO
                CALL DO'ERRL
        ENDIF
ENDCIF

        QTY'SPACE = SPACE
        QTY'LOC'NO = LIN'LOC'NO
        QTY'TRK'NO = LIN'TRK'NO
        QTY'ATT'NO1 = SUB'ATT'NO1
        QTY'ATT'NO2 = SUB'ATT'NO2
        \SETKEY PRE="QTY" MODE="KEY" APID="IN"
        ADDL QTY FROM QTY'REC KEY QTY'KEY                       !![28]
        IF QTY'CTL # 0 THEN
            RDLOCK QTY INTO QTY'REC KEY QTY'KEY
            IF QTY'CTL # 0THEN
                ENTRY = "SERIOUS inventory problems - halt work!"
                \KERROR QTY MODE="K" CNO="N" APID="IN"
                CALL DO'ERRL
            ENDIF
        ENDIF
        \SETKEY PRE="QTY" MODE="KY2" APID="IN"
        \ADDKEY PRE="QTY" MODE="KY2" APID="IN"
RETURN

MAP1 TRANS'MODE,B,1
UPDATE'QTY'REC:
    STQTY = TQTY                    !!COMMITTED AMOUNT TO REDUCE QTY BY
    CUTS = 0                        !!CLEAR FLAG

    \VICS'POST QTY ACTION="UPDATE'QTY" APID="IN"
    \CHI QTY

    \OS'CHECK                           !!check stock limits

    IF CUTS # 0 THEN
        QTY'KATT'NO1 = ABS(TQTY3)
        \VICS'RDLOCK QTY ALTPRE="QTY3" ACTION="ADD'QTY" APID="IN" V''ADDL="Y"
        QTY'REC = QTY3'REC
        \VICS'POST QTY ACTION="PROC'CUTS" APID="IN"
    ENDIF
    CALL ADD'MOV'REC
RETURN

MAP1 QTY'CHANGE,B,5                                     !![13]
MAP1 NPQTY,B,5

UPDATE'QTY:
        QTY'CHANGE = 0
        IF (HDR'BULK = "Y" AND QTY'BULK = "Y") OR IN2'WL'YN # "Y" OR &
                (SGN(NPQTY) = -1 AND ARC'INV'CR'YN(HDR'DOC'TYPE) = "Y") THEN
            NPQTY = 0
        ELSE
            NPQTY = TQTY
        ENDIF
        IF PRH'CUTS'YN = "Y" AND QTY'CUT'SIZE # 0 THEN
                IF TQTY/QTY'CUT'SIZE # INT(TQTY/QTY'CUT'SIZE) THEN
                        IF TQTY > 0 THEN
!!ON HAND AMOUNT TO REDUCE QTY BY
                                \FROUND (INT(TQTY/QTY'CUT'SIZE)+1)*QTY'CUT'SIZE TQTY
                        ELSE
!!ON HAND AMOUNT TO ADD TO QTY WITH
                                \FROUND (INT(TQTY/QTY'CUT'SIZE))*QTY'CUT'SIZE TQTY
                        ENDIF
                        TQTY3 = TQTY - STQTY    !!ON HAND AMOUNT TO ADD BACK
                        CUTS = 1                !!FLAG
                ENDIF
        ENDIF
        CASE
        IF PROC'FTIME = 2 THEN
                PROC'FTIME = 0
!![9]           IF PRH'INVENTORY = "Y" THEN
                IF PRH'INVENTORY # "N" THEN     !![9]
                        QTY'ON'HAND = QTY'ON'HAND - TQTY
                        QTY'PQTY = QTY'PQTY - NPQTY
                ENDIF
                IF BO'FLAG = 1 THEN
                        QTY'BACKORDER = QTY'BACKORDER - BQTY
                ENDIF
                IF HDR'TSAFETY = "Y" AND HDR'PROC = 6 THEN
!!Second part of transfer we took from reserved and bumped safety.
                    QTY'SAFETY = QTY'SAFETY - TQTY
                ENDIF
        ELSE
        IF STQTY > 0 OR PROC'FTIME = 1 THEN
                IF HDR'PROC < 5 THEN
                        QTY'COMMITTED = QTY'COMMITTED - STQTY
                        QTY'CHANGE = QTY'CHANGE - STQTY
                ELSE
                        QTY'RESERVED = QTY'RESERVED - STQTY
!!Won't be here for 2nd part of transfer (FTIME = 2)
                ENDIF
                IF PRH'INVENTORY # "N" THEN     !![9]
                        QTY'ON'HAND = QTY'ON'HAND - TQTY
                        QTY'PQTY = QTY'PQTY - NPQTY
                ENDIF
                IF BO'FLAG = 1 THEN
                        QTY'BACKORDER = QTY'BACKORDER - BQTY
                ENDIF
        ELSE
                IF HDR'PROC < 5 THEN
                        IF HDR'DOC'TYPE = OEC'INT'CR THEN
                                QTY'COMMITTED = QTY'COMMITTED - STQTY
                                QTY'CHANGE = QTY'CHANGE - STQTY
                        ELSE
                                QTY'RAN = QTY'RAN + STQTY
                        ENDIF
                ELSE
                        QTY'RESERVED = QTY'RESERVED + STQTY
                ENDIF
                CASE
                IF SUB'RPROC = 2 THEN
!![9]                   IF PRH'INVENTORY = "Y" THEN
                        IF PRH'INVENTORY # "N" THEN     !![9]
                                QTY'ON'HAND = QTY'ON'HAND - TQTY
!!                              QTY'PQTY = QTY'PQTY - NPQTY
                                QTY'PQTY = QTY'PQTY - TQTY
                        ENDIF
                ELSE
                IF SUB'RPROC = 3 THEN
!![9]                   IF PRH'INVENTORY = "Y" THEN
                        IF PRH'INVENTORY # "N" THEN     !![9]
                                QTY'BAD = QTY'BAD - TQTY
                        ENDIF
                        CUTS = CUTS * 3
                ELSE
                IF SUB'RPROC = 4 THEN
!![9]                   IF PRH'INVENTORY = "Y" THEN
                        IF PRH'INVENTORY # "N" THEN     !![9]
                                QTY'RETURN = QTY'RETURN - TQTY
                        ENDIF
                        CUTS = CUTS * 4
                ELSE
                IF SUB'RPROC = 0 THEN
                        SUB'RPROC = 2
!![9]                   IF PRH'INVENTORY = "Y" THEN
                        IF PRH'INVENTORY # "N" THEN     !![9]
                                QTY'ON'HAND = QTY'ON'HAND - TQTY
                                QTY'PQTY = QTY'PQTY - NPQTY
                        ENDIF
                ELSE
!![9]                   IF PRH'INVENTORY = "Y" THEN
                        IF PRH'INVENTORY # "N" THEN     !![9]
                                QTY'HOLD = QTY'HOLD - TQTY
                        ENDIF
                        CUTS = CUTS * 2
                ENDCASE
        ENDCASE
        QTY'BACKORDER = QTY'BACKORDER MAX 0
        QTY'NEW'STOCK = QTY'NEW'STOCK MIN QTY'BACKORDER 
RETURN

PROC'SRL:
        IF HDR'PROC = 5 OR (HDR'PROC = 6 AND TRX'FTIME # 1) THEN
                GOTO DON'T'LOOK'FOR'SRL !!WE ARE CREATING A NEW SERIAL ITEM
                                        !!EITHER BY SHOP ORDER OR TRANSFER
                                        !!TO ANOTHER LOCATION
        ENDIF
        SRLB'KLOC'NO = TRX'LOC'NO
        SRLB'KTRK'NO = TRX'TRK'NO
        SRLB'KATT'NO1 = TRX'ATT'NO1
        SRLB'KATT'NO2 = TRX'ATT'NO2
        SRLB'KSERIAL = SUB'SERIAL
        SRLB'RCN = 0
        USE SRLB INTO SRL'REC KEY SRLB'KEY
        DO
                GETNXT SRLB INTO SRL'REC
                \CHKKEY PRE="SRLB" LASTFIX="SERIAL" APID="IN"
        LOOP UNTIL SRLB'CTL # 0 OR (SRL'HDR'LOC'NO = HDR'LOC'NO         AND &
                                        SRL'DOC'TYPE = HDR'DOC'TYPE     AND &
                                        SRL'HDR'NO = HDR'NO             AND &
                                        SRL'LIN'NO = LIN'NO             AND &
                                        SRL'SUB'NO = SUB'NO)             OR &
                                        TQTY < 0
!!IF A RETURN, ANY SRL RECORD W/ CORRECT SRL NUMBER WILL DO
        IF SRLB'CTL # 0 THEN
                ENTRY = "Serial record NOT FOUND: "+SUB'SERIAL+"/"
                \SUBERR PRE="SUB" APID="OE"
                CALL DO'ERRL
                RETURN                          !!KLUDGE
        ENDIF
    DON'T'LOOK'FOR'SRL:
        CASE
        IF HDR'PROC < 5 THEN
                IF TQTY > 0 THEN
!!NORMAL SHIPPING, ADD SHP KEY TO SRL RECORD
                        SRL'RCN = SRLB'RCN
                        \VICS'RDLOCK SRL BY'RCN="Y" APID="IN"
                        IF SRL'CTL = 0 THEN
                                \VICS'POST SRL ACTION="UNS'SRL" APID="IN"
!![17]                          \SETKEY PRE="SRLS" MODE="KEY" APID="IN"
!![17]                          ADDKEY SRLS
                        ENDIF
                        CALL ADD'SRL'KEYS                       !![17]
                ELSE
!!RETURN, ADD NEW RECORD WITH RCV KEYS ONLY
                        CALL ADD'SRL'RECORD
                        \VICS'RDLOCK SRL APID="IN"
                        IF SRL'CTL = 0 THEN
                                \VICS'POST SRL ACTION="UR'SRL" APID="IN"
                        ENDIF
!![17]                  \SETKEY PRE="SRLB" MODE="KEY" APID="IN" !![14]
!![17]                  ADDKEY SRLB                             !![14]
                        CALL ADD'SRL'KEYS                       !![17]
                ENDIF
        ELSE
        IF HDR'PROC = 5 THEN
!!SHOP ORDER, SO WE CREATE NEW SRL RECORD FROM SCRATCH, ADD RCV KEYS.
                SRL'SPACE = SPACE
                CALL ADD'SRL'RECORD
                \VICS'RDLOCK SRL APID="IN"
                IF SRL'CTL = 0 THEN
                        \VICS'POST SRL ACTION="USO'SRL" APID="IN"
                ENDIF
                \SETKEY PRE="SRLB" MODE="KEY" APID="IN" !![14]
                ADDKEY SRLB                             !![14]
        ELSE                                            !!HDR'PROC = 6
                IF TRX'FTIME = 1 THEN
!!TRANSFER, FIRST TIME WE SHIP FROM STATED LOCATION BY ADDING SHP KEYS
                        SRL'RCN = SRLB'RCN
                        \VICS'RDLOCK SRL BY'RCN="Y" APID="IN"
                        IF SRL'CTL = 0 THEN
                                \VICS'POST SRL ACTION="UTS'SRL" APID="IN"
                                \SETKEY PRE="SRLS" MODE="KEY" APID="IN"
                                ADDKEY SRLS
                        ENDIF
                ELSE
!!SECOND TIME WE RECEIVE INTO NEW LOCATION
                        CALL ADD'SRL'RECORD
                        \VICS'RDLOCK SRL APID="IN"
                        IF SRL'CTL = 0 THEN
                                \VICS'POST SRL ACTION="UTR'SRL" APID="IN"
                        ENDIF
!![17]                  \SETKEY PRE="SRLB" MODE="KEY" APID="IN" !![14]
!![17]                  ADDKEY SRLB                             !![14]
                        CALL ADD'SRL'KEYS                       !![17]
                ENDIF
        ENDCASE
RETURN

ADD'SRL'RECORD:
!!      SRL'SPACE = SPACE                       !!DO NOT SET TO SPACE
        SRL'RGLI'YEAR = TRX'GLI'YEAR
        SRL'RGLI'NO = TRX'GLI'NO
        SRL'RBCH'NO = TRX'BCH'NO
        SRL'RTRX'NO = TRX'NO
        SRL'RNO = 10
        \SETKEY PRE="SRL" MODE="KEY" APID="IN"
        ADD SRL FROM SRL'REC KEY SRL'KEY
!![14]  \SETKEY PRE="SRLB" MODE="KEY" APID="IN"
!![14]  ADDKEY SRLB
RETURN

ADD'SRL'KEYS:                                           !![17]
        \SETKEY PRE="SRL" MODE="KY2" APID="IN" BAD1="SRLP" BAD2="SRLS"
        \ADDKEY PRE="SRL" MODE="KY2" APID="IN" BAD1="SRLP" BAD2="SRLS"
RETURN

UNS'SRL:
        SRL'FLAG = "N"
        SRL'SELL'DTT = SDATE
        SRL'CUS'NO = HDR'NAM'NO
        SRL'SGLI'YEAR = TRX'GLI'YEAR
        SRL'SGLI'NO = TRX'GLI'NO
        SRL'SBCH'NO = TRX'BCH'NO
        SRL'STRX'NO = TRX'NO
        SRL'SNO = 10
        SRL'SSTAT = "P"
RETURN

UR'SRL:
        SRL'FLAG = STR(TRX'TPROC)
        SRL'SELL'DTT = 0
        SRL'PUR'DTT = SDATE
        SRL'CUS'NO = 0
        SRL'SGLI'YEAR = 0
        SRL'SGLI'NO = 0
        SRL'SBCH'NO = 0
        SRL'STRX'NO = 0
        SRL'SNO = 0
        SRL'SSTAT = "U"
        SRL'RSTAT = "P"
RETURN

USO'SRL:
        SRL'LOC'NO = TRX'LOC'NO
        SRL'TRK'NO = TRX'TRK'NO
        SRL'ATT'NO1 = TRX'ATT'NO1
        SRL'ATT'NO2 = TRX'ATT'NO2
        SRL'SERIAL = SUB'SERIAL
        SRL'CUS'NO = 0
        SRL'SELL'DTT = 0
        SRL'PUR'DTT = SDATE
        SRL'RSTAT = "P"
        SRL'FLAG = "1"
        SRL'SSTAT = "U"
RETURN

UTS'SRL:
        SRL'FLAG = "N"
        SRL'SELL'DTT = SDATE
        SRL'CUS'NO = HDR'NAM'NO
        SRL'SGLI'YEAR = TRX'GLI'YEAR
        SRL'SGLI'NO = TRX'GLI'NO
        SRL'SBCH'NO = TRX'BCH'NO
        SRL'STRX'NO = TRX'NO
        SRL'SNO = 10
        SRL'SSTAT = "P"
RETURN

UTR'SRL:
        SRL'LOC'NO = TRX'LOC'NO         !!RCV LOC'NO
        SRL'SELL'DTT = 0
!![20]  SRL'PUR'DTT = SDATE
        IF SRL'PUR'DTT = 0 THEN SRL'PUR'DTT = SDATE     !![20]
        SRL'CUS'NO = 0
        SRL'SGLI'YEAR = 0
        SRL'SGLI'NO = 0
        SRL'SBCH'NO = 0
        SRL'STRX'NO = 0
        SRL'SNO = 0
        SRL'SSTAT = "U"
        SRL'RSTAT = "P"
        SRL'FLAG = "1"
RETURN

ADD'QTY:
!!
!!WE ADD NEW RECORD USING ORIGINAL QTY'REC - TO KEEP CAGE, AISLE, BIN, ETC.
!!THE SAME AS THE ORIGINAL SPOOL WAS WHEN CUT SMALLER.
!!
        QTY'CUT'SIZE = ABS(TQTY3)
        QTY'ON'HAND = 0
        QTY'ON'ORDER = 0
        QTY'BACKORDER = 0
        QTY'RESERVED = 0
        QTY'COMMITTED = 0
        QTY'SAFETY = 0
        QTY'RAN = 0
        QTY'RETURN = 0
        QTY'BAD = 0
        QTY'HOLD = 0
        QTY'NEW'STOCK = 0
        QTY'OVR'SHORT = 0
        QTY'MSGNO = 0
        \SETKEY PRE="QTY" MODE="KEY" APID="IN"
!!      DD QTY FROM QTY'REC KEY QTY'KEY
        ADDL QTY FROM QTY'REC KEY QTY'KEY
        \SETKEY PRE="QTY" MODE="KY2" APID="IN"
        \ADDKEY PRE="QTY"            APID="IN"
        QTY3'REC = QTY'REC                      !!We restore later.
RETURN

PROC'CUTS:
        CASE
        IF CUTS = 1 THEN
                QTY'ON'HAND = QTY'ON'HAND + TQTY3
        ELSE
        IF CUTS = 2 THEN
                QTY'HOLD = QTY'HOLD + TQTY3
        ELSE
        IF CUTS = 3 THEN        
                QTY'BAD = QTY'BAD + TQTY3
        ELSE
        IF CUTS = 4 THEN
                QTY'RETURN = QTY'RETURN + TQTY3
        ENDCASE
RETURN

MAP1 WRK,B,5                                            !![4]
MAP1 SLH'NEEDS'SUB,B,1                                  !![4]
MAP1 PHS'NEEDS'SUB,B,1                                  !![4]
MAP1 MIL'NEEDS'SUB,B,1                                  !![12]

POST'TO'TRX:
        CALL FIND'PRH
        TRX'FTIME = 1
        CASE
        IF HDR'PROC >= 5 OR TC'YN = "Y" THEN
                FLAG = 3
        ELSE
        IF XXX'COM'QTY >= 0 THEN
                FLAG = 1
        ELSE
                FLAG = 2
        ENDCASE
        DO
                REPOST'TRANSFER = 0     !![13] moved from before 'DO'
!![1] - test date below.
                CASE
                IF FIRST'GLI(FLAG) = 1 OR BATCH'DTT # SDATE THEN
                                        !!Lookup and lock or add and lock as needed
                        CALL SETUP'BATCHES
                ELSE
                IF LAST'FLAG # FLAG THEN
                                        !!Re-RDLOCK the current type of batch
MAP1 GGG'CNT,B,2
                    GGG'CNT = 0
                    DO
                        RDLOCK GLI SGLI'RCN(FLAG)
                        IF GLI'CTL # 4 EXIT
                        GGG'CNT = GGG'CNT + 1
                        IF GGG'CNT > 2 THEN
                            RDLOCK GLI SGLI'RCN(FLAG)
                            EXIT
                        ENDIF
                        \SLEEP 1
                    LOOP
                    BBB'CNT = 0
                    DO
                        RDLOCK BCH SBCH'RCN(FLAG)
                        IF BCH'CTL # 4 EXIT
                        BBB'CNT = BBB'CNT + 1
                        IF BBB'CNT > 1 THEN
                            RDLOCK BCH SBCH'RCN(FLAG)
                            EXIT
                        ENDIF
                        \SLEEP 1
                    LOOP
                ENDCASE
                CALL POST'TO'TRX'FROM'GLI
        LOOP UNTIL REPOST'TRANSFER = 0
RETURN

MAP1 REPOST'TRANSFER,B,1

POST'TO'TRX'FROM'GLI:
!!      CALL POST'TO'TRX'FROM'BCH
!!RETURN
POST'TO'TRX'FROM'BCH:
        LAST'FLAG = FLAG                        !!Save current type of batch
        IF XXX'KIT'BUILD # "Y" OR (FLAG # 1 AND HDR'PROC = 5) THEN
            CALL GEN'TRX'NO                         !!Generate new TRX'NO
            CALL ADD'TRX'RECORD
            IF TRX'CTL = 0 THEN
                \VICS'POST TRX ACTION="UPDATE'TRX" APID="IN"
            ENDIF
        ENDIF
        CASE
        IF HDR'PROC = 5 OR XXX'KIT'BUILD = "Y" THEN
!!              KTD'TTL'PRICE = 0                                                                               !![29]
!!              KTD'TTL'QTY = 0                                                                         !![29]
!!              KTD'TTL'COUNT = 0                                                                               !![29]
!!              \DOKITS OE="Y" AVAILABLE="N" PACTION="KTD'PRICING" &
!!                      OPR'ACTION="DUMMY'LABEL" PRH="PRH2" TS'ATT'NO1="TRX'ATT'NO1"
                CALL FIND'PRH                                                                                   !![29]
                MAP1 MASTER'PRICE,F
                MAP1 MASTER'DISC,F
                IF HDR'PROC >= 5 THEN
                    PRC'KTRK'NO = LIN'TRK'NO
                    PRC'KUOM'NO = LIN'UOM'NO
                    CALL KTD'USE'PRC
                    MASTER'PRICE = PRC'AMT
                    MASTER'DISC = 0
                ELSE
                    MASTER'PRICE = LIN'PRICE
                    MASTER'DISC = LIN'DISC
                ENDIF
                MASTER'PRICE = MASTER'PRICE/(10^PRH'PFRACTION)
                MASTER'DISC = MASTER'DISC/(10^PRH'PFRACTION)
!!              KTD'TTL'SNET = 0                                                                                !![29]
!!              KTD'TTL'SGROSS = 0                                                                      !![29]
                \DOKITS OE="Y" AVAILABLE="N" PACTION="KTD'SET'FINAL'TRX" &
                        OPR'ACTION="DUMMY'LABEL" PRH="PRH2" TS'ATT'NO1="TRX'ATT'NO1"
                CALL FIND'PRH           !![22]
                \\DUMLBL                !!Do not read qty record ...
        ELSE
        IF HDR'PROC = 6 OR TC'YN = "Y" THEN
                            !!tc'yn = "cuts"
                IF TRX'FTIME = 1 THEN
                        SAV'LOC'NO = LIN'LOC'NO
                        LIN'LOC'NO = HDR'LOC'NO
                        TRX'FTIME = 0
                        REPOST'TRANSFER = 1
                ELSE
                        LIN'LOC'NO = SAV'LOC'NO
                ENDIF                   
        ENDCASE
RETURN

ADD'TRX'RECORD:
        TRX'SPACE = SPACE
        TRX'GLI'YEAR = GLI'YEAR
        TRX'MODE = SET'GLI'MODE(FLAG)
        TRX'GLI'NO = GLI'NO
        TRX'BCH'NO = BCH'NO
        TRX'NO = TRX'KNO
        TRX'STAT = "P"                          !!POSTED!!!
        TRX'DATE = SDATE
        IF PRH'USE'SRL'YN # "N" THEN
                TRX'SRL'COUNT = 1
        ELSE
                TRX'SRL'COUNT = 0
        ENDIF
        TRX'GL'CNO = HDR'GL'CNO
        TRX'PROC = HDR'PROC
        TRX'HDR'LOC'NO = HDR'LOC'NO
        TRX'DOC'TYPE = HDR'DOC'TYPE
        TRX'HDR'NO = HDR'NO
        TRX'LIN'NO = LIN'NO
        IF SUB'FLAG # 0 THEN TRX'SUB'NO = SUB'NO        !!ELSE = 0
        TRX'SHIPMENT = NEW'SHIPMENT
!!      TRX'MSGNO = 0
        IF KITFLG = 1 THEN
            TRX'TPROC = 1            !! if Kit Return should not be zero
        ENDIF
        \SETKEY PRE="TRX" MODE="KEY" APID="IN"
        ADDL TRX FROM TRX'REC KEY TRX'KEY
        IF TRX'CTL # 0 THEN
                ENTRY = "BAD TRX ADD, DATA="+TRX'GLI'YEAR+"-"+TRX'GLI'NO+"-"+TRX'BCH'NO+"-"+TRX'NO
                \LINERR APID="OE"
                CALL DO'ERRL
                ENTRY = "BAD TRX'ADD, KEY="+TRX'KGLI'YEAR+"-"+TRX'KGLI'NO+"-"+TRX'KBCH'NO+"-"+TRX'KNO
                \LINERR APID="OE"
                GOTO DO'ERRL                            !!KLUDGE
        ENDIF
        IF FIRST'TRX'NO = -1 THEN FIRST'TRX'NO = TRX'NO
        LAST'TRX'NO = TRX'NO
        TRXD'RCN = TRX'RCN
        TRXD'KLOC'NO = 0
        TRXD'KTRK'NO = 0
        TRXD'KATT'NO1 = 0
        TRXD'KATT'NO2 = 0
        TRXD'KDATE = 0
        ADDKEY TRXD
RETURN

UPDATE'TRX:
        IF INC'FACE'YN = "Y" THEN
                TRX'INTERFACE = PRH'INTERFACE
        ELSE
                TRX'INTERFACE = 1                       !!NONE
        ENDIF
        TRX'LOC'NO = LIN'LOC'NO
        TRX'PLN'NO = PRH2'PLN'NO
        TRX'TRK'NO = LIN'TRK'NO
        IF SUB'FLAG = 0 THEN
                FOR I = 1 TO 4
                        TRX'ATT'NO(I) = 0
                NEXT I
                TRX'QFRACTION = LIN'QFRACTION
        ELSE
                FOR I = 1 TO 4
                        TRX'ATT'NO(I) = SUB'ATT'NO(I)
                NEXT I
                TRX'QFRACTION = SUB'QFRACTION
                IF TC'YN = "Y" AND TRX'FTIME = 0 THEN
                        TRX'ATT'NO(1) = ABS(TQTY3) 
                ENDIF
        ENDIF
        TRX'PFRACTION = LIN'PFRACTION
        TRX'CFRACTION = PRH'CFRACTION                   !![14]
        IF TRX'INTERFACE = 5 THEN
                IF LIN'UOM'NO # 1 THEN
                        ENTRY = "ERROR in UOM ("+LIN'UOM'NO+")! "
                        \LINERR APID="OE"
                        CALL DO'ERRL
                ENDIF
                LIN'UOM'NO = 1
        ENDIF
        TRX'UOM'NO = LIN'UOM'NO
!!      TRX'QTY = XXX'COM'QTY
!!I think that tests 2 and 4 (TRX'FTIME # 1) are not valid ...
!!I changed them to test < 0 instead of > 0 !![18]?
!!
        CASE
        IF FLAG = 1                                                     OR &
                (TRX'FTIME = 1 AND HDR'PROC = 6 AND XXX'COM'QTY > 0)    OR &
                (TRX'FTIME # 1 AND HDR'PROC = 6 AND XXX'COM'QTY < 0)    OR &
                (TRX'FTIME = 1 AND TC'YN = "Y"  AND XXX'COM'QTY > 0)    OR &
                (TRX'FTIME # 1 AND TC'YN = "Y"  AND XXX'COM'QTY < 0)    THEN
                                                !!Shipment
                IF TC'YN # "Y" THEN
                        TRX'SUNITS = XXX'COM'QTY
                ELSE
                        TRX'SUNITS = TQTY3
                ENDIF
                IF FLAG = 3 THEN
                        TRX'SRA'FLAG = "S"
                ENDIF
                IF PRH'INTERFACE # 4 THEN
                        TRX'SGROSS = LIN'PRICE
                        TRX'SNET   = LIN'PRICE-LIN'DISC
                ELSE
                        \FROUND ((LIN'CALC'PRICE*LIN'SQ'FEET*PFACTOR)/QFACTOR/9)/PFACTOR TRX'SGROSS     !![6]
                        \FROUND (((LIN'CALC'PRICE-LIN'CALC'DISC)*LIN'SQ'FEET*PFACTOR)/QFACTOR/9)/PFACTOR TRX'SNET       !![6]
                        TRX'EXT'AMT = TRX'SGROSS
                        \FROUND (TRX'SGROSS/(XXX'COM'QTY/QFACTOR)) TRX'SGROSS           !![6]
                        \FROUND (TRX'SNET/(XXX'COM'QTY/QFACTOR)) TRX'SNET               !![6] 
                ENDIF                   
        ELSE
        IF FLAG = 2 THEN
                                                !!Return
                TRX'TUNITS = -XXX'COM'QTY
                TRX'TGROSS = LIN'PRICE
                TRX'TNET = LIN'PRICE-LIN'DISC
!!LIN below does not work!
                CASE
                IF XXX'RPROC = 1 THEN
                        TRX'TPROC = 3
                ELSE
                IF XXX'RPROC = 2 THEN
                        TRX'TPROC = 1
                ELSE
                IF XXX'RPROC = 3 THEN
                        TRX'TPROC = 2
                ELSE
                IF XXX'RPROC = 0 THEN
                        XXX'RPROC = 2
                        TRX'TPROC = 1
                ELSE
                        TRX'TPROC = 5
                ENDCASE
        ELSE
                                                !!Shop order/Transfer
                IF TC'YN # "Y" THEN
                        TRX'OUNITS = XXX'COM'QTY
                ELSE
                        TRX'OUNITS = TQTY3
                ENDIF
                IF HDR'PROC = 5 THEN
                    TRX'OCOST = KIT'COST
                ENDIF
                TRX'SRA'FLAG = "R"
        ENDCASE

        CALL SET'FINAL'TRX
        IF PRH'USE'SRL'YN # "N" CALL PROC'SRL
RETURN

COPY    "FIXQTY[SA]"                    !!FIX'QTY:

SET'FINAL'TRX:
                    !!FIX'QTY should be first because it sets SRA'FLAG and
                    !!TRX'QTY which is needed for everything else.
        CALL FIX'QTY
        IF TRX'MODE # "O" OR TRX'SRA'FLAG # "R" OR HDR'PROC = 6 THEN
            QTY'LOC'NO = TRX'LOC'NO
            QTY'TRK'NO = TRX'TRK'NO
            QTY'ATT'NO1 = TRX'ATT'NO1
            QTY'ATT'NO2 = TRX'ATT'NO2
            CALL DO'SETCST
            \FACTORQTY MODE="C"
            \FROUND ((COST'QTY/CFACTOR)*100)*(UOM'NUNITS/UOM'DUNITS) COST'QTY
!!          \FROUND COST'QTY/QFACTOR TRX'RCOST
            TRX'CFRACTION = PRH'CFRACTION
            IF TRX'SRA'FLAG = "R" THEN            !!Receipt
                IF HDR'PROC = 6 THEN              !!Transfer (receipt portion)
                    TRX'RCOST = TRANSFER'COST'AMT
                ELSE
                    TRX'RCOST = COST'QTY
                ENDIF
            ENDIF
        ENDIF
MAP1 TRANSFER'COST'AMT,B,5
        IF TRX'SRA'FLAG = "R" AND HDR'PROC = 5 THEN
!!to get the cost on the receipt portion of a shop order
            FOR II = 1 TO 5
                TRX'COST'AMT(II) = KIT'COST
            NEXT II
        ELSE
            FOR II = 1 TO 5
                TRX'COST'AMT(II) = COST'QTY
            NEXT II
            IF HDR'PROC = 6 AND TRX'SRA'FLAG = "S" THEN
                TRANSFER'COST'AMT = COST'QTY  !!Transfer shipment portion
            ENDIF
        ENDIF
!![3]   TRX'WOF'NO = UKY'NO
        TRX'WOF'NO = UKY'NO             !![4]
        BCH'COUNT = BCH'COUNT + 1
        IF BCH'MODE = "O" THEN
!!
!!IF A TRANSFER, RECEIVING PORTION ...
!!
                IF TRX'SRA'FLAG = "R" AND PRH'LOT'YN = "Y" THEN 
                        CALL SET'LOT'KEY
                        \VICS'RDLOCK LOT APID="IN"
                        IF LOT'CTL = 0 THEN
                                \VICS'POST LOT ACTION="UPDATE'LOT" APID="IN"
                        ELSE
                                ENTRY = "Invalid "+INC'LOT'DESC+" on: "
                                \KERROR LOT MODE="K" APID="IN"
                                CALL DO'ERRL
                                ENTRY = "Check transaction: "
                                \KERROR TRX MODE="D" APID="IN"
                                CALL DO'ERRL
                        ENDIF
                ENDIF
        ENDIF
        UPDATE BCH SBCH'RCN(FLAG)
RETURN

\COPYF LOT IN

UPDATE'LOT:
        LOT'LOC'NO = TRX'LOC'NO
RETURN

SET'LOT'KEY:
        IF INC'DUP'LOTS # "N" THEN
                LOT'KKEY'TRK'NO = TRX'TRK'NO
        ELSE
                LOT'KKEY'TRK'NO = 0
        ENDIF
        \SETLOT PRE="QTY"               !!SET LOT'KNO
RETURN

MAP1 KTD'TTL'PRICE,F
MAP1 KTD'TTL'QTY,F
MAP1 KTD'TTL'COUNT,B,2
!!MAP1 KTD'TTL'SNET,F
!!MAP1 KTD'TTL'SGROSS,F
MAP1 WRK'AMT,F

KTD'USE'PRC:
    IF PRH'PC'BY'LOC = "Y" THEN
        PRC'KLOC'NO = LIN'LOC'NO
    ELSE
        PRC'KLOC'NO = 0
    ENDIF
    PRC'SPACE = SPACE
    USE PRC INTO PRC'REC KEY PRC'KEY
RETURN

!!KTD'PRICING:                                                                    !![29]
!!    SQTY = TQTY
!!    TQTY = (TQTY*DOKITS'THIS'MULT)/10^PRH'QFRACTION
!!    CALL KTD'USE'PRC
!!    WRK'AMT = TQTY/10^PRH'QFRACTION     !!Floating point value
!!    \FROUND (KTD'TTL'PRICE+(WRK'AMT*(PRC'AMT/10^PRH'PFRACTION))) KTD'TTL'PRICE
!!    KTD'TTL'QTY = KTD'TTL'QTY + WRK'AMT
!!    KTD'TTL'COUNT = KTD'TTL'COUNT + 1
!!    TQTY = SQTY
!!RETURN

MAP1 KITFLG,B,1
KTD'SET'FINAL'TRX:
        SQTY = TQTY
        TQTY = (TQTY*DOKITS'THIS'MULT)/10^PRH'QFRACTION
        IF FLAG = 2 THEN
            TQTY = TQTY * -1
            KITFLG = 1            !!Returned a Kit - fix TRX'TPROC below
        ELSE
            KITFLG = 0
        ENDIF
        CALL GEN'TRX'NO                         !!Generate new TRX'NO
        CALL ADD'TRX'RECORD
        \VICS'POST TRX ACTION="KTD'UPDATE'TRX" APID="IN"
        TQTY = SQTY
RETURN  

KTD'UPDATE'TRX:
        IF INC'FACE'YN = "Y" THEN
                TRX'INTERFACE = PRH2'INTERFACE
        ELSE
                TRX'INTERFACE = 1                       !!NONE
        ENDIF
        TRX'LOC'NO = LIN'LOC'NO
        TRX'PLN'NO = PRH2'PLN'NO
        TRX'TRK'NO = KTD'TRK'NO
        FOR I = 1 TO 4
                TRX'ATT'NO(I) = KTD'ATT'NO(I)
        NEXT I
        TRX'PFRACTION = PRH'PFRACTION
        TRX'QFRACTION = PRH'QFRACTION
        TRX'UOM'NO = KTD'UOM'NO
        TRX'SUNITS = TQTY
!![29] TRX'SGROSS = LIN'PRICE
!![29] TRX'SNET = LIN'PRICE-LIN'DISC
!![29] - below
    KTD'TTL'COUNT = KTD'TTL'COUNT - 1
!!  CASE
!!NOTE: you could round remainder if count=0 but it would never be right
!!      so you might as well not bother.  Below code is also wrong!
!!  IF KTD'TTL'COUNT = 0 THEN                                   !!Last item
!!      \FROUND ((MASTER'PRICE*(TQTY/10^PRH'QFRACTION))*100) WRK'AMT
!!      WRK'AMT = WRK'AMT/100
!!      \FROUND ((WRK'AMT-KTD'TTL'SGROSS)/(TQTY/10^PRH'QFRACTION))*(10^PRH'PFRACTION)*(10^PRH'QFRACTION) TRX'SGROSS
!!      \FROUND (((MASTER'PRICE-MASTER'DISC)*(TQTY/10^PRH'QFRACTION))*100) WRK'AMT
!!      WRK'AMT = WRK'AMT/100
!!      \FROUND ((WRK'AMT-KTD'TTL'SNET)/(TQTY/10^PRH'QFRACTION))*(10^PRH'PFRACTION)*(10^PRH'QFRACTION) TRX'SNET
!!  ELSE
    IF KTD'TTL'PRICE = 0 THEN
        IF KTD'TTL'QTY = 0 THEN
            TRX'SGROSS = LIN'PRICE
            TRX'SNET = LIN'PRICE-LIN'DISC

            IF KTD'TTL'COUNT = 1 THEN
                ENTRY = "Suspicious gross/net on "
                \KERROR LIN MODE="D" CNO="N"
                CALL DO'ERRL
            ENDIF
        ELSE
            WRK'AMT = TQTY/(10^PRH'QFRACTION)
            \FROUND (WRK'AMT/KTD'TTL'QTY)*MASTER'PRICE TRX'SGROSS
            \FROUND (WRK'AMT/KTD'TTL'QTY)*(MASTER'PRICE-MASTER'DISC) TRX'SNET
        ENDIF
    ELSE
        PRC'KTRK'NO = PRH2'TRK'NO
        PRC'KUOM'NO = KTD'UOM'NO
        CALL KTD'USE'PRC                !!get this kit piece price
        WRK'AMT = PRC'AMT/(10^PRH'PFRACTION)
        \FROUND ((MASTER'PRICE/KTD'TTL'PRICE)*WRK'AMT)*(10^PRH'PFRACTION) TRX'SGROSS
        \FROUND (((MASTER'PRICE-MASTER'DISC)/KTD'TTL'PRICE)*WRK'AMT)*(10^PRH'PFRACTION) TRX'SNET
    ENDIF
!!  ENDCASE
!!  \FROUND ((KTD'TTL'SGROSS+((TRX'SGROSS/10^PRH'PFRACTION)*(TQTY/10^PRH'QFRACTION)))*100) KTD'TTL'SGROSS
!!  KTD'TTL'SGROSS=KTD'TTL'SGROSS/100
!!  \FROUND ((KTD'TTL'SNET+((TRX'SNET/10^PRH'PFRACTION)*(TQTY/10^PRH'QFRACTION)))*100) KTD'TTL'SNET
!!  KTD'TTL'SNET=KTD'TTL'SNET/100
!![29] - above
    TRX'SRA'FLAG = "S"
    CALL SET'FINAL'TRX
RETURN  

GEN'TRX'NO:
        TRX'KGLI'YEAR = GLI'YEAR
        TRX'KGLI'NO = GLI'NO
        TRX'KBCH'NO = BCH'NO
        TRX'KNO = -1
        RDKEY TRX
        GETPRV TRX
        TRX'KEY = TRX'KEY2
        IF TRX'KGLI'YEAR = GLI'YEAR AND TRX'KGLI'NO = GLI'NO AND        &
                                TRX'KBCH'NO = BCH'NO AND TRX'CTL = 0 THEN
                TRX'KNO = TRX'KNO + 1
                IF TRX'KNO > 9999 THEN
                        WRITE BCH SBCH'RCN(FLAG)
                        CALL ADD'BCH
!![22] - below
                        IF BSCD'RCN # 0 THEN
                                RDLOCK BSC BSCD'RCN INTO BSC'REC
                                IF BSC'CTL = 0 THEN
                                        BSC'BCH'NO(FLAG) = BCH'NO
                                        WRITE BSC
                                ENDIF
                        ENDIF
!![22] - above
                        TRX'KNO = 1
                ENDIF
        ELSE
                TRX'KNO = 1
        ENDIF
RETURN

MAP1    BATCH'DTT,B,3                           !![1]
MAP1    ADDED'GLI,B,1
MAP1    ADDED'BCH,B,1
COPYF "BSCFIL[IN]",=BSC2,NOAFR

SETUP'BATCHES:                  !![19] - rewritten - see OEWRIT.OLD
        ADDED'GLI = 0
        ADDED'BCH = 0

        FIRST'GLI(FLAG) = 0                     !!Set flag
        BATCH'DTT = SDATE                       !![1]

        DTT = SDATE
        \FINDYR USE'ONLY="Y"            !!Finds GL'YEAR and GL'PERIOD

        BSC2'REC = BSC'REC

        IF BSC'GLI'YEAR(FLAG) = GL'YEAR THEN
                GLI'KYEAR = BSC'GLI'YEAR(FLAG)
                GLI'KNO = BSC'GLI'NO(FLAG)
                USE GLI INTO GLI'REC KEY GLI'KEY
                IF GLI'STAT # "U" OR GLI'PERIOD # GL'PERIOD THEN GLI'CTL = 3    !![10]
                IF GLI'TRY'POST # "" THEN GLI'CTL = 3           !![23]
                IF GLI'COUNT >= 9999 THEN GLI'CTL = 3           !![22]
                IF GLI'CTL = 0 THEN
                    GGG'CNT = 0
                    DO
                        RDLOCK GLI INTO GLI'REC KEY GLI'KEY
                        IF GLI'CTL # 4 EXIT
                        GGG'CNT = GGG'CNT + 1
                        IF GGG'CNT > 2 EXIT
                        \SLEEP 1
                    LOOP
                    IF GLI'STAT # "U" OR GLI'PERIOD # GL'PERIOD THEN
                        GLI'CTL = 3
                    ENDIF
                    IF GLI'TRY'POST # "" THEN GLI'CTL = 3           !![23]
                    IF GLI'COUNT >= 9999 THEN GLI'CTL = 3
                    IF GLI'CTL # 0 THEN
                        REL GLI
                        GLI'CTL = 3
                    ENDIF
                ENDIF
        ELSE
                GLI'KYEAR = GL'YEAR
                GLI'CTL = 3
        ENDIF

        IF GLI'CTL # 0 THEN                             !![10]
                FOR II = 1 TO 5
                        IF BSC'GLI'YEAR(II) = GL'YEAR AND II # FLAG AND &
                                        GLI'KNO # BSC'GLI'NO(II) THEN
                                GLI'KNO = BSC'GLI'NO(II)
                                USE GLI INTO GLI'REC KEY GLI'KEY
                                IF GLI'STAT # "U" OR GLI'PERIOD # GL'PERIOD THEN GLI'CTL = 3
                                IF GLI'TRY'POST # "" THEN GLI'CTL = 3           !![23]
                                IF GLI'COUNT >= 9999 THEN GLI'CTL = 3
                                IF GLI'CTL = 0 THEN
                                    GGG'CNT = 0
                                    DO
                                        RDLOCK GLI INTO GLI'REC KEY GLI'KEY
                                        IF GLI'CTL # 4 EXIT
                                        GGG'CNT = GGG'CNT + 1
                                        IF GGG'CNT > 2 EXIT
                                        \SLEEP 1
                                    LOOP
                                    IF GLI'STAT # "U" OR GLI'PERIOD # GL'PERIOD THEN GLI'CTL = 3
                                    IF GLI'TRY'POST # "" THEN GLI'CTL = 3           !![23]
                                    IF GLI'COUNT >= 9999 THEN GLI'CTL = 3
                                    IF GLI'CTL # 0 THEN
                                        REL GLI
                                        GLI'CTL = 3
                                    ELSE
                                        BSC'GLI'YEAR(FLAG) = BSC'GLI'YEAR(II)
                                        BSC'GLI'NO(FLAG) = BSC'GLI'NO(II)
                                        BSC'BCH'NO(FLAG) = 0
                                        II = 10
                                    ENDIF
                                ENDIF
                        ENDIF
                NEXT II
        ENDIF

        IF GLI'CTL # 0 THEN
                CALL ADD'GLI
                BCH'KGLI'YEAR = GLI'YEAR
                BCH'KGLI'NO = GLI'NO
                CALL ADD'BCH                    !!And add a batch
        ELSE
!!              RDLOCK GLI                      !!Lock
                SGLI'RCN(FLAG) = GLI'RCN        !!Set sav rcn
                BCH'KGLI'YEAR = GLI'YEAR
                BCH'KGLI'NO = GLI'NO
                IF BSC'BCH'NO(FLAG) # 0 THEN
                        BCH'KNO = BSC'BCH'NO(FLAG)
                        USE BCH INTO BCH'REC KEY BCH'KEY
                ELSE
                        BCH'CTL = 3
                ENDIF
!![22] - test count below
                IF BCH'STAT # "P" OR BCH'MODE # SET'GLI'MODE(FLAG)      OR &
                        BCH'COUNT >= 9999 OR BCH'SOURCE = "MF" THEN
                                                !!Add as needed
                        BCH'CTL = 3
                ENDIF
                IF BCH'CTL = 0 THEN
                    BBB'CNT = 0
                    DO
                        RDLOCK BCH INTO BCH'REC KEY BCH'KEY
                        IF BCH'CTL # 4 EXIT
                        BBB'CNT = BBB'CNT + 1
                        IF BBB'CNT > 1 THEN
                            RDLOCK BCH INTO BCH'REC KEY BCH'KEY
                            EXIT
                        ENDIF
                        \SLEEP 1
                    LOOP
                    IF BCH'STAT # "P" OR BCH'MODE # SET'GLI'MODE(FLAG) OR &
                                BCH'COUNT >= 9999 OR BCH'CTL # 0 THEN
                                                        !!Add as needed
                        REL BCH
                        BCH'CTL = 3
                    ENDIF
                ENDIF
MAP1 BBB'CNT,B,2
                IF BCH'CTL # 0 THEN
                        BCH'KNO = 1
                        BCH8'KEY = BCH'KEY
                        USE BCH8 INTO BCH'REC KEY BCH8'KEY
                        DO
                            GETNXT BCH8 INTO BCH'REC
                            \CHKKEY BCH APID="IN" ALTKEY="BCH8"
                            IF BCH8'CTL # 0 EXIT
                            IF BCH'STAT # "P" OR BCH'MODE # SET'GLI'MODE(FLAG)      OR &
                                            BCH'COUNT >= 9999 OR BCH'SOURCE = "MF" THEN
                                REPEAT
                            ENDIF
                            BBB'CNT = 0
                            DO
                                RDLOCK BCH BCH8'RCN INTO BCH'REC
                                IF BCH'CTL # 4 EXIT
                                BBB'CNT = BBB'CNT + 1
                                IF BBB'CNT > 1 THEN
!!Try again w/out sleep
                                    RDLOCK BCH BCH8'RCN INTO BCH'REC
                                    EXIT
                                ENDIF
                                \SLEEP 1
                            LOOP
                            IF BCH'STAT # "P" OR BCH'MODE # SET'GLI'MODE(FLAG)OR &
                                    BCH'COUNT >= 9999 OR BCH'SOURCE = "MF" OR &
                                    BCH'CTL # 0 THEN
                                REL BCH
                                BCH'CTL = 3
                            ENDIF
                        LOOP
                        IF BCH'CTL # 0 THEN
                            CALL ADD'BCH
                        ELSE
                            SBCH'RCN(FLAG) = BCH'RCN
                        ENDIF
                ELSE
!!                      RDLOCK BCH              !!lock
                        SBCH'RCN(FLAG) = BCH'RCN
                ENDIF
        ENDIF
!!
        IF (BCH'GLI'YEAR # BSC'GLI'YEAR(FLAG) OR BCH'GLI'NO # BSC'GLI'NO(FLAG)  &
                        OR BCH'NO # BSC'BCH'NO(FLAG)) AND BSCD'CTL = 0 THEN
                BSC'SPACE = SPACE
                RDLOCK BSC BSCD'RCN INTO BSC'REC
                IF (BSC2'GLI'YEAR(FLAG) # BSC'GLI'YEAR(FLAG)            OR &
                                BSC2'GLI'NO(FLAG) # BSC'GLI'NO(FLAG)    OR &
                                BSC2'BCH'NO(FLAG) # BSC'BCH'NO(FLAG)) THEN
                        REL BSC
                        IF ADDED'BCH = 1 THEN
                                \DELKEY PRE="BCH" MODE="ALL" APID="IN"
                                DELETE BCH
                                IF ADDED'GLI = 0 THEN
                                        GLI'COUNT = GLI'COUNT - 1
                                        UPDATE GLI
                                ENDIF
                        ELSE
                                REL BCH
                        ENDIF
                        IF ADDED'GLI = 1 THEN
                                \DELKEY PRE="GLI" MODE="ALL" APID="IN"
                                DELETE GLI
                        ELSE
                                REL GLI
                        ENDIF
                        ENTRY = "RELOOPING THRU SETUP'BATCHES "+BSC'GLI'YEAR(FLAG)+"-"+BSC'GLI'NO(FLAG)+"-"+BSC'BCH'NO(FLAG)+" "+BSC2'GLI'YEAR(FLAG)+"-"+BSC2'GLI'NO(FLAG)+"-"+BSC2'BCH'NO(FLAG)
                        CALL DO'ERRLO
                        DTT = GLI'LSTDT
                        ENTRY = "ADDED'BCH "+ADDED'BCH+" ADDED'GLI "+ADDED'GLI+" GLI'KEY "+GLI'KYEAR+"-"+GLI'KNO+" CTL "+GLI'CTL+" DATED "+DMN+"/"+DDY+"/"+DYR
                        CALL DO'ERRLO
                        GOTO SETUP'BATCHES              !!KLUDGE
                ENDIF
                BSC'GLI'YEAR(FLAG) = BCH'GLI'YEAR
                BSC'GLI'NO(FLAG) = BCH'GLI'NO
                BSC'BCH'NO(FLAG) = BCH'NO
                WRITE BSC BSCD'RCN FROM BSC'REC         !!Update schedule
        ENDIF
RETURN

ADD'GLI:
        ADDED'GLI = 1
        GLI'KNO = -1
        USE GLI INTO GLI'REC KEY GLI'KEY
        GETPRV GLI INTO GLI'REC
        \CHKKEY PRE="GLI" APID="IN"
        IF GLI'CTL # 0 THEN
                GLI'KNO = 1
        ELSE
                IF GLI'NO >= 9999 THEN
                        GLI'KYEAR = GLI'KYEAR + 1
                        ENTRY = "Year incremented to "+GLI'KYEAR
                        \HDRERR APID="OE"
                        CALL DO'ERRL
!!If to many GLI batches, in desperation add to the year
GOTO ABORT
                        GOTO ADD'GLI                    !!KLUDGE
                ELSE
                        GLI'KNO = GLI'NO + 1
                ENDIF
        ENDIF
        GLI'SPACE = SPACE
        GLI'YEAR = GLI'KYEAR
        GLI'NO = GLI'KNO
        GLI'DESC = BSC'DESC + " auto added"
        GLI'CRID = "OEPQTY"
        GLI'CRDT = DATE
        GLI'LSTID = UB'UID
        GLI'LSTDT = DATE
        GLI'STAT = "U"
!!      GLI'COUNT = 0
!!      GLI'PSTID = ""
!!      GLI'PSTDT = 0
        GLI'PQU'STAT = "U"
        IF GL'PERIOD < 1 OR GL'PERIOD > 13 THEN 
                ENTRY = "Invalid period on posting - check IN posting schedule."
                \HDRERR PRE="HDR" APID="OE"
                ENTRY = ENTRY + ", set to 13"
                CALL DO'ERRL
                GL'PERIOD = 13
        ENDIF
        GLI'PERIOD = GL'PERIOD  
!!      GLI'GLH'BATCH = 0
        GLI'LIFO'BCD = INC'LIFO'BCD
        GLI'FIFO'BCD = INC'FIFO'BCD
        GLI'AVG'BCD = INC'AVG'BCD
        GLI'FAIR'BCD = INC'FAIR'BCD
        GLI'STD'BCD = INC'STD'BCD
!!      GLI'MSGNO = 0
        DO
                ADDL GLI FROM GLI'REC KEY GLI'KEY
                IF GLI'CTL = 0 EXIT
                IF GLI'NO >= 9999 THEN
                        GLI'KYEAR = GLI'KYEAR + 1
                        ENTRY = "Year incremented to "+GLI'KYEAR
                        \HDRERR APID="OE"
                        CALL DO'ERRL
!!If to many GLI batches, in desperation add to the year
                        REL GLI
                        GOTO ADD'GLI                    !!KLUDGE
                ENDIF
                GLI'NO = GLI'NO + 1
                GLI'KNO = GLI'NO
        LOOP
        SGLI'RCN(FLAG) = GLI'RCN                        !!set save rcn
        \SETKEY PRE="GLI" MODE="KY2" APID="IN"
        \ADDKEY PRE="GLI" MODE="KY2" APID="IN"
RETURN

ADD'BCH:
        ADDED'BCH = 1
        BCH'KNO = -1
        USE BCH INTO BCH'REC KEY BCH'KEY
        GETPRV BCH INTO BCH'REC
        \CHKKEY PRE="BCH" APID="IN"
        IF BCH'CTL # 0 THEN
                BCH'KNO = 1
        ELSE
                IF BCH'NO >= 9999 THEN
                        WRITE GLI SGLI'RCN(FLAG)
!!If to many batches in GLI just add a new GLI
                        CALL ADD'GLI
!![22] - below
                        IF BSCD'RCN # 0 THEN
                                RDLOCK BSC BSCD'RCN INTO BSC'REC
                                IF BSC'CTL = 0 THEN
                                        BSC'GLI'NO(FLAG) = GLI'NO
                                        WRITE BSC
                                ENDIF
                        ENDIF
!![22] - above
!!And then a new batch
                        GOTO ADD'BCH                    !!KLUDGE
                ELSE
                        BCH'KNO = BCH'NO + 1
                ENDIF
        ENDIF
        BCH'SPACE = SPACE
        BCH'GLI'YEAR = BCH'KGLI'YEAR
        BCH'GLI'NO = BCH'KGLI'NO
        BCH'NO = BCH'KNO
        BCH'DESC = BSC'DESC + " auto added"
        BCH'BSC'NO = BSC'NO
        BCH'CRID = "OEPQTY"
        BCH'CRDT = DATE
        BCH'LSTID = UB'UID
        BCH'LSTDT = DATE
        BCH'STAT = "P"
        BCH'COUNT = 0
        BCH'TYPE = "I"
        BCH'MODE = SET'GLI'MODE(FLAG)
        BCH'SOURCE = "OE"
        BCH'PSTID = "OEPQTY"
        BCH'PSTDT = DATE
!!      BCH'PQU'STAT = ""
!!      BCH'MSGNO = 0
        DO
                ADDL BCH FROM BCH'REC KEY BCH'KEY
                IF BCH'CTL = 0 EXIT
                IF BCH'NO >= 9999 AND BCH'CTL # 0 THEN
                        WRITE GLI SGLI'RCN(FLAG)
!!If to many batches in GLI just add a new GLI
                        REL BCH
                        CALL ADD'GLI
!!And then a new batch
                        GOTO ADD'BCH                    !!KLUDGE
                ENDIF
                BCH'NO = BCH'NO + 1
                BCH'KNO = BCH'NO
        LOOP
        SBCH'RCN(FLAG) = BCH'RCN                        !!Save rcn
        \SETKEY PRE="BCH" MODE="KY2" APID="IN"
        \ADDKEY PRE="BCH" MODE="KY2" APID="IN"
        GLI'COUNT = GLI'COUNT + 1
        UPDATE GLI SGLI'RCN(FLAG)
RETURN

FIND'AND'WRITE'HST:
        IF HDR'PROC >= 5 RETURN
        HST'KYEAR = GL'YEAR
        HST'KCUS'NO = HDR'NAM'NO
        IF ARC'HST'SEC'YN # "N" THEN
                HST'KSEC'NO = PRH'SEC'NO
                HST'KTRK'NO = 0
                HST'KTYPE = "S"
                CALL POST'HST
        ENDIF
        IF ARC'HST'PRH'YN # "N" THEN
                HST'KSEC'NO = 0
                HST'KTRK'NO = TRX'TRK'NO
                HST'KTYPE = "P"
                CALL POST'HST
        ENDIF
RETURN

POST'HST:
        \VICS'RDLOCK HST APID="AR" ACTION="ADD'HST" V''ADDL="Y" !![28]
        \VICS'POST HST APID="AR" ACTION="UPDATE'HST"
RETURN

UPDATE'HST:
        COPY "TTLHST[SA]",PERIOD="GL'PERIOD" USE'TRX="Y"
RETURN

ADD'HST:
        HST'SPACE = SPACE
        HST'YEAR = HST'KYEAR
        HST'CUS'NO = HST'KCUS'NO
        HST'SEC'NO = HST'KSEC'NO
        HST'TRK'NO = HST'KTRK'NO
        HST'TYPE = HST'KTYPE
        HST'QFRACTION = PRH'QFRACTION
        ADDL HST FROM HST'REC KEY HST'KEY                       !![28]
        \SETKEY PRE="HST" MODE="KY2" APID="AR"
        \ADDKEY PRE="HST" MODE="KY2" APID="AR"
RETURN

SET'SUB'REC:
        SUB'SPACE = SPACE
        SUB'DOC'TYPE = LIN'DOC'TYPE
        SUB'HDR'LOC'NO = LIN'HDR'LOC'NO
        SUB'HDR'NO = LIN'HDR'NO
        SUB'LIN'NO = LIN'NO
        \SETSUB
RETURN

\COPYF MOV WL
\COPYF IN2 IN

SF'NO'WL:
    SF = 1
    IF IN2'RCN # UB'CNO THEN
        USE IN2 UB'CNO INTO IN2'REC
        IF IN2'CTL # 0 THEN
            ENTRY = "Cannot find IN2 for "+UB'CNO
            CALL DO'ERRL
            GOTO ABORT
        ENDIF
    ENDIF
    IF IN2'WL'YN # "Y" RETURN        !!KLUDGE
    IF WLC'RCN # UB'CNO THEN
        \READCTL APID="WL" GL="N"
    ENDIF
    IF WLC'USE'VFH'YN = "Y" RETURN    !!KLUDGE
    SF = 0
RETURN

ADD'MOV'REC:
    IF SGN(NPQTY) = -1 AND ARC'INV'CR'YN(HDR'DOC'TYPE) = "Y" THEN
        RETURN                    !!KLUDGE
    ENDIF
    CALL SF'NO'WL
    IF SF = 0 THEN
        IF HDR'BULK = "Y" AND QTY'BULK = "Y" THEN
            CALL POST'BPF
        ELSE
            IF SUB'RPROC = 2 AND NPQTY < 0 THEN
                CALL ADD'TO'RIP
            ELSE
                CALL ADD'MOV'REC2
            ENDIF
        ENDIF
    ENDIF
RETURN

ADD'MOV'REC2:
    IF NPQTY = 0 RETURN              !!KLUDGE
    MOV'SPACE = SPACE
!!  MOV'MVH'NO = 0
    MOV'LOC'NO = QTY'LOC'NO
    MOV'TRK'NO = QTY'TRK'NO
    MOV'ATT'NO1 = QTY'ATT'NO1
    MOV'ATT'NO2 = QTY'ATT'NO2
    \SETKEY MOV APID="WL" MODE="KEY"
    ADDL MOV FROM MOV'REC KEY MOV'KEY
    IF MOV'CTL # 0 THEN
        RDLOCK MOV INTO MOV'REC KEY MOV'KEY
        IF MOV'CTL # 0 THEN
            ENTRY = "Cannot get MOV: "
            \KERROR MOV MODE="K" APID="WL" CNO="N"
            CALL DO'ERRL
        ENDIF
        \SETKEY MOV APID="WL" MODE="KY2"
        IF MOV'STAT # "U" THEN
            ENTRY = "Invalid "+MOV'STAT+" status on: "
            \KERROR MOV CNO="N" APID="WL"
            CALL DO'ERR
        ENDIF
    ELSE
        MOV'STAT = "U"
        \SETKEY MOV APID="WL" MODE="KY2"
        \ADDKEY MOV APID="WL" MODE="KY2"
    ENDIF
    MOV'PQTY = QTY'PQTY
    MOV'COM'QTY = MOV'COM'QTY + NPQTY
    \UPDKEY MOV APID="WL" MODE="KY2"
    WRITE MOV FROM MOV'REC
RETURN

COPY "UPDPHS[IN]",JOURNAL="Y",FINDEM="N"

\COPYF BPF WL
COPYF "BPFKEY[WL]",=BPF2

LCL1 BB,F
LCL1 QREMAIN,B,5
LCL1 TQ,B,5
LCL1 II,F
LCL1 L,F
\COPYF RIP WL
LCL1 TOP,D
LCL1 BTM,F

POST'BPF:
    BPF2'KLOC'NO = LIN'HDR'LOC'NO
    BPF2'KDOC'TYPE = LIN'DOC'TYPE
    BPF2'KHDR'NO = LIN'HDR'NO
    BPF2'KSHIPMENT = LIN'SHIPMENT + 1
    BPF2'KLIN'NO = LIN'NO
    BPF2'KSIZE = 0                        !!not t-shirt
    BPF2'KCGE'NO = 0
    BPF2'KASL'NO = 0
    BPF2'KSCT'NO = 0
    BPF2'KBIN'NO = 0
    USE BPF2 INTO BPF'REC KEY BPF2'KEY
    QREMAIN = TQTY
    DO
        GETNXT BPF2 INTO BPF'REC
        \CHKKEY BPF APID="WL" ALTKEY="BPF2" LASTFIX="SIZE"
        IF BPF2'CTL # 0 EXIT
        RDLOCK BPF BPF2'RCN INTO BPF'REC
        IF BPF'CTL = 0 AND BPF'TRK'NO = QTY'TRK'NO AND &
                                BPF'ATT'NO1 = QTY'ATT'NO1 AND &
                                        BPF'ATT'NO2 = QTY'ATT'NO2 AND &
                                                        QREMAIN > 0 THEN
            RIP'KLOC'NO = BPF'LOC'NO
            RIP'KCGE'NO = BPF'CGE'NO
            RIP'KASL'NO = BPF'ASL'NO
            RIP'KSCT'NO = BPF'SCT'NO
            RIP'KBIN'NO = BPF'BIN'NO
            RDLOCK RIP INTO RIP'REC KEY RIP'KEY
            IF RIP'CTL = 0 THEN
                TOP = 16 : BTM = 1
                FOR BB = 1 TO 16
                    IF RIP'TRK'NO(BB) = QTY'TRK'NO AND &
                                            RIP'ATT'NO(BB) = QTY'ATT'NO1 THEN
                        TQ = RIP'QTY(BB) MIN QREMAIN
                        IF TQ/RIP'PPC(BB) = INT(TQ/RIP'PPC(BB)) AND &
                                                    TQ >= RIP'QTY(BB) THEN
                            TOP = BB
                            BTM = BB
                            BB = 101
                        ENDIF
                    ENDIF
                NEXT BB
                IF TOP # BTM THEN
                    FOR BB = 1 TO 16
                        IF RIP'TRK'NO(BB) = QTY'TRK'NO AND &
                                            RIP'ATT'NO(BB) = QTY'ATT'NO1 THEN
                            TQ = RIP'QTY(BB) MIN QREMAIN
                            IF TQ >= RIP'QTY(BB) THEN
                                TOP = BB
                                BTM = BB
                                BB = 101
                            ENDIF
                        ENDIF
                    NEXT BB
                ENDIF
                FOR BB = BTM TO TOP
                    IF RIP'TRK'NO(BB) = QTY'TRK'NO AND &
                                            RIP'ATT'NO(BB) = QTY'ATT'NO1 THEN
                        TQ = RIP'QTY(BB) MIN QREMAIN
                        RIP'QTY(BB) = RIP'QTY(BB) - TQ
                        QREMAIN = QREMAIN - TQ
                        IF RIP'QTY(BB) = 0 AND RIP'PERM(BB) # "Y" THEN
                            RIP'TRK'NO(BB) = 0
                        ENDIF
                        IF RIP'TRK'NO(BB) = 0 THEN
                            RIP'PERM(BB) = "N"
                            RIP'QTY(BB) = 0
                            RIP'PPC(BB) = 0
                            RIP'ATT'NO(BB) = 0
                        ENDIF
                    ENDIF
                    IF QREMAIN <= 0 THEN BB = 101
                NEXT BB
             KLUDGE'MOVE:
                II = 0
                FOR L = 2 TO 16
                    IF RIP'TRK'NO(L) # 0 AND RIP'TRK'NO(L-1) = 0 THEN
                        IF II = 0 THEN II = L - 1
                    ENDIF
                NEXT L
                IF II # 0 THEN
                    FOR L = II TO 15
                        RIP'TRK'NO(L) = RIP'TRK'NO(L+1)
                        RIP'PPC(L) = RIP'PPC(L+1)
                        RIP'QTY(L) = RIP'QTY(L+1)
                        RIP'ATT'NO(L) = RIP'ATT'NO(L+1)
                        RIP'PERM(L) = RIP'PERM(L+1)
                     NEXT L
                     RIP'TRK'NO(16) = 0
                     RIP'PPC(16) = 0
                     RIP'QTY(16) = 0
                     RIP'ATT'NO(16) = 0
                     GOTO KLUDGE'MOVE
                ENDIF
                WRITE RIP FROM RIP'REC
            ENDIF
        ENDIF
        IF BPF'CTL = 0 THEN
            \SETKEY BPF APID="WL" MODE="ALL"
            \DELKEY BPF APID="WL" MODE="ALL"
            DELETE BPF
        ENDIF
    LOOP
RETURN

\COPYF BPF WL
\COPYF BPF WL BPF2 MODE="PRI"

DEL'BPF:
    CALL SF'NO'WL
    IF SF = 0 THEN
        BPF'SPACE = SPACE
        BPF'LOC'NO = HDR'LOC'NO
        BPF'DOC'TYPE = HDR'DOC'TYPE
        BPF'HDR'NO  = HDR'NO
        BPF'SHIPMENT = HDR'SHIPMENT
        \SETKEY BPF MODE="KEY" APID="WL" ALTKEY="BPF2"
        USE BPF2 INTO BPF'REC KEY BPF2'KEY
        DO
            GETNXT BPF2 INTO BPF'REC
            \CHKKEY BPF APID="WL" ALTKEY="BPF2" LASTFIX="SHIPMENT"
            IF BPF2'CTL # 0 EXIT
            RDLOCK BPF BPF2'RCN INTO BPF'REC
            IF BPF'CTL = 0 THEN
                \SETKEY BPF MODE="ALL" APID="WL"
                \DELKEY BPF MODE="ALL" APID="WL"
                DELETE BPF
            ENDIF
        LOOP
    ENDIF
RETURN

ADD'TO'RIP:
    RIP'KLOC'NO = LIN'LOC'NO
    RIP'KCGE'NO = QTY'CGE'NO(1)
    RIP'KASL'NO = QTY'ASL'NO(1)
    RIP'KSCT'NO = QTY'SCT'NO(1)
    RIP'KBIN'NO = QTY'BIN'NO(1)
    RDLOCK RIP INTO RIP'REC KEY RIP'KEY
    IF QTY'CGE'NO(1) # 0 AND QTY'ASL'NO(1) # 0 AND QTY'SCT'NO(1) # 0 AND &
            QTY'BIN'NO(1) # 0 AND RIP'CTL # 0 THEN
        RIP'SPACE = SPACE
        RIP'LOC'NO = RIP'KLOC'NO
        RIP'CGE'NO = RIP'KCGE'NO
        RIP'ASL'NO = RIP'KASL'NO
        RIP'SCT'NO = RIP'KSCT'NO
        RIP'BIN'NO = RIP'KBIN'NO
        \DSPBIN RIP'DSPBIN RIP SORT="Y"
        ADDL RIP FROM RIP'REC KEY RIP'KEY
        IF RIP'CTL = 0 THEN
            \SETKEY RIP MODE="KY2" APID="WL" BAD1="RIPT"
            \ADDKEY RIP MODE="KY2" APID="WL" BAD1="RIPT"
            RIP'CTL = 0
        ENDIF
    ENDIF
    IF RIP'CTL = 0 THEN
        FOR BB = 1 TO 16
            IF RIP'TRK'NO(BB) = QTY'TRK'NO AND &
                        RIP'ATT'NO(BB) = QTY'ATT'NO1 THEN
!!TQTY is negative as this is a return
                RIP'QTY(BB) = RIP'QTY(BB) - TQTY
                BB = 101
                WRITE RIP FROM RIP'REC
            ENDIF
        NEXT BB
        IF BB < 100 THEN
            FOR BB = 1 TO 16
                IF RIP'TRK'NO(BB) = 0 THEN
                    RIP'TRK'NO(BB) = QTY'TRK'NO
                    RIP'QTY(BB) = RIP'QTY(BB) - TQTY
                    RIP'PERM(BB) = "Y"
                    MEA'SPACE = SPACE
                    MEA'KTRK'NO = QTY'TRK'NO
                    MEA'KUOM'NO = LIN'UOM'NO
                    \VUSE MEA
                    IF MEA'CTL = 0 THEN
                        RIP'PPC(BB) = MEA'PPC
                    ELSE
                        RIP'PPC(BB) = 1
                    ENDIF
                    \SETKEY RIPT MODE="KEY" APID="WL"
                    RIPT'KTRK'NO1 = RIP'TRK'NO(BB)
                    RIPT'KATT'NO1 = RIP'ATT'NO(BB)
                    ADDKEY RIPT
                    BB = 101
                    WRITE RIP FROM RIP'REC
                ENDIF
            NEXT BB
            IF BB < 100 THEN
                ENTRY = "1: Could not find RIP location to put away inventory!"
                \KERROR RIP MODE="K" CNO="N" APID="WL"
                CALL DO'ERRL
            ENDIF
        ENDIF
    ELSE
        ENTRY = "2: Could not find RIP location to put away inventory!"
        \KERROR RIP MODE="K" CNO="N" APID="WL"
        CALL DO'ERRL
    ENDIF
RETURN
