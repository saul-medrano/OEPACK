! @#$^&* 9 0 938 0 0 0 0 0 0 0 0 0 0 *&^$#@ 
!* Updated on 02-Oct-95 at 4:41 PM by Cheryl Eberhard; edit time: 5:51:54
!!SUBWGA,8.0(8)
!!VUE-like Transactional copy file for SUBFIL 
!!              Single file File I/O Module for full data entry
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
!!           2933 W Middle Verde Rd
!!              Camp Verde, AZ  86322
!!               (928) 567-3727
!!
!!              Vertical Integrated Computer Systems, Camp Verde, AZ  USA
!!              All rights reserved.
!!
!!
!!     Model program VUESUB written by:
!!
!!              Eric S. Eberhard and Kenneth J. Shook
!!              May 2, 1985 - June 30, 1985, and on and on and on ...
!!
!!
!!      Copied from VUETRX.MOD by ESE & KJS
!!
!!      Revision History:
!!
!!7.2   11/30/88 ASC    New version created       
!!
!![1] 11/02/89 ESE      See OE1HDR.BV                   
!!
!![2] 11/06/89 ESE      Fix lot number display.
!!
!![3] 03/06/90 KJS      Fixed occasional problem where square yards would
!!                      display when not appropriate.
!!
!![4] 11/14/90 HLB      Would not allow backorder entry due to mixup in
!!                      use of OE1WGS.S and OE2WGS.S
!!
!!
!![5] 04/05/91 KJS      Added OEC'SKIP'UP flag (OE1WGA)
!!
!![6] 12/04/91 ESE      Remove unneeded code.
!!
!![7] 06/05/92 HLB      Corrected a subscript out of range error.
!!                      Could cause it by entering a , in the order
!!                      quantity field and pressing escape.
!!
!!7.4   09/30/95 CAE    New version created
!!
!!8.0  03/31/97 CAE     New version created
!!
!![8]  03/31/97 CAE     NEW: Changes for year 2000.
!!
!!
!!      Search for '!!MODIFY:' after initial generation!!
!!      Modify other things at your own risk.
!!      Watch out if you mess with cursor positioning.  (Veeeeery Tricky)
!!
!!
!!Symbol Definitions
!!       SUBSUB                 - Program name
!!       Sub Line Items                 - Key field one prompt
!!       SUB            - DB prefix
!!       SUBFIL.DBD     - AMOS file name of main data file
!!       Sub Line Items                 - File description
!!       SUB'HDR'LOC'NO                 - Main key field one
!!       Y              - If yes, company controlled file
!!       OESUB          - Full DBID
!!
!!File Definitions
        COPYF   "SUBFIL"                        !!Data file name
        COPYF   "SUBFIL",=ZZZ|NM|,NOAFR !!Duplicate for read/write
        COPYF   "SUBKEY"                        !!Primary key file name
        COPYF   "WGAFIL"
        COPYF   "WGAFIL",=ZZZW,NOAFR

!!MODIFY: Other COPYF's , map type COPY's, and MAP statements go here.

        MAP1    SUB'II,F
        MAP1    SET'XXX,B,1
        MAP1    DZPC(6),S,6
        MAP1    BO(6),S,1
        MAP1    SAFR3,S,50
        MAP1    SAV'ORDER'QTY,F         !!MUST BE FLOATING - SEE EXPLODE
        MAP1    SAV'USR'OQTY,F          !!DITTO
        MAP1    SAV'COM'QTY,B,5
        MAP1    SUBWIN'EDT,X,520        !!07/31/89 KJS  
        MAP1    LAST'SUB'NO,B,2
        LCL1    SUB'FTIME,B,1
    MAP1        VV,F
        MAP1    LAST'GAFR'II,B,1
        MAP1    FIRST'GAFR'II,B,1

        MAP1 DP'SPACE,S
                MAP2 DP'REC
                        MAP3 DOZENS(6),B,5
                        MAP3 PIECES(6),B,5
                        MAP3 PQTY(6),B,5        !!Pre-built qty

        MAP1 WGA'TITLES'REC
                MAP2 WGA'TITLES'SPACE,S
                        MAP3 WGA'TITLES(6),S,7
        MAP1 WGA2'TITLES'REC
                        MAP2 WGA2'TITLES(6),S,7
        MAP1 WGA'AFR(6),S,1

INIT|NM|:                               !! Called once for each page

        TLR(|NM|) = 13                  !!MODIFY: Physical top row of edit area
        TLC(|NM|) = 3                   !!MODIFY: Physical top col of edit area
        BRR(|NM|) = 13                  !!MODIFY: Physical btm row of edit area
        BRC(|NM|) = 78                  !!MODIFY: Physical btm col of edit area 
        TSCR(|NM|) = 15                 !!MODIFY: Physical top of display
        BSCR(|NM|) = 23                 !!MODIFY: Physical bottom of display
        SSCR(|NM|) = BSCR(|NM|) - TSCR(|NM|)    ! Size of screen - 1
        MSCR(|NM|) = INT(SSCR(|NM|)/2)  ! Middle of screen
        LST'GKD(|NM|) = 1               !!MODIFY: Last GKD field number
        GKD'START'FLD(|NM|)= 1          !!MODIFY: Field number to start on in GKD
RETURN

PRE'SET'UP|NM|:                         ! Called at EVERY start of current page
        !!MODIFY: set the following 4 variables as needed
  
        FIRST'TIME'PG(3) = 0            !Fake out totalling
        REAL'PG = PG

        GKD'FLG(|NM|) = 1               ! Will cause gkd to be skipped during
!!                                      ! Mode after initial return
        IF ARCHIVE'FLAG = "S" THEN
                ENTRY = "Cannot enter sub level with this archive settting!"
                CALL DO'ERR
                GOTO ABORT
        ENDIF
        IF IN'FLAG # 0 OR CMN'SAV'RCN(3) = 0 OR CMN'SAV'RCN(2) # TMP'RCN(2) THEN
                RPT'FLG(|NM|) = 2
        ELSE
                RPT'FLG(|NM|) = 5
                RPT'RCN(|NM|) = 2
                TMP'RCN(|NM|) = CMN'SAV'RCN(3)
        ENDIF
        IF ARCHIVE'FLAG # "C" THEN FIRST'TIME'PG(|NM|) = 0      !!STOP TOTALING

                                        ! Set to 2 for data entry program
                                        ! that starts in change mode/HOME
                                        ! Set to 3 for report (like 2) except 
                                        ! the VDSP starts empty (clear)
                                        ! Set to 4 for data entry (like 2) except 
                                        ! the VDSP starts empty (clear)
                                        ! Set to 5 to call CENTER'LINE
                                        ! (Used for inter-program chaining
                                        !  when RCN is pre-set)

!!      MAN'HOME(|NM|) = 0              ! Set to 4 to disable automatic add 
                                        ! when batch is empty (automatically
                                        ! set to 4 if RPT'FLG(|NM|)=4).
                                        ! Set=1 when RPT'FLG(|NM|)=4 to enable 
                                        ! Auto add feature ... silly?
                                        ! E.G.:  if 4 then nothing will
                                        ! EVER fill screen until you do
                                        ! as in RPT'FLG(|NM|) = 4

!!      RPT'RCN(|NM|) = 0               ! Set to 1 to have this program
                                        ! return a RCN in this field when
                                        ! [RETURN] is pressed by user. (RPT = 1)
                                        ! (NOTE:  generate w/VUERPT.MOD if
                                        ! always used as a report screen.
                                        ! This may be used to change on the
                                        ! fly the screen behavior)
!!      TOGGLE'BTM(|NM|) = 1            ! Put this line to have bottom
                                        ! blank on entry into program

        \FACTORQTY MODE="Q" PRE="LIN" MASKSET="2"
        \FACTORQTY MODE="P" PRE="LIN" MASKSET="2"


!!      DFLT'SCHEMA(|NM|) = ##          ! Set to default schema number

!!      DFLT'HEADER(|NM|) = ##          ! Set to default schema header number


        XXX'SPACE = SPACE
        LAST'SUB'NO = 0
MAP1 MAX'SUB'NO,B,1
MAP1 MIN'SUB'NO,B,1
        MIN'SUB'NO = 0
        IF PRH'ATT'SPACE = SPACE THEN
                ATT'KATH'NO = PRH'ATH'NO1
                ATT'KNO = -1
                RDKEY ATT
                GETPRV ATT
                ATT'KEY = ATT'KEY2
                IF ATT'CTL = 0 AND ATT'KATH'NO = PRH'ATH'NO1 THEN
                        MAX'SUB'NO = ATT'KNO
                ELSE
                        ENTRY = "1:No attributes for this part???"
                        CALL DO'ERR
                        GOTO ABORT                      !!Kludge
                ENDIF
                ATT'KNO = 1
                RDKEY ATT
                GETNXT ATT
                ATT'KEY = ATT'KEY2
                IF ATT'CTL = 0 AND ATT'KATH'NO = PRH'ATH'NO1 THEN
                        MIN'SUB'NO = ATT'KNO
                ELSE
                        ENTRY = "2:No attributes for this part???"
                        CALL DO'ERR
                        GOTO ABORT                      !!Kludge
                ENDIF
        ELSE
                FOR II = 1 TO 6
                        IF PRH'ATT'DESC(II) # "" THEN
                                IF MIN'SUB'NO = 0 THEN MIN'SUB'NO = II
                                MAX'SUB'NO = II
                        ENDIF
                NEXT II
        ENDIF
RETURN

        MAP1 TOP'TRX'FIELD|NM|,B,2      !!MODIFY: Change to suit trx'field
        MAP1 BTM'TRX'FIELD|NM|,B,2      !!MODIFY: Change to suit trx'field
        MAP1 LAST'TRX'FIELD|NM|,B,2     !!MODIFY: Change to suit trx'field

SYMBOL  TRX'FIELD  = "SUB'NO"           !!MODIFY:
SYMBOL  TRX'STEP = "1"                  !!MODIFY: Transaction step > 0
SYMBOL  HIGH'KEY = "MAX'SUB'NO"         !!MODIFY: to highest legal key value
                                        !!        or will get error in 
                                        !!        prepare to add
SYMBOL SCREEN = "OE1WGS"                !!MODIFY: Primary screen name
SYMBOL ASCREEN = "OE2WGS"               !!MODIFY: Primary screen name

SYMBOL DSP        = "DSP'SUB"   ! Display transaction field over GKD
                                ! May have secondary if two key fields
SYMBOL VDSP       = "VDSP'SUB"  ! Must be primary display field in scan 
                                ! display row, must start in column 3 or >,
                                ! normally followed by one or more secondary
                                ! display fields
SYMBOL VDSP'MSG = "VDSP'SUB'MSG"        ! display message flag field

!!MODIFY:  add symbol statements if more than 8 keys for level, remove
!!         comment marks as needed
SYMBOL TEST'KEY3="TEST'KEY3'|NM|"
SYMBOL TEST'KEY4="TEST'KEY4'|NM|"
SYMBOL TEST'KEY5="TEST'KEY5'|NM|"

        COPYS "|SCREEN|"
        COPYS "|ASCREEN|"

ADD'RECORD|NM|:
        SUB'MSGNO = 0
!!      ADD SUB KEY SUB'KEY
        IF LIN'TORDER'QTY(SUB'NO) # 0 AND REAL'PG = |NM| THEN
                SUB'CTL = 3
        ELSE
                SUB'CTL = 0
                LIN'TORDER'QTY(SUB'NO) = SUB'USR'OQTY
                LIN'TCOM'QTY(SUB'NO) = SUB'USR'CQTY
        ENDIF
        XXX'REC = SUB'XXX'OVR
        TMP'CTL(|NM|) = SUB'CTL
        TMP'RCN(|NM|) = SUB'NO
        IF TMP'CTL(|NM|) = 0 THEN
                IF INC'DZ'PIECES # "N" THEN
                        WGA'DOZENS(SUB'NO) = FIX(SUB'USR'OQTY/12)
                        WGA'PIECES(SUB'NO) = SUB'USR'OQTY - (WGA'DOZENS(SUB'NO)*12)
                ELSE
                        WGA'PIECES(SUB'NO) = SUB'USR'OQTY
                ENDIF
                ZZZW'REC = WGA'REC
                HDR'SHP'DONE = "N"
                HDR'PICK'NEED = "Y"
                ZZZ|NM|'SPACE = SPACE           !!Trick UPDATE'QTY'REC
                ZZZX'SPACE = SPACE              !!Ditto
                IF XXX'COM'QTY # 0 OR XXX'BO'QTY # 0 CALL UPDATE'QTY'REC
                IF XXX'REC # SUB'XXX'OVR THEN
                                                !!In case UPDATE'QTY'REC
                                                !!changed values.
                        SUB'XXX'OVR = XXX'REC
                        LIN'TORDER'QTY(SUB'NO) = SUB'USR'OQTY
                        LIN'TCOM'QTY(SUB'NO) = SUB'USR'CQTY
                ENDIF
                CALL ADDCOUNT|NM|
                CALL ZSAVAMT|NM|
                CALL ADDTOTAL|NM|
                CIF UP'CHARGES = "Y" THEN
                    CIF WGA'PRGNAM="OE1WGA" THEN
                        IF OEC'SKIP'UP # "Y" CALL ADD'UP'QTY
                    ENDCIF
                ENDCIF
!!
!!Below occurs in mark add while in SUB level
!!
                IF REAL'PG = |NM| AND TOP'MARK # 0 CALL PRE'DSP'TOTALS
        ENDIF
RETURN

MAP1 USED'FTIME,B,1
MAP1 DB'II,F

PRE'SET'USE|NM|:
        CALL SET'KEY|NM|
USE|NM|:                                !Kludge to save a call
        CALL SET'SUB'REC
        USED'FTIME = 1 - SGN(SUB'CTL)
        DB'II = SUB'KNO
RETURN

!!MODIFY: Remove comments below and adjust SUBD to secondary key name for
!!        alphabetic display area if used
!!
!!USE'2ND'KEY|NM|:
!!      USE SUBD INTO SUB'REC KEY SUBD'KEY
!!      TMP'CTL(|NM|) = SUBD'CTL
!!      TMP'RCN(|NM|) = SUBD'RCN
!!RETURN

PRE'GETNXT|NM|:
GETNXT|NM|:
        IF USED'FTIME = 1 THEN
                USED'FTIME = 0
                SUB'CTL = 0
                SUB'KNO = DB'II
        ELSE
                SUB'CTL = 1
                DO UNTIL DB'II >= 6 OR SUB'CTL = 0
                        DB'II = DB'II + 1
                        IF DB'II >= 1 AND DB'II <= 6 THEN
                                IF LIN'TORDER'QTY(DB'II) # 0 THEN
                                        SUB'KNO = DB'II
                                        SUB'CTL = 0
                                ENDIF
                        ENDIF
                LOOP
        ENDIF
        IF SUB'CTL = 0 THEN
                CALL SET'SUB'REC
        ELSE
                TMP'RCN(|NM|) = 0
                TMP'CTL(|NM|) = 1
        ENDIF
        IF SET'XXX = 0 THEN XXX'REC = SUB'XXX'OVR
RETURN

PRE'GETPRV|NM|:
GETPRV|NM|:
        USED'FTIME = 0
        SUB'CTL = 1
        DO UNTIL DB'II <= 1 OR SUB'CTL = 0
                DB'II = DB'II - 1
                IF DB'II >= 1 AND DB'II <= 6 THEN
                        IF LIN'TORDER'QTY(DB'II) # 0 THEN
                                SUB'KNO = DB'II
                                SUB'CTL = 0
                        ENDIF
                ENDIF
        LOOP
        IF SUB'CTL = 0 THEN
                CALL SET'SUB'REC
        ELSE
                TMP'RCN(|NM|) = 0
                TMP'CTL(|NM|) = 1
        ENDIF
        IF SET'XXX = 0 THEN XXX'REC = SUB'XXX'OVR
RETURN

!!MODIFY:  delete below for full screen windowing of messages
!!         or if message field not MSGNO fix ZZZ|NM|'MSGNO
!!
PROC'MSG|NM|:                                   !!SUB'MSGNO message processing
RETURN                                          !!SUB'MSGNO message processing

KEY'EQ'ENTRY|NM|:
        OLD'KEY'TYPE(|NM|) = KEY'TYPE(|NM|)             !!save current key order
!!MODIFY: Change as below as needed for multiple field keys in GKD
!!        OR for multiple key types (numeric and alpha)
!!
!!
!!      For multiple field keys: (with alphabetic 2nd key allowed in 1st field)
!!
!!      IF ECFLD = 1 THEN 
!!              IF VAL(ENTRY) = 0 AND ENTRY # 0 THEN
!!                                              !!test for secondary key
!!                                              !!above test solely for alpha
!!                                              !!key test only
!!                      SUB'DESC = ENTRY                !!MODIFY: Field name of 2nd key
!!                      KEY'TYPE(|NM|) = 2      !!flag as secondary ALPHA key
!!              ELSE
!!                      |TRX'FIELD| = ENTRY     !!Field name of 1st field in
!!                                              !!primary key
!!                      KEY'TYPE(|NM|) = 1      !!flag as primary NUMERIC key
!!              ENDIF
!!
!!      ELSE
!!      IF ECFLD = 2 THEN
!!              SUB'FLD2 = ENTRY                        !!Field name of 2nd field in
!!                                              !!primary NUMERIC key
!!      ELSE
!!      IF ECFLD = 3 THEN
!!              SUB'FLD2 = ENTRY                        !!Field name of 3rd field in
!!                                              !!primary NUMERIC key
!!        :
!!       etc.
!!      ENDIF
!!      ENDIF
!!      ENDIF
!!        :
!!       etc.
!!
!!
!!      For alpha and numeric keys with single key field:
!!
!!      IF VAL(ENTRY) = 0 AND ENTRY # 0 THEN
!!              SUB'DESC = ENTRY                !!MODIFY: Field name of secondary key
!!              KEY'TYPE(|NM|) = 2      !!flag as secondary ALPHA key
!!      ELSE
!!              |TRX'FIELD| = ENTRY     !!Field name of primary key
!!              KEY'TYPE(|NM|) = 1      !!flag as primary numeric key
!!      ENDIF
!!
!!MODIFY: if using multiple key fields or alpha and numeric, delete the 
!!        following line:  MUST delete if GKD'FLG(|NM|) = 1  !!!!!!
!!        Also, not needed if one key field (use above).

!!      |TRX'FIELD| = ENTRY     

RETURN

BAD'KEY'TEST|NM|:
        SF = 0          ! MUST BE HERE
        IF SUB'HDR'LOC'NO # HDR'LOC'NO OR SUB'DOC'TYPE # HDR'DOC'TYPE &
                OR SUB'HDR'NO # LIN'HDR'NO THEN TMP'CTL(|NM|) = 3
        IF SUB'LIN'NO # LIN'NO THEN TMP'CTL(|NM|) = 3
        IF ARCHIVE'FLAG = "A" THEN
                IF SUB'SHIPMENT # LIN'SHIPMENT THEN TMP'CTL(|NM|) = 3
        ENDIF
!!MODIFY: change as below to reject certain records during scan
!!
!! TO EXIT SEARCH (pseudo EOF):
!!
!!      IF SUB'BAD1 # SUB'KBAD1 OR SUB'BAD2 # SUB'KBAD2 THEN
!!              TMP'CTL(|NM|) = 3
!!      ENDIF
!!
!! TO SKIP A SINGLE RECORD:
!!
!!      IF SUB'BAD1 = "BAD'CASE" THEN SF = 1
!!
RETURN

PRE'DSP|NM|:

!!MODIFY:  Add lookups, date formatting, etc. prior to CALL |DSP| 

        IF SPG = 0 AND PG = REAL'PG THEN
                IF PRH'ATT'SPACE = SPACE THEN
                        ATT'SPACE = SPACE
                        ATT'KATH'NO = PRH'ATH'NO1
                        ATT'KNO = SUB'NO
                        USE ATT INTO ATT'REC KEY ATT'KEY
                ELSE
                        ATT'NO = SUB'NO
                        CALL SET'ATT'CODE
                ENDIF
                CALL |DSP|
                CALL DSP'ATT'DESC
        ENDIF
!!MODIFY: to use a display field when display portion is off, enable and
!!        modify the following as appropriate
!!
!!      IF TOGGLE'BTM(|NM|) # 0 THEN
!!              CALL DSP'DESCRIPTION
!!      ENDIF
!!
!!MODIFY: if allowing F1,F3 or F6 to change records in a window, you must 
!!        do similar to following:
!!
!!      IF IN'WIN'FLAG = 0 THEN
!!              CALL |DSP|
!!      ELSE
!!              CALL WIN'DISPLAY
!!      ENDIF
!!
RETURN

!!MODIFY: if allowing F1,F3 or F6 to change records in a window, you must 
!!        do similar to following in action of window call:  
!!              (see notes above and in DO'WINDOW)
!!      
!!ACTION:
!!      IN'WIN'FLAG = 1
!!      DSP WINDOW
!!      BSAV'RCN = SUB'RCN
!!      BSAV'SXLN = SXLN(|NM|)
!!      BSAV'SCR'TOP = SCR'TOP(|NM|)
!!AC1:
!!      DDB WINDOW
!!      GDB WINDOW
!!      IF INX2 = FN'1 OR INX2 = FN'6 THEN
!!              WRITE record in window
!!              IF INX2 = FN'1 THEN
!!                      CALL DOWN'LINE
!!              ELSE
!!                      CALL UP'LINE
!!              ENDIF
!!              READ record in window           !! NEW RECORD
!!              GOTO AC1
!!      ELSE            
!!              REL record in window
!!              IF INX2 = FN'3 THEN
!!                      CALL UP'LINE
!!                      READ record in winodw   !!NEW RECORD
!!                      GOTO AC1
!!              ENDIF
!!              IF SCR'TOP(|NM|) = BSAV'SCR'TOP AND SCR'TOP(|NM|) + BSAV'SXLN < |SCR'MAX| THEN
!!                      IF SCR'RCN(|NM|,SCR'TOP(|NM|) + BSAV'SXLN) = BSAV'RCN THEN TOGGLE'BUDGETS = 0
!!              ENDIF
!!      ENDIF
!!RETURN

COPYF "WGSFIL"

PRE'VDSP|NM|:
        IF TOGGLE'BTM(|NM|) = 0 AND SPG = 0 AND PG = REAL'PG THEN

!!MODIFY:  Add lookups, date formatting, etc. prior to CALL |VDSP| 

                IF PRH'ATT'SPACE = SPACE THEN
                        ATT'KATH'NO = PRH'ATH'NO(1)
                        ATT'KNO = SUB'ATT'NO(1)
                        ATT'SPACE = SPACE
                        USE ATT INTO ATT'REC KEY ATT'KEY
                ELSE
                        ATT'NO = SUB'NO
                        CALL SET'ATT'CODE
                ENDIF
                \FACTORQTY MODE="Q" PRE="XXX" MASKSET="2"
                \FROUND ((LIN'PRICE-LIN'DISC)*XXX'ORDER'QTY)/QFACTOR SNET
                CALL SET'WGS
                CALL SET'WGS'DZPC
                CALL |VDSP|
                CALL PRE'VDSP'MSG|NM|                   !!SUB'MSGNO message processing
        ENDIF
RETURN

PRE'VDSP'MSG|NM|:
RETURN

UPDATE'2ND'KEYS|NM|:
CHECK'HEADER|NM|:
SET'2ND'KEYS|NM|:
DEL'2ND'KEYS|NM|:
        !!MODIFY: normally needed only if special key rebuild is used
RETURN

SET'KEY|NM|:
!!MODIFY: normally no changes needed
        \SETKEY PRE="SUB" MODE="KEY"
        IF ARCHIVE'FLAG # "C" THEN
!!WRONG         \SETKEY PRE="SHS" MODE="KEY" ALTPRE="SUB"
                SHS'HDR'LOC'NO = SUB'HDR'LOC'NO
                SHS'DOC'TYPE = SUB'DOC'TYPE
                SHS'HDR'NO = SUB'HDR'NO
                SHS'LIN'NO = SUB'LIN'NO
                SHS'NO = SUB'NO
                SHS'SHIPMENT = SUB'SHIPMENT
                \SETKEY PRE="SHS" MODE="KEY"
        ENDIF
RETURN

DO'DUP|NM|:
!!MODIFY: Add any duplicating logic here

        ZZZ|NM|'REC = SUB'REC
        SUB'SPACE = SPACE       !! clear record
        CALL SET'FROM'HIGHER|NM|
        IF INC'DZ'PIECES # "N" THEN
                \DEFALT |SCREEN|
        ELSE
                \DEFALT |ASCREEN|
        ENDIF
!![6]   CALL SET'SUB'FRACTIONS
!!      SUB'QFRACTION = 0
!!      SUB'LFRACTION = 0
!!      SUB'XFRACTION = 0
!!      SUB'HFRACTION = 0
        SUB'LENGTH = 1
        SUB'WIDTH = 1
        SUB'HEIGHT = 1
        SUB'KIT'BUILD = LIN'KIT'BUILD
        SUB'ATT'NO1 = SUB'NO
        XXX'REC = SUB'XXX'OVR
RETURN

SET'HIGH'VALUES|NM|:
!!MODIFY: change as needed for multiple field keys
!!        NOTE:  set data fields, not key fields
        |TRX'FIELD| = |HIGH'KEY|
        CALL SET'FROM'HIGHER|NM|
RETURN

SET'LOW'VALUES|NM|:
!!MODIFY: needed when using batch and transaction type files where batch
!!        would remain as a constant
!!        NOTE:  set data fields, not key fields
        SUB'SPACE = SPACE
        CALL SET'FROM'HIGHER|NM|
        SUB'NO = 1
RETURN

SET'FROM'HIGHER|NM|:
        SUB'HDR'LOC'NO = HDR'LOC'NO
        SUB'DOC'TYPE = HDR'DOC'TYPE
        SUB'HDR'NO = LIN'HDR'NO
        SUB'LIN'NO = LIN'NO
        IF ARCHIVE'FLAG = "A" THEN                              !![2]
                SUB'SHIPMENT = LIN'SHIPMENT
        ENDIF
RETURN

PRE'ABORT|NM|:
        SUB'RCN = 0
REL|NM|:                        !! PLACED HERE TO SAVE SPACE
RETURN

!SET'RT'FN|NM|:                 !! IN LINSUB.CPY
!!MODIFY: Set function key you want to happen automatically on return
!!used only if multiple pages or automatic windows used
!!
!!      INX2 = FN'D
!
!RETURN

GDB'DO'WINDOW|NM|:
!!MODIFY:  if doing a window from add mode - implement below line if you
!!         would like automatic add to continue.  You may want to do
!!         this only SELECT(|NM|) = 1 (add mode only, not insert mode)
!!
!!      IF SELECT(|NM|) = 1 OR SELECT(|NM|) = 2 THEN NXT'F(|NM|) = 1
!!
        GKD'F = 0
        CALL DO'WINDOW|NM|'2
!!MODIFY: Set below to suit if using windows or multiple pages  
!!      IF WINDOW = FN'M GOTO GDB'DO'WINDOW     ! RETURN FROM MENU
!!
!!      Display totals changed from lower level, etc
!!
!!
!!      ECFLD = 0       !! SHOULD BE HERE IF YOU WANT WINDOW TO NOT
!!                      !! RETURN TO THE SAME FIELD IT CAME FROM
RETURN

DO'WINDOW|NM|:
!!MODIFY:  remove below for full screen message !!SUB'MSGNO message processing
!!
        GKD'F = 1
DO'WINDOW|NM|'2:
        CASE
        IF WINDOW = FN'7 THEN
                CALL DO'WINDOW'FN'7                     !!HDRSUB.CPY
        ELSE
        IF WINDOW = FN'B OR WINDOW = FN'C OR WINDOW = FN'X OR WINDOW = FN'8 THEN
                ENTRY = "Not possible on sub line items"
                CALL DO'ERR
        ELSE
        IF WINDOW = FN'M THEN
                CALL DO'WINDOW'FN'M             !!IN HDRSUB.CPY
                IF ACTION = "~" THEN
                        GOTO DO'WINDOW|NM|'2
                ENDIF
        ELSE
        IF WINDOW = FN'R THEN
                CALL DO'WINDOW'FN'R             !!IN HDRSUB.CPY
        ELSE
        IF WINDOW = FN'Z AND XXX'USR'OQTY < 0 THEN
                CALL RPROC                      !!IN LINSUB.CPY
                CALL PRE'UPDATE|NM|
        ELSE
        IF WINDOW = FN'P OR WINDOW = FN'T OR WINDOW = FN'L OR           &
                                                        WINDOW = FN'U THEN
                CALL PROC'CHAIN                 !!LINSUB.CPY
                PG = 3
        ENDCASE
        REAL'PG = |NM|
RETURN

!!MODIFY: Set below to suit if using windows or multiple pages  
!!PROC'SUBSEL:
!!      DSP SUBSEL
!!      WK3'ACTION = ""
!!RE'GDB'|NM|SEL:
!!      WINDOW = 0
!!      ECFLD = 0
!!      GDB SUBSEL
!!      IF INX1 = I'ESC OR INX2 = FN'10 THEN
!!              XLN = SXLN(|NM|)
!!              CALL PRE'VDSP|NM|
!!              XLN = 0
!!              RETURN
!!      ENDIF
!!      IF INX1 = I'FUN THEN 
!!              WINDOW = INX2
!!      ELSE
!!      IF WK3'ACTION = "T" THEN
!!              WINDOW = FN'T
!!      ELSE
!!      IF WK3'ACTION = "L" THEN
!!              WINDOW = FN'L
!!      ELSE
!!      IF WK3'ACTION = "P" THEN
!!              WINDOW = FN'P
!!      ELSE
!!      IF WK3'ACTION = "U" THEN
!!              WINDOW = FN'U
!!      ELSE
!!      IF WK3'ACTION = "D" THEN
!!              WINDOW = FN'D
!!      ENDIF
!!      ENDIF
!!      ENDIF
!!      ENDIF
!!      ENDIF
!!      ENDIF
!!
!!      IF WINDOW = FN'D THEN
!!              CALL PRE'PROC'SUB'M
!!      ENDIF
!!      IF WINDOW = FN'T OR WINDOW = FN'L OR WINDOW = FN'P OR &
!!         WINDOW = FN'U GOTO CHAIN'REPORT
!!      GOTO RE'GDB'SUBSEL
!!RETURN

!!MODIFY: Set below to suit if using windows or multiple pages  
!!

!!PRE'PROC'SUB'FN:                      !! FROM FUNCTION KEY
!!      PG = 2
!!      \WINDOW VUEMLT="|NM|" ACTION="BEGIN" WINNUM="2" FULL="Y"
!!      CALL PRE'UPDATE1                !! IF KEEPING TOTALS IN PAGE 1
!!      PG = 1
!!      XLN = SXLN(|NM|)                !! ONLY NEEDED IF VDSP CHANGES
!!      CALL PRE'VDSP|NM|
!!      XLN = 0
!!RETURN

!!MODIFY: Set below to suit if using windows or multiple pages  
!!PRE'PROC'SUB'M:                       !! FROM MENU
!!      PG = 2
!!      \WINDOW SCREEN="SUB SEL" ACTION="BEGIN" WINNUM="2" FULL="Y"
!!      CALL PRE'UPDATE1                !! IF KEEPING TOTALS IN PAGE 11
!!      PG = 1
!!      XLN = 0
!!RETURN

PRE'GKD'SCREEN|NM|:
        IF WIN'F = 9 THEN
                INX1 = I'FUN
                INX2 = FN'10
        ELSE
                IF INC'DZ'PIECES # "N" THEN
                        GKD |SCREEN|
                ELSE
                        GKD |ASCREEN|
                ENDIF
        ENDIF
RETURN

PRE'GDB'SCREEN|NM|:
        IF WIN'F # 0 THEN
                INX1 = I'FUN
                IF WIN'F = 9 THEN
                        INX2 = FN'10
                ELSE
                        INX2 = FN'4 
                        WIN'F = 0
                ENDIF
                RETURN                  !!KLUDGE
        ENDIF
        IF ARCHIVE'FLAG = "C" THEN
                CALL DO'ATH'AFR
                IF LIN'PACK'FLAG # 0 THEN
                        ?TAB(24,1) ; TAB(-1,A'RED) ; "Cannot change - box packing already entered - any key to exit";
                        CALL L24CHR                                             
                ELSE
                        IF INC'DZ'PIECES # "N" THEN     !![4]
                                GDB |SCREEN|
                        ELSE
                                GDB |ASCREEN|
                        ENDIF
                        CALL TST'FUNC
                        IF TST'FUNC = 0         AND &
                                PRH'PMSGNO # 0  AND &
                                SUB'MSGNO = 0   AND &
                                PRH'PFORCE = "Y" THEN
                            IF HDR'PROC # 5 THEN
                                INX1 = I'FUN
                                INX2 = FN'8
                            ENDIF
                        ENDIF
                ENDIF
                IF INX2 = FN'4 OR INX2 = FN'3 OR INX2 = FN'10 THEN
                        CALL DO'DSP'OE2LINS
                ENDIF                           
        ELSE
                PRINT TAB(24,1) ; TAB(-1,A'RED) ; "Cannot change archives - any key to exit";
                CALL L24CHR
        ENDIF
RETURN

DO'ATH'AFR:
        CALL OTHERS'GAFR3
RETURN

PRE'DSP'SCREEN|NM|:
!!MODIFY: Lookup background display values here ONLY IF 'dspsw # 220'
!!
!!      IF dspsw # 220 THEN
!!              look other values
!!      ENDIF

        IF INC'DZ'PIECES # "N" THEN
                DSP |SCREEN|
        ELSE
                DSP |ASCREEN|
        ENDIF
        SUBWIN'EDT = EDT'OVR
        CALL DO'ATH'AFR
        IF dspsw # 220 THEN
                ?TAB(13,20);TAB(-1,11);"/";TAB(13,42);"/";TAB(13,65);"/";TAB(-1,12);
        ENDIF
RETURN

PRE'DDB'SCREEN|NM|:
!!MODIFY: Lookup and display other values here
        CALL SET'WGS
        SUB'FTIME = SHIPPING            !!Set to 1 if inn shipping mode
        DDB'DONE1 = 0
        IF INC'DZ'PIECES # "N" THEN
                DDB |SCREEN|
        ELSE
                DDB |ASCREEN|
        ENDIF
        DDB'DONE1 = 0
RETURN

PRE'USE|NM|:
        SUB'KNO = TMP'RCN(|NM|)
        CALL SET'SUB'REC
        XXX'REC = SUB'XXX'OVR
!!MODIFY:  Set RCN to 0 if secondary key that allows duplicates is used
!!         as an alternate lookup key - e.g.:
!!      SUBD'RCN = 0
RETURN

PRE'READ|NM|:
        SUB'KNO = TMP'RCN(|NM|)
POST'PRE'READ|NM|:
        CALL SET'SUB'REC2
        XXX'REC = SUB'XXX'OVR
        ZZZX'REC = XXX'REC
        ZZZ|NM|'REC = SUB'REC
        IF SELECT(|NM|) = 3 THEN CHG'QTY'KEY = 2  !!Force lookup available
        CIF EOFTTL|NM| THEN
                CALL SAVTOTAL|NM|
        ENDCIF
RETURN

PRE'WRITE|NM|:
PRE'UPDATE|NM|:
        SUB'XXX'OVR = XXX'REC
        IF ZZZ|NM|'REC # SUB'REC AND ARCHIVE'FLAG = "C" THEN
                IF INC'DZ'PIECES # "N" THEN
                        WGA'DOZENS(SUB'NO) = FIX(SUB'USR'OQTY/12)
                        WGA'PIECES(SUB'NO) = SUB'USR'OQTY - (WGA'DOZENS(SUB'NO)*12)
                ELSE
                        WGA'PIECES(SUB'NO) = SUB'USR'OQTY
                ENDIF
                ZZZW'REC = WGA'REC
                HDR'SHP'DONE = "N"
                HDR'PICK'NEED = "Y"
                CALL UPDATE'QTY'REC
                LIN'TORDER'QTY(SUB'NO) = SUB'USR'OQTY
                LIN'TCOM'QTY(SUB'NO) = SUB'USR'CQTY
                SUB'XXX'OVR = XXX'REC
                CALL ADDTOTAL|NM|
                CIF UP'CHARGES = "Y" THEN
                    CIF WGA'PRGNAM="OE1WGA" THEN
                        IF OEC'SKIP'UP # "Y" CALL ADD'UP'QTY
                    ENDCIF
                ENDCIF
                CALL SAVTOTAL|NM|
                ZZZX'REC = XXX'REC
                ZZZ|NM|'REC = SUB'REC
        ENDIF
RETURN

PRE'INSERT|NM|:
        I = |TRX'FIELD|
        IF TOP'LOADED(|NM|) = 0 AND SCR'TOP(|NM|) + SXLN(|NM|) <= 1 CALL POP'BOTTOM
        IF SCR'TOP(|NM|) + SXLN(|NM|) > 1 THEN
                TMP'RCN(|NM|) = SCR'RCN(|NM|,SCR'TOP(|NM|) + (SXLN(|NM|) - 1))
                |CALL'PRE'USE|

                IF I > |TRX'FIELD| THEN
                        I = INT((I - |TRX'FIELD|)/2) + |TRX'FIELD|
                ELSE
                        I = |TRX'FIELD| + |TRX'STEP|
                ENDIF

        ELSE
                I = |TRX'FIELD| - (INT(I/2) MIN 1)
        ENDIF

        IF I = |TRX'FIELD| THEN I = 0
RETURN

TRX'FIELD'EQ'I|NM|:
!!MODIFY: normally no changes needed
!!         If not setting key fields automatically delete entire routine.
        |TRX'FIELD| = I
RETURN

I'PLUS'TRX'STEP|NM|:
!!MODIFY: normally no changes needed
!!         If not setting key fields automatically delete entire routine.
        I = |TRX'FIELD| + |TRX'STEP|
RETURN

I'EQ'TRX'STEP|NM|:
!!MODIFY: normally no changes needed
!!         If not setting key fields automatically delete entire routine.
!!      I = |TRX'STEP|
        I = MIN'SUB'NO MAX 1
        OE1WGA'ADDING = 1
RETURN

DEL'RECORD|NM|:
        CALL DEL'UNCOMMIT
        IF SKIP'MARK # 0 THEN
                TMP'CTL(|NM|) = 3
                DELLOW = 1
        ELSE
                IF SUB'NO > 0 AND SUB'NO <= 6                   !![7]
                        LIN'TORDER'QTY(SUB'NO) = 0
                        LIN'TCOM'QTY(SUB'NO) = 0
                ENDIF                                           !![7]
                SUB'RCN = TMP'RCN(|NM|)
                TMP'CTL(|NM|) = 0
                SUB'CTL = 0
                IF SUB'NO > 0 AND SUB'NO <= 6 THEN              !![7]
                        WGA'DOZENS(SUB'NO) = 0
                        WGA'PIECES(SUB'NO) = 0
                        IF HDR'BO'YN = "Y" THEN
                                WGA'BDOZENS(SUB'NO) = 0
                                WGA'BPIECES(SUB'NO) = 0
                                CALL SHOW'BO'S
                        ENDIF
                ENDIF                                           !![7]
                ZZZW'REC = WGA'REC
                HDR'SHP'DONE = "N"
                HDR'PICK'NEED = "Y"
                CALL SUBTOTAL|NM|
        ENDIF
RETURN

!COPYF "LINFIL",=LTCC,NOAFR
MAP1 LTCC'REC,X,256

DEL'UNCOMMIT:                           !!CALLED IN DEL'RECORD
        SKIP'MARK = 0
        QTY'CTL = 0
!!      TCC'REC[257;256] = LIN'REC      !!WARNING:  THIS IS DANGEROUS
        LTCC'REC = LIN'REC
        TCC'REC[1;128] = SUB'REC        !!WHEN ONE CHANGES FILE LAYOUTS!
        TCC'REC[129;128] = XXX'REC      !!DITTO
        XXX'SPACE = SPACE               !!TRICK UPDATE'QTY'REC
        LIN'SPACE = SPACE               !!DITTO
        SUB'SPACE = SPACE
        CALL UPDATE'QTY'REC             !!Fix QTY record
!!      LIN'REC = TCC'REC[257;256]
        LIN'REC = LTCC'REC
        SUB'REC = TCC'REC[1;128]
        XXX'REC = TCC'REC[129;128]
        CALL FIND'PRH                   !![47]
        IF QTY'CTL # 0 THEN
                PRINT TAB(24,1) ; TAB(-1,A'RED) ; "Could not uncommit - CTRL-Z to confirm delete";
                CALL L24CHR
                IF lastchb = 26 THEN
                        ENTRY = "You MUST recalculate committed/backorder quantities"
                        CALL DO'ERR
                        SKIP'MARK = 0
                ELSE
                        SKIP'MARK = 1
                ENDIF
        ENDIF
RETURN

SET'ADD'KEY|NM|:
        TMP'CTL(|NM|) = 0
!!MODIFY: if not automatically setting key field this routine must be changed
!!        or if using ALPHA or other unusual keys.  See I'PLUS'TRX'STEP, ETC.
!!       (usually if key is a string).  If not setting the key delete the
!!      first line below.

        IF I > |HIGH'KEY| THEN TMP'CTL(|NM|) = 9
        IF TMP'CTL(|NM|) # 9 THEN
                CALL TRX'FIELD'EQ'I|NM|
        ENDIF
RETURN

MARK'IT'L25|NM|:
!!MODIFY: To defeat mark mode (add and delete) add the following lines:
        ENTRY = "Mark Mode Not Supported"
        CALL DO'ERR
        TOP'MARK = 0
        BTM'MARK = 0
RETURN

DEL'IT'L25|NM|:
!!MODIFY: CHANGE MESSAGE TO SUIT (carefull of length)
        L25 = "'G' will delete from "+TOP'TRX'FIELD|NM|+" to "+BTM'TRX'FIELD|NM|+": [ESC] to abort"
RETURN

!!! E'ADD'L25|NM|:      NOT NEEDED      SRC 9/7/85
!!MODIFY: CHANGE MESSAGE TO SUIT (carefull of length)
!!!     L25="CTRL- B> Insert | Z> Delete | F4 Add | [RET] Change | F10 Exit"
!!! RETURN

UPDATE'REC'L25'ADD|NM|:
!!MODIFY: CHANGE MESSAGE TO SUIT (carefull of length)
        L25="F1 upd/nxt | F2 upd/chg | F3 or F4 cancel/chg | F8 msg | Func M>enu"
RETURN

UPDATE'REC'L25'CHANGE|NM|:
!!MODIFY: CHANGE MESSAGE TO SUIT (carefull of length)
        L25="F1 upd/nxt | F2 upd/sel | F3 cancel/nxt | F4 cancel/sel | F8 msg | Func M>enu"
RETURN

!DO'BAR'L25|NM|:        !!IN HDRSUB.CPY
!!MODIFY: CHANGE MESSAGE TO SUIT (carefull of length)
!       L25="CTRL- B> Insert | Z> Delete | F4 Add | [RET] Change | F10 Exit"
!RETURN

MARK'TOP|NM|:
RETURN

MARK'BTM|NM|:
RETURN

LAST'EQ'TRX|NM|:
        LAST'TRX'FIELD|NM| = |TRX'FIELD|
RETURN

CHECK'DEL|NM|:
!!MODIFY: Change as needed for status checking prior to all deletes.   
!!        You may differentiate mark deletes by testing TOP'MARK # 0.
!!        ASKDEL|NM| will only be called if SKIP'MARK AND ABORT'MARK
!!        are both set to zero in this routine! 
!!
        DELLOW = 0
        CALL CHECK'DEL'ALL
RETURN

!!DEL'Q'S|NM|:
!!MODIFY: Change as needed for delete questions prior to all deletes.  
!!        Called after ASKDEL|NM| if delete is to be performed.
!!        You may differentiate mark deletes by testing TOP'MARK # 0.
!!        Will delete only if SKIP'MARK AND ABORT'MARK are
!!        both set to zero in this routine!     
!!
!!      CALL delete'questions
!!      IF "N" THEN SKIP'MARK = 1               !!skip single delete
!!      CALL TST'FUNC : ABORT'MARK = TST'FUNC   !!abort mark delete and single
!!RETURN

!!CHECK'ADD|NM|:
!!MODIFY:  TO DISALLOW ADDING, SET ABORT'ADD = 1
!!          E.G. IF BAT'STAT = "P" THEN ABORT'ADD = 1   
!!
!!RETURN

MARK'ADD'Q'S|NM|:
!!MODIFY: Change as needed for mark add questions
!!
RETURN

LCL1 XSTART,F
LCL1 XEND,F
MAP1 OLD'KEY3,B,2

MARK'ADD|NM|:
RETURN

!!MODIFY: messages - be careful not to change the intent of the message!!
!!        normally no changes needed.

!EMPTY'MSG|NM|:                         !!IN HDRSUB.CPY
!       ENTRY = "Sub Line Items  Empty - you are not authorized to add"
!RETURN

ONFIL'MSG|NM|:
        ENTRY = ENTRY + " Sub Line Item already on file"
RETURN

!!ALL BELOW IN HDRSUB.CPY

!CHANG'MSG|NM|:
!       ENTRY = "You are not authorized to change Sub Line Items "
!RETURN

!DELET'MSG|NM|:
!       ENTRY = "You are not authorized to delete from Sub Line Items "
!RETURN

!ADD'MSG|NM|:
!       ENTRY = "You are not authorized to add to Sub Line Items "
!RETURN

!INSERT'MSG|NM|:
!       ENTRY = "Sorry, no room to insert in Sub Line Items "
!RETURN

!HIGH'MSG|NM|:
!       ENTRY = "The highest Sub Line Items Key already added"
!RETURN

!!BELOW are optional labels.  They are "stacked" on one return to save
!!program size.  To implement one of them seperate it and put in the
!!RETURN
!!
!!To use below features add a "GLOBAL EOFTTL?="Y" " To top of VUEMLT program
!!where ? is the page number of this level
!!
ZTOTAL|NM|:
        IF ARCHIVE'FLAG # "C" RETURN            !!Kludge
        LIN'ORDER'QTY = 0
        LIN'BO'QTY = 0
        LIN'COM'QTY = 0
        LIN'SUB'COUNT = 0
!!      IN'SUB'KTD = 0          !TEMP USE
        CALL ZLIN'USR
ZSAVAMT|NM|:                            !Kludge to save a call.  bfd
        IF ARCHIVE'FLAG # "C" RETURN            !!Kludge
        SAV'ORDER'QTY = 0
        SAV'USR'OQTY = 0
        SAV'COM'QTY = 0
RETURN

ZLIN'USR:
        IF ARCHIVE'FLAG # "C" RETURN            !!Kludge
        LIN'USR'OQTY = 0
        LIN'USR'CQTY = 0        
        LIN'USR'BQTY = 0
RETURN

SAVTOTAL|NM|:
        IF ARCHIVE'FLAG # "C" RETURN            !!Kludge
        IF XXX'COM'QTY # SAV'COM'QTY OR XXX'ORDER'QTY # SAV'ORDER'QTY THEN
                CALL SHOW'TOTALS|NM|
        ENDIF
        SAV'ORDER'QTY = XXX'ORDER'QTY
        SAV'COM'QTY = XXX'COM'QTY
        SAV'USR'OQTY = XXX'USR'OQTY
RETURN

ADDCOUNT|NM|:
        IF ARCHIVE'FLAG # "C" RETURN            !!Kludge
        LIN'SUB'COUNT = LIN'SUB'COUNT + 1
RETURN

ADDTOTAL|NM|:
        IF ARCHIVE'FLAG # "C" RETURN            !!Kludge
        LIN'ORDER'QTY = LIN'ORDER'QTY - SAV'ORDER'QTY + XXX'ORDER'QTY
        IF HDR'QUOTE'YN = "Y" THEN
                LIN'COM'QTY = 0
                LIN'BO'QTY = 0
        ELSE
                LIN'COM'QTY = LIN'COM'QTY - SAV'COM'QTY + XXX'COM'QTY
                LIN'BO'QTY = LIN'ORDER'QTY - LIN'COM'QTY
        ENDIF
        CALL DIVIDE'TO'USR'LINS
RETURN

DIVIDE'TO'USR'LINS:
!!      LWH = 1
        IF LIN'ORDER'QTY # 0 THEN
                XXX2'REC = XXX'REC
                XXX'REC = LIN'XXX'OVR
                XXX'REC = XXX2'REC
                LIN'USR'CQTY = LIN'COM'QTY
                LIN'USR'OQTY = LIN'ORDER'QTY
                IF HDR'QUOTE'YN = "Y" THEN
                        LIN'USR'BQTY = 0
                ELSE
                        LIN'USR'BQTY = LIN'USR'OQTY - LIN'USR'CQTY
                ENDIF
        ELSE
                CALL ZLIN'USR
        ENDIF
RETURN

SUBTOTAL|NM|:
        IF ARCHIVE'FLAG # "C" RETURN            !!Kludge
        LIN'ORDER'QTY = LIN'ORDER'QTY - XXX'ORDER'QTY
        IF HDR'QUOTE'YN # "Y" THEN
                LIN'COM'QTY = LIN'COM'QTY - XXX'COM'QTY
                LIN'BO'QTY = LIN'ORDER'QTY - LIN'COM'QTY
        ENDIF
        CIF UP'CHARGES = "Y" THEN
            CIF WGA'PRGNAM="OE1WGA" THEN
                IF OEC'SKIP'UP # "Y" CALL SUB'OLD'UP'QTY
            ENDCIF
        ENDCIF
        LIN'SUB'COUNT = LIN'SUB'COUNT - 1
        CALL DIVIDE'TO'USR'LINS
        XXX'SPACE =SPACE
        CALL SHOW'TOTALS|NM|
RETURN

DO'DSP'OE2LINS:
        IF INC'DZ'PIECES # "N" THEN
                WGA'DOZENS(SUB'NO) = FIX(XXX'USR'OQTY/12)
                WGA'PIECES(SUB'NO) = XXX'USR'OQTY - (WGA'DOZENS(SUB'NO)*12)
        ELSE
                WGA'PIECES(SUB'NO) = XXX'USR'OQTY
        ENDIF
!!      CALL DSP'OE2LINS
        II = SUB'NO
        CALL DSP'DOZENSII

        XXX2'REC = XXX'REC
        XXX'REC = LIN'XXX'OVR

        CALL DSP'PRICING

        II = HDR'LIN'TOTAL
        HDR'LIN'TOTAL = HDR'LIN'TOTAL -  SAV'PRICE + LIN'NET
        CALL PRE'DSP'TOTALS
        HDR'LIN'TOTAL = II

        XXX'REC = XXX2'REC
RETURN

SHOW'TOTALS|NM|:
!!MODIFY: as below for displaying totals
        IF PG = |NM| AND SPG = 0 AND PG = REAL'PG THEN
                CALL DO'DSP'OE2LINS
        ENDIF
RETURN  

!!MODIFY:       Add your screen edits, etc below

DO'DDBEDT3:
        LIN2'REC = LIN'REC              !!SAVE LIN'REC
        CALL ADDTOTAL|NM|               !!FAKE OUT DSP'OE2LINS AS IF UPDATED
        IF (SHIPPING # 1 OR CHG'QTY'KEY # 2) THEN
                \DDBEDT PREDDB="DDB'CHANGES3" DDBEDT="SUBWIN'EDT" SINX="Y"
        ENDIF
        CALL DO'DSP'OE2LINS
        LIN'REC = LIN2'REC              !!RESTORE LIN'REC
        CALL DO'ATH'AFR
RETURN

DDB'CHANGES3:
        CALL SET'WGS
        IF INC'DZ'PIECES # "N" THEN
                DDB |SCREEN|
        ELSE
                DDB |ASCREEN|
        ENDIF
RETURN

OTHERS'GAFR3:
    IF INC'DZ'PIECES # "N" THEN
        \GAFR |SCREEN| SETUP="SAFR3" DOAFR="N" OTHERS="E"               &
                AFR="OEC'BO'QTY'AFR:WGS'DOZENS(3)+OEC'BO'QTY'AFR:WGS'PIECES(3)" DOAFR="N" NEW="Y"
    ELSE
        \GAFR |ASCREEN| SETUP="SAFR3" DOAFR="N" OTHERS="E"              &
                AFR="OEC'BO'QTY'AFR:WGS'PIECES(3)" DOAFR="N" NEW="Y"
    ENDIF
RETURN

VDSP'SUB:
        CALL VDSP'SUB1
RETURN

\\DUMLBL

CIF SELECT'OFF = "S" THEN                       !!Schema selector

CHECK'SCHEMA|NM|:
                !!MODIFY:  Set SF = 1 to skip a schema during tabbing
                !!         and disallow it for use in this report.
        IF ARCHIVE'FLAG = "C" THEN
                IF SCH'PDBID # "OESUB" THEN SF = 1
        ELSE
                IF SCH'PDBID # "OESHS" THEN SF = 1
        ENDIF
RETURN

!CHECK'HEADER|NM|:
!               !!MODIFY:  Set SF = 1 to skip a header during tabbing
!               !!         and disallow it for use in this report.
!RETURN

SET'STARTKEY|NM|:
SET'ENDKEY|NM|:
RETURN

GLOBAL DO'SELECT|NM|="DO'SELECT|NM|"

DO'SELECT|NM|:
        ENTRY = "Select not allowed on sub level!"
        CALL DO'ERR
RETURN

!!MODIFY:  to disallow schema select on this level only code as follows

!!GLOBAL DO'SELECT|NM|="MY'DO'SELECT|NM|"
!!
!!MY'DO'SELECT|NM|:
!!      ENTRY = "Select/Sort not allowed on this level"
!!      CALL DO'ERR
!!      V''DO'SELECT = 2                !!Force re-entry into GKD
!!RETURN
!!
ENDCIF

SET'WGS:
    IF INC'DZ'PIECES # "N" THEN
        WGS'DOZENS(1) = FIX(XXX'ORDER'QTY/12)
        WGS'PIECES(1) = XXX'ORDER'QTY - (WGS'DOZENS(1)*12)
        WGS'DOZENS(2) = FIX(XXX'COM'QTY/12)
        WGS'PIECES(2) = XXX'COM'QTY - (WGS'DOZENS(2)*12)
        WGS'DOZENS(3) = FIX(XXX'BO'QTY/12)
        WGS'PIECES(3) = XXX'BO'QTY - (WGS'DOZENS(3)*12)
    ELSE
        WGS'PIECES(1) = XXX'ORDER'QTY
        WGS'PIECES(2) = XXX'COM'QTY 
        WGS'PIECES(3) = XXX'BO'QTY
    ENDIF
RETURN

E'WGS'DOZENS1:
        \TSTF
        II = SUB'NO
        IF EDSPSW = 0 THEN
                VV = WGS'DOZENS(1)
                CALL E'WGS'EAMT
                IF IER1 = 0 RETURN                      !!Kludge
                WGS'DOZENS(1) = VV
                XXX'USR'OQTY = (WGS'DOZENS(1)*12) + WGS'PIECES(1)
                IF XXX'USR'OQTY # 0 CALL E'ORDER'QTY            !!LINWGA.CPY
                CALL SET'WGS
                WGA'DOZENS(SUB'NO) = WGS'DOZENS(1)
                ENTRY = WGS'DOZENS(1)
        ENDIF
        GOTO EDIT'DOZENS
RETURN

E'WGS'DOZENS2:
        \TSTF
        II = SUB'NO
        IF EDSPSW = 0 THEN
                VV = WGS'DOZENS(2)
                CALL E'WGS'EAMT
                IF IER1 = 0 RETURN                      !!Kludge
                WGS'DOZENS(2) = VV
                XXX'USR'CQTY = (WGS'DOZENS(2)*12) + WGS'PIECES(2)
                CALL E'COM'QTY                          !!LINWGA.CPY
                CALL SET'WGS
        ENDIF
        GOTO EDIT'DOZENS
RETURN

E'WGS'DOZENS3:
        \TSTF
        II = SUB'NO
        IF EDSPSW = 0 THEN
                VV = WGS'DOZENS(3)
                CALL E'WGS'EAMT
                IF IER1 = 0 RETURN                      !!Kludge
                WGS'DOZENS(3) = VV
                XXX'USR'BQTY = (WGS'DOZENS(3)*12) + WGS'PIECES(3)
                CALL E'BO'QTY                           !!LINWGA.CPY
                CALL SET'WGS
        ENDIF
        GOTO EDIT'DOZENS
RETURN

E'WGS'PIECES1:
        \TSTF
        IF EDSPSW = 0 THEN
                II = SUB'NO
                VV = WGS'PIECES(1)
                CALL E'WGS'EAMT
                IF IER1 = 0 RETURN                      !!Kludge
                WGS'PIECES(1) = VV
                IF INC'DZ'PIECES # "N" THEN
                        XXX'USR'OQTY = (WGS'DOZENS(1)*12) + WGS'PIECES(1)
                ELSE
                        XXX'USR'OQTY = WGS'PIECES(1)
                ENDIF
                CALL E'ORDER'QTY                        !!LINWGA.CPY
                CALL SET'WGS
                WGA'PIECES(SUB'NO) = WGS'PIECES(1)
                ENTRY = WGS'PIECES(1)
        ENDIF
        GOTO EDIT'PIECES
RETURN

E'WGS'PIECES2:
        \TSTF
        IF EDSPSW = 0 THEN
                II = SUB'NO
                VV = WGS'PIECES(2)
                CALL E'WGS'EAMT
                IF IER1 = 0 RETURN                      !!Kludge
                WGS'PIECES(2) = VV
                IF INC'DZ'PIECES # "N" THEN
                        XXX'USR'CQTY = (WGS'DOZENS(2)*12) + WGS'PIECES(2)
                ELSE
                        XXX'USR'CQTY = WGS'PIECES(2)
                ENDIF
                CALL E'COM'QTY                          !!LINWGA.CPY
                CALL SET'WGS
        ENDIF
        GOTO EDIT'PIECES
RETURN

E'WGS'PIECES3:
        \TSTF
        IF EDSPSW = 0 THEN
                II = SUB'NO
                VV = WGS'PIECES(3)
                CALL E'WGS'EAMT
                IF IER1 = 0 RETURN                      !!Kludge
                WGS'PIECES(3) = VV
                IF INC'DZ'PIECES # "N" THEN
                        XXX'USR'BQTY = (WGS'DOZENS(3)*12) + WGS'PIECES(3)
                ELSE
                        XXX'USR'BQTY = WGS'PIECES(3)
                ENDIF
                CALL E'BO'QTY                           !!LINWGA.CPY
                CALL SET'WGS
        ENDIF
        GOTO EDIT'PIECES
RETURN

E'WGS'GKD:
        IF (SELECT(|NM|) = 1 OR SELECT(|NM|) = 2) AND   &
           (INX1 = I'RT OR INX1 = I'TAB OR INX2 = FN'5 OR INX2 = 255) THEN
                \TABC FROM="SUB" ON="ATT" BAD1="ATH'NO"         &
                        KFLD1="PRH'ATH'NO(1)" AKEY="CODE"       &
                        FLDNAM="SUB'NO" APID="IN" SMALL="Y"
                SUB'ATT'NO(1) = SUB'NO
        ENDIF
        IF (ARC'INV'CR'YN(HDR'DOC'TYPE) # "Y" OR &
           ARC'INV'DR'YN(HDR'DOC'TYPE) # "N" OR &
           OEC'TRAN'CR # "Y") AND INX1 = I'RT CALL AVAILABLE
        CALL E'GKD      
RETURN

SET'WGS'DZPC:
        FOR II = 1 TO 3
                IF WGS'DOZENS(II) # 0 OR WGS'PIECES(II) # 0 THEN
                    IF INC'DZ'PIECES # "N" THEN
                        DZPC(II) = (ABS(WGS'DOZENS(II)) USING "###")+"/"+ABS(WGS'PIECES(II))
                    ELSE
                        DZPC(II) = WGS'PIECES(II)
                    ENDIF
                ELSE
                        DZPC(II) = ""
                ENDIF
        NEXT II
RETURN

E'WGS'EAMT:
        \EAMT TYPE="HDR'DOC'TYPE" FLDNAM=VV DOERR="GOTO"  &
                APID="AR" DO'TSTFUNC="N"
RETURN

LCL1 SAV'ADDING,B,1

SET'SUB'REC:
        SAV'ADDING = OE1WGA'ADDING
        OE1WGA'ADDING = 0
        CALL SET'SUB'REC2
        OE1WGA'ADDING = SAV'ADDING
RETURN

SET'SUB'REC2:
        SUB'SPACE = SPACE
        CALL SET'FROM'HIGHER|NM|
        \SETSUB CHECK'ADD="Y"
        SHS'CTL = SUB'CTL
        SHS'RCN = SUB'RCN
        TMP'CTL(|NM|) = SUB'CTL
        TMP'RCN(|NM|) = SUB'RCN
RETURN

COPY "WGAWGA",SCREEN="OE1WGA",ASCREEN="OE4WGA",NM="2"

CIF COMMENTS AND NOT COMMENTS

        THE FOLLOWING LABELS IN WGAWGA.CPY

                PRE'DSP'TOTALS:
                SET'ATT'CODE:
                PRE'DSP'DOZENS:
                DSP'DOZENSII:
                E'DOZENS:
                E'PIECES:
                UPDATE'DOZENS'PIECES:
                WGA'EAMT:
                EDIT'DOZENS:
                EDIT'PIECES:
                SET'COM'QTY:
                SET'EXCEEDS:
                SHOW'BO'S:
ENDCIF

MAP1 OE1WGA'ADDING,B,1

E'ADD|NM|:
        IF INX1 = I'RT THEN OE1WGA'ADDING = 1
        GOTO DO'E'ADD
RETURN

DELETE'IT|NM|:
        IF SELECT(|NM|) = 1 OR SELECT(|NM|) = 2 THEN OE1WGA'ADDING = 1
        GOTO DO'DELETE'IT
RETURN

