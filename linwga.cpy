! @#$^&* 106 34 3829 0 0 0 0 0 0 0 0 0 6198 *&^$#@ 
!!*Updated on 15-Apr-96 at 5:30 PM by Eric Eberhard; edit time: 26:55:08
!!LINWGA,8.0[99]
!!VUE-like Transactional copy file for LINFIL
!!              Single file File I/O Module for full data entry
!!
!!                       V. I. C. S.
!!                    of Camp Verde, AZ
!%W% %G% %U% - SCCS

IF 1 = 0 ? "VERSION %W%: %G%"
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
!!       Vertical Integrated Computer Systems, Camp Verde, AZ  USA 
!!       All rights reserved.   
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
!![0] 04/12/90 ESE      Created special .CPY file.  See LINSUB for
!!                      common changes.
!!
!![13] 07/25/90 ESE     Log changes in price.
!!
!![14] 07/30/90 KJS     Added RPROC question if doing credit.
!!
!![15] 09/13/90 ESE     Skip location nicely.
!!
!!      Search for '!!MODIFY:' after initial generation!!
!!      Modify other things at your own risk.
!!      Watch out if you mess with cursor positioning.  (Veeeeery Tricky)
!!
!![16] 09/14/90 ESE     Make location default correctly ...
!!
!![17] 09/18/90 ESE     Use our macro to put on QUEUE. 
!!
!![18] 10/04/90 BAP     Modified LINP, LHSP, and Added LHSC and LHSD keys
!!
!![19] 10/20/90 HLB     ESE made changes to chain to OE1SHP for picking
!!                      tickets.  Trying to make it work for non-COD
!!
!![20] 11/14/90 HLB     Mapped unmapped variables
!!                      Also fixed a wrong access to OEC'UP'BUCKET and
!!                      changed MASKSET to 4.
!!
!![21] 11/14/90 HLB     Unable to enter backorders because of mixup between
!!                      OE1WGS.S AND OE2WGS.S
!!
!![22] 11/14/90 HLB     Changed DO'PRICE to work if not using 100 dozen 
!!                      pricing or not using cash pricing
!!
!![23] 11/27/90 KJS     Fixed single size & 0 quantity problem.
!!                      Fixed change price & tab problem (pieces only mode).
!!
!![24] 12/05/90 HLB     Changed OELOST default to Order available/backorder
!!
!![25] 12/19/90 ESE     Optionally display quantity available and also
!!                      Cache ATT'REC.
!!
!![26] 01/16/91 KJS     Fixed price computation problem if not using dz/pc
!!
!![27] 01/31/91 KJS/ESE If u changed price on case pricing then net was bad.
!!
!![28] 02/25/91 KJS     Fix net problem.
!![29] 03/07/91 KJS     Fixed net problem, again.
!![30] 03/14/91 KJS     Removed LWH stuff.  Fixed the net problem for real!
!!
!![31] 03/21/91 BAP     Fixed unmapped OEC variables.
!!
!![32] 04/05/91 ESE/KJS Added OEC'SKIP'UP flag
!!
!![33] 04/24/91 ESE     Dangerous code to CACHE qty records.
!!
!![34] 04/29/91 ESE     Bugs fixed in above.
!!
!![35] 05/10/91 ESE     Speed tricks.
!!
!![36] 06/14/91 HLB     Log price change messages according to OEC'MSG'LOG
!!
!![37] 06/17/91 ESE     Fix subscript out of range in case when B/O used.
!!
!![38] 06/19/91 ESE     Fix bad old QTY?
!!
!![39] 07/10/91 ESE     Add link to OE1SHP ability (KJS's idea).
!!
!![40] 07/14/91 ESE     Pass PRH array to OE1SHP.
!!
!![41] 07/19/91 ESE     Remove edits [33] and [34].
!!
!![42] 07/25/91 ESE     Fix totals problem when out of stock.
!!
!![43] 08/20/91 KJS     Fixed invalid invoice problem when selling cases,
!!                      using dz/pcs, and changing price.
!!
!![44] 08/23/91 ESE     Use new VICQTY.SBR and some clean-ups.
!!
!![45] 08/26/91 HLB     Problem with availability lookup in [44] 
!!                      PRH'RCN was not always set.
!!
!![46] 08/26/91 ESE     Added short pricing calculation for garment
!!                      interface.
!!
!![47] 08/27/91 HLB     Bug in garment interface when changing parts
!!                      on an already existing line.
!!
!![48] 08/27/91 ESE     More of above.
!!
!![49] 08/29/91 HLB     [46] Broke case pricing
!!
!![50] 09/03/91 ESE     Speed tricks
!!
!![51] 09/20/91 ESE     Some "INVALID INVOICE" probs fixed, some
!!                      bugs in [50] fixed.
!![52] 10/14/91 ESE     Odd size not displaying.  Some speed tricks.
!!
!![53] 10/30/91 ESE     Invalid invoices
!!
!![54] 11/05/91 ESE     Allow more space for part # if not multi-loc.
!!
!![55] 11/06/91 ESE     Pricing for special customer type pricing wrong
!!                      because of one of the speed tricks.
!![56] 11/06/91 ESE     More special pricing fixes.
!!
!![57] 11/07/91 ESE     Would not change return flag correctly on func-z.
!!
!![58] 12/04/91 ESE     Remove unneeded code.
!!
!![59] 12/10/91 ESE/HLB Cannot add line number zero any more.
!!
!![60] 12/26/91 ESE     Display correct location in multi-location orders.
!!
!![61] 01/27/92 ESE     Check cash terms correctly.
!!
!![62] 01/29/92 ESE     Make so that F4 to cancel still requires and order
!!                      quantity ....
!!
!![63] 02/10/92 ESE     Option to not discount cases
!!
!![64] 02/20/92 HLB     If not using dozens and pieces, do not assume that
!!                      the "middle" price is for a dozen.  This done to
!!                      facilitate "6-packs".
!![65] 02/20/92 ESE     Override customer type added.
!!
!![66] 02/25/92 ESE     Make sure prices round out to piece price ...
!!
!![67] 03/02/92 ESE     Make override type not override special types.
!!
!![68] 03/10/92 ESE     Make cancel/replace invoice directly.
!!                      Make 100 dz. special ...
!!
!![69] 03/11/92 ESE     Remove all discount calculations.
!!
!![70] 03/11/92 ESE     Use new PRH attribute caching.
!!
!![71] 04/03/92 ESE     On type 1200-1299 always use the special price.
!!
!![72] 04/17/92 ESE     Fix invalid invoice in some discount cases.
!!                      Also made 100dz price not discount if not
!!                      discounting cases.
!!
!![73] 04/21/92 ESE     Do not print pick ticket on internal credit.
!!
!![74] 05/07/92 ESE     Did not update QTY properly if change from order
!!                      to quote.
!!
!![75] 07/08/92 HLB     FIX: If you are using dozens and pieces and you
!!                      enter more than 12 pieces on a credit, the price
!!                      was calculating wrong.  
!!
!![76] 07/30/92 ESE     FIX:  rounding error if casepricing w/discount.
!!
!![77] 08/11/92 ESE     FIX:  if started with 0 pieces of an item and then
!!                      changed it to have some pieces and then changed
!!                      it back to zero - all without leaving the field -
!!                      then it would set the most previously non-zero
!!                      size to zero.  If you did not understand that,
!!                      assume it is very rare indeed and forget it!
!!
!![78] 11/06/92 ESE     7EW:  Enable auto double x-large pricing and also
!!                      disable VICQTY.SBR.
!!
!![79] 11/06/92 ESE     NEW: High number of cases special price.
!!
!![80] 12/16/92 ESE     FIX:  subscript out of range.
!!
!![81] 01/13/93 ESE/HLB FIX:  backorders if using dozens/pieces.
!!
!![82] 10/08/93 ESE     NEW: make prototype messages work.
!!
!![83] ??/??/?? ???
!!
!![84] 10/22/93 ESE     FIX: overflow of GOSUB stack.
!!
!![85] 11/16/93 ESE     Fix: pricing for 100 dz price if no case price.
!!
!![86] 11/24/93 HLB     FIX: If OEC'CALC'YN = "N" and running on a UNIX
!!                      system and not doing sales tax by location, the
!!                      sales tax would not calculate.
!!
!![87] 02/03/94 ESE     NEW: another variation on "20" case pricing.
!!
!![88] 05/04/94 ESE     NEW: check even piece pricing if changed.
!!
!![89] 08/23/94 ESE     FIX: spurious even dozens price error message.
!!
!![90] 12/05/94 HLB     NEW: Use the option to force dozens display.
!!
!![91] 12/14/94 HLB     NEW: If using option to force dozens display, and
!!                      overriding prices, allow entry of dozens price rather
!!                      than for actual UOM.
!!
!![92] 01/06/95 ESE     NEW: Implement "slip" pricing (see OEC'SLIP'PRICE).
!!
!![93] 08/10/95 HLB     FIX: If add/change/delete line, update last
!!                      update user and date on header.
!!
!!7.4   09/30/95 CAE    New version created
!!
!![94] 04/15/96 ESE     NEW: some screen print stuff.
!!
!![95] 03/17/97  ESE   NEW: clean up HPRICE display.
!!
!!8.0  03/31/97 CAE     New version created
!!
!![96] 03/31/97 CAE     NEW: Changes for year 2000.
!!
!![97] 04/11/97 ESE/CAE FIX: Sales tax was not always being computed
!!
!![98] 05/15/97  ESE   NEW:  Use ADDL
!!
!![99] 05/19/97  ESE   FIX:  Fix location display problem
!!
!  8.272  03/17/21 ese      Fix "cannot reserve" code and some cosmetices
!  8.273  11/15/21 cae      Copyright
!  8.274  06/11/22 ese      Made so totals come out even when USR'OQTY is
!                           not set
!  8.275  09/22/23 j5724    Don't allow non Lead parts on quotes with
!                           Lead flag set to Y
!  8.276  06/08/24 j5724    Change save RCNs to B,4
!  8.277  04/21/25 j4878    Stahl's Quantity break fractional pricing changes
!  8.278  07/03/25 j4878    Update Quantity Break Level pricing for additional levels 100 and 250 Removed display of QBK
!                           level to fix the UOM Ttles for the QBK pricing on the detail line
!  8.279  07/06/25 j4878    Fix buand QBK pricing display
!  8.280  08/19/25 j5512    add logic so that best price between quote, tier and special price is selected
!  8.281  08/25/25 j5512    Modifications to allow program to find lowest
!                           price for customer
! !!COMMENT-MARKER!!LANGUAGE=BASIC!!    DO NOT REMOVE  DO NOT REMOVE
!!
!!Symbol Definitions
!!       LINSUB                 - Program name
!!       Line Items             - Key field one prompt
!!       LIN                    - DB prefix
!!       LINFIL.DBD             - AMOS file name of main data file
!!       Line Items             - File description
!!       LIN'HDR'LOC'NO         - Main key field one
!!       Y                      - If yes, company controlled file
!!       OELIN                  - Full DBID
!!
!!
!!
GLOBAL DSPPRH'DSPLEN="19"
!!
!!File Definitions
!!
        COPYF   "LINFIL"                        !!Data file name
        COPYF   "LINFIL",=ZZZ|NM|,NOAFR         !!Duplicate for read/write
        COPYF   "LINKEY"                        !!Primary key file name
        COPYF   "LINBKY"                        !!Secondary Key file
        COPYF   "LINPKY"
        \COPYF  TRM AR MODE="PRI"

!!      COPYF   "LINFIL",=LIN3,NOAFR
        \SIZEOF LIN
        MAP1 SAVE'ORIG'PRICE,S,25
        MAP1 ORIG'PRICE,S,25
        MAP1    LIN3'REC,X,|V''TMP|
        COPYF   "LINKEY"

        LCL1    APRICE(5),B,5
        LCL1    TPRICE(4),B,1
        MAP1    CLOSEOUT,S,20
        MAP1    AUTO'DXL,B,1
        MAP1    SPECIAL'ADD,B,1         !![35]
        MAP1    SPECIAL'LAST,B,2        !![35]
        MAP1    V''ADD'F,B,1            !![35]
                                        !![35]NOTE: HDRSUB changed to call
                                        !!UPDATE'2ND'KEYS2 after each 
                                        !!ADD'RECORD2 per this change.
        MAP1    ZZZ|NM|'RCN,B,4
        MAP1    SNET,B,5
        MAP1    CHG'QTY'KEY,B,1         !!Values: (need to compute available?)
                                        !!      0 = No change to key (no)
                                        !!      1 = QTY key changed (yes)
                                        !!      2 = Artificial change (yes)
                                        !!
        MAP1    MY'DATE,S,20
        MAP1    SHIP'PROMPT,S,4
        MAP1    PROM'PROMPT1,S,4
        MAP1    PROM'PROMPT2,S,5

        MAP1    BO'PROMPT,S,3
        LCL1    OPR'FLAG,B,1
        LCL1    LAST'LIN'NO,B,2
        MAP1    SHIPPING,B,1
        MAP1    SAV'RPROC,B,1
        MAP1    I'CAN'MAKE'QTY,B,5
        MAP1    I'ORDERED'QTY,B,5
        LCL1    SAV'PRI'NO1,B,2
        LCL1    SAV'PRI'NO2,B,2
        LCL1    SAV'PROM'DTT,B,3
        MAP1    LAST'TRK'NO,B,3
        MAP1    LAST'UOM'NO,B,2
        MAP1    LOCK'FLAG,B,1
        MAP1    LAST'RCN,B,4
        MAP1    CALC'HTOTAL,B,5
        MAP1    LOOP'LIN'FLAG,B,1
        MAP1    LAST'PRICE,B,5
        MAP1    LAST'DISC,B,5
        MAP1    SPG,B,1
        MAP1    CREDIT,S,14
        MAP1    DSP'NET,B,5
        CIF UP'CHARGES = "Y" THEN
            MAP1    TMP'ADD'UP'NEEDED,B,1
        ENDCIF
        LCL1    CASE'QQ,B,1
        LCL1    DZ'QQ,B,1
        MAP1    QDAYS,B,3

        COPYF   "XXXFIL"
        COPYF   "XXXFIL",=ZZZX,NOAFR
!!      COPYF   "XXXFIL",=ZZZXSAV
        \SIZEOF XXX
        MAP1    ZZZXSAV'REC,X,|V''TMP|
!!      COPYF   "XXXFIL",=XXX2
        MAP1    XXX2'REC,X,|V''TMP|

!!MODIFY: Other COPYF's , map type COPY's, and MAP statements go here.

        COPYF   "LINFIL",=LIN2,NOAFR
!!      COPYF   "SUBFIL",=SUB2,NOAFR
!!      COPYF   "SUBFIL",=SUB3,NOAFR
        \SIZEOF SUB
        MAP1    SUB2'REC,X,|V''TMP|
        MAP1    SUB3'REC,X,|V''TMP|
        COPYF   "STXFIL[IN]",NOAFR
        COPYF   "STXKEY[IN]",NOAFR
    COPYF       "MEAFIL[IN]",NOAFR
    COPYF       "MEAKEY[IN]",NOAFR

!!      MAP1    LWSSAV,X,402
        LCL1    SAVLIN,X,2592           !!Save area screen OE1LIN
        MAP1    SAFR2,S,166             !!Ditto (must match size of ENTRY)
        MAP1    NET,B,5                 !!NET price for display
        MAP1    AVAILABLE,B,5           !!Available inventory
        MAP1    PRICE,B,5               !!Default Price
        MAP1    DISC,B,5                !!Default Discount
        MAP1    RETRO,B,5               !!Retoactive discount
        LCL1    TPG,B,1                 !!Test page number
        MAP1    DELLOW,B,1              !!DELETE flag from lower level
        MAP1    DO'NXT,B,1              !!In DO'WINDOW
        MAP1    DDB'DONE1,B,1           !!FLAG
        MAP1    SRCN,B,4                !!SAVE RCN FOR FILL'BO
        MAP1    SBO'QTY,B,5             !!SAVED TO HELP DECIDE BO KEY UPDATES
        MAP1    GKD'F,B,1               !!WINDOW FROM A GKD OCCURED
        MAP1    SAV'PRICE,B,5
        MAP1    SAV'DISC,B,5
        MAP1    SAV'BO,B,5
        MAP1    SAV'COM,B,5
        MAP1    SHP'PRICE1,B,5
        MAP1    SHP'DISC1,B,5
        MAP1    SHP'PRICE2,B,5
        MAP1    SHP'DISC2,B,5
        MAP1    SAV'STX1ABLE,B,5
        MAP1    SAV'STX2TOTAL,B,5
        MAP1    TQTY,F
        MAP1    TUOM,B,2
        MAP1    BQTY,F
        MAP1    SIGN,F
        MAP1    COM'CNT,F
        MAP1    QQ,F
        MAP1    II,F
        MAP1    JJ,F
        MAP1    XP,F
        MAP1    XQ,F
        MAP1    XL,F
        MAP1    XX,F
        MAP1    XH,F
        MAP1    DOKITS'FLAG,B,1
        MAP1    LOOP'SUB'FLAG,B,1
        MAP1    HDR'SHP'LOC'NO,B,2
        MAP1    TAX'PERC,B,5
        MAP1    PRH'FORMATTED,S,29
        MAP1    SGDB'F,B,1
        MAP1    SAV'LIN'NO,B,2
        MAP1    SUB'FLAG,B,1            !!0 = NO SUBS
                                        !!1 = MEMO (NON-INVENTORIED) SUBS
                                        !!2 = SUBS THAT ARE INVENTORIED
        MAP1    SAVAILABLE,B,5
        MAP1    TMP'LOC'NO,B,2
        MAP1    TMP'QTY,B,5
        MAP1    TMP'UQTY,B,5
        MAP1    SMALL,X,2286    !!(WAS 720) - USED IN HDRSUB AS WELL (SAVE SCREEN)
                                !!AR1OEW MADE BIGGER
        MAP1    SHIP'FLG,B,1
        MAP1    BO'FLAG,B,1
        MAP1    AVAILABLE'EACH,B,5      !![25]

        MAP1    MOD'BASE,B,5    !!Used in \MOD macro call
MAP1 PART'DESC,S,71

INIT|NM|:                               !! Called once for each page

        TLR(|NM|) = 6                   !!MODIFY: Physical top row of edit area
        TLC(|NM|) = 3                   !!MODIFY: Physical top col of edit area
        BRR(|NM|) = VDSP'LIN - 2        !!MODIFY: Physical btm row of edit area
        BRC(|NM|) = 78                  !!MODIFY: Physical btm col of edit area 
        TSCR(|NM|) = VDSP'LIN           !!MODIFY: Physical top of display
        BSCR(|NM|) = 23                 !!MODIFY: Physical bottom of display
        SSCR(|NM|) = BSCR(|NM|) - TSCR(|NM|)    ! Size of screen - 1
        MSCR(|NM|) = INT(SSCR(|NM|)/2)  ! Middle of screen
        LST'GKD(|NM|) = 1               !!MODIFY: Last GKD field number
        GKD'START'FLD(|NM|)= 1          !!MODIFY: Field number to start on in GKD

RETURN

CIF UP'CHARGES = "Y" THEN
    MAP1 UP'HDR,S,21
ENDCIF
CIF USE'CAP = "Y" THEN
    MAP1 HIGH'COL,B,1
ENDCIF

PRE'SET'UP|NM|:                         ! Called at EVERY start of current page
        CONTROL'Z'FLAG = 0
        CALL TABPRH'CLEAR2
        BOTTOM'AFU = 0
CIF USE'CAP = "Y" THEN
        HIGH'COL = 0
ENDCIF
        CIF UP'CHARGES = "Y" THEN
            IF OEC'SKIP'UP # "Y" THEN
                UP'HDR = "Cumulative Up Charge:"
            ENDIF
        ENDCIF
        CALL SET'LINE'HDR1
        SPECIAL'ADD = 1                 !![35]
        SPECIAL'LAST = 0                !![35]
!![15] Moved here from OE1HDR.BV
        MAP1 SKIP'LOC,B,1
        IF SAV'LOC'BY'LIN = "T" AND HDR'PROC < 5 THEN
                SKIP'LOC = 1
                OEC'LOC'BY'LIN = "E"
        ELSE    
                SKIP'LOC = 0
                OEC'LOC'BY'LIN = SAV'LOC'BY'LIN
        ENDIF
!![15] Above
        CIF UP'CHARGES = "Y" THEN
            IF ARCHIVE'FLAG = "C" THEN ADD'UP'NEEDED = 1
        ENDCIF
        WGA'SPACE = SPACE
        UOM'TITLES'SPACE = SPACE(50)
        UOM2'TITLES'REC = UOM'TITLES'REC
        DP'SPACE = SPACE
        WGA'TITLES'SPACE = SPACE(50)
        WGA2'TITLES'REC = WGA'TITLES'REC

!![WC] Clear Quantity Break Titles
        CALL CLEAR'QBK'TITLES

        REAL'PG = PG

        GKD'FLG(|NM|) = 1               ! Will cause gkd to be skipped during
!!                                      ! Mode after initial return
        IF IN'FLAG # 1 OR CMN'SAV'RCN(2) = 0 THEN
                IF ARCHIVE'FLAG = "C" AND SHIPPING # 1 THEN
                        RPT'FLG(|NM|) = 0       ! Set to 1 if this is a report screen
                ELSE
                        RPT'FLG(|NM|) = 2
                ENDIF
        ELSE
                RPT'FLG(|NM|) = 5
                RPT'RCN(|NM|) = 2
                TMP'RCN(|NM|) = CMN'SAV'RCN(2)
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
!!      TOGGLE'BTM(|NM|) = 1

!!      DFLT'SCHEMA(|NM|) = ##          ! Set to default schema number

!!      DFLT'HEADER(|NM|) = ##          ! Set to default schema header number


        WIN'F = 0

        CALL OTHERS'GAFR2
YANK'SETUP:
        IF ARCHIVE'FLAG = "C" THEN
                CALL FIND'ZONE                          !!HDRSUB.CPY
!![83]          LOC'KNO = HDR'LOC'NO
!!              CALL FIND'LOC
!!              IF CUS'LOC'NO = 0 THEN                  !![16]
!!                      HDR'SHP'LOC'NO = LOC'SHP'LOC'NO
!!              ELSE
!!                      HDR'SHP'LOC'NO = CUS'LOC'NO     !![16]
!![83]          ENDIF
!![83] - see edit [102] in HDRSUB.CPY
                TAX'PERC = 0
                CALL GET'SF'TAXABLE
                IF CUS'TAXABLE = "Y" OR CUS'TAXABLE = "S" OR SF = 0 THEN
                        STX'KLOC'NO = HDR'LOC'NO
                        STX'KNO = CUS'STX'NO
                        TSTTAX = "S"
                        CALL GET'STX
                        IF STX'CTL # 0 THEN
                                ENTRY = "WARNING:  Customer tax code invalid: "+STX'KLOC'NO+"-"+STX'KNO
                                CALL DO'ERR
                        ELSE
                                TAX'PERC = TTL'STX'PERC
                        ENDIF
                ENDIF
        ENDIF

        IF ARCHIVE'FLAG # "S" THEN
                ARCHIVE'SWAP = "Line Archives"
        ELSE
                ARCHIVE'SWAP = "Finish Archive"
        ENDIF
        CALL GET'CASH'TERMS
RETURN


        MAP1 TOP'TRX'FIELD|NM|,B,2      !!MODIFY: Change to suit trx'field
        MAP1 BTM'TRX'FIELD|NM|,B,2      !!MODIFY: Change to suit trx'field
        MAP1 LAST'TRX'FIELD|NM|,B,2     !!MODIFY: Change to suit trx'field

SYMBOL  TRX'FIELD  = "LIN'NO"           !!MODIFY:
SYMBOL  TRX'STEP = "10"                 !!MODIFY: Transaction step > 0
SYMBOL  HIGH'KEY = "9999"       !!MODIFY: to highest legal key value
                                        !!        or will get error in 
                                        !!        prepare to add
SYMBOL SCREEN = "OE1WGA"                !!MODIFY: Primary screen name
SYMBOL ASCREEN = "OE4WGA"               !!MODIFY: Primary screen name

SYMBOL DSP        = "DSP'LIN"   ! Display transaction field over GKD
                                ! May have secondary if two key fields
SYMBOL VDSP       = "VDSP'LIN"  ! Must be primary display field in scan 
                                ! display row, must start in column 3 or >,
                                ! normally followed by one or more secondary
                                ! display fields
SYMBOL VDSP'MSG = "VDSP'LIN'MSG"        ! display message flag field

!!MODIFY:  add symbol statements if more than 8 keys for level, remove
!!         comment marks as needed

        COPYS "|SCREEN|"
        COPYS "|ASCREEN|"                               !![21]
        COPYS "OE2WGA"

ADD'RECORD|NM|:
        FN'K'RCN = 0
        IF TOP'MARK # 0 OR YANK'FLAG # 0 THEN
                CALL DO'PRICE
        ENDIF
        LIN'MSGNO = 0           !!LIN'MSGNO message processing
        CIF V''ADDL # "Y" THEN
            ADD LIN FROM LIN'REC KEY LIN'KEY
        CELSE
            ADDL LIN FROM LIN'REC KEY LIN'KEY
        ENDCIF
    ADDED:
        XXX'REC = LIN'XXX'OVR
        TMP'CTL(|NM|) = LIN'CTL
        TMP'RCN(|NM|) = LIN'RCN
        IF TMP'CTL(|NM|) = 0 THEN
                HDR'SHP'DONE = "N"
                HDR'PICK'NEED = "Y"
                ZZZ|NM|'SPACE = SPACE           !!TRICK UPDATE'QTY'REC
                ZZZX'SPACE = SPACE              !!DITTO
                IF XXX'REC # LIN'XXX'OVR THEN
                                                !!In case UPDATE'QTY'REC
                                                !!changed values.
                    CIF V''ADDL # "Y" THEN
                        RDLOCK LIN              !!MUST LOCK HERE
                    ENDCIF
                                                !!CANNOT CALL PRE'READ
                    LIN'XXX'OVR = XXX'REC
                    CIF V''ADDL # "Y" THEN
                        WRITE LIN               !!CANNOT CALL PRE'WRITE
                    CELSE
                        UPDATE LIN         !![98] CANNOT CALL PRE'UPDATE
                    ENDCIF
                ENDIF
                CALL SET'2ND'KEYS|NM|
!![35]          IF LIN'TRK'NO # 0 THEN ADDKEY LINP
!![35]          IF XXX'BO'QTY # 0 THEN ADDKEY LINB
                V''ADD'F = 1                            !![35]
                CALL ADDCOUNT|NM|
                CALL ZSAVAMT|NM|
                CALL ADDTOTAL|NM|
!!
!!Below occurs in mark add while in LIN level
!!
                IF REAL'PG = |NM| AND TOP'MARK # 0 CALL PRE'DSP'TOTALS
        ENDIF
RETURN

LOCAL ADDKEY'SKIP1                                              !![35]
LOCAL ADDKEY'SKIP2                                              !![35]

UPDATE'2ND'KEYS|NM|:
!!      \UPDKEY PRE="LIN" MODE="KY2"
        IF ARCHIVE'FLAG # "C" RETURN
!![18]BELOW
!!      IF LINP'KLOC'NO # LIN'LOC'NO OR LINP'KTRK'NO # LIN'TRK'NO THEN
        IF V''ADD'F = 1 GOTO ADDKEY'SKIP1                       !![35]
        IF LINP'KLOC'NO # LIN'LOC'NO OR LINP'KTRK'NO # LIN'TRK'NO OR &
           LINP'KPROM'DTT # LIN'PROM'DTT THEN
!![18]ABOVE
                IF LINP'KTRK'NO # 0 THEN DELKEY LINP
            ADDKEY'SKIP1:                                       !![35]
                IF LIN'TRK'NO # 0 THEN
                        \SETKEY PRE="LINP" MODE="KEY"
                        ADDKEY LINP
                ENDIF
        ENDIF
        IF V''ADD'F = 1 THEN                                    !![35]
                V''ADD'F = 0                                    !![35]
                GOTO ADDKEY'SKIP2                               !![35]
        ENDIF                                                   !![35]
        IF LINB'KTRK'NO # LIN'TRK'NO OR LINB'KPRI'NO1 # LIN'PRI'NO1 OR  &
                        LINB'KPROM'DTT # LIN'PROM'DTT               OR  &
                                LINB'KPRI'NO2 # LIN'PRI'NO2         OR  &
                                (SBO'QTY = 0 AND XXX'BO'QTY # 0)    OR  &
                                (SBO'QTY # 0 AND XXX'BO'QTY = 0)        THEN
                IF SBO'QTY # 0 THEN DELKEY LINB
            ADDKEY'SKIP2:                                       !![35]
                IF XXX'BO'QTY # 0 THEN
                        \SETKEY PRE="LINB" MODE="KEY"
                        ADDKEY LINB
                ENDIF
        ENDIF
RETURN

PRE'SET'USE|NM|:
!!MODIFY: use one of the below pairs of lines:
!!        Use as is for numeric only, remove 1st pair and enable second pair 
!!        for alpha and numeric

        CALL SET'KEY|NM|
!!      CALL USE|NM|
!!
!!      ON KEY'TYPE(|NM|)+1 CALL SET'KEY|NM|,SET'KEY|NM|,SET'2ND'KEYS|NM|
!!      ON KEY'TYPE(|NM|)+1 CALL USE|NM|,USE|NM|,USE'2ND'KEY|NM|
!!RETURN

USE|NM|:
        IF ARCHIVE'FLAG = "C" THEN
                USE LIN INTO LIN'REC KEY LIN'KEY
                TMP'CTL(|NM|) = LIN'CTL
                TMP'RCN(|NM|) = LIN'RCN
        ELSE
                USE LHS INTO LIN'REC KEY LHS'KEY
                TMP'CTL(|NM|) = LHS'CTL
                TMP'RCN(|NM|) = LHS'RCN
        ENDIF
!!      TOTAL'FLAG = 1
RETURN

!!MODIFY: Remove comments below and adjust LIND to secondary key name for
!!        alphabetic display area if used
!!
!!USE'2ND'KEY|NM|:
!!      USE LIND INTO LIN'REC KEY LIND'KEY
!!      TMP'CTL(|NM|) = LIND'CTL
!!      TMP'RCN(|NM|) = LIND'RCN
!!RETURN

PRE'GETNXT|NM|:
!!MODIFY: pick one of the below lines:
!!        Use as is for numeric only, remove 1st line and enable second line 
!!        for alpha and numeric

!!              CALL GETNXT|NM|
!!              ON KEY'TYPE(|NM|)+1 CALL GETNXT|NM|,GETNXT|NM|,GETNXT'2ND'KEY|NM|
!!RETURN

GETNXT|NM|:
        IF ARCHIVE'FLAG = "C" THEN
                GETNXT LIN
                TMP'CTL(|NM|) = LIN'CTL
                TMP'RCN(|NM|) = LIN'RCN
        ELSE
                GETNXT LHS INTO LIN'REC
                TMP'CTL(|NM|) = LHS'CTL
                TMP'RCN(|NM|) = LHS'RCN
        ENDIF
        XXX'REC = LIN'XXX'OVR
!!      TOTAL'FLAG = 1
RETURN

!!MODIFY: Remove comments below and adjust LIND to secondary key name for
!!        alphabetic display area if used
!!
!!GETNXT'2ND'KEY|NM|:
!!      GETNXT LIND
!!      TMP'CTL(|NM|) = LIND'CTL
!!      TMP'RCN(|NM|) = LIND'RCN
!!RETURN

PRE'GETPRV|NM|:
!!MODIFY: pick one of the below lines:
!!        Use as is for numeric only, remove 1st line and enable second line 
!!        for alpha and numeric

!!      CALL GETPRV|NM|
!!      ON KEY'TYPE(|NM|)+1 CALL GETPRV|NM|,GETPRV|NM|,GETPRV'2ND'KEY|NM|
!!RETURN

GETPRV|NM|:
        IF ARCHIVE'FLAG = "C" THEN
                GETPRV LIN
                TMP'CTL(|NM|) = LIN'CTL
                TMP'RCN(|NM|) = LIN'RCN
        ELSE
                GETPRV LHS INTO LIN'REC
                TMP'CTL(|NM|) = LHS'CTL
                TMP'RCN(|NM|) = LHS'RCN
        ENDIF
        XXX'REC = LIN'XXX'OVR
!!      TOTAL'FLAG = 1
RETURN

!!MODIFY: Remove comments below and adjust LIND to secondary key name for
!!        alphabetic display area if used
!!
!!GETPRV'2ND'KEY|NM|:
!!      GETPRV LIND
!!      TMP'CTL(|NM|) = LIND'CTL
!!      TMP'RCN(|NM|) = LIND'RCN
!!RETURN

!!MODIFY:  use below for full screen windowing of messages
!!
!!PROC'MSG|NM|:                                 !!LIN'MSGNO message processing
!!      ENTRY = "    Line Items Messages"               !!LIN'MSGNO message processing
!!      GETMSG LIN'MSGNO "OEMSG" 78                     !!LIN'MSGNO message processing
!!      \POPL25                                 !!LIN'MSGNO message processing
!!RETURN                                        !!LIN'MSGNO message processing

!!MODIFY:  delete below for full screen windowing of messages
!!         or if message field not MSGNO fix ZZZ|NM|'MSGNO
!!
PROC'MSG|NM|:                                   !!LIN'MSGNO message processing
        ENTRY = OEC'LIN'RULER
        LAST'TRX'FIELD1 = LIN'MSGNO
        IF PRH'PMSGNO # 0 THEN
                JJ = -PRH'PMSGNO
        ELSE
                JJ = OEC'LIN'MSGNO
        ENDIF
        CALL PROC'MSG'ALL
        LIN'MSGNO = LAST'TRX'FIELD1
RETURN                                          !!LIN'MSGNO message processing

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
!!                      LIN'DESC = ENTRY                !!MODIFY: Field name of 2nd key
!!                      KEY'TYPE(|NM|) = 2      !!flag as secondary ALPHA key
!!              ELSE
!!                      |TRX'FIELD| = ENTRY     !!Field name of 1st field in
!!                                              !!primary key
!!                      KEY'TYPE(|NM|) = 1      !!flag as primary NUMERIC key
!!              ENDIF
!!
!!      ELSE
!!      IF ECFLD = 2 THEN
!!              LIN'FLD2 = ENTRY                        !!Field name of 2nd field in
!!                                              !!primary NUMERIC key
!!      ELSE
!!      IF ECFLD = 3 THEN
!!              LIN'FLD2 = ENTRY                        !!Field name of 3rd field in
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
!!              LIN'DESC = ENTRY                !!MODIFY: Field name of secondary key
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
        IF LIN'HDR'LOC'NO # HDR'LOC'NO &
                        OR LIN'HDR'NO # HDR'NO THEN TMP'CTL(|NM|) = 3
        IF LIN'DOC'TYPE # HDR'DOC'TYPE THEN TMP'CTL(|NM|) = 3
        CASE
        IF ARCHIVE'FLAG = "A" THEN
                IF LIN'SHIPMENT # HDR'SHIPMENT THEN TMP'CTL(|NM|) = 3
        ELSE
        IF ARCHIVE'FLAG = "S" THEN
                IF LIN'TRK'NO # FLIP'TRK'NO THEN SF = 1
        ENDCASE

!!MODIFY: change as below to reject certain records during scan
!!
!! TO EXIT SEARCH (pseudo EOF):
!!
!!      IF LIN'BAD1 # LIN'KBAD1 OR LIN'BAD2 # LIN'KBAD2 THEN
!!              TMP'CTL(|NM|) = 3
!!      ENDIF
!!
!! TO SKIP A SINGLE RECORD:
!!
!!      IF LIN'BAD1 = "BAD'CASE" THEN SF = 1
!!
RETURN

PRE'DSP|NM|:

!!MODIFY:  Add lookups, date formatting, etc. prior to CALL |DSP| 

        IF SPG # 0 OR PG # REAL'PG RETURN
        CIF UP'CHARGES = "Y" THEN
            IF SELECT(|NM|) = 1 OR SELECT(|NM|) = 2 THEN
                IF OEC'SKIP'UP # "Y" CALL CLEAR'UP'CHARGE
            ELSE
                IF ARCHIVE'FLAG = "C" THEN
                        IF OEC'SKIP'UP # "Y" THEN
                                CALL FIND'PRH
                                CIF UP'CHARGES = "Y" THEN
                                    CALL SHOW'UP'CHARGE
                                ENDCIF
                        ENDIF
                ENDIF
            ENDIF
        ENDCIF
        CALL |DSP|
RETURN

!!MODIFY: if allowing F1,F3 or F6 to change records in a window, you must 
!!        do similar to following in action of window call:  
!!              (see notes above and in DO'WINDOW)
!!      
!!ACTION:
!!      IN'WIN'FLAG = 1
!!      DSP WINDOW
!!      BSAV'RCN = LIN'RCN
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
!!                      READ record in winodw   !! NEW RECORD
!!                      GOTO AC1
!!              ENDIF
!!              IF SCR'TOP(|NM|) = BSAV'SCR'TOP AND SCR'TOP(|NM|) + BSAV'SXLN < |SCR'MAX| THEN
!!                      IF SCR'RCN(|NM|,SCR'TOP(|NM|) + BSAV'SXLN) = BSAV'RCN THEN TOGGLE'BUDGETS = 0
!!              ENDIF
!!      ENDIF
!!RETURN

PRE'VDSP|NM|:
        IF TOGGLE'BTM(|NM|) = 0 AND SPG = 0 AND PG = REAL'PG AND BOTTOM'AFU = 0 THEN

!!MODIFY:  Add lookups, date formatting, etc. prior to CALL |VDSP| 
                IF SELECT(|NM|) = 1 AND LIN'ORDER'QTY = 0 THEN  !![50]
                        ? TAB(-1,14);TAB(VDSP'LIN+XLN,3);"  (ADD MODE)";TAB(-1,9);
                        CALL VDSP'LIN'MSG
                        RETURN                                  !!KLUDGE
                ENDIF
                CALL FIND'PRH
                CALL DO'DSPPRH
                IF INC'PRH'MODE = 1 THEN PRH'FORMATTED[9,-1] = PRH'DESC
                LOC'KNO = LIN'LOC'NO                    !![60]
                CALL FIND'SDESC                         !!HDRSUB.CPY !![60]
                IF INC'MULTI'LOC = "Y" THEN             !![54] - put in LOC
                    IF ENVSTR[21;1] = "Y" THEN
                        \STRIP PRH'FORMATTED 8
                    ENDIF
                    IF OC2'MAXLOC'DSP < 1 OR OC2'MAXLOC'DSP > 4 THEN OC2'MAXLOC'DSP = 4
                    ENTRY = LOC'SDESC+SPACE(5)
                    PRH'FORMATTED = ENTRY[1,OC2'MAXLOC'DSP] + " " + PRH'FORMATTED
                ENDIF
                CIF UP'CHARGES = "Y" THEN
                    TMP'ADD'UP'NEEDED = ADD'UP'NEEDED
                    ADD'UP'NEEDED = 0
                ENDCIF
                CALL LOAD'WGA
                CIF UP'CHARGES = "Y" THEN
                    ADD'UP'NEEDED = TMP'ADD'UP'NEEDED
                ENDCIF
                FOR II = 1 TO 6
                        IF WGA'DOZENS(II) # 0 OR WGA'PIECES(II) # 0 THEN
                            IF INC'DZ'PIECES # "N" THEN
                                DZPC(II) = (ABS(WGA'DOZENS(II)) USING "###")+"/"+ABS(WGA'PIECES(II))
                            ELSE
                                DZPC(II) = WGA'PIECES(II) USING "######"
                            ENDIF
                        ELSE
                            DZPC(II) = ""
                        ENDIF
                        IF WGA'BDOZENS(II) # 0 OR WGA'BPIECES(II) # 0 THEN
                            BO(II) = "*"
                        ELSE
                            BO(II) = " "
                        ENDIF
                NEXT II
                IF ENVSTR[21;1] = "Y" THEN
                    \STRIP PRH'FORMATTED 7
                ENDIF
                IF DZPC(1) # "" THEN
                    PRH'FORMATTED = PRH'FORMATTED + SPACE(30)
                    PRH'FORMATTED = PRH'FORMATTED[1;17] + " " + &
                                             BO(1) + (DZPC(1) USING "######")
                ENDIF

                IF ARCHIVE'FLAG = "C" THEN
                        DSP'NET = LIN'NET
                ELSE
                        CALL COMPUTE'NETS
                        DSP'NET = CNETP                 !![69]
                ENDIF
MAP1 SDSP'NET,S,10
MAP1 DSP'LIN'NO,B,2
                SDSP'NET = (DSP'NET/100 USING "##,###.##-")
                DSP'LIN'NO = LIN'NO
                CALL |VDSP|
                LOC'SDESC = SAV'LOC'SDESC
                CALL PRE'VDSP'MSG|NM|                   !!LIN'MSGNO message processing
        ENDIF
RETURN

PRE'VDSP'MSG|NM|:
        IF TOGGLE'BTM(|NM|) = 0 AND SPG = 0 AND PG = REAL'PG AND BOTTOM'AFU = 0 THEN    !!LIN'MSGNO message processing
                IF LIN'MSGNO # 0 THEN MSG'FLAG = "*"    !!LIN'MSGNO message processing
                CALL |VDSP'MSG|                         !!LIN'MSGNO message processing
                MSG'FLAG = " "                          !!LIN'MSGNO message processing
        ENDIF                                           !!LIN'MSGNO message processing
RETURN

DEL'2ND'KEYS|NM|:
!!MODIFY: normally needed only if special key rebuild is used
        IF V''ADD'F = 0 THEN                                    !![35]
                CALL SET'2ND'KEYS|NM|
                \DELKEY PRE="LIN" MODE="KY2"
        ENDIF
RETURN

SET'KEY|NM|:
!!MODIFY: normally no changes needed
        IF ARCHIVE'FLAG = "C" THEN
                \SETKEY PRE="LIN" MODE="KEY"
        ELSE
                \SETKEY PRE="LHS" MODE="KEY" ALTPRE="LIN"
        ENDIF
RETURN

SET'2ND'KEYS|NM|:
!!MODIFY: normally no changes needed
!!        if modified must also modify ADD'RECORD|NM|
!!
        IF ARCHIVE'FLAG = "C" THEN
                \SETKEY PRE="LIN" MODE="KY2"
                SBO'QTY = XXX'BO'QTY
        ENDIF
RETURN

DO'DUP|NM|:
!!MODIFY: Add any duplicating logic here
        WGA'SPACE = SPACE
        TMP'RCN(|NM|) = 0
        ZZZ|NM|'LOC'NO = LIN'LOC'NO
        ZZZ|NM|'TRK'NO = LIN'TRK'NO
        LIN'SPACE = SPACE       !! clear record
        LIN'PRI'NO1 = HDR'PRI'NO1
        LIN'PRI'NO2 = HDR'PRI'NO2
        LIN'PROM'DTT = HDR'PROM'DTT
        ENTRY = "Type"+STR(HDR'DOC'TYPE)+"LOC_NO"
        SAV'LIN'NO = HDR'SHP'LOC'NO
        \DG GETVALUE _OESETUP ENTRY HDR'SHP'LOC'NO TAGVAR="Y"
        IF DG'CTL = 0 AND HDR'SHP'LOC'NO # 0 THEN
            HARD'LOC'NO = 2
            ZZZ|NM|'LOC'NO = HDR'SHP'LOC'NO
        ELSE
            HDR'SHP'LOC'NO = SAV'LIN'NO
        ENDIF
        IF HDR'PROC <= 5 THEN
                IF HARD'LOC'NO # 0 THEN
                    LIN'LOC'NO = ZZZ|NM|'LOC'NO
                ENDIF
                IF LIN'LOC'NO = 0 THEN  !!OR HARD'LOC'NO = 0 THEN
                    LIN'LOC'NO = HDR'SHP'LOC'NO
                ENDIF
        ELSE
                LIN'LOC'NO = ZZZ|NM|'LOC'NO
                IF LIN'LOC'NO = HDR'LOC'NO THEN
!!If first line item this stops the problem of defaulting to an invalid LOC'NO
                        LOC'KNO = 0
                        RDKEY LOC
                        GETNXT LOC
                        LOC'KEY = LOC'KEY2
                        IF LOC'KNO = HDR'LOC'NO THEN
                                GETNXT LOC
                                LOC'KEY = LOC'KEY2
                        ENDIF
                        LIN'LOC'NO = LOC'KNO
                ENDIF
        ENDIF
        NET = 0
        LIN'HDR'LOC'NO = HDR'LOC'NO
        LIN'DOC'TYPE = HDR'DOC'TYPE
        LIN'HDR'NO = HDR'NO
        LIN'HOLD'YN = "N"
        LIN'SUB'COUNT = 0
        LIN'CMG'NO = HDR'CMG'NO
        LIN'LENGTH = 1
        LIN'HEIGHT = 1
        LIN'WIDTH = 1
        LIN'UOM'NO = 1
        LIN'TRK'NO = ZZZ|NM|'TRK'NO
        IF AUTO'DXL = 1 OR AUTO'DXL = 99 THEN
            LIN'TRK'NO = AUTO'TRK'NO
            LIN'LOC'NO = ZZZ|NM|'LOC'NO
            AUTO'DXL = 2
        ELSE
        IF MAPZAP'STYLEX'LEN # 0 THEN
            PRH2'KTRK'NO = LIN'TRK'NO
            \VUSE PRH2 KTRK'NO PASSED'PRH'ARRAY AMOS="Y"
            ENTRY = PRH2'PART
            PRH2'PART[MAPZAP'STYLEX'POS;MAPZAP'STYLEX'LEN] = SPACE(20)
            IF PRH2'PART # ENTRY THEN
                \SETKEY PRHP MODE="KEY" APID="IN" ALTKEY="PRHP2" ALTPRE="PRH2"
                USE PRHP2 INTO PRH2'REC KEY PRHP2'KEY
                IF PRHP2'CTL = 0 THEN
                    LIN'TRK'NO = PRH2'TRK'NO
                    LIN'LOC'NO = ZZZ|NM|'LOC'NO
                ENDIF
            ENDIF
        ENDIF
        ENDIF
        IF AUTO'DXL # 2 THEN AUTO'DXL = 0
        IF LIN'LOC'NO = 0 THEN
            LIN'LOC'NO = HDR'SHP'LOC'NO
        ENDIF
        XXX'REC = LIN'XXX'OVR
        SAVE'ORIG'PRICE = ""
        IF PG = REAL'PG CALL DSP'ORIG'PRICE
RETURN

SET'HIGH'VALUES|NM|:
!!MODIFY: change as needed for multiple field keys
!!        NOTE:  set data fields, not key fields
        LIN'HDR'LOC'NO = HDR'LOC'NO
        LIN'DOC'TYPE = HDR'DOC'TYPE
        LIN'HDR'NO = HDR'NO
        |TRX'FIELD| = |HIGH'KEY|
        CASE
        IF ARCHIVE'FLAG = "A" THEN
                LIN'SHIPMENT = HDR'SHIPMENT
        ELSE
        IF ARCHIVE'FLAG = "S" THEN
                LIN'SHIPMENT = 255
        ENDCASE
RETURN

SET'LOW'VALUES|NM|:
!!MODIFY: needed when using batch and transaction type files where batch
!!        would remain as a constant
!!        NOTE:  set data fields, not key fields
        LIN'SPACE = SPACE
        LIN'HDR'LOC'NO = HDR'LOC'NO
        LIN'DOC'TYPE = HDR'DOC'TYPE
        LIN'HDR'NO = HDR'NO
        IF ARCHIVE'FLAG = "A" THEN LIN'SHIPMENT = HDR'SHIPMENT
RETURN

PRE'ABORT|NM|:
!!      IF ARCHIVE'FLAG = "C" AND CTRLC'ABORT = 0 THEN
!!              SNDAFR = ""
!!              IF SELECT(1) = 1 OR SELECT(1) = 2 THEN
!                       IF HDR'LIN'COM # 0 THEN SNDAFR[1;1] = OEC'SHP'ADD
!                       IF HDR'PROC < 5 THEN SNDAFR[2;1] = OEC'CASH'ADD
!               ELSE
!                       IF HDR'LIN'COM # 0 THEN SNDAFR[1;1] = OEC'SHP'CHG
!                       IF HDR'PROC < 5 THEN SNDAFR[2;1] = OEC'CASH'CHG
!               ENDIF
!               IF SNDAFR[2;1] = "Y" CALL DO'WINDOW'FN'G
!               IF SNDAFR[1;1] = "Y" AND HDR'QUOTE'YN # "Y" THEN
!                       IF SHIPPING = 0 CALL SHIPPING'ON
!                       WINDOW = FN'P
!                       CALL PROC'CHAIN
!               ENDIF
!!      ENDIF
        CALL TEST'FN'P
PRE'ABORT2|NM|:
        CAPTTL = 0
        SAV'CAPTTL = 0
        LIN'RCN = 0
        CALL MARK'CLR           !! Clear prior block marks
        CALL SAS                !! SET'ARCHIVE'SWAP
        IF HDR'LIN'COUNT = 0 THEN
            ENTRY = "No line items, delete order?"
            \L24CHR NOMSG="Y" CLR="Y"
            IF UCS(ENTRY) # "N" THEN CONTROL'Z'FLAG = 1 ELSE CONTROL'Z'FLAG = 0
        ENDIF
REL|NM|:                        !! PLACED HERE TO SAVE SPACE
        REL LIN
RETURN

SET'RT'FN|NM|:
SET'RT'FN3:
!!MODIFY: Set function key you want to happen automatically on return
!!used only if multiple pages or automatic windows used
!!
!!      IF SELECT(|NM|) = 1 OR SELECT(|NM|) = 2 THEN INX2 = FN'I
RETURN

GDB'DO'WINDOW|NM|:
!!MODIFY:  if doing a window from add mode - implement below line if you
!!         would like automatic add to continue.  You may want to do
!!         this only SELECT(|NM|) = 1 (add mode only, not insert mode)
!!
        IF SUB'FLAG # 0 AND SELECT(|NM|) # 3 THEN NXT'F(|NM|) = 1
!!
        GKD'F = 0
        CALL DO'WINDOW|NM|'2
!!MODIFY: Set below to suit if using windows or multiple pages  
!!      IF WINDOW = FN'M GOTO GDB'DO'WINDOW     ! RETURN FROM MENU
!!
!!      Display totals changed from lower level, etc
!!
RETURN

DO'WINDOW|NM|:
        IF PRH'CTL # 0 RETURN
        CALL SET'2ND'KEYS|NM|           !!GKD WINDOW CALL MAY REQUIRE KEY
                                        !!UPDATE IF BO QTY CHANGES
        GKD'F = 1                       !!FLAG GKD WINDOW CALL
DO'WINDOW|NM|'2:
        REAL'PG = |NM|
!       IF WINDOW = FN'E THEN
!               WINDOW = FN'K
!               CALL DO'WINDOW|NM|'2
!               WINDOW = SAVE'INX2
!       ENDIF
        IF WINDOW = FN'8 THEN
                CALL PROC'MSG
                IF ZZZ|NM|'MSGNO # LIN'MSGNO THEN
                        UPDATE LIN FROM LIN'REC
                        ZZZ|NM|'REC = LIN'REC
                        XLN = SXLN(|NM|)
                        CALL PRE'VDSP'MSG|NM|
                        XLN = 0
                ENDIF
        ELSE
                CASE
                IF WINDOW = FN'7 THEN
                        IF lastfn > 0 THEN
                                WINDOW = lastfn -1
                        ELSE
                                WINDOW = 254
                        ENDIF
                        CALL DO'WINDOW'FN'7                     !!HDRSUB.CPY
                        ECFLD = WINDOW
                        WINDOW = 255
                ELSE
                IF WINDOW = FN'D OR WINDOW = FN'I THEN
                        CALL FIND'PRH
                        IF SUB'FLAG # 0 THEN
                                IF GKD'F = 1 THEN
!!                                      fldend = 8
                                        CALL PRE'DDB'SCREEN|NM|
                                        FROM'PG = 8
                                ELSE
                                        IF WINDOW = FN'I THEN
                                                DO'NXT = 1
                                        ELSE
                                                DO'NXT = 0
                                        ENDIF
                                ENDIF
                                XXX'LENGTH = LFACTOR
                                XXX'HEIGHT = HFACTOR
                                XXX'WIDTH = XFACTOR
                                PG = 3
                                LIN'XXX'OVR = XXX'REC
                                \WINDOW VUEMLT="|NM|" ACTION="BEGIN" TLR="11" SAVSCR=SAVLIN
                                XXX'REC = LIN'XXX'OVR
                                PG = 2
                                IF WIN'F # 9 CALL PRE'PRE'UPDATE|NM| 
                                IF FROM'PG = 8 THEN
                                        CALL CLEAR'INPUT'AREA
                                        FROM'PG = 0
                                        GKD'F = 1
                                ELSE
                                        GKD'F = 0
                                ENDIF
                                REAL'PG = |NM|  !!LEAVE HERE FOR VDSP
                                CASE
                                IF WIN'F = 9 THEN
                                        IF LIN'SUB'COUNT = 0 THEN
                                                CALL SET'KEY|NM|
                                                CALL DEL'RECORD|NM|
                                                IF TMP'CTL(|NM|) = 0 THEN
                                                        SCR'CNT(|NM|) = SCR'CNT(|NM|) - 1
                                                        CALL SET'2ND'KEYS|NM|
                                                        CALL DEL'2ND'KEYS|NM|
                                                ENDIF
                                        ENDIF
                                        WIN'F = 0
                                        CALL SET'EOF
                                ELSE
                                IF SHIPPING = 1 THEN
                                        WIN'F = 4
                                ELSE
                                IF DO'NXT = 1 THEN
                                        WINDOW = 255
                                ENDCASE
                                XLN = SXLN(|NM|)
                                CALL PRE'VDSP|NM|
                                XLN = 0
                        ELSE
                                IF INX2 # FN'I THEN
                                        ENTRY = "Cannot enter Sub Line Items on this part"
                                        CALL DO'ERR
                                ENDIF
                        ENDIF
                ELSE
                IF WINDOW = FN'W THEN
                        IF YANK'FLAG = 3 THEN
                                INX2 = FN'10
                                RETURN                  !!KLUDGE
                        ENDIF
                        IF ARCHIVE'FLAG = "S" THEN
!!In the middle of a FLIP process
                                HHS'KLOC'NO = LIN'HDR'LOC'NO
                                HHS'KDOC'TYPE = LIN'DOC'TYPE
                                HHS'KNO = LIN'HDR'NO
                                HHS'KSHIPMENT = LIN'SHIPMENT
                                CALL USE1
                                IF TMP'CTL(1) # 0 THEN
                                        ENTRY = "Cannot find archived header - displaying current header"
                                        CALL DO'ERR
                                ENDIF
                        ENDIF
                        SGDB'F = GDB'F
                        PG = 1
                        \WINDOW VUEMLT="|NM|" TLR="4" ACTION="PROC'HDR" &
                                        SAVSCR=SAVLIN L25NUM="2" L25="Y"
                        CALL SET'BKG'OE1WGA
!!                      CALL BKG'OE1WGA
                        IF INC'DZ'PIECES # "N" THEN
                                CALL BKG'|SCREEN|
                        ELSE
                                CALL BKG'|ASCREEN|
                        ENDIF
                        PG = 2
                        CALL PRE'READ|NM|
                        GDB'F = SGDB'F
                        INX1 = I'RT
                        INX2 = 0
                ELSE
                IF WINDOW = FN'M THEN
                        CALL DO'WINDOW'FN'M             !!IN HDRSUB.CPY
                        IF ACTION = "~" THEN
                                GOTO DO'WINDOW|NM|'2
                        ELSE
                                REAL'PG = |NM|  !!LEAVE HERE
                                XLN = SXLN(|NM|)
                                CALL PRE'VDSP|NM|
                                XLN = 0
                        ENDIF                           
                ELSE
                IF WINDOW = FN'A THEN
                        CALL ARCHIVE'FLIP
                ELSE
                IF WINDOW = FN'R THEN
                        CALL DO'WINDOW'FN'R             !!IN HDRSUB.CPY
                ELSE
                IF WINDOW = FN'Y THEN
                  CIF USE'KITS AND USE'KITS = "Y" THEN
                    CALL BLDWIN'PROC
                    IF XXX'KIT'BUILD # ZZZ2'KIT'BUILD THEN
MAP1 SKIT'BUILD,S,1
                        SKIT'BUILD = XXX'KIT'BUILD
                        XXX'KIT'BUILD = ZZZ2'KIT'BUILD
                        CALL DELLOW'SUB
                        XXX'KIT'BUILD = SKIT'BUILD
                    ENDIF
                    CALL PRE'UPDATE|NM|
                  ENDCIF
                    WINDOW = 255
                ELSE
                IF WINDOW = FN'12 THEN
                        IF lastfn > 0 THEN
                                WINDOW = lastfn -1
                        ELSE
                                WINDOW = 254
                        ENDIF
                        CMN'SPACE = SPACE
                        CMN'FRM'PRGNAM="OE1HDR"
                        CMN'TO'PRGNAM="WB1PIC"
                        \EXPROG WB1PIC LINKPASS="LIN'REC" APID="WB" LFA="L" COMMON="CMN'REC"
                        dspsw = 220
                        CALL PRE'DSP'SCREEN|NM|
                        ECFLD = WINDOW
                        WINDOW = 255
                        GOTO DO'WINDOW'END|NM|
                ELSE
                IF ARCHIVE'FLAG # "C" THEN
                        GOTO DO'WINDOW'END|NM|          !!KLUDGE        
                ELSE
!!QUIT IF IN ARCHIVES ....
                IF WINDOW = FN'G THEN
                        CALL DO'WINDOW'FN'G     !!IN HDRSUB'CPY
                ELSE
                IF WINDOW = FN'Z THEN
                    IF XXX'USR'OQTY < 0 THEN
                        IF HDR'DOC'TYPE = OEC'INT'CR THEN
                            ENTRY = "Cannot change on internal credits!"
                            CALL DO'ERR
                        ELSE
                            CALL RPROC
                            CALL PRE'UPDATE|NM|
                        ENDIF
                    ELSE
                        CMN'SPACE = SPACE
                        CMN'FRM'PRGNAM="OE1HDR"
                        CMN'TO'PRGNAM="WB1CPR"
                        CMN'SAV'RCN(1) = LIN'TRK'NO
                        \WINDOW PRGNAM="IN1PRH" LFA="L" COMMON="CMN'REC" &
                                    APID="IN" VUEMLT="|NM|" FULL="Y"
                        WINDOW = 255
                        GOTO DO'WINDOW'END|NM|
                    ENDIF
                ELSE
!               IF WINDOW = FN'V THEN
!                       CALL DO'WINDOW'FN'V !!IN HDRSUB.CPY
!                       IF LIN'HOLD'YN # "N" AND LIN'HOLD'YN # "O" THEN
!                               CALL FIND'PRH
!                               IF SUB'FLAG = 0 THEN
!                                       CALL ZERO'COM'SET'BO
!                               ELSE
!                                       CALL ZERO'SUB'QTYS
!                               ENDIF
!                       ENDIF
!                       REAL'PG = |NM|          !!LEAVE HERE
!                       XLN = SXLN(|NM|)
!                       CALL PRE'VDSP|NM|
!                       XLN = 0
!                       CALL PRE'UPDATE|NM|
!               ELSE
                IF WINDOW = FN'L THEN
                    GOTO PROC'CHAIN
                ELSE
                IF WINDOW = FN'P OR WINDOW = FN'T OR WINDOW = FN'U THEN
                    GOTO PROC'CHAIN
                ELSE
                IF WINDOW = FN'X OR WINDOW = FN'C OR WINDOW = FN'B      &
                                                        OR WINDOW = FN'K THEN
                        ECFLD = lastfn - 1                      !![51]
                        CASE
                        IF WINDOW = FN'X THEN
                                CALL RESEQ'ORDER1
                                IF TST'FUNC = 1 GOTO DO'WINDOW'END|NM|  !!KLUDGE
                        ELSE
                        IF WINDOW = FN'C THEN
                                CALL DO'CONS
                                IF TST'FUNC = 1 GOTO DO'WINDOW'END|NM|  !!KLUDGE
                        ELSE
                        IF WINDOW = FN'B THEN
                                CALL FILL'BO
                        ELSE
                                CALL CHANGE'PRICE               !!FN'K
                        ENDCASE
                        PG = |NM|
                        IF WINDOW = FN'K OR WINDOW = FN'B THEN
                            CALL PRE'UPDATE|NM|
                        ENDIF
                        IF WINDOW # FN'K THEN
                            CALL SET'EOF
                        ENDIF
!![51] - change price "BAD INVOICE"
                        WINDOW = 255
                ENDCASE
        ENDIF
DO'WINDOW'END|NM|:
        REAL'PG = |NM|
RETURN

PROC'CHAIN:
        IF ARCHIVE'FLAG = "A" AND WINDOW = FN'P THEN
            HHS'RCN = 0
            REL HHS
            CMN'SPACE = SPACE
            CMN'OTHER[1;4] = HDR'LOC'NO
            CMN'OTHER[5;1] = HDR'DOC'TYPE
            CMN'OTHER[7;9] = HDR'NO
            CMN'OTHER[17;3] = HDR'SHIPMENT
            CMN'FRM'PRGNAM = "NA2RPT"
            \WINDOW PRGNAM="OERPRT" APID="OE" COMMON="CMN'REC" PUSH="Y" NOCLR="Y" FULL="Y"
            INX2 = 0
            CMN'SPACE = SPACE
            RDCOND HHS TMP'RCN(|NM|) INTO HHS'REC
            RETURN
        ENDIF
        IF PG = 3 THEN  
                CALL PRE'ABORT3
                PG = 2
                XXX'REC = LIN'XXX'OVR
                CALL PRE'UPDATE2
        ENDIF
        CALL TEST'FN'P
        IF IER1 # 0 OR WINDOW # FN'P THEN
                CALL PRE'UPDATE1
!![84]          CALL CHAIN'OUT  
                GOTO CHAIN'OUT                          !![84]
        ENDIF
        REAL'PG = |NM|                                  !![84]
RETURN

TEST'FN'P:
       CIF WGA'PRGNAM="OE1WGA" THEN
            IF SPWASTR[6;1] # "Y" AND INC'INTERFACE = 5 THEN
                XCALL GETENV,"LOC"+STR(HDR'LOC'NO)+"_PATH",ENTRY    !!HOB
                IF ENTRY # "" THEN
                    IF HDR'PICK'VER > 0 THEN
                        ENTRY = "You do not have security to re-pick HOB orders!"
                        CALL DO'ERR
                        RETURN
                    ENDIF
                ENDIF
            ENDIF
        ENDCIF
        IF ARCHIVE'FLAG = "C" THEN
                IF OEC'HOLD'COM # "Y" THEN
                        CALL GET'CUS
                        CALL TEST'HOLDS                 !!IN CMPCRL.CPY 
                ELSE
                        IF HDR'QUOTE'YN = "Y" THEN
                                ENTRY = "Cannot ship a quote!"
                                CALL DO'ERR
                        ELSE
                                IER1 = 1
                        ENDIF
                ENDIF
        ENDIF
RETURN


SET'EOF:
        IF PG = REAL'PG CALL CLR'BAR
        CALL CLEAR'INPUT'AREA
SET'EOF2:
        IF WINDOW # FN'P THEN
                CALL MAN'HOME
        ELSE
                FIRST'TIME'PG(|NM|) = 1                 !!Force totaling
                CALL MAN'EOF
        ENDIF
        IF GKD'F = 0 AND PG > 1 THEN
                WIN'F = 2
                WINDOW = 255
        ENDIF
RETURN

ARCHIVE'FLIP:
        IF ARCHIVE'FLAG = "S" THEN
                                        !!IN FLIP POSITION, RETURN
                GOTO ABORT
        ELSE
                                        !!NOT IN FLIP, DO FLIP 
                IF HDR'SHIPMENT # 0 THEN
                        CMN'SPACE = SPACE
                        CMN'TO'PRGNAM  = "OE1HDR"
                        CMN'FRM'PRGNAM = "OE1HDR"
                        CMN'SAV'RCN(1) = TMP'RCN(1)
                        CMN'SAV'RCN(2) = TMP'RCN(2)
                        IF ARCHIVE'FLAG = "A" THEN
                                CMN'SAV'RCN(4) = 1
                        ELSE
                                CMN'SAV'RCN(4) = 0
                        ENDIF
                        \WINDOW PRGNAM="OE1HDR" FULL="Y" VUEMLT="|NM|"  &
                                                             COMMON="CMN'REC"
                ELSE
                        ENTRY = "No previous shipments - i.e. no archives!"
                        CALL DO'ERR
                ENDIF
        ENDIF
RETURN

RPROC:                                  !!Used in SUBSUB.CPY as well
    IF PRH'INVENTORY = "N" THEN
        XXX'RPROC = 2
    ELSE
        \WINDOW TLR="7" TLC="39" BRR="9" BRC="80"               &
                        VUEMLT="Y" OVRLAY="RPROC" SAVSCR="SMALL"        &
                                                L25NUM="6" L25="Y"
    ENDIF
RETURN

MAP1 FROM'PG,B,1

PROC'HDR:
        CALL PRE'UPDATE1                                !!Update 1st
        CALL UPDATE'2ND'KEYS1                           !!Update 1st
        CALL DO'DDB'HDR
    ECFLD = 0
PROC'HDR2:
        CALL PRE'GDB'SCREEN1
        CALL TST'FUNC
        IF TST'FUNC = 0 THEN
                CALL PRE'UPDATE1
                CALL UPDATE'2ND'KEYS1
        ELSE
                HDR'REC = ZZZ1'REC
        ENDIF
        IF TST'FUNC = 0 THEN
                IF INX2 = FN'D THEN INX2 = FN'10
                IF INX2 >= FN'A OR INX2 = FN'7 OR INX2 = FN'8 OR INX2 = FN'12 THEN
                        FROM'PG = 2
                        WINDOW = INX2
                        CALL GDB'DO'WINDOW1
                        FROM'PG = 0
                        CASE
                        IF WINDOW = FN'D THEN
                                INX2 = FN'10
                        ELSE
                        IF WINDOW = FN'Y THEN
                                PG = 2
                                CALL SET'EOF2
                        ELSE
                        IF WINDOW = FN'8 THEN
                                GOTO PROC'HDR2
                        ENDCASE
                ENDIF
        ENDIF
RETURN

!!MODIFY: Set below to suit if using windows or multiple pages  
!!PROC'LINSEL:
!!      DSP LINSEL
!!      WK3'ACTION = ""
!!RE'GDB'|NM|SEL:
!!      WINDOW = 0
!!      ECFLD = 0
!!      GDB LINSEL
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
!!              CALL PRE'PROC'LIN'M
!!      ENDIF
!!      IF WINDOW = FN'T OR WINDOW = FN'L OR WINDOW = FN'P OR &
!!         WINDOW = FN'U GOTO CHAIN'REPORT
!!      GOTO RE'GDB'LINSEL
!!RETURN

!!MODIFY: Set below to suit if using windows or multiple pages  
!!

!!PRE'PROC'LIN'FN:                      !! FROM FUNCTION KEY
!!      PG = 2
!!      \WINDOW VUEMLT="|NM|" ACTION="BEGIN" WINNUM="2" FULL="Y"
!!      CALL PRE'UPDATE1                !! IF KEEPING TOTALS IN PAGE 1
!!      PG = 1
!!      XLN = SXLN(|NM|)                !! ONLY NEEDED IF VDSP CHANGES
!!      CALL PRE'VDSP|NM|
!!      XLN = 0
!!RETURN

!!MODIFY: Set below to suit if using windows or multiple pages  
!!PRE'PROC'LIN'M:                       !! FROM MENU
!!      PG = 2
!!      \WINDOW SCREEN="LIN SEL" ACTION="BEGIN" WINNUM="2" FULL="Y"
!!      CALL PRE'UPDATE1                !! IF KEEPING TOTALS IN PAGE 11
!!      PG = 1
!!      XLN = 0
!!RETURN

MAP1 LAST'SEL,B,1

PRE'GKD'SCREEN|NM|:
        LAST'SEL = SELECT(|NM|)
        AUTO'DXL = 0
        CIF UP'CHARGES = "Y" THEN
            ADD'UP'NEEDED = 0
        ENDCIF
        IF INC'DZ'PIECES # "N" THEN
                GKD |SCREEN|
        ELSE
                GKD |ASCREEN|
        ENDIF
        IF YANK'FLAG = 3 AND INX2 = FN'W THEN INX2 = FN'10 !!In the middle of a yank process
!!      IF SHIPPING = 1 AND INX2 = FN'10 THEN
!!              CALL SHIPPING'OFF
!!              GOTO PRE'GKD'SCREEN|NM|
!!      ENDIF
RETURN

PRE'GDB'SCREEN|NM|:
        IF WIN'F # 0 THEN
                INX1 = I'FUN
                CASE
                IF WIN'F = 5 THEN
                        INX2 = FN'4
                        WIN'F = 4
                ELSE
                IF WIN'F = 4 THEN
                        IF LIN'NO # LAST'LIN'NO THEN
                                INX2 = FN'3     !!SHIPPING MODE
                                LAST'LIN'NO = LIN'NO
                        ELSE
                                INX2 = FN'4     !!BUT AT END OF LIN
                                LAST'LIN'NO = 0
                        ENDIF
                ELSE
                        INX2 = FN'4     !!SEE HDRSUB.CPY IF THIS NEEDS EXPANDING
                        WIN'F = 0
                ENDCASE
                RETURN                  !!KLUDGE
        ENDIF
        SUB'FLAG = 0
        CALL AFR'PRH
        IF ARCHIVE'FLAG = "C" THEN
                IF SHIPPING = 1 AND SUB'FLAG # 0                        AND &
                            LIN'NO # LAST'LIN'NO AND (LIN'HOLD'YN = "N" OR  &
                                LIN'HOLD'YN = "O")                      THEN
                        INX1 = I'FUN
                        IF LIN'SUB'COUNT # 0 THEN
                                INX2 = FN'D
                        ELSE
                                INX2 = FN'1
                        ENDIF
                ELSE
                        CASE
                        IF SHIPPING = 1 AND LIN'NO = LAST'LIN'NO THEN
                                INX2 = FN'4
                                INX1 = I'FUN
                        ELSE
                        IF SHIPPING = 1 AND (LIN'HOLD'YN # "N"          AND &
                                              LIN'HOLD'YN # "O")        THEN
                                INX2 = FN'1
                                INX1 = I'FUN
                        ELSE
                                IF UOM'PUNITS = 0 OR SHIPPING = 0 THEN
                                        IF ECFLD = 0 THEN          !![15]
                                                ECFLD = SKIP'LOC   !![??]
                                        ENDIF                      !![15]
                                        IF AUTO'DXL = 2 THEN
                                           CALL FINAL'PART'BOOKKEEPING
                                           CALL PRE'PRE'DSP'WGA'AVAIL
                                           CALL PRE'DSP'WGA'AVAIL
                                           ECFLD = 2
                                        ENDIF
                                        AUTO'DXL = 0
                                        IF INC'DZ'PIECES # "N" THEN
                                                GDB |SCREEN|
                                        ELSE
                                                GDB |ASCREEN|
                                        ENDIF
                                        CALL TST'FUNC
                                        IF TST'FUNC = 0 THEN
                                                IF PRH'PMSGNO # 0 AND &
                                                   LIN'MSGNO = 0  AND &
                                                   PRH'PFORCE = "Y" THEN
                                                        INX1 = I'FUN
                                                        INX2 = FN'8
                                                ENDIF
!!MAP1 SAVE'INX2,B,1
MAP1 FN'K'RCN,B,4
!                                               IF INC'TS'SUBI = 3 AND &
!                                                  FN'K'RCN # LIN'RCN THEN !![94]
!                                                       FN'K'RCN = LIN'RCN
!                                                       IF INX2 >= FN'A OR &
!                                                          INX2 = FN'8 THEN
!                                                               SAVE'INX2 = INX2
!                                                               INX2 = FN'E
!                                                       ELSE
!                                                               INX2 = FN'K
!                                                               INX1 = I'FUN
!                                                       ENDIF
!                                               ENDIF                   !![94]
                                        ELSE
!![51] - below - fix another "BAD INVOICE" problem.  Actually DSP only prob.
                                                IF SELECT(|NM|) # 1 THEN
                                                        XLN = SXLN(|NM|)
                                                        CALL PRE'VDSP|NM|
                                                        XLN = 0
                                                ENDIF
                                                CALL CLEAR'INPUT'AREA'LIN2
                                        ENDIF
                                ENDIF
                        ENDCASE
                ENDIF
!               IF SHIPPING = 1 THEN
!                       CASE
!                       IF INX2 = FN'10 THEN
!                               CALL SHIPPING'OFF
!                               GOTO PRE'GDB'SCREEN|NM|
!                       ELSE
!                       IF INX1 = I'RT OR INX1 = I'ESC THEN
!                               INX1 = I'FUN
!                               INX2 = FN'1
!                       ENDCASE
!                       IF INX2 = FN'1 THEN LAST'LIN'NO = LIN'NO ELSE LAST'LIN'NO = 0
!               ENDIF
!!              IF TST'FUNC # 1 AND INX2 < FN'A AND LIN'BO'QTY # 0 AND &
!!                                              ZZZ|NM|'BO'QTY = 0 THEN
!!Do this EVEN if FN'D - I like it better that way!
!!                      INX1 = I'FUN
!!                      INX2 = FN'I
!!              ENDIF
        ELSE
                PRINT TAB(24,1) ; TAB(-1,A'RED) ; TAB(-1,9) ; "Cannot change archives - any key to exit";
                CALL L24CHR
                IF YANK'FLAG = 3 AND INX2 = FN'W THEN INX2 = FN'10 !!In the middle of a yank process
        ENDIF
RETURN

AFR'PRH:
        CALL OTHERS'GAFR2
        SUB'FLAG = 2
        CASE
        IF HDR'QUOTE'YN = "Y" THEN
                CALL SET'QUOTE
        ELSE
        IF XXX'ORDER'QTY < 0 THEN
                CALL SET'COM'BO
        ELSE
        IF HDR'BO'YN = "N" THEN
                XXX'BO'QTY = 0
        ENDCASE
        CALL SAFR2
        IF INC'DZ'PIECES # "N" THEN
                FOR II = 1 TO 6
                        IF WGA'AFR(II) = "E" THEN
                                HYP(II) = "/"
                        ELSE
                        IF WGA'AFR(II) = "X" THEN
                                HYP(II) = " "
                        ELSE
                                HYP(II) = ""
                        ENDIF
                        ENDIF
                NEXT II
                CALL DSP'WGA'HYP
        ENDIF
RETURN

PRE'DSP'SCREEN|NM|:
!!MODIFY: Lookup background display values here ONLY IF 'dspsw # 220'
!!
        IF dspsw # 220 CALL SET'BKG'OE1WGA

        IF INC'DZ'PIECES # "N" THEN
                DSP |SCREEN|
        ELSE
                DSP |ASCREEN|
        ENDIF
        TABWIN'EDT = EDT'OVR
        CALL SAFR2
        IF HDR'PROC >= 5 CALL DSP'PROMPT'CLR
        IF dspsw # 220 THEN
                IF ARCHIVE'FLAG # "C" CALL PRE'DSP'TOTALS
                CALL DSP'LINE'HDR
        ENDIF
!!      IF dspsw # 220 AND (HDR'SHP'NO # 0 OR HDR'SHIP'TO # 0) THEN
!!              LST'SPACE = SPACE
!!              CALL GET'CUS
!!      ENDIF
RETURN

MAP1 LST2'NAME,S,32

SET'BKG'OE1WGA:
        MAP1 BACKORDERS,S,11
        IF HDR'BO'YN = "N" OR ARCHIVE'FLAG # "C" THEN
                BACKORDERS = "" 
        ELSE
                BACKORDERS = "Backorders:"
        ENDIF
        IF HDR'DROP'YN = "N" THEN HDR'SHP'NO = HDR'NAM'NO
        IF HDR'SHIP'TO = 0 AND HDR'SHP'NO = LST'NO THEN !![35]
                LST2'REC = LST'REC                      !![35]
        ELSE
                \USELST HDR'SHP'NO ALTFLD="HDR'SHIP'TO" ALTPRE="LST2"
        ENDIF
        IF HDR'DROP'YN = "N" THEN HDR'SHP'NO = 0
        \NLSTNAME OUTPUT="LST2'NAME" PRE="LST2"
        LOC'KNO = HDR'LOC'NO
        CALL FIND'LOC
        ENTRY = RIGHT(SPACE(20)+LOC'SDESC+": "+HDR'LOC'NO+"-"+HDR'DOC'TYPE+"-"+HDR'NO,20)
        IF HDR'DOC'TYPE >= 1 AND HDR'DOC'TYPE <= 9 THEN
                IF ARC'INV'CR'YN(HDR'DOC'TYPE) = "Y" AND &
                   ARC'INV'DR'YN(HDR'DOC'TYPE) = "N" THEN
                        CREDIT="*** CREDIT ***"
                ELSE
                        IF HDR'HOLD'YN # "O" AND HDR'HOLD'YN # "N" THEN
                                IF HDR'HOLD'YN = "7" THEN
                                        CREDIT = "Over CR Limit"
                                ELSE
                                        \FINTAB OECRST HDR'HOLD'YN CREDIT
                                ENDIF
                        ELSE
                                CREDIT = SPACE(14)
                        ENDIF
                ENDIF
        ELSE
                CREDIT = SPACE(14)
        ENDIF
RETURN

MAP1    CNT,B,1                         !![20]

PRE'DDB'SCREEN|NM|:
!!MODIFY: Lookup and display other values here
        DDB'DONE1 = 0
        IF SELECT(|NM|) = 1 AND LIN'ORDER'QTY = 0 AND fldend = 0        AND &
                                                        AUTO'DXL # 2 THEN
                fldend = 1
                IF INC'DZ'PIECES # "N" THEN
                        DDB |SCREEN|
                ELSE
                        DDB |ASCREEN|
                ENDIF
                fldend = 0
                RETURN                                          !!KLUDGE
        ENDIF
        IF LIN'TRK'NO # 0 THEN
                CALL FIND'PRH
                FOR II = 1 TO 6
                        WGA'TITLES(II) = SPACE(7)
                        WGA'AFR(II) = "X"
                NEXT II
                IF PRH'ATT'SPACE # SPACE THEN
!![70] - this option, delete caching option.
                        ATT'SPACE = SPACE
                        FOR II = 1 TO 6
                                IF PRH'ATT'DESC(II) # "" THEN
                                        ATT'NO = II
                                        CALL SET'ATT'CODE
                                        WGA'TITLES(II) = ATT'CODE
                                        WGA'AFR(II) = "E"
                                ENDIF
                        NEXT II
                ELSE
                        ATT'KATH'NO = PRH'ATH'NO1
                        ATT'KNO = 0
                        USE ATT INTO ATT'REC KEY ATT'KEY
                        DO
                                GETNXT ATT INTO ATT'REC
                                IF ATT'CTL # 0 OR ATT'ATH'NO # ATT'KATH'NO OR &
                                                                ATT'NO > 6 EXIT
                                II = ATT'NO
                                IF II >= 1 AND II <= 6 THEN
                                        IF ATT'DESC # "" THEN
                                            WGA'TITLES(II) = ATT'DESC
                                        ELSE
                                            WGA'TITLES(II) = ATT'CODE
                                        ENDIF
                                        WGA'AFR(II) = "E"
                                ENDIF
                        LOOP
                ENDIF
                IF PRH'ODD'TRK'NO # 0 CALL DDB'ODD'SIZES
                CALL SET'HPRICE
                CALL AFR'PRH
        ENDIF
        IF INC'DZ'PIECES # "N" THEN
                DDB |SCREEN|
        ELSE
                DDB |ASCREEN|
        ENDIF
        IF LIN'TRK'NO # 0 THEN
                UOM'TITLES'SPACE = SPACE(50)
!!              IF INC'TS'SUBI # 4 THEN
                  FOR II = 1 TO 6
                        IF WGA'AFR(II) = "Y" THEN
!!                              DZPC(II)="Nxt Ln"
                                DZPC(II)="   *  "
                                ON II CALL DSP'DOZENS1,DSP'DOZENS2,DSP'DOZENS3, &
                                        DSP'DOZENS4,DSP'DOZENS5,DSP'DOZENS6
                        ENDIF   
                  NEXT II
                !![WC] Test PRH'QTD'COUNT GT Zero
                IF PRH'QTD'COUNT = 0 THEN
                    FOR II = 3 TO 1 STEP -1
                        LIN'UOM'NO = LIN'UOM'FACTOR(II)
                        IF LIN'UOM'NO # 0 THEN
                            CALL FIND'UOM
                            IF UOM'DUNITS # 0 THEN
                                UOM'TITLES(II) = RIGHT(SPACE(6)+UOM'DESC,6)
                            ELSE
                                UOM'TITLES(II) = "**BAD**"
                            ENDIF
                        ENDIF
                    NEXT II
                    CALL CLEAR'QBK'TITLES
                ELSE
                    UOM'TITLES'SPACE = SPACE(50)
                    UOM'TITLES(1) = "QbkPrc"
                    CALL GET'QBK'PRICES !!!
                ENDIF

!!              ENDIF
                LIN'UOM'NO = 1
!![35] - above
                CALL FORCE'DSP'WGA'TITLES
                CALL PRE'DSP'UOM'TITLES
                CALL PRE'DSP'WGP'PRICES
!!why?          UOM2'TITLES'SPACE = SPACE(50)
!!why?          CALL PRE'DSP'UOM'TITLES

                CALL DSP'NET
                IF OEC'SKIP'UP = "Y" AND ARCHIVE'FLAG = "C" AND INC'TS'SUBI # 4 THEN
                        CALL DSP'HPRICE'NOTE
                ENDIF
!![35]  ENDIF
                IF ARCHIVE'FLAG # "C" THEN
                        \STRDTT DTT="LIN'SHIP'DTT" DDATE="ENTRY"
                        ENTRY = "Shipped: "+ENTRY+", #: "+STR(LIN'SHIPMENT)
                        CALL DSP'SHIPPED
                ELSE
                        ENTRY = ""
                        CALL DSP'SHIPPED
                ENDIF
                IF HDR'BO'YN # "N" THEN
                    FOR II = 1 TO 6
                        SUB'NO = II
                        CALL SHOW'BO'S
                    NEXT II
                ENDIF
        ENDIF                                                   !![35]
        DDB'DONE1 = 1
        ? TAB(7,36);TAB(-1,A'CYAN);
RETURN

FORCE'DSP'WGA'TITLES:
    ? TAB(7,35);TAB(-1,14);
   \CLRWIN 7 36 7 77
    IF INC'DZ'PIECES # "N" THEN
        CALL DSP'WGA'TITLES
    ELSE
        CALL DSP'CYAN
        CALL DSP'AWGA'TITLES
        CALL DSP'CYAN
    ENDIF
    WGA2'TITLES'REC = WGA'TITLES'REC
RETURN

PRE'DSP'WGA'TITLES:
        IF WGA2'TITLES'REC # WGA'TITLES'REC THEN
            CALL FORCE'DSP'WGA'TITLES
        ENDIF
RETURN

PRE'DSP'UOM'TITLES:
        IF UOM2'TITLES'REC # UOM'TITLES'REC THEN
                IF INC'TS'SUBI # 4 THEN  !![WC] OneStop is set to "1" Wholesale Simple Blanks
                    CALL DSP'UOM'TITLES
                    IF PRH'QTD'COUNT = 0 THEN
                        CALL CLEAR'QBK'TITLES
                    ENDIF
                ENDIF
                CALL PRE'DSP'WGP'PRICES
                UOM2'TITLES'REC = UOM'TITLES'REC
        ENDIF
RETURN

CLEAR'QBK'TITLES:
!![WC] Clear Quantity Break Titles

        QBK'TAG = SPACE(4)

        FOR QQ = 1 TO 3
            QBK'TITLES(QQ) = SPACE
        NEXT QQ
        CALL DSP'QBK'TITLES'PRC        !!
RETURN

DO'DDBEDT2:
   IF DDB'DONE1 = 1 THEN
       \DDBEDT PREDDB="DDB'CHANGES" SINX="Y" DDBEDT="TABWIN'EDT"
       CALL AFR'PRH
   ENDIF
RETURN

PRE'USE|NM|:
        USE LIN TMP'RCN(|NM|) INTO LIN'REC
!!      TOTAL'FLAG = 1
        XXX'REC = LIN'XXX'OVR
        TMP'CTL(|NM|) = LIN'CTL

!!MODIFY:  Set RCN to 0 if secondary key that allows duplicates is used
!!         as an alternate lookup key - e.g.:
!!      LIND'RCN = 0
RETURN

!!MAP1    NOPLACE,B,1             !![20]


PRE'READ|NM|:
        LIN'RCN = TMP'RCN(|NM|)
        READ LIN
POST'PRE'READ|NM|:
        IF LIN'QFREE'FRT = "Y" OR LIN'QFREE'FRT = "N" THEN
            HPRICE'NOTE = "Prices from quote"
            ENTRY = "Prices from quote: "
            \KERROR LIN MODE="D" CNO="N"
            XCALL VICSIT,26,ENTRY,"/logs/oe1wga.DAILY"
        ELSE
            HPRICE'NOTE = SPACE
        ENDIF
        IF SPECIAL'LAST # LIN'NO THEN SPECIAL'ADD = 0               !![35]
        IF SELECT(|NM|) # 1 AND SELECT(|NM|) # 2 THEN V''ADD'F = 0  !![35]
!!
!![98] not anymore
!!ADD'RECORD CONTAINS A RDLOCK THAT DOES NOT NOT NOT CALL THIS ROUTINE!!!!
!!
!!
        TMP'CTL(|NM|) = LIN'CTL
        TMP'RCN(|NM|) = LIN'RCN         !!SPECIAL TO THIS PROGRAM
        XXX'REC = LIN'XXX'OVR
        ZZZX'REC = XXX'REC
        ZZZ|NM|'REC = LIN'REC
        LHP'SPACE = SPACE
        IF HDR'MULTI'PO = "Y" THEN
            LHP'KHDR'LOC'NO = LIN'HDR'LOC'NO
            LHP'KDOC'TYPE = LIN'DOC'TYPE
            LHP'KHDR'NO = LIN'HDR'NO
            IF ARCHIVE'FLAG = "C" THEN
                LHP'KSHIPMENT = LIN'SHIPMENT+1
            ELSE
                LHP'KSHIPMENT = LIN'SHIPMENT
            ENDIF
            LHP'KLIN'NO = LIN'NO
            USE LHP INTO LHP'REC KEY LHP'KEY
        ENDIF
        IF LHP'PO = "" THEN LHP'PO = HDR'PO
        IF REAL'PG = |NM| CALL DSP'LHP'PO
        CALL SAVTOTAL|NM|
        IF PG = |NM| THEN
                IF LIN'TRK'NO # 0 THEN
                        CALL FIND'PRH
                        CALL FIND'UOM
                        IF SELECT(|NM|) # 1 AND SELECT(|NM|) # 2 THEN
                                CHG'QTY'KEY = 2
                        ELSE
                                CHG'QTY'KEY = 1
                        ENDIF
                        CALL LOAD'WGA
!!                      TOTAL'FLAG = 0
                ELSE
                        SUB'FLAG = 0
                        WGA'SPACE = SPACE
                ENDIF
        ELSE
                WGA'SPACE = SPACE
        ENDIF
        ZZZW'REC = WGA'REC
RETURN

LOAD'WGA:
        WGA'SPACE = SPACE
MAP1 T'USR'OQTY,B,5
        T'USR'OQTY = 0
!!      IF LIN'USR'OQTY # 0 THEN
           CALL SET'LOW'VALUES3
           CALL PRE'SET'USE3
           DO
                SET'XXX = 1
                CALL PRE'GETNXT3
                SET'XXX = 0
                CALL BAD'KEY'TEST3
                IF TMP'CTL(3) # 0 EXIT
                II = SUB'NO
                IF II >= 1 AND II <= 6 THEN
                        IF INC'DZ'PIECES # "N" THEN
                            IF ARCHIVE'FLAG = "C" THEN
                                        WGA'DOZENS(II) = FIX(SUB'USR'OQTY/12)
                                        WGA'PIECES(II) = SUB'USR'OQTY - (WGA'DOZENS(II)*12)
                                        WGA'BDOZENS(II) = FIX(SUB'USR'BQTY/12)
                                        WGA'BPIECES(II) = SUB'USR'BQTY - (WGA'BDOZENS(II)*12)
                            ELSE
                                        WGA'DOZENS(II) = FIX(SUB'USR'CQTY/12)
                                        WGA'PIECES(II) = SUB'USR'CQTY - (WGA'DOZENS(II)*12)
                            ENDIF
                        ELSE
                            IF ARCHIVE'FLAG = "C" THEN
                                        WGA'PIECES(II) = SUB'USR'OQTY
                                        WGA'BPIECES(II) = SUB'USR'BQTY
                            ELSE
                                        WGA'PIECES(II) = SUB'USR'CQTY
                            ENDIF
                        ENDIF
                        CIF UP'CHARGES = "Y" THEN
                            IF OEC'SKIP'UP # "Y" AND &
                               ADD'UP'NEEDED = 1 AND &
                               PRH'ITP'NO >= 1 AND PRH'ITP'NO <=100 THEN
                                    XXX'REC = SUB'XXX'OVR
                                    CALL ADD'UP'QTY
                            ENDIF
                        ENDCIF
                        T'USR'OQTY = T'USR'OQTY + SUB'USR'OQTY
                        IF T'USR'OQTY = LIN'USR'OQTY EXIT
                ENDIF
           LOOP
!!      ENDIF
RETURN

PRE'PRE'UPDATE|NM|:
        CALL PRE'UPDATE|NM|
        IF ((SBO'QTY = 0 AND XXX'BO'QTY # 0)  OR &
           (SBO'QTY # 0 AND XXX'BO'QTY = 0)) AND GKD'F = 1 THEN
                CALL UPDATE'2ND'KEYS|NM|
        ENDIF
RETURN

PRE'UPDATE|NM|:
        OPR'FLAG = 1
        GOTO UPDATE'WRITE|NM|                   !!KLUDGE
!PRE'WRITE|NM|:
!!
!![98] not anymore
!!ADD'RECORD CONTAINS A WRITE THAT DOES NOT NOT NOT CALL THIS ROUTINE!!!!
!!
!!
        OPR'FLAG = 2
UPDATE'WRITE|NM|:
        IF XXX'KIT'BUILD # "Y" THEN XXX'KIT'BUILD = "N"
        IF LIN'LOC'NO # LIN'HDR'LOC'NO THEN HDR'MULTI = 1
        LIN'SUB'COUNT = 0
        LIN'UOM'NO = 1
        CALL FIND'PRH
        IF PRH'HAZ'NO # 0 AND HDR'HAZ'YN # "Y" THEN
            HDR'HAZ'YN = "Y"
        ENDIF
        FOR SUB'II = 1 TO 6
                IF LIN'TORDER'QTY(SUB'II) # 0 THEN
                        LIN'SUB'COUNT = LIN'SUB'COUNT + 1
                ELSE
                        LIN'TCOM'QTY(SUB'II) = 0
                ENDIF
        NEXT SUB'II
        LIN'XXX'OVR = XXX'REC
        CASE
        IF ZZZ|NM|'REC # LIN'REC AND ARCHIVE'FLAG = "C" THEN
                !![93] Next IF
                IF SELECT(1) # 1 AND SELECT(1) # 2 THEN
                    HDR'LSTID = UB'UID
                    \CNV'DTT DATE HDR'LSTDT
                ENDIF
                HDR'SHP'DONE = "N"
                HDR'PICK'NEED = "Y"
                CALL UPDATE'QTY'REC
                LIN'XXX'OVR = XXX'REC
                IF LIN'COM'QTY = 0 AND PRH'INVENTORY # "N" THEN 
                    LIN'OVERIDE = 0
                ENDIF
                UPDATE LIN
                LIN3'REC = LIN'REC
                CALL ADDTOTAL|NM|
                CALL SAVTOTAL|NM|
                LIN'XXX'OVR = XXX'REC           
                IF LIN3'REC # LIN'REC THEN
                        UPDATE LIN
                        CALL ADDTOTAL|NM|
                        CALL SAVTOTAL|NM|
                ENDIF
                IF OPR'FLAG = 2 THEN
                        REL LIN
                ENDIF
                ZZZ|NM|'REC = LIN'REC
                ZZZX'REC = XXX'REC
        ELSE
                IF OPR'FLAG = 2 THEN
                        REL LIN
                ENDIF
        ENDCASE
RETURN

FACTOR'FIX:
        XXX'QFRACTION = 0                       !![58] - replaced
        LIN'PFRACTION = 2

!!WC] Check for Stahl's DTF items based item type number
        IF PRH'ITP'NO = 9 THEN
            LIN'PFRACTION = 3
        ENDIF

        XXX'LFRACTION = 0
        XXX'XFRACTION = 0
        XXX'HFRACTION = 0
FACTOR'SET:
        \FACTORQTY MODE="L" PRE="XXX" MASKSET="4"       !![20]
        \FACTORQTY MODE="X" PRE="XXX" MASKSET="4"       !![20]
        \FACTORQTY MODE="H" PRE="XXX" MASKSET="4"       !![20]
        \FACTORQTY MODE="Q" PRE="XXX"
        \FACTORQTY MODE="P" PRE="LIN"
RETURN

PRE'INSERT|NM|:
!!MODIFY:  may need to modify if TRX'FIELD is not last GKD field - save last 
!!         GKD fields here - e.g. SAV'PTC=LIN'PTC : LIN'CST = GLA'CST
!!         (key is LIN'MACT-LIN'CST-LIN'PTC and TRX'FIELD is LIN'MACT - 
!!         the first field).  Also, changes may have to be made for
!!         ALPHA keys or non-automaticly generated keys.
!!      
!!         If not setting key fields automatically delete entire routine.
!!
!!WARNING: the entire PRE'INSERT|NM|: routine is tricky, be careful
!!

        I = |TRX'FIELD|
        IF TOP'LOADED(|NM|) = 0 AND SCR'TOP(|NM|) + SXLN(|NM|) <= 1 CALL POP'BOTTOM
        IF SCR'TOP(|NM|) + SXLN(|NM|) > 1 THEN
                TMP'RCN(|NM|) = SCR'RCN(|NM|,SCR'TOP(|NM|) + (SXLN(|NM|) - 1))
                |CALL'PRE'USE|

!!MODIFY: if TRX'FIELD is not last GKD field then add test:
!!        IF I # TRX'FIELD around below test.  May need changes for ALPHA keys
!!        as well.

                IF I > |TRX'FIELD| THEN
                        I = INT((I - |TRX'FIELD|)/2) + |TRX'FIELD|
                ELSE
                        I = |TRX'FIELD| + |TRX'STEP|
                ENDIF

        ELSE
                I = |TRX'FIELD| - (INT(I/2) MIN INT(|TRX'STEP|/2))
        ENDIF

        IF I = |TRX'FIELD| THEN I = 0

!!MODIFY: if TRX'FIELD is not last GKD field remove above test and use similar
!!        to below.
!!
!!      IF I < 1 OR I = TRX'FIELD THEN
!!              IF LIN'PTC # 9999 AND (LIN'PTC + 1) # SAV'PTC THEN
!!                      LIN'PTC = LIN'PTC + 1
!!                      LIN'CST = 0
!!              ELSE
!!              IF LIN'CST # 9999 AND (LIN'CST +1) # SAV'CST THEN
!!                      LIN'CST = LIN'CST + 1
!!              ELSE
!!                      I = 0
!!              ENDIF
!!              ENDIF
!!      ENDIF
!!
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
        I = |TRX'STEP|
RETURN

DEL'RECORD|NM|:
        IF SELECT(1) # 1 OR LIN'ORDER'QTY # 0 CALL DELLOW'SUB
        IF SKIP'MARK # 0 OR DELLOW = 1 THEN
                TMP'CTL(|NM|) = 3
                DELLOW = 1
        ELSE
                LIN'RCN = TMP'RCN(|NM|)
                DELETE LIN KEY LIN'KEY  
                TMP'CTL(|NM|) = LIN'CTL
                IF LIN'CTL = 0 THEN
                        HDR'SHP'DONE = "N"
                        HDR'PICK'NEED = "Y"
                        IF LIN'MSGNO # 0 THEN
                                DELMSG LIN'MSGNO "OEMSG"
                        ENDIF
                        !![93] Next IF
                        IF SELECT(1) # 1 AND SELECT(1) # 2 THEN
                                HDR'LSTID = UB'UID
                                \CNV'DTT DATE HDR'LSTDT
                        ENDIF
                        IF REAL'PG # 1 THEN
                            FST'BOX = 2
                            IF SELECT(1) # 1 CALL DELOOP'BXL
CIF V''OE1DEL = "N" THEN
                            IF HDR'MULTI'PO = "Y" AND LIN'NO # 0 THEN
                                LHP'KHDR'LOC'NO = HDR'LOC'NO
                                LHP'KDOC'TYPE = HDR'DOC'TYPE
                                LHP'KHDR'NO = HDR'NO
                                LHP'KSHIPMENT = HDR'SHIPMENT + 1
                                LHP'KLIN'NO = LIN'NO
                                RDLOCK LHP INTO LHP'REC KEY LHP'KEY
                                IF LHP'CTL = 0 THEN
                                    \SETKEY LHP MODE="KY2"
                                    \DELKEY LHP MODE="ALL"
                                    DELETE LHP
                                ENDIF
                            ENDIF
ENDCIF
                        ENDIF
                        CALL SUBTOTAL|NM|
                        SELECT(|NM|) = 4
                ENDIF
        ENDIF
RETURN

DELLOW'SUB:
        PG = 3
        ZZZXSAV'REC = ZZZX'REC
        LIN'XXX'OVR = XXX'REC
        FOR SUB'II = 1 TO 6
                IF LIN'TORDER'QTY(SUB'II) # 0 THEN
                        TMP'RCN(3) = SUB'II
                        CALL PRE'READ3
                        CALL DEL'RECORD3
                ENDIF
        NEXT SUB'II
        XXX'REC = LIN'XXX'OVR
        ZZZX'REC = ZZZXSAV'REC
        PG = 2
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

!!MARK'IT'L25|NM|:
!!MODIFY: CHANGE MESSAGE TO SUIT (carefull of length)

DEL'IT'L25|NM|:
!!MODIFY: CHANGE MESSAGE TO SUIT (carefull of length)
        L25 = "'G' will delete from "+TOP'TRX'FIELD|NM|+" to "+BTM'TRX'FIELD|NM|+": [ESC] to abort"
RETURN

UPDATE'REC'L25'CHANGE|NM|:
UPDATE'REC'L25'ADD|NM|:
!!MODIFY: CHANGE MESSAGE TO SUIT (carefull of length)
        L25="F1-F2 upd |F3-F4 can |Fn M>enu |B>O fill |P>ost |W>header |D>etail"
RETURN

!DO'BAR'L25|NM|:                !!IN HDRSUB.CPY
!!MODIFY: CHANGE MESSAGE TO SUIT (carefull of length)
!       L25="CTRL- B> Insert | Z> Delete | F4 Add | [RET] Change | F10 Exit"
!RETURN

MARK'TOP|NM|:
        TOP'TRX'FIELD|NM| = |TRX'FIELD|
RETURN

MARK'BTM|NM|:
        BTM'TRX'FIELD|NM| = |TRX'FIELD|
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
!!
        DELLOW = 0
        CALL CHECK'DEL'ALL
RETURN

DEL'Q'S3:
DEL'Q'S|NM|:
!!MODIFY: Change as needed for delete questions prior to all deletes.  
!!        Called after ASKDEL|NM| if delete is to be performed.
!!        You may differentiate mark deletes by testing TOP'MARK # 0.
!!        Will delete only if SKIP'MARK AND ABORT'MARK are
!!        both set to zero in this routine!     
!!
!!      CALL delete'questions
!!      IF "N" THEN SKIP'MARK = 1               !!skip single delete
!!      CALL TST'FUNC : ABORT'MARK = TST'FUNC   !!abort mark delete and single
RETURN

!!CHECK'ADD|NM|:
!!MODIFY:  TO DISALLOW ADDING, SET ABORT'ADD = 1
!!          E.G. IF BAT'STAT = "P" THEN ABORT'ADD = 1   
!!
!!RETURN

!!MARK'ADD'Q'S|NM|:
!!MODIFY: Change as needed for mark add questions
!!
!!      CALL ADD'QUESTIONS
!!      CALL TST'FUNC : ABORT'MARK = TST'FUNC   !!abort mark add
!!RETURN

MARK'ADD|NM|:
!!FIX SHPPED'QTY
!!OTHERS?????
        CALL FIND'PRH
        IF LIN'SUB'COUNT = 0 AND SUB'FLAG # 0 RETURN
        XXX'REC = LIN'XXX'OVR
        IF LAST'TRX'FIELD|NM| + (|TRX'STEP| * I) <= |HIGH'KEY| AND      &
                                                        TMP'CTL(|NM|) = 0 THEN
                SAV'LIN'NO = LIN'NO
                |TRX'FIELD| = LAST'TRX'FIELD|NM| + (|TRX'STEP| * I)
                LIN'OVERIDE = 0
                LIN'PACK'FLAG = 0
                CALL SET'KEY|NM|
                IF SUB'FLAG = 0 THEN
                        CALL ADJUST'AVL'LIN
                        CALL ADD'RECORD|NM|
                ELSE
                        CALL ZTOTAL3
                        CALL COPY'SUBS
                        CALL ADD'RECORD|NM|
                ENDIF
                BO'FLAG = 0
                IF TMP'CTL(|NM|) # 0 THEN
                        I = BTM'MARK
                ELSE                                            !![35]
                        CALL UPDATE'2ND'KEYS|NM|                !![35]
                        CIF V''ADDL = "Y" THEN
                                CALL REL|NM|                            !![98]
                        ENDCIF
                ENDIF                                           !![35]
        ELSE
                I = BTM'MARK
        ENDIF
        PG = REAL'PG
RETURN

COPY'SUBS:
!!      LIN'XXX'OVR = XXX'REC           !!This MUST be true prior to here
        FOR SUB'II = 1 TO 6
                IF LIN'TORDER'QTY(SUB'II) # 0 THEN
                        TMP'RCN(3) = SUB'II
                        CALL PRE'READ3
                        CALL ADJUST'AVL'SUB
                        CALL ADD'RECORD3
!!no-op         CALL REL3               !![98]
                ENDIF
        NEXT SUB'II
        XXX'REC = LIN'XXX'OVR           !!MUST be done after \CPYLVL
        PG = REAL'PG
RETURN

ADJUST'AVL'SUB:
        ZZZX'SPACE = SPACE
        ZZZ3'SPACE = SPACE
        SUB'HDR'NO = HDR'NO             !!NEEDED FOR \CPYLVL IN HDRSUB.CPY
        SUB'LIN'NO = LIN'NO
        PG = 3                          !!For available routine
        XXX'REC = SUB'XXX'OVR
        CALL ADJUST'AVL'GENERIC
        SUB'XXX'OVR = XXX'REC
        PG = 3                          !!For write in \CPYLVL
RETURN

ADJUST'AVL'LIN:
        ZZZX'SPACE = SPACE
        ZZZ2'SPACE = SPACE
        PG = 2                                  !!For available routine
        CALL FIND'UOM
        XXX'REC = LIN'XXX'OVR
        CALL ADJUST'AVL'GENERIC
        LIN'XXX'OVR = XXX'REC
        PG = 2                          !!For write in \CPYLVL
RETURN

ADJUST'AVL'GENERIC:
        BO'FLAG = 1
        IF XXX'ORDER'QTY > 0 THEN
                CALL AVAILABLE
                IF XXX'ORDER'QTY > AVAILABLE THEN
                        XXX'COM'QTY = 0
                ELSE
                        XXX'COM'QTY = XXX'ORDER'QTY
                ENDIF
        ENDIF
        CALL SET'COM'BO
RETURN

!!MODIFY: messages - be careful not to change the intent of the message!!
!!        normally no changes needed.

!EMPTY'MSG|NM|:                 !!IN HDRSUB.CPY
!       ENTRY = "Line Items  Empty - you are not authorized to add"
!RETURN

ONFIL'MSG|NM|:
        ENTRY = ENTRY + " Line Item already on file"
RETURN

!CHANG'MSG|NM|:
!       ENTRY = "You are not authorized to change Line Items "
!RETURN

!DELET'MSG|NM|:
!       ENTRY = "You are not authorized to delete from Line Items "
!RETURN

!ADD'MSG|NM|:
!       ENTRY = "You are not authorized to add to Line Items "
!RETURN

!INSERT'MSG|NM|:
!       ENTRY = "Sorry, no room to insert in Line Items "
!RETURN

!HIGH'MSG|NM|:
!       ENTRY = "The highest Line Items Key already added"
!RETURN

!!BELOW are optional labels.  They are "stacked" on one return to save
!!program size.  To implement one of them seperate it and put in the
!!RETURN
!!
!!To use below features add a "GLOBAL EOFTTL?="Y" " To top of VUEMLT program
!!where ? is the page number of this level
!!
DEFINE UPBUCKET
        CIF 0 = "0" THEN
                \SETBKT OEC'UP'BUCKET 0
        CELSE
                \SETBKT OEC'UP'BUCKET |0 +
        ENDCIF
ENDMACRO

MAP1 CAPTTL,B,5

ZTOTAL|NM|:
!!MODIFY:  if using EOF that totals, zero totals here
        IF ARCHIVE'FLAG # "C" RETURN            !!Kludge
        CAPTTL = 0
        HDR'LIN'TOTAL = 0
        HDR'LIN'DISC = 0
        HDR'LIN'BO = 0
        HDR'LIN'COM = 0
        HDR'LIN'COUNT = 0       
        HDR'SHP'TOTAL = 0
        HDR'SHP'DISC = 0
        HDR'STX1ABLE = 0
        HDR'STX2TOTAL = 0
!!      HDR'SHP'RETRO = 0               !-Upcharge
        CIF UP'CHARGES = "Y" THEN
            IF OEC'SKIP'UP # "Y" THEN
                \UPBUCKET 0
            ENDIF
            IF OEC'SKIP'UP # "Y" CALL CLEAR'UP'QTY
            HDR'TMP'UC = 0
        ENDCIF
ZSAVAMT|NM|:                            !Kludge to save a call.  bfd
        IF ARCHIVE'FLAG # "C" RETURN            !!Kludge
        CIF UP'CHARGES = "Y" THEN
            MAP1    SAV'ITP'NO,B,2                  !![20]
        ENDCIF
        SAV'PRICE = 0
        SAV'DISC = 0
        SAV'BO = 0
        SAV'COM = 0
        SHP'PRICE1 = 0
        SHP'DISC1 = 0
        SAV'STX1ABLE = 0
        SAV'STX2TOTAL = 0
        CIF UP'CHARGES = "Y" THEN
            SAV'ITP'NO = 0
        ENDCIF
        SAV'CAPTTL = 0
RETURN

MAP1    NETP,F,6                        !![20]
MAP1    NETD,F,6                        !![20]
MAP1    CNETP,F,6                       !![20]
MAP1    CNETD,F,6                       !![20]
MAP1    CALC'NETP,F,6                   !![20]
MAP1    CALC'NETD,F,6                   !![20]
MAP1    SAV'CAPTTL,B,5

SAVTOTAL|NM|:
!!MODIFY:  if using EOF that totals, zero totals here
        IF ARCHIVE'FLAG # "C" RETURN            !!Kludge
        CALL FIND'PRH
        QFACTOR = 10^XXX'QFRACTION
        PFACTOR = 10^LIN'PFRACTION
        SAV'BO = SGN(ABS(XXX'BO'QTY))           !!NUMBER OF LINE ITEMS W/BO
        SAV'COM = SGN(ABS(XXX'COM'QTY))         !!NUMBER OF LINE ITEMS committed
!!Used XX and XH as temp variables here
!!      \FROUND ((LIN'PRICE*XXX'ORDER'QTY*100)/QFACTOR)/PFACTOR XX
!!      \FROUND ((LIN'DISC*XXX'ORDER'QTY*100)/QFACTOR)/PFACTOR XH
        CALL COMPUTE'NETS
        XX = NETP
!![69]  XH = NETD
!![69]  IF SAV'PRICE # XX OR SAV'DISC # XH CALL SHOW'TOTALS|NM|
        IF SAV'PRICE # XX CALL SHOW'TOTALS|NM|                  !![69]

        SAV'PRICE = XX
!![69]  SAV'DISC = XH

!!      \FROUND ((LIN'PRICE*XXX'COM'QTY*100)/QFACTOR)/PFACTOR SHP'PRICE1
!!      \FROUND ((LIN'DISC*XXX'COM'QTY*100)/QFACTOR)/PFACTOR SHP'DISC1
        SHP'PRICE1 = CNETP
!![69]  SHP'DISC1 = CNETD
        CIF UP'CHARGES = "Y" THEN
            SAV'ITP'NO = PRH'ITP'NO
        ENDCIF

        IF PRH'TAXABLE = "Y" AND TAX'PERC # 0 THEN
                SAV'STX1ABLE = SHP'PRICE1 !![69]- SHP'DISC1
        ELSE
                SAV'STX1ABLE = 0
        ENDIF
        SAV'STX2TOTAL = 0
        IF PRH'STX'NO # 0 AND (CUS'TAXABLE = "Y" OR CUS'TAXABLE = "E") THEN
                STX'KLOC'NO = LIN'LOC'NO
                STX'KNO = PRH'STX'NO
                TSTTAX = "E"
                CALL GET'STX
                IF STX'CTL = 0 THEN
!![69]                  \FROUND ((SHP'PRICE1-SHP'DISC1)*TTL'STX'PERC)/10000 SAV'STX2TOTAL
                        \FROUND (SHP'PRICE1*TTL'STX'PERC)/10000 SAV'STX2TOTAL !![69]
                ENDIF
        ENDIF
        SAV'CAPTTL = LIN'USR'OQTY
RETURN  

ADDCOUNT|NM|:
        IF ARCHIVE'FLAG # "C" RETURN            !!Kludge
        HDR'LIN'COUNT = HDR'LIN'COUNT + 1
RETURN

MAP1 SAV'RTOTAL(2),B,5

ADDTOTAL|NM|:
!!MODIFY:  if using EOF that totals, add to totals here
        IF ARCHIVE'FLAG # "C" RETURN            !!Kludge
        CALL FIND'PRH
        QFACTOR = 10^XXX'QFRACTION
        PFACTOR = 10^LIN'PFRACTION
!!      \FROUND ((LIN'PRICE*XXX'ORDER'QTY*100)/QFACTOR)/PFACTOR XX
!!      \FROUND ((LIN'DISC*XXX'ORDER'QTY*100)/QFACTOR)/PFACTOR XH
        CALL COMPUTE'NETS
        XX = NETP
!![69]  XH = NETD
        HDR'LIN'TOTAL = HDR'LIN'TOTAL - SAV'PRICE + XX
!![69]  HDR'LIN'DISC = HDR'LIN'DISC - SAV'DISC + XH
        HDR'LIN'BO = HDR'LIN'BO + SGN(ABS(XXX'BO'QTY)) - SAV'BO
        HDR'LIN'COM = HDR'LIN'COM + SGN(ABS(XXX'COM'QTY)) - SAV'COM
!!      \FROUND ((LIN'PRICE*XXX'COM'QTY*100)/QFACTOR)/PFACTOR SHP'PRICE2
!!      \FROUND ((LIN'DISC*XXX'COM'QTY*100)/QFACTOR)/PFACTOR SHP'DISC2
        SHP'PRICE2 = CNETP
!![69]  SHP'DISC2 = CNETD
        HDR'SHP'TOTAL = HDR'SHP'TOTAL - SHP'PRICE1 + SHP'PRICE2
!![69]  HDR'SHP'DISC = HDR'SHP'DISC - SHP'DISC1 + SHP'DISC2

        HDR'STX1ABLE = HDR'STX1ABLE - SAV'STX1ABLE
        IF PRH'TAXABLE = "Y" AND TAX'PERC # 0 THEN
             HDR'STX1ABLE = HDR'STX1ABLE + SHP'PRICE2 !![69]- SHP'DISC2)
        ENDIF
        HDR'STX2TOTAL = HDR'STX2TOTAL - SAV'STX2TOTAL
        IF PRH'STX'NO # 0 AND (CUS'TAXABLE = "Y" OR CUS'TAXABLE = "E") THEN
                STX'KLOC'NO = LIN'LOC'NO
                STX'KNO = PRH'STX'NO
                TSTTAX = "E"
                CALL GET'STX
                IF STX'CTL = 0 THEN
!![69]                  \FROUND HDR'STX2TOTAL+((SHP'PRICE2-SHP'DISC2)*TTL'STX'PERC)/10000 HDR'STX2TOTAL
                        \FROUND HDR'STX2TOTAL+(SHP'PRICE2*TTL'STX'PERC)/10000 HDR'STX2TOTAL !![69]
                ENDIF
        ENDIF
        CAPTTL = CAPTTL + LIN'USR'OQTY - SAV'CAPTTL
RETURN

SUBTOTAL|NM|:
        IF ARCHIVE'FLAG # "C" RETURN            !!Kludge
        CALL FIND'PRH
        HDR'LIN'COUNT = HDR'LIN'COUNT - 1
        HDR'LIN'TOTAL = HDR'LIN'TOTAL - SAV'PRICE
!![69]  HDR'LIN'DISC = HDR'LIN'DISC - SAV'DISC
        HDR'SHP'TOTAL = HDR'SHP'TOTAL - SHP'PRICE1
!![69]  HDR'SHP'DISC = HDR'SHP'DISC - SHP'DISC1
        HDR'LIN'BO = HDR'LIN'BO - SAV'BO
        HDR'LIN'COM = HDR'LIN'COM - SAV'COM
        HDR'STX1ABLE = HDR'STX1ABLE - SAV'STX1ABLE
        HDR'STX2TOTAL = HDR'STX2TOTAL - SAV'STX2TOTAL
        CAPTTL = CAPTTL - SAV'CAPTTL
        CALL SHOW'TOTALS|NM|
RETURN

SHOW'TOTALS|NM|:
!!MODIFY: as below for displaying totals
        IF REAL'PG = |NM| AND BO'FLAG = 0 AND SPG = 0 THEN
                CALL PRE'DSP'TOTALS
        ENDIF
RETURN  

!!MODIFY        Add your screen edits, etc below

!!COPYS "OE4WGA"


E'TRK'NO:
!!      IF INX2 = FN'M RETURN
        IF EDSPSW = 0 THEN
                IF SHIPPING = 1 AND PG # 1 THEN
                        EDSPSW = 2              !!BECAUSE WHEN \AFR SET
                        CALL E'TRK'NO           !!E'TRK'NO NOT CALLED W/EDSPSW = 2
                        EDSPSW = 0              !!AND WE NEED THIS!!!!!!!!
                ENDIF
        ENDIF                           !!AFR FIXED?????
!!      IF EDSPSW = 255 AND LIN'TRK'NO = 0 THEN
!!              ALPHSW = 0              !!Normally done in TABPRH.MAC
!!              ENTRY = ""
!!              CALL PRE'DSP'PRH'DESC
!!              RETURN                  !!KLUDGE
!!      ENDIF
        \TABPASS
        IF (EFCHG # 0 OR TABPASS = 1) AND EDSPSW = 0 THEN CHG'QTY'KEY = 1
        IF EDSPSW = 0 THEN
            IF INX2 = FN'12 THEN
                XCALL GETENV,"PRHURL",MY'DATE
                IF MY'DATE = "" THEN INX2 = FN'C12
            ENDIF
            IF INX2 = FN'C12 OR INX2 = FN'S12 THEN
                AUTO'DXL = 0
                CALL HANDLE'ODD'STUFF
                IF AUTO'DXL # 0 THEN
                    AUTO'DXL = 0
                    CHG'QTY'KEY = 1
                ELSE
                    AUTO'TRK'NO = 0
                ENDIF
                IF AUTO'TRK'NO # 0 THEN
                    LIN'TRK'NO = AUTO'TRK'NO
                    CALL FIND'PRH
                    INX1 = I'RT : INX2 = 0 : IER1 = 0
                    CALL CHANGED'PART
                    CALL TABPRH'CLEAR
!!                  ECFLD = ECFLD + 1
                    RETURN                        !!KLUDGE
                ELSE
                    IF TABPRH'A = MAPZAP'STYLE'POS AND &
                                            MAPZAP'STYLEX'POS # 0 THEN
                        INX1 = I'TAB
                        INX2 = 0
                    ELSE
                        INX1 = I'FUN
                        INX2 = FN'5
                    ENDIF
                ENDIF
            ENDIF
        ENDIF
        \TABPRH EDIT="T" FLDNAM="LIN'TRK'NO" DSPNAM="PRE'DSP'PRH'DESC"  &
                PTP'MODE="I" QTY="N" SF'TEST="E'TRK'NO'SF"              &
                PTPNAM="LIN'EXT'TRK'NO" TABPRH'CLEAR="TABPRH'CLEAR"     &
                STARTLAST="Y" FIND'PRH="ARRAY'PRH" &   !![40] add FIND'PRH
                TABPRH'CLEAR2="TABPRH'CLEAR2"

        IF HDRRPT'ASK'PRH = 1 RETURN                            !!KLUDGE
CHANGED'PART:
        IF PRH'ACTIVE = "I" OR PRH'ACTIVE = "P" THEN
            CASE
            IF PRH'ON'SALE = "Y" AND PRH'ACTIVE = "I" THEN
                CLOSEOUT = "Instock Only/Sale"
            ELSE
            IF PRH'ON'SALE # "Y" AND PRH'ACTIVE = "I" THEN
                CLOSEOUT = "Instock Only"
            ELSE
            IF PRH'ON'SALE = "Y" AND PRH'ACTIVE = "Q" THEN
                CLOSEOUT = "Future Mill/Sale"
            ELSE
            IF PRH'ON'SALE # "Y" AND PRH'ACTIVE = "Q" THEN
                CLOSEOUT = "Future Mill Dir."
            ELSE
            IF PRH'ON'SALE = "Y" AND PRH'ACTIVE = "P" THEN
                CLOSEOUT = "New/On Sale"
            ELSE
            IF PRH'ON'SALE # "Y" AND PRH'ACTIVE = "P" THEN
                CLOSEOUT = "New Part"
            ENDCASE
        ELSE
            IF PRH'ACTIVE = "C" THEN
                IF PRH'ON'SALE = "Y" THEN
                    CLOSEOUT = "Close-Out/On Sale"
                ELSE
                    CLOSEOUT = "Close-Out"
                ENDIF
            ELSE
                CASE
                IF PRH'ACTIVE = "D" THEN
                    IF PRH'ON'SALE = "Y" THEN
                        CLOSEOUT = "Discon. by Us/Sale"
                    ELSE
                        CLOSEOUT = "Discontinued by Us"
                    ENDIF
                ELSE
                IF PRH'ACTIVE = "M" THEN
                    IF PRH'ON'SALE = "Y" THEN
                        CLOSEOUT = "Discon. by MFG/Sale"
                    ELSE
                        CLOSEOUT = "Discontinued by MFG"
                    ENDIF
                ELSE
                IF PRH'ACTIVE = "X" THEN
                    IF PRH'ON'SALE = "Y" THEN
                        CLOSEOUT = "Drop Ship Part/Sale"
                    ELSE
                        CLOSEOUT = "Drop Ship Part"
                    ENDIF
                ELSE
                IF PRH'ACTIVE = "Z" THEN
                    IF PRH'ON'SALE = "Y" THEN
                        CLOSEOUT = "Drop Ship Part/Sale"
                    ELSE
                        CLOSEOUT = "Drop Ship Part"
                    ENDIF
                ELSE
                IF PRH'ACTIVE = "S" THEN
                    IF PRH'ON'SALE = "S" THEN
                        CLOSEOUT = "Special Order/Sale"
                    ELSE
                        CLOSEOUT = "Special Order Part"
                    ENDIF
                ELSE
                IF PRH'ACTIVE = "F" THEN
                    IF PRH'ON'SALE = "S" THEN
                        CLOSEOUT = "Future Disc/On Sale"
                    ELSE
                        CLOSEOUT = "Future Discontinued"
                    ENDIF
                ELSE
                    IF PRH'ON'SALE = "Y" THEN
                        DTT = PRH'EXPIRE'DTT
                        CLOSEOUT = "Sale ends " + DMN + "/" + DDY + "/" + DYR[2;2]
                    ELSE
                        CLOSEOUT = ""
                    ENDIF
                ENDCASE
            ENDIF
        ENDIF
        CALL DSP'CLOSEOUT
        IF EDSPSW # 0 RETURN                                    !!KLUDGE
        IF TST'FUNC = 1 OR INX1 = I'UA RETURN                   !!KLUDGE
      CIF USE'KITS AND USE'KITS = "Y" THEN
        CALL SET'KIT'BUILD
      CELSE
        XXX'KIT'BUILD = "N"
      ENDCIF
        DP'SPACE = SPACE
        LAST'GAFR'II = 0
        FIRST'GAFR'II = 0
        IF LIN'TRK'NO # 0 THEN
                CALL FIND'UOM
                FOR II = 1 TO 6
                        WGA'TITLES(II) = SPACE(7)               
                        WGA'AFR(II) = "X"
                NEXT II
                CALL PRE'PRE'DSP'WGA'AVAIL
                IF LAST'GAFR'II = 0 THEN
                        ENTRY = "Invalid part - no attributes!"
                        GOTO DO'ERR             !!KLUDGE
                ENDIF
                IF PRH'ODD'TRK'NO # 0 CALL SET'ODD'SIZES
                CALL PRE'DSP'WGA'AVAIL
                IF CHG'QTY'KEY = 1 THEN
                    CALL FORCE'DSP'WGA'TITLES
                ELSE
                    CALL PRE'DSP'WGA'TITLES
                ENDIF
        ENDIF
        CALL TST'FUNC
        CIF USE'KITS AND USE'KITS = "Y" THEN
            IF INX2 = FN'B OR CHG'QTY'KEY # 0 THEN
                CALL WINDOW'TS'KTD
            ENDIF
        ENDCIF
        IF CHG'QTY'KEY = 1 THEN
            FOR QQ = 1 TO 3
                LIN'TCPRICE(QQ) = 0
                LIN'TPRICE(QQ) = 0
                LIN'UOM'FACTOR(QQ) = 0
            NEXT QQ
            CALL SET'HPRICE
            CALL AFR'PRH
        ENDIF
        XCALL GETENV,"OE1WGA_FORCE_MSG",ENTRY
        IF UCS(ENTRY) = "Y" AND PRH'PMSGNO # 0 AND LIN'MSGNO = 0 AND (PRH'PFORCE = "Y" OR &
                    PRH'PFORCE = "B") THEN
            CALL PROC'MSG
        ENDIF
        IF CHG'QTY'KEY = 1 THEN
!!              LIN'UOM'FACTOR(1) = 0
FINAL'PART'BOOKKEEPING:
              CIF USE'KITS AND USE'KITS = "Y" THEN
                CALL SET'KIT'BUILD
              CELSE
                XXX'KIT'BUILD = "N"
              ENDCIF
                CALL ADJUST'PART
                CALL PRE'DSP'DOZENS
                CALL DO'PRICE
                CALL FACTOR'FIX
                CALL PRE'UPDATE|NM|
                CALL UPDATE'2ND'KEYS|NM|
                XLN = SXLN(|NM|)
                CALL PRE'VDSP|NM|
                XLN = 0
        ELSE
                CALL PRE'DSP'WGP'PRICES
                CALL DSP'PRICING
        ENDIF
        IF IER1 # 0 THEN
                IER1 = 0
                FOR II = 1 TO 3
                        IF LIN'UOM'FACTOR(II) # 0 THEN IER1 = 1
                NEXT II
                IF IER1 = 0 THEN
                        ENTRY = "No active price records! "+PRCD'KLOC'NO+" "+PRCD'KTRK'NO+" "+PRCD'KUOM'NO
                        CALL DO'ERR
                ENDIF
        ENDIF
        CIF UP'CHARGES = "Y" THEN
            IF OEC'SKIP'UP # "Y" CALL SHOW'UP'CHARGE                !!Must compute 1st as result is
        ENDCIF
RETURN

PRE'PRE'DSP'WGA'AVAIL:
    IF PRH'ATT'SPACE # SPACE THEN
        ATT'SPACE = SPACE
        FOR QQ = 1 TO 6
            IF PRH'ATT'DESC(QQ) # "" THEN
                ATT'NO = QQ
                CALL SET'ATT'CODE
                CALL E'TRK'NO'ATT
            ENDIF
        NEXT QQ
    ELSE
        ATT'KATH'NO = PRH'ATH'NO1
        ATT'KNO = 0
        USE ATT INTO ATT'REC KEY ATT'KEY
        DO
            GETNXT ATT INTO ATT'REC
            IF ATT'CTL # 0 OR ATT'ATH'NO # ATT'KATH'NO OR ATT'NO > 6 EXIT
            CALL E'TRK'NO'ATT
        LOOP
    ENDIF
RETURN

E'TRK'NO'ATT:
        II = ATT'NO
        IF II >= 1 AND II <= 6 THEN
                WGA'TITLES(II) = ATT'DESC
                SUB'SPACE = SPACE
                CALL SET'FROM'HIGHER3
                SUB'NO = II
                IF WGA'DOZENS(II) # 0 OR WGA'PIECES(II) # 0 THEN !![35]
                        CALL PRE'SET'USE3
                ENDIF
                ZZZ3'REC = SUB'REC
                ZZZX'REC = SUB'XXX'OVR
                IF INC'DSPAVL # "N" THEN        !![25]
                        CALL AVAILABLE
                        PQTY(II) = PAVAILABLE
                        IF INC'DZ'PIECES # "N" THEN
                                DOZENS(II) = FIX(AVAILABLE/12)
                                IF DOZENS(II) > 999 THEN
                                        DOZENS(II) = 999
                                        PIECES(II) = 99
                                ELSE
                                        PIECES(II) = AVAILABLE-(DOZENS(II)*12)
                                ENDIF
                        ELSE
                                PIECES(II) = AVAILABLE
                        ENDIF
                ELSE                            !![25]
                        PIECES(II) = -1         !![25]
                ENDIF                           !![25]
                IF FIRST'GAFR'II = 0 THEN FIRST'GAFR'II = II
                LAST'GAFR'II = II
                WGA'AFR(II) = "E"
        ENDIF
RETURN

PRE'DSP'PRH'DESC:
        IF HDRRPT'ASK'PRH = 1 CALL DSP'PRH'DESC1 ELSE CALL DSP'PRH'DESC
RETURN

!UOM'ERROR:
!        ENTRY = "Invalid UOM - Numerator or denominator = 0 on: "+LIN'UOM'NO
!        GOTO DO'ERR
!RETURN

COPYF "APAKEY[IN]"

E'TRK'NO'SF:
        ENTRY = ""
        CASE
        IF HDR'QUOTE'YN = "Y" AND HDR'LEAD'ITEMS = "Y" AND PRH'PRC'TRK'NO # 0 THEN
            ENTRY = "Must enter only Lead Items"
        ELSE
        IF PRH'ATH'NO1 = 0 THEN
            ENTRY = "Invalid attribute set-up!"
        ELSE
        IF PRH'ACTIVE = "N" THEN
            ENTRY = "Part not active"
        ELSE
        IF INC'MULTI'CNO = "Y" THEN
            APA'KPLN'NO = PRH'PLN'NO
            APA'KGL'CNO = HDR'GL'CNO
            RDKEY APA
            IF APA'CTL # 0 THEN
                ENTRY = "Invalid Product Line/G/L Company combination"
            ENDIF
        ELSE
        IF PRH'INTERFACE # 5 THEN
            ENTRY = "Not a valid part (interface)"
        ELSE
        IF PRH'HAZ'NO # 0 THEN
            CALL CHECK'HAZARDOUS
        ENDCASE
        CIF USE'KITS AND USE'KITS = "Y" THEN
            IF (PRH'KTD'FLAG = "B" OR PRH'KTD'FLAG = "O") AND HDR'PROC = 6 THEN
                    ENTRY = "Cannot tranfer Build or Optional Build Kits"
            ENDIF
        ENDCIF
        IF LEN(ENTRY) # 0 THEN SF = 2
RETURN

MAP1 TMP'ITEMS,B,5
MAP1 SAV'TRK'KEY,B,3
MAP1 UOM'TITLES'REC
        MAP2 UOM'TITLES'SPACE,S
                MAP3 UOM'TITLES(6),S,6
MAP1 UOM2'TITLES'REC                            !!Carefull
        MAP2 UOM2'TITLES'SPACE,S,36


!![WC] Display Quantity Break Titles
MAP1 QBK'TITLES(10),S,12
MAP1 QBK'TAG,S,4
!!

MAP1 CNT'OFFSET,B,1

SHORT'PRC:
        IF INC'SHORT'PRC # "Y" GOTO LONG'PRC
        IF CUS'DISC'PERC # 0 GOTO LONG'PRC                      !![56]
        IF SPR'CTL = 0 GOTO LONG'PRC                            !![55]
        CNT'OFFSET = 0
        IF OC2'CHK'QUOTE = "Y" THEN
            IF QUOTE'NAM'NO # HDR'NAM'NO OR QUOTES'YN # "N" THEN
                GOTO LONG'PRC
            ENDIF
        ENDIF
        IF PRH'PRC'UOM'NO = 1                                   AND &
                        ((PRH'LOC'NO = PRCD'KLOC'NO             AND &
                          PRH'PC'BY'LOC="Y")                    OR   &
                        (PRH'LOC'NO = 0 AND PRH'PC'BY'LOC="N")) THEN
                PRC'OVR = PRH'PRC'OVR
                PRC'CTL = 0
        ELSE
                PRC'KTRK'NO = PRCD'KTRK'NO
                PRC'KUOM'NO = 1
                PRC'KLOC'NO = PRCD'KLOC'NO
                PRC'SHORT'PRC = "N"
                \VUSE PRC
        ENDIF
        IF PRC'SHORT'PRC # "Y" GOTO LONG'PRC
        CNT = 0
        DO
                CNT = CNT + 1
                IF CNT = 1 THEN
                        LIN'UOM'NO = 1
                        PRICE = PRC'AMT
                ELSE
                        IF CNT > 3 EXIT
                        LIN'UOM'NO = PRC'UOM(CNT-1)
                        PRICE = PRC'PRICE(CNT-1)
                ENDIF
                IF LIN'UOM'NO = 0 THEN
                        CNT'OFFSET = CNT'OFFSET + 1
                        REPEAT
                ENDIF
                CALL FIND'UOM
                IF UOM'DUNITS # 0 THEN
                        LIN'UOM'FACTOR(CNT-CNT'OFFSET) = UOM'NUNITS/UOM'DUNITS
                        UOM'TITLES(CNT-CNT'OFFSET) = RIGHT(SPACE(6)+UOM'DESC,6)
                ELSE
                        LIN'UOM'FACTOR(CNT-CNT'OFFSET) = 1
                        UOM'TITLES(CNT) = "**BAD**"
                ENDIF
                DISC = 0
                RETRO = 0
                LIN'TCPRICE(CNT-CNT'OFFSET) = PRICE
        LOOP 
        CNT = CNT - CNT'OFFSET - 1              !![49]
RETURN

LONG'PRC:
        CALL FIND'VIA
        IF VIA'PLOC'NO # 0 AND PRC'LOC'NO # 0 THEN
            MY'LOC'NO = VIA'PLOC'NO
        ELSE
            MY'LOC'NO = PRC'LOC'NO
        ENDIF

        PRCD'KLOC'NO  = MY'LOC'NO
        RDKEY PRCD
        DO
                GETNXT PRCD
                PRCD'KEY = PRCD'KEY2
                IF PRCD'CTL # 0 OR PRCD'KTRK'NO # SAV'TRK'KEY   OR &
                        PRCD'KLOC'NO # MY'LOC'NO                OR &
                                             PRCD'KUOM'NO >= 1200 EXIT
                TMP'ITEMS = 1
                TMP'QTY = 1
                LIN'UOM'NO = PRCD'KUOM'NO
                CALL FIND'UOM
                CNT = CNT + 1
                IF UOM'DUNITS # 0 THEN
                    LIN'UOM'FACTOR(CNT) = UOM'NUNITS/UOM'DUNITS
                    UOM'TITLES(CNT) = RIGHT(SPACE(6)+UOM'DESC,6)
                ELSE
                    LIN'UOM'FACTOR(CNT) = 1
                    UOM'TITLES(CNT) = "**BAD**"
                ENDIF
                QQ = UOM'NO
                CALL PRICE'MAC
                IF PG = REAL'PG CALL DSP'ORIG'PRICE
                LIN'TCPRICE(CNT-CNT'OFFSET) = PRICE
                IF PRH'QTD'COUNT > 0 THEN
                    CALL GET'QBK'PRICES
                ENDIF
        LOOP UNTIL CNT = 3
RETURN

CIF USE'CAP = "Y" THEN
    CAP'PRC:
        CNT = 3
        FOR XX = 1 TO 4
            IF XX >= TYP'COL'BASE THEN
                PRCD'KUOM'NO = OEC'COLQ(XX) * 12
                RDKEY PRCD
                IF PRCD'CTL # 0 THEN
                    GETPRV PRCD
                    PRCD'KEY = PRCD'KEY2
                    IF PRCD'CTL # 0 OR PRCD'KTRK'NO # SAV'TRK'KEY   OR &
                                                        PRCD'KLOC'NO # MY'LOC'NO THEN
                        ENTRY = "Invalid pricing for "+PRCD'KTRK'NO+" this order is a mess! "
                        \KERROR LIN MODE="D" CNO="N"
                        ENTRY = ENTRY + " XX="+XX+" COLQ="+OEC'COLQ(XX)
                        GOTO DO'ERR
                    ENDIF
                ENDIF
            ENDIF
            IF XX <= 3 THEN
                LIN'UOM'FACTOR(XX) = OEC'COLQ(XX)*12
            ELSE
                LIN'CAP'HFAC = OEC'COLQ(4)
            ENDIF
            IF XX >= TYP'COL'BASE THEN
                QQ = PRCD'KUOM'NO
                CALL PRICE'MAC
                IF XX <= 3 THEN
                    LIN'TCPRICE(XX) = PRICE
                ELSE
                    LIN'CAP'HPRICE = (PRICE/OEC'COLQ(4))/12
                ENDIF
            ENDIF
        NEXT XX
        IF TYP'COL'BASE > 1 THEN
            IF TYP'COL'BASE = 4 THEN
                PRICE = LIN'CAP'HPRICE
            ELSE
                PRICE = LIN'TCPRICE(TYP'COL'BASE)/LIN'UOM'FACTOR(TYP'COL'BASE)
            ENDIF
            FOR XX = 1 TO (TYP'COL'BASE-1)
                LIN'TCPRICE(XX) = PRICE*LIN'UOM'FACTOR(XX)
            NEXT XX
        ENDIF
        QQ = 1
        CALL PRICE'MAC
        LIN'CAP'PCEPRC = (PRICE/LIN'CAP'PCEPRC)
    RETURN

ENDCIF

FIX'PRICES:                                             !![63]
        IF OEC'DISC'CASE = "N" THEN                     !![66]
            CALL GET'CASE'QQ
            IF CASE'QQ > 1 THEN
                IF LIN'TPRICE(CASE'QQ) # 0 AND LIN'UOM'FACTOR(CASE'QQ) # 0 THEN
                    QQ = LIN'TPRICE(CASE'QQ)/LIN'UOM'FACTOR(CASE'QQ)
                    IF LIN'TPRICE(1) < QQ THEN LIN'TPRICE(1) = QQ
                    IF DZ'QQ > 1 THEN
                        IF (LIN'TPRICE(DZ'QQ)/LIN'UOM'FACTOR(DZ'QQ)) < QQ THEN
                            LIN'TPRICE(DZ'QQ) = QQ * LIN'UOM'FACTOR(DZ'QQ)
                        ENDIF
                    ENDIF
                ENDIF
            ENDIF
        ENDIF
!![66] - to end of routine.
        FOR II = 1 TO 3
            IF LIN'UOM'FACTOR(II) > 1 THEN
                \FROUND LIN'TPRICE(II)/LIN'UOM'FACTOR(II) LIN'TPRICE(II)
                LIN'TPRICE(II) = LIN'TPRICE(II) * LIN'UOM'FACTOR(II)
            ENDIF
        NEXT II
RETURN

SHORT'PRC2:
        IF PRC'UOM(3) # 0 THEN
                LIN'UOM'NO = 1200
                CALL FIND'UOM
                IF UOM'DUNITS # 0 THEN
                        LIN'UOM'FACTOR(2) = UOM'NUNITS/UOM'DUNITS
                        UOM'TITLES(2) = " Dozen"
                ELSE
                        LIN'UOM'FACTOR(2) = 1
                        UOM'TITLES(2) = "**BAD**"
                ENDIF
                PRICE = PRC'PRICE(3)
                DISC = 0
                RETRO = 0
        ELSE
                SF = 1
        ENDIF
RETURN

LONG'PRC2:
                        !![85] - added IF below in case regular pricing
                        !!       already did a getnxt to this price.
        IF PRCD'CTL # 0 OR PRCD'KTRK'NO # SAV'TRK'KEY    OR     &
                   PRCD'KLOC'NO # MY'LOC'NO OR PRCD'KUOM'NO # 1200 THEN
                GETNXT PRCD
                PRCD'KEY = PRCD'KEY2
        ENDIF

!!      IF PRCD'CTL # 0 OR PRCD'KTRK'NO # SAV'TRK'KEY    OR     &
!!                 PRCD'KLOC'NO # MY'LOC'NO OR PRCD'KUOM'NO # 1200 THEN
        IF PRCD'CTL # 0 OR PRCD'KTRK'NO # SAV'TRK'KEY OR PRCD'KUOM'NO # 1200 THEN
                SF = 1
                PRICE = 0
        ELSE
                LIN'UOM'NO = 1200
                CALL FIND'UOM
                IF UOM'DUNITS # 0 THEN
                        LIN'UOM'FACTOR(2) = UOM'NUNITS/UOM'DUNITS
                        UOM'TITLES(2) = " Dozen"
                ELSE
                        LIN'UOM'FACTOR(2) = 1
                        UOM'TITLES(2) = "**BAD**"
                ENDIF
                QQ = 1200
                CALL PRICE'MAC          !Find 100 dz price?
        ENDIF
RETURN

MAP1 WRK'TYP'NO,B,2                             !![55]

SET'HPRICE:                                     !![79] - this routine
!!LIN'QFREE'FRT only set if price came from a quote
        SF = 0
        IF LIN'QFREE'FRT = "Y" OR LIN'QFREE'FRT = "N" RETURN !!KLUDGE
        CALL SEE'IF'HPRICE
        HPRICE'NOTE = SPACE             !![95]
        IF SF # 0 THEN                  !!Possible special price
            CIF USE'CAP = "Y" THEN
                IF INC'TS'SUBI = 4 THEN
                    PRH2'HPRICE = LIN'CAP'HPRICE
                ELSE
            ENDCIF
            IF PRH'PRC'TRK'NO # 0 THEN
                IF PRH2'TRK'NO # PRH'PRC'TRK'NO THEN
                    PRH2'KTRK'NO = PRH'PRC'TRK'NO
                    PRH2'SPACE = SPACE
                    \VUSE PRH2 KTRK'NO PASSED'PRH'ARRAY AMOS="Y"
                ENDIF
            ELSE
                PRH2'REC = PRH'REC
            ENDIF
            CIF USE'CAP = "Y" THEN
                ENDIF
            ENDCIF
            IF PRH2'PC'BY'LOC = "Y" THEN
                \COPYF PRC IN PRC4 MODE="PRI"
                ENTRY = "Type"+STR(HDR'DOC'TYPE)+"LOC_NO"
                \DG GETVALUE _OESETUP ENTRY PRICE'LOC'NO TAGVAR="Y"
                IF DG'CTL = 0 AND PRICE'LOC'NO # 0 THEN
                    IF PRICE'LOC'NO = LIN'LOC'NO THEN
                        MY'LOC'NO = HDR'LOC'NO
                    ENDIF
                ENDIF

                CALL FIND'VIA
                PRC4'KTRK'NO = PRH2'TRK'NO
                PRC4'KUOM'NO = 1201
                PRC4'KLOC'NO = MY'LOC'NO
                USE PRC4 INTO PRC4'REC KEY PRC4'KEY
                IF PRC4'CTL = 0 THEN
                    PRH2'HPRICE = PRC4'AMT/12
                ENDIF
            ENDIF
            IF PRH2'HPRICE = 0 THEN
                SF = 0                          !!Nope, not possible
            ENDIF
        ENDIF
        IF PRH2'HPRICE # 0 THEN
            IF PRICE'LOC'NO = 0 THEN
                IF VIA'PLOC'NO # 0 THEN
                    PRICE'LOC'NO = VIA'PLOC'NO
                ELSE
                    PRICE'LOC'NO = LIN'LOC'NO
                ENDIF
            ENDIF
            \PRICE'PPD LOC=PRICE'LOC'NO
            PRH2'HPRICE = PRH2'HPRICE - PRICE'PPD'DISC
        ENDIF
        IF SF = 2 THEN
            \DG GETSTRING _OESETUP oe1qih_20_case_desc ENTRY
            IF DG'CTL = 0 AND ENTRY # "" THEN
                HPRICE'NOTE = ENTRY + " pricing!!!"
            ELSE
                HPRICE'NOTE = STR(INC'HCASES) + " case special pricing!!!!!!!"
            ENDIF
        ELSE
            IF SF = 1 THEN
                II = INC'HCASES - TCASES
                CIF USE'CAP = "Y" THEN
                    SYMBOL CAPKLUDGE="OR INC'TS'SUBI=4"
                    IF INC'HCASES < 999 OR |CAPKLUDGE| PWASTR[8;1] = "Y" THEN
                CELSE
                    SYMBOL CAPKLUDGE=""
                ENDCIF
                IF INC'HCASES < 999 OR PWASTR[8;1] = "Y" THEN
                    IF INC'DZ'PIECES = "Y" THEN
                        HPRICE'NOTE = (II USING "###") + " cases for price " + &
                                (PRH2'HPRICE/100 USING "$$###.##") + "/" + &
                                ((PRH2'HPRICE*12)/100 USING "$$###.##")
                    ELSE
                        HPRICE'NOTE = (II USING "###") + " cases needed for a price of "      + &
                        (PRH2'HPRICE/100 USING "$$###.##")
                  ENDIF
                ENDIF
            ENDIF
        ENDIF
        IF SF = 0 THEN
            PRICE = 0
        ELSE
            PRICE = PRH2'HPRICE
        ENDIF
RETURN

MAP1 HPRICE'YN,S,1
LCL1 OTCPRICE(3),B,5

SETUP'TYP:
        IF HDR'PROC >= 5 THEN           !![80] this case.
                TYP'KNO = 1
                USE TYP INTO TYP'REC KEY TYP'KEY
                GETNXT TYP INTO TYP'REC
                WRK'TYP'NO = TYP'NO
        ELSE
                SPR'CTL = 3
                SPR'SPACE = SPACE
                IF CUS'SPR'COUNT # 0 THEN
                        SPR'KCUS'NO = HDR'NAM'NO
                        IF SPR'SEC'NO # PRH'SEC'NO OR SPR'CTL # 0 THEN
                                SPR'KSEC'NO = PRH'SEC'NO
                                USE SPR INTO SPR'REC KEY SPR'KEY
                        ENDIF
                        IF SPR'CTL # 0 THEN
                                SPR'KSEC'NO = 0
                                USE SPR INTO SPR'REC KEY SPR'KEY
                        ENDIF
                ENDIF
                IF SPR'CTL = 0 THEN
                        WRK'TYP'NO = SPR'TYP'NO
                ELSE
                        WRK'TYP'NO = HDR'NAM'TYP'NO
                ENDIF
                IF WRK'TYP'NO # TYP'NO THEN
                        TYP'KNO = WRK'TYP'NO
                        \VUSE TYP
                        IF TYP'CTL # 0 THEN
                            TYP'SPACE = SPACE
                            FOR QQ = 1 TO 3
                                TYP'CPRICE(QQ) = QQ
                                TYP'NPRICE(QQ) = QQ
                            NEXT QQ
                            TYP'CYN = "N"
                            TYP'NYN = "N"
                        ENDIF
                ENDIF
        ENDIF
CIF USE'CAP = "Y" THEN
    IF INC'TS'SUBI = 4 THEN
        FOR QQ = 1 TO 3
            TPRICE(QQ) = QQ
        NEXT QQ
        HPRICE'YN = "Y"
    ELSE
ENDCIF
        FOR QQ = 1 TO 3
            IF CASH'TERMS = "Y" THEN
                TPRICE(QQ) = TYP'CPRICE(QQ)
            ELSE
                TPRICE(QQ) = TYP'NPRICE(QQ)
            ENDIF
        NEXT QQ
        IF CASH'TERMS = "Y" THEN
            HPRICE'YN = TYP'CYN
        ELSE
            HPRICE'YN = TYP'NYN
        ENDIF
        IF PRH'PC'BY'LOC = "Y" THEN
            CALL FIND'VIA
            IF VIA'PLOC'NO # 0 THEN
                CSD'KLOC'NO = VIA'PLOC'NO
            ELSE
                CSD'KLOC'NO = LIN'LOC'NO
            ENDIF
        ELSE
            CSD'KLOC'NO = 0
        ENDIF
        IF PRH'PRC'TRK'NO = 0 THEN
            CSD'KTRK'NO = PRH'TRK'NO
        ELSE
            CSD'KTRK'NO = PRH'PRC'TRK'NO
        ENDIF
        CSD'KUOM'NO = 1
        CSD'KCUS'NO = HDR'NAM'NO
        RDKEY CSD
        IF CSD'CTL = 0 THEN
            HPRICE'YN = "N"
            FOR QQ = 1 TO 3
                TPRICE(QQ) = 1
            NEXT QQ
            USING'CSD = 1
        ELSE
            USING'CSD = 0
        ENDIF
CIF USE'CAP = "Y" THEN
    ENDIF
ENDCIF
RETURN

MAP1 USING'CSD,B,1

DO'PRICE:
        FOR QQ = 1 TO 5
            IF QQ <= 3 THEN
                OTCPRICE(QQ) = LIN'TCPRICE(QQ)
            ENDIF
            IF QQ <= 4 THEN
                TPRICE(QQ) = 0
            ENDIF
            APRICE(QQ) = 0
        NEXT QQ
        CALL SETUP'TYP
!!      APRICE(5) = 0
        IF USING'CSD = 0 THEN
            IF LIN'UOM'FACTOR(1) # 0 AND HPRICE'YN = "Y" AND INC'TS'SUBI # 4 THEN
                CALL SET'HPRICE
                APRICE(5) = PRICE * SGN(SF)
                IF SF = 2 THEN
                        FOR QQ = 1 TO 3
                                LIN'TCPRICE(QQ) = APRICE(5)*LIN'UOM'FACTOR(QQ)
                                TPRICE(QQ) = 5
                        NEXT QQ
                        TPRICE(4) = 5
                        IF APRICE(5) # 0 THEN
                                CALL GET'CASE'QQ
                                GOTO SKIP'SPECIAL'PRICE
                        ENDIF
                ENDIF
            ELSE
                HPRICE'NOTE = "Y"
            ENDIF
        ENDIF
        IF PRH'PC'BY'LOC = "N" THEN
            PRCD'KLOC'NO = 0
        ELSE
            CALL FIND'VIA
            IF VIA'PLOC'NO # 0 THEN
                PRCD'KLOC'NO = VIA'PLOC'NO
            ELSE
                PRCD'KLOC'NO = LIN'LOC'NO
            ENDIF
        ENDIF
        PRC'LOC'NO = PRCD'KLOC'NO
        IF PRH'PRC'TRK'NO = 0 THEN
            PRCD'KTRK'NO = LIN'TRK'NO
        ELSE
            PRCD'KTRK'NO = PRH'PRC'TRK'NO
        ENDIF
        SAV'TRK'KEY = PRCD'KTRK'NO
        PRCD'KUOM'NO = 1
        CNT = 0
        UOM'TITLES'SPACE = SPACE(36)
CIF USE'CAP = "Y" THEN
        IF INC'TS'SUBI = 4 THEN
            CALL CAP'PRC
        ELSE
ENDCIF
        IF INC'SHORT'PRC = "Y" THEN             !![46] - all of this!
            CALL SHORT'PRC
        ELSE
            CALL LONG'PRC
        ENDIF
CIF USE'CAP = "Y" THEN
        ENDIF
ENDCIF
        IF CNT < 3 THEN                         !!Clean up rest of WGP
            FOR II = CNT+1 TO 3
                LIN'TCPRICE(II) = 0             !Calc Prices By UOM
                LIN'UOM'FACTOR(II) = 0          !UOM factor
            NEXT II
        ENDIF
        CALL GET'CASE'QQ
        IF HPRICE'NOTE = "Y" AND HPRICE'YN = "Y" THEN
            CALL SET'HPRICE
            APRICE(5) = PRICE * SGN(SF)
            IF SF = 2 THEN
                FOR QQ = 1 TO 3
                    LIN'TCPRICE(QQ) = APRICE(5)*LIN'UOM'FACTOR(QQ)
                    TPRICE(QQ) = 5
                NEXT QQ
                TPRICE(4) = 5
                IF APRICE(5) # 0 GOTO SKIP'SPECIAL'PRICE
            ENDIF
        ELSE
        IF LIN'QFREE'FRT = "" THEN
            HPRICE'NOTE = SPACE
        ENDIF
        ENDIF
!!GETS 100 DZ PRICE INTO "PRICE" in case no 100 Dozens price
        IF INC'TS'SUBI # 4 THEN
            CASE
            IF CUS'100'DZ = "Y" THEN
                PRICE = 0
            ELSE
            IF CHANGE'100'DZ'TO = "C" AND CASE'QQ # 0 THEN
                PRICE = LIN'TPRICE(CASE'QQ)/LIN'UOM'FACTOR(CASE'QQ)
            ELSE
            IF CHANGE'100'DZ'TO = "P" THEN
                PRICE = LIN'TPRICE(1)
            ELSE
            IF DZ'QQ # 0 THEN
                PRICE = LIN'TPRICE(DZ'QQ)/LIN'UOM'FACTOR(DZ'QQ)
            ELSE
                PRICE = LIN'TPRICE(1)
            ENDCASE
            PRICE = PRICE*12
            FOR QQ = 1 TO 3
              IF TPRICE(QQ) = 4 OR CUS'100'DZ = "Y" THEN
                  IF INC'SHORT'PRC = "Y" AND PRC'SHORT'PRC = "Y" THEN
                      CALL SHORT'PRC2
                  ELSE
                      CALL LONG'PRC2
                  ENDIF
                  QQ = 5
              ENDIF
            NEXT QQ
        ENDIF
        APRICE(1) = LIN'TCPRICE(1)              !!PIECE
        IF DZ'QQ # 0 THEN               !!DOZEN
            APRICE(2) = LIN'TCPRICE(DZ'QQ)/LIN'UOM'FACTOR(DZ'QQ)
        ELSE
            APRICE(2) = APRICE(1)
        ENDIF
        IF CASE'QQ # 0 THEN             !!CASE
            APRICE(3) = LIN'TCPRICE(CASE'QQ)/LIN'UOM'FACTOR(CASE'QQ)
        ELSE
            APRICE(3) = APRICE(2)
        ENDIF
        APRICE(4) = PRICE/12                    !!100 DZ PRICE
        !!WE SET APRICE(5) AT SET'HPRICE        !!MULTI CASE PRICE

        IF APRICE(2) = 0 THEN           !!ZERO DZ USE PIECE
            APRICE(2) = APRICE(1)
        ENDIF
        IF APRICE(3) = 0 THEN           !!ZERO CASE USE DZ
            APRICE(3) = APRICE(2)
        ENDIF
        IF APRICE(4) = 0 THEN           !!ZERO 100 DZ USE DZ
            CASE
            IF CHANGE'100'DZ'TO = "C" THEN
                APRICE(4) = APRICE(3)
            ELSE
            IF CHANGE'100'DZ'TO = "P" THEN
                APRICE(4) = APRICE(1)
            ELSE
                APRICE(4) = APRICE(2)
            ENDCASE
        ENDIF
        IF APRICE(5) = 0 THEN           !!ZERO MULTI CASE USE CASE
            IF CHANGE'100'DZ'TO = "C" THEN
                APRICE(5) = APRICE(4)
            ELSE
                APRICE(5) = APRICE(3)
            ENDIF
        ENDIF
!!Now we have all five prices in a per piece array.
        IF CUS'100'DZ = "Y" AND PRICE # 0 THEN
            LIN'TCPRICE(1) = APRICE(4)      !!Set piece price
            IF DZ'QQ # 0 THEN                       !!Set dozens price
                LIN'TCPRICE(DZ'QQ) = APRICE(4)*LIN'UOM'FACTOR(DZ'QQ)
            ENDIF
            IF CASE'QQ # 0 THEN                     !!Set case price
                LIN'TCPRICE(CASE'QQ) = APRICE(4)*LIN'UOM'FACTOR(CASE'QQ)
            ENDIF
        ELSE
            LIN'TCPRICE(1) = APRICE(TPRICE(1))      !!Set piece price
            IF DZ'QQ # 0 THEN                       !!Set dozens price
                LIN'TCPRICE(DZ'QQ) = APRICE(TPRICE(2))*LIN'UOM'FACTOR(DZ'QQ)
            ENDIF
            IF CASE'QQ # 0 THEN                     !!Set case price
                LIN'TCPRICE(CASE'QQ) = APRICE(TPRICE(3))*LIN'UOM'FACTOR(CASE'QQ)
            ENDIF
        ENDIF
  SKIP'SPECIAL'PRICE:
        FOR QQ = 1 TO 3
            IF INX1 = I'TAB OR OTCPRICE(QQ) # LIN'TCPRICE(QQ) OR &
                                          LOOP'LIN'FLAG = 6 THEN
                LIN'TPRICE(QQ) = LIN'TCPRICE(QQ)
            ENDIF
        NEXT QQ
        CALL FIX'PRICES                         !![72]
        LIN'UOM'NO = 1
        CALL FIND'UOM                           !!Set it back to LIN'UOM'NO
        IF TOP'MARK = 0 AND YANK'FLAG = 0 THEN
            OPR'FLAG = 1
            CALL PRE'DSP'UOM'TITLES
        ENDIF
        IF SPG # 0 OR |NM| # REAL'PG OR TOP'MARK # 0 OR YANK'FLAG # 0 THEN
            CALL SET'LIN'PRICE'DISC
            LIN'NET = NETP
            RETURN                                  !!KLUDGE
        ENDIF

        CALL PRE'DSP'WGP'PRICES

DSP'PRICING:
        CALL SET'LIN'PRICE'DISC
        LIN'NET = NETP                                  !![69]
        CALL DSP'NET
        IF OEC'SKIP'UP = "Y" AND ARCHIVE'FLAG = "C" AND INC'TS'SUBI # 4 THEN
            CALL DSP'HPRICE'NOTE
        ENDIF
RETURN

MAP1 CAPAMT,B,5
CIF USE'CAT = "Y" THEN
    LCL1 ORG'QQ,B,2
ENDCIF

MAP1 TST,S,1

PRICE'MAC:
!!Do not put QQ in quotes - it is a local!
!![67] - below use WRK'TYP'NO not HDR'NAM'TYP'NO, look up TYP record, add SPR'ALREADY
    IF OEC'PPRGNAM = "OEPACK" THEN RETRO = 0

!![WC] Ignore >>>>>>>>
CIF USE'CAP = "Y" THEN
    IF INC'TS'SUBI = 4 THEN
        IF QQ = 1 THEN
            TST = "Y"
            PRCD'KUOM'NO = 1
            USE PRCD INTO PRC'REC KEY PRCD'KEY
            IF PRCD'CTL # 0 OR PRC'AMT = 0 THEN
                CAPAMT = TYP'PCE'BASE
                CAPAMT = (CAPAMT MAX 1) MIN 4
                QQ = OEC'COLQ(CAPAMT)*12
                CAPAMT = TYP'PCE'AMT
                LIN'CAP'PCEPRC = QQ
            ELSE
                !!QQ=1
                CAPAMT = 0
                LIN'CAP'PCEPRC = 1
            ENDIF
            ORG'QQ = QQ
        ELSE
            TST = "N"
            ORG'QQ = QQ
            IF TYP'BRK'BASE > 0 THEN
                CAPAMT = TYP'BRK'BASE
                CAPAMT = (CAPAMT MAX 1) MIN 4
                QQ = (OEC'COLQ(CAPAMT)*12) MAX ORG'QQ
            ENDIF
            CAPAMT = -TYP'BRK'AMT
        ENDIF
        TMP'QTY = QQ
        II = QQ
        TMP'UQTY = 1
    ELSE
!![WC] Ignore <<<<<<<<<
ENDCIF
        CAPAMT = 0
        TST = "N"
CIF USE'CAP = "Y" THEN
    ENDIF
ENDCIF
    CALL CHECK'QUOTE
    IF SF = 0 OR ON'SALE'AND'QUOTE = "Y" THEN
                !!NOT a quote -- or -- quote and on sale
!!Note - took out PRICE'QTY was = II was related to SQ'FEET ...
        SPRICE = PRICE            !!Quote price
        SDISC = DISC
        MAP1 MY'LOC'NO,B,3
        MAP1 PRICE'LOC'NO,B,3
        CALL FIND'VIA
        PRICE'LOC'NO = 0
        ENTRY = "Type"+STR(HDR'DOC'TYPE)+"LOC_NO"
        \DG GETVALUE _OESETUP ENTRY PRICE'LOC'NO TAGVAR="Y"
        IF DG'CTL = 0 AND PRICE'LOC'NO # 0 THEN
            IF PRICE'LOC'NO = LIN'LOC'NO THEN
                PRICE'LOC'NO = HDR'LOC'NO
            ELSE
                PRICE'LOC'NO = 0
            ENDIF
        ENDIF
        IF PRICE'LOC'NO = 0 THEN
            IF VIA'PLOC'NO # 0 THEN
                PRICE'LOC'NO = VIA'PLOC'NO
            ELSE
                PRICE'LOC'NO = LIN'LOC'NO
            ENDIF
        ENDIF

        \PRICE UOM=QQ QTY="TMP'QTY" TYPE="WRK'TYP'NO"               &
                PRICE="PRICE" DISC="DISC" LOC="PRICE'LOC'NO"          &
                RETRO="RETRO" CUS'NO="HDR'NAM'NO" ITEMS="TMP'ITEMS" &
                UQTY="TMP'UQTY" SPR'ALREADY="Y"                     &
                PRHPTR="PASSED'PRH'ARRAY" TYPPTR="TYPPTR"           &
                PRCPTR="PRCPTR" CAPS="CAPAMT" PIECE="TST"           &
                ORG'NET="ORIG'PRICE"
        IF PRC'SLD'RET'YN = "N" OR OEC'PPRGNAM = "OEPACK" THEN RETRO = 0
        IF QQ > 12 AND OEC'DISC'CASE = "N" THEN DISC = 0        !![72]
        PRICE = PRICE - DISC - RETRO
        IF PRH'ON'SALE = "Y" AND DISC > 0 THEN
            CASE
            IF QQ = 1 THEN
                SAVE'ORIG'PRICE = (ORIG'PRICE/100 USING "##.##") + "ea"
            ELSE
            IF QQ = 6 THEN
                SAVE'ORIG'PRICE = SAVE'ORIG'PRICE + " " + (ORIG'PRICE/6/100 USING "###.##") + "pk"
            ELSE
            IF QQ = 12 THEN
                SAVE'ORIG'PRICE = SAVE'ORIG'PRICE + " " + (ORIG'PRICE/12/100 USING "###.##") + "dz"
            ELSE
                SAVE'ORIG'PRICE = SAVE'ORIG'PRICE + " " + (ORIG'PRICE/LIN'UOM'FACTOR(3)/100 USING "###.##") + "cs"
            ENDCASE
        ENDIF
        DISC = 0 : RETRO = 0
        CIF USE'CAP = "Y" THEN
            IF INC'TS'SUBI = 4 AND ORG'QQ # QQ THEN
                PRICE = (PRICE/QQ)*ORG'QQ
            ENDIF
        ENDCIF
                    !!Sale price is in PRICE
        IF ON'SALE'AND'QUOTE = "Y" THEN
            IF (PRICE-DISC) < (SPRICE-SDISC) THEN
                ENTRY = "Prices from sale part "
                \KERROR HDR ALTPRE="HDR2" CNO="N" MODE="D"
                \STRDTT DTT=HDR2'PROM'DTT DDATE="MY'DATE"
                ENTRY = ENTRY + " Expires: " + MY'DATE + " Free: "+ HDR2'FREE'FRT + " Line: "
                \KERROR LIN CNO="N" MODE="D"
                XCALL VICSIT,26,ENTRY,"/logs/oe1qut.DAILY"
            ELSE
                PRICE = SPRICE
                DISC = SDISC
                ENTRY = "Prices from quote "
                \KERROR HDR ALTPRE="HDR2" CNO="N" MODE="D"
                \STRDTT DTT=HDR2'PROM'DTT DDATE="MY'DATE"
                ENTRY = ENTRY + " Expires: " + MY'DATE + " Free: "+ HDR2'FREE'FRT + " Line: "
                \KERROR LIN CNO="N" MODE="D"
                XCALL VICSIT,26,ENTRY,"/logs/oe1qut.DAILY"
            ENDIF
        ENDIF
    ELSE
        SF = 0
    ENDIF
RETURN

MAP1 SAV'TRK'NO,B,3

SET'LIN'PRICE'DISC:
        CALL COMPUTE'NETS
        IF GTTL'QTY # 0 THEN
                \FROUND CALC'NETP/GTTL'QTY LIN'CALC'PRICE
                \FROUND NETP/GTTL'QTY LIN'PRICE
        ELSE
                LIN'CALC'PRICE = LIN'TCPRICE(1)
                LIN'PRICE = LIN'TPRICE(1)
        ENDIF
RETURN

!!MAP1 TOTAL'FLAG,B,1
MAP1 TTL'QTY,F,6                !![20]
MAP1 TTL'CQTY,F,6               !![20]
MAP1 GTTL'QTY,F,6               !![20]
MAP1 GTTL'CQTY,F,6              !![20]

COMPUTE'NETS:
        IF ARCHIVE'FLAG = "C" THEN
                LIN'SLIP'PRICE = OEC'SLIP'PRICE
        ENDIF
        GTTL'QTY = 0
        GTTL'CQTY = 0
        CALC'NETP = 0
        CALC'NETD = 0
        NETP = 0
        NETD = 0
        CNETP = 0
        CNETD = 0

        IF PG = |NM| THEN
                IF LIN'USR'BQTY = 0 OR LIN'USR'CQTY = 0 THEN
                        CALL LOAD'WGA
                ELSE
                        WGA'SPACE = SPACE
                        CALL SET'LOW'VALUES3
                        CALL PRE'SET'USE3
                        DO
                                SET'XXX = 1
                                CALL PRE'GETNXT3
                                SET'XXX = 0
                                CALL BAD'KEY'TEST3
                                IF TMP'CTL(3) # 0 EXIT
                                II = SUB'NO
                                IF II >= 1 AND II <= 6 THEN
                                        IF INC'DZ'PIECES # "N" THEN
                                            IF ARCHIVE'FLAG = "C" THEN
                                                        WGA'DOZENS(II) = FIX(SUB'USR'OQTY/12)
                                                        WGA'PIECES(II) = SUB'USR'OQTY - (WGA'DOZENS(II)*12)
                                                        WGA'BDOZENS(II) = FIX(SUB'USR'BQTY/12)
                                                        WGA'BPIECES(II) = SUB'USR'BQTY - (WGA'BDOZENS(II)*12)
                                            ELSE
                                                        WGA'DOZENS(II) = FIX(SUB'USR'CQTY/12)
                                                        WGA'PIECES(II) = SUB'USR'CQTY - (WGA'DOZENS(II)*12)
                                            ENDIF
                                        ELSE
                                            IF ARCHIVE'FLAG = "C" THEN
                                                        WGA'PIECES(II) = SUB'USR'OQTY
                                                        WGA'BPIECES(II) = SUB'USR'BQTY
                                            ELSE
                                                        WGA'PIECES(II) = SUB'USR'CQTY
                                            ENDIF
                                        ENDIF
                                ENDIF
                        LOOP
                ENDIF
        ENDIF
          FOR ZZ = 1 TO 6
            IF INC'DZ'PIECES # "N" THEN
                        TTL'QTY = (WGA'DOZENS(ZZ)*12) + WGA'PIECES(ZZ)
                        TTL'CQTY = TTL'QTY - ((WGA'BDOZENS(ZZ)*12) + WGA'BPIECES(ZZ)) !![81]
            ELSE
                        TTL'QTY = WGA'PIECES(ZZ)
                        TTL'CQTY = TTL'QTY - WGA'BPIECES(ZZ)
            ENDIF
            IF TTL'QTY # 0 THEN
                  GTTL'QTY = GTTL'QTY + TTL'QTY
CIF USE'CAP = "Y" THEN
                  IF INC'TS'SUBI = 4 THEN
                                II = WGA'PIECES(ZZ)
                                TTL'QTY = TTL'QTY - II
                                NETP = NETP + (II * LIN'CAP'PCEPRC)
                                CALC'NETP = CALC'NETP + (II * LIN'CAP'PCEPRC)
                                II = WGA'DOZENS(ZZ)*12
                                IF CAPTTL >= (OEC'COLQ(4)*12) OR LIN'CAP'PCOL = 4 OR &
                                                                HIGH'COL = 4 THEN
                                        NETP = NETP + (II * LIN'CAP'HPRICE)
                                        CALC'NETP = CALC'NETP + (II * LIN'CAP'HPRICE)
                                        HIGH'COL = 4
                                ELSE
                                        FOR QQ = 3 TO 1 STEP -1
                                                IF CAPTTL >= (OEC'COLQ(QQ)*12) OR QQ = 1 OR &
                                                        QQ = LIN'CAP'PCOL OR QQ = HIGH'COL THEN
                                        NETP = NETP + (II*(LIN'TPRICE(QQ) / &
                                        LIN'UOM'FACTOR(QQ)))
                                        CALC'NETP = CALC'NETP + (II*(LIN'TCPRICE(QQ) / &
                                        LIN'UOM'FACTOR(QQ)))
                                                        HIGH'COL = HIGH'COL MAX QQ
                                                        QQ = 0
                                                ENDIF
                                        NEXT QQ
                                ENDIF
                  ELSE
ENDCIF
                   FOR QQ = 3 TO 1 STEP -1
                        IF LIN'UOM'FACTOR(QQ) # 0 THEN
                                II = FIX(TTL'QTY/LIN'UOM'FACTOR(QQ))
                                IF II # 0 AND LIN'SLIP'PRICE = "Y" THEN
                                        CALL CVT'PRICES
                                        NETP = NETP + (TTL'QTY * LIN'TPRICE(QQ))
                                        CALC'NETP = CALC'NETP + (TTL'QTY * LIN'TCPRICE(QQ))
                                        CALL UCVT'PRICES
                                        QQ = 1
                                ELSE
                                    IF PRH'QTD'COUNT = 0 THEN
                                        NETP = NETP + (II * LIN'TPRICE(QQ))
                                        CALC'NETP = CALC'NETP + (II * LIN'TCPRICE(QQ))
                                    ELSE
                                        CALL SET'QBK'PRC'BY'OQTY
                                        !![WC] Temporary used DSP'NET to Round the QBK Pricing to $#.##

                                        MOD'BASE = (II*LIN'TCPRICE(QQ))
                                        \FROUND MOD'BASE/10 DSP'NET

                                        NETP = NETP + DSP'NET
                                        CALC'NETP = CALC'NETP + DSP'NET
                                    ENDIF

                                    TTL'QTY = TTL'QTY - (II * LIN'UOM'FACTOR(QQ))
                                    IF TTL'QTY = 0 THEN QQ = 1
                                ENDIF
                        ENDIF
                   NEXT QQ
                  ENDIF
CIF USE'CAP = "Y" THEN
            ENDIF
ENDCIF

            IF TTL'CQTY # 0 THEN
                  GTTL'CQTY = GTTL'CQTY + TTL'CQTY
CIF USE'CAP = "Y" THEN
                  IF INC'TS'SUBI = 4 THEN
                                II = WGA'PIECES(ZZ) - WGA'BPIECES(ZZ)
                                TTL'CQTY = TTL'CQTY - II
                                CNETP = CNETP + (II * LIN'CAP'PCEPRC)
                                II = (WGA'DOZENS(ZZ) - WGA'BDOZENS(ZZ))*12
                                IF CAPTTL >= (OEC'COLQ(4)*12) OR LIN'CAP'PCOL = 4 OR &
                                                                                HIGH'COL = 4 THEN
                                        CNETP = CNETP + (II * LIN'CAP'HPRICE)
                                        HIGH'COL = 4
                                ELSE
                                        FOR QQ = 3 TO 1 STEP -1
                                                IF CAPTTL >= (OEC'COLQ(QQ)*12) OR QQ = 1 OR &
                                                                        LIN'CAP'PCOL = QQ OR HIGH'COL = QQ THEN
                                        CNETP = CNETP + (II*(LIN'TPRICE(QQ) / &
                                        LIN'UOM'FACTOR(QQ)))
                                                        HIGH'COL = HIGH'COL MAX QQ
                                                        QQ = 0
                                                ENDIF
                                        NEXT QQ
                                ENDIF
                  ELSE
ENDCIF
                   FOR QQ = 3 TO 1 STEP -1
                        IF LIN'UOM'FACTOR(QQ) # 0 THEN
                                II = FIX(TTL'CQTY/LIN'UOM'FACTOR(QQ))
                                IF II # 0 AND LIN'SLIP'PRICE = "Y" THEN
                                        CALL CVT'PRICES
                                        CNETP = CNETP + (TTL'CQTY * LIN'TPRICE(QQ))
                                        CALL UCVT'PRICES
                                        QQ = 1
                                ELSE
                                    IF PRH'QTD'COUNT = 0 THEN
                                        CNETP = CNETP + (II * LIN'TPRICE(QQ))
                                    ELSE
                                        MOD'BASE = (II*LIN'TPRICE(QQ))
                                        \FROUND MOD'BASE/10 DSP'NET

                                        CNETP = CNETP + DSP'NET
                                    ENDIF
                                        TTL'CQTY = TTL'CQTY - (II * LIN'UOM'FACTOR(QQ))
                                        IF TTL'CQTY = 0 THEN QQ = 1
                                ENDIF
                        ENDIF
                   NEXT QQ
                  ENDIF
CIF USE'CAP = "Y" THEN
            ENDIF
ENDCIF
          NEXT ZZ
RETURN

CIF USED OR NOT USED THEN               !![88] could not see where used

E'LIN'PRICE:
        IF EDSPSW # 0 GOTO E'LIN'PRICE'END
        IF INX1 = I'TAB THEN
                CALL DO'PRICE
                IER1 = 0
        ELSE
                CALL TST'FUNC
                IF TST'FUNC = 1 OR INX1 = I'UA GOTO E'LIN'PRICE'END  !!KLUDGE
        ENDIF
        IF EFCHG # 0 OR INX1 = I'TAB THEN
!!Changed price
!![69]          IF LIN'PRICE < LIN'DISC THEN
!!Override pricing code belongs here
!![69]                  ENTRY = "WARNING:  Discount exceeds price"
!![69]                  CALL DO'ERR
!![69]          ELSE
                        CALL DSP'PRICING
!![69]          ENDIF
        ENDIF
E'LIN'PRICE'END:
        CALL EDIT'PRICE
RETURN

ENDCIF

DDB'CHANGES:
        fldstr = 3
!!      fldend = 8
        IF INC'DZ'PIECES # "N" THEN
                DDB |SCREEN|
        ELSE
                DDB |ASCREEN|
        ENDIF
        fldstr = 0
        fldend = 0
RETURN

FIND'VIA:
        IF VIA'NO # HDR'VIA'NO THEN
                VIA'SPACE = SPACE
                VIA'SHP'FLAG = 3
                VIA'KNO = HDR'VIA'NO
                \VUSE VIA
        ENDIF
RETURN

MAP1 USE'QTY'FLAG,B,1

AVAILABLE:                                      !![44] - rewrote a lot
        AVAILABLE = 0
        IF PG = 3 AND SUB'FLAG = 2 THEN
            IF SUB'ATT'NO1 < 1 OR SUB'ATT'NO1 > 6 THEN
                ENTRY = "Cannot compute available(1): "+SUB'ATT'NO1
                GOTO DO'ERR
            ENDIF
            NOTAVL(SUB'ATT'NO1) = 0
        ELSE
            IF II < 1 OR II > 6 THEN
                ENTRY = "Cannot compute available(1): "+II
                GOTO DO'ERR
            ENDIF
            NOTAVL(II) = 0
        ENDIF
        IF HDR'QUOTE'YN = "Y" GOTO SET'QUOTE            !!KLUDGE
        QTY'LOC'NO = LIN'LOC'NO
        QTY'TRK'NO = LIN'TRK'NO
        IF PG = 3 AND SUB'FLAG = 2 THEN
                QTY'ATT'NO1 = SUB'ATT'NO1
        ELSE
                QTY'ATT'NO1 = II
        ENDIF
        IF QTY'ATT'NO1 = 0 RETURN                       !!KLUDGE
        QTY'ATT'NO2 = 0
        \SETKEY QTY APID="IN" MODE="KEY"
        IF PRH'INVENTORY # "N" THEN
                USE'QTY'FLAG = 1
                CALL AVAILABLE'COMPUTE
                USE'QTY'FLAG = 0
                IF PRH'INVENTORY = "O" THEN LIN'OVERIDE = 1

                IF AVAILABLE = 0 AND NOTAVL'BIN # 0 THEN
                    IF QTY'CGE'NO(1) = NOTAVL'BIN OR &
                                   QTY'ASL'NO(1) = NOTAVL'BIN OR &
                                   QTY'SCT'NO(1) = NOTAVL'BIN OR &
                                          QTY'BIN'NO(1) = NOTAVL'BIN THEN
                        IF PG = 3 AND SUB'FLAG = 2 THEN
                            NOTAVL(SUB'ATT'NO1) = 1
                        ELSE
                            NOTAVL(II) = 1
                        ENDIF
                    ENDIF
                ENDIF
        ELSE
                                                        !!NON-INVENTORIED
            AVAILABLE = 99999999999
            LIN'OVERIDE = 1
        ENDIF
        IF (HDR'BO'YN = "N" OR (PRH'ACTIVE # "Y" AND &
                (ENVSTR[19;1] # "Y" AND PRH'ACTIVE # "O"))) AND PG = 3 AND &
                         AVAILABLE <= 0 AND BO'FLAG = 0 AND &
                            LIN'OVERIDE = 0  THEN  !!bo'one'time
                ENTRY = "No Stock available, no backorders allowed, Func-Y Overrides"
                CALL DO'ERR
        ENDIF
        IF BO'FLAG = 0 THEN
                IF AVAILABLE <= 0 AND LIN'OVERIDE = 0 THEN
                    SNDAFR = "T"
                ELSE
                    SNDAFR = "E"
                ENDIF
                IF PG = 3 THEN
                    CALL SNDAFR'GAFR3'C'QTY
                    CALL SAFR3
                ENDIF
        ENDIF
RETURN

MAP1 DOKITS'ATT'NO1,B,2
MAP1 PAVAILABLE,B,5

AVAILABLE'COMPUTE:                      !![51] - fix this section.
        CIF USE'KITS AND USE'KITS = "Y" THEN
            IF XXX'KIT'BUILD = "Y" THEN
                IF PRH'KTD'FLAG = "O" THEN
                    IF USE'QTY'FLAG = 1 THEN
                        USE QTY INTO QTY'REC KEY QTY'KEY
                    ELSE
                        QTY'CTL = 0
                    ENDIF
                    IF QTY'CTL = 0 CALL AVAILABLE'ONE ELSE AVAILABLE = 0
                    PAVAILABLE = AVAILABLE
                ELSE
                    PAVAILABLE = 0
                ENDIF
                DOKITS'ATT'NO1 = QTY'KATT'NO1
                CALL AVAILABLE'BUILD
                QTY'KATT'NO1 = DOKITS'ATT'NO1    !!ruined in \DOKITS
            ENDIF
        ENDCIF
        IF USE'QTY'FLAG = 1 THEN
            USE QTY INTO QTY'REC KEY QTY'KEY
        ELSE
            QTY'CTL = 0
        ENDIF
        IF QTY'CTL = 0 THEN
            CALL AVAILABLE'ONE
        ELSE
            AVAILABLE = 0
        ENDIF
        IF ZZZ2'LOC'NO = LIN'LOC'NO AND ZZZ2'TRK'NO = LIN'TRK'NO THEN
!!i.e.  CURRENT LINE ITEM COMMITTED INCLUDED IN QTY COM TOTALS
            TQTY = LIN'TCOM'QTY(QTY'KATT'NO1)
            TQTY = ZZZ2'TCOM'QTY(QTY'KATT'NO1)
            AVAILABLE = (AVAILABLE + TQTY) MIN 99999999999
            AVAILABLE = (AVAILABLE - UNDO'OLD'QTY) MAX 0
            UNDO'OLD'QTY = 0
        ENDIF
        AVAILABLE = AVAILABLE MAX 0
RETURN

AVAILABLE'ONE:  
        IF UOM'NUNITS # 0 THEN
                \AVAILABLE INVENTORY="PRH'INVENTORY"            !!DOKITS.MAC
                                !!DIVIDE TO RELATE TO CURRENT UOM
!!Prevent overflow of alpha 11 digits for non-inventoried items
!![16] Added .00...01 below - basic bug.
                AVAILABLE'EACH = AVAILABLE              !![25]
                AVAILABLE = (AVAILABLE * (UOM'DUNITS/UOM'NUNITS)+.00000000001) MIN 99999999999
        ELSE
                AVAILABLE = 0
        ENDIF
RETURN

CIF USE'KITS AND USE'KITS = "Y" THEN
  AVAILABLE'BUILD:
        DOKITS'FLAG = 1
        CALL DOKITS'MACRO
        CALL FIND'UOM
        IF UOM'NUNITS # 0 THEN
                        !!DIVIDE TO CURRENT UOM
!!Prevent overflow of alpha 11 digits for non-inventoried items
                AVAILABLE'EACH = AVAILABLE              !![25]
                AVAILABLE = (AVAILABLE * (UOM'DUNITS/UOM'NUNITS)) MIN 99999999999  
        ELSE
                AVAILABLE = 0
        ENDIF
  RETURN
ENDCIF

COPYF   "UOMFIL[IN]",=UOM2,NOAFR
COPYF   "UOMKEY[IN]",=UOM2              !!UNCOMMIT'DEL AND UOM PROBLEM

CIF USE'KITS AND USE'KITS = "Y" THEN
  DOKITS'MACRO:
        \DOKITS OE="Y" AVAILABLE="Y" PACTION="PACTION"          &
                        OPR'ACTION="OPR'ACTION" PRH="PRH2" UOM="UOM2" &
                        AVAILABLE'ABORT="OE1HDR" ATT1="DOKITS'ATT'NO1"
RETURN

OPR'ACTION:
   IF DOKITS'FLAG = 1 THEN
        USE QTY INTO QTY'REC KEY QTY'KEY
   ELSE
   IF DOKITS'FLAG = 3 THEN
        IF LOOP'SUB'FLAG # 1 THEN
                CALL RDCOND'QTY                 !!OPTIONAL LOCK ON UNCOMMIT
        ELSE
                CALL RDLOCK'QTY
        ENDIF
        IF QTY'CTL # 0 CALL ADD'QTY             !! SHOULD NOT HAPPEN
   ELSE
   IF DOKITS'FLAG >= 2 THEN
        CALL RDLOCK'QTY
        IF QTY'CTL # 0 CALL ADD'QTY             !! NEW ITEM, ADD RECORD
   ENDIF
   ENDIF
   ENDIF
   IF QTY'CTL # 0 THEN DOKITS'ABORT = "Q"               !!ABORT
  RETURN
ENDCIF

!![44]AVAILABLE'ONE - removed label
COPYF   "UOMFIL[IN]",=UOM2,NOAFR
COPYF   "UOMKEY[IN]",=UOM2              !!DEL'UNCOMMIT AND UOM PROBLEM

EDIT'PRICE:
        \DSPQTY MODE="P"
RETURN

CHAIN'OUT:
        CMN'SPACE = SPACE
        CMN'SAV'RCN(1) = HDR'RCN
        IF HDR'POST'LEVEL > 0 THEN
                CMN'FRM'PRGNAM = "RPSTH"+ARCHIVE'FLAG
        ELSE
                CMN'FRM'PRGNAM = "OE1HDR"
        ENDIF
        TST = "N"                       !!In user job
        LOC'KNO = HDR'LOC'NO
        CALL FIND'LOC
        IF HDR'POST'LEVEL > 0 THEN
!!Repost, set function key.
                IF HDR'ACTION # "" THEN
                        WINDOW = ASC(HDR'ACTION) - 48
                ELSE
                        WINDOW = FN'P
                ENDIF
        ENDIF
        CASE
        IF WINDOW = FN'P THEN
                IF HDR'PROC < 5 AND HDR'POST'LEVEL = 0 AND &
                                        HDR'DOC'TYPE # OEC'INT'CR THEN
                        IF OEC'PPRGNAM = "OEPACK" THEN
                                CMN'TO'PRGNAM = "OEPACK"
                        ELSE
                                CMN'TO'PRGNAM = "OE1SHP"
                        ENDIF
!![73] - test only if not internal credit.
                        IF INC'AUTOPACK # "Y" AND HDR'PICK'NEED # "N" AND &
                                                        HDR'POST'LEVEL = 0 THEN
                                ENTRY = "Cannot invoice without printing picking ticket!"
                                GOTO DO'ERR                     !!KLUDGE
                        ENDIF
                ELSE
                        CMN'TO'PRGNAM = "OERCTL"
                ENDIF
                HDR'ACTION = "P"
!![68] - test below for internal credit.
CIF USE'PQU = "Y" THEN
                IF LOC'INV'BGP > 0 THEN 
                        TST = "C"
                        BGP'PRI = LOC'INV'BGP
                ENDIF
ENDCIF
        ELSE
        IF WINDOW = FN'U THEN
                HDR'ACTION = "U"
                CMN'TO'PRGNAM = "OERCTL"
CIF USE'PQU = "Y" THEN
                IF LOC'PACK'BGP > 0 THEN
                        TST = "C"
                        BGP'PRI = LOC'PACK'BGP
                ENDIF
ENDCIF
        ELSE
        IF WINDOW = FN'L THEN
                HDR'ACTION = "L"
                CMN'TO'PRGNAM = "OERCTL"
CIF USE'PQU = "Y" THEN
                IF LOC'ORD'BGP > 0 THEN
                        TST = "C"
                        BGP'PRI = LOC'ORD'BGP
                ENDIF
ENDCIF
        ELSE
        IF WINDOW = FN'T THEN
                IF HDR'DOC'TYPE = OEC'INT'CR THEN               !![73]
                        ENTRY = "Cannot pick internal credits!"
                        GOTO DO'ERR                     !!KLUDGE
                ENDIF
                HDR'ACTION = "E"
                IF HDR'PROC < 5 AND HDR'POST'LEVEL = 0   AND &
                                        OEC'PPRGNAM[1;2] = "OE"  AND &
                                HDR'DOC'TYPE # OEC'INT'CR       THEN
                        CMN'TO'PRGNAM = OEC'PPRGNAM
                ELSE
                        CMN'TO'PRGNAM = "OERCTL"
CIF USE'PQU = "Y" THEN
                        IF LOC'PICK'BGP > 0 THEN
                                TST = "C"
                                BGP'PRI = LOC'PICK'BGP
                        ENDIF
ENDCIF
                ENDIF
        ENDCASE
        HDR'PQU'STAT = "Q"
        IF PG >= 2 CALL PRE'ABORT2|NM|
        PG = 1
        CALL PRE'UPDATE1
        SNDCMN CMN'REC
        CASE
        IF HDR'POST'LEVEL > 0 THEN
                ENTRY = "PASS THRU"
                CMN'OTHER = ENTRY
                XCALL DB,22,HDR'PKT             !!Release before chain.
                SNDCMN CMN'REC                  !!SND AGAIN FOR PASS THRU
                CHAIN "SYB1:SYXRES.RUN"
        ELSE
        IF CMN'TO'PRGNAM = "OE1SHP" OR CMN'TO'PRGNAM = "OEPACK" THEN
        ENTRY = CMN'TO'PRGNAM + ".RUN"
                IF V''SYSTEM # "UNIX" THEN
CIF USE'PQU = "Y" THEN
                  IF LOC'INV'BGP # 0 THEN
                        V''WINDOW'MEM0 = MEM(0)
                        XCALL ABUTIL,15,ENTRY,V''WINDOW'SIZE,V''WINDOW'LFA
                        IF V''WINDOW'LFA = 2 THEN
                                V''WINDOW'SIZE = V''WINDOW'MEM0 + 1
                        ELSE
!!Fake memory requirement as ABUTIL is ultra-safe ... risky?
                                V''WINDOW'SIZE = (V''WINDOW'SIZE - 52000) MAX 0
                        ENDIF
                  ELSE
                        V''WINDOW'SIZE = V''WINDOW'MEM0 + 1
                  ENDIF
CELSE
                        V''WINDOW'SIZE = V''WINDOW'MEM0 + 1
ENDCIF
                ELSE
                        V''WINDOW'SIZE = 0
                ENDIF
                IF V''WINDOW'SIZE <= V''WINDOW'MEM0 THEN
!![40] - pass PRH'ARRAY below.
                        CMN'TO'PRGNAM="LINK"
                        SNDCMN CMN'REC
                        CMN'TO'PRGNAM=OEC'PPRGNAM
                        \HDRLNK 1 ENTRY
                        IF CMN'FRM'PRGNAM="EXTRNL" THEN
                                CALL SET'FLIP
                                SELECT(1) = 3
                        ELSE
                                SELECT(1) = 1
                        ENDIF
                        PG = 1                          !!Maybe this work?
                        GDB'F = 0
                        TOGGLE'BTM(1) = TMP'TOGGLE
                        IF V''SYSTEM="UNIX" THEN
!!                              IF MEM(32) > 80 THEN
!!                                      CHAIN "oe1hdr"
!!                              ENDIF
                                LCL1 CCCC,B,1
                                CCCC = CCCC + 1
                                IF CCCC >= 10 THEN
                                        PTR'YES = "YES"
                                        SNDCMN PTR'PASS
                                        CHAIN "oe1hdr"
                                ENDIF
                        ENDIF
                        GOTO BEGIN
                ELSE
                        SNDCMN CMN'REC
                        CHAIN OEC'PPRGNAM
                ENDIF
!![39] Above
        ELSE
        IF CMN'TO'PRGNAM = OEC'PPRGNAM THEN
                CHAIN OEC'PPRGNAM
        ELSE
CIF USE'PQU = "Y" THEN
                IF TST # "N" THEN
                        XCALL PRTQUE,TST
                ENDIF
                IF TST="N" THEN
                        CHAIN CMN'TO'PRGNAM
                ELSE
MAP1 TST,S,1
MAP1 QNO,B,4
MAP1 BGP'PRI,B,1        
                        ENTRY = STR(HDR'NO) USING "#ZZZZZZ"
                        IF ENTRY[1;1] = "0" THEN
                                ENTRY[1;1] = STR(HDR'DOC'TYPE)
                        ENDIF
                        QNO = ENTRY
                        \PUTQUE TST """OERCTL""" BGP'PRI HDR'RCN QNO !![22]
                ENDIF
CELSE
                CHAIN CMN'TO'PRGNAM
ENDCIF
        ENDCASE
        CALL PRE'ABORT1
CHAIN "OE1HDR"

GLOBAL CLEAR'INPUT'AREA2="CLEAR'INPUT'AREA'LIN"

CLEAR'INPUT'AREA'LIN:
        CALL DO'CLEAR'INPUT'AREA
        PRINT TAB(24,1) ; TAB(-1,9) ;
        IF HDR'BO'YN # "N" CALL CLEAR'BO'S
        CALL CLEAR'INPUT'AREA'LIN2
RETURN

CLEAR'INPUT'AREA'LIN2:
        IF BOTTOM'AFU # 0 THEN
                \WINDOW SAVE="N" RESTORE="N" REPAINT="Y" &
                                TLR="TSCR(|NM|)" WINNUM="7"
                CALL SET'LINE'HDR1
                CALL DSP'LINE'HDR
                CALL CLR'BAR
                IF BOTTOM'AFU = 2 THEN
                    BOTTOM'AFU = 0
                    CALL PREP'FOR'RELOAD
                    IF LAST'SEL = 1 THEN
                        SELECT(|NM|) = 1
                        NXT'F(|NM|) = 1
                        KEY'TYPE(|NM|) = 0
                        CALL PREP'FOR'ADD
                        INX1 = 1 : INX2 = 0
                    ELSE
                        RPT'FLG(|NM|) = 5
                        CALL CENTER'LINE
                        RPT'FLG(|NM|) = 0
                    ENDIF
                ELSE
                    BOTTOM'AFU = 0
                    XLN = SXLN(|NM|)
                    CALL PRE'VDSP|NM|
                ENDIF
                XLN = 0
        ENDIF
RETURN

FIND'UOM:
        IF UOM'NO # LIN'UOM'NO OR UOM'CTL # 0 THEN
                UOM'NO = LIN'UOM'NO
                UOM'DUNITS = 1
                UOM'FD = "F"
                UOM'QFRACTION = 0
                UOM'CTL = 0
                CASE
                IF LIN'UOM'NO = 1 THEN
                        UOM'NUNITS = 1
                        UOM'DESC = "Each"
                ELSE
                IF LIN'UOM'NO = 12 OR LIN'UOM'NO = 1200 THEN
                        UOM'NUNITS = 12
                        UOM'DESC = "Dozen"
                ELSE
                IF LIN'UOM'NO = 24 OR LIN'UOM'NO = 36 OR LIN'UOM'NO = 48 &
                                OR LIN'UOM'NO = 72 OR LIN'UOM'NO = 144 THEN
                        UOM'NUNITS = LIN'UOM'NO
                        UOM'DESC = "Cs/"+STR(LIN'UOM'NO/12)
                ELSE
                        IF LIN'UOM'NO = UOM2'NO AND UOM2'CTL = 0 THEN
                                UOM'REC = UOM2'REC
                                UOM'CTL = 0
                        ELSE
                                UOM'KNO = LIN'UOM'NO
                                USE UOM INTO UOM'REC KEY UOM'KEY
                        ENDIF
                ENDCASE
        ENDIF
RETURN

FIND'UOM2:
!       IF UOM2'NO # TUOM OR UOM2'CTL # 0 THEN
!               IF TUOM # 1 THEN
!                       IF TUOM = UOM'NO AND UOM'CTL = 0 THEN
!                               UOM2'REC = UOM'REC
!                               UOM2'CTL = 0
!                       ELSE
!                               UOM2'KNO = TUOM
!                               USE UOM2 INTO UOM2'REC KEY UOM2'KEY
!                       ENDIF
!               ELSE
!                       UOM2'NO = 1
!                       UOM2'NUNITS = 1
!                       UOM2'DUNITS = 1
!                       UOM2'DESC = "Each"
!                       UOM2'FD = "F"
!                       UOM2'QFRACTION = 0
!                       UOM2'CTL = 0
!               ENDIF
!       ENDIF
!       IF UOM2'CTL # 0 OR UOM2'DUNITS = 0 THEN
!               TQTY = 0
!               BQTY = 0
!       ELSE
!       ENDIF
        UOM2'REC = UOM'REC
!       CALL SET'T'B'QTYS'2
RETURN

SET'T'B'QTYS:
       LIN'UOM'NO = 1
       CALL FIND'UOM
       TQTY = TQTY * (UOM'NUNITS/UOM'DUNITS)
       BQTY = BQTY * (UOM'NUNITS/UOM'DUNITS)
RETURN

!SET'T'B'QTYS'2:
!       TQTY = TQTY * (UOM2'NUNITS/UOM2'DUNITS)
!       BQTY = BQTY * (UOM2'NUNITS/UOM2'DUNITS)
!RETURN

FIND'PRH:                               !![40] - rewrote this
!!NOTE:  if LIN'TRK'NO # 0 causes probs put the test in PRE'VDSP instead!
        CALL ARRAY'PRH
        IF PRH'CTL # 0 THEN
                ENTRY = "Invalid part number line: "+LIN'NO+", part: "+LIN'TRK'NO
                CALL DO'ERR
                SUB'FLAG = 0
        ELSE
                SUB'FLAG = 2
                IF (PRH'ATH'NO1 = 0 OR PRH'ATH'NO2 # 0) AND LIN'TRK'NO # 0 THEN
                        ENTRY = "Invalid attributes on: "+PRH'TRK'NO+"-"+PRH'ATH'NO1+"-"+PRH'ATH'NO2+" aborting!"
                        CALL DO'ERRL
                        GOTO ABORT
                ENDIF
        ENDIF
        CALL FACTOR'SET
RETURN

ARRAY'PRH:                             !!uses LIN'TRK'NO for key
    \VUSEPRH
RETURN

MAP1 SAVSTX'LOC'NO,B,3  

GET'STX:
        IF STX'NO # STX'KNO OR SAVSTX'LOC'NO # STX'KLOC'NO THEN
            SAVSTX'LOC'NO = STX'KLOC'NO
            STX'CTL = 1
            XCALL VICSIT,7,106,STX'REC,0,STXPTR,5,STX'PKT,STX'KEY
            IF STX'CTL # 0 THEN
                STX'SPACE = SPACE
                USE STX INTO STX'REC KEY STX'KEY
                XCALL VICSIT,7,105,STX'REC,0,STXPTR,5,STX'PKT,STX'KEY
            ENDIF
            IF STX'CTL # 0 OR STX'NO # STX'KNO THEN
                STX'KLOC'NO = 0
                STX'SPACE = SPACE
                \VUSE STX
                STX'KLOC'NO = SAVSTX'LOC'NO
                XCALL VICSIT,7,105,STX'REC,0,STXPTR,5,STX'PKT,STX'KEY
                STX'KLOC'NO = 0
            ENDIF
        ELSE
            STX'CTL = 0
        ENDIF
        IF STX'CTL = 0 CALL COMPUTE'TTL'STX'PERC
RETURN

MAP1 TSTTAX,S,1

COPY "TTLSTX",CACHE="Y",TST'TAXABLE="TSTTAX" !!COMPUTE'TTL'STX'PERC:, GET'SF'TAXABLE:

!!COMMON EDIT ROUTINES AND MACROS

!![44]FIX'QTY'FRACTIONS:
!!      XQ = 10^(PRH'QFRACTION-XXX'QFRACTION)
!!      XP = 10^(PRH'PFRACTION-LIN'PFRACTION)
!!      \FROUND XXX'ORDER'QTY*XQ XXX'ORDER'QTY
!!      \FROUND XXX'COM'QTY*XQ XXX'COM'QTY
!!      \FROUND XXX'BO'QTY*XQ XXX'BO'QTY
!!      \FROUND XXX'USR'OQTY*XQ XXX'USR'OQTY
!!      \FROUND XXX'USR'CQTY*XQ XXX'USR'CQTY
!!      \FROUND XXX'USR'BQTY*XQ XXX'USR'BQTY
!![44]RETURN

MAP1 SAV'OVERIDE,B,1

E'ORDER'QTY:
    IF INX2 = FN'12 RETURN                                      !!KLUDGE
        IF INX2 = FN'Y THEN
                CALL SET'OVERIDE
                SAV'OVERIDE = LIN'OVERIDE
        ELSE
                SAV'OVERIDE = 0
        ENDIF
        CASE
        IF XXX'USR'OQTY = 0 THEN
                ENTRY = "MUST enter an order quantity"        !!(bdl)
                GOTO DO'ERR                             !!KLUDGE
        ELSE
        IF XXX'USR'OQTY < 0 THEN
                IF HDR'PROC = 5 THEN
                        ENTRY = "Credits not allowed on shop orders!"
                        GOTO DO'ERR                             !!KLUDGE
                ENDIF
        ENDCASE
!![30]  CALL MULTIPLY'TO'MY'ORDER
        XXX'ORDER'QTY = XXX'USR'OQTY    !![30]
        IF XXX'USR'OQTY = 0 RETURN                              !!KLUDGE
        IF CHG'QTY'KEY = 2 AND (INX1 = I'FUN OR LIN'OVERIDE = 1) THEN
                CHG'QTY'KEY = 0         !!Don't bother if function key exit
                                        !!Or an overide condition
        ENDIF
        IF CHG'QTY'KEY # 0 CALL ADJUST'PART             !!Snuff COM qty
        IF EFCHG # 0 OR INX1 = I'TAB THEN CHG'QTY'KEY = 1
        IF CHG'QTY'KEY # 0 THEN
                CALL ADJUST'PQTY                        !!Available, Etc.
                IF CHG'QTY'KEY = 2 AND XXX'ORDER'QTY > 0 THEN
                        IF PG = |NM| THEN
                                XXX'COM'QTY = XXX'COM'QTY MIN ZZZ|NM|'COM'QTY
                        ELSE
                                XXX'COM'QTY = XXX'COM'QTY MIN ZZZ3'COM'QTY
                        ENDIF
                        CALL SET'COM'BO
                ENDIF
        ELSE
                IF HDR'QUOTE'YN = "Y" THEN
                        CALL SET'QUOTE
                        IF PG = 3 CALL XN'GAFR3'C'B'QTY
                ENDIF
        ENDIF
        ON PG-1 CALL SAFR2,SAFR3
        IF CHG'QTY'KEY # 0 THEN
                IF IER1 # 0 AND XXX'USR'OQTY # 0 THEN CHG'QTY'KEY = 0
                ON PG-1 CALL DO'DDBEDT2,DO'DDBEDT3
        ENDIF
        IF XXX'USR'OQTY < 0 AND LIN'RPROC = 0 THEN
                IF HDR'DOC'TYPE # OEC'INT'CR THEN
                        CALL RPROC'PUSH
                        LIN'RPROC = XXX'RPROC
                ELSE
                        LIN'RPROC = 2
                        XXX'RPROC = 2
                ENDIF
        ENDIF
        IF INX1 = I'TAB OR XXX'USR'OQTY = 0 THEN IER1 = 0 !!Stay here on TAB
                                !!OR IF ADJUST'PART SET ORDER QTY TO ZERO
RETURN

RPROC'PUSH:
        IF PRH'INVENTORY = "N" THEN
                XXX'RPROC = 2
        ELSE
                W'TLR = 7
                W'BRR = 9
                W'TLC = 39
                W'BRC = 80
                W'ON = 6
                CALL DO'WINDOW'W2               !!IN HDRSUB.CPY
        ENDIF
!!      \WINDOW TLR="7" TLC="39" BRR="9" BRC="80"       &
!!              PUSH="Y" OVRLAY="RPROC" SAVSCR="SMALL"  &
!!                                              L25NUM="6" L25="Y"
RETURN

RPROC'SCR:
        DSP RPROC
        DDB RPROC
        GDB RPROC
RETURN

MAP1 CMN'PO1ADD,@CMN'OTHER
        LCL2 AUTO'PO,B,1

ASK'PO1ADD:
        DSP OE2WGA
        CALL GET'ADD'PO
        CALL TST'FUNC
        IF TST'FUNC # 0 OR INX1 = I'ESC THEN
                SNDAFR = "N"
        ELSE
                SNDAFR = ENTRY
        ENDIF
RETURN

ADJUST'PQTY:
!!Changed order quantity
        CASE
        IF HDR'QUOTE'YN = "Y" THEN
                CALL SET'QUOTE
                IF PG = 3 CALL XN'GAFR3'C'B'QTY
        ELSE
        IF XXX'ORDER'QTY >= 0 THEN
                IF LIN'HOLD'YN = "N" OR LIN'HOLD'YN = "O" THEN
                        CALL AVAILABLE
                        IF LIN'OVERIDE # 1 THEN
                                XXX'COM'QTY = XXX'ORDER'QTY MIN AVAILABLE
                        ELSE
                                XXX'COM'QTY = XXX'ORDER'QTY
                        ENDIF
                        SNDAFR = "E"
                        IF PG = 3 CALL SNDAFR'GAFR3'C'QTY
                        SNDAFR = OEC'BO'QTY'AFR
                        IF PG = 3 CALL SNDAFR'GAFR3'B'QTY
                ELSE
                        XXX'COM'QTY = 0
                        IF PG = 3 CALL XN'GAFR3'C'B'QTY
                ENDIF
        ELSE
                AVAILABLE = 0
                XXX'COM'QTY = XXX'ORDER'QTY
                IF PG = 3 CALL XN'GAFR3'C'B'QTY
        ENDCASE
        IF HDR'QUOTE'YN # "Y" THEN
                CALL SET'COM'BO
                CALL BO'CHECK
        ENDIF
        IF HDR'PROC < 5 THEN
                IF OEC'QTY'BREAKS # "N" THEN
                        CALL DO'PRICE
                ELSE
                        CALL DSP'PRICING
                ENDIF
        ENDIF
        IF XXX'ORDER'QTY <= 0 OR CHG'QTY'KEY = 2 RETURN         !!KLUDGE
        ENTRY = "N"
        CASE
        IF PRH'AUTO'PO = 4 THEN
                PRINT TAB(24,1) ; TAB(-1,A'CYAN) ; "Add P.O.?";TAB(-1,A'NORM); TAB(-1,9);
                \WINDOW PUSH="Y" L25NUM="4" L25="Y"                     &
                          ACTION="ASK'PO1ADD" TLR="22" NOCLR="Y" REPAINT="N"
                ENTRY = SNDAFR
        ELSE
        IF PRH'AUTO'PO = 2 OR (PRH'AUTO'PO = 3                          AND &
                                        XXX'COM'QTY # XXX'ORDER'QTY)    THEN
                ENTRY = "Y"
        ENDCASE
        IF ENTRY = "Y" THEN
                IF PG = 2 THEN
                        LIN2'REC = LIN'REC
                        LIN'XXX'OVR = XXX'REC
                        UPDATE LIN                      !!CANNOT CALL PRE'UPDATE
                ELSE
                        SUB2'REC = SUB'REC
                        SUB'XXX'OVR = XXX'REC
                ENDIF
                XXX2'REC = XXX'REC
                CMN'SPACE = SPACE
                CMN'TO'PRGNAM = "PO1ADD"
                CMN'FRM'PRGNAM = "OE1HDR"
                FOR II = 1 TO REAL'PG
                        CMN'SAV'RCN(II) = TMP'RCN(II)
                NEXT II
                AUTO'PO = PRH'AUTO'PO
                PRINT TAB(24,1) ; TAB(-1,A'RED) ; "Adding P.O. - Please Wait"; TAB(-1,9) ;
                \WINDOW PUSH="Y" APID="PO" PRGNAM="PO1ADD"              &
                                        COMMON="CMN'REC" NOCLR="Y" FULL="Y"
                IF PG = 2 THEN
                        LIN'REC = LIN2'REC
                        UPDATE LIN                      !!CANNOT CALL PRE'UPDATE
                ELSE
                        SUB'REC = SUB2'REC              !!DITTO
                ENDIF
                XXX'REC = XXX2'REC
        ENDIF
        PRINT TAB(24,1) ; TAB(-1,9) ;
RETURN

ADJUST'PART:
!!Change part number or location or UOM - zero quantities
    LIN'CAP'PCOL = 0
    CALL FACTOR'FIX
    IF PG = |NM| THEN
                IF CHG'QTY'KEY # 2 THEN
                        IF PRH'INVENTORY # "N" THEN
                                LIN'OVERIDE = 0 !!Clear overide
                                IF SAV'OVERIDE # 0 THEN
                                        LIN'OVERIDE = SAV'OVERIDE
                                        SAV'OVERIDE = 0
                                ENDIF
                        ELSE
                                LIN'OVERIDE = 1 !!Serial, non-inventory
                                                !!always overide
                        ENDIF
                        LIN'CALC'PRICE = 0
!![69]                  LIN'CALC'DISC = 0
                        LIN'PRICE = 0
!![69]                  LIN'DISC = 0
                        PRICE = 0
!![69]                  DISC = 0
                ENDIF
                CASE
                IF LIN'SUB'COUNT # 0 AND (LIN'TRK'NO # ZZZ|NM|'TRK'NO   OR &
                                        LIN'LOC'NO # ZZZ|NM|'LOC'NO) THEN
!!CHANGED PART OR LOCATION W/ SUB ITEMS - DELETE SUB ITEMS
                        CALL DELLOW'SUB
                ELSE
                IF SUB'FLAG = 0 OR LIN'SUB'COUNT = 0 THEN
!!IF NO SUB ITEMS ON NEW PART OR NO CURRENT SUB ITEMS
                        CALL ZERO'COM'SET'BO
                ENDCASE
    ENDIF
RETURN  

MAP1 HARD'LOC'NO,B,1

E'LOC'NO'LIN:
        \TABPASS
        IF INX2 = FN'7 THEN
                IF HARD'LOC'NO = 0 THEN
                        ENTRY = "Locking location number to "+STR(LIN'LOC'NO)
                ELSE
                        ENTRY = "Will set location to default of "+STR(HDR'SHP'LOC'NO)
                ENDIF
                IF HARD'LOC'NO <= 1 THEN
                    HARD'LOC'NO = 1 - HARD'LOC'NO
                ENDIF
                GOTO DO'ERR
        ELSE
        IF INX2 = FN'Z THEN
                CALL FIND'VIA
                CASE
                IF VIA'MTYPE = "U" AND VIA'MAN'YN = "A" OR &
                                VIA'MAN'YN = "L" OR VIA'MAN'YN = "1" OR &
                                VIA'MAN'YN = "X" THEN
                        ZON'DAYS = 1
                ELSE
                IF VIA'MTYPE = "U" AND VIA'MAN'YN = "S" OR &
                                VIA'MAN'YN = "E" OR VIA'MAN'YN = "2" OR &
                                VIA'MAN'YN = "Y" THEN
                        ZON'DAYS = 2
                ELSE
                IF VIA'MTYPE = "U" AND VIA'MAN'YN = "R" OR &
                                VIA'MAN'YN = "3" OR VIA'MAN'YN = "Z" THEN
                        ZON'DAYS = 3
                ELSE
                        ZON'KTBL'NO = VIA'TBL'NO(1)
                        ZON'KLOC'NO = LIN'LOC'NO
                        IF HDR'SHP'COU = "CA" THEN
!!                          CALL CA'ZONE
                            ZON'DAYS = 0
                        ELSE
                        IF HDR'SHP'COU = "US" OR HDR'SHP'COU = "USA" OR &
                                HDR'SHP'COU = "PR" OR HDR'SHP'COU = "VI" THEN
                            ZON'KSTART'ZIP = HDR'SHP'ZIP
                            USE ZON INTO ZON'REC KEY ZON'KEY
                            IF ZON'CTL # 0 THEN
                                GETPRV ZON INTO ZON'REC
                            ENDIF
                            \CHKKEY ZON APID="IN"
                            IF HDR'SHP'ZIP > ZON'END'ZIP THEN ZON'CTL = 1
                        ELSE
                            ZON'KSTART'ZIP = HDR'SHP'COU
                            USE ZON INTO ZON'REC KEY ZON'KEY
                        ENDIF
                        ENDIF
                        IF ZON'CTL # 0 THEN
                            ZON'DAYS = 0
                        ELSE
                            IF VIA'MTYPE = "U" AND VIA'MAN'YN = "C" THEN
                                IF ZON'DAYS = 0 THEN ZON'DAYS = ZON'ZONE
                            ELSE
                                ZON'DAYS = 0
                            ENDIF
                        ENDIF
                ENDCASE
                IF ZON'DAYS = 0 THEN
                        ENTRY = "Information not available on delivery!"
                ELSE
                        XP = ZON'DAYS
                        LOC'KNO = LIN'LOC'NO
                        CALL FIND'LOC
                        IF LOC'OC = "C" THEN
                                II = ZON'DAYS + 1
                        ELSE
                                II = ZON'DAYS
                        ENDIF
                        ZON'DAYS = 0
                        DTT'DOW = DATE
                        DO UNTIL II = 0
                                ZON'DAYS = ZON'DAYS + 1
                                DAY'OF'WEEK = DAY'OF'WEEK + 1           
                                DDY = DDY + 1
                                IF DAY'OF'WEEK > 6 THEN DAY'OF'WEEK = 0
                                IF DAY'OF'WEEK <= 4 AND (DMN # 12 OR DDY # 25) THEN
                                        II = II - 1
                                ENDIF
                        LOOP
                        \CNV'DTT DTT="DATE"
                        YDAYS = YDAYS + ZON'DAYS
                        QQ = YDAYS - (7 * INT(YDAYS/7)) + 1
                        IF QQ = 8 THEN QQ = 1
                        \UCNV'DTT
                        \STRDTT DDATE="ENTRY"
!                       ENTRY = "Delivery in " + STR(XP) + " days on " + &
!                               DAYS(QQ) + ", " + ENTRY + " (Zone "+ZON'ZONE+")."
                        ENTRY = "Delivery in " + STR(XP) + " days " + &
                                ", " + ENTRY + " (Zone "+ZON'ZONE+")."
                ENDIF
                GOTO DO'ERR
        ENDIF
        ENDIF
        IF EDSPSW = 0 AND (EFCHG = 1 OR TABPASS = 1) THEN CHG'QTY'KEY = 1
        TAB'LOC'NO = LIN'LOC'NO
        CALL E'LOC'NO   
        IF EDSPSW = 0 AND (SELECT(|NM|) = 1 OR SELECT(|NM|) = 2) AND LOC'ACTIVE'YN = "N" THEN
            ENTRY = "Location Inactive, choose another location!"
            CALL DO'ERR
        ENDIF
        LIN'LOC'NO = TAB'LOC'NO
RETURN

E'COM'QTY:
        IF EDSPSW = 0 THEN
                CALL TST'FUNC
                IF TST'FUNC # 1 THEN
                        XXX'COM'QTY = XXX'USR'CQTY
                        IF INX2 = FN'Y CALL SET'OVERIDE
                        IF INX1 = I'TAB THEN 
                            XXX'COM'QTY = AVAILABLE MIN XXX'ORDER'QTY
                            IER1 = 0
                            CALL SET'COM'BO
                            ON PG-1 CALL DO'DDBEDT2,DO'DDBEDT3
                        ENDIF
                        IF EFCHG # 0 THEN
                           IF XXX'USR'CQTY # 0 THEN
!!Changed ship/committed quantity
                                IF INX1 # I'UA THEN
                                    CASE
                                    IF XXX'COM'QTY > XXX'ORDER'QTY THEN
                                        ENTRY = "Exceeds Order Quantity of "+(XXX'ORDER'QTY/QFACTOR USING QMASK1)
                                        CALL DO'ERR
                                        IF XXX'ORDER'QTY > 0 THEN
                                            XXX'COM'QTY = AVAILABLE MIN XXX'ORDER'QTY
                                        ELSE
                                            XXX'COM'QTY = AVAILABLE MAX XXX'ORDER'QTY
                                        ENDIF
                                    ELSE
                                    IF XXX'COM'QTY > AVAILABLE THEN
                                        IF LIN'OVERIDE # 1 THEN
                                            ENTRY = "Exceeds available stock of "+(AVAILABLE/QFACTOR USING QMASK1)
                                            IF SPWASTR[8;1] = "Y" THEN
                                                ENTRY = ENTRY + ", FN'Y Overrides"
                                            ENDIF
                                            CALL DO'ERR
                                            IF XXX'ORDER'QTY > 0 THEN
                                                XXX'COM'QTY = AVAILABLE MIN XXX'ORDER'QTY
                                            ELSE
                                                XXX'COM'QTY = AVAILABLE MAX XXX'ORDER'QTY
                                            ENDIF
                                        ELSE
                                            IF PRH'INVENTORY # "N" THEN
                                                PRINT TAB(24,1) ; TAB(-1,A'RED) ; "OVERRIDING available qty of ";(AVAILABLE/QFACTOR USING QMASK1); TAB(-1,9) ;
                                            ENDIF
                                        ENDIF
                                    ENDCASE
                                ENDIF
                           ENDIF
                           CALL SET'COM'BO
                        ENDIF
                        IF INX1 # I'UA CALL BO'CHECK
                        IF EFCHG # 0 THEN
                                ON PG-1 CALL DO'DDBEDT2,DO'DDBEDT3
                        ENDIF
                ENDIF
        ENDIF
RETURN

LCL1 OCOL'OVR1                          !!COLORS for overide display
        MAP2 OCOL(2),B,1
LCL1 OCOL'OVR2,@OCOL'OVR1
        LCL2 OCOL1,B,1,|A'CYAN|
        LCL2 OCOL2,B,1,|A'RED|

LCL1 OFLAG'OVR1                         !!FLAGS for overide display
        MAP2 OFLAG(2),S,1
LCL1 OFLAG'OVR2,@OFLAG'OVR1
        LCL2 OFLAG1,S,1," "
        LCL2 OFLAG2,S,1,"*"

SET'OVERIDE:
        \DOPASS
        CASE
        IF LIN'OVERIDE = 0 AND SPWASTR[8;1] # "Y" THEN
                ENTRY = "You are not authorized to override!"
                CALL DO'ERR
        ELSE
                LIN'OVERIDE = 1 - LIN'OVERIDE
        ENDCASE
        IF LIN'OVERIDE = 1 THEN
                INX1 = I'RT
                EFCHG = 1
        ELSE
                INX1 = I'TAB
        ENDIF
        INX2 = 0
        PRINT TAB(24,1) ; TAB(-1,9) ;
RETURN

E'BO'QTY:
        IF EDSPSW = 0 THEN
                CALL TST'FUNC
                IF TST'FUNC # 1 AND INX1 # I'UA AND EFCHG # 0 THEN
!![30]                  CALL MULTIPLY'TO'MY'BO
                        XXX'BO'QTY = XXX'USR'BQTY       !![30]
                        IF EFCHG # 0 THEN
                                XXX'ORDER'QTY = XXX'BO'QTY + XXX'COM'QTY
                                XXX'USR'OQTY = XXX'USR'BQTY + XXX'USR'CQTY
                                ON PG-1 CALL DO'DDBEDT2,DO'DDBEDT3
                                CALL DSP'PRICING
                        ENDIF
                        IF XXX'USR'BQTY = 0 RETURN !!KLUDGE
                ENDIF
        ENDIF
RETURN

BO'CHECK:
        IF  (HDR'BO'YN = "N" OR PRH'ACTIVE # "Y") AND XXX'BO'QTY # 0 THEN
            IF ENVSTR[19;1] = "Y" AND PRH'ACTIVE # "O" THEN  !!bo'one'time
                CALL BO'ERROR
                XXX'ORDER'QTY = XXX'COM'QTY
                XXX'USR'OQTY = XXX'USR'CQTY
                XXX'BO'QTY = 0
                XXX'USR'BQTY = 0
                EFCHG = 1
            ENDIF
        ENDIF
RETURN

BO'ERROR:
        IF HDR'PROC < 5 THEN
                CASE
                IF HDR'BO'YN = "N" THEN
                        ENTRY = "Backorders NOT allowed for this order!"
                ELSE
                        ENTRY = "Backorders NOT allowed for this part!"
                ENDCASE
                CALL DO'ERR
        ENDIF
RETURN

SUB'FROM'LIN'QTY'REC:
        IF ZZZ2'LOC'NO # LIN'LOC'NO OR ZZZ2'UOM'NO # LIN'UOM'NO THEN
!!WE CHANGED LOCATION OR UOM OR KIT TYPE WHEN WE HAD SUB LINE ITEMS
!!WE MUST UNCOMMIT ALL SUB LINE ITEMS
                CALL ZERO'SUB'QTYS
        ENDIF
RETURN

ZERO'SUB'QTYS:
        LOOP'SUB'FLAG = 1
        PRINT TAB(24,1) ; TAB(-1,A'RED) "Zeroing committed quantity on sub line items";
        CALL LOOP'SUB
        PRINT TAB(24,1) ; TAB(-1,9) ;
RETURN

LOOP'SUB:
   IF LOOP'SUB'FLAG # 2 AND LOOP'SUB'FLAG # 4 THEN
        IF LOOP'SUB'FLAG <= 3 THEN
                ZZZXSAV'REC = ZZZX'REC
                LIN'XXX'OVR = XXX'REC
        ENDIF
        PG = 3
        FOR SUB'II = 1 TO 6
                IF LIN'TORDER'QTY(SUB'II) # 0 THEN
                        TMP'RCN(3) = SUB'II
                        CALL PRE'READ3
                        CALL LOOP'SUB'ACTION
                        CALL PRE'UPDATE3
                ENDIF
        NEXT SUB'II
        PG = 2
        IF LOOP'SUB'FLAG <= 3 THEN
                XXX'REC = LIN'XXX'OVR
                ZZZX'REC = ZZZXSAV'REC
        ENDIF
   ENDIF
RETURN

LOOP'SUB'ACTION:
        CASE
        IF LOOP'SUB'FLAG = 1 THEN
!!ZERO COMMITTED QTYS
                PRINT TAB(24,60) SUB'NO;
                CALL ZERO'COM'SET'BO
        ELSE
        IF LOOP'SUB'FLAG = 3 THEN
!!FILL BACKORDERS (CALLED FROM HDRSUB.CPY)
                IF LOCK'FLAG = 1 THEN PRINT TAB(24,60) LIN'NO ;
                PRINT TAB(24,70) "- ";SUB'NO;
                CALL AVAILABLE  
                XXX'COM'QTY = XXX'ORDER'QTY MIN AVAILABLE
                CALL SET'COM'BO
        ENDCASE
RETURN

UPDATE'QTY'REC:                 !![44] - rewrote routine
!![74]  IF HDR'QUOTE'YN = "Y" RETURN    !!KLUDGE
        IF (HDR'QUOTE'YN = "Y" AND ZZZ1'QUOTE'YN = "Y") RETURN  !!KLUDGE !![74]
        IF PG = 2 THEN
                IF LIN'SUB'COUNT # 0 AND                        &
                                (ZZZX'COM'QTY # 0 OR ZZZX'BO'QTY # 0) THEN
!!WE CAN ONLY BE HERE IF WE HAD SUB LINE ITEMS AND STILL DO
!!BECAUSE IF THE PART CHANGED DELLOW'SUB WOULD EXECUTE IN ADJUST'PART
                        GOTO SUB'FROM'LIN'QTY'REC       !!KLUDGE
!!WE DO NOT RECOMMIT - WE LEAVE THAT TO THE USER ...
                ENDIF
                SUB'SPACE = SPACE
                ZZZ3'REC = SUB'REC
        ENDIF
        IF PG # 3 RETURN                !![44] - KLUDGE
        IF ZZZ2'LOC'NO # LIN'LOC'NO OR ZZZ2'TRK'NO # LIN'TRK'NO           OR &
                                ZZZ2'UOM'NO  # LIN'UOM'NO                 OR &
                                ZZZ3'ATT'NO1 # SUB'ATT'NO1                OR &
                                ZZZ3'ATT'NO2 # SUB'ATT'NO2                OR &
                                ZZZX'BO'QTY  # XXX'BO'QTY                 OR &
                                ZZZX'COM'QTY # XXX'COM'QTY                THEN
!!SOMETHING THAT MATTERS CHANGED
!![74]          IF ZZZX'COM'QTY # 0 OR ZZZX'BO'QTY # 0 THEN
                IF (ZZZX'COM'QTY # 0 OR ZZZX'BO'QTY # 0)                AND &
                                               ZZZ1'QUOTE'YN # "Y" THEN !![74]
!!WE PREVIOUSLY HAD COMMITTED OR BO'D ITEMS TO UNCOMMIT
                        XXX2'REC = XXX'REC              !!SAVE IT
                        LIN2'REC = LIN'REC
                        SUB2'REC = SUB'REC
                        XXX'REC = ZZZX'REC              !!SET TO OLD
                        LIN'REC = ZZZ2'REC
                        SUB'REC = ZZZ3'REC
!![44]                  IF PG = 2 THEN
!!                              IF LIN'TRK'NO # 0 THEN
!!                                      CALL FIND'PRH
!!                              ELSE
!!                                      PRH'CTL = 3
!!                              ENDIF
!!                              IF PRH'CTL # 0 OR SUB'FLAG # 0 GOTO SKIP'OLD
!![44]                  ENDIF
                        CALL UNDO'OLD
                        IF QTY'CTL # 0 THEN
                                                !!NOTE: XXX'REC = ZZZX'REC
                                                !!CAUSE COULD NOT UNCOMMIT
                                RETURN
                        ENDIF
!!              SKIP'OLD:
                        XXX'REC = XXX2'REC      !!RESTORE TO CURRENT
                        LIN'REC = LIN2'REC
                        SUB'REC = SUB2'REC
                ENDIF
!![74]          IF XXX'COM'QTY # 0 OR XXX'BO'QTY # 0 THEN
                IF (XXX'COM'QTY # 0 OR XXX'BO'QTY # 0)                  AND &
                                                HDR'QUOTE'YN # "Y" THEN !![74]
!!WE CURRENTLY HAVE ITEMS TO COMMIT OR BO
!![44]                  IF PG = 2 THEN
!!                              IF LIN'TRK'NO # 0 THEN
!!                                      CALL FIND'PRH
!!                              ELSE
!!                                      PRH'CTL = 3
!!                              ENDIF
!!                              IF SUB'FLAG # 0 OR PRH'CTL # 0 GOTO SKIP'NEW
!![44]                  ENDIF
                        QTY'CTL = 0
                        TQTY = XXX'COM'QTY
                        BQTY = XXX'BO'QTY
                        IF XXX'KIT'BUILD # "Y" THEN
                                        !!KITS LOCK THEMSELVES IN MACRO
                            CALL LOCK'NEW'IF'NEEDED
                        ENDIF
                        IF TQTY = 0 AND BQTY = 0 THEN QTY'CTL = 3
                        IF QTY'CTL # 0 THEN
                                        !!DO NOT COMMIT ... COM'QTY NOW
                                        !!SET TO ZERO ...
                                RETURN  
                        ENDIF
                        CALL REDO'NEW
                ENDIF
        ENDIF
!!  SKIP'NEW:
        IF QTY'CTL = 0 AND QTY'RCN # 0 THEN
            REL QTY
            QLOCKED = 0
            QTY'CTL = 0
        ENDIF
RETURN

MAP1 QLOCKED,B,1                        !![44]
MAP1 VICQTY'SAV'TRK'NO,B,3              !![48]
MAP1 UNDO'OLD'QTY,B,5

UNDO'OLD:                               !![44] - re-writes
     CIF USE'KITS AND USE'KITS = "Y" THEN
        IF ZZZX'KIT'BUILD = "Y" THEN
!!THIS DOES ALL WRITING AS IT GOES
                CALL UNCOMMIT'KIT
                IF DOKITS'FLAG = 9 THEN
                        ENTRY = "BAD OLD KIT: "
                        \HDRERR APID="OE"
                        ENTRY = ENTRY+"/"+QTY'KLOC'NO+"-"+QTY'KTRK'NO+"-"+QTY'KATT'NO1+"-"+QTY'KATT'NO2+" line not updated"
                        CALL DO'ERRL
                        QTY'CTL = 3
                ELSE
                        QTY'CTL = 0
                ENDIF
        ENDIF
     ENDCIF
            QTY'LOC'NO = ZZZ2'LOC'NO
            QTY'TRK'NO = ZZZ2'TRK'NO
            QTY'ATT'NO1 = ZZZ3'ATT'NO1
!!          QTY'ATT'NO2 = ZZZ3'ATT'NO2
            \SETKEY QTY APID="IN" MODE="KEY"
            QLOCKED = 0
            QTY'CTL = 0
            QTY'RCN = 0
            IF LOOP'SUB'FLAG # 1 THEN
                CALL RDCOND'QTY
            ELSE
                CALL RDLOCK'QTY
            ENDIF
            QLOCKED = 1
            TQTY = ZZZX'COM'QTY
            BQTY = ZZZX'BO'QTY
            TUOM = ZZZ2'UOM'NO
            CALL FIND'UOM2
            IF TQTY = 0 AND BQTY = 0 THEN QTY'CTL = 3
            IF QTY'CTL # 0 THEN
                    IF QTY'CTL # 4 THEN
                            ENTRY = "BAD OLD QTY: "
                            \HDRERR APID="OE"
                            ENTRY = ENTRY+"/"+QTY'KLOC'NO+"-"+QTY'KTRK'NO+"-"+QTY'KATT'NO1+"-"+QTY'KATT'NO2+" line not updated"
                            CALL DO'ERRL
                    ELSE
                            ENTRY = "OLD QTY record locked: Item not updated"
                            CALL DO'ERR
                    ENDIF
                    IER1 = 1
            ELSE
                    SIGN = -1
                    CALL COMMIT'QTY
                    IF ZZZ2'LOC'NO # LIN2'LOC'NO                      OR &
                                    ZZZ2'TRK'NO # LIN2'TRK'NO         OR &
                                    HDR'QUOTE'YN = "Y"              THEN !![74]
!!Don't need to test attr as they cannot change in t-shirt interface
!!                                  ZZZ3'ATT'NO1 # SUB2'ATT'NO1       OR &
!!                                  ZZZ3'ATT'NO2 # SUB2'ATT'NO2       OR &
!!WE CHANGED PARTS SO JUST CALL WRITE'QTY REC
                            IF QLOCKED = 1 THEN
                                    QTY'NEW'STOCK = QTY'NEW'STOCK MIN QTY'BO'QTY MAX 0
                                    WRITE QTY
                                    CIF USE'CHI = "Y" THEN
                                        \CHI QTY
                                    ENDCIF
                                    QLOCKED = 0
                            ENDIF
                    ELSE
                        UNDO'OLD'QTY = XXX'COM'QTY
                    ENDIF
            ENDIF
!!CIF USE'KITS AND USE'KITS = "Y" THEN
!!  ENDIF
!!ENDCIF
RETURN

LOCK'NEW'IF'NEEDED:                     !![44] - rewrites
        IF (ZZZ2'LOC'NO # LIN'LOC'NO OR ZZZ2'TRK'NO # LIN'TRK'NO        OR &
                (PG = 3 AND (SUB'ATT'NO1 # ZZZ3'ATT'NO1                 OR &
                             SUB'ATT'NO2 # ZZZ3'ATT'NO2)) )                &
                                        OR                                 &
                        (ZZZX'COM'QTY = 0 AND ZZZX'BO'QTY = 0)          THEN
!!
!!WE NEED TO LOCK A NEW QTY REC BECAUSE OF PART CHANGE
!!                              OR
!!WE NEED TO LOCK THE NEW QTY REC BECAUSE IT IS THE SAME AS THE OLD
!!                                      BUT DUE TO ZERO QTY'S WAS NEVER LOCKED
!![44]          CALL LOCK'NEW'QTY
                QTY'LOC'NO = LIN'LOC'NO
                QTY'TRK'NO = LIN'TRK'NO
                QTY'ATT'NO1 = SUB'ATT'NO1
                QTY'ATT'NO2 = SUB'ATT'NO2
                \SETKEY QTY APID="IN" MODE="KEY"
           QLOCKED = 0
           QTY'CTL = 0
           QTY'RCN = 0
CIF V''VICQTY="Y" THEN
           IF INC'VICQTY'YN = "Y" CALL FIND'PRH
           IF INC'VICQTY'YN # "Y" OR PRH'RCN > INC'MAX'VICQTY'NO                OR &
                                QTY'KLOC'NO > INC'MAX'VICQTY'LOC        OR &
                                HDR'PROC >= 5 OR BQTY # 0               OR &
                                (TQTY < 0 AND HDR'DOC'TYPE # OEC'INT'CR) THEN
                QLOCKED = 1
                CALL RDLOCK'QTY
                IF QTY'CTL # 0 THEN
                        CALL ADD'QTY
                        IF QTY'CTL # 0 THEN
                                CALL ZERO'COM'SET'BO
                                ENTRY = "BAD NEW QTY: "
                                \HDRERR APID="OE"
                                ENTRY = ENTRY+"/"+QTY'KLOC'NO+"-"+QTY'KTRK'NO+"-"+QTY'KATT'NO1+"-"+QTY'KATT'NO2+" line not updated"
                                CALL DO'ERRL
                                IER1 = 1
                        ENDIF
                ENDIF
           ENDIF
CELSE
        QLOCKED = 1
        CALL RDLOCK'QTY
        IF QTY'CTL # 0 THEN
                CALL ADD'QTY
                IF QTY'CTL # 0 THEN
                        CALL ZERO'COM'SET'BO
                        ENTRY = "BAD NEW QTY: "
                        \HDRERR APID="OE"
                        ENTRY = ENTRY+"/"+QTY'KLOC'NO+"-"+QTY'KTRK'NO+"-"+QTY'KATT'NO1+"-"+QTY'KATT'NO2+" line not updated"
                        CALL DO'ERRL
                        IER1 = 1
                ENDIF
        ENDIF
!!      ENDIF                           11/19/93 HLB    Extra ENDIF
ENDCIF
        ENDIF
RETURN

REDO'NEW:
     CIF USE'KITS AND USE'KITS = "Y" THEN
        IF XXX'KIT'BUILD = "Y" THEN
                CALL COMMIT'KIT
                IF DOKITS'FLAG = 8 THEN
                        ENTRY = "BAD NEW KIT: "
                        \HDRERR APID="OE"
                        ENTRY = ENTRY+"/"+QTY'KLOC'NO+"-"+QTY'KTRK'NO+"-"+QTY'KATT'NO1+"-"+QTY'KATT'NO2+" line not updated properly"
                        CALL DO'ERRL
                        CALL ZERO'COM'SET'BO
                        QTY'CTL = 3
                ELSE
                        QTY'CTL = 0
                ENDIF
        ENDIF
     ENDCIF
            IF LIN'OVERIDE # 1 THEN
                IF XXX'COM'QTY > 0 THEN
!!                  IF BO'FLAG = 0 THEN
!!                      CALL AVAILABLE'COMPUTE
!!                  ELSE
!!                      IF XXX'COM'QTY > AVAILABLE CALL AVAILABLE'COMPUTE
                        CALL AVAILABLE'COMPUTE
!!                  ENDIF
                ENDIF
                IF XXX'COM'QTY > AVAILABLE THEN
                        ENTRY = "Cannot reserve all committed stock - adjusting."
                        CALL DO'ERR
                        XXX'COM'QTY = AVAILABLE
                        CALL SET'COM'BO
                        CALL BO'CHECK
                        SUB'ORDER'QTY = XXX'ORDER'QTY
                        SUB'COM'QTY = XXX'COM'QTY
                        SUB'BO'QTY = XXX'BO'QTY
                        SUB'USR'OQTY = XXX'ORDER'QTY
                        SUB'USR'CQTY = XXX'COM'QTY
                        SUB'USR'BQTY = XXX'BO'QTY
                        XXX'USR'OQTY = XXX'ORDER'QTY
                        XXX'USR'CQTY = XXX'COM'QTY
                        XXX'USR'BQTY = XXX'BO'QTY
                        LIN'TCOM'QTY(SUB'NO) = SUB'USR'CQTY
                        LIN'TORDER'QTY(SUB'NO) = SUB'USR'OQTY
                        CALL LOAD'WGA
                ENDIF                   
            ENDIF
            SIGN = 1
            TQTY = XXX'COM'QTY              !!MUST REDO AS AVAILABLE
            BQTY = XXX'BO'QTY               !!AND SET'COM'BO, ETC.
                                        !!AFFECT THESE NUMBERS  
                                        !!ESPECIALLY TQTY IN AVAILABLE
            CALL COMMIT'QTY
            IF QLOCKED = 1 THEN
                QTY'NEW'STOCK = QTY'NEW'STOCK MIN QTY'BO'QTY MAX 0
                WRITE QTY
                CIF USE'CHI = "Y" THEN
                    \CHI QTY
                ENDCIF
                QLOCKED = 0
            ENDIF
!!CIF USE'KITS AND USE'KITS = "Y" THEN
!!  ENDIF
!!ENDCIF
RETURN

RDLOCK'QTY:
        DO
                RDLOCK QTY INTO QTY'REC KEY QTY'KEY
                IF QTY'REC[2;1] # CHR(0) AND QTY'CTL = 0 THEN
                        REL QTY
                        ENTRY = "Waiting for a repost to finish!"
                        CALL DO'ERR
                        QTY'CTL = 4
                        \SLEEP 4
                ENDIF
        LOOP UNTIL QTY'CTL # 4
RETURN

RDCOND'QTY:
        LOCK'FLAG = 0
        DO
                RDCOND QTY INTO QTY'REC KEY QTY'KEY
                IF QTY'REC[2;1] # CHR(0) AND QTY'CTL = 0 THEN
                        REL QTY
                        ENTRY = "Waiting for a repost to finish!"
                        CALL DO'ERR
                        QTY'CTL = 4
                ENDIF
                IF QTY'CTL = 4 THEN
                        PRINT TAB(24,1);  "Quantity record locked ...";TAB(-1,9);CHR(7);
                        ? TAB(-1,14);TAB(24,48);"Press F10 to ABORT UPDATE: ";TAB(-1,A'NORM);
                        LOCK'FLAG = 1
                        EROW = 24 : ECOL = 78 : EMAX = 1 : ETYP = 16391
                        EMIN = 0
                        XCALL GIN,2,FNO,EDT
                        IF INX2 = FN'10 THEN
                                QTY'CTL = 44
                                INX1 = I'RT
                                INX2 = 0
                        ELSE
                                QTY'CTL = 4
                                \SLEEP 4
                        ENDIF
                ENDIF
        LOOP UNTIL QTY'CTL # 4
        IF QTY'CTL = 44 THEN QTY'CTL = 4
        IF LOCK'FLAG = 1 THEN PRINT TAB(24,1) ; TAB(-1,9) ;
RETURN

ADD'QTY:
        QTY'SPACE = SPACE
        QTY'LOC'NO = QTY'KLOC'NO
        QTY'TRK'NO = QTY'KTRK'NO
        QTY'ATT'NO1 = QTY'KATT'NO1
        QTY'ATT'NO2 = QTY'KATT'NO2
        ADD QTY FROM QTY'REC KEY QTY'KEY
        IF QTY'CTL = 0 THEN
                \SETKEY PRE="QTY" MODE="KY2" APID="IN"
                \ADDKEY PRE="QTY" MODE="KY2" APID="IN"
                RDLOCK QTY INTO QTY'REC KEY QTY'KEY
        ELSE
                CALL RDLOCK'QTY
        ENDIF
RETURN

COMMIT'QTY:                                     !![44] - rewrites
        OPR'FLAG = 0
        IF TQTY >= 0 THEN
                IF HDR'PROC < 5 THEN
                        QTY'COMMITTED = QTY'COMMITTED + (TQTY * SIGN)
                        OPR'FLAG = 1
                ELSE
                        QTY'RESERVED = QTY'RESERVED + (TQTY * SIGN)
                ENDIF
                QTY'BACKORDER = QTY'BACKORDER + (BQTY * SIGN)
        ELSE
                IF HDR'PROC < 5 THEN
                        IF HDR'DOC'TYPE = OEC'INT'CR THEN
                                QTY'COMMITTED = QTY'COMMITTED + (TQTY * SIGN)
                                OPR'FLAG = 1
                        ELSE
                                QTY'RAN = QTY'RAN - (TQTY * SIGN)
                        ENDIF
                ELSE
                        QTY'RESERVED = QTY'RESERVED - (TQTY * SIGN)
                ENDIF
        ENDIF
RETURN

SET'QUOTE:
        LIN'OVERIDE = 0
        XXX'COM'QTY = 0
        XXX'BO'QTY = 0
        XXX'USR'BQTY = 0
        XXX'USR'CQTY = 0
RETURN

SET'COM'BO:                             !!CALLED JUST AFTER CHANGING COM'QTY
        IF XXX'ORDER'QTY >= 0 THEN
            XXX'COM'QTY = XXX'COM'QTY MAX 0
        ELSE
            XXX'COM'QTY = XXX'ORDER'QTY
        ENDIF
!![30]  CALL DIVIDE'TO'USR'COM          !!SET NEW USER COM'QTY
        XXX'USR'CQTY = XXX'COM'QTY              !![30]

!SET'BO:
        IF HDR'QUOTE'YN = "Y" GOTO SET'QUOTE            !!KLUDGE
        XXX'BO'QTY = XXX'ORDER'QTY - XXX'COM'QTY        !!SET NEW "MY" BO

        XXX'USR'BQTY = XXX'USR'OQTY - XXX'USR'CQTY      !!SET NEW USER BO
        IF HDR'BO'YN # "N" THEN
            IF SUB'NO >= 1 AND SUB'NO <= 6 THEN         !![37]
                    IF INC'DZ'PIECES # "N" THEN
                        WGA'BDOZENS(SUB'NO) = FIX(XXX'USR'BQTY/12)
                        WGA'BPIECES(SUB'NO) = XXX'USR'BQTY - (WGA'BDOZENS(SUB'NO)*12)
                    ELSE
                        WGA'BPIECES(SUB'NO) = XXX'USR'BQTY
                    ENDIF
                    CALL SHOW'BO'S
            ENDIF
        ENDIF
RETURN

ZERO'COM'SET'BO:                        !!CALLED TO SNUFF COM QTY
        XXX'COM'QTY = 0                 !!WASTE IT
        CALL SET'COM'BO
RETURN

OTHERS'GAFR2:
        IF INC'MULTI'LOC = "N" THEN OEC'LOC'BY'LIN = "Y"
        IF HDR'PROC >= 5 THEN
                SNDAFR[1;1] = "E"
                SNDAFR[2;1] = "X"
                SNDAFR[3;1] = "X"
                SNDAFR[4;1] = "X"
        ELSE
                IF OEC'LOC'BY'LIN = "T" THEN            !![15]
                        SNDAFR[1;1] = "E"
                ELSE
                        SNDAFR[1;1] = OEC'LOC'BY'LIN
                ENDIF
        ENDIF
        IF LIN'HOLD'YN # "N" AND LIN'HOLD'YN # "O" THEN SNDAFR[5;1] = "Y" ELSE SNDAFR[5;1] = "E"
        IF XXX'ORDER'QTY >= 0 THEN
                SNDAFR[6;1] = OEC'PRI1'BY'LI
                SNDAFR[7;1] = OEC'PRI2'BY'LI
                SNDAFR[8;1] = OEC'PROM'BY'LI
!![31]above
        ELSE
                SNDAFR[6;1] = "Y"
                SNDAFR[7;1] = "Y"
                SNDAFR[8;1] = "Y"
        ENDIF           
!!      \GAFR OE1WGA SETUP="SAFR2" DOAFR="N" OTHERS="E" NEW="Y"         &
!!              AFR="SNDAFR[1;1]:LIN'LOC'NO+SNDAFR[6;1]:LIN'PRI'NO1+SNDAFR[7;1]:LIN'PRI'NO2+SNDAFR[8;1]:LIN'PROM'DTT+SNDAFR[4;1]:LIN'CMG'NO"
        IF HARD'LOC'NO = 2 THEN
            SNDAFR[1;1] = "Y"
        ENDIF
        \GAFR OE1WGA SETUP="SAFR2" DOAFR="N" OTHERS="E" NEW="Y"         &
                AFR="SNDAFR[1;1]:LIN'LOC'NO"
        V''GAFR'SENTRY = ENTRY
        ENTRY = SAFR2
        IF INC'DZ'PIECES # "N" THEN
                \GETFLD |SCREEN| WGA'DOZENS(1) V''TMP
                FOR II = 1 TO 6 
                        gafr(|V''TMP|+(II*2)-2) = WGA'AFR(II)
                        gafr(|V''TMP|+(II*2)-1) = WGA'AFR(II)
                NEXT II
        ELSE
                \GETFLD |ASCREEN| WGA'PIECES(1) V''TMP
                FOR II = 1 TO 6 
                        gafr(|V''TMP|+II-1) = WGA'AFR(II)
                NEXT II
        ENDIF
        SAFR2 = ENTRY
        ENTRY=V''GAFR'SENTRY
RETURN

XN'GAFR3'C'B'QTY:
        SNDAFR = "Y"
SNDAFR'GAFR3'C'QTY:
        IF LIN'HOLD'YN # "N" AND LIN'HOLD'YN # "O" THEN SNDAFR = "Y"
        \GAFR OE1WGS MODIFY="SAFR3" AFR="SNDAFR:WGS'DOZENS(2)+SNDAFR:WGS'PIECES(2)" NEW="Y"
        CALL XN'GAFR3'B'QTY
RETURN

XN'GAFR3'B'QTY:
        SNDAFR = "Y"
SNDAFR'GAFR3'B'QTY:
        \GAFR OE1WGS MODIFY="SAFR3" AFR="SNDAFR:WGS'DOZENS(3)+SNDAFR:WGS'PIECES(3)" NEW="Y"
RETURN

SAFR2:
        e = SAFR2
AFR:
        IF SHIPPING = 1 THEN
                FOR II = 1 TO 130
                        IF gafr(II) = "E" THEN gafr(II) = "T"
                NEXT II
                IF LIN'HOLD'YN = "N" OR LIN'HOLD'YN = "O" THEN
                        CALL AFR'MACRO
                        e = ""
                        gafr'flag = "*"
                        IF PG # 2 THEN
                                \GETFLD OE1WGS WGS'DOZENS(2) GLOBAL
                                gafr(|V''TMP|) = "E"
                                gafr(|V''TMP|+1) = "E"
                        ENDIF
                ENDIF
        ENDIF
        CALL AFR'MACRO
RETURN

AFR'MACRO:
        AFRMAPS'SKIP'TEST = 1                           !!EDIT = "N"
        CALL GAFR'DO'AFR
        \\GAFRDOAFR
RETURN

SAFR3:
        e = SAFR3
        CALL AFR
RETURN

!![30]\LWH              !!LENGTH, WIDTH, ETC. EDITS
                        !!CONTAINS:
                        !!              E'LENGTH:
                        !!              E'WIDTH:
                        !!              E'HEIGHT:
                        !!              EDIT'FRAC'WRK:
                        !!              DIVIDE'TO'USR'COM: 
                        !!              DIVIDE'TO'USR'ORDER:
                        !!              MULTIPLY'TO'MY'COM:
                        !!              MULTIPLY'TO'MY'BO:
                        !!              MULTIPLY'TO'MY'ORDER:
                        !!              COMPUTE'LWH:


MANIFEST'ON:
        ENTRY = "Manifest Mode"
        CALL DSP'SHIPPING
RETURN

!SHIPPING'ON:
!       SHIPPING = 1
!       ENTRY = "Shipping Mode" 
!       CALL DSP'SHIPPING
!RETURN

!SHIPPING'OFF:
!        WIN'F = 0
!        SHIPPING = 0
!        SELECT(2) = 0
!        SELECT(3) = 0
MANIFEST'OFF:
        XCALL OUT,"/HL(" + "2" +","+"63"+","+ "78"+")/"
RETURN

COPYF "BXLFIL",NOAFR

CHECK'SCHEMA|NM|:
                !!MODIFY:  Set SF = 1 to skip a schema during tabbing
                !!         and disallow it for use in this report.
        IF ARCHIVE'FLAG = "C" THEN
                IF SCH'PDBID # "OELIN" THEN SF = 1
        ELSE
                IF SCH'PDBID # "OELHS" THEN SF = 1
        ENDIF
RETURN

CHECK'HEADER|NM|:
                !!MODIFY:  Set SF = 1 to skip a header during tabbing
                !!         and disallow it for use in this report.
RETURN

SET'STARTKEY|NM|:
        CALL SET'KEY|NM|
        CALL SET'LOW'VALUES|NM|
        IF ARCHIVE'FLAG = "C" THEN
                V''STARTKEY = LIN'KEY
        ELSE
                V''STARTKEY = LHS'KEY
        ENDIF
RETURN

SET'ENDKEY|NM|:
        CALL SET'HIGH'VALUES|NM|
        CALL SET'KEY|NM|
        IF ARCHIVE'FLAG = "C" THEN
                V''ENDKEY = LIN'KEY
                \CHKCMP LIN ACTION="SET V''CMP'CONTROL"         !!Set company control
        ELSE
                V''ENDKEY = LHS'KEY
                \CHKCMP LHS ACTION="SET V''CMP'CONTROL"         !!Set company control
        ENDIF
!!MODIFY:  to set company control manually, comment above and set
!!         V''CMP'CONTROL = "Y" if company controlled, "N" otherwise.
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

CVT'PRICES:             !!Makes prices all piece price
    CALL GET'CASE'QQ
    IF LIN'UOM'FACTOR(1) # 0 THEN
        LIN'TPRICE(1) = LIN'TPRICE(1)/LIN'UOM'FACTOR(1)
        LIN'TCPRICE(1) = LIN'TCPRICE(1)/LIN'UOM'FACTOR(1)
    ENDIF
    IF DZ'QQ > 1 THEN
        LIN'TPRICE(DZ'QQ) = LIN'TPRICE(DZ'QQ)/LIN'UOM'FACTOR(DZ'QQ)
        LIN'TCPRICE(DZ'QQ) = LIN'TCPRICE(DZ'QQ)/LIN'UOM'FACTOR(DZ'QQ)
    ENDIF
    IF CASE'QQ > 1 THEN
        LIN'TPRICE(CASE'QQ) = LIN'TPRICE(CASE'QQ)/LIN'UOM'FACTOR(CASE'QQ)
        LIN'TCPRICE(CASE'QQ) = LIN'TCPRICE(CASE'QQ)/LIN'UOM'FACTOR(CASE'QQ)
    ENDIF
RETURN

UCVT'PRICES:            !!Takes piece price back to correct price
    CALL GET'CASE'QQ
    IF LIN'UOM'FACTOR(1) # 0 THEN
        LIN'TPRICE(1) = LIN'TPRICE(1)*LIN'UOM'FACTOR(1)         !![64]
        LIN'TCPRICE(1) = LIN'TCPRICE(1)*LIN'UOM'FACTOR(1)       !![64]
    ENDIF
    IF DZ'QQ > 1 THEN
        LIN'TPRICE(DZ'QQ) = LIN'TPRICE(DZ'QQ)*LIN'UOM'FACTOR(DZ'QQ)     !![64]
        LIN'TCPRICE(DZ'QQ) = LIN'TCPRICE(DZ'QQ)*LIN'UOM'FACTOR(DZ'QQ)   !![64]
    ENDIF
    IF CASE'QQ > 1 THEN
        LIN'TPRICE(CASE'QQ) = LIN'TPRICE(CASE'QQ)*LIN'UOM'FACTOR(CASE'QQ)
        LIN'TCPRICE(CASE'QQ) = LIN'TCPRICE(CASE'QQ)*LIN'UOM'FACTOR(CASE'QQ)
    ENDIF
RETURN

CHANGE'PRICE:
        \DOPASS
        IF PWASTR[8;1] = "Y" THEN
            CALL GET'CASE'QQ
CIF USE'CAP = "Y" THEN
    SYMBOL CAPKLUDGE="INC'TS'SUBI=4 OR "
    IF |CAPKLUDGE| INC'DZ'PIECES = "N" THEN
CELSE
    SYMBOL CAPKLUDGE=""
ENDCIF
            IF INC'DZ'PIECES = "N" THEN
                  IF DZ'QQ > 0 THEN
                        LIN'TPRICE(DZ'QQ) = LIN'TPRICE(DZ'QQ)/LIN'UOM'FACTOR(DZ'QQ)     !![64]
                  ENDIF
                  IF CASE'QQ > 0 THEN
                        LIN'TPRICE(CASE'QQ) = LIN'TPRICE(CASE'QQ)/12
                  ENDIF
            ENDIF
CIF USE'CAP = "Y" THEN
                IF INC'TS'SUBI = 4 THEN
                    \WINDOW VUEMLT="|NM|" ACTION="DO'OE3WGA" &
                                REPAINT="Y"  RESTORE="Y" AUTO="Y" &
                                SAVSCR=SAVLIN L25NUM="2" L25="Y" &
                                OVRLAY="OE5WGA"
                    CALL DSP'PRICING
                ELSE
ENDCIF
                    \WINDOW VUEMLT="|NM|" ACTION="DO'OE3WGA" NOCLR="Y"      &
                                REPAINT="N"  RESTORE="Y" TLR="10" BRR="10"      &
                                SAVSCR=SAVLIN L25NUM="2" L25="Y"
CIF USE'CAP = "Y" THEN
                ENDIF
ENDCIF
!![13] - log change in prices.
CIF USE'CAP = "Y" THEN
    SYMBOL CAPKLUDGE="INC'TS'SUBI=4 OR "
    IF |CAPKLUDGE| INC'DZ'PIECES = "N" THEN
CELSE
    SYMBOL CAPKLUDGE=""
ENDCIF
        IF INC'DZ'PIECES = "N" THEN
            IF DZ'QQ > 0 THEN
                LIN'TPRICE(DZ'QQ) = LIN'TPRICE(DZ'QQ)*LIN'UOM'FACTOR(DZ'QQ) !![64]
            ENDIF
            IF CASE'QQ > 0 THEN
                LIN'TPRICE(CASE'QQ) = LIN'TPRICE(CASE'QQ)*12
            ENDIF
        ENDIF
CIF USE'CAP = "Y" THEN
        IF INC'TS'SUBI = 4 THEN
            CALL DSP'PRICING
            CALL PRE'DSP'WGP'PRICES
        ENDIF
ENDCIF
        FOR II = 1 TO 3
            IF LIN'TPRICE(II) # LIN'TCPRICE(II) AND &  !![36]
                OEC'MSG'LOG = "B" OR OEC'MSG'LOG = "P" THEN !![36]
                ENTRY = "Changed price on: "
                \KERROR LIN MODE="D" CNO="N"
                XCALL VICSIT,26,ENTRY,"/logs/oe1wga.DAILY"
                II = 7
            ENDIF
        NEXT II
    ELSE
        ENTRY = "You are not authorized to change prices."
        CALL DO'ERR
    ENDIF
RETURN

DO'OE3WGA:                                              !!FIX FOR CAPS
CIF USE'CAP = "Y" THEN
    IF INC'TS'SUBI = 4 THEN
        DSP OE5WGA
        LIN'TPRICE(1) = LIN'TPRICE(1)/(OEC'COLQ(1)*12)
        LIN'TPRICE(3) = LIN'TPRICE(3)/OEC'COLQ(3)
        DDB OE5WGA
        LIN'TPRICE(1) = LIN'TPRICE(1)*(OEC'COLQ(1)*12)
        LIN'TPRICE(3) = LIN'TPRICE(3)*OEC'COLQ(3)
    ELSE
ENDCIF
        DSP OE3WGA
CIF USE'CAP = "Y" THEN
    ENDIF
ENDCIF
DO'OE3WGA'2:
    ECFLD = 0
    CALL GET'CASE'QQ
    ENTRY = "*EEE"
    IF INC'TS'SUBI # 4 THEN
        FOR II = 1 TO 3
            IF LIN'UOM'FACTOR(II) = 0 THEN
                gafr(II) = "Y"
            ENDIF
        NEXT II
        IF ENTRY = "*YYY" THEN
            CALL DO'PRICE
            ENTRY = "*EEE"
            FOR II = 1 TO 3
                IF LIN'UOM'FACTOR(II) = 0 THEN
                    gafr(II) = "Y"
                ENDIF
            NEXT II
            IF ENTRY = "*YYY" THEN
                ENTRY = "NO PRICING RECORDS FOUND??"
                GOTO DO'ERR             !!KLUDGE
            ENDIF
        ENDIF
        \AFR EDIT="N"
    ENDIF
CIF USE'CAP = "Y" THEN
    IF INC'TS'SUBI = 4 THEN
        LIN'TPRICE(1) = LIN'TPRICE(1)/(OEC'COLQ(1)*12)
        LIN'TPRICE(3) = LIN'TPRICE(3)/OEC'COLQ(3)
        GDB OE5WGA
        LIN'TPRICE(1) = LIN'TPRICE(1)*(OEC'COLQ(1)*12)
        LIN'TPRICE(3) = LIN'TPRICE(3)*OEC'COLQ(3)
    ELSE
ENDCIF
        IF CASE'QQ > 0 THEN
            LIN'TPRICE(CASE'QQ) = LIN'TPRICE(CASE'QQ)/LIN'UOM'FACTOR(CASE'QQ)*12 !!Show / Dozen
        ENDIF
        GDB OE3WGA
        IF CASE'QQ > 0 THEN
            LIN'TPRICE(CASE'QQ) = LIN'TPRICE(CASE'QQ)*LIN'UOM'FACTOR(CASE'QQ)/12 !!Store / Case
        ENDIF
CIF USE'CAP = "Y" THEN
    ENDIF
ENDCIF
RETURN

E'WGP'PRICE:
        \TSTF
        CALL GET'CASE'QQ
        CASE
        IF INX1 = I'TAB THEN
            CALL DO'PRICE
            IF INC'DZ'PIECES = "N" THEN
                IF DZ'QQ # 0 THEN
                    LIN'TPRICE(DZ'QQ) = LIN'TPRICE(DZ'QQ)/LIN'UOM'FACTOR(DZ'QQ) !![64]
                ENDIF
                IF CASE'QQ # 0 THEN
                    LIN'TPRICE(CASE'QQ) = LIN'TPRICE(CASE'QQ)/LIN'UOM'FACTOR(CASE'QQ) !!Show / each
                ENDIF
            ELSE
                IF CASE'QQ # 0 THEN
                    LIN'TPRICE(CASE'QQ) = LIN'TPRICE(CASE'QQ)/LIN'UOM'FACTOR(CASE'QQ)*12 !![43]
                ENDIF
                IF OEC'FORCE'DOZ = "Y" AND DZ'QQ > 0 THEN
                    LIN'TPRICE(DZ'QQ) = (LIN'TPRICE(DZ'QQ)/LIN'UOM'FACTOR(DZ'QQ))*12
                ENDIF
            ENDIF
            IER1 = 0
        ELSE
        IF EFCHG # 0 THEN
CIF USE'CAP = "Y" THEN
    SYMBOL CAPKLUDGE="INC'TS'SUBI=4 OR "
    IF |CAPKLUDGE| INC'DZ'PIECES = "N" THEN
CELSE
    SYMBOL CAPKLUDGE=""
ENDCIF
            IF INC'DZ'PIECES = "N" THEN
                IF DZ'QQ # 0 THEN
                    LIN'TPRICE(DZ'QQ) = LIN'TPRICE(DZ'QQ)*LIN'UOM'FACTOR(DZ'QQ)     !![64]
                ENDIF
                IF CASE'QQ # 0 THEN
                    LIN'TPRICE(CASE'QQ) = LIN'TPRICE(CASE'QQ)*LIN'UOM'FACTOR(CASE'QQ) !![27]
                ENDIF
            ELSE
!![88] - add checks below on even piece price.
                IF CASE'QQ # 0 THEN
                    IF LIN'TPRICE(CASE'QQ)/12 # INT(LIN'TPRICE(CASE'QQ)/12) THEN
                        ENTRY = "Case price not an even piece price!"
                        CALL DO'ERR
                    ENDIF
                    LIN'TPRICE(CASE'QQ) = LIN'TPRICE(CASE'QQ)*LIN'UOM'FACTOR(CASE'QQ)/12 !![43]
                ENDIF
                IF DZ'QQ # 0 THEN
                    IF LIN'TPRICE(DZ'QQ)/LIN'UOM'FACTOR(DZ'QQ) #  &
                           INT(LIN'TPRICE(DZ'QQ)/LIN'UOM'FACTOR(DZ'QQ)) THEN
                        ENTRY = "Dozen price not an even piece price!"
                        CALL DO'ERR
                    ENDIF
                ENDIF
            ENDIF
                IF INC'TS'SUBI # 4 CALL DSP'PRICING
CIF USE'CAP = "Y" THEN
    SYMBOL CAPKLUDGE="INC'TS'SUBI=4 OR "
    IF |CAPKLUDGE| INC'DZ'PIECES = "N" THEN
CELSE
    SYMBOL CAPKLUDGE=""
ENDCIF
            IF INC'DZ'PIECES = "N" THEN
                IF DZ'QQ # 0 THEN
                    LIN'TPRICE(DZ'QQ) = LIN'TPRICE(DZ'QQ)/LIN'UOM'FACTOR(DZ'QQ)     !![64]
                ENDIF
                IF CASE'QQ # 0 THEN
                    LIN'TPRICE(CASE'QQ) = LIN'TPRICE(CASE'QQ)/LIN'UOM'FACTOR(CASE'QQ) !![27]
                ENDIF
            ELSE
                IF CASE'QQ # 0 THEN
                    LIN'TPRICE(CASE'QQ) = LIN'TPRICE(CASE'QQ)/LIN'UOM'FACTOR(CASE'QQ)*12 !![43]
                ENDIF
            ENDIF
        ENDCASE
RETURN

    COPY    "WGACPY"

MAP1 HYP(6),S,1
MAP1 ORG'LOC'NO,B,2
MAP1 BOTTOM'AFU,B,1
MAP1 DPS'REC
        MAP2 SDOZENS(6),B,5
        MAP3 SPIECES(6),B,5

MAP1 NOTAVL(6),B,1

PRE'DSP'WGA'AVAIL:
   IF INC'DSPAVL = "N" RETURN           !!KLUDGE
   LOCK'FLAG = 0
   FOR II = 1 TO 6
        IF WGA'TITLES(II) = "" THEN
                DZPC(II) = SPACE(6)
        ELSE
                IF INC'DZ'PIECES # "N" THEN
                        DZPC(II) = (ABS(DOZENS(II)) USING "###") + "/" + ABS(PIECES(II)) + SPACE(2)
                ELSE
                        DZPC(II) = PIECES(II) USING "######"
                ENDIF
                IF DOZENS(II) # 0 OR PIECES(II) # 0 THEN
                        LOCK'FLAG = 1                   !!We have some inventory
                ELSE
                IF NOTAVL(II) = 1 THEN
                    DZPC(II) = "N/A"
                ENDIF
                ENDIF
        ENDIF
    CIF USE'KITS AND USE'KITS = "Y" THEN
        IF PQTY(II) # 0 AND XXX'KIT'BUILD = "Y" THEN
            IF WGA'TITLES(II)[-1,-1] # "*" THEN
                WGA'TITLES(II) = WGA'TITLES(II)[1,5] + "*"
            ENDIF
        ENDIF
    ENDCIF
            IF WGA'TITLES(II)[-1,-1] = "*" THEN
                WGA'TITLES(II) = WGA'TITLES(II)[1,-2]
            ENDIF
!!  CIF USE'KITS AND USE'KITS = "Y" THEN
!!      ENDIF
!!  ENDCIF
   NEXT II
   CALL PRE'DSP'WGA'TITLES
   IF LOCK'FLAG = 0 THEN
       FOR II = 1 TO 6
           IF NOTAVL(II) # 0 THEN
               LOCK'FLAG = 2                  !!No inventory, and not available
           ENDIF
       NEXT II
   ENDIF
   IF ENVSTR[6;2] = "N" AND ENVSTR[8;2] = "N" THEN !oe1wga'dsploca = "N"
        CALL DSP'WGA'AVAIL                         !oe1wga'dspcola = "N"
        IF LOCK'FLAG = 2 THEN
            PRINT TAB(9,37) ; TAB(-1,A'RED) ; SPACE(40) ; TAB(9,37) ;
            ? "Not available in this Location." ;
        ENDIF
        RETURN                                                  !!KLUDGE
   ENDIF
   IF ENVSTR[8;2] = "N" THEN            !!oe1wga'dspcola = "N"
       DSP'LIN'NO = LIN'LOC'NO
       LOC'KNO = LIN'LOC'NO
       CALL FIND'LOC
       PRH'FORMATTED = LOC'DESC
       IF LOC'OC = "C" THEN
            PRH'FORMATTED = PRH'FORMATTED + " *Closed"
       ENDIF
   ELSE
       DSP'LIN'NO = 1
       CALL LOAD'TRK'NOS
       LIN'TRK'NO = ORG'TRK'NO
       CALL FIND'PRH
!      CALL DO'DSPPRH
       ATTD'KATH'NO = ROT'ATH'NO(2)
       ATTD'KCODE = PRH'PART[VAL(ENVSTR[8;2])+1,-1]    !oe1wga'dspcola
       \STRIP ATTD'KCODE
       \VUSE ATTD VREC="ATT'REC"
       IF ATTD'CTL = 0 THEN
           PRH'FORMATTED = ATT'DESC
       ELSE
           PRH'FORMATTED = PRH'DESC
       ENDIF
   ENDIF
   SDSP'NET = "N/A"
   FOR II = 1 TO |MAX'ALOC|
       IF ALOC'NO(II) = LIN'LOC'NO THEN
           SDSP'NET = ALOC'ZONE(II)
           II = |MAX'ALOC| + 1
       ENDIF
   NEXT II
   SDSP'NET = RIGHT(SPACE(9)+SDSP'NET,9)
   CALL DSP'BOTTOMS'SETUP
   IF ENVSTR[6;2] = "Y" OR ENVSTR[8;2] # "N" THEN !oe1wga'dsploca = "Y"
        JJ = 12                                   !oe1wga'dspcola # "N"
   ELSE
        JJ = VAL(ENVSTR[6;2])                     !oe1wga'dsploca
   ENDIF
   JJ = JJ - 1
   IF DZPC(1) # "" THEN
       PRH'FORMATTED = PRH'FORMATTED + SPACE(17)
       PRH'FORMATTED = PRH'FORMATTED[1;17] + " " + &
                                             BO(1) + (DZPC(1) USING "######")
   ENDIF
   CALL |VDSP|
   IF LOCK'FLAG = 2 THEN
          PRINT TAB(VDSP'LIN,26) ; SPACE(43) ; &
                TAB(VDSP'LIN,26) ; TAB(-1,A'RED) ;
          IF ENVSTR[6;2] # "N" THEN                !oe1wga'dsploca
                ? "Not available in this Color.   " ; TAB(-1,A'CYAN) ;
          ELSE
                ? "Not available in this Location." ; TAB(-1,A'CYAN) ;
          ENDIF
   ENDIF
   DPS'REC = DP'REC                     !!SAVES DOZENS(II) & PIECES(II)
   IF ENVSTR[8;2] = "N" THEN                       !oe1wga'dspcola
     FOR QQ = 1 TO |MAX'ALOC|
        IF ALOC'NO(QQ) # 0 THEN
           IF ALOC'NO(QQ) # LIN'LOC'NO THEN
                ORG'LOC'NO = LIN'LOC'NO
                LIN'LOC'NO = ALOC'NO(QQ)
                IF PRH'INVENTORY # "N" THEN
                    CALL DSP'BOTTOMS
                ENDIF
                TQTY = 0
                LIN'LOC'NO = ORG'LOC'NO
           ENDIF
        ELSE
                QQ = |MAX'ALOC| + 1
        ENDIF
     NEXT QQ
     LOC'KNO = LIN'LOC'NO
     CALL FIND'LOC
   ELSE
        FOR QQ = 2 TO 12
            LIN'TRK'NO = DSP'TRK'NO((DSP'TRK'PAGE*12)+QQ)
            IF LIN'TRK'NO # 0 THEN
                CALL DSP'BOTTOMS
            ENDIF
        NEXT QQ
        LIN'TRK'NO = ORG'TRK'NO
        CALL FIND'PRH
        CALL DO'DSPPRH
   ENDIF
   XLN = 0
   DP'REC = DPS'REC                     !!RESTORE
RETURN

MAP1 DSP'WGP'PRICES
    MAP2 DSP'WGP'SPACE,S
        MAP3 DSP'WGP'PRICE(3),S,8
MAP1 SAVE'PRICE,B,5
PRE'DSP'WGP'PRICES:            !![WC] using vdsp from screen OE1WGA
    DSP'WGP'SPACE = SPACE(24)
    TYD'SPACE = SPACE
    CALL GET'TYD
    \FROUND TYD'AMT/TYD'UOM'NO SAVE'PRICE USING "#,###.##"
    IF LIN'TPRICE(1) => SAVE'PRICE AND HDR'QUOTE'YN # "Y" AND TYD'UOM'NO # 0 THEN
        LIN'TPRICE(1) = (TYD'AMT/TYD'UOM'NO)
        LIN'TPRICE(2) = (LIN'TPRICE(1)*LIN'UOM'FACTOR(2))
        LIN'TPRICE(3) = TYD'AMT
        HPRICE'NOTE = "Prices from Tier or On Sale"
    ENDIF
    IF LIN'UOM'FACTOR(1) # 0 THEN
        IF PRH'QTD'COUNT = 0 THEN
            DSP'WGP'PRICE(1) = LIN'TPRICE(1)/100 USING "#,###.##"
        ELSE
            ENTRY = LIN'TPRICE(1)/10^PRH'PFRACTION  USING "$###.###"
            DSP'WGP'PRICE(1) = ENTRY
        ENDIF
    ENDIF
    CALL GET'CASE'QQ

!![WC] Ignore  ------------------------------------------------
CIF USE'CAP = "Y" THEN
    IF INC'TS'SUBI = 4 THEN
        UOM'TITLES(1) = (LIN'CAP'PCEPRC/100 USING "##.##")
        UOM'TITLES(2) = (LIN'TPRICE(1)/LIN'UOM'FACTOR(1)/100 USING "##.##")
        ENTRY = "Pieces: "+UOM'TITLES(1)+" Dozens: "+&
                                        (OEC'COLQ(1) USING "##")+": "+UOM'TITLES(2)
        IF REAL'PG = |NM| THEN
            PRINT TAB(9,3) TAB(-1,A'WHT) ; ENTRY;
        ENDIF
        UOM'TITLES(1) = (LIN'TPRICE(2)/LIN'UOM'FACTOR(2)/100 USING "##.##")
        UOM'TITLES(2) = (LIN'TPRICE(3)/LIN'UOM'FACTOR(3)/100 USING "##.##")
        UOM'TITLES(3) = (LIN'CAP'HPRICE/100 USING "##.##")
        ENTRY = (OEC'COLQ(2) USING "##")+": "+UOM'TITLES(1)+" "+&
                                (OEC'COLQ(3) USING "###")+": "+ &
        UOM'TITLES(2)+" "+(OEC'COLQ(4) USING "###")+": "+UOM'TITLES(3)
        IF REAL'PG = |NM| THEN
            PRINT TAB(10,3) TAB(-1,A'WHT) ; ENTRY;
        ENDIF
        RETURN                                  !!KLUDGE KLUDGE
    ELSE
ENDCIF

!![WC]  ----------------------------------------------------- Ignore above

        IF DZ'QQ # 0 THEN
            IF INC'DZ'PIECES # "N" THEN
                IF OEC'FORCE'DOZ # "Y" THEN             !![90]
                    DSP'WGP'PRICE(DZ'QQ) = LIN'TPRICE(DZ'QQ)/100 USING "#,###.##"
                ELSE                                    !![90]
                    DSP'WGP'PRICE(DZ'QQ) = LIN'TPRICE(DZ'QQ)/LIN'UOM'FACTOR(DZ'QQ)*12/100 USING "#,###.##" !![90]
                ENDIF                                   !![90]
            ELSE
                DSP'WGP'PRICE(DZ'QQ) = LIN'TPRICE(DZ'QQ)/LIN'UOM'FACTOR(DZ'QQ)/100 USING "#,###.##" !![64]
            ENDIF
        ENDIF

        IF CASE'QQ # 0 THEN
            IF INC'DZ'PIECES # "N" THEN
                DSP'WGP'PRICE(CASE'QQ) = LIN'TPRICE(CASE'QQ)/LIN'UOM'FACTOR(CASE'QQ)*12/100 USING "#,###.##"
            ELSE
                DSP'WGP'PRICE(CASE'QQ) = LIN'TPRICE(CASE'QQ)/LIN'UOM'FACTOR(CASE'QQ)/100 USING "#,###.##"
            ENDIF
        ENDIF
CIF USE'CAP = "Y" THEN
    ENDIF
ENDCIF
    CALL DSP'WGP'PRICES
RETURN

GET'QBK'PRICES:
        QBK'TAG = "Qbk:"

        PRC'KTRK'NO = PRH'TRK'NO
        PRC'KUOM'NO = 1
        PRC'KLOC'NO = 0
        USE PRC INTO PRC'REC KEY PRC'KEY
        IF PRC'CTL = 0 THEN
            ENTRY = PRC'AMT/10^PRH'PFRACTION  USING "$###.###"
        ELSE
            ENTRY = "BAD PRC"
        ENDIF
        DSP'WGP'PRICE(1) = ENTRY
        QQ = 1
!![WC] Set first level QBK pricing used base price for each
        QBK'TITLES(1) = RIGHT(SPACE(4)+"1:",4)+ENTRY

        QTD'SPACE = SPACE

        QTD'LOC'NO = PRC'LOC'NO
        QTD'TRK'NO = PRH'TRK'NO
        QTD'UOM'NO = 1
        QTD'QTY    = 2

        \SETKEY PRE="QTD" MODE="KEY" APID="IN"

        USE QTD INTO QTD'REC KEY QTD'KEY
        DO
            QQ = QQ + 1

            GETNXT QTD
            \CHKKEY QTD APID="IN"
            IF QTD'CTL # 0 THEN EXIT
            ENTRY = QTD'AMT/10^PRH'PFRACTION  USING "$###.###"

            IF QQ <= PRC'QTD'COUNT+1   THEN
                ENTRY = QTD'AMT/10^PRH'PFRACTION  USING "$###.###"
                QBK'TITLES(QQ) = RIGHT(SPACE(4)+QTD'QTY+":",4)+ENTRY
            ELSE
                EXIT
            ENDIF
        LOOP

RETURN

SET'QBK'PRC'BY'OQTY:
    IF PRC'TRK'NO # PRH'TRK'NO THEN
        PRC'KTRK'NO = PRH'TRK'NO
        PRC'KUOM'NO = 1
        PRC'KLOC'NO = 0
        USE PRC INTO PRC'REC KEY PRC'KEY
    ENDIF
    IF ARCHIVE'FLAG = "C" THEN
        IF QQ < 3 THEN                         !!Clean up rest of WGP
            FOR JJ = QQ+1 TO 3
                    LIN'TPRICE(JJ) = 0             !Calc Prices By UOM
                LIN'TCPRICE(JJ) = 0             !Calc Prices By UOM
            NEXT JJ
        ENDIF

        DSP'WGP'SPACE = SPACE(24)

        IF II >= PRC'MIN'QTY THEN
            QTD'LOC'NO = 0
            QTD'TRK'NO = PRH'TRK'NO
            QTD'UOM'NO = 1
            QTD'QTY = 99999999            !! high value for Get Prv
            \SETKEY PRE="QTD" MODE="KEY" APID="IN"

            USE QTD INTO QTD'REC KEY QTD'KEY

            DO
                IF II < QTD'QTY THEN
                    GETPRV QTD
                ENDIF
                IF II >= QTD'QTY THEN EXIT
            LOOP

            \CHKKEY QTD APID="IN"

            IF QTD'CTL = 0 THEN
                !![WC] Set new each price based on quantity break
                LIN'TCPRICE(QQ) = QTD'AMT
                LIN'TPRICE(QQ) = QTD'AMT
                UOM'TITLES(QQ) = "Qbk"+(QTD'QTY USING "##Z")
                ENTRY = LIN'TCPRICE(QQ)/10^PRH'PFRACTION  USING "$###.###"
                DSP'WGP'PRICE(1) = ENTRY
            ENDIF
        ELSE
            LIN'TCPRICE(QQ) = PRC'AMT
            LIN'TPRICE(QQ) = PRC'AMT
            UOM'TITLES(QQ) = "Qbk"+(1 USING "##Z")
            ENTRY = LIN'TCPRICE(QQ)/10^PRH'PFRACTION  USING "$###.###"
            DSP'WGP'PRICE(QQ) = ENTRY
        ENDIF
    ELSE
        UOM'TITLES(QQ) = "ShpPrc"
        ENTRY = LIN'TCPRICE(QQ)/10^PRH'PFRACTION  USING "$###.###"
        DSP'WGP'PRICE(QQ) = ENTRY
    ENDIF
    CALL DSP'UOM'TITLES
    CALL DSP'WGP'PRICES
RETURN

\COPYF PRH IN PRH5 MODE="PRI"
\COPYF PRH IN PRH2 MODE="PRI"
!!\COPYF PRH IN PRH3 MODE="DTA"
\SIZEOF PRH IN
MAP1 PRH3'REC,X,|V''TMP|
MAP1 PRH3'RCN,B,4                                       !![52]

DDB'ODD'SIZES:
        IF PRH2'TRK'NO # PRH'ODD'TRK'NO THEN
                PRH2'KTRK'NO = PRH'ODD'TRK'NO
                \VUSE PRH2 KTRK'NO PASSED'PRH'ARRAY AMOS="Y"
        ELSE
                PRH2'CTL = 0
        ENDIF
        IF PRH2'CTL = 0 THEN
                IF PRH2'ATT'SPACE # SPACE THEN
!![70] - this option, delete caching option.
                        FOR II = 1 TO 6
                                IF PRH2'ATT'DESC(II) # "" AND &
                                                WGA'TITLES(II) = "" THEN
                                        WGA'AFR(II) = "Y"
                                        ATT'NO = II
                                        PRH3'REC = PRH'REC
                                        PRH'REC = PRH2'REC
                                        CALL SET'ATT'CODE
                                        PRH'REC = PRH3'REC
                                        WGA'TITLES(II) = ATT'CODE
                                ENDIF
                        NEXT II
                ELSE
                        ATT'KATH'NO = PRH2'ATH'NO1
                        ATT'KNO = 0
                        USE ATT INTO ATT'REC KEY ATT'KEY
                        DO
                                GETNXT ATT INTO ATT'REC
                                IF ATT'CTL # 0 OR ATT'ATH'NO # ATT'KATH'NO OR &
                                                                ATT'NO > 6 EXIT
                                II = ATT'NO
                                IF II >= 1 AND II <= 6 THEN
                                    IF WGA'TITLES(II) = "" THEN
                                        WGA'AFR(II) = "Y"
                                        WGA'TITLES(II) = ATT'DESC
                                    ENDIF
                                ENDIF
                        LOOP
                ENDIF
        ENDIF
RETURN

SET'ODD'SIZES:
CIF USE'CAP = "Y" THEN
    IF INC'TS'SUBI = 4 RETURN
ENDCIF
        IF PRH2'TRK'NO # PRH'ODD'TRK'NO THEN
                PRH2'KTRK'NO = PRH'ODD'TRK'NO
                \VUSE PRH2 KTRK'NO PASSED'PRH'ARRAY AMOS="Y"
        ELSE
                PRH2'CTL = 0
        ENDIF
        IF PRH2'CTL = 0 THEN
                IF PRH2'ATT'SPACE # SPACE THEN
!![70] - this option, delete caching option.
                        ATT'SPACE = SPACE
                        FOR QQ = 1 TO 6
                                IF PRH2'ATT'DESC(QQ) # "" THEN
                                        ATT'NO = QQ
                                        PRH3'REC = PRH'REC
                                        PRH'REC = PRH2'REC
                                        CALL SET'ATT'CODE
                                        PRH'REC = PRH3'REC
                                        CALL SET'ODD'SIZES2
                                ENDIF
                        NEXT QQ
                ELSE
                        ATT'KATH'NO = PRH2'ATH'NO1
                        ATT'KNO = 0
                        USE ATT INTO ATT'REC KEY ATT'KEY
                        DO
                                GETNXT ATT INTO ATT'REC
                                IF ATT'CTL # 0 OR ATT'ATH'NO # ATT'KATH'NO OR &
                                                                ATT'NO > 6 EXIT
                                CALL SET'ODD'SIZES2
                        LOOP
                ENDIF
        ENDIF
RETURN

SET'ODD'SIZES2:
        II = ATT'NO
        IF II >= 1 AND II <= 6 THEN
            IF WGA'TITLES(II) = "" THEN
                WGA'AFR(II) = "Y"
                WGA'TITLES(II) = ATT'DESC
                ZZZ3'SPACE = SPACE
                SUB'SPACE = SPACE
                IF INC'DSPAVL # "N" THEN        !![25]
                        PRH3'REC = PRH'REC
                        PRH3'RCN = PRH'RCN              !![52]
                        PRH'REC = PRH2'REC
                        PRH'RCN = PRH2'RCN              !![52]
                        LIN'TRK'NO = PRH'TRK'NO
                        CALL AVAILABLE
                        PRH'REC = PRH3'REC
                        PRH'RCN = PRH3'RCN              !![52]
                        LIN'TRK'NO = PRH'TRK'NO
                        IF INC'DZ'PIECES # "N" THEN
                            DOZENS(II) = FIX(AVAILABLE/12)
                            IF DOZENS(II) > 999 THEN
                                        DOZENS(II) = 999
                                        PIECES(II) = 99
                            ELSE
                                        PIECES(II) = AVAILABLE-(DOZENS(II)*12)
                            ENDIF
                        ELSE
                            PIECES(II) = AVAILABLE
                        ENDIF
                ENDIF                           !![25]
            ENDIF
        ENDIF
RETURN

MAP1 LDESC,S,30

PRE'DSP'LOC'DESC'LIN:
        LDESC = LOC'DESC
        LOC'DESC = LOC'SDESC
        IF LOC'REAL'YN = "D" AND LOC'OC = "O" THEN
                LOC'DESC = "*No same day ship*"
        ELSE
        IF LOC'OC = "C" THEN
                LOC'DESC = LOC'DESC + " *** Closed ***"
        ENDIF
        ENDIF
        CALL DSP'LOC'DESC'LIN
        LOC'DESC = LDESC
RETURN

MAP1 CASH'TERMS,S,1

GET'CASH'TERMS:
        IF HDR'PROC < 5 THEN
                CASE
                IF TRH'NO = HDR'TRH'NO AND TRH'COD'STATUS > 1 THEN !![50]
                        CASH'TERMS = "Y"
                ELSE
                IF TRM'NO # HDR'TRH'NO THEN
                        XCALL VICSIT,7,106,TRM'REC,HDR'TRH'NO,TRMPTR,5,TRM'PKT
                        IF TRM'CTL # 0 THEN
                                TRM'KNO = HDR'TRH'NO
                                TRM'KCASE = 0
                                TRM'KSEQ = 0
                                USE TRM INTO TRM'REC KEY TRM'KEY
                                GETNXT TRM INTO TRM'REC
                                XCALL VICSIT,7,105,TRM'REC,HDR'TRH'NO,TRMPTR,5,TRM'PKT
                        ENDIF
                        IF TRM'NO # HDR'TRH'NO OR TRM'CTL # 0 THEN
                                ENTRY = "Invalid terms: "+TRM'KNO
                                CALL DO'ERRL
                                GOTO ABORT
                        ENDIF
                        IF TRM'DUE'DAYS = 0 THEN        
                                CASH'TERMS = "Y"
                        ELSE
                                CASH'TERMS = "N"
                        ENDIF
                ENDCASE
        ENDIF
RETURN
!!
!![35] - below
!!
GLOBAL PREP'FOR'ADD|NM|="PREP'FOR'ADD|NM|"
GLOBAL PREP'FOR'ADD'BRANCH="PREP'FOR'ADD'BRANCH"

PREP'FOR'ADD|NM|:
        IF TOP'MARK # 0 OR PG # |NM| OR SEL1(|NM|) # 0                  OR &
                        SPECIAL'ADD = 0 OR SCR'CNT(|NM|) >= |SCR'MAX|   OR &
                        (SPECIAL'LAST = 0 AND (SCR'CNT(|NM|) # 0        OR &
                            OEC'SKIP'FUT = "Y" OR &
                                                HDR'LIN'COUNT # 0)) THEN
                SPECIAL'ADD = 0
                GOTO DO'PREP'FOR'ADD
        ENDIF
        CALL CLR'BAR
        BTM'LOADED(|NM|) = 1
        TOP'LOADED(|NM|) = 1
        I = SPECIAL'LAST + |TRX'STEP|
        SPECIAL'LAST = I
        GOTO PREP'FOR'ADD'BRANCH        
RETURN

GLOBAL DELETE'IT|NM|="DELETE'IT|NM|"

DELETE'IT|NM|:
        SPECIAL'ADD = 0
        GOTO DO'DELETE'IT
RETURN
!!
!![35] - above

E'OE1WGA'GKD:                                   !![59]
        IF INX1 = I'RT AND (SELECT(|NM|) = 1 OR SELECT(|NM|) = 2) AND &
                                        LIN'NO = 0 THEN
                ENTRY = "Cannot enter line #0!"
                GOTO DO'ERR
        ENDIF
        CALL E'GKD
RETURN

CIF UP'CHARGES = "Y" THEN
    ADD'UP'QTY:
        IF ARCHIVE'FLAG # "C" RETURN            !!Kludge
        CIF UP'CHARGES = "Y" THEN
            IF OEC'SKIP'UP # "Y" CALL SUB'OLD'UP'QTY
        ENDCIF

        IF PRH'ITP'NO > 0 AND PRH'ITP'NO <= 100 THEN
                TST'ITP'NO = PRH'ITP'NO
                IF OEC'SKIP'UP # "Y" CALL COMPUTE'UP'CHARGE
                SAV'RTOTAL(1) = UP'RTOTAL(1)                    !!Previous up charge
                SAV'RTOTAL(2) = UP'RTOTAL(2)                    !!Previous up charge
                UP'OQTY(PRH'ITP'NO,SUB'NO) = UP'OQTY(PRH'ITP'NO,SUB'NO) + XXX'ORDER'QTY
                IF HDR'QUOTE'YN # "Y" THEN
                        UP'CQTY(PRH'ITP'NO,SUB'NO) = UP'CQTY(PRH'ITP'NO,SUB'NO) + XXX'COM'QTY
                ELSE
                        UP'CQTY(PRH'ITP'NO,SUB'NO) = UP'CQTY(PRH'ITP'NO,SUB'NO) + XXX'ORDER'QTY
                ENDIF
                IF OEC'SKIP'UP # "Y" CALL COMPUTE'UP'CHARGE     !!New up charge
!!Add diference between old up charge and new up charge
                IF OEC'SKIP'UP # "Y" THEN
                        \UPBUCKET "(UP'RTOTAL(2)-SAV'RTOTAL(2))"
                ENDIF
                HDR'TMP'UC = HDR'TMP'UC + (UP'RTOTAL(1) - SAV'RTOTAL(1))
        ENDIF
    RETURN

    SUB'OLD'UP'QTY:
        IF ARCHIVE'FLAG # "C" RETURN            !!Kludge
        IF SAV'ITP'NO > 0 AND SAV'ITP'NO < 100 THEN
                TST'ITP'NO = SAV'ITP'NO
                IF OEC'SKIP'UP # "Y" CALL COMPUTE'UP'CHARGE
                SAV'RTOTAL(1) = UP'RTOTAL(1)                    !!Previous up charge
                SAV'RTOTAL(2) = UP'RTOTAL(2)                    !!Previous up charge
                UP'OQTY(SAV'ITP'NO,SUB'NO) = UP'OQTY(SAV'ITP'NO,SUB'NO) - SAV'ORDER'QTY
                IF HDR'QUOTE'YN # "Y" THEN
                        UP'CQTY(SAV'ITP'NO,SUB'NO) = UP'CQTY(SAV'ITP'NO,SUB'NO) - SAV'COM'QTY
                ELSE
                        UP'CQTY(SAV'ITP'NO,SUB'NO) = UP'CQTY(SAV'ITP'NO,SUB'NO) - SAV'ORDER'QTY
                ENDIF
                IF OEC'SKIP'UP # "Y" CALL COMPUTE'UP'CHARGE     !!New up charge
!!Subtract difference  between old up charge and new up charge.
                IF OEC'SKIP'UP # "Y" THEN
                        \UPBUCKET "(UP'RTOTAL(2)-SAV'RTOTAL(2))"
                ENDIF
                HDR'TMP'UC = HDR'TMP'UC + (UP'RTOTAL(1)-SAV'RTOTAL(1))
        ENDIF
    RETURN
ENDCIF

CIF COMMENTS AND NOT COMMENTS

        THE FOLLOWING LABELS IN WGAWGA.CPY (copied in SUBWGA.CPY)

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

LCL1 QQQ,F

GET'CASE'QQ:
CIF USE'CAP = "Y" THEN
  IF INC'TS'SUBI = 4 THEN
        CASE'QQ = 3
        DZ'QQ = 2
  ELSE
ENDCIF
        CASE'QQ = 0
        DZ'QQ = 0

    IF LIN'UOM'FACTOR(3) # 0 THEN
        CASE'QQ = 3
        DZ'QQ = 2
    ELSE
        FOR QQQ = 3 TO 1 STEP -1
            IF LIN'UOM'FACTOR(QQQ) > 12 AND LIN'UOM'FACTOR(QQQ) # 1200 AND &
                                                        CASE'QQ = 0 THEN
                        CASE'QQ = QQQ
            ENDIF
            IF LIN'UOM'FACTOR(QQQ) <= 12 AND LIN'UOM'FACTOR(QQQ) > 1 AND &
                                                        DZ'QQ = 0 THEN
                DZ'QQ = QQQ
            ENDIF
        NEXT QQQ
        IF CASE'QQ = 0 THEN
            CALL FIND'PRH
            IF PRH'MEA'TRK'YN = "Y" AND PRH'MEA'UOM'NO = LIN'UOM'NO AND &
                              (PRH'PRC'TRK'NO = 0 OR PRH'WGTS'BY = "N") THEN
                MEA'SPACE = SPACE
                MEA'TRK'NO = PRH'TRK'NO
                MEA'UOM'NO = PRH'MEA'UOM'NO
                MEA'PRH'OVR = PRH'MEA'OVR
                MEA'CTL = 0
            ELSE
                MEA'UOW'NO = 1
                IF PRH'TRK'NO # LIN'TRK'NO OR &
                                  PRH'PRC'TRK'NO = 0 OR PRH'WGTS'BY # "Y" THEN
                        MEA'KTRK'NO = LIN'TRK'NO
                ELSE
                        MEA'KTRK'NO = PRH'PRC'TRK'NO
                ENDIF
                MEA'KUOM'NO = LIN'UOM'NO
                \VUSE MEA
            ENDIF
            IF MEA'KTRK'NO # MEA'TRK'NO THEN
                MEA'PRH'OVR = PRH'MEA'OVR
            ENDIF
            IF MEA'TREAT'CASE = "D" THEN
                CASE'QQ = DZ'QQ
                DZ'QQ = 0
            ENDIF
        ENDIF
    ENDIF
CIF USE'CAP = "Y" THEN
  ENDIF
ENDCIF
RETURN

MAP1 O'HPRICE,B,1

HPRICING:
        IF OEC'QTY'BREAKS # "N" THEN
            CALL DO'PRICE                   !!Always check pricing
        ELSE
            CALL SEE'IF'HPRICE
            IF SF # O'HPRICE THEN           !!Different pricing needed
                CALL DO'PRICE
            ELSE
            IF SF = 1 AND LEN(HPRICE'NOTE) > 4 THEN !![95] LEN test added.
                II = INC'HCASES - TCASES
                IF HPRICE'NOTE # "Prices from quote" THEN
                    HPRICE'NOTE[1;3] = II USING "###"
                ENDIF
            ENDIF
            ENDIF
            CALL DSP'PRICING
        ENDIF
RETURN

MAP1 TCASES,B,5
MAP1 HPRICE'NOTE,S,40

SEE'IF'HPRICE:
    SF = 0
    CALL SETUP'TYP
CIF USE'CAP = "Y" THEN
    IF INC'TS'SUBI = 4 THEN
        IF TYP'BRK'BASE = 4 THEN
            SF = 2
        ELSE
            II = OEC'COLQ(4) - INT(CAPTTL/12)
            IF II <= 0 THEN SF = 2 ELSE SF = 1
        ENDIF
    ELSE
ENDCIF
          IF INC'HCASES > 0 AND HPRICE'YN = "Y" THEN
                CALL GET'CASE'QQ
                IF CASE'QQ > 0 THEN
                    IF TPRICE(1) = 5 AND TPRICE(2) = 5 AND TPRICE(3) = 5 THEN
                        SF = 2
                    ELSE
                        TCASES = 0
                        IF INC'HEASY = "Y" THEN !![87] this method.
!!This method is not "real" cases - e.g. 1/2 case sml + 1/2 case lrg = 1 case.
                            FOR II = 1 TO 6
                                TCASES = TCASES + LIN'TORDER'QTY(II)
                            NEXT II
                            TCASES = ABS(TCASES)
                            \FROUND TCASES/LIN'UOM'FACTOR(CASE'QQ) TCASES
                        ELSE
!!This method is "real" cases - counts only whole cases for each size.
                            FOR II = 1 TO 6
                                TCASES = TCASES + ABS(INT(LIN'TORDER'QTY(II)/LIN'UOM'FACTOR(CASE'QQ)))
                            NEXT II
                        ENDIF
                        IF TCASES >= INC'HCASES THEN SF = 2 ELSE SF = 1
                    ENDIF
                ENDIF
          ENDIF
CIF USE'CAP = "Y" THEN
    ENDIF
ENDCIF
RETURN

E'XXX'RPROC:
        \TABEDTC PROC1 XXX'RPROC
RETURN

FIND'LOC:
!![99] IF LOC'NO # LOC'KNO THEN
!![99]  \VUSE LOC - LOCPTR MODE="1107"
!![99] ENDIF
    \VUSE LOC
RETURN

MAP1 LINE'HDR,S,4
MAP1 NET'HDR,S,4

SET'LINE'HDR1:
        LINE'HDR = "Line"
        IF INC'MULTI'LOC = "Y" THEN     !![54] - set PART'DESC
            PART'DESC = "Loc "[1,OC2'MAXLOC'DSP]+" Part               XS     SM     MD     LG     XL 2/3/4X"
!Loc  Part               XS     SM     MD     LG     XL 2/3/4X
        ELSE
            PART'DESC = "Style #                XS     SM     MD     LG     XL 2/3/4X"
        ENDIF
        IF INC'DZ'PIECES = "N" THEN
            NET'HDR = " Net"
        ENDIF
RETURN

SET'LINE'HDR2:
    IF ENVSTR[8;2] = "N" THEN            !oe1wga'dspcola = "N"
        LINE'HDR = "Loc."
        PART'DESC = "         "
        NET'HDR = "Zone"
!!Does not happen for OSTP -- see SET'LINE'HDR1 which it does use
        CALL DSP'LINE'HDR3
        CALL DSP'LINE'HDR4
        CALL DSP'LINE'HDR5
        CALL DSP'LINE'HDR6
        CALL DSP'LINE'HDR7
        CALL DSP'LINE'HDR8
        IF INC'DZ'PIECES = "N" THEN
            NET'HDR = " Net"
        ENDIF
    ELSE
        LINE'HDR = "No  "
        PART'DESC = "Color    "
        NET'HDR = "    "
    ENDIF
RETURN

DSP'LINE'HDR:
        IF INC'DZ'PIECES = "N" THEN
            CALL DSP'LINE'HDR1
        ELSE
            CALL DSP'LINE'HDR2
        ENDIF
        NET'HDR = " Net"
        CALL DSP'NET'HDR
RETURN

!ADJ'UOM'TITLES:                         !![94]
!        FOR II = 2 TO 3
!                IF LIN'UOM'FACTOR(II) # 0 THEN
!                        UOM'TITLES(1) = UOM'TITLES(II)
!                ENDIF
!        NEXT II
!        UOM'TITLES(2) = "  Cost"
!        UOM'TITLES(3) = "  Last"
!RETURN

MAP1 DSP'TRK'SPACE,S
    MAP2 DSP'TRK'NO(300),B,3
    MAP2 DSP'TRK'CNT,B,2
    MAP2 DSP'TRK'POS,B,2
    MAP2 DSP'TRK'PAGE,B,1
    MAP2 ORG'TRK'NO,B,3

LOAD'TRK'NOS:
    DSP'TRK'SPACE = SPACE
    ORG'TRK'NO = PRH'TRK'NO
    DSP'TRK'NO(1) = ORG'TRK'NO
    DSP'TRK'CNT = 1
    JJ = VAL(ENVSTR[8;2])         !!Length of part w/out color, oe1wga'dspcola
    PRHP'KPART = PRH'PART[1,JJ]
    USE PRHP INTO PRH'REC KEY PRHP'KEY
    DO
        GETNXT PRHP INTO PRH'REC
        IF PRHP'CTL # 0 OR PRH'PART[1,JJ] # PRHP'KPART EXIT
        IF PRH'TRK'NO = ORG'TRK'NO REPEAT
        IF PRH'ACTIVE = "N" REPEAT
        SF = 0
        CALL E'TRK'NO'SF
        IF SF # 0 REPEAT
        DSP'TRK'CNT = DSP'TRK'CNT + 1
        DSP'TRK'NO(DSP'TRK'CNT) = PRH'TRK'NO
    LOOP UNTIL DSP'TRK'CNT = 300
    DSP'TRK'POS = 1
RETURN

DSP'BOTTOMS'L25:
        IF ENVSTR[6;2] # "N" THEN        !!oe1wga'dsploca # "N"
            L25="F1 update |F4 can |Fn M>enu |P>ost |W>header |D>etail |F11 choose location"
        ELSE
            IF DSP'TRK'CNT > 12 THEN
                L25="F1 upd |F4 can |P>ost |W>header |F11 choose colors |Page Up/Down to scroll"
            ELSE
                L25="F1 update |F4 can |Fn M>enu |P>ost |W>header |D>etail |F11 choose colors"
            ENDIF
        ENDIF
        \CURL25 NEWL25="L25"
RETURN

DSP'BOTTOMS'SETUP:
   IF BOTTOM'AFU = 0 THEN
        \PUSHEDT
        \WINDOW SAVE="Y" RESTORE="N" REPAINT="N" &
                                        TLR="TSCR(|NM|)" WINNUM="7"
        \POPEDT
        BOTTOM'AFU = 1
        CALL DSP'BOTTOMS'L25
   ENDIF
   PRINT TAB(VDSP'LIN,1) ; TAB(-1,10) ;
   CALL SET'LINE'HDR2
   ?TAB(-1,14);
   \CLRWIN 11 2 11 29
   CALL DSP'LINE'HDR
   NET'HDR = "Zone"
   CALL DSP'NET'HDR
!! SDSP'NET = ""
RETURN

DSP'BOTTOMS:
    FOR II = 1 TO 6
        IF PRH'ODD'TRK'NO # 0 THEN
            IF PRH'ATT'SPACE # SPACE AND PRH'ATT'DESC(II) # "" THEN
                CALL AVAILABLE
            ELSE
                ATT'KATH'NO = PRH'ATH'NO1
                ATT'KNO = II
                RDKEY ATT
                IF ATT'CTL = 0 THEN
                    CALL AVAILABLE
                ELSE
                    PRH3'REC = PRH'REC
                    PRH3'RCN = PRH'RCN              !![52]
                    PRH'REC = PRH2'REC
                    PRH'RCN = PRH2'RCN              !![52]
                    LIN'TRK'NO = PRH'TRK'NO
                    CALL AVAILABLE
                    PRH'REC = PRH3'REC
                    PRH'RCN = PRH3'RCN              !![52]
                    LIN'TRK'NO = PRH'TRK'NO
                ENDIF
            ENDIF
        ELSE
            CALL AVAILABLE
        ENDIF
        IF AVAILABLE # 0 THEN TQTY = 1
        IF WGA'TITLES(II) = "" THEN
            DZPC(II) = SPACE(6)
        ELSE
        IF INC'DZ'PIECES # "N" THEN
            DOZENS(II) = FIX(AVAILABLE/12)
            IF DOZENS(II) > 999 THEN
                DOZENS(II) = 999
                PIECES(II) = 99
            ELSE
                PIECES(II) = AVAILABLE-(DOZENS(II)*12)
            ENDIF
            DZPC(II) = (ABS(DOZENS(II)) USING "###") + "/" + ABS(PIECES(II)) + SPACE(2)
        ELSE
            DZPC(II) = AVAILABLE USING "######"
        ENDIF
        ENDIF
    NEXT II
    IF TQTY = 1 OR ENVSTR[6;2] = "N" THEN  !oe1wga'dsploca = "N"
        IF ENVSTR[8;2] = "N" THEN          !oe1wga'dspcola = "N"
            XLN = XLN + 1
            DSP'LIN'NO = ALOC'NO(QQ)
            LOC'KNO = ALOC'NO(QQ)
            CALL FIND'LOC
            PRH'FORMATTED = LOC'DESC
            IF LOC'OC = "C" THEN
                PRH'FORMATTED = PRH'FORMATTED + " *Closed"
            ENDIF
        ELSE
            XLN = QQ - 1
            DSP'LIN'NO = QQ
            CALL FIND'PRH
!           CALL DO'DSPPRH
            ATTD'KATH'NO = ROT'ATH'NO(2)
            ATTD'KCODE = PRH'PART[VAL(ENVSTR[8;2])+1,-1]  !oe1wga'dspcola
            \STRIP ATTD'KCODE
            \VUSE ATTD VREC="ATT'REC"
            IF ATTD'CTL = 0 THEN
               PRH'FORMATTED = ATT'DESC
            ELSE
               PRH'FORMATTED = PRH'DESC
            ENDIF
        ENDIF
        SDSP'NET = ALOC'ZONE(QQ)
        SDSP'NET = RIGHT(SPACE(9)+SDSP'NET,9)
        IF DZPC(1) # "" THEN
            PRH'FORMATTED = PRH'FORMATTED + SPACE(17)
            PRH'FORMATTED = PRH'FORMATTED[1;17] + " " + BO(1) + DZPC(1)
        ENDIF
        CALL |VDSP|
        IF ENVSTR[8;2] = "N" THEN        !!oe1wga'dspcola = "N"
            JJ = JJ - 1
            IF JJ <= 0 THEN QQ = 13
        ENDIF
    ENDIF
RETURN

DO'DSPPRH:
    \DSPPRH FLDNAM="LIN'TRK'NO" DSPFLD="PRH'FORMATTED" NOUSE="Y"
RETURN

CIF USE'KITS AND USE'KITS = "Y" THEN
  UNCOMMIT'KIT:
        COM'CNT = 0
        DOKITS'FLAG = 3
        DOKITS'ATT'NO1 = ZZZ3'ATT'NO1
        CALL DOKITS'MACRO
        IF DOKITS'ABORT # "N" AND DOKITS'ABORT # "Z" THEN
                DOKITS'FLAG = 9
                CALL DOKITS'MACRO
!![48] - remove spurious message by clearing DOKITS'FLAG
                IF DOKITS'ABORT = "N" OR DOKITS'ABORT = "Z" THEN
                        DOKITS'FLAG = 3
                ENDIF
        ENDIF
RETURN

CIF USE'KITS AND USE'KITS = "Y" THEN
COMMIT'KIT:
        COM'CNT = 0
        DOKITS'FLAG = 2
        DOKITS'ATT'NO1 = SUB'ATT'NO1
        CALL DOKITS'MACRO
        IF DOKITS'ABORT # "N" AND DOKITS'ABORT # "Z" THEN
                DOKITS'FLAG = 8
                CALL DOKITS'MACRO
!![48] - remove spurious message by clearing DOKITS'FLAG
                IF DOKITS'ABORT = "N" OR DOKITS'ABORT = "Z" THEN
                        DOKITS'FLAG = 2
                ENDIF
                CALL ZERO'COM'SET'BO
        ENDIF 
RETURN
ENDCIF

PACTION:
        IF DOKITS'FLAG = 1 RETURN       !!JUST COMPUTE AVAILABLE IN MACRO
        IF PRH2'INVENTORY = "N" RETURN  !!NO RECORD TO WRITE !![47]
CIF USE'KITS AND USE'KITS = "Y" THEN
        II = DOKITS'THIS'MULT/(10^XXX'QFRACTION)
        CASE
        IF DOKITS'FLAG = 2 THEN
!!COMMIT THE STOCK
                \FROUND XXX'COM'QTY*II TQTY
                \FROUND XXX'BO'QTY*II BQTY
                CALL SET'T'B'QTYS
                IF ((TQTY > DOKITS'AVL AND TQTY > 0)                    OR &
                            (TQTY = 0 AND BQTY = 0)) AND LIN'OVERIDE # 1 THEN
                        ENTRY = "Cannot reserve all committed stock - backordering all"
                        CALL DO'ERR
                        DOKITS'ABORT = "Y"
                ELSE
                        SIGN = 1
                        CALL COMMIT'QTY
                ENDIF
                COM'CNT = COM'CNT + 1   !!Counts number of commited
        ELSE
        IF DOKITS'FLAG = 3 THEN
!!UNCOMMIT THE STOCK
                \FROUND ZZZX'COM'QTY*II TQTY
                \FROUND ZZZX'BO'QTY*II BQTY
                TUOM = ZZZ2'UOM'NO
                CALL FIND'UOM2
                SIGN = -1
                CALL COMMIT'QTY
                COM'CNT = COM'CNT + 1   !!Counts number of commited recs
        ELSE
        IF DOKITS'FLAG = 8 THEN
!!REVERSE COMMIT THE STOCK
                COM'CNT = COM'CNT - 1
                IF COM'CNT <= 0 THEN
                        DOKITS'ABORT = "Y"              !!DONE
                        RETURN                          !!KLUDGE
                ENDIF
                \FROUND XXX'COM'QTY*II TQTY
                \FROUND XXX'BO'QTY*II BQTY
                CALL SET'T'B'QTYS
                SIGN = -1
                CALL COMMIT'QTY
        ELSE
        IF DOKITS'FLAG = 9 THEN
!!REVERSE UNCOMMIT THE STOCK
                COM'CNT = COM'CNT - 1
                IF COM'CNT <= 0 THEN
                        DOKITS'ABORT = "Y"              !!DONE
                        RETURN                          !!KLUDGE
                ENDIF
                \FROUND ZZZX'COM'QTY*II TQTY
                \FROUND ZZZX'BO'QTY*II BQTY
                TUOM = ZZZ2'UOM'NO
                CALL FIND'UOM2
                SIGN = 1
                CALL COMMIT'QTY
        ENDCASE
ENDCIF
        QTY'NEW'STOCK = QTY'NEW'STOCK MIN QTY'BO'QTY MAX 0
        WRITE QTY
        CIF USE'CHI = "Y" THEN
            \CHI QTY
        ENDCIF
  RETURN
ENDCIF
!!\COPYF PRH IN PRH2

CIF USE'KITS AND USE'KITS = "Y" THEN
  WINDOW'TS'KTD:
    IF (CHG'QTY'KEY # 0 AND LIN'TRK'NO # ZZZ2'TRK'NO) OR &
                                         LIN'KTD'TRK'NO = 0 THEN
        KTD'KPRH'TRK'NO = LIN'TRK'NO
        KTD'KPRH'ATT1 = 0
        KTD'KPRH'ATT2 = 0
        KTD'KSEQ = 0
        USE KTD INTO KTD'REC KEY KTD'KEY
        DO
            GETNXT KTD INTO KTD'REC
            \CHKKEY KTD APID="IN"
            IF KTD'CTL # 0 EXIT
        LOOP UNTIL KTD'REPLACE = "Y" OR KTD'REPLACE = "F"
        IF KTD'CTL = 0 THEN
            LIN'KTD'TRK'NO = KTD'TRK'NO
        ELSE
            LIN'KTD'TRK'NO = 0
        ENDIF
    ENDIF
    IF LIN'KTD'TRK'NO # 0 AND (INX2 = FN'B OR KTD'REPLACE = "F") THEN
        \WINDOW PUSH="Y" OVRLAY="KTDPRH" AUTO="Y" SAVSCR="SMALL"
        IF LIN'KTD'TRK'NO # ZZZ2'KTD'TRK'NO THEN
            SAV'TRK'NO = LIN'KTD'TRK'NO
            LIN'KTD'TRK'NO = ZZZ2'KTD'TRK'NO
            CALL DELLOW'SUB
            LIN'KTD'TRK'NO = SAV'TRK'NO
            SAV'TRK'NO = 0
        ENDIF
        CALL PRE'PRE'DSP'WGA'AVAIL
        CALL PRE'DSP'WGA'AVAIL
        ECFLD = 3            !!First QTY field
        IER1 = 0
    ELSE
        IF INX2 = FN'B THEN
            ENTRY = "No blank in this kit to replace!"
            GOTO DO'ERR
        ENDIF
    ENDIF
    CALL PRE'DSP'BLANK
RETURN

E'TS'KTD:
    \TABPRH EDIT="T" FLDNAM="LIN'KTD'TRK'NO" DSPNAM="DSP'KTD'PRH'DESC" &
                        PRE="PRH2" SF'TEST="KTD'SF'TEST" QTY="N" DOERR="GOTO"
RETURN

KTD'SF'TEST:
    IF PRH2'TS'TYPE # "B" THEN
        ENTRY = "Item selected is not a blank!"
        SF = 2
    ELSE
    IF PRH2'KTD'FLAG # "S" THEN
        ENTRY = "Cannot select a kit as a blank!"
        SF = 2
    ELSE
    IF PRH2'SGP'NO # PRH'SGP'NO THEN
        ENTRY = "Style groups do not match!"
        SF = 2
    ELSE
    IF PRH2'WLDE # PRH'WLDE THEN !AND ALLOW'DYE = "N" THEN (oeshop uses)
        ENTRY = "Color codes do not match!"
        SF = 2
    ELSE
    IF PRH2'ATH'NO1 # PRH'ATH'NO1 THEN
        ENTRY = "Allowable sizes must match!"
        SF = 2
    ENDIF
    ENDIF
    ENDIF
    ENDIF
    ENDIF
RETURN

PRE'DSP'BLANK:
    IF LIN'KTD'TRK'NO # 0 THEN
        \DSPPRH FLDNAM="LIN'KTD'TRK'NO" DSPFLD="ENTRY" NOUSE="N" PRE="PRH2"
        ENTRY = "Blank Used: "+ENTRY+" "+PRH2'DESC
    ELSE
    IF LIN'TRK'NO # 0 THEN
        ENTRY = ""
    ELSE
        ENTRY = "Blank not selected"
    ENDIF
    ENDIF
    CALL DSP'BLANK
RETURN

BLDWIN'PROC:                            !!Used in SUBSUB.CPY as well.
        IF PRH'KTD'FLAG = "O" AND HDR'PROC # 6 THEN
                \WINDOW TLR="7" TLC="39" BRR="9" BRC="61"               &
                        VUEMLT="Y" OVRLAY="BLDWIN" SAVSCR="SMALL"       &
                                                L25NUM="6" L25="Y"
                CALL PRE'PRE'DSP'WGA'AVAIL
                CALL PRE'DSP'WGA'AVAIL
        ELSE
        IF WINDOW # FN'I THEN
                IF PRH'KTD'FLAG = "B" THEN
                        ENTRY = "This kit MUST ALWAYS be built"
                ELSE
                IF PRH'KTD'FLAG = "P" THEN
                        ENTRY = "This kit is already built"
                ELSE
                IF HDR'PROC = 6 THEN
                        ENTRY = "You may only transfer built kits"
                ELSE
                        ENTRY = "This part is not available as a kit"
                ENDIF                   !![40]
                ENDIF                   !![40]
                ENDIF                   !![40]
                CALL DO'ERR
        ENDIF
        ENDIF
RETURN

SET'KIT'BUILD:
    IF HDR'PROC = 5 THEN
         XXX'KIT'BUILD = "Y"        !!Shop orders always build (duh)
    ELSE
    IF HDR'PROC = 6 THEN
         XXX'KIT'BUILD = "N"        !!On transfer can't build
    ELSE
!!regular orders
        IF ARC'INV'CR'YN(HDR'DOC'TYPE) = "Y" THEN
            XXX'KIT'BUILD = "N"   !! force return merchandise to Kit Master part
         ELSE
         IF PRH'KTD'FLAG = "O" THEN
             IF XXX'KIT'BUILD # "N" THEN XXX'KIT'BUILD = "Y"
         ELSE
         IF PRH'KTD'FLAG = "B" THEN
             XXX'KIT'BUILD = "Y"     !!Build/Explode
         ELSE
             XXX'KIT'BUILD = "N"     !!Single/Pre-built
         ENDIF
         ENDIF
         ENDIF
    ENDIF
    ENDIF
  RETURN
ENDCIF

MAP1 QUOTE'NAM'NO,B,3
MAP1 QUOTES'YN,S,1

CHECK'QUOTE:
    SF = 0
    LIN'QFREE'FRT = ""
    ON'SALE'AND'QUOTE = "N"
    IF OC2'CHK'QUOTE # "Y" RETURN              !!KLUDGE
    IF HDR'QUOTE'YN = "Y" RETURN               !!KLUDGE
    IF QUOTE'NAM'NO = HDR'NAM'NO THEN
        IF QUOTES'YN = "N" RETURN              !!KLUDGE
    ENDIF
    QUOTE'NAM'NO = HDR'NAM'NO
    QUOTES'YN = "N"
    \CNV'DTT DTT="DATE" YDAYS="QDAYS"
    HDRC2'KNAM'NO = HDR'NAM'NO
    HDRC2'KLOC'NO = 0
    HDRC2'KDOC'TYPE = 0
    HDRC2'KNO = 0
    USE HDRC2 INTO HDR2'REC KEY HDRC2'KEY
    DO
        GETNXT HDRC2 INTO HDR2'REC
        IF HDR2'NAM'NO # HDR'NAM'NO OR HDRC2'CTL # 0 EXIT
        IF HDR2'QUOTE'YN # "Y" REPEAT
        \CNV'DTT DTT="HDR2'PROM'DTT"
        IF YDAYS < QDAYS REPEAT
        QUOTES'YN = "Y"
        IF HDR2'LEAD'ITEMS = "Y" THEN
            PRH2'KTRK'NO = LIN'TRK'NO
            USE PRH2 INTO PRH2'REC KEY PRH2'KEY
        ENDIF
        LIN2'KHDR'LOC'NO = HDR2'LOC'NO
        LIN2'KDOC'TYPE = HDR2'DOC'TYPE
        LIN2'KHDR'NO = HDR2'NO
        LIN2'KNO = 0
        USE LIN2 INTO LIN2'REC KEY LIN2'KEY
        DO
            GETNXT LIN2 INTO LIN2'REC
            \CHKKEY LIN ALTPRE="LIN2" ALTKEY="LIN2"
            IF LIN2'CTL # 0 EXIT
            IF HDR2'LEAD'ITEMS = "Y" THEN
                IF PRH2'PRC'TRK'NO # 0 THEN
                    SAV'TRK'NO = PRH2'PRC'TRK'NO
                ELSE
                    SAV'TRK'NO = LIN'TRK'NO
                ENDIF
                IF LIN2'TRK'NO = SAV'TRK'NO THEN
                    CALL SET'FROM'QUOTE
                ENDIF
            ELSE
                IF LIN2'TRK'NO = LIN'TRK'NO THEN
                    CALL SET'FROM'QUOTE
                ENDIF
            ENDIF
        LOOP UNTIL SF = 1
    LOOP UNTIL SF = 1
    SAV'TRK'NO = 0
RETURN

SET'FROM'QUOTE:
    CASE
    IF QQ = LIN'UOM'FACTOR(1) THEN
        PRICE = LIN2'TPRICE(1)
        DISC = 0
    ELSE
    IF QQ = LIN'UOM'FACTOR(2) THEN
        PRICE = LIN2'TPRICE(2)
        DISC = 0
    ELSE
    IF QQ = LIN'UOM'FACTOR(3) THEN
        PRICE = LIN2'TPRICE(3)
        DISC = 0
    ELSE
        IF QQ = 1200 THEN
            PRICE = LIN2'PRICE * 12
            DISC = LIN2'DISC * 12
        ENDIF
    ENDCASE
    LIN'QFREE'FRT = HDR2'FREE'FRT
    RETRO = 0
    CALL CHECK'ON'SALE
    SF = 1                        !!Means ON'QUOTE
RETURN

MAP1 ON'SALE'AND'QUOTE,S,1
MAP1 SPRICE,B,5
MAP1 SDISC,B,5

CHECK'ON'SALE:
    PRH'KTRK'NO = LIN'TRK'NO
    USE PRH INTO PRH'REC KEY PRH'KEY
    IF PRH'CTL # 0 THEN
        ENTRY = "Could not find part "
        \KERROR PRH APID="IN" MODE="D" CNO="N"
        CALL DO'ERR
    ELSE
        IF PRH'PRC'TRK'NO # 0 THEN
            PRH5'KTRK'NO = PRH'PRC'TRK'NO
            USE PRH5 INTO PRH5'REC KEY PRH5'KEY
            IF PRH5'CTL # 0 THEN
                ENTRY = "Could not find priced-by part "
                \KERROR PRH ALTPRE="PRH5" MODE="D" APID="IN" CNO="N"
                CALL DO'ERR
            ELSE
                ON'SALE'AND'QUOTE = PRH5'ON'SALE
            ENDIF
        ELSE
            ON'SALE'AND'QUOTE = PRH'ON'SALE
        ENDIF
    ENDIF
    IF LIN'QFREE'FRT = "Y" OR LIN'QFREE'FRT = "N" THEN
        HPRICE'NOTE = "Prices from quote"
        CALL DSP'HPRICE'NOTE
    ENDIF
RETURN

CHECK'HAZARDOUS:
    HAZ'SPACE = SPACE
    HAZ'KNO = PRH'HAZ'NO
    \VUSE HAZ
    IF HAZ'CTL # 0 THEN
        \STRIP PRH'PART 3
        ENTRY = "Hazardous material error on part: "+PRH'PART+"-"+PRH'TRK'NO+"-"+PRH'DESC
        RETURN
    ENDIF
    IF HDR'SHP'COU # "US" AND HAZ'OUTSIDE'US # "Y" THEN
        \STRIP PRH'PART 3
        ENTRY = "Cannot ship this item outside US: "+PRH'PART+"-"+PRH'TRK'NO+" c="+HDR'SHP'COU
        RETURN
    ENDIF
    IF HAZ'NOT'ST(1) # "" THEN
        SF = 1                !!Does not apply
        FOR QQ = 1 TO 52
            IF HDR'SHP'ST = HAZ'NOT'ST(QQ) THEN
                SF = 0        !!Applies to this state
                QQ = 100
            ENDIF
        NEXT QQ
    ELSE
        SF = 0                !!Applies to all states
    ENDIF
    IF SF = 1 THEN
        SF = 0
        GOTO STATE'DONE
    ENDIF
    IF SF = 0 AND QQ = 101 THEN
        ENTRY = "Cannot ship this item to: "+HDR'SHP'ST
        RETURN
    ENDIF
  STATE'DONE:
    IF (VIA'MTYPE = "G" AND HAZ'ALLOW'USPS # "Y") OR &
           (VIA'MTYPE = "E" AND HAZ'ALLOW'USPS # "Y") THEN
        \STRIP PRH'PART 3
        ENTRY = "Cannot ship this item USPS: "+PRH'PART+"-"+PRH'TRK'NO+" VIA="+VIA'NO
        RETURN
    ENDIF
    IF (VIA'MTYPE = "U" AND (VIA'MAN'YN # "C" AND VIA'MAN'YN # "G" &
                        AND VIA'MAN'YN # "H")) OR                          &
            (VIA'MTYPE = "F" AND (VIA'MAN'YN # "C" AND VIA'MAN'YN # "H")) THEN
        IF HAZ'ALLOW'AIR # "Y" THEN
            \STRIP PRH'PART 3
            ENTRY = "Cannot ship this item by air: "+PRH'PART+"-"+PRH'TRK'NO+"-"+PRH'DESC
            RETURN
        ENDIF
    ELSE
        IF HAZ'ALLOW'GRND # "Y" THEN
            \STRIP PRH'PART 3
            ENTRY = "Cannot ship this item by ground: "+PRH'PART+"-"+PRH'TRK'NO
            RETURN
        ENDIF
    ENDIF
RETURN

GET'TYD:
    PRC'KTRK'NO = LIN'TRK'NO
    PRC'KUOM'NO = LIN'UOM'NO
    IF PRH'PC'BY'LOC = "Y" THEN             !![X]
        PRC'KLOC'NO = LIN'LOC'NO
    ELSE
        PRC'KLOC'NO = 0
    ENDIF
    USE PRC INTO PRC'REC KEY PRC'KEY
    IF PRC'CTL = 0 THEN
        TYD'SPACE = SPACE
        TYD'KLOC'NO = PRC'LOC'NO
        TYD'KTRK'NO = PRC'TRK'NO
        IF PRC'TYD'BY'UOM = "N" THEN
            TYD'KUOM'NO = 0
        ELSE
            TYD'KUOM'NO = LIN'UOM'FACTOR(TYP'COL'BASE)
        ENDIF
        TYD'KTYP'NO = TYP'KNO
        USE TYD INTO TYD'REC KEY TYD'KEY
        IF TYD'CTL # 0
            TYD'SPACE = SPACE
        ENDIF
    ENDIF
RETURN

