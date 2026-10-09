! @#$^&* 1193 18 215 0 0 0 0 0 0 0 0 0 1196 *&^$#@ 
!* Updated on 11-Sep-95 at 1:57 PM by Eric Eberhard; edit time: 1:55:09
!!CMPCRL.CPY,8.0[7]
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
!!Compute credit limit - 
!!
!!      COMPUTE'CR'LIMIT used in:  HDRSUB.CPY, LINSUB.CPY, OERBAK.BV, OE1SHP.BV
!!      TEST'HOLDS       used in:  HDRSUB.CPY, LINSUB.CPY, OE1SHP.BV, OEPACK.BV
!!
!!E.G. OE1HDR, OE1WGA, OE1OFE, OE1SHP, OERBAK, OEPACK need recompil if changed!
!!
!!09/09/87 ESE  
!!
!!REVISION HISTORY:
!!
!!7.2   11/30/88 ASC    New version created       
!!
!![1] 04/25/90 KJS      Added commit review status.
!!
!![2] 06/04/90 ESE      COD checks only current order.
!!
!![3] 06/05/90 KJS      Added customer CR'STAT setting.
!!
!![4] 08/16/90 ESE/RG   Incorrectly added payments in mode 1
!!                      all programs recompiled, revision not changed.
!!
!![5] 11/14/90 HLB      Mapped BALFWD
!!
!![6] 03/20/91 ESE      Test TMP'AR if OEPDOC is at EOD.
!!
!![7] 05/14/91 ESE      Check credit limit differently.
!!
!![8] 08/30/91 ESE      Another credit limit check!
!!
!![9] 09/24/91 ESE      Check credit early if fast A/R and also
!!                      subtract current order from fast AR.
!!
!![10] 02/26/92 ESE     Check past due invoices.
!!
!![11] 10/28/92 ESE     Do not check credits.
!!
!![12] 07/09/93 ESE/HLB NEW: Do not remove the hold flag if not
!!                      recalculating the credit limit.
!!
!![13] 06/24/94 HLB     NEW: Added CHECK'CREDIT'CARD
!!
!![14] 08/08/94 ESE     NEW: Do not check credit limits on credit cards.
!!
!![7.4] 03/14/95 ESE    NEW: 7.4 features added.
!!
!![1] 04/26/95 ESE     FIX: autopack did not check past due correctly.
!!
!![2] 05/05/95 HLB      FIX: Past due checking did not work if the
!!                      aging selection was by invoice date.
!!
!![3] 09/06/95 HLB      FIX: If checking past due, the credit limit was being
!!                      "lost" as if the credit limit was zero.
!!
!![4] 12/03/96  ESE   NEW: use the AFR record in case needed for balfwd.
!!
!![5] 12/11/96  ESE   NEW: Add WBWHAP checks.
!!
!!8.0  03/31/97 CAE     New version created
!!
!![6]  03/31/97 CAE     NEW: Changes for year 2000.
!!
!![7] 05/12/97 SLM/ROM  FIX: If customer is "Commit Review" set HDR'HOLD'YN
!!                                                   to "R" so it displays on OE1HDR screen
!!                                                       Requires OECRST.TAB & OECRS2.TAB
!!
!  8.29   09/11/10 ese      Added testing for ALWAYS_ZERO which makes
!                           pre-paids go to credit manager.
!  8.30   10/13/00 slm      Add environment variable SKIP_CREDIT_CHK.
!                           If ARC'INV'DR'YN() = Y and this new env
!                           variable = Y then skip credit checking.
!  8.31   03/19/01 slm      Make it not check credit if a transfer.
!  8.32   05/24/01 bdl      Changed for OEPACK, when credit check fails
!                           and OEC'CR'STAT is set to something other
!                           than 0, change the customer's credit status
!                           (CUS'CR'STAT) to OEC'CR'STAT set in the
!                           control record.
!  8.33   06/15/01 ese      Mass changed for multiple things including
!                           credit change for grace days, lots of PLD
!                           version 55 changes, especially oversize,
!                           some kit changes.
!  8.34   06/20/01 bdl      Recompiled with changes to WCS (either
!                           directly or through a copy) Moved EMAIL to
!                           the bottom and changed from 30 to 80
!                           characters Added an Array to track 5 orders
!                           for each ezid
!  8.35   07/30/04 cae      Do not check credit card order authorizations
!                           Correct totals on final OE screen
!  8.36   08/04/04 cae      Added new security oe1sec if set to "N"
!                           cannot delete orders, fixed problem when
!                           changing credit status on orders and then
!                           going back into order for customers with a 4
!                           or 7 cr'status, fixed compiling problems
!  8.37   09/21/04 cae      If changing order hold to 5 allow the user to
!                           go past the hdr'nam'no field
!  8.38   09/27/04 cae      Corrected duplicate map statments
!  8.39   11/15/04 cae      If cancelling a web order would receive an
!                           error message on new order that WCS record
!                           vanished
!  8.40   05/03/05 cae      Reduce number of variables
!  8.41   08/14/05 cae      Corrections for looking up environment
!                           variables
!  8.42   12/13/05 cae      Corrections for WB credit cards
!  8.43   12/21/05 jmcook   corrected copyrights
!  8.44   12/22/06 cae      Add environment variable to not change credit
!                           status if you manually put an order on type 5
!                           credit hold
!  8.45   01/03/07 cae      Add environmentvariable
!  8.46   07/19/07 ese      test credit limit (not just aging) according
!                           to control record on CODs
!  8.47   12/14/07 cae      Change copyright
!  8.48   12/21/07 cae      Problem when changing from a customer on hold
!                           to a customer not on hold
!  8.49   01/22/08 ese      Changed to use new hdr'sub'no instead of hack
!                           pstid
!  8.50   02/13/08 cae      Check the allow/disallow for terms for
!                           oepack; was a problem with split orders
!  8.51   04/14/08 cae      Changes for WL interface
!  8.52   12/01/08 cae      Don't put transfer on hold
!  8.53   01/13/09 cae      Add environment variable to check the dealer
!                           application field. If set then it checks for
!                           a particular mill and sets the credit flag to
!                           4 if dealer application not set to Y
!  8.54   01/16/09 cae      Changed to check for dealer application
!  8.55   06/29/09 cae      Changes for security for credit cards; only
!                           allow user to change terms if order on credit
!                           hold, fix for invoicing transfer orders
!  8.56   08/10/09 elaine   copyright
!  8.57   08/27/09 cae      Changes for totals only and computation of
!                           credit
!  8.58   08/28/09 ese      Handle total'only only used sometimes
!  8.59   12/29/09 elaine   copyright
!  8.60   06/02/10 cae      Do not check credit on transfers
!  8.61   01/05/11 cae      Copyright
!  8.62   12/28/11 cae      Copyright
!  8.63   01/23/13 cae      Added debug
!  8.64   01/24/13 cae      Added debug for WL
!  8.65   01/29/13 cae      Added debug for WL
!  8.66   10/18/13 cae      Changes to logging
!  8.67   11/12/13 cae      Add debug
!  8.68   06/27/14 ese      Comment out a lot of debug; add debug for
!                           bagger
!  8.69   10/08/14 ese      Changes for bagger mass checkin
!  8.70   05/07/15 cae      Remove debug
!  8.71   01/04/16 ese      Extensive debug added
!  8.72   02/08/16 cae      Remove debug
!  8.73   05/05/17 cae      Don't consider VISA terms as a prepaid
!  8.74   10/31/17 cae      Copyright
!  8.75   09/15/20 cae      Copyright
!  8.76   03/15/21 ese      Remove variables not needed
!  8.77   03/16/21 ese      Reduced not needed maps
!  8.78   11/15/21 cae      Copyright
!  8.79   07/18/25 j5724    Add TIME'IT macro. Delete MBX at begin. Don't
!                           call Malvern twice. Add logs for TIME'IT
!  8.80   07/23/25 j5724    Replace \TIME'IT with \TIME'CSV
!  8.81   08/25/25 j5512    cleaned up time'csv macro from this cpy.
! !!COMMENT-MARKER!!LANGUAGE=BASIC!!    DO NOT REMOVE  DO NOT REMOVE
!!
CIF NOT ERRORS THEN
        SYMBOL ERRORS="Y"
ENDCIF

CIF NOT HOLDS THEN
        SYMBOL HOLDS="Y"
ENDCIF

CIF NOT MY'DO'ERR THEN
    SYMBOL MY'DO'ERR="DO'ERR"
ENDCIF

\COPYF HST APID="AR" MODE="PRI"
!\COPYF DOC APID="AR"
COPYF "DOCFIL[AR]",NOAFR
COPYF "DOCAKY[AR]"
\COPYF TRH APID="AR" MODE="PRI"
COPYF "ATQFIL[CV]",NOAFR
COPYF "ATQTKY[CV]"

MAP1 CHANGED'HOLD'FLAG,B,1
MAP1 CHANGED'HOLD'FLAG'VALUE,S,1
LCL1 DOIT,B,1                                   !![9]
LCL1 TST'LIMIT,B,5
MAP1 V''HOLD'FLAG,B,1
LCL1 TEST'TERMS,S,1
LCL1 BALFWD'DONE,B,3
LCL1 TODAY,F
LCL1 HOSED,B,1
CIF NOT WGA'PRGNAM THEN
    LCL1 ENVSTR,S,17
ENDCIF
LCL1 ORG'HOLD'YN,S,1

COMPUTE'CR'LIMIT:
        CIF MY'PRGNAM = "OEPACK" THEN
            TIME'CSV="COMPUTE'CR'LIMIT:"
             \TIME'CSV
        ENDCIF

        MAP1 OEPACK'NO'CHANGE'FLAG,S,1
        ORG'HOLD'YN = HDR'HOLD'YN
        IF WL'DEBUG # "" THEN
            ENTRY = "Order: "
            \KERROR HDR MODE="D" APID="OE" CNO="N"
            ENTRY = ENTRY + " cmpcrl start compute cr limit hold="+HDR'HOLD'YN+" P="+HDR'PROC+" O="+ORG'HOLD'YN
            CIF MY'PRGNAM = "OE1SHP" THEN
                CALL LOG'ENTRY
            CELSE
                \LABLOGGER ENTRY WL'DEBUG
            ENDCIF
        ENDIF

        IF HDR'PROC > 4 THEN
            HDR'HOLD'YN = "N"
            RETURN        !! Don't check credit if Transfer order
        ENDIF

        CIF SEL'PROG="OEPACK" THEN
            CALL CHECK'DEALER
            IF HOSED # 0 RETURN
        ENDCIF

        CIF SEL'PROG="OEPACK" THEN
            IF WBWHAP = 0 THEN
                \TCOUNT'START TCOUNT "Compute credit limit"
            ENDIF
        ENDCIF

        CIF HOLDS # "N" THEN                            !![5] all this
           HOSED = 0
           CALL CHECK'WBWHAP
           IF HOSED # 0 RETURN
        ENDCIF
        IF HDR'DOC'TYPE >= 1 AND HDR'DOC'TYPE <= 9 THEN
!!Skip credit only documents
                IF ARC'INV'DR'YN(HDR'DOC'TYPE) = "N" RETURN     !!KLUDGE

MAP1 SKIP'CREDIT'CHK,S,1
                XCALL GETENV,"SKIP_CREDIT_CHK",SKIP'CREDIT'CHK
                IF SKIP'CREDIT'CHK # "Y" THEN SKIP'CREDIT'CHK = "N"
                IF ARC'INV'DR'YN(HDR'DOC'TYPE) = "Y" AND SKIP'CREDIT'CHK = "Y"  RETURN     !!KLUDGE

        ENDIF
        DOIT = 0                                !![9]
        CIF SEL'PROG = "OE1HDR" THEN            !![9]
                IF CUS'CR'LIMIT # 0 AND CUS'CR'LIMIT # 99999999999      AND &
                    (HDR'SHP'TOTAL - HDR'SHP'DISC) = 0 AND HDR'PROC < 5 AND &
                                                OEC'CRL'FLAG = 5        AND &
                                        (CUS'FAST'AR > CUS'CR'LIMIT)    THEN
                        DOIT = 1
                ENDIF
        ENDCIF

        IF CUS'CR'LIMIT # 0 AND CUS'CR'LIMIT # 99999999999              AND &
                    (HDR'SHP'TOTAL - HDR'SHP'DISC) > 0 AND HDR'PROC < 5 THEN
                DOIT = 1
        ENDIF

!!If DOIT is '1' then we have to check credit limit
        TRH'KNO = HDR'TRH'NO
        \VUSE TRH VPTR="|TRHPTR|"
        IF TRH'CTL # 0 THEN
            CIF ERRORS # "N" THEN
                ENTRY = "Terms(1) "+TRH'KNO+" not found!"
                CALL |MY'DO'ERR|
            ENDCIF
            CALL HOSE'HOLD'YN
            RETURN                                            !!KLUDGE
        ENDIF
        IF TRH'CTP'NO # 0 THEN
!!Don't check credit limit if credit card
            DOIT = 0
            HOSED = 0
        ELSE
!!Customer skip only applies to non-credit card orders!
            IF CUS'CRL'SKIP = "Y" RETURN                    !!KLUDGE
        ENDIF
        IF OEC'PAST'DUE # "N" AND DOIT = 0 THEN DOIT = 2
!!If DOIT is 2 then we check past due only, not credit limit (credit cards)
!       IF WL'DEBUG # "" THEN
!           ENTRY = "Order: "
!           \KERROR HDR MODE="D" APID="OE" CNO="N"
!           ENTRY = ENTRY + " 2: cmpcrl hold="+HDR'HOLD'YN
!           CIF MY'PRGNAM = "OE1SHP" THEN
!               CALL LOG'ENTRY
!           CELSE
!               \LABLOGGER ENTRY WL'DEBUG
!           ENDCIF
!       ENDIF
        IF CUS'CRL'SKIP # "Y" THEN              !!In case this is credit card
                CASE
                IF CUS'IMAX # 0 THEN
                        TST'LIMIT = CUS'IMAX * 100
                ELSE
                IF TRH'CTL = 0 AND TRH'CTP'NO # 0 THEN
                        TST'LIMIT = ARC'CC'IMAX * 100
                ELSE
                IF TRH'COD'STATUS = 3 THEN
                        DOC'SPACE = SPACE
                        DOC'CUS'NO = CUS'NO
                        \SETKEY DOCA MODE="KEY" APID="AR"
                        RDKEY DOCA
                        GETNXT DOCA
                        DOCA'KEY = DOCA'KEY2
                        IF DOCA'CTL = 0 AND DOCA'KCUS'NO = CUS'NO THEN
                                TST'LIMIT = ARC'NFT'IMAX * 100
                        ELSE
                                TST'LIMIT = ARC'CASH'IMAX * 100
                        ENDIF
                ELSE
                IF TRH'COD'STATUS = 2 THEN
                        TST'LIMIT = ARC'CHK'IMAX * 100
                ELSE
                        TST'LIMIT = ARC'NET'IMAX * 100
                ENDCASE
!!Now check the individual invoice against our individual limit
                IF (HDR'SHP'TOTAL - HDR'SHP'DISC) > TST'LIMIT AND &
                                                TST'LIMIT > 0 AND &
                                                TST'LIMIT < 99999900 THEN
                        CIF ERRORS # "N" THEN
                           ENTRY = "Exceeds invoice limit of: "+((TST'LIMIT/100) USING "###,###.##-")
                           CALL |MY'DO'ERR|
                        ENDCIF
!                       IF WL'DEBUG # "" THEN
!                           ENTRY = "Order: "
!                           \KERROR HDR MODE="D" APID="OE" CNO="N"
!                           ENTRY = ENTRY + " 3: cmpcrl:="+HDR'HOLD'YN+" total="+HDR'SHP'TOTAL+" exceeds limit="+TST'LIMIT
!                           CIF MY'PRGNAM = "OE1SHP" THEN
!                               CALL LOG'ENTRY
!                           CELSE
!                               \LABLOGGER ENTRY WL'DEBUG
!                           ENDCIF
!                       ENDIF
                        CALL HOSE'HOLD'YN
                        DOIT = 0                !!No need for other tests ....
                ELSE
                   IF CUS'CRL'EXPDTT # 0 THEN
                        \CNV'DTT DATE TODAY
                        \CNV'DTT CUS'CRL'EXPDTT YDAYS
                        IF TODAY > YDAYS THEN
                            CIF ERRORS # "N" THEN
                                \STRDTT DTT="CUS'CRL'EXPDTT" DDATE="ENTRY"
                                ENTRY = "Customer credit limit has expired on " + ENTRY
                                CALL |MY'DO'ERR|
                            ENDCIF
!                           IF WL'DEBUG # "" THEN
!                               ENTRY = "Order: "
!                               \KERROR HDR MODE="D" APID="OE" CNO="N"
!                               ENTRY = ENTRY + " 4: cmpcrl:="+HDR'HOLD'YN+" limit expired"
!                               CIF MY'PRGNAM = "OE1SHP" THEN
!                                   CALL LOG'ENTRY
!                               CELSE
!                                   \LABLOGGER ENTRY WL'DEBUG
!                               ENDCIF
!                           ENDIF
                            CALL HOSE'HOLD'YN
                            DOIT = 0                !!No need for other tests ....
                        ENDIF
                   ENDIF
                ENDIF
        ENDIF
        IF DOIT # 0 THEN
                CIF SEL'PROG # "OE1HDR" AND SEL'PROG # "OEPACK" THEN
                        LCL1 CASH'TERMS,S,1
                        \COPYF TRM AR MODE="PRI"
                        IF TRH'NO = HDR'TRH'NO AND TRH'COD'STATUS > 1 THEN
                                CASH'TERMS = "Y"
                        ELSE
                                IF TRM'NO # HDR'TRH'NO THEN
                                        TRM'KNO = HDR'TRH'NO
                                        TRM'KCASE = 0
                                        TRM'KSEQ = 0
                                        USE TRM INTO TRM'REC KEY TRM'KEY
                                        GETNXT TRM INTO TRM'REC
                                !![2]
                                ELSE
                                        TRM'KNO = TRM'NO
                                ENDIF
                                IF TRM'NO # TRM'KNO OR TRM'CTL # 0 OR &
                                                TRM'DUE'DAYS = 0 THEN   
                                        CASH'TERMS = "Y"
                                ELSE
                                        CASH'TERMS = "N"
                                ENDIF
                        ENDIF
                ENDCIF
!!Test terms is used for deciding past due checking ... Y = SKIP past due
!!                                                      N = CHECK past due
                CASE
                IF OEC'COD'NET = "C" THEN
!!If only cash orders checked as COD
                        IF TRH'COD'STATUS = 3 THEN
!!If COD CASH we don't test past due
                                TEST'TERMS = "Y"
                        ELSE
!!Other COD or net we do ...
                                TEST'TERMS = "N"
                        ENDIF
                ELSE
                IF OEC'COD'NET = "N" THEN
!!If nobody checked like COD then we check past due always
                        TEST'TERMS = "N"
                ELSE
!!If everybody checked as COD, then all COD terms skip past due test
!!whether or not cash terms.
                        TEST'TERMS = CASH'TERMS
                ENDCASE
                TST'LIMIT = CUS'CR'LIMIT
                IF OEC'SKIPDOC = "Y" THEN                       !![6]
                        TST'LIMIT = TST'LIMIT - CUS'TMP'AR      !![6]
                ENDIF                                           !![6]

!!Test past due status if not fast method
                CIF SEL'PROG="OEPACK" THEN
                        IF WBWHAP = 0 THEN
                                \TCOUNT TCOUNT
                        ENDIF
                ENDCIF
                IF OEC'CRL'FLAG # 5 OR DOIT = 2 CALL TEST'PAST'DUE !!Because days till due = 0 ...
              IF HOSED = 0 AND DOIT # 2 THEN
!!Now lets check out the credit limit
                CASE
                IF CASH'TERMS = "Y" AND TEST'TERMS # "N" THEN
                        TST'LIMIT = TST'LIMIT - (HDR'SHP'TOTAL - HDR'SHP'DISC) 
                ELSE
                IF OEC'CRL'FLAG = 5 THEN
                        TST'LIMIT = TST'LIMIT - CUS'FAST'AR - &
                                        (HDR'SHP'TOTAL - HDR'SHP'DISC) !![9]  
!!Need to check past due here if fast method
                        IF TST'LIMIT >= 0 CALL TEST'PAST'DUE
                ELSE
                IF OEC'CRL'FLAG = 4 THEN
                        CALL COMPUTE'CR'LIMIT4
                ELSE
                IF OEC'CRL'FLAG = 3 THEN
                        CALL COMPUTE'CR'LIMIT3
                ELSE
                IF OEC'CRL'FLAG = 2 THEN
                        CALL COMPUTE'CR'LIMIT2
                ELSE
                        CALL COMPUTE'CR'LIMIT1
                ENDCASE
                CIF SEL'PROG="OEPACK" THEN
                    IF WBWHAP = 0 THEN
                        \TCOUNT TCOUNT
                    ENDIF
                ENDCIF
                IF TST'LIMIT < 0 THEN
                        CIF ERRORS # "N" THEN
                           ENTRY = "Exceeds credit limit by: "+((-TST'LIMIT/100) USING "###,###.##-")
                           CALL |MY'DO'ERR|
                        ENDCIF
                        IF WL'DEBUG # "" THEN
                            ENTRY = "Order: "
                            \KERROR HDR MODE="D" APID="OE" CNO="N"
                            ENTRY = ENTRY + " 5: cmpcrl:="+HDR'HOLD'YN+" exceeds limit"
                            CIF MY'PRGNAM = "OE1SHP" THEN
                                CALL LOG'ENTRY
                            CELSE
                                \LABLOGGER ENTRY WL'DEBUG
                            ENDCIF
                        ENDIF
                        CALL HOSE'HOLD'YN
                ENDIF
            ENDIF
        ENDIF
        IF HOSED = 0 CALL CHECK'CREDIT'CARD             !!Just the credit card check
        CIF MY'PRGNAM = "OEPACK" THEN
            TIME'CSV="COMPUTE'CR'LIMIT: END"
             \TIME'CSV
        ENDCIF
RETURN

        LCL1 XDAYS,F
        LOCAL RE'DOCP
        COPYF "DOCFIL[AR]",NOAFR
        COPYF "DOCPKY[AR]"
        LCL1 DOC'CASH'TERMS,S,1                 !![2]
        LOCAL TEST'CASH'TERMS                   !![2]
        \COPYF TRM AR MODE="PRI"                !![2]
        \COPYF AGE AR MODE="PRI"                !![2]

LOCAL CHECK'AGING
LOCAL DO'BALFWD

TEST'PAST'DUE:          !!Do not test COD or if control record not set.
                        !!Credit cards likely return as TEST'TERMS = "Y"
                        !!because number of days till due = 0
        IF OEC'PAST'DUE = "N" OR TEST'TERMS = "Y" RETURN        !!KLUDGE
        DOCP'KCUS'NO = CUS'NO
        DOCP'KTYPE = 1
        DMN = 12 : DDY = 31 : DYR = 50
        DOCP'KORG'DDTT = DTT
        DOCP'KORG'NO = 0
        !![2] Next IF
        IF ARC'WHICH'DTT # 4 THEN
                RDKEY DOCP      
        ELSE
                USE DOCP INTO DOC'REC KEY DOCP'KEY
        ENDIF
!![2]   RDKEY DOCP      
     RE'DOCP:
        GETNXT DOCP
        DOCP'KEY = DOCP'KEY2
        IF DOCP'KCUS'NO = CUS'NO AND DOCP'CTL = 0 THEN
                IF ARC'WHICH'DTT # 4 THEN       !![2]
                        DTT = DOCP'KORG'DDTT
                !![2] ELSE
                ELSE
                        \CNVDOC NPRE="AGE'DOC" DPRE="DOC"
                        AGE'KDOC'NO = AGE'DOC'NO
                        AGE'KCASE = 0
                        AGE'KSEQ = 0
                        USE AGE INTO AGE'REC KEY AGE'KEY
                        GETNXT AGE INTO AGE'REC
                        \CHKKEY AGE LASTFIX="DOC'NO" APP="AR"
                        IF AGE'CTL = 0 THEN
                                DTT = AGE'DUE'DTT
                        ELSE
                                DTT = 0
                        ENDIF
                ENDIF
                IF DTT # 0 THEN
                        \CNV'DTT DTT
                ELSE
                        YDAYS = 0
                ENDIF
                IF OC2'ORD'GRACE = 0 THEN
                    XDAYS = YDAYS + ARC'FC'GRACE
                ELSE
                    XDAYS = YDAYS + OC2'ORD'GRACE
                ENDIF
                \CNV'DTT DATE
                IF YDAYS > XDAYS THEN                            !!Past due
                        IF ARC'WHICH'DTT # 4 THEN               !![2]
                                USE DOCP DOCP'RCN INTO DOC'REC
                        ENDIF                                   !![2]
                        IF DOC'AMT <= 0 GOTO RE'DOCP            !!But a credit
                        !![2] Used to always go to RE'DOCP
                        IF DOC'DTT = DOC'ORG'DDTT THEN
                                IF ARC'WHICH'DTT # 4 THEN
                                        GOTO RE'DOCP    !!COD???
                                ELSE    !![16] ELSE all new
                                        CALL TEST'CASH'TERMS                                                                            
                                        IF DOC'CASH'TERMS = "Y" THEN
                                                GOTO RE'DOCP
                                        ENDIF
                                ENDIF
                        ENDIF
!!We have at least one past due non-cod invoice ...
                        DOIT = 0
                        IF OEC'PAST'DUE = "A" OR OEC'PAST'DUE = "D" THEN
!!Now we check the allowances ...
                                CALL CHECK'AGING
                        ENDIF
                        IF DOIT = 0 THEN
                                CIF ERRORS # "N" THEN
                                   ENTRY = "Customer has invoice(s) that are past due!"
                                   CALL |MY'DO'ERR|
                                ENDCIF
!                               IF WL'DEBUG # "" THEN
!                                   ENTRY = "Order: "
!                                   \KERROR HDR MODE="D" APID="OE" CNO="N"
!                                   ENTRY = ENTRY + " 6: cmpcrl:="+HDR'HOLD'YN+" past due"
!                                   CIF MY'PRGNAM = "OE1SHP" THEN
!                                       CALL LOG'ENTRY
!                                   CELSE
!                                       \LABLOGGER ENTRY WL'DEBUG
!                                   ENDCIF
!                               ENDIF
                                CALL HOSE'HOLD'YN
                        ENDIF
                ENDIF
        ENDIF
        !![2] I put this in in case anyone is counting on the TRH
        !!     matching the order.
        IF ARC'WHICH'DTT = 4 AND HDR'TRH'NO # TRH'NO THEN
            TRH'KNO = HDR'TRH'NO
            \VUSE TRH VPTR="|TRHPTR|"
        ENDIF
RETURN
!![10] - above

!![2] This routine
TEST'CASH'TERMS:
        IF TRH'NO # DOC'TRH'NO THEN
            TRH'KNO = DOC'TRH'NO
            \VUSE TRH VPTR="|TRHPTR|"
            IF TRH'CTL # 0 THEN TRH'SPACE = SPACE
        ENDIF
        IF TRH'NO = DOC'TRH'NO AND TRH'COD'STATUS > 1 THEN
            DOC'CASH'TERMS = "Y"
        ELSE
            IF TRM'NO # DOC'TRH'NO THEN
                TRM'KNO = DOC'TRH'NO
                TRM'KCASE = 0
                TRM'KSEQ = 0
                USE TRM INTO TRM'REC KEY TRM'KEY
                GETNXT TRM INTO TRM'REC
            ELSE
                TRM'KNO = TRM'NO
            ENDIF
        ENDIF
        IF TRM'NO # TRM'KNO OR TRM'CTL # 0 OR TRM'DUE'DAYS = 0 THEN     
            DOC'CASH'TERMS = "Y"
        ELSE
            DOC'CASH'TERMS = "N"
        ENDIF
RETURN

HOSE'HOLD'YN:           !![10] - made a subroutine
        IF WL'DEBUG # "" THEN
            ENTRY = "Order: "
            \KERROR HDR MODE="D" APID="OE" CNO="N"
            ENTRY = ENTRY + " hose hold start hold="+HDR'HOLD'YN
            CIF MY'PRGNAM = "OE1SHP" THEN
                CALL LOG'ENTRY
            CELSE
                \LABLOGGER ENTRY WL'DEBUG
            ENDCIF
        ENDIF
        HOSED = 1
!!      CIF SEL'PROG = "OE1HDR" THEN                   !! OLD
        CIF SEL'PROG = "OEPACK" THEN                       !! NEW
           IF (HDR'SHP'TOTAL - HDR'SHP'DISC) > 0 THEN !![9]
                IF OEC'CR'STAT = 0 THEN
                    HDR'HOLD'YN = "7"
                ELSE
                    CUS'KNO = HDR'NAM'NO
                    RDLOCK CUS INTO CUS'REC KEY CUS'KEY
                    IF CUS'CTL = 0 THEN
                        CUS'CR'STAT = OEC'CR'STAT
                        WRITE CUS FROM CUS'REC
                        IF OEC'CR'STAT = 4 OR   &
                                OEC'CR'STAT = 5 THEN
                            HDR'HOLD'YN = "7"
                        ENDIF
                    ELSE
                        HDR'HOLD'YN = "7"
                    ENDIF
                ENDIF
            ENDIF
        CELSE
            HDR'HOLD'YN = "7"
        ENDCIF
        IF WL'DEBUG # "" THEN
            ENTRY = "Order: "
            \KERROR HDR MODE="D" APID="OE" CNO="N"
            ENTRY = ENTRY + " hose hold end hold="+HDR'HOLD'YN+" OEC="+OEC'CR'STAT
            CIF MY'PRGNAM = "OE1SHP" THEN
                CALL LOG'ENTRY
            CELSE
                \LABLOGGER ENTRY WL'DEBUG
            ENDCIF
        ENDIF
RETURN

COMPUTE'CR'LIMIT1:
        HST'SPACE = SPACE
        \SETKEY PRE="HST" APID="AR" MODE="KEY"
        RDKEY HST
        GETNXT HST
        HST'KEY = HST'KEY2
        II = HST'KYEAR
        HST'KYEAR = 128
        RDKEY HST
        GETPRV HST
        JJ = HST'KYEAR
        HST'SPACE = SPACE
        HST'CUS'NO = HDR'NAM'NO
        \SETKEY PRE="HST" APID="AR" MODE="KEY"
        FOR II = II TO JJ
                HST'KYEAR = II
                HST'KTYPE = "O"
                USE HST INTO HST'REC KEY HST'KEY
                IF HST'CTL = 0 THEN
!![4] - below had minus HST'PAY'AMT(I), should be plus.
                        FOR I = 1 TO 13
                                TST'LIMIT = TST'LIMIT -         &
                                        HST'SALE'AMT(I) +       &
                                        HST'PAY'AMT(I)
                        NEXT I                  
                ENDIF
        NEXT II
RETURN

LCL1 BAL'DTT,B,3
MAP1 BALFWD,B,5                 !![5]

COMPUTE'CR'LIMIT2:
        CALL DO'BALFWD
        TST'LIMIT = TST'LIMIT - BALFWD
RETURN

COMPUTE'CR'LIMIT3:
        CALL COMPUTE'CR'LIMIT2
        TST'LIMIT = TST'LIMIT - (HDR'SHP'TOTAL - HDR'SHP'DISC)
RETURN

CIF NOT PACKET THEN
        SYMBOL PACKET="0"
ENDCIF

!!\COPYF HDR OE HDR|PACKET|
COPYF "HDRFIL[OE]",=HDR|PACKET|,NOAFR
COPYF "HDRCKY[OE]",=HDRC|PACKET|

COMPUTE'CR'LIMIT4:
        CALL COMPUTE'CR'LIMIT2
        HDRC|PACKET|'KNAM'NO = HDR'NAM'NO
        HDRC|PACKET|'KLOC'NO = 0
        HDRC|PACKET|'KDOC'TYPE = 0
        HDRC|PACKET|'KNO = 0
        USE HDRC|PACKET| INTO HDR|PACKET|'REC KEY HDRC|PACKET|'KEY
        DO
            GETNXT HDRC|PACKET| INTO HDR|PACKET|'REC
            IF HDRC|PACKET|'CTL # 0 OR HDR|PACKET|'NAM'NO # HDR'NAM'NO EXIT
            TST'LIMIT = TST'LIMIT - (HDR|PACKET|'SHP'TOTAL - HDR|PACKET|'SHP'DISC)
        LOOP
RETURN

CIF HOLDS # "N" THEN

  CIF WBWHAP'CHECK # "Y" THEN
    LCL1 WBWHAP,B,1
  ENDCIF

CHECK'DEALER:
    MAP1 OEPACK'CHECK'DEALER,S,1
    MAP1 FNAME,S,2
    XCALL GETENV,"OEPACK_NO_CHANGE_FLAG",OEPACK'NO'CHANGE'FLAG
    XCALL GETENV,"OEPACK_CHECK_DEALER",OEPACK'CHECK'DEALER

    IF OEPACK'CHECK'DEALER = "Y" THEN
        XCALL GETENV,"MILL_CODE",FNAME
        IF FNAME # "" THEN
            IF CUS'DLR'APP'YN # "Y" THEN
                MAP1 PRHPTR,X,30
                LIN'KHDR'LOC'NO = HDR'LOC'NO
                LIN'KDOC'TYPE = HDR'DOC'TYPE
                LIN'KHDR'NO = HDR'NO
                LIN'KNO = 0
                USE LIN INTO LIN'REC KEY LIN'KEY
                DO
                    GETNXT LIN INTO LIN'REC
                    \CHKKEY LIN APP="OE"
                    IF LIN'CTL # 0 EXIT
                    PRH'KTRK'NO = LIN'TRK'NO
                    \VUSE PRH
                    IF PRH'PART[1;INC'ROT'LEN] = FNAME THEN
                        ENTRY = FNAME + " Dealer Application not on file!"
                        CALL |MY'DO'ERR|
                        CALL CMPCRL'MSG'APPEND
                        IF WL'DEBUG # "" THEN
                            ENTRY = "Order: "
                            \KERROR HDR MODE="D" APID="OE" CNO="N"
                            ENTRY = ENTRY + " 10: cmpcrl No dealer application"
                            CIF MY'PRGNAM = "OE1SHP" THEN
                                CALL LOG'ENTRY
                            CELSE
                                \LABLOGGER ENTRY WL'DEBUG
                            ENDCIF
                        ENDIF
                        CALL HOSE'HOLD'YN
                        DOIT = 0                !! No need for other tests
                        RETURN
                    ENDIF
                LOOP
            ENDIF
        ENDIF
    ENDIF
RETURN

TEST'HOLDS:
    IF HDR'PROC = 6 RETURN            !!KLUDGE
    CIF MY'PRGNAM = "OEPACK" THEN
        TIME'CSV="TEST'HOLDS:"
        \TIME'CSV
    ENDCIF
    IER1 = 1
!![11] - below
    HOSED = 0
    CALL CHECK'WBWHAP               !![5]
    IF HOSED # 0 RETURN         !![5]
    IF HDR'DOC'TYPE >= 1 AND HDR'DOC'TYPE <= 9 THEN
        IF ARC'INV'DR'YN(HDR'DOC'TYPE) = "N" THEN
            CALL CHECK'CREDIT'CARD  !![13]
            RETURN  !!KLUDGE
        ENDIF
    ENDIF
    IF HDR'BO'YN = "S" AND HDR'LIN'BO # 0 THEN
        ENTRY = "Cannot ship until ALL backorders are filled"
        CALL |MY'DO'ERR|
    ENDIF
    CIF SEL'PROG="OEPACK" THEN
        TRH'KNO = HDR'TRH'NO
        \VUSE TRH VPTR="|TRHPTR|"
        IF TRH'CTL # 0 THEN
            CIF ERRORS # "N" THEN
                ENTRY = "Terms(9) "+TRH'KNO+" not found!"
                CALL |MY'DO'ERR|
            ENDCIF
            CALL HOSE'HOLD'YN
            RETURN                                            !!KLUDGE
        ENDIF
        \ALLOW PRE="TRH" ALLOWED="SF" LOC'NO="HDR'LOC'NO"
        IF SF = 1 THEN
            ENTRY = TRH'DESC + " not allowed at this location!"
            CALL |MY'DO'ERR|
            RETURN
        ENDIF
    ENDCIF
    IF HDR'HOLD'YN # "O" THEN
        IF ENVSTR[17;1] = "" THEN
            XCALL GETENV,"OE1WGA_ALWAYS_ZERO",ENTRY
            ENVSTR[17;1] = ENTRY
            IF ENVSTR[17;1] # "Y" THEN ENVSTR[17;1] = "N"
        ENDIF
        IF ENVSTR[17;1] = "Y" THEN
            TRH'KNO = HDR'TRH'NO
            \VUSE TRH VPTR="|TRHPTR|"
            IF TRH'CTL # 0 AND HDR'PROC # 6 THEN
                ENTRY = "Terms(2) "+TRH'KNO+" not found!"
                CALL |MY'DO'ERR|
                IF WL'DEBUG # "" THEN
                    ENTRY = "Order: "
                    \KERROR HDR MODE="D" APID="OE" CNO="N"
                    ENTRY = ENTRY + " 13: cmpcrl setting hold to 7"
                    CIF MY'PRGNAM = "OE1SHP" THEN
                        CALL LOG'ENTRY
                    CELSE
                        \LABLOGGER ENTRY WL'DEBUG
                    ENDCIF
                ENDIF
                HDR'HOLD'YN = "7"
            ENDIF
        ENDIF
        CASE
        IF ENVSTR[17;1] = "Y" AND TRH'RANK = 0 AND TRH'CTP'NO = 0 THEN
            ENTRY = "Pre-paids always need credit approval!"
            CALL |MY'DO'ERR|
            IF WL'DEBUG # "" THEN
                ENTRY = "Order: "
                \KERROR HDR MODE="D" APID="OE" CNO="N"
                ENTRY = ENTRY + " 8: cmpcrl:="+HDR'HOLD'YN+" prepaids need approval changed="+CHANGED'HOLD'FLAG
                CIF MY'PRGNAM = "OE1SHP" THEN
                    CALL LOG'ENTRY
                CELSE
                    \LABLOGGER ENTRY WL'DEBUG
                ENDCIF
            ENDIF
            IF CHANGED'HOLD'FLAG # 1 THEN
                HDR'HOLD'YN = "7"
            ENDIF
        ELSE
        IF HDR'QUOTE'YN = "Y" THEN
            ENTRY = "Cannot ship a quote"
            CALL |MY'DO'ERR|
        ELSE
        IF CUS'CR'STAT = 4 THEN
            CASE
            IF V''HOLD'FLAG = 1 AND CHANGED'HOLD'FLAG = 1 THEN
                IF CHANGED'HOLD'FLAG'VALUE = "Y" THEN
                    ENTRY = "Order on temporary hold; customer on review status"
                ELSE
                    ENTRY = "Order on hold; customer on review status"
                ENDIF
            ELSE
            IF V''HOLD'FLAG = 1 THEN
                ENTRY = "WARNING-Customer is on Review Status.  ORDER WILL NOT SHIP!"
            ELSE
            IF HDR'HOLD'YN = "5" AND OEPACK'NO'CHANGE'FLAG = "Y" THEN
                ENTRY = "Cannot ship - order on hold; customer on review status"
                CHANGED'HOLD'FLAG = 1
            ELSE
                ENTRY = "Cannot ship - customer on review status"
            ENDCASE
            CALL |MY'DO'ERR|
            IF WL'DEBUG # "" THEN
                ENTRY = "Order: "
                \KERROR HDR MODE="D" APID="OE" CNO="N"
                ENTRY = ENTRY + " 14: cmpcrl:="+HDR'HOLD'YN+" changed="+CHANGED'HOLD'FLAG+" OE="+OEPACK'NO'CHANGE'FLAG
                CIF MY'PRGNAM = "OE1SHP" THEN
                    CALL LOG'ENTRY
                CELSE
                    \LABLOGGER ENTRY WL'DEBUG
                ENDCIF
            ENDIF
            IF CHANGED'HOLD'FLAG # 1 THEN
                HDR'HOLD'YN = "4"
            ENDIF
        ELSE
        IF CUS'CR'STAT = 7 THEN
            CASE
            IF V''HOLD'FLAG = 1 AND CHANGED'HOLD'FLAG = 1 THEN
                IF CHANGED'HOLD'FLAG'VALUE = "Y" THEN
                    ENTRY = "Order on temporary hold; customer on review status"
                ELSE
                    ENTRY = "Order on hold; customer on commit review status"
                ENDIF
            ELSE
            IF V''HOLD'FLAG = 1 THEN
                ENTRY = "WARNING-Customer is on Commit Review; order will not ship"
            ELSE
            IF HDR'HOLD'YN = "7" THEN
                ENTRY = "Cannot ship - order on hold/customer on connit review"
                CHANGED'HOLD'FLAG = 1
            ELSE
                ENTRY = "Cannot ship - customer on commit review status"
            ENDCASE
            CALL |MY'DO'ERR|
            IF WL'DEBUG # "" THEN
                ENTRY = "Order: "
                \KERROR HDR MODE="D" APID="OE" CNO="N"
                ENTRY = ENTRY + " 24: cmpcrl:="+HDR'HOLD'YN+" changed="+CHANGED'HOLD'FLAG
                CIF MY'PRGNAM = "OE1SHP" THEN
                    CALL LOG'ENTRY
                CELSE
                    \LABLOGGER ENTRY WL'DEBUG
                ENDCIF
            ENDIF
            IF CHANGED'HOLD'FLAG # 1 THEN
                HDR'HOLD'YN = "R"
            ENDIF
        ELSE
        IF CUS'CR'STAT = 5 THEN
            IF V''HOLD'FLAG = 1 THEN
                ENTRY = "Customer is on HOLD status.  No orders accepted."
            ELSE
                ENTRY = "Cannot ship - customer on hold status"
            ENDIF
            CALL |MY'DO'ERR|
            IF WL'DEBUG # "" THEN
                ENTRY = "Order: "
                \KERROR HDR MODE="D" APID="OE" CNO="N"
                ENTRY = ENTRY + " 15: cmpcrl:="+HDR'HOLD'YN+" Hold changed to 5"
                CIF MY'PRGNAM = "OE1SHP" THEN
                    CALL LOG'ENTRY
                CELSE
                    \LABLOGGER ENTRY WL'DEBUG
                ENDCIF
            ENDIF
            HDR'HOLD'YN = "5"
        ELSE
        IF HDR'HOLD'YN = "5" AND CUS'CR'STAT = 5 THEN
            ENTRY = "Cannot pick/invoice an order on hold status"
            CALL |MY'DO'ERR|
            HDR'HOLD'YN = "5"
        ELSE
        IF HDR'HOLD'YN = "5" AND CUS'CR'STAT # 5 THEN
            HDR'HOLD'YN = ""
            IF WL'DEBUG # "" THEN
                ENTRY = "Order: "
                \KERROR HDR MODE="D" APID="OE" CNO="N"
                ENTRY = ENTRY + " 16: cmpcrl calling compute cr limit"
                CIF MY'PRGNAM = "OE1SHP" THEN
                    CALL LOG'ENTRY
                CELSE
                    \LABLOGGER ENTRY WL'DEBUG
                ENDCIF
            ENDIF
            CALL COMPUTE'CR'LIMIT
        ELSE
        IF HDR'HOLD'YN = "Y" AND WBWHAP = 0 THEN
            ENTRY = "Cannot ship an order that is on HOLD"
            CALL |MY'DO'ERR|
        ELSE
!![12] Moved HOLD'YN into both CIFS 
            CIF SEL'PROG # "OE1HDR" THEN
                HDR'HOLD'YN = "N"
                IF WL'DEBUG # "" THEN
                    ENTRY = "Order: "
                    \KERROR HDR MODE="D" APID="OE" CNO="N"
                    ENTRY = ENTRY + " 17: cmpcrl calling compute cr limit O="+ORG'HOLD'YN
                    CIF MY'PRGNAM = "OE1SHP" THEN
                        CALL LOG'ENTRY
                    CELSE
                        \LABLOGGER ENTRY WL'DEBUG
                    ENDCIF
                ENDIF
                CALL COMPUTE'CR'LIMIT
            CELSE
!![9] - below
                IF OEC'HOLD'COM # "Y" OR OEC'CRL'FLAG = 5 THEN
                    HDR'HOLD'YN = "N"
                    IF WL'DEBUG # "" THEN
                        ENTRY = "Order: "
                        \KERROR HDR MODE="D" APID="OE" CNO="N"
                        ENTRY = ENTRY + " 18: cmpcrl calling compute cr limit O="+ORG'HOLD'YN
                        CIF MY'PRGNAM = "OE1SHP" THEN
                            CALL LOG'ENTRY
                        CELSE
                            \LABLOGGER ENTRY WL'DEBUG
                        ENDCIF
                    ENDIF
                    CALL COMPUTE'CR'LIMIT
                ELSE
                    IF HDR'HOLD'YN = "" THEN
                        IF WL'DEBUG # "" THEN
                            ENTRY = "Order: "
                            \KERROR HDR MODE="D" APID="OE" CNO="N"
                            ENTRY = ENTRY + " 19: cmpcrl setting hold to N O="+ORG'HOLD'YN
                            CIF MY'PRGNAM = "OE1SHP" THEN
                                CALL LOG'ENTRY
                            CELSE
                                \LABLOGGER ENTRY WL'DEBUG
                            ENDCIF
                        ENDIF
                        HDR'HOLD'YN = "N"
                    ENDIF
                ENDIF
            ENDCIF
        ENDCASE
    ENDIF
    IF HDR'CRID = "WBWHAP" AND HDR'HOLD'YN = "N" THEN
        IF WBC'HOLD'YN # "N" THEN
            HDR'HOLD'YN = "Y"                       !!Back on manual hold.
        ELSE
            HDR'HOLD'YN = ORG'HOLD'YN               !!Back on original hold.
        ENDIF
        IF WL'DEBUG # "" THEN
            ENTRY = "Order: "
            \KERROR HDR MODE="D" APID="OE" CNO="N"
            ENTRY = ENTRY + " Back on manual hold "+HDR'HOLD'YN+" O="+ORG'HOLD'YN
            CIF MY'PRGNAM = "OE1SHP" THEN
                CALL LOG'ENTRY
            CELSE
                \LABLOGGER ENTRY WL'DEBUG
            ENDCIF
        ENDIF
    ENDIF
!   IF WL'DEBUG # "" THEN
!       ENTRY = "Order: "
!       \KERROR HDR MODE="D" APID="OE" CNO="N"
!       ENTRY = ENTRY + " final result: "+HDR'HOLD'YN+" CRID="+HDR'CRID+" WBC="+WBC'HOLD'YN+" O="+ORG'HOLD'YN
!       CIF MY'PRGNAM = "OE1SHP" THEN
!           CALL LOG'ENTRY
!       CELSE
!           \LABLOGGER ENTRY WL'DEBUG
!       ENDCIF
!   ENDIF
    CIF MY'PRGNAM = "OEPACK" THEN
        TIME'CSV="TEST'HOLDS: END"
        \TIME'CSV
    ENDCIF
RETURN
ENDCIF

!![13] This subroutine
CHECK'CREDIT'CARD:
    CIF MY'PRGNAM = "OEPACK" THEN
        TIME'CSV="CHECK'CREDIT'CARD:"
         \TIME'CSV
    ENDCIF
        TRH'KNO = HDR'TRH'NO
        \VUSE TRH VPTR="|TRHPTR|"
        IF TRH'CTL = 0 AND TRH'CTP'NO # 0 THEN
                \UCNVATQ AKEY="Y"
                RDLOCK ATQT INTO ATQ'REC KEY ATQT'KEY           
                IF ATQT'CTL = 0 THEN
                    \VERIFY'ATQ
                    WRITE ATQT FROM ATQ'REC
                ELSE
CIF SEL'PROG # "OE1HDR" AND SEL'PROG # "WLPACK" THEN
                    IF ATQT'KSOURCE = "WB" THEN
                        ATQT'KSOURCE = "OE"
                        RDLOCK ATQT INTO ATQ'REC KEY ATQT'KEY
                        IF ATQT'CTL = 0 THEN
                            \VERIFY'ATQ
                            WRITE ATQT FROM ATQ'REC
                        ENDIF
                    ELSE
                        ENTRY = "You must enter Credit Card info before processing!? "
                        \KERROR ATQT MODE="K" CNO="N" APID="CV"
                        CALL |MY'DO'ERR|L
                        IF WL'DEBUG # "" THEN
                            ENTRY = "Order: "
                            \KERROR HDR MODE="D" APID="OE" CNO="N"
                            ENTRY = ENTRY + " 9: cmpcrl:="+HDR'HOLD'YN+" enter cc info"
                            CIF MY'PRGNAM = "OE1SHP" THEN
                                CALL LOG'ENTRY
                            CELSE
                                \LABLOGGER ENTRY WL'DEBUG
                            ENDCIF
                        ENDIF
                        CALL HOSE'HOLD'YN
                    ENDIF
ENDCIF
                ENDIF
        ENDIF
RETURN

LCL1 BALANCE,B,5
LCL1 AA'CR,S,1
LCL1 X,F
\COPYF AGE AR MODE="PRI"
COPYF "AFRFIL[AR]",NOAFR

DO'BALFWD:
        CIF MY'PRGNAM = "OEPACK" THEN
            TIME'CSV="DO'BALFWD:"
            \TIME'CSV
        ENDCIF
        IF BALFWD'DONE # CUS'NO THEN
                IF AFR'RCN # UB'CNO THEN
                        USE AFR UB'CNO INTO AFR'REC
                ENDIF
                BALFWD'DONE = CUS'NO
                IF OEC'PAST'DUE = "A" THEN
                        AA'CR = "Y"
                ELSE
                        AA'CR = "N"
                ENDIF
!!              DMN = 12                !!Would age wrong ...
!!              DDY = 31
!!              DYR = 99
                BAL'DTT = DATE
                \BALFWD BALDTT=BAL'DTT NAMPRE="CUS" POSTFLG="""B""" APID="AR"   &
                        AGE'BUCKETS="Y" BEGIN="Y" CUT'BY'FLG="D" AGEBY="""A"""  &
                        AA'CR=AA'CR
        ENDIF
        CIF MY'PRGNAM = "OEPACK" THEN
            TIME'CSV="DO'BALFWD: END"
             \TIME'CSV
        ENDCIF
RETURN

LCL1 TMP'LIMIT,B,5

CHECK'AGING:
        CIF MY'PRGNAM = "OEPACK" THEN
            TIME'CSV="CHECK'AGING:"
             \TIME'CSV
        ENDCIF
        CALL DO'BALFWD
        BALANCE = AGE'BUCKETS(1)
        FOR X = 2 TO 5
                BALANCE = BALANCE + AGE'BUCKETS(X)
        NEXT X
        X = ARC'PD'MIN * 100
        IF (AGE'BUCKETS(3) <= 0 AND AGE'BUCKETS(4) <= 0                 AND &
                                                AGE'BUCKETS(5) <= 0)    OR  &
                                                BALANCE <= X            THEN
!!If the 60+ buckets are all 0 or if entire past due less than control rec min
!!then maybe we are ok
                IF AGE'BUCKETS(2) <= X OR (BALANCE <= X         AND &
                                                OEC'PAST'DUE = "A") THEN
                        DOIT = 1
!!we are ok because 1st bucket less than control rec or balance is
                ELSE
                        IF CUS'PERC'PASTD = 0 THEN
                                TMP'LIMIT = ARC'PERC'PASTD
                        ELSE
                                TMP'LIMIT = CUS'PERC'PASTD
                        ENDIF
                        TMP'LIMIT = (TMP'LIMIT * .01) * BALANCE
                        IF AGE'BUCKETS(2) <= TMP'LIMIT THEN
                                DOIT = 1                
!!we are ok because we computed a limit we slipped under
                        ENDIF
                ENDIF
        ENDIF
!!if DOIT is returned as non-zero then this customer is ok!
        CIF MY'PRGNAM = "OEPACK" THEN
            TIME'CSV="CHECK'AGING: END"
             \TIME'CSV
        ENDCIF
RETURN

COPYF "WBCFIL[WB]",NOAFR
\COPYF WCS WB BAD1="WCSN"

CHECK'WBWHAP:                                                           !![5] new
    CIF MY'PRGNAM = "OEPACK" THEN
        TIME'CSV="CHECK'WBWHAP:"
         \TIME'CSV
    ENDCIF
    IF HDR'CRID = "WBWHAP" AND HDR'SOURCE = "OE" RETURN !!KLUDGE
    IF HDR'CRID # "WBWHAP" RETURN                       !!KLUDGE
    CIF SEL'PROG="OEPACK" OR SEL'PROG = "OE1SHP" THEN
        SYMBOL TTO = "TOTALS'ONLY"
    CELSE
        SYMBOL TTO="""N"""
    ENDCIF
    IF HDR'CRID = "WBWHAP" AND |TTO| = "Y" AND HDR'SUB'NO = 1 RETURN !!KLUDGE
    IF HDR'HOLD'YN = "O" RETURN                     !!KLUDGE
    IF WBC'RCN # UB'CNO THEN
        USE WBC UB'CNO INTO WBC'REC
        IF WBC'CTL # 0 THEN
            ENTRY = "Invalid WBC control record for "+UB'CNO + "!"
            CALL |MY'DO'ERR|
            GOTO HOSE'HOLD'YN                               !!KLUDGE
        ENDIF
    ENDIF
!!  IF WBC'HOLD'YN # "N" THEN
!!              ENTRY = "WEB order automatically placed on hold!"
!!      CALL |MY'DO'ERR|
!!      GOTO HOSE'HOLD'YN                               !!KLUDGE
!!  ENDIF
    WCSS'KSUB'NO = HDR'SUB'NO
    USE WCSS INTO WCS'REC KEY WCSS'KEY
    IF WCSS'CTL # 0 THEN
        ENTRY = "WCS record vanished! "+WCSS'KSUB'NO+" in cmpcrl.cpy"
        CALL |MY'DO'ERR|
        IF WL'DEBUG # "" THEN
            ENTRY = "Order: "
            \KERROR HDR MODE="D" APID="OE" CNO="N"
            ENTRY = ENTRY + " cmpcrl wcs vanished"
            CIF MY'PRGNAM = "OE1SHP" THEN
                CALL LOG'ENTRY
            CELSE
                \LABLOGGER ENTRY WL'DEBUG
            ENDCIF
        ENDIF
        GOTO HOSE'HOLD'YN                               !!KLUDGE
    ENDIF
    IF WCS'ACTIVE # "Y" THEN
        ENTRY = "WCS record no longer shows active!"
        CALL |MY'DO'ERR|
        IF WL'DEBUG # "" THEN
            ENTRY = "Order: "
            \KERROR HDR MODE="D" APID="OE" CNO="N"
            ENTRY = ENTRY + " cmpcrl wcs not active"
            CIF MY'PRGNAM = "OE1SHP" THEN
                CALL LOG'ENTRY
            CELSE
                \LABLOGGER ENTRY WL'DEBUG
            ENDCIF
        ENDIF
        GOTO HOSE'HOLD'YN                               !!KLUDGE
    ENDIF
    IF WCS'HOLD'FLAG # "N" AND WCS'HOLD'FLAG # "" THEN
        IF HDR'HOLD'YN # "O" THEN
            ENTRY = "WCS hold flag "+WCS'HOLD'FLAG+" is set!"
            CALL |MY'DO'ERR|
            IF WL'DEBUG # "" THEN
                ENTRY = "Order: "
                \KERROR HDR MODE="D" APID="OE" CNO="N"
                ENTRY = ENTRY + " wcs hold flag set"
                CIF MY'PRGNAM = "OE1SHP" THEN
                    CALL LOG'ENTRY
                CELSE
                    \LABLOGGER ENTRY WL'DEBUG
                ENDCIF
            ENDIF
            GOTO HOSE'HOLD'YN                               !!KLUDGE
        ENDIF
    ENDIF
RETURN

CMPCRL'MSG'APPEND:
    MSG'DID = "OEMSG"
    IF HDR'MSGNO = 0 THEN
        MSG'SPACE = SPACE
        MSG'DATA = ENTRY
        ADD MSG
        HDR'MSGNO = MSG'RCN
    ELSE
        USE MSG HDR'MSGNO INTO MSG'REC
        DO WHILE MSG'CH # 0 AND MSG'CTL = 0
            USE MSG MSG'CH INTO MSG'REC
        LOOP
        MM = MSG'RCN
        MSG'SPACE = SPACE
        MSG'DATA = ENTRY
        ADD MSG
        NN = MSG'RCN
        RDLOCK MSG MM INTO MSG'REC
        MSG'CH = NN
        WRITE MSG
        MSG'RCN = NN        !!So we leave routine w/correct MSG'RCN
    ENDIF
RETURN

MAP1 MM,F
MAP1 NN,F


