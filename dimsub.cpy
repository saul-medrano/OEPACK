! @#$^&* 433 0 484 0 0 0 0 0 0 0 0 0 0 *&^$#@ 
!* Updated on 02-Oct-95 at 4:51 PM by Cheryl Eberhard; edit time: 22:47:20
!!DIMSUB,8.0
!!Shipping subroutines
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
!!     Model program VUESUB written by:
!!
!!              Eric S. Eberhard and Kenneth J. Shook
!!              May 2, 1985 - June 30, 1985, and on and on and on ...
!!
!!
!!
!!   COPYRIGHT 1985, 1986 by VERTICAL INTEGRATED COMPUTER SYSTEMS (V.I.C.S.)
!!                              ALL RIGHTS RESERVED WORLDWIDE
!!                              all programs written with this model belong
!!                              to Vertical Integrated Computer Systems 
!!                              unless otherwise licensed by V.I.C.S.
!!
!!
!!      Copied from VUETRX.MOD by Eric S. Eberhard and Ken J. Shook
!!

        MAP1    TAMT,B,5
        LCL1    SPOT,F
        MAP1    DIM'INDICATOR,S,1
        MAP1    CHG'TO'VIA'NO,B,2
        MAP1    ASTERISK'SET,S,1
        MAP1    OVERSIZE'BOX,B,2
        MAP1    CUBIC'SIZE,B,2
        MAP1    DIM'WEIGHT,B,2
        MAP1    LARGE'SIZE,B,2
        MAP1    NEW'SWGT,B,5
        MAP1    MANIFEST'WEIGHT,B,2

E'USPS'WGT'RECTANGLE:
    \TSTF
    CASE
    IF VIA'MTYPE = "G" AND VIA'MAN'YN = "G" THEN !!Global Express
        \TABEDTC INBAL1 WGT'RECTANGLE APID="IN" DSPLEN="14"
    ELSE
    IF VIA'MTYPE = "G" AND VIA'MAN'YN = "E" THEN  !!Express
        \TABEDTC INBAL4 WGT'RECTANGLE APID="IN" DSPLEN="14"
    ELSE
    IF VIA'MTYPE = "G" AND VIA'MAN'YN = "P" THEN  !!Priority
        \TABEDTC INBAL2 WGT'RECTANGLE APID="IN" DSPLEN="14"
    ELSE
    IF VIA'MTYPE = "E" AND VIA'MAN'YN = "P" OR VIA'MAN'YN = "U" OR &
                           VIA'MAN'YN = "F" OR VIA'MAN'YN = "S" OR &
                           VIA'MAN'YN = "R" OR VIA'MAN'YN = "T" OR &
                           VIA'MAN'YN = "X" OR VIA'MAN'YN = "Y" THEN
                      !!Endicia Priority !! Endicia Domestic
        \TABEDTC INENE3 WGT'RECTANGLE APID="IN" DSPLEN="20"
        IF WGT'RECTANGLE = "E" OR WGT'RECTANGLE = "X" OR WGT'RECTANGLE = "N" OR &
                WGT'RECTANGLE = "M" OR WGT'RECTANGLE = "O" OR WGT'RECTANGLE = "C" OR &
                WGT'RECTANGLE = "Y" OR WGT'RECTANGLE = "S" THEN
            BOX'LENGTH = 0
            BOX'WIDTH = 0
            BOX'HEIGHT = 0
            BOX'GIRTH = 0
            WGT'D'YN = "N"
            \GAFR PACK11 AFR="X:BOX'WIDTH+X:BOX'HEIGHT+X:BOX'GIRTH"
            \GAFR PACK11 AFR="X:WGT'D'YN+X:BOX'LENGTH"
        ENDIF
    ELSE
    IF VIA'MTYPE = "E" AND VIA'MAN'YN = "E" AND VIA'INT'YN = "N" THEN
                                                  !!Endicia Express Domestic
        \TABEDTC INENE2 WGT'RECTANGLE APID="IN" DSPLEN="20"
        IF WGT'RECTANGLE = "L" OR WGT'RECTANGLE = "M" OR WGT'RECTANGLE = "X" THEN
            BOX'LENGTH = 0
            BOX'WIDTH = 0
            BOX'HEIGHT = 0
            BOX'GIRTH = 0
            WGT'D'YN = "N"
            \GAFR PACK11 AFR="X:BOX'WIDTH+X:BOX'HEIGHT+X:BOX'GIRTH"
            \GAFR PACK11 AFR="X:WGT'D'YN+X:BOX'LENGTH"
        ENDIF
    ELSE
    IF VIA'MTYPE = "E" AND VIA'MAN'YN = "1" THEN  !!Endicia First Class
        \TABEDTC INENFR WGT'RECTANGLE APID="IN" DSPLEN="20"
        IF WGT'RECTANGLE = "C" OR WGT'RECTANGLE = "L" THEN
            BOX'LENGTH = 0
            BOX'WIDTH = 0
            BOX'HEIGHT = 0
            BOX'GIRTH = 0
            WGT'D'YN = "N"
            \GAFR PACK11 AFR="X:BOX'WIDTH+X:BOX'HEIGHT+X:BOX'GIRTH"
            \GAFR PACK11 AFR="X:WGT'D'YN+X:BOX'LENGTH"
        ENDIF
    ELSE
    IF VIA'MTYPE = "E" AND VIA'MAN'YN = "D" THEN    !!Endicia Parcel Post
        \TABEDTC INENE4 WGT'RECTANGLE APID="IN" DSPLEN="20"
    ELSE
    IF VIA'MTYPE = "E" AND VIA'MAN'YN = "E" AND VIA'INT'YN = "Y" THEN
                                                   !!Endicia Express Intnl
        \TABEDTC INENE1 WGT'RECTANGLE APID="IN" DSPLEN="20"
    ELSE
       \TABEDTC INBALO WGT'RECTANGLE APID="IN" DSPLEN="14"
    ENDCASE
RETURN

E'USPS'BOX'LENGTH:
    \TSTF
    IF VIA'MTYPE = "G" THEN
        IF EFCHG # 0 THEN
            IF BOX'LENGTH = 0 THEN
                BOX'WIDTH = 0
                BOX'HEIGHT = 0
                BOX'GIRTH = 0
                WGT'D'YN = "N"
                \GAFR PACK11 AFR="X:BOX'WIDTH+X:BOX'HEIGHT+X:BOX'GIRTH"
                \GAFR PACK11 AFR="E:WGT'D'YN"
                \DDBEDT SCREEN="PACK11" SINX="Y" DDBEDT="SAV'PACKS"
            ELSE
                IF BOX'LENGTH > 108 THEN
                    ENTRY = "Maximum length exceeded"
                    CALL DO'ERR
                ENDIF
                \GAFR PACK11 AFR="E:BOX'WIDTH+E:BOX'HEIGHT+E:BOX'GIRTH"
            ENDIF
        ENDIF
    ELSE
        IF EFCHG # 0 THEN
            IF WGT'RECTANGLE = "F" OR WGT'RECTANGLE = "P" THEN
                IF BOX'LENGTH = 0 THEN
                    ENTRY = "You must enter box length for Endicia!"
                    CALL DO'ERR
                    BOX'GIRTH = 0
                    WGT'D'YN = "N"
                    \GAFR PACK11 AFR="E:BOX'WIDTH+E:BOX'HEIGHT+X:BOX'GIRTH"
                    \GAFR PACK11 AFR="X:WGT'D'YN"
                    \DDBEDT SCREEN="PACK11" SINX="Y" DDBEDT="SAV'PACKS"
                ELSE
                    IF BOX'LENGTH > 108 AND HDR'SHP'COU = "US" THEN
                        ENTRY = "Maximum length exceeded"
                        GOTO DO'ERR
                    ELSE
                        CALL CHECK'COUNTRY'LENGTH'LIMITATIONS
                        IF SF = 1 THEN
                            GOTO DO'ERR
                        ENDIF
                    ENDIF
                    BOX'GIRTH = 0
                    WGT'D'YN = "N"
                    \GAFR PACK11 AFR="E:BOX'WIDTH+E:BOX'HEIGHT+X:BOX'GIRTH"
                    \GAFR PACK11 AFR="X:WGT'D'YN"
                ENDIF
            ELSE
                BOX'LENGTH = 0
                BOX'WIDTH = 0
                BOX'HEIGHT = 0
                BOX'GIRTH = 0
                WGT'D'YN = "N"
                \GAFR PACK11 AFR="X:BOX'WIDTH+X:BOX'HEIGHT+X:BOX'GIRTH"
                \GAFR PACK11 AFR="X:WGT'D'YN+X:BOX'LENGTH"
            ENDIF
        ENDIF
        IF (VIA'MAN'YN = "1" AND WGT'RECTANGLE = "P") OR &
                   WGT'RECTANGLE = "F" OR WGT'RECTANGLE = "P" THEN
            IF BOX'LENGTH = 0
                ENTRY = "You must enter box length for Endicia!"
                CALL DO'ERR
            ENDIF
        ENDIF
    ENDIF
RETURN

E'USPS'BOX'WIDTH:
    \TSTF
    IF VIA'MTYPE = "G" THEN
        IF BOX'LENGTH # 0 AND BOX'WIDTH = 0 THEN
            ENTRY = "You must enter width to compute dimensional weight"
            GOTO DO'ERR
        ENDIF
        IF BOX'WIDTH > BOX'LENGTH THEN
            ENTRY = "The length must be the longest side of a package"
            GOTO DO'ERR
        ENDIF
        IF BOX'WIDTH > 0 THEN
            \GAFR PACK11 AFR="E:BOX'HEIGHT+X:BOX'GIRTH"
        ELSE
            \GAFR PACK11 AFR="X:BOX'HEIGHT+E:BOX'GIRTH"
        ENDIF
    ELSE
        IF BOX'LENGTH # 0 AND BOX'WIDTH = 0 THEN
            ENTRY = "You must enter width for Endicia!"
            GOTO DO'ERR
        ENDIF
        IF BOX'WIDTH > BOX'LENGTH THEN
            ENTRY = "The length must be the longest side of a package"
            GOTO DO'ERR
        ENDIF
        IF BOX'WIDTH > 0 THEN
            IF HDR'SHP'COU # "US" THEN
                CALL CHECK'COUNTRY'WIDTH'LIMITATIONS
                IF SF = 1 THEN
                    CALL DO'ERR
                ENDIF
            ENDIF
            \GAFR PACK11 AFR="E:BOX'HEIGHT+X:BOX'GIRTH+X:WGT'D'YN"
        ENDIF
    ENDIF
RETURN

E'USPS'BOX'HEIGHT:
    \TSTF
    IF VIA'MTYPE = "G" THEN
        IF BOX'HEIGHT = 0 THEN
            ENTRY = "You must enter height to compute dimensional weight"
            GOTO DO'ERR
        ELSE
            IF BOX'HEIGHT > BOX'LENGTH THEN
                ENTRY = "The length must be the longest side of a package"
                GOTO DO'ERR
            ENDIF
            BOX'GIRTH = (BOX'HEIGHT * 2) + (BOX'WIDTH * 2)
            CALL COMPUTE'OS'STUFF   !!place !!real
        ENDIF
    ELSE
        IF BOX'HEIGHT = 0 THEN
            ENTRY = "You must enter height for Endicia!"
            GOTO DO'ERR
        ELSE
            IF BOX'HEIGHT > BOX'LENGTH THEN
                ENTRY = "The length must be the longest side of a package"
                GOTO DO'ERR
            ENDIF
        ENDIF
        IF HDR'SHP'COU # "US" THEN
            CALL CHECK'COUNTRY'HEIGHT'LIMITATIONS
            IF SF = 1 THEN
                CALL DO'ERR
            ENDIF
            CALL CHECK'COUNTRY'SIZE'LIMITATIONS
            IF SF = 1 THEN
                CALL DO'ERR
            ENDIF
        ENDIF
    ENDIF
RETURN

E'USPS'BOX'GIRTH:
    \TSTF
    IF BOX'GIRTH = 0 THEN
        ENTRY = "You need a girth to continue (or height and width)"
        GOTO DO'ERR
    ENDIF
    CALL COMPUTE'OS'STUFF    !!place  !!real
RETURN

E'FEDEX'RECTANGLE:
    \TSTF
    WGT'RECTANGLE = "1"
    \TABEDTC FEDEX WGT'RECTANGLE APID="IN" DSPLEN="25"
RETURN

MAP1 COMPUTE'OS,B,1

MAP1 UPS'INTERNATIONAL,B,1

GET'INTERNATIONAL:
    UPS'INTERNATIONAL = 0                    !!Domestic
    IF UCS(HDR'SHP'COU) # "US" AND     &
        UCS(HDR'SHP'COU) # "PR" AND UCS(HDR'SHP'COU) # "VI" AND &
        UCS(HDR'SHP'COU) # "AS" AND UCS(HDR'SHP'COU) # "GU" AND &
        UCS(HDR'SHP'COU) # "MH" AND UCS(HDR'SHP'COU) # "FM" AND &
        UCS(HDR'SHP'COU) # "MP" AND UCS(HDR'SHP'COU) # "PW" THEN
                    UPS'INTERNATIONAL  = 1  !!International
    ENDIF
RETURN

CIF NOT MY'PRGNAM OR MY'PRGNAM # "OE1SHP" THEN

!!GET'LRG'PKG:             !!Sets NEW'SWGT and AMT1 is billing charge
    GET'LARGE'PKG:             !!place  !!cache
        \DG GETVALUE _UPSETUP UPS_Method ENTRY
        IF DG'CTL = 0 AND ENTRY[1;1] = "O" THEN
            RETURN                        !!KLUDGE
        ENDIF
        AMT1 = 0
        NEW'SWGT = 0
!!Large package must be 90 (soft coded) lbs minimum)  DOLLAR PART ONLY

!!SET BITS AND WGT FLAG IF NEEDED AND CLEAR ADDCHG BITS AND WGT FLAG
        VIA'KNO = HDR'VIA'NO
        \VUSE VIA
        ENTRY = "UPS_LargePackage_"+STR(VIA'FTBL'NO)+"_"+AZONE(1)[-1,-1]+"."
        SENTRY = ENTRY
        CALL GET'USE'XML        !!Makes reference ADDCHG
        IF DG'CTL = 0 THEN
            \DG GETVALUE ADDCHG minBillableWeight NEW'SWGT
            IF DG'CTL = 0 AND NEW'SWGT # 0 THEN
                \DG GETVALUE ADDCHG length_plus_girth.value ENTRY
                SPOT = BLENGTH+((BWIDTH+BHEIGHT)*2)
                IF SPOT > VAL(ENTRY) THEN
                    \DG GETVALUE ADDCHG length_plus_girth.amt AMT1
                ENDIF
                IF AMT1 = 0 THEN
                    \DG GETVALUE ADDCHG length.value ENTRY
                    IF BLENGTH > VAL(ENTRY) THEN
                        \DG GETVALUE ADDCHG length.amt AMT1
                    ENDIF
                ENDIF
                NEW'SWGT = NEW'SWGT*100
            ENDIF
        ENDIF
        IF AMT1 = 0 THEN
            NEW'SWGT = 0
        ENDIF
    RETURN

    MAP1 SAV'SWGT,B,1

    MAP1 LARGE'PKG'AMT,B,5
    MAP1 LARGE'PKG'BNO,B,5
    MAP1 ADD'LARGE'PKG'AMT,B,1

    GET'ADDCHG'AMT1:  !!place !!cache
                          !!gets dollars and weights for large and additional
                          !!sets WGT fields

        AMT1 = 0
        CALL GET'LARGE'PKG          !!To get dollars in AMT1 only (leave wgt alone)
                                    !!Also uses correct VIA record
        IF LARGE'PKG'BNO # BNO AND ADD'LARGE'PKG'AMT # 0 THEN
!!Now we are getting sick
            LARGE'PKG'AMT = LARGE'PKG'AMT + AMT1 !!Save this amount
            LARGE'PKG'BNO = BNO
        ENDIF
        ADD'LARGE'PKG'AMT = 0
        IF AMT1 = 0 THEN
            WGT'LGE'PKG'YN = "N"
        ELSE
            WGT'LGE'PKG'YN = "Y"
            WGT'AC'YN = "N"
            RETURN               !!KLUDGE -- if large, no additional handling
        ENDIF

        ENTRY = "UPS_AdditionalHandling_"+STR(VIA'FTBL'NO)+"_"+AZONE(1)[-1,-1]+"."
        SENTRY = ENTRY
        AMT1 = 0
        SF = 1                !!The order we are looking for
        CALL GET'ADDCHG
        IF AMT1 = 0 THEN      !!No charge for order 1
            SF = 2
            CALL GET'ADDCHG
            IF AMT1 = 0 THEN    !!Nor order 2
                SF = 3
                CALL GET'ADDCHG
                IF AMT1 = 0 THEN    !!Nor order 3
                    SF = 4
                    CALL GET'ADDCHG
                ENDIF
            ENDIF
        ENDIF
        IF AMT1 = 0 THEN
            WGT'AC'YN = "N"
        ELSE
            WGT'AC'YN = "Y"
        ENDIF
        SF = 0
    RETURN

    GET'USE'XML:
        ENTRY = SENTRY[1,-2]
        \DG GETCHILD _UPSETUP ENTRY ADDCHG TAGVAR="Y"
        IF DG'CTL # 0 THEN
            RETURN                                            !!KLUDGE
        ENDIF
        \DG GETVALUE ADDCHG use ENTRY
     GET'USE'XML2:
        IF DG'CTL = 0 THEN
            SPOT = INSTR(1,SENTRY,"_")
            SPOT = INSTR(SPOT+1,SENTRY,"_")
            ENTRY = SENTRY[1,SPOT]+ENTRY+"."
            SENTRY = ENTRY
            ENTRY = SENTRY[1,-2]
            \DG GETCHILD _UPSETUP ENTRY ADDCHG TAGVAR="Y"
            IF DG'CTL # 0 THEN
                ENTRY = SENTRY + " from " + ENTRY + " not found!"
                CALL DO'ERRL
                GOTO DEAD'ABORT
            ENDIF
            \DG GETVALUE ADDCHG use ENTRY
            IF DG'CTL = 0 THEN
                GOTO GET'USE'XML2
            ENDIF
        ENDIF
        DG'CTL = 0
    RETURN

    GET'ADDCHG:           !!place  !!cache
        CALL GET'USE'XML
        \DG GETVALUE ADDCHG weight.order ENTRY
        IF DG'CTL = 0 AND ENTRY = STR(SF) THEN
            CALL ADDCHG'WEIGHT
            IF AMT1 # 0 RETURN                                !!KLUDGE
        ENDIF
        \DG GETVALUE ADDCHG girth.order ENTRY
        IF DG'CTL = 0 AND ENTRY = STR(SF) THEN
            CALL ADDCHG'GIRTH
            IF AMT1 # 0 RETURN                                !!KLUDGE
        ENDIF
        \DG GETVALUE ADDCHG length.order ENTRY
        IF DG'CTL = 0 AND ENTRY = STR(SF) THEN
            CALL ADDCHG'LENGTH
            IF AMT1 # 0 RETURN                                !!KLUDGE
        ENDIF
        \DG GETVALUE ADDCHG width.order ENTRY
        IF DG'CTL = 0 AND ENTRY = STR(SF) THEN
            CALL ADDCHG'WIDTH
!!          IF AMT1 # 0 RETURN                                !!KLUDGE
        ENDIF
    RETURN

    ADDCHG'WEIGHT:
        \DG GETVALUE _UPSETUP UPS.wgtadj WGT'ADJ    !!For scale error
!!      TAMT = INT((BCWEIGHT/100) + .99)*100            !!Original
        TAMT = INT(((BCWEIGHT/100)-WGT'ADJ)+.99)*100
        \DG GETVALUE ADDCHG weight.max ENTRY
        IF DG'CTL = 0 THEN
            IF TAMT > (VAL(ENTRY)*100) THEN
                \DG GETVALUE ADDCHG weight.amt AMT1
            ENDIF
        ENDIF
    RETURN

    ADDCHG'GIRTH:
        TAMT = ((BHEIGHT * 2) + (BWIDTH * 2)) + BLENGTH
        \DG GETVALUE ADDCHG girth.max ENTRY
        IF DG'CTL = 0 THEN
            IF TAMT > VAL(ENTRY) THEN
                \DG GETVALUE ADDCHG girth.amt AMT1
            ENDIF
        ENDIF
    RETURN

    ADDCHG'LENGTH:
        \DG GETVALUE ADDCHG length.max ENTRY
        IF DG'CTL = 0 THEN
            IF BLENGTH > VAL(ENTRY) THEN
                \DG GETVALUE ADDCHG length.amt AMT1
            ENDIF
        ENDIF
    RETURN

    ADDCHG'WIDTH:
        \DG GETVALUE ADDCHG width.max ENTRY
        IF DG'CTL = 0 THEN
            IF BWIDTH > VAL(ENTRY) THEN
                \DG GETVALUE ADDCHG width.amt AMT1
            ENDIF
        ENDIF
    RETURN

    MAP1 BITFLD,F

    SET'WGT'AC'YN:    !!place !!cache  !!only to get WGT'AC'YN set
        IF VIA'MTYPE # "U" OR VIA'CHG'TYPE = "TPB" THEN
            WGT'AC'YN = "N"
            RETURN                                    !!KLUDGE
        ENDIF
        IF II # 1 THEN
            RETURN                                !!KLUDGE
        ENDIF
        \DG GETVALUE _UPSETUP UPS_Method ENTRY
        IF DG'CTL = 0 AND ENTRY[1;1] # "O" THEN
            WGT'AC'YN = "N"
            CALL GET'ADDCHG'AMT1            !!weight, amt, WGT all set in here
        ENDIF
        IF AMT1 = 0 THEN
            \DG GETVALUE _UPSETUP UPS_Method ENTRY
            IF DG'CTL = 0 AND (ENTRY[1;1] = "O" OR ENTRY[1;1] = "B") THEN
                CALL FIND'TBL
                AMT1 = TBL'ADD'HAN
                IF AMT1 # 0 THEN
                    WGT'AC'YN = "Y"
                ENDIF
            ENDIF
        ENDIF
    RETURN
ENDCIF

E'WGT'DIM'YN:
    IF WGT'D'YN = "Y" THEN
        IF VIA'MTYPE = "U" AND (VIA'MAN'YN = "N" OR VIA'MAN'YN = "L" &
                OR VIA'MAN'YN = "E" OR VIA'MAN'YN = "G" OR VIA'MAN'YN = "O" &
                OR VIA'MAN'YN = "T") THEN
            ENTRY = "Dimensional only valid for Air/Ground Services"
            CALL DO'ERR
            WGT'D'YN = "N"
            \DDBEDT SCREEN="PACK12" SINX="Y" DDBEDT="SAV'PACKS"
        ELSE
            IF VIA'MTYPE = "F" AND (WGT'AC'YN = "01" OR WGT'AC'YN = "02" &
                    OR WGT'AC'YN = "04" OR WGT'AC'YN = "05") THEN
                ENTRY = "Dimensional only valid for boxes"
                CALL DO'ERR
                WGT'D'YN = "N"
                \DDBEDT SCREEN="PACK09" SINX="Y" DDBEDT="SAV'PACKS"
            ENDIF
        ENDIF
    ENDIF
RETURN

DETERMINE'WEIGHT'FOR'BILLING:            !!wtf - do this too
    CASE
    IF WGT'LGE'PKG'YN = "Y" THEN !! Only UPS and FedEx
        IF BOX'WEIGHT < DIM'WEIGHT THEN
            NEW'SWGT = INT((DIM'WEIGHT + 99)/100) MAX 90
            WGT'D'YN = "Y"
        ELSE
            NEW'SWGT = BOX'WEIGHT
            WGT'D'YN = "Y"
        ENDIF
    ELSE
    IF BOX'WEIGHT < DIM'WEIGHT THEN
        NEW'SWGT = INT((DIM'WEIGHT + 99)/100)
        WGT'D'YN = "Y"                    !!Dimensional
    ELSE
        IF VIA'1Z'SERV = "YH" OR VIA'1Z'SERV = "YN" OR VIA'1Z'SERV = "YP" THEN
            NEW'SWGT = (BOX'WEIGHT * 16)/100
        ELSE
            NEW'SWGT = INT((BOX'WEIGHT + 99)/100)
            WGT'D'YN = "N"                    !!Not Dimensional
        ENDIF
    ENDCASE
    BOX'SWGT = NEW'SWGT
    CALL SET'BOX'CHARGES'FROM'WGT'REAL !!Set bits from real   !!place  !!real
RETURN

DETERMINE'DIM'RATE:
    IF UPC'RCN = 0 THEN
        ENTRY = "Program error: your program needs to look up UPC"
        GOTO ABORT
    ENDIF
    CASE
!!ground, ground 100 weight
    IF VIA'MTYPE = "U" AND (VIA'MAN'YN = "C" OR VIA'MAN'YN = "H") THEN
        CASE
        IF BOX'ZONE = "2" THEN
            CALC'TBL'AMT = ABS(BOX'SWGT) * UPC'GND'ZONE2D
        ELSE
        IF BOX'ZONE = "3" THEN
            CALC'TBL'AMT = ABS(BOX'SWGT) * UPC'GND'ZONE3D
        ELSE
        IF BOX'ZONE = "4" THEN
            CALC'TBL'AMT = ABS(BOX'SWGT) * UPC'GND'ZONE4D
        ELSE
        IF BOX'ZONE = "5" THEN
            CALC'TBL'AMT = ABS(BOX'SWGT) * UPC'GND'ZONE5D
        ELSE
        IF BOX'ZONE = "6" THEN
            CALC'TBL'AMT = ABS(BOX'SWGT) * UPC'GND'ZONE6D
        ELSE
        IF BOX'ZONE = "7" THEN
            CALC'TBL'AMT = ABS(BOX'SWGT) * UPC'GND'ZONE7D
        ELSE
        IF BOX'ZONE = "8" THEN
            CALC'TBL'AMT = ABS(BOX'SWGT) * UPC'GND'ZONE8D
        ELSE
        IF BOX'ZONE = "44" THEN
            CALC'TBL'AMT = ABS(BOX'SWGT) * UPC'GND'ZONE44
        ELSE
        IF BOX'ZONE = "45" THEN
            CALC'TBL'AMT = ABS(BOX'SWGT) * UPC'GND'ZONE45
        ELSE
        IF BOX'ZONE = "46" THEN
            CALC'TBL'AMT = ABS(BOX'SWGT) * UPC'GND'ZONE46
        ELSE
            IF RATE'SHOPPING # 1 THEN
                ENTRY = "Not a valid zone for ground dimensional weight " + BOX'ZONE
                CALL DO'ERR
            ENDIF
        ENDCASE
    ELSE                    !!Next Day Air, Next Day Air Hundredweight
    IF VIA'MTYPE = "U" AND (VIA'MAN'YN = "A" OR VIA'MAN'YN = "1" OR &
                            VIA'MAN'YN = "X") AND VIA'SERV'TYPE = "01" THEN
        CASE
        IF BOX'ZONE = "102" THEN
            CALC'TBL'AMT = ABS(BOX'SWGT) * UPC'NXT'ZONE2D
        ELSE
        IF BOX'ZONE = "103" THEN
            CALC'TBL'AMT = ABS(BOX'SWGT) * UPC'NXT'ZONE3D
        ELSE
        IF BOX'ZONE = "104" THEN
            CALC'TBL'AMT = ABS(BOX'SWGT) * UPC'NXT'ZONE4D
        ELSE
        IF BOX'ZONE = "105" THEN
            CALC'TBL'AMT = ABS(BOX'SWGT) * UPC'NXT'ZONE5D
        ELSE
        IF BOX'ZONE = "106" THEN
            CALC'TBL'AMT = ABS(BOX'SWGT) * UPC'NXT'ZONE6D
        ELSE
        IF BOX'ZONE = "107" THEN
            CALC'TBL'AMT = ABS(BOX'SWGT) * UPC'NXT'ZONE7D
        ELSE
        IF BOX'ZONE = "108" THEN
            CALC'TBL'AMT = ABS(BOX'SWGT) * UPC'NXT'ZONE8D
        ELSE
        IF BOX'ZONE = "124" THEN
            CALC'TBL'AMT = ABS(BOX'SWGT) * UPC'NXT'ZONE24
        ELSE
        IF BOX'ZONE = "125" THEN
            CALC'TBL'AMT = ABS(BOX'SWGT) * UPC'NXT'ZONE25
        ELSE
        IF BOX'ZONE = "126" THEN
            CALC'TBL'AMT = ABS(BOX'SWGT) * UPC'NXT'ZONE26
        ELSE
            IF RATE'SHOPPING # 1 THEN
                ENTRY = "Not a valid zone for next day dimensional weight " + BOX'ZONE
                CALL DO'ERR
            ENDIF
        ENDCASE
    ELSE                    !!Next Day Air AM
    IF VIA'MTYPE = "U" AND VIA'MAN'YN = "A" AND VIA'SERV'TYPE = "14" THEN
        CASE
        IF BOX'ZONE = "102" THEN
            CALC'TBL'AMT = ABS(BOX'SWGT) * UPC'NXM'ZONE2D
        ELSE
        IF BOX'ZONE = "103" THEN
            CALC'TBL'AMT = ABS(BOX'SWGT) * UPC'NXM'ZONE3D
        ELSE
        IF BOX'ZONE = "104" THEN
            CALC'TBL'AMT = ABS(BOX'SWGT) * UPC'NXM'ZONE4D
        ELSE
        IF BOX'ZONE = "105" THEN
            CALC'TBL'AMT = ABS(BOX'SWGT) * UPC'NXM'ZONE5D
        ELSE
        IF BOX'ZONE = "106" THEN
            CALC'TBL'AMT = ABS(BOX'SWGT) * UPC'NXM'ZONE6D
        ELSE
        IF BOX'ZONE = "107" THEN
            CALC'TBL'AMT = ABS(BOX'SWGT) * UPC'NXM'ZONE7D
        ELSE
        IF BOX'ZONE = "108" THEN
            CALC'TBL'AMT = ABS(BOX'SWGT) * UPC'NXM'ZONE8D
        ELSE
        IF BOX'ZONE = "124" THEN
            CALC'TBL'AMT = ABS(BOX'SWGT) * UPC'NXM'ZONE24
        ELSE
            IF RATE'SHOPPING # 1 THEN
                ENTRY = "Not a valid zone for next day AM dimensional weight " + BOX'ZONE
                CALL DO'ERR
            ENDIF
        ENDCASE
        IF UPC'NXM'CHARGE = 0 THEN
            IF RATE'SHOPPING # 1 THEN
                ENTRY = "Extra charge for Next Day AM is set to zero!"
                CALL DO'ERR
            ENDIF
        ENDIF
    ELSE                    !!Next Day Air Saver
    IF VIA'MTYPE = "U" AND VIA'MAN'YN = "A" AND VIA'SERV'TYPE = "13" THEN
        CASE
        IF BOX'ZONE = "132" THEN
            CALC'TBL'AMT = ABS(BOX'SWGT) * UPC'NXS'ZONE2D
        ELSE
        IF BOX'ZONE = "133" THEN
            CALC'TBL'AMT = ABS(BOX'SWGT) * UPC'NXS'ZONE3D
        ELSE
        IF BOX'ZONE = "134" THEN
            CALC'TBL'AMT = ABS(BOX'SWGT) * UPC'NXS'ZONE4D
        ELSE
        IF BOX'ZONE = "135" THEN
            CALC'TBL'AMT = ABS(BOX'SWGT) * UPC'NXS'ZONE5D
        ELSE
        IF BOX'ZONE = "136" THEN
            CALC'TBL'AMT = ABS(BOX'SWGT) * UPC'NXS'ZONE6D
        ELSE
        IF BOX'ZONE = "137" THEN
            CALC'TBL'AMT = ABS(BOX'SWGT) * UPC'NXS'ZONE7D
        ELSE
        IF BOX'ZONE = "138" THEN
            CALC'TBL'AMT = ABS(BOX'SWGT) * UPC'NXS'ZONE8D
        ELSE
            IF RATE'SHOPPING # 1 THEN
                ENTRY = "Not a valid zone for next day saver dimensional weight " + BOX'ZONE
                CALL DO'ERR
            ENDIF
        ENDCASE
    ELSE                    !!Second Day Air AM
    IF VIA'MTYPE = "U" AND VIA'MAN'YN = "S" AND VIA'SERV'TYPE = "59" THEN
        CASE
        IF BOX'ZONE = "242" THEN
            CALC'TBL'AMT = ABS(BOX'SWGT) * UPC'SEM'ZONE2D
        ELSE
        IF BOX'ZONE = "243" THEN
            CALC'TBL'AMT = ABS(BOX'SWGT) * UPC'SEM'ZONE3D
        ELSE
        IF BOX'ZONE = "244" THEN
            CALC'TBL'AMT = ABS(BOX'SWGT) * UPC'SEM'ZONE4D
        ELSE
        IF BOX'ZONE = "245" THEN
            CALC'TBL'AMT = ABS(BOX'SWGT) * UPC'SEM'ZONE5D
        ELSE
        IF BOX'ZONE = "246" THEN
            CALC'TBL'AMT = ABS(BOX'SWGT) * UPC'SEM'ZONE6D
        ELSE
        IF BOX'ZONE = "247" THEN
            CALC'TBL'AMT = ABS(BOX'SWGT) * UPC'SEM'ZONE7D
        ELSE
        IF BOX'ZONE = "248" THEN
            CALC'TBL'AMT = ABS(BOX'SWGT) * UPC'SEM'ZONE8D
        ELSE
            IF RATE'SHOPPING # 1 THEN
                ENTRY = "Not a valid zone for 2nd day AM dimensional weight " + BOX'ZONE
                CALL DO'ERR
            ENDIF
        ENDCASE
    ELSE                    !!2nd Day Air, 2nd Day Hundredweight
    IF VIA'MTYPE = "U" AND (VIA'MAN'YN = "S" OR VIA'MAN'YN = "2" OR &
                            VIA'MAN'YN = "Y") AND VIA'SERV'TYPE = "02" THEN
        CASE
        IF BOX'ZONE = "202" THEN
            CALC'TBL'AMT = ABS(BOX'SWGT) * UPC'SEC'ZONE2D
        ELSE
        IF BOX'ZONE = "203" THEN
            CALC'TBL'AMT = ABS(BOX'SWGT) * UPC'SEC'ZONE3D
        ELSE
        IF BOX'ZONE = "204" THEN
            CALC'TBL'AMT = ABS(BOX'SWGT) * UPC'SEC'ZONE4D
        ELSE
        IF BOX'ZONE = "205" THEN
            CALC'TBL'AMT = ABS(BOX'SWGT) * UPC'SEC'ZONE5D
        ELSE
        IF BOX'ZONE = "206" THEN
            CALC'TBL'AMT = ABS(BOX'SWGT) * UPC'SEC'ZONE6D
        ELSE
        IF BOX'ZONE = "207" THEN
            CALC'TBL'AMT = ABS(BOX'SWGT) * UPC'SEC'ZONE7D
        ELSE
        IF BOX'ZONE = "208" THEN
            CALC'TBL'AMT = ABS(BOX'SWGT) * UPC'SEC'ZONE8D
        ELSE
        IF BOX'ZONE = "224" THEN
            CALC'TBL'AMT = ABS(BOX'SWGT) * UPC'SEC'ZONE24
        ELSE
        IF BOX'ZONE = "225" THEN
            CALC'TBL'AMT = ABS(BOX'SWGT) * UPC'SEC'ZONE25
        ELSE
        IF BOX'ZONE = "226" THEN
            CALC'TBL'AMT = ABS(BOX'SWGT) * UPC'SEC'ZONE26
        ELSE
            IF RATE'SHOPPING # 1 THEN
                ENTRY = "Not a valid zone for 2nd day dimensional weight " + BOX'ZONE
                CALL DO'ERR
            ENDIF
        ENDCASE
    ELSE                    !!Third Day Air, Third Day Hundred Weight
    IF VIA'MTYPE = "U" AND VIA'MAN'YN = "R" OR VIA'MAN'YN = "3" OR &
                            VIA'MAN'YN = "Z" THEN
        CASE
        IF BOX'ZONE = "302" THEN
            CALC'TBL'AMT = ABS(BOX'SWGT) * UPC'THD'ZONE2D
        ELSE
        IF BOX'ZONE = "303" THEN
            CALC'TBL'AMT = ABS(BOX'SWGT) * UPC'THD'ZONE3D
        ELSE
        IF BOX'ZONE = "304" THEN
            CALC'TBL'AMT = ABS(BOX'SWGT) * UPC'THD'ZONE4D
        ELSE
        IF BOX'ZONE = "305" THEN
            CALC'TBL'AMT = ABS(BOX'SWGT) * UPC'THD'ZONE5D
        ELSE
        IF BOX'ZONE = "306" THEN
            CALC'TBL'AMT = ABS(BOX'SWGT) * UPC'THD'ZONE6D
        ELSE
        IF BOX'ZONE = "307" THEN
            CALC'TBL'AMT = ABS(BOX'SWGT) * UPC'THD'ZONE7D
        ELSE
        IF BOX'ZONE = "308" THEN
            CALC'TBL'AMT = ABS(BOX'SWGT) * UPC'THD'ZONE8D
        ELSE
            IF RATE'SHOPPING # 1 THEN
                ENTRY = "Not a valid zone for third day dimensional weight " + BOX'ZONE
                CALL DO'ERR
            ENDIF
        ENDCASE
    ELSE
        IF RATE'SHOPPING # 1 THEN
            ENTRY = "Program error: this ship via not set up for dimensional!"
            CALL DO'ERR
        ENDIF
    ENDCASE
RETURN

E'DIM'BOX'LENGTH:
!!Fedex
    IF VIA'MTYPE = "F" THEN
        CASE
        IF WGT'AC'YN = "01" THEN
!!22 LB Max (10kg box)
            \GAFR PACK09 AFR="X:BOX'LENGTH+X:BOX'WIDTH+X:BOX'HEIGHT"
            BOX'LENGTH = 16
            BOX'WIDTH = 13
            BOX'HEIGHT = 11
            BOX'GIRTH = (BOX'HEIGHT * 2) + (BOX'WIDTH * 2)
            RETURN                        !!KLUDGE
        ELSE
        IF WGT'AC'YN = "02" THEN
!!56 LB Max (25kg box)
            \GAFR PACK09 AFR="X:BOX'LENGTH+X:BOX'WIDTH+X:BOX'HEIGHT"
            BOX'LENGTH = 22
            BOX'WIDTH = 17
            BOX'HEIGHT = 14
            BOX'GIRTH = (BOX'HEIGHT * 2) + (BOX'WIDTH * 2)
            RETURN                        !!KLUDGE
        ELSE
        IF WGT'AC'YN = "06" THEN
!!20 LB Max (FedEx Tube)
            \GAFR PACK09 AFR="X:BOX'LENGTH+X:BOX'WIDTH+X:BOX'HEIGHT"
            BOX'LENGTH = 38
            BOX'WIDTH = 6
            BOX'HEIGHT = 5
            BOX'GIRTH = (BOX'HEIGHT * 2) + (BOX'WIDTH * 2)
            RETURN                        !!KLUDGE
        ELSE
            \GAFR PACK09 AFR="E:BOX'LENGTH+E:BOX'WIDTH+E:BOX'HEIGHT"
        ENDCASE
    ENDIF
    IF INX2 # FN'3 AND INX2 # FN'4 THEN
        \TSTF
    ENDIF
    IF INX1 = I'ESC THEN
        INX1 = I'FUN
        INX2 = FN'1
    ELSE
        IF INX1 = I'FUN THEN
            STD'KNO = INX2
            IF STD'KNO >= 1 AND STD'KNO <= 8 OR &
                    STD'KNO >=59 AND STD'KNO <= 66 THEN
                \VUSE STD
                IF STD'CTL = 0 THEN
                    BOX'LENGTH = STD'LENGTH
                    BOX'WIDTH = STD'WIDTH
                    BOX'HEIGHT = STD'HEIGHT
                ELSE
                    ENTRY = "NO such box ("+STD'KNO+")!"
                    GOTO DO'ERR
                ENDIF
                MAP1 RE'DDB,B,1
                RE'DDB = 1
                INX2 = FN'10
                CALL COMPUTE'OS'STUFF   !!place !!real
                RETURN                    !!KLUDGE
            ENDIF
        ENDIF
    ENDIF
    IF EFCHG # 0 THEN
        IF BOX'LENGTH = 0 THEN
            BOX'WIDTH = 0
            BOX'HEIGHT = 0
            BOX'GIRTH = 0
            IF VIA'MTYPE # "F" THEN
                WGT'D'YN = "N"
                \GAFR PACK12 AFR="X:BOX'WIDTH+X:BOX'HEIGHT+E:WGT'D'YN"
                \DDBEDT SCREEN="PACK12" SINX="Y" DDBEDT="SAV'PACKS"
            ELSE
                \GAFR PACK09 AFR="X:BOX'WIDTH+X:BOX'HEIGHT"
                \DDBEDT SCREEN="PACK09" SINX="Y" DDBEDT="SAV'PACKS"
            ENDIF
        ENDIF
    ENDIF
RETURN

E'DIM'BOX'WIDTH:
    \TSTF
    IF VIA'MTYPE = "U" AND (VIA'MAN'YN = "A" OR VIA'MAN'YN = "S" OR &
              VIA'MAN'YN = "R" OR VIA'MAN'YN = "H" OR VIA'MAN'YN = "1" OR &
              VIA'MAN'YN = "2" OR VIA'MAN'YN = "3" OR VIA'MAN'YN = "F" OR &
              VIA'MAN'YN = "X" OR VIA'MAN'YN = "Y" OR VIA'MAN'YN = "Z" OR &
              VIA'MAN'YN = "P" OR VIA'MAN'YN = "W" OR VIA'MAN'YN = "B" OR &
              VIA'MAN'YN = "D" OR VIA'MAN'YN = "I") OR VIA'MTYPE = "F" THEN
!!            VIA'MTYPE = "D" THEN
        IF BOX'LENGTH # 0 AND BOX'WIDTH = 0 THEN
            ENTRY = "You must enter width to compute dimensional"
            GOTO DO'ERR
        ENDIF
    ENDIF
    IF BOX'WIDTH > BOX'LENGTH THEN
        ENTRY = "The length must be the longest side of a package"
        GOTO DO'ERR
    ENDIF
    CASE
    IF VIA'MTYPE = "F" THEN
        IF BOX'WIDTH > 0 THEN
            \GAFR PACK09 AFR="E:BOX'HEIGHT"
        ELSE
            \GAFR PACK09 AFR="X:BOX'HEIGHT"
        ENDIF
    ELSE
        IF BOX'WIDTH > 0 THEN
            \GAFR PACK12 AFR="E:BOX'HEIGHT"
        ELSE
            \GAFR PACK12 AFR="X:BOX'HEIGHT"
        ENDIF
    ENDCASE
RETURN

E'DIM'BOX'HEIGHT:
    \TSTF
    IF BOX'HEIGHT = 0 THEN
        ENTRY = "You must enter height to compute dimensional"
        GOTO DO'ERR
    ELSE
        IF BOX'HEIGHT > BOX'LENGTH THEN
            ENTRY = "The length must be the longest side of a package"
            GOTO DO'ERR
        ENDIF
        BOX'GIRTH = (BOX'HEIGHT * 2) + (BOX'WIDTH * 2)
        CALL COMPUTE'OS'STUFF    !!place !!real
    ENDIF
RETURN

MAP1 CANADA'AS'DOMESTIC,S,1
MAP1 UPS'SPECIAL'DIVISOR,S,1

DETERMINE'DIM'EXTRA'CHARGES:
!!If CANADA'AS'DOMESTIC is set to Y, then the calculation of dimensional
!!weight will use the same divisor as UPS Domestic services
        \DG GETVALUE _UPSETUP UPS_Special.Canada_as_domestic ENTRY
        IF DG'CTL = 0 AND ENTRY = "Y" THEN
            CANADA'AS'DOMESTIC = "Y"
        ELSE
            CANADA'AS'DOMESTIC = "N"
        ENDIF

        \DG GETVALUE _UPSETUP UPS_Special.divisor ENTRY
        IF DG'CTL = 0 AND ENTRY = "Y" THEN
            UPS'SPECIAL'DIVISOR = "Y"
        ELSE
            UPS'SPECIAL'DIVISOR = "N"
        ENDIF
        CASE
!USPS
        IF VIA'MTYPE = "G" OR VIA'MTYPE = "E" THEN
            DIM'WEIGHT = CUBIC'SIZE/166 * 100
        ELSE
!!UPS International, sometimes Canada
        IF VIA'MTYPE = "U" AND ((VIA'MAN'YN = "I" OR VIA'MAN'YN = "W" OR &
                VIA'MAN'YN = "D" OR VIA'MAN'YN = "P") OR                &
                (VIA'MAN'YN = "B" AND CANADA'AS'DOMESTIC = "N")) THEN
            IF UPC'INT'DIV # 0 THEN
                DIM'WEIGHT = CUBIC'SIZE/UPC'INT'DIV * 100
            ELSE
                DIM'WEIGHT = CUBIC'SIZE/139 * 100
            ENDIF
        ELSE
!!UPS Domestic Ground & Air, sometimes Canada
        IF VIA'MTYPE = "U" THEN
            IF UPS'SPECIAL'DIVISOR = "Y" THEN
                CASE
!!Canada
                IF VIA'MAN'YN = "B" THEN
                    DIM'WEIGHT = CUBIC'SIZE/350 * 100
                ELSE
!!International
                IF VIA'MAN'YN = "I" OR VIA'MAN'YN = "W" OR VIA'MAN'YN = "D" OR &
                        VIA'MAN'YN = "P" THEN
                    DIM'WEIGHT = CUBIC'SIZE/350 * 100
                ELSE
!!All Domestic
                IF CUBIC'SIZE > 1728 THEN
                    DIM'WEIGHT = CUBIC'SIZE/166 * 100
                ENDCASE

CIF OLD AND NOT OLD THEN
                CASE
!!Surepost
                IF VIA'1Z'SERV = "YH" OR VIA'1Z'SERV = "YN" OR VIA'1Z'SERV = "YP" &
                        OR VIA'1Z'SERV = "YT" OR VIA'1Z'SERV = "YW" THEN

                        DIM'WEIGHT = CUBIC'SIZE/139 * 100
                ELSE
!!Canada less than 5184 (3 cubic feet)
                IF VIA'MAN'YN = "B" AND CUBIC'SIZE < 5184 THEN
                    DIM'WEIGHT = CUBIC'SIZE/225 * 100
                ELSE
!!Canada more than 5184
                IF VIA'MAN'YN = "B" AND CUBIC'SIZE > 5184 THEN
                    DIM'WEIGHT = CUBIC'SIZE/220 * 100
                ELSE
!!Ground commercial and Ground residential
                IF VIA'MAN'YN = "C" AND CUBIC'SIZE < 5184 THEN
                    DIM'WEIGHT = CUBIC'SIZE/225 * 100
                ELSE
!!Ground commercial and Ground residential more than 5184
                IF VIA'MAN'YN = "C" AND CUBIC'SIZE > 5184 THEN
                    DIM'WEIGHT = CUBIC'SIZE/220 * 100
                ELSE
!!All domestic and standard to Canada > 1728 cubic inches
                    DIM'WEIGHT = CUBIC'SIZE/139 * 100
                ENDCASE
ENDCIF
            ELSE
!!Domestic Divisor in UP Dimensional Control Record
                IF UPC'DOM'DIV # 0 THEN
                    DIM'WEIGHT = CUBIC'SIZE/UPC'DOM'DIV * 100
                ELSE
                    DIM'WEIGHT = CUBIC'SIZE/139 * 100
                ENDIF
           ENDIF
        ELSE
        IF VIA'MTYPE = "F" AND VIA'INT'YN = "N" THEN
!!Fedex Domestic
            IF UPC'DOM'DIV # 0 THEN
                DIM'WEIGHT = CUBIC'SIZE/UPC'DOM'DIV * 100
            ELSE
                DIM'WEIGHT = CUBIC'SIZE/139 * 100        !!Dimensional Weight
            ENDIF
        ELSE
!!Fedex International
        IF VIA'MTYPE = "F" AND VIA'INT'YN = "Y" THEN
            IF UPC'INT'DIV # 0 THEN
                DIM'WEIGHT = CUBIC'SIZE/UPC'INT'DIV * 100
            ELSE
                DIM'WEIGHT = CUBIC'SIZE/139 * 100
            ENDIF
        ENDCASE
        IF ASTERISK'SET = "Y" THEN
            WGT'D'YN = "Y"
        ENDIF
        IF VIA'MTYPE = "U" THEN
            \DG GETVALUE _UPSETUP UPS_max_box_weight ENTRY
            IF DG'CTL = 0 AND ENTRY # "" THEN
                IF BOX'WEIGHT > ENTRY THEN
                    IF WGT'LGE'PKG'YN = "Y" AND WGT'AC'YN = "Y" THEN
                        WGT'AC'YN = "N"
                    ENDIF
                ENDIF
            ELSE
                IF BOX'WEIGHT > 5000 THEN              !!Actual weight
                    IF WGT'LGE'PKG'YN = "Y" AND WGT'AC'YN = "Y" THEN
                        WGT'AC'YN = "N"                !!No additonal charges if large
                    ENDIF
                ENDIF
            ENDIF
       ENDIF
!!If large can knock off ac'yn
       IF VIA'MTYPE = "F" THEN
           IF BOX'WEIGHT > 7000 THEN
               IF WGT'LGE'PKG'YN = "Y "AND WGT'AC'YN = "Y" THEN
                   WGT'AC'YN = "N"
                ENDIF
           ENDIF
       ENDIF
RETURN

MAP1    SAV'PACKS,X,540
\COPYF STD UP
MAP1 STDPTR,X,30

DO'PACK09:
    DSP PACK09
    SAV'PACKS = EDT'OVR
    DDB PACK09
    GDB PACK09
RETURN

DO'PACK11:
    DSP PACK11
    SAV'PACKS = EDT'OVR
    DDB PACK11
    GDB PACK11
RETURN

DO'PACK12:
    DSP PACK12
    SAV'PACKS = EDT'OVR
    DDB PACK12
    GDB PACK12
RETURN

CHANGE'VIA'FOR'BASIC:
    IF HDR'SPECIAL = "A" RETURN            !!KLUDGE
    IF HDR'SPECIAL = "T" RETURN            !!KLUDGE
    IF HDR'SPECIAL = "W" RETURN            !!KLUDGE
    IF HDR'SPECIAL = "S" RETURN            !!KLUDGE
    IF HDR'SPECIAL = "K" RETURN            !!KLUDGE
    IF HDR'SPECIAL = "H" RETURN            !!KLUDGE
!!      CHG'TO'VIA'NO = 0
        CASE
        IF HDR'BOX'COUNT > 1 THEN
                    !!UPS Basic only allowed to have one box
            \DG GETINT _UPSETUP UPSBasic.ToGroundViaNo CHG'TO'VIA'NO
            IF DG'CTL # 0 THEN
                ENTRY = "UPS BASIC: Need to change this to UPS Ground, upsetup.xml wrong1!"
                CALL DO'ERRL
                GOTO ABORT
            ELSE
                ENTRY = "UPS BASIC: Changed VIA to "+STR(CHG'TO'VIA'NO)+" because more than one box!"
                CALL DO'ERRL
            ENDIF
        ELSE
        IF HDR'SHP'ST = "AK" OR HDR'SHP'ST = "HI" THEN
            !!UPS Basic only allowed in 48 contiguous
            \DG GETINT _UPSETUP UPSBasic.ToGroundViaNo CHG'TO'VIA'NO
            IF DG'CTL # 0 THEN
                ENTRY = "UPS BASIC: Need to change this to UPS Ground, upsetup.xml wrong1!"
                CALL DO'ERRL
                GOTO ABORT
            ELSE
                ENTRY = "UPS BASIC: Changed VIA to "+STR(CHG'TO'VIA'NO)+" because not 48 contiguous states!"
                CALL DO'ERRL
            ENDIF
        ELSE
        IF IS'POB = "Y" THEN
            \DG GETVALUE _UPSETUP UPSBasic.MaxPOBWgt III
            IF DG'CTL # 0 THEN
                WGT'TST = 0
            ELSE
                WGT'TST = III * 100
            ENDIF
            IF (NEW'SWGT * 100) > WGT'TST AND LARGE'SIZE > 108 THEN
                \DG GETINT _UPSETUP UPSBasic.ToGroundViaNo CHG'TO'VIA'NO
                IF DG'CTL # 0 THEN
                    ENTRY = "UPS BASIC: Need to change this to UPS Ground, upsetup.xml wrong2!"
                    CALL DO'ERRL
                    GOTO ABORT
                ELSE
                    ENTRY = "UPS BASIC: " + ENTRY + "UPS Basic weight limit exceeded: "+STR(NEW'SWGT)+">"+STR(WGT'TST/100)+" VIA changed to "+STR(CHG'TO'VIA'NO)+" large size="+STR(LARGE'SIZE)
                    CALL DO'ERRL
                ENDIF
            ENDIF
        ELSE
            \DG GETVALUE _UPSETUP UPSBasic.MaxRegWgt III
            IF DG'CTL # 0 THEN
                WGT'TST = 0
            ELSE
                WGT'TST = III * 100
            ENDIF
            IF (NEW'SWGT * 100) > WGT'TST OR LARGE'SIZE > 84 THEN
                \DG GETINT _UPSETUP UPSBasic.ToGroundViaNo CHG'TO'VIA'NO
                IF DG'CTL # 0 THEN
                    ENTRY = "UPS BASIC: Need to change this to UPS Ground, upsetup.xml wrong3!"
                    CALL DO'ERRL
                    GOTO ABORT
                ELSE
                    ENTRY = "UPS BASIC: UPS Basic weight limit exceeded: "+STR(NEW'SWGT)+">"+STR(WGT'TST/100)+" VIA changed to "+STR(CHG'TO'VIA'NO)+" or size "+STR(LARGE'SIZE)
                    CALL DO'ERRL
                ENDIF
            ENDIF
        ENDCASE
RETURN

LCL1 NEED'GT'LBS,B,1
LCL1 VTAG,S,40

CHANGE'VIA'FOR'SUREPOST:
    IF HDR'SPECIAL = "A" RETURN            !!KLUDGE
    IF HDR'SPECIAL = "T" RETURN            !!KLUDGE
    IF HDR'SPECIAL = "W" RETURN            !!KLUDGE
    IF HDR'SPECIAL = "S" RETURN            !!KLUDGE
    IF HDR'SPECIAL = "K" RETURN            !!KLUDGE
    IF HDR'SPECIAL = "H" RETURN            !!KLUDGE
    NEED'GT'LBS = 0
    IF HDR'BOX'COUNT > 1 OR NEW'SWGT >= 1 THEN
!!Multi box we have to keep the same and > lbs makes most sense
        NEED'GT'LBS = 1
    ENDIF

!!  CHG'TO'VIA'NO = 0

    \DG GETVALUE _UPSETUP UPSSurepost.MaxWgt III
    IF DG'CTL # 0 THEN
        WGT'TST = 0
    ELSE
        WGT'TST = III * 100
    ENDIF
    IF (NEW'SWGT * 100) > WGT'TST OR LARGE'SIZE > 130 THEN
        \DG GETINT _UPSETUP UPSSurepost.ToGroundViaNo CHG'TO'VIA'NO
        IF DG'CTL # 0 THEN
            ENTRY = "UPS Surepost: Need to change this to UPS Ground, upsetup.xml wrong5!"
            CALL DO'ERRL
            GOTO ABORT
        ELSE
            ENTRY = "UPS Surepost: UPS Surepost weight limit exceeded: "+STR(NEW'SWGT)+">"+STR(WGT'TST/100)+" VIA changed to "+STR(CHG'TO'VIA'NO)+" or size "+STR(LARGE'SIZE)
            CALL DO'ERRL
        ENDIF
    ELSE
        \DG GETVALUE _UPSETUP UPSSurepost.LessThanLbs ENTRY
        IF DG'CTL = 0 AND ENTRY = "Y" THEN
          IF VIA'1Z'SERV = "YH" OR VIA'1Z'SERV = "YN" THEN
            IF NEED'GT'LBS # 0 THEN
                VTAG = "WgtSwitchVia"+STR(HDR'VIA'NO)
                \DG GETINT _UPSETUP VTAG ENTRY TAGVAR="Y"
                IF DG'CTL = 0 AND VAL(ENTRY) # 0 THEN
                    CHG'TO'VIA'NO = VAL(ENTRY)
                ELSE
                    ENTRY = "UPS Surepost: Need to change this to > 1 lbs, upsetup.xml wrong6!"
                    CALL DO'ERRL
                    GOTO ABORT
                ENDIF
            ENDIF
          ELSE
            IF NEED'GT'LBS = 0 THEN
                VTAG = "WgtSwitchVia"+STR(HDR'VIA'NO)
                \DG GETINT _UPSETUP VTAG ENTRY TAGVAR="Y"
                IF DG'CTL = 0 AND VAL(ENTRY) # 0 THEN
                    CHG'TO'VIA'NO = VAL(ENTRY)
                ELSE
                    ENTRY = "UPS Surepost: Need to change this to < 1 lbs, upsetup.xml wrong6!"
                    CALL DO'ERRL
                    GOTO ABORT
                ENDIF
            ENDIF
          ENDIF
        ENDIF
    ENDIF
RETURN

!!BOX'CHARGES from WGT

SET'BOX'CHARGES'FROM'WGT'REAL:       !!Set bits from !!real
    \SETBOXFROMWGT CLRWGT="Y" CACHE'FIELDS="N"   !!place !!real
RETURN


