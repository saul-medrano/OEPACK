! @#$^&* 9 0 121 0 0 0 0 0 0 0 0 0 135 *&^$#@ 
!* Updated on 02-Oct-95 at 4:48 PM by Cheryl Eberhard; edit time: 1:55:01
!!DSPTOT.CPY 8.0(1) - display final shipping totals
!!
!!                        V. I. C. S.
!!                     of Camp Verde, AZ
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
!!07/10/95 ESE  Used in OEPACK.BV and |PRE|SUB (OE1WGA).and WBWHAP.BV
!![1] 11/19/96  ESE   Added WBWHAP stuff.
!!
!!8.0  03/31/97 CAE     New version created
!!
!!
!!
LOCAL   PRE'VDSP'AMT1
CIF ALL'VALUES = "Y" THEN
    SYMBOL DSPER="VALUE"
CELSE
CIF WBWHAP # "Y" THEN
    CIF PRINT # "Y" THEN
        MAP1 AMT1,B,5                   !!Has to be mapped for OEPACK
    ENDCIF
    SYMBOL DSPER = "PACK02"
CELSE
    SYMBOL DSPER = "WBWHAP"
ENDCIF
ENDCIF
CIF PRINT = "Y" THEN
    SYMBOL PRINT = "PRINT"
    SYMBOL DSPER = "PRINT"
    LCL1 AMT1,B,5
ENDCIF

LCL1    SAMT1,B,5

CIF NOT PRE THEN
        SYMBOL PRE="HDR"
ENDCIF

CIF NOT BOX'TYPE THEN
        \GSTRIP |PRE| 3
        CIF V''TMP="BOX" THEN
                SYMBOL BOX'TYPE="B"
        CELSE
        CIF V''TMP="BXV" THEN
                SYMBOL BOX'TYPE="A"
        CELSE
                SYMBOL BOX'TYPE="N"
        ENDCIF
        ENDCIF
ENDCIF

CIF BOX'TYPE # "N" THEN
        SYMBOL STX1="|PRE|'SHP'STX1"
        SYMBOL STX2="|PRE|'SHP'STX2"
CELSE
        SYMBOL STX1="|PRE|'STX1FINAL"
        SYMBOL STX2="|PRE|'STX2FINAL"
ENDCIF

CIF NOT DSP'CASH THEN
    CIF BOX'TYPE # "N" OR ALL'VALUES = "Y" THEN
        SYMBOL DSP'CASH = "Y"
    CELSE
        SYMBOL DSP'CASH = "N"
    ENDCIF
ENDCIF

DSP'|DSPER|'TOTALS:
    CIF WBWHAP # "Y" AND ALL'VALUES # "Y" THEN
        PRINT TAB(-1,CR'OFF);
    ENDCIF
    CIF PRINT = "PRINT" THEN
        PRINT "PRE=|PRE| BOX'TYPE=|BOX'TYPE|"
    ENDCIF
    \HDRSUM PRE="|PRE|" BOX'TYPE="|BOX'TYPE|"
    XLN = 0
    AMT1 = |PRE|'SHP'TOTAL
    CIF PRINT = "PRINT" THEN
        PRINT "|PRE|'SHP'TOTAL=";
    ENDCIF
    CALL PRE'VDSP'AMT1
    CIF BOX'TYPE = "N" THEN
        AMT1 = -|PRE|'SHP'DISC
        CIF PRINT = "PRINT" THEN
            PRINT "|PRE|'SHP'DISC=";
        ENDCIF
        CALL PRE'VDSP'AMT1
    ENDCIF
    AMT1 = FREE'FRT'HSUM
    CIF PRINT = "PRINT" THEN
        PRINT "-FREE'FRT'SUM=";
    ENDCIF
    CALL PRE'VDSP'AMT1
    IF INC'INTERFACE # 5 THEN
        AMT1 = -DISC'HSUM
        CIF PRINT = "PRINT" THEN
            PRINT "-DISC'HSUM=";
        ENDCIF
        CALL PRE'VDSP'AMT1
    ENDIF
    CIF BOX'TYPE = "N" THEN
        AMT1 = -DISC'HSUM
        CIF PRINT = "PRINT" THEN
            PRINT "-DISC'HSUM=";
        ENDCIF
        CALL PRE'VDSP'AMT1
        AMT1 = CHARGE'HSUM
        CIF PRINT = "PRINT" THEN
            PRINT "CHARGE'HSUM=";
        ENDCIF
    CELSE
        IF INC'INTERFACE # 5 THEN
            AMT1 = CHARGE'HSUM
            CIF PRINT = "PRINT" THEN
                PRINT "CHARGE'HSUM=";
            ENDCIF
        ELSE
            AMT1 = DISC'HSUM + CHARGE'HSUM
            CIF PRINT = "PRINT" THEN
                PRINT "DISC'HSUM+CHARGE'HSUM=";
            ENDCIF
        ENDIF
    ENDCIF
    CALL PRE'VDSP'AMT1
    AMT1 = (|PRE|'SHP'TOTAL-|PRE|'SHP'DISC+CHARGE'HSUM+DISC'HSUM+FREE'FRT'HSUM)
    CIF PRINT = "PRINT" THEN
        PRINT "(|PRE|'SHP'TOTAL-|PRE|'SHP'DISC+CHARGE'HSUM+DISC'HSUM+FREE'FRT'HSUM)=";
    ENDCIF
    CALL PRE'VDSP'AMT1
    AMT1 = |STX1|
    CIF PRINT = "PRINT" THEN
        PRINT "|STX1|=";
    ENDCIF
    CALL PRE'VDSP'AMT1
    AMT1 = |STX2|
    CIF PRINT = "PRINT" THEN
        PRINT "|STX2|=";
    ENDCIF
    CALL PRE'VDSP'AMT1
    AMT1 = VIA'HSUM
    CIF PRINT = "PRINT" THEN
        PRINT "VIA'HSUM=";
    ENDCIF
    CALL PRE'VDSP'AMT1
    AMT1 = INVOICE'HSUM
    CIF PRINT = "PRINT" THEN
        PRINT "INVOICE'HSUM=";
    ENDCIF
    CALL PRE'VDSP'AMT1
    CIF DSP'CASH # "N" THEN
        AMT1 = APPLY'HSUM+CASH'HSUM
        CIF PRINT = "PRINT" THEN
            PRINT "APPLY'HSUM+CASH'HSUM=";
        ENDCIF
        CALL PRE'VDSP'AMT1
        XLN = XLN + 1           !!Skip underline
        AMT1 = BALANCE'HSUM
        CIF PRINT = "PRINT" THEN
            PRINT "BALANCE'HSUM=";
        ENDCIF
        CALL PRE'VDSP'AMT1
        XLN = 0
    ENDCIF
    CIF WBWHAP # "Y" AND ALL'VALUES # "Y" THEN
        PRINT TAB(-1,CR'ON);
    ENDCIF
RETURN

PRE'VDSP'AMT1:
    CIF PRINT = "PRINT" THEN
        PRINT AMT1
    CELSE
    CIF WBWHAP # "Y" AND ALL'VALUES # "Y" THEN
        CALL VDSP'AMT1
        XLN = XLN + 1
    CELSE
        CALL LOAD'VDSP'AMT
    ENDCIF
    ENDCIF
RETURN
