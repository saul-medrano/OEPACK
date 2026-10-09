! @#$^&* 7 0 90 0 0 0 0 0 0 0 0 0 91 *&^$#@ 
!* Updated on 03-Oct-95 at 1:30 PM by Cheryl Eberhard; edit time: 0:13:40
!!FIND UNITS OF MEASURE 7.1
!!
!!
!!Copyright (c) 1986,1987,1988,1989,1990,1991,1992,1993,1994,1995,1996,1997,
!!              1998,1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,
!!              2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021,
!!              2022 by
!!
!!      VERTICAL INTEGRATED COMPUTER SYSTEMS
!!             2933 W Middle Verde Rd
!!              Camp Verde, AZ  86322
!!               (928) 567-3727
!!
!!              Vertical Integrated Computer Systems, Camp Verde, AZ  USA
!!              All rights reserved.
!!
!!
!!
CIF NOT UOM THEN
        SYMBOL UOM="UOM"
ENDCIF
!!
CIF NOT ERROR THEN
        SYMBOL ERROR="Y"
ENDCIF
!!
\COPYF  UOM IN |UOM| MODE="PRI"
!!
CIF NOT PRE 
        CIF NOT FLDNAM THEN
                SYMBOL PRE="LIN"
        CELSE
                SYMBOL PRE="123456"
        ENDCIF
ENDCIF
!!
CIF NOT FLDNAM THEN
        SYMBOL FLDNAM="|PRE|'UOM'NO"
ENDCIF
!!
CIF NOT APID THEN
        \SETAPP V''TMP                  !!ESE, GENCODE sets APID
        SYMBOL APID="|V''TMP|"
ENDCIF
!!
!!
FIND'|UOM|:
        IF |UOM|'NO # |FLDNAM| OR |UOM|'CTL # 0 THEN
                CASE
                IF |FLDNAM| = 1 THEN
                        |UOM|'NO = 1
                        |UOM|'NUNITS = 1
                        |UOM|'DUNITS = 1
                        |UOM|'CTL = 0
                        |UOM|'DESC = "Each"
                        |UOM|'PUNITS = 0
                        |UOM|'QFRACTION = 0
                        |UOM|'FD = "F"
                ELSE
                IF (|FLDNAM| = 12 OR |FLDNAM| = 1200) AND &
                                                INC'INTERFACE = 5 THEN
                        |UOM|'NO = 12
                        |UOM|'DUNITS = 1
                        |UOM|'CTL = 0
                        |UOM|'NUNITS = 12
                        |UOM|'DESC = "Dozen"
                        |UOM|'PUNITS = 0
                        |UOM|'QFRACTION = 0
                        |UOM|'FD = "F"
                ELSE
                IF (|FLDNAM| = 24 OR |FLDNAM| = 36 OR |FLDNAM| = 48 &
                              OR |FLDNAM| = 72 OR |FLDNAM| = 144) AND &
                                                INC'INTERFACE = 5 THEN
                        |UOM|'NO = |FLDNAM|
                        |UOM|'DUNITS = 1
                        |UOM|'CTL = 0
                        |UOM|'NUNITS = |FLDNAM|
                        |UOM|'DESC = "Cs/"+STR(|FLDNAM|/12)
                        |UOM|'PUNITS = 0
                        |UOM|'QFRACTION = 0
                        |UOM|'FD = "F"
                ELSE
                        |UOM|'KNO = |FLDNAM|
                        |UOM|'SPACE = SPACE
                        USE |UOM| INTO |UOM|'REC KEY |UOM|'KEY
                        IF |UOM|'CTL # 0 THEN
                                CIF ERROR # "N" THEN
                                        ENTRY = |UOM|'KNO+" UOM not found on: "
                                        CIF PRE = "LIN" OR PRE="LHS" THEN
                                                ENTRY = |PRE|'DOC'TYPE+ &
                                                    |PRE|'HDR'NO+"-"+|PRE|'NO
!!                                          \KERROR PRE="|PRE|" APID="|APID|"
                                        ENDCIF
                                        CALL DO'ERRL
                                CELSE
                                        |UOM|'DESC = "*** Not found ***"
                                ENDCIF
                                |UOM|'DUNITS = 1
                                |UOM|'NUNITS = 1
                        ENDIF
                ENDCASE
        ENDIF
RETURN

