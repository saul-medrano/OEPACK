! @#$^&* 9 0 84 0 0 0 0 0 0 0 0 0 85 *&^$#@ 
!* Updated on 02-Oct-95 at 4:40 PM by Cheryl Eberhard; edit time: 6:21:08
!!TTLSTX.CPY,8.0[7]
!!                       V. I. C. S.
!!                    of Camp Verde, AZ
!!
!!Copyright (c) 1986,1987,1988,1989,1990,1991,1992,1993,1994,1995,1996,1997,
!!              2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021,
!!              2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021,
!!              2022 by
!!
!!
!!      VERTICAL INTEGRATED COMPUTER SYSTEMS
!!           2933 W Middle Verde Rd
!!              Camp Verde, AZ  86322
!!               (928) 567-3727
!!              All rights reserved.
!!
!!
!!10/28/87 ESE  
!!
!!USED IN:              OEXONE.BV
!!                      OERBAK.BV
!!                      OE1SHP.BV
!!                      LINSUB.CPY (OE1HDR.BV)
!!                      LINWGA.CPY (OE1WGA.BV)
!!                      CLCHST.CPY[SR] (ARXCLC.BV[AR])
!!                      OEPDOC.BV
!!                      OEPQTY.BV
!!                      OERTC4.BV
!!                      TTLHDR.CPY
!!
!!      REVISION HISTORY:
!!
!!7.2   11/30/88 ASC    New version created       
!!
!![1] 11/03/89 KJS/ESE  Made to work for ARXCLC
!!
!![2] 05/03/90 ESE      Re-done again for new system.
!!
!![3] 11/14/90 HLB      Found while tracking down unmapped variables.
!!                      Unsure of affect.
!!                      Also KJS fixed weirdness with ALTS and ALTKEY
!!
!![4] 05/09/91 ESE      Speed it up?
!!
!![5] 10/22/91 ESE      Make pickups tax correctly.
!!
!![6] 08/13/93 ESE      Caching added all over.
!!
!!7.4   09/30/95 CAE    New version created
!!
!!8.0  03/31/97 CAE     New version created
!!
!![7]  03/31/97 CAE     NEW: Changes for year 2000.
!!
!!

CIF NOT PRE THEN                        !!KJS
        SYMBOL PRE="STX"
ENDCIF

CIF NOT HDRPRE THEN
        SYMBOL HDRPRE="HDR"
ENDCIF

CIF NOT FLDNAM THEN
        MAP1 TTL'|PRE|'PERC,B,3
        SYMBOL FLDNAM="TTL'|PRE|'PERC"
ENDCIF

LCL1 STX'LU,S,30
LCL1 II,F,6

\COPYF STX IN |PRE| MODE="PRI"
\COPYF STE IN MODE="PRI"
\COPYF STS IN MODE="PRI"
\COPYF LOC IN MODE="DTA"                                !![5]
\COPYF LOC IN STXL MODE="PRI"                           !![5]
COPYF "LSTFIL[MC]",=TTLSTX,NOAFR
COPYF "LSTFKY[MC]",=USELST,NOAFR                        !![5]
!![3] COPYF "LSTFKY[MC]",=TTLSTX,NOAFR  !! Won't copy because BPP only
!!                                      !! tracks 5 characters.
\COPYF CIT MC BAD1="CITC"
!!\COPYF CTY MC

SYMBOL CACHE="N"

COMPUTE'|FLDNAM|:
        |FLDNAM| = 0
        FOR II = 1 TO 3
                |FLDNAM| = |FLDNAM| + |PRE|'PERC(II)
        NEXT II
        IF |PRE|'STS'COUNT # 0 THEN
                IF |HDRPRE|'DROP'YN = "N" THEN |HDRPRE|'SHP'NO = |HDRPRE|'NAM'NO
                CIF LST'ALREADY # "N" OR NOT LST'ALREADY THEN
                        IF |HDRPRE|'SHP'NO # TTLSTX'NO AND |HDRPRE|'SHP'NO = LST'NO THEN
                                TTLSTX'REC = LST'REC
                        ENDIF
                ENDCIF
                IF |HDRPRE|'VIA'NO >= OEC'SCUS'VIA              AND &
                                |HDRPRE|'VIA'NO <= OEC'ECUS'VIA THEN
!!If pickup
                        IF LOC'NO = |HDRPRE|'LOC'NO THEN
                                STXL'REC = LOC'REC
                        ENDIF
                        IF STXL'NO # |HDRPRE|'LOC'NO THEN
                                CIF CACHE="Y" THEN
                                        STXL'KNO = |HDRPRE|'LOC'NO
                                        \VUSE STXL - LOCPTR MODE="1107"
                                CELSE
                                        STXL'KNO = |HDRPRE|'LOC'NO
                                        USE STXL INTO STXL'REC KEY STXL'KEY
                                ENDCIF
                        ENDIF
                        IF TTLSTX'NO # STXL'LST'NO THEN
                                \USELST STXL'LST'NO                     &
                                        ALTPRE="TTLSTX" ALTKEY="USELST"
                        ENDIF
!![5] - above
                ELSE
                        IF |HDRPRE|'SHP'NO # TTLSTX'NO                  OR &
                                                |HDRPRE|'SHIP'TO # 0    THEN
!![4] - above
!![5] - used USELST key below.
                                \USELST |HDRPRE|'SHP'NO                 &
                                        ALTFLD="|HDRPRE|'SHIP'TO"       &
                                        ALTPRE="TTLSTX" ALTKEY="USELST"
                        ENDIF
                ENDIF
                IF |HDRPRE|'DROP'YN = "N" THEN |HDRPRE|'SHP'NO = 0
                IF USELST'CTL # 0 THEN
                        ENTRY = "Invalid TTLSTX LST record: "
!![3]                   \KERROR LSTF MODE="K" ALTKEY="TTLSTX" APID="MC"
                        \KERROR LSTF MODE="K" ALTKEY="USELST" APID="MC"
                        GOTO DO'ERRL                    !!KLUDGE
                ENDIF
                STS'CTL = 1
                STS'KSTX'LOC'NO = |PRE|'LOC'NO
                STS'KSTX'NO = |PRE|'NO
                STS'KST = TTLSTX'ST
                CIF CACHE="Y" THEN
                        LCL1 STSPTR,X,30
                        XCALL VICSIT,7,106,STS'REC,0,STSPTR,5,STS'PKT,STS'KEY
                ENDCIF                  
                IF STS'CTL # 0 THEN
                        STS'SPACE = SPACE
                        USE STS INTO STS'REC KEY STS'KEY                
                        CIF CACHE="Y" THEN
                                XCALL VICSIT,7,105,STS'REC,0,STSPTR,5,STS'PKT,STS'KEY
                        ENDCIF                  
                ENDIF
                IF STS'CTL # 0 OR STS'STX'NO # |PRE|'NO THEN
                        STS'KST = ""
                        CIF CACHE="Y" THEN
                                STS'SPACE = SPACE
                                \VUSE STS - VPTR=STSPTR
                        CELSE
                                USE STS INTO STS'REC KEY STS'KEY                
                        ENDCIF
                ENDIF
                IF STS'CTL = 0 THEN
                        IF CUS'TAXABLE = "Y" OR CUS'TAXABLE = |TST'TAXABLE| THEN
                            FOR II = 1 TO 3
                                STS'FLAG(II) = "Y"
                            NEXT II
                        ENDIF
                        |FLDNAM| = 0            
                        FOR II = 1 TO 3
                                IF STS'FLAG(II) # "N" THEN
                                        |FLDNAM| = |FLDNAM| + STS'PERC(II)
                                ENDIF
                        NEXT II
                        IF INC'STX'BY = "C" THEN
                                CIT'KCOU'ABV = TTLSTX'COUNTRY
                                CIT'KST = TTLSTX'ST
                                CIT'KCITY = UCS(TTLSTX'CITY)
                                USE CIT INTO CIT'REC KEY CIT'KEY
                                IF CIT'CTL # 0 THEN
                                        ENTRY = "Invalid city: "
                                        \KERROR CIT APID="MC" MODE="K"
                                        GOTO DO'ERRL    !!KLUDGE
                                ENDIF
                                STE'KCTY'NO = CIT'CTY'NO
                                STX'LU = TTLSTX'CITY
                        ELSE
                                STE'KCTY'NO = 0
                                STX'LU = |HDRPRE|'SHP'ZIP
                        ENDIF
                        IF STS'STE'COUNT # 0 THEN
                                STE'KSTX'LOC'NO = |PRE|'LOC'NO
                                STE'KSTX'NO = |PRE|'NO
                                STE'KSTS'ST = TTLSTX'ST
                                STE'KSTART'LU = UCS(STX'LU)
                                USE STE INTO STE'REC KEY STE'KEY
                                IF STE'CTL # 0 THEN
                                        IF INC'STX'BY # "C" THEN
                                                GETPRV STE INTO STE'REC
                                        ELSE
                                                STE'KSTART'LU = ""
                                                USE STE INTO STE'REC KEY STE'KEY
                                        ENDIF
                                ENDIF
                                \CHKKEY STE APID="IN"
                                IF STE'CTL = 0                        AND &
                                                (STX'LU <= STE'END'ZIP OR  &
                                                 INC'STX'BY = "C")      THEN
                                        |FLDNAM| = 0            
                                        FOR II = 1 TO 3
                                                IF STS'FLAG(II) # "N" THEN
                                                        |FLDNAM| = |FLDNAM| + STE'PERC(II)
                                                ENDIF
                                        NEXT II
                                ENDIF
                        ENDIF
                ENDIF
        ENDIF
RETURN

CIF SFTAXABLE'PRE THEN
        CIF SFTAXABLE'PRE # HDRPRE THEN
                ERROR - different HDRPRE for TTLSTX
                MACRO ERROR - different HDRPRE for TTLSTX
        ENDCIF
ENDCIF

GLOBAL SFTAXABLE'PRE="|HDRPRE|"

DEFINE SFTAXABLE
LCL1 II,F,6
        GET'SF'TAXABLE:
                SF = 1
                FOR II = 1 TO 10
                        IF INC'ST'TAX(II) # "" THEN SF = 0 : II = 11
                NEXT II
                IF SF = 0 THEN
!![3]                   IF |SFTAXABLE'PRE|'DROP'YN = "N" THEN |SFTAXABLE'PRE|SHP'NO = |SFTAXABLE'PRE|'NAM'NO
                        IF |SFTAXABLE'PRE|'DROP'YN = "N" THEN |SFTAXABLE'PRE|'SHP'NO = |SFTAXABLE'PRE|'NAM'NO
!![5] - used USELST key below
                        \USELST |SFTAXABLE'PRE|'SHP'NO                  &
                                ALTFLD="|SFTAXABLE'PRE|'SHIP'TO"        &
                                ALTPRE="TTLSTX" ALTKEY="USELST"
                        IF |SFTAXABLE'PRE|'DROP'YN = "N" THEN |SFTAXABLE'PRE|'SHP'NO = 0
                        SF = 1
                        FOR II = 1 TO 10
                                IF INC'ST'TAX(II) = TTLSTX'ST THEN
                                        SF = 0
                                        II = 11
                                ENDIF
                        NEXT II
                ENDIF
        RETURN
ENDMACRO
\\SFTAXABLE


