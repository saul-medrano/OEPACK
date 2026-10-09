!CMP 
MAP1   HDR'REC
	MAP2	HDR'CNO,B,1			!Cno
					!=IR
					!\2
	MAP2	HDR'LOC'NO,B,2			!Location
					!=IR
					!\4
	MAP2	HDR'DOC'TYPE,B,1		!Document Type
					!=IR
					!\1
	MAP2	HDR'NO,B,3			!Order Number
					!=IR
					!\7
	MAP2	HDR'SHIPMENT,B,2		!Shipment Number
					!=IR
					!\4
	MAP2	HDR'QUOTE'YN,S,1		!Quote?
					!=BRN
					!|"N"
	MAP2	HDR'GL'CNO,B,1			!G/L Company
					!=IR
					!\2
	MAP2	HDR'NAM'NO,B,3			!Customer Number
					!=IR
					!\7
	MAP2	HDR'SHIP'TO,B,2			!Ship to
					!=IR
					!\4
	MAP2	HDR'DROP'YN,S,1			!Drop Ship?
					!=RN
	MAP2	HDR'BO'YN,S,1			!Back Orders Allowed?
					!=RN
	MAP2	HDR'DEL'ALT'YN,S,1		!Delete Alternates?
					!=RN
	MAP2	HDR'SHP'NO,B,3			!Ship To If Drop Ship
					!=IR
					!\7
	MAP2	HDR'TRH'NO,B,2			!Terms
					!=IR
					!\4
	MAP2	HDR'VIA'NO,B,2			!Ship Via
					!=IR
					!\4
	MAP2	HDR'FOB'NO,B,2			!FOB Number
					!=IR
					!\4
	MAP2	HDR'PO,S,15			!PO Number
					!=RPCAI
	MAP2	HDR'SOURCE,S,2			!Source of order
					!=RCAI
	MAP2	HDR'HOLD'YN,S,1			!On Hold?
					!=RN
	MAP2	HDR'OVR1,X			!Ovr1
					!=XRA
	MAP3	HDR'PRI'NO1,B,1			!Priority One
					!=IR
					!\2
	MAP3	HDR'PRI'NO2,B,1			!Priority Two
					!=IR
					!\2
	MAP2	HDR'OVR2,@HDR'OVR1		!Ovr2
					!=RA
	MAP3	HDR'PRI'NO(2),B,1		!Priorities Array
					!=IR
					!\2
	MAP2	HDR'LIN'COUNT,B,2		!Number Of Line Items
					!=IR
					!\4
	MAP2	HDR'LIN'TOTAL,B,5		!Order Total
					!=IR
					!\#,###,###.##-
	MAP2	HDR'LIN'DISC,B,5		!Discount Total
					!=IR
					!\#,###,###.##-
	MAP2	HDR'SHP'TOTAL,B,5		!Shippable Total
					!=IR
					!\#,###,###.##-
	MAP2	HDR'SHP'DISC,B,5		!Shippable Line Discount
					!=IR
					!\#,###,###.##-
	MAP2	HDR'OVR9,X			!Ovr9
					!=XRA
	MAP3	HDR'SHP'ODISC,B,5		!Shippable Order Discount
					!=IR
					!\#,###,###.##-
	MAP3	HDR'SHP'RETRO,B,5		!Shippable Retroactive Disc
					!=IR
					!\#,###,###.##-
	MAP3	HDR'SHP'VTTL(7),B,5		!Ship Via Totals
					!=IR
					!\#,###,###.##-
	MAP2	HDR'OVR10,@HDR'OVR9		!Ovr10
					!=RA
	MAP3	HDR'VIA'SUM(2),B,5		!Ship Via 2=BOX, 1=ORDER
					!=IR
					!\#,###,###.##-
	MAP3	HDR'CD(6),B,5			!Charges & Discounts
					!=IR
					!\#,###,###.##-
	MAP3	HDR'BVIA'NO,B,2			!Bill Ship Via
					!=IR
					!\####
	MAP3	HDR'ZONE'PERC,B,1		!Percent or Zone free frt
					!=IR
					!\##
	MAP3	HDR'BIT'FLAG,B,1		!More bits (see MAN'FLAG)
					!=IR
					!\##
	MAP3	HDR'RTN'CCCC,B,1		!Return Code not used
					!=IR
					!\##
	MAP2	HDR'OLD'CASH,B,5		!Old cash Applied
					!=IR
					!\#,###,###.##-
	MAP2	HDR'OLD'DR,B,5			!Old DR Applied
					!=IR
					!\#,###,###.##-
	MAP2	HDR'OLD'CR,B,5			!Old CR Applied
					!=IR
					!\#,###,###.##-
	MAP2	HDR'LIN'BO,B,2			!Line BO Count
					!=IR
					!\4
	MAP2	HDR'ORDER'VER,B,1		!Order version
					!=IR
					!\2
	MAP2	HDR'PICK'VER,B,1		!Picking Version
					!=IR
					!\2
	MAP2	HDR'ORDER'DTT,B,3		!Order Date
					!=DIR
					!\8
	MAP2	HDR'PROM'DTT,B,3		!Promised Date
					!=DIR
					!\8
	MAP2	HDR'OVR3,X			!Ovr3
					!=XRA
	MAP3	HDR'SHIP'DTT,B,3		!Ship Date
					!=DIR
					!\8
	MAP2	HDR'OVR4,@HDR'OVR3		!Ovr4
					!=RA
	MAP3	HDR'PSTDT,B,3			!Posting (Shipment) Date
					!=DIR
					!\8
	MAP2	HDR'PSTID,S,6			!Posting ID
					!=RCAI
	MAP2	HDR'SLOC,B,1			!Shipping Location
					!=IR
					!\1
	MAP2	HDR'LSTDT,B,2			!Last Date Updated
					!=JIR
					!\4
	MAP2	HDR'LSTID,S,6			!Last User To Update
					!=RCAI
	MAP2	HDR'CRDT,B,3			!Creation Date
					!=DIR
					!\8
	MAP2	HDR'CRID,S,6			!Creator's ID
					!=RCAI
	MAP2	HDR'ACTION,S,1			!Action (For Printing)
					!=RCA
	MAP2	HDR'PQU'STAT,S,1		!Print Queue Stat
					!=RCA
	MAP2	HDR'STX1ABLE,B,5		!Line Item Taxable Total
					!=IR
					!\#,###,###.##-
	MAP2	HDR'STX1FINAL,B,5		!Line Item Tax Total
					!=IR
					!\#,###,###.##-
	MAP2	HDR'STX2TOTAL,B,5		!Order Tax Total
					!=IR
					!\#,###,###.##-
	MAP2	HDR'STX2FINAL,B,5		!Order Tax Total (Shippable)
					!=IR
					!\#,###,###.##-
	MAP2	HDR'WEIGHT,B,5			!Weight
					!=IR
					!\#,###,###.##-
	MAP2	HDR'NAM'TYP'NO,B,2		!Customer Type
					!=IR
					!\4
	MAP2	HDR'PROC,B,1			!Order Processing Flag
					!=IR
					!\1
	MAP2	HDR'SHP'DONE,S,1		!Is Shipping Done?
					!=RN
	MAP2	HDR'STX'NO,B,2			!Sales Tax (last shipment)
					!=IR
					!\4
	MAP2	HDR'UDOC'TYPE,B,1		!Unposting Document Type
					!=IR
					!\1
	MAP2	HDR'INTERNAL,B,3		!Internal Invoice Number
					!=IR
					!\7
	MAP2	HDR'CCR'NO,B,1			!Customer Credit Card #
					!=IR
					!\1
	MAP2	HDR'OVR7,X			!Ovr7
					!=XRA
	MAP3	HDR'PRGNAM,S,6			!Last posted program name
					!=RPCA
	MAP2	HDR'OVR8,@HDR'OVR7		!Ovr8
					!=RA
	MAP3	HDR'CLOSE'LOC,B,2		!Closest location
					!=IR
					!\####
	MAP3	HDR'ORG'LOC,B,2			!Original location
					!=IR
					!\####
	MAP3	HDR'FFP'TOTAL,B,2		!Free freight header total
					!=IR
					!\####
	MAP2	HDR'SAV'OCASH,B,5		!Save Old Cash
					!=IR
					!\#,###,###.##-
	MAP2	HDR'PICK'NEED,S,1		!Is A Picking Ticket Needed?
					!=RN
	MAP2	HDR'CMG'NO,B,3			!Commission Group
					!=IR
					!\7
	MAP2	HDR'LIN'COM,B,2			!Line Committed Count
					!=IR
					!\4
	MAP2	HDR'BOX'COUNT,B,2		!Box count
					!=IR
					!\4
	MAP2	HDR'ZERO,S,2			!Zero Overlay For HHS
					!=RPCA
	MAP2	HDR'MULTI,B,1			!Multiple locations?
					!=IR
					!\1
	MAP2	HDR'MAN'FLAG,B,1		!Manual entry (cash,dr,cr)
					!=IR
					!\1
	MAP2	HDR'MAN'DTT,B,3			!Manifest date
					!=DIR
					!\8
	MAP2	HDR'MSGNO,B,3			!Message Chain
					!=IR
					!\7
	MAP2	HDR'MAN'YN,S,1			!Waiting On Manifest?
					!=RY
	MAP2	HDR'SHP'ZIP,S,9			!Shipping Zip Code
					!=RPCA
	MAP2	HDR'MSGNO2,B,3			!Second Message Number
					!=IR
					!\7
	MAP2	HDR'OVR5,X			!Overlay
					!=XRA
	MAP3	HDR'TMP'UC,B,5			!Temp up charge (int=5)
					!=IR
					!\#,###,###.##-
	MAP2	HDR'OVR6,@HDR'OVR5		!Overlay
					!=RA
	MAP3	HDR'TAXABLE,S,1			!Taxable
					!=RA
	MAP3	HDR'REPRINT'YN,S,1		!Reprint Counter Receipt
					!=RCA
	MAP3	HDR'OVR'FILL1,S,3		!Overlay Filler
					!=RAI
	MAP2	HDR'FREE'FRT,S,1		!Free Freight
					!=BFRY
					!|CUS'FREE'FRT
	MAP2	HDR'COMPLETE,S,1		!Complete Flag (HHS Only)
					!=RN
	MAP2	HDR'POST'LEVEL,B,1		!Posting Level
					!=IR
					!\2
	MAP2	HDR'CART'BIN,B,1		!Bin In Cart
					!=IR
					!\2
	MAP2	HDR'SHIP'BLIND,S,1		!Ship Blind
					!=RPCA
	MAP2	HDR'WGT'L,B,3			!Weight low sales
					!=IR
					!\7
	MAP2	HDR'WGT'S,B,3			!Weight standard sales
					!=IR
					!\7
	MAP2	HDR'LSM,S,1			!Low, Std, or Mixed
					!=BRA
	MAP2	HDR'SUB'NO,B,5			!EZID Number If Web Order
					!=IR
					!\11
	MAP2	HDR'BULK,S,1			!Bulk pick?
					!=BRN
					!|"N"
	MAP2	HDR'SHP'COU,S,3			!Shipping Country
					!=RCA
	MAP2	HDR'SHP'ST,S,2			!Shipping State
					!=RCA
	MAP2	HDR'TAX'LOC,S,4			!U-Haul Taxing Location
					!=RAI
	MAP2	HDR'TAX'PCT,B,3			!U-Haul Tax Percentage
					!=IR
					!\7
	MAP2	HDR'DEL'LOC,S,4			!Delivery Taxing Location
					!=RAI
	MAP2	HDR'DEL'PCT,B,3			!Delivery Tax Percentage
					!=IR
					!\7
	MAP2	HDR'REMOTE,S,1			!Order Info Sent To Big Box
					!=RCAI
	MAP2	HDR'ACTUAL'FRT,B,5		!Actual Freight Charged
					!=IR
					!\11
	MAP2	HDR'TUB,B,2			!Tub number for WL
					!=IR
					!\4
	MAP2	HDR'LVIA'NO(5),B,2		!Rate Shop Via Numbers
					!=IR
					!\4
	MAP2	HDR'ORG'VIA'NO,B,2		!Original Ship Via No
					!=IR
					!\4
	MAP2	HDR'WGT'A,B,3			!Weight AIR sales
					!=IR
					!\7
	MAP2	HDR'AMAZON'YN,S,1		!Amazon Y If Amazon Order
					!=FBRN
					!|"N"
	MAP2	HDR'BAD'TIH'NO,B,2		!Filler
					!=IR
					!\4
	MAP2	HDR'ZONE,S,4			!Packing ship zone
					!=BRA
	MAP2	HDR'SPI'WGT,B,2			!Special packing weight
					!=IR
					!\4
	MAP2	HDR'ASPI'WGT,B,2		!Actual Special packing weight
					!=IR
					!\4
	MAP2	HDR'LPKG'BOX,B,2		!Last Packaging/heavy Box
					!=IR
					!\4
	MAP2	HDR'TIH'NO,B,4			!Picking ticket in WL
					!=IR
					!\8
	MAP2	HDR'PUL'UID,S,6			!Puller user id
					!=RAI
	MAP2	HDR'CHK'UID,S,6			!Checker user id
					!=RAI
	MAP2	HDR'MOV'NO,B,3			!Movement Ticket Number
					!=IR
					!\7
	MAP2	HDR'BAD'TRACK,S,1		!Y=Invalid tracking number
					!=RCA
	MAP2	HDR'HOW'PACK,S,1		!C=Conv,A=Alg,S=Switch,W=Wgt
					!=RCA
	MAP2	HDR'FILLIT2,S,68		!Fillit2 filler, main one
					!=RAI
	MAP2	HDR'PUSTAMP,B,4			!Time stamp for cus pick up
					!=GIR
					!\8
	MAP2	HDR'SKIP'AVS,S,1		!Skip AVS on this order
					!=RCA
	MAP2	HDR'HAZ'YN,S,1			!Hazardous Materials On Order
					!=RCA
	MAP2	HDR'BQUEUE,S,1			!Bagger queue on AIX?
					!=RCAI
	MAP2	HDR'MADE'UP,B,1			!1=made up weight/l/w/h
					!=IR
					!\2
	MAP2	HDR'FLENGTH,B,2			!Fudge Length for bagger
					!=IR
					!\4
	MAP2	HDR'FWIDTH,B,2			!Fudge Width for bagger
					!=IR
					!\4
	MAP2	HDR'FHEIGHT,B,2			!Fudge Height for bagger
					!=IR
					!\4
	MAP2	HDR'TSAFETY,S,1			!Transfer To Safety?
					!=BFRN
					!|"N"
	MAP2	HDR'BREASON,S,1			!Bagger reason failed
					!=RCAI
	MAP2	HDR'WMI2'PRT,S,1		!Was label 2 printed bagger?
					!=RCAI
	MAP2	HDR'BLENGTH,B,5			!Length for bagger
					!=IR
					!\11
	MAP2	HDR'BWIDTH,B,5			!Width for bagger
					!=IR
					!\11
	MAP2	HDR'BHEIGHT,B,5			!Height for bagger
					!=IR
					!\11
	MAP2	HDR'BWEIGHT,B,5			!Height for bagger
					!=IR
					!\11
	MAP2	HDR'BAGGER,S,1			!Bagger
					!=RCAI
	MAP2	HDR'SPK'NO,B,1			!Standard Packaging Number
					!=IR
					!\2
	MAP2	HDR'PKGS'TYPE,S,1		!Packaging Type
					!=RCA
	MAP2	HDR'TIT'DAYS,B,1		!Time In Transit Days
					!=IR
					!\1
	MAP2	HDR'RELEASE'YN,S,1		!Order release called worked
					!=RCA
	MAP2	HDR'LEAD'ITEMS,S,1		!Only Allow Lead Items On Quote
					!=RCA
	MAP2	HDR'WMI'RCN,B,3			!Walmart RCN when shipped
					!=IR
					!\7
	MAP2	HDR'OBC'NO,B,2			!WL Order batch number
					!=IR
					!\4
	MAP2	HDR'MULTI'PO,S,1		!Multiple PO's per Order
					!=RCA
	MAP2	HDR'RES'YN,S,1			!Residential flag
					!=RCAI
	MAP2	HDR'FFP'NO,B,2			!Free freight profile
					!=IR
					!\4
	MAP2	HDR'PP'YN,S,1			!Pick/Pack order?
					!=BRY
	MAP2	HDR'WBV'CODE,S,2		!WBV code for Walmart
					!=RCA
	MAP2	HDR'SPECIAL,S,1			!Special processing
					!=BRCA
	MAP2	HDR'MAKE'PO,S,1			!Low, Std, or Mixed
					!=BFRY
					!|"Y"
	MAP2	HDR'PICK'STAT,B,1		!Picking Status
					!=IR
					!\2
	MAP2	HDR'TSTAMP,B,4			!Time stamp (added usually)
					!=GIR
					!\8
	MAP2	HDR'FPICK'UDT,B,4		!First Picking Date/Time
					!=GIR
					!\8
	MAP2	HDR'LPICK'UDT,B,4		!Last Picking Date/time
					!=GIR
					!\8
	MAP2	HDR'MAN'UDT,B,4			!Manifset Date/Time
					!=GIR
					!\8
	MAP2	HDR'APOI,S,22			!Additional PO Information
					!=RAI
	MAP2	HDR'CHANNEL,B,2			!Sales Channel
					!=IR
					!\4
!K:OEHDR
!K:OEHDRC
!K:OEHDRL
17777  MAP1    HDR'RCC,@HDR'REC
17778          MAP2    HDR'SPACE,S,512
18000  MAP1    HDR'AFR,S,512
