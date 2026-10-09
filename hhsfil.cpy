!CMP 
MAP1   HHS'REC
	MAP2	HHS'CNO,B,1			!Company Number
					!=IR
					!\2
	MAP2	HHS'LOC'NO,B,2			!Location Number
					!=IR
					!\4
	MAP2	HHS'DOC'TYPE,B,1		!Document Type
					!=IR
					!\1
	MAP2	HHS'NO,B,3			!Order Number
					!=IR
					!\7
	MAP2	HHS'SHIPMENT,B,2		!Shipment Number
					!=IR
					!\4
	MAP2	HHS'QUOTE'YN,S,1		!Quote YN
					!=BRN
					!|"N"
	MAP2	HHS'GL'CNO,B,1			!G/L Company Number
					!=IR
					!\2
	MAP2	HHS'NAM'NO,B,3			!Customer/Vendor Number
					!=IR
					!\7
	MAP2	HHS'SHIP'TO,B,2			!Ship To Alternate Address
					!=IR
					!\4
	MAP2	HHS'DROP'YN,S,1			!Drop Ship Y/N
					!=BRN
					!|"N"
	MAP2	HHS'BO'YN,S,1			!Backorder Allowed Y/N
					!=BRY
					!|CUS'BO'YN
	MAP2	HHS'DEL'ALT'YN,S,1		!Delete Ship To Y/N
					!=BRN
					!|"N"
	MAP2	HHS'SHP'NO,B,3			!Ship To Drop Ship="Y" else 0
					!=IR
					!\7
	MAP2	HHS'TRH'NO,B,2			!Terms Number
					!=BIR
					!\4
					!||NAM|'TRH'NO
	MAP2	HHS'VIA'NO,B,2			!Ship VIA Number
					!=IR
					!\4
	MAP2	HHS'FOB'NO,B,2			!FOB Number
					!=IR
					!\4
	MAP2	HHS'PO,S,15			!Purchase Order Number
					!=RPCAI
	MAP2	HHS'SOURCE,S,2			!Source APID
					!=RCAI
	MAP2	HHS'HOLD'YN,S,1			!Hold Flag
					!=BRN
					!|"N"
	MAP2	HHS'OVR1,X			!Ovr1
					!=XRA
	MAP3	HHS'PRI'NO1,B,1			!Priority 1
					!=IR
					!\2
	MAP3	HHS'PRI'NO2,B,1			!Priority 2
					!=IR
					!\2
	MAP2	HHS'OVR2,@HHS'OVR1		!
					!=XRAI
	MAP3	HHS'PRI'NO(2),B,1		!Priorities
					!=IR
					!\2
	MAP2	HHS'LIN'COUNT,B,2		!Number Of Line Items
					!=IR
					!\4
	MAP2	HHS'LIN'TOTAL,B,5		!Total Order
					!=IR
					!\11
	MAP2	HHS'LIN'DISC,B,5		!Line Item Discount Total
					!=IR
					!\11
	MAP2	HHS'SHP'TOTAL,B,5		!Shipped Total
					!=IR
					!\11
	MAP2	HHS'SHP'DISC,B,5		!Shipped Line Discount
					!=IR
					!\11
	MAP2	HHS'OVR9,X			!Ovr9
					!=XRA
	MAP3	HHS'SHP'ODISC,B,5		!Shippable Order Discount
					!=IR
					!\#,###,###.##-
	MAP3	HHS'SHP'RETRO,B,5		!Shippable Retroactive Disc
					!=IR
					!\#,###,###.##-
	MAP3	HHS'SHP'VTTL(7),B,5		!Ship Via Totals
					!=IR
					!\#,###,###.##-
	MAP2	HHS'OVR10,@HHS'OVR9		!Ovr10
					!=RA
	MAP3	HHS'VIA'SUM(2),B,5		!Ship Via 2=BOX, 1=ORDER
					!=IR
					!\#,###,###.##-
	MAP3	HHS'CD(6),B,5			!Charges & Discounts
					!=IR
					!\#,###,###.##-
	MAP3	HHS'BVIA'NO,B,2			!Bill Ship Via
					!=IR
					!\####
	MAP3	HHS'ZONE'PERC,B,1		!Percent or Zone free frt
					!=IR
					!\##
	MAP3	HHS'BIT'FLAG,B,1		!More bits (see MAN'FLAG)
					!=IR
					!\##
	MAP3	HHS'RTN'CCCC,B,1		!Return Code not used
					!=IR
					!\##
	MAP2	HHS'OLD'CASH,B,5		!Old Cash Applied To Order
					!=IR
					!\11
	MAP2	HHS'OLD'DR,B,5			!Old Debits Applied To Order
					!=IR
					!\11
	MAP2	HHS'OLD'CR,B,5			!Old Credits Applied To Order
					!=IR
					!\11
	MAP2	HHS'LIN'BO,B,2			!Line Items W/ BO Count
					!=IR
					!\4
	MAP2	HHS'ORDER'VER,B,1		!Order Version Number
					!=IR
					!\2
	MAP2	HHS'PICK'VER,B,1		!Picking Ticket Version
					!=IR
					!\2
	MAP2	HHS'ORDER'DTT,B,3		!Order Date
					!=DIR
					!\8
	MAP2	HHS'PROM'DTT,B,3		!Promised Date
					!=DIR
					!\8
	MAP2	HHS'OVR3,X			!Ovr3
					!=XRA
	MAP3	HHS'SHIP'DTT,B,3		!Posting (Ship) Date
					!=DIR
					!\8
	MAP2	HHS'OVR4,@HHS'OVR3		!Ovr4
					!=XRAI
	MAP3	HHS'PSTDT,B,3			!Posting (Ship) Date
					!=DIR
					!\8
	MAP2	HHS'PSTID,S,6			!Posting User ID
					!=RA
	MAP2	HHS'SLOC,B,1			!Shipping location
					!=IR
					!\1
	MAP2	HHS'LSTDT,B,2			!Last Date Modified
					!=JIR
					!\4
	MAP2	HHS'LSTID,S,6			!Last User To Change
					!=RA
	MAP2	HHS'CRDT,B,3			!Creation Date
					!=DIR
					!\8
	MAP2	HHS'CRID,S,6			!Created User ID
					!=RA
	MAP2	HHS'ACTION,S,1			!Action To Take (Post, Etc)
					!=RCA
	MAP2	HHS'PQU'STAT,S,1		!Print Queue Status
					!=RCA
	MAP2	HHS'STX1ABLE,B,5		!Taxable Amount Sales Tax
					!=IR-
					!\11
	MAP2	HHS'STX1FINAL,B,5		!Total Sales Tax (this ship)
					!=IR-
					!\11
	MAP2	HHS'STX2TOTAL,B,5		!Entire Order Other Tax Total
					!=IR-
					!\11
	MAP2	HHS'STX2FINAL,B,5		!Total Other Tax (this ship)
					!=IR-
					!\11
	MAP2	HHS'WEIGHT,B,5			!Total WEIGHT
					!=IR-
					!\11
	MAP2	HHS'NAM'TYP'NO,B,2		!Customer type
					!=IR
					!\4
	MAP2	HHS'PROC,B,1			!Proccessing type
					!=BIR
					!\1
					!|1
					!`"|PROC(20)"
	MAP2	HHS'SHP'DONE,S,1		!Shipping done?
					!=BRY
	MAP2	HHS'STX'NO,B,2			!Sales Tax Number
					!=BIR
					!\4
	MAP2	HHS'UDOC'TYPE,B,1		!Unposting Invoice Type
					!=BIR
					!\1
	MAP2	HHS'INTERNAL,B,3		!Invoice Number
					!=BIR
					!\7
	MAP2	HHS'CCR'NO,B,1			!Customer Credit Card #
					!=IR
					!\1
	MAP2	HHS'OVR7,X			!Ovr7
					!=XRA
	MAP3	HHS'PRGNAM,S,6			!Last posted program name
					!=RPCA
	MAP2	HHS'OVR8,@HHS'OVR7		!Ovr8
					!=RA
	MAP3	HHS'CLOSE'LOC,B,2		!Closest location
					!=IR
					!\####
	MAP3	HHS'ORG'LOC,B,2			!Original location
					!=IR
					!\####
	MAP3	HHS'FFP'TOTAL,B,2		!Free freight header total
					!=IR
					!\####
	MAP2	HHS'SAV'OCASH,B,5		!Save Old Cash
					!=IR
					!\11
	MAP2	HHS'BATCH'YN,S,1		!Batch Y/N (N=not in batch)
					!=BRY
	MAP2	HHS'CMG'NO,B,3			!Commission group number
					!=IR
					!\7
	MAP2	HHS'LIN'COM,B,2			!Line Items W/ Commited Count
					!=IR
					!\4
	MAP2	HHS'BOX'COUNT,B,2		!Box count
					!=IR
					!\4
	MAP2	HHS'YEAR,B,1			!Year posted to
					!=WIR
					!\2
	MAP2	HHS'PERIOD,B,1			!Period posted to
					!=IR
					!\2
	MAP2	HHS'HDR'GONE,B,1		!Header Gone?
					!=IR
					!\1
	MAP2	HHS'INV'YN,S,1			!Invoice Printed?
					!=BRN
					!|"N"
	MAP2	HHS'MAN'DTT,B,3			!Manifest Date
					!=DIR
					!\8
	MAP2	HHS'MSGNO,B,3			!Message Chain
					!=IR
					!\5
	MAP2	HHS'MAN'YN,S,1			!Manifest Needed?
					!=RY
	MAP2	HHS'SHP'ZIP,S,9			!Shipping Zip Code
					!=RPCA
	MAP2	HHS'MSGNO2,B,3			!Second Message Number
					!=IR
					!\7
	MAP2	HHS'MARGIN,B,5			!Margin on this order
					!=IR
					!\#,###,###.##-
	MAP2	HHS'FREE'FRT,S,1		!Free Freight
					!=BFRY
					!|CUS'FREE'FRT
	MAP2	HHS'COMPLETE,S,1		!Complete Flag
					!=RN
	MAP2	HHS'POST'LEVEL,B,1		!Posting Level
					!=IR
					!\2
	MAP2	HHS'MARGINF,B,1			!Margin computed flag
					!=BFIR
					!\1
	MAP2	HHS'SHIP'BLIND,S,1		!Ship Blind
					!=RCA
	MAP2	HHS'WGT'L,B,3			!Weight low sales
					!=IR
					!\7
	MAP2	HHS'WGT'S,B,3			!Weight standard sales
					!=IR
					!\7
	MAP2	HHS'LSM,S,1			!Low, Std, or Mixed
					!=BRA
	MAP2	HHS'SUB'NO,B,5			!EZID Number If Web Order
					!=IR
					!\11
	MAP2	HHS'BULK,S,1			!Bulk pick?
					!=BRN
					!|"N"
	MAP2	HHS'SHP'COU,S,3			!Ship Country
					!=RCA
	MAP2	HHS'SHP'ST,S,2			!Ship State
					!=RCA
	MAP2	HHS'TAX'LOC,S,4			!U-Haul Taxing Location
					!=RAI
	MAP2	HHS'TAX'PCT,B,3			!U-Haul Tax Percentage
					!=IR
					!\7
	MAP2	HHS'DEL'LOC,S,4			!Delivery Taxing Location
					!=RAI
	MAP2	HHS'DEL'PCT,B,3			!Delivery Tax Percentage
					!=IR
					!\7
	MAP2	HHS'REMOTE,S,1			!Order Info Sent To Big Box
					!=RCAI
	MAP2	HHS'ACTUAL'FRT,B,5		!Actual Freight Charged
					!=IR
					!\11
	MAP2	HHS'TUB,B,2			!Tub number for WL
					!=IR
					!\4
	MAP2	HHS'LVIA'NO(5),B,2		!Rate Shop Via Numbers
					!=IR
					!\4
	MAP2	HHS'ORG'VIA'NO,B,2		!Original Ship Via No
					!=IR
					!\4
	MAP2	HHS'WGT'A,B,3			!Weight AIR sales
					!=IR
					!\7
	MAP2	HHS'AMAZON'YN,S,1		!Amazon Y If Amazon Order
					!=BFRN
					!|"N"
	MAP2	HHS'TMP'FLAG,B,1		!Temp for folder packing
					!=IR
					!\2
	MAP2	HHS'TMP'FILL,B,1		!Filler
					!=IR
					!\2
	MAP2	HHS'ZONE,S,4			!Packing ship zone
					!=BRA
	MAP2	HHS'SPI'WGT,B,2			!Special packing weight
					!=IR
					!\4
	MAP2	HHS'ASPI'WGT,B,2		!Actual Special packing weight
					!=IR
					!\4
	MAP2	HHS'LPKG'BOX,B,2		!Last Pkg/heavy Box
					!=IR
					!\4
	MAP2	HHS'TIH'NO,B,4			!Picking ticket in WL
					!=IR
					!\8
	MAP2	HHS'PUL'UID,S,6			!Puller user id
					!=RAI
	MAP2	HHS'CHK'UID,S,6			!Checker user id
					!=RAI
	MAP2	HHS'MOV'NO,B,3			!Movement Ticket Number
					!=IR
					!\7
	MAP2	HHS'BAD'TRACK,S,1		!Y=Invalid tracking number
					!=RCA
	MAP2	HHS'HOW'PACK,S,1		!C=Conv,A=Alg,S=Switch,W=Wgt
					!=RCA
	MAP2	HHS'FILLIT2,S,68		!Fillit2 filler, main one
					!=RAI
	MAP2	HHS'PUSTAMP,B,4			!Time stamp for cus pick up
					!=GIR
					!\8
	MAP2	HHS'SKIP'AVS,S,1		!Skip AVS on this order
					!=RCA
	MAP2	HHS'HAZ'YN,S,1			!Hazardous Materials On Order
					!=RCA
	MAP2	HHS'BQUEUE,S,1			!Bagger queue on AIX?
					!=RCAI
	MAP2	HHS'MADE'UP,B,1			!1=made up weight/l/w/h
					!=IR
					!\2
	MAP2	HHS'FLENGTH,B,2			!Fudge Length for bagger
					!=IR
					!\4
	MAP2	HHS'FWIDTH,B,2			!Fudge Width for bagger
					!=IR
					!\4
	MAP2	HHS'FHEIGHT,B,2			!Fudge Height for bagger
					!=IR
					!\4
	MAP2	HHS'TSAFETY,S,1			!Transfer To Safety?
					!=BFRN
					!|"N"
	MAP2	HHS'BREASON,S,1			!Bagger reason failed
					!=RCAI
	MAP2	HHS'WMI2'PRT,S,1		!Was label 2 printed bagger?
					!=RCAI
	MAP2	HHS'BLENGTH,B,5			!Length for bagger
					!=IR
					!\11
	MAP2	HHS'BWIDTH,B,5			!Width for bagger
					!=IR
					!\11
	MAP2	HHS'BHEIGHT,B,5			!Height for bagger
					!=IR
					!\11
	MAP2	HHS'BWEIGHT,B,5			!Height for bagger
					!=IR
					!\11
	MAP2	HHS'BAGGER,S,1			!Bagger
					!=RCAI
	MAP2	HHS'SPK'NO,B,1			!Standard Package
					!=IR
					!\1
	MAP2	HHS'PKGS'TYPE,S,1		!Packaging Type
					!=RCA
	MAP2	HHS'TIT'DAYS,B,1		!Time In Transit Days
					!=IR
					!\1
	MAP2	HHS'RELEASE'YN,S,1		!Order release worked
					!=RCA
	MAP2	HHS'LEAD'ITEMS,S,1		!Only Allow Lead Items On Quote
					!=RCA
	MAP2	HHS'WMI'RCN,B,3			!Walmart RCN when shipped
					!=IR
					!\7
	MAP2	HHS'OBC'NO,B,2			!WL Order batch number
					!=IR
					!\4
	MAP2	HHS'MULTI'PO,S,1		!Multiple PO's per Order
					!=RA
	MAP2	HHS'RES'YN,S,1			!Residential flag
					!=RCAI
	MAP2	HHS'FFP'NO,B,2			!Free freight profile
					!=IR
					!\4
	MAP2	HHS'PP'YN,S,1			!Pick/Pack order?
					!=BRY
	MAP2	HHS'WBV'CODE,S,2		!WBV code for Walmart
					!=RCA
	MAP2	HHS'SPECIAL,S,1			!Special processing
					!=BRCA
	MAP2	HHS'MAKE'PO,S,1			!Low, Std, or Mixed
					!=BFRY
					!|"Y"
	MAP2	HHS'PICK'STAT,B,1		!Picking Status
					!=IR
					!\2
	MAP2	HHS'TSTAMP,B,4			!Time stamp (added usually)
					!=GIR
					!\8
	MAP2	HHS'FPICK'UDT,B,4		!First Picking Date/Time
					!=GIR
					!\8
	MAP2	HHS'LPICK'UDT,B,4		!Last Picking Date/time
					!=GIR
					!\8
	MAP2	HHS'MAN'UDT,B,4			!Manifset Date/Time
					!=GIR
					!\8
	MAP2	HHS'APOI,S,22			!Additional PO Information
					!=RAI
	MAP2	HHS'CHANNEL,B,2			!Sales Channel
					!=IR
					!\4
!K:OEHHS
!K:OEHHSC
!K:OEHHSD
!K:OEHHSL
!K:OEHHSP
!K:OEHHSX
17777  MAP1    HHS'RCC,@HHS'REC
17778          MAP2    HHS'SPACE,S,512
18000  MAP1    HHS'AFR,S,512
