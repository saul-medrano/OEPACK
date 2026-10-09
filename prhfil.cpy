! @#$^&* 21 24 0 0 0 0 0 0 0 0 0 0 0 *&^$#@ 
!CMP 
MAP1   PRH'REC
	MAP2	PRH'CNO,B,1			!Company Number
					!=IR
					!\2
	MAP2	PRH'TRK'NO,B,3			!Tracking number
					!=IR
					!\7
	MAP2	PRH'PART,S,20			!Part number with multi divisio
					!=RPCA
	MAP2	PRH'POVR1,@PRH'PART		!Overlay
					!=XRA
	MAP3	PRH'MILL,S,2			!Mill
					!=RPCA
	MAP3	PRH'STYLE,S,9	 		!Style
					!=RPCA
	MAP3	PRH'STYLEX,S,1			!Double X-tra Large
					!=RPCA
	MAP3	PRH'COLOR,S,2	 		!Color
					!=RPCA
	MAP3	PRH'PFIL,S,6	 		!FILLER
					!=RPCA
	MAP2	PRH'DESC,S,30			!Description
					!=RPUA
	MAP2	PRH'PC'BY'LOC,S,1		!Price/Cost By Location
					!=BRN
					!`SEC'PC'BY'LOC
	MAP2	PRH'PRC'COUNT,B,2		!Price Records Count
					!=IR
					!\4
	MAP2	PRH'KTD'COUNT,B,2		!Kit Count
					!=IR
					!\4
	MAP2	PRH'KTD'FLAG,S,1		!Is Part A Kit
					!=BRCA
					!|"S"
					!`"|KTDFLG(18)"
	MAP2	PRH'KTD'AVAIL,S,1		!Available In Kit Etc
					!=BRCA
					!|"S"
					!`"|KTDAVL(18)"
	MAP2	PRH'HAZ'NO,B,1			!Hazardous Material Code
					!=IR
					!\2
	MAP2	PRH'AUTO'PO,B,1			!Auto PO flag
					!=IR
					!\2
	MAP2	PRH'CWR'NO,B,2			!Customer Warranty Code
					!=IR
					!\4
	MAP2	PRH'MWR'NO,B,2			!MFG Warranty Code
					!=IR
					!\4
	MAP2	PRH'SELL'UOM,B,2		!Default Sell UOM
					!=IR
					!\4
	MAP2	PRH'LOT'YN,S,1			!Use lot numbers (Y/N)?
					!=BRN
					!|"N"
	MAP2	PRH'USE'SRL'YN,S,1		!Use Serial Numbers
					!=BRN
					!|"N"
	MAP2	PRH'INVENTORY,S,1		!Inventoried Item
					!=BRY
					!|"Y"
	MAP2	PRH'ACTIVE,S,1			!Active Flag
					!=BRCA
					!|"Y"
					!`"|PRHACT(13)"
	MAP2	PRH'TAXABLE,S,1			!Taxable Flag
					!=BRY
					!|"Y"
	MAP2	PRH'STX'NO,B,2			!Extra Tax Number
					!=IR
					!\4
					!`E'STX'NO
	MAP2	PRH'PLN'NO,B,2			!G/L Product Line
					!=IR
					!\4
	MAP2	PRH'SEC'NO,B,2			!Catalog Product Line
					!=IR
					!\4
	MAP2	PRH'QFRACTION,B,1		!Precision Of Quantity
					!=IR
					!\2
	MAP2	PRH'PFRACTION,B,1		!Precision Of Price
					!=BIR
					!\1
					!|2
					!`"<6"
	MAP2	PRH'CFRACTION,B,1		!Precision Of Cost
					!=BIR
					!\1
					!|PFRACTION
					!`"<6"
	MAP2	PRH'USC'NO,S,4			!US customs number
					!=RPCAI
	MAP2	PRH'WGTS'BY,S,1			!Using pricing by for weights
					!=BRN
					!`IN2'WGTS'BY
	MAP2	PRH'ON'SALE,S,1			!Part On Sale
					!=RCA
	MAP2	PRH'EXPIRE'DTT,B,3		!Sale Expiration Date
					!=DIR
					!\8
	MAP2	PRH'FILLX,S,1			!Filler
					!=BRPCAI
	MAP2	PRH'ATH'OVR1,X			!Ath'ovr1
					!=XRA
	MAP3	PRH'ATH'NO1,B,2			!Attribute 1
					!=IR
					!\4
	MAP3	PRH'ATH'NO2,B,2			!Attribute 2
					!=IR
					!\4
	MAP3	PRH'MATH'NO1,B,2		!Memo Attribute 1
					!=IR
					!\4
	MAP3	PRH'MATH'NO2,B,2		!Memo Attribute 2
					!=IR
					!\4
	MAP2	PRH'ATH'OVR2,@PRH'ATH'OVR1	!
					!=XRAI
	MAP3	PRH'ATH'NO(4),B,2		!Ath'no
					!=IR
					!\4
	MAP2	PRH'ATT'OVR1,X			!
					!=XRA
	MAP3	PRH'ATT'NO1,B,2			!Attribute Default 1
					!=IR
					!\4
	MAP3	PRH'ATT'NO2,B,2			!Attribute Default 2
					!=IR
					!\4
	MAP3	PRH'MATT'NO1,B,2		!Memo Attribute Default 1
					!=IR
					!\4
	MAP3	PRH'MATT'NO2,B,2		!Memo Default Attribute 2
					!=IR
					!\4
	MAP2	PRH'ATT'OVR2,@PRH'ATT'OVR1	!Att'ovr2
					!=XRAI
	MAP3	PRH'ATT'NO(4),B,2		!Att'no
					!=IR
					!\4
	MAP2	PRH'PRC'OVR,X			!
					!=XRAI
	MAP3	PRH'LOC'NO,B,2			!Location Number
					!=IR
					!\4
	MAP3	PRH'PRC'UOM'NO,B,2		!Price Uom No
					!=IR
					!\4
	MAP3	PRH'PRC'AMT,B,5			!Price Amount
					!=IR
					!\11
	MAP3	PRH'PRC'SAMT,B,5		!Set-Up Amount
					!=IR
					!\11
	MAP3	PRH'PRC'MAMT,B,5		!Min Amount
					!=IR
					!\11
	MAP3	PRH'PRC'LAMT,B,5		!Line Min Amount
					!=IR
					!\11
	MAP3	PRH'QTD'COUNT,B,2		!Qtd Record Count
					!=IR
					!\4
	MAP3	PRH'TYD'COUNT,B,2		!Type Discount Count
					!=IR
					!\4
	MAP3	PRH'SLD'COUNT,B,2		!Sld Record Count
					!=IR
					!\4
	MAP3	PRH'CSD'COUNT,B,2		!Customer Price
					!=IR
					!\4
	MAP3	PRH'QTD'BY'UOM,S,1		!Quantity Discount By UOM
					!=BFRY
					!|"Y"
	MAP3	PRH'TYD'BY'UOM,S,1		!Type Discounts By UOM
					!=BFRY
					!|SEC'TYD'BY'UOM
	MAP3	PRH'QTD'BY'USR,S,1		!Quantity Discount By User Qty
					!=BFRN
					!|"N"
	MAP3	PRH'SLD'DN,S,1			!Sales Discounts - Dollar/Num
					!=BRCA
					!|SEC'SLD'DN
					!`"|DOLNUM(20)"
	MAP3	PRH'SLD'RET'YN,S,1		!Sales Volume Retroactive?
					!=BFRY
					!|SEC'SLD'RET'YN
	MAP3	PRH'SLD'ORDSHP,S,1		!Volume Disc By Order/Shipable
					!=RA
	MAP3	PRH'QTY'STEP,B,5		!Quantity Break Step
					!=IR
					!\11
	MAP3	PRH'MIN'QTY,B,5			!Minimum Quantity Break
					!=IR
					!\11
	MAP2	PRH'OVR2,@PRH'PRC'OVR		!Ovr2
					!=XRAI
	MAP3	PRH'PRC'SPACE,S,48		!SPACE overlay for PRH'PRC
					!=RA
	MAP2	PRH'CMR'NO,B,2			!Commission Record
					!=IR
					!\4
					!|SEC'CMR'NO
	MAP2	PRH'XFRACTION,S,1		!Width Fraction
					!=RCA
	MAP2	PRH'LFRACTION,S,1		!Length Fraction
					!=RCA
	MAP2	PRH'HFRACTION,S,1		!Height Fraction
					!=RCA
	MAP2	PRH'CUTS'YN,S,1			!Use Cuts (Y/N)?
					!=BRN
					!|"N"
	MAP2	PRH'NEW'TRK'NO,B,3		!NEW Tracking number
					!=IR
					!\7
	MAP2	PRH'GTIN,S,15			!GTIN Number
					!=RA
	MAP2	PRH'LPG,B,1			!Link Page (for OE1HDR)
					!=IR
					!\2
	MAP2	PRH'PRC'TRK'NO,B,3		!Pricing Part Number
					!=IR
					!\7
	MAP2	PRH'PRC'PERC,B,3		!Pricing Percentage
					!=IR
					!\####.##
	MAP2	PRH'INTERFACE,B,1		!Interface
					!=BIR
					!\2
					!|SEC'INTERFACE
	MAP2	PRH'PMSGNO,B,3			!Prototype Message Chain
					!=IR
					!\5
	MAP2	PRH'PFORCE,S,1			!Force Prototype Message?
					!=RN
	MAP2	PRH'SPRGNAM,S,6			!Special Program Name For OE
					!=RCAI
	MAP2	PRH'ITP'NO,B,2			!Type Number
					!=IR
					!\4
					!|SEC'ITP'NO
	MAP2	PRH'MSGNO,B,3			!Message Chain
					!=IR
					!\5
	MAP2	PRH'TIME'OVR1,X			!Overlay for Time
					!=XRAI
	MAP3	PRH'TIME,B,2			!Time
					!=IR
					!\4
	MAP2	PRH'TIME'OVR2,@PRH'TIME'OVR1	!Overlay for style type
					!=XRAI
	MAP3	PRH'SGP'NO,S,2			!Style Group number
					!=RPCA
	MAP2	PRH'DEP'NO,B,2			!Department Number
					!=IR
					!\4
	MAP2	PRH'C'OVR1,X			!Overlay for Carpet width
					!=XRAI
	MAP3	PRH'CWIDTH,B,4			!Carpet width
					!=IR
					!\9
	MAP2	PRH'C'OVR2,@PRH'C'OVR1		!Overlay for Carpet width
					!=XRAI
	MAP3	PRH'HPRICE,B,4			!High qty price
					!=IR
					!\9
	MAP2	PRH'ODD'TRK'NO,B,3		!WGA Odd Size Part Number
					!=IR
					!\7
	MAP2	PRH'POFRACTION,B,1		!Precision Of PO's
					!=BIR
					!\1
					!|2
					!`"<6"
	MAP2	PRH'MEA'TRK'YN,S,1		!MEA Set Correctly?
					!=RN
	MAP2	PRH'MEA'UOM'NO,B,2		!MEA Unit of Measure
					!=BIR
					!\4
	MAP2	PRH'MEA'OVR,X			!Overlay for PRH
					!=XRAI
	MAP3	PRH'UOW'NO,B,2			!Unit of Weight
					!=BIR
					!\4
					!|SEC'UOW'NO
	MAP3	PRH'MEA'OVR1,X			!
					!=XRAI
	MAP4	PRH'SHP'WGT,B,5			!Gross Shipping Weight
					!=IR
					!\11
	MAP4	PRH'NET'WGT,B,5			!Net Shipping Weight
					!=IR
					!\11
	MAP4	PRH'CUBES,B,5			!Size in Cubes
					!=IR
					!\11
	MAP4	PRH'MEA'FILL,B,2		!Fill
					!=IR
					!\4
	MAP3	PRH'MEA'OVR2,@PRH'MEA'OVR1	!Ovr2
					!=XRAI
	MAP4	PRH'WGT(6),B,2			!Garment weights
					!=IR
					!\4
	MAP4	PRH'CASE'TO'1,B,1		!Cases To Treat As 1
					!=IR
					!\2
	MAP4	PRH'TREAT'CASE,S,1		!Treat As A Case?
					!=BRY
					!|"Y"
	MAP4	PRH'2ND'COL,B,1			!2nd Column
					!=IR
					!\2
	MAP4	PRH'DZ'PER'CS,B,1		!Dozens per case
					!=IR
					!\2
	MAP4	PRH'DIM'WGT,B,1			!Dimensional weight
					!=IR
					!\2
	MAP4	PRH'FREEFRT'YN,B,1		!Free Freight (y/n)
					!=BFIR
					!\1
					!|"Y"
	MAP2	PRH'ATT'OVR3,X			!
					!=XRA
	MAP3	PRH'ATT'DESC(6),S,2		!Attribute Descriptions
					!=RCA
	MAP2	PRH'ATT'OVR4,@PRH'ATT'OVR3	!
					!=XRAI
	MAP3	PRH'ATT'SPACE,S,12		!Attribute Space for tests
					!=RCA
	MAP2	PRH'CMG'NO,B,3			!Royalties
					!=IR
					!\7
	MAP2	PRH'TS'TYPE,S,1			!T-Shirt Type
					!=RCAI
	MAP2	PRH'COC'OVR1,X			!Overlay for Color Code
					!=XRAI
	MAP3	PRH'WLDE,S,1			!White/Light/Dark/etc
					!=RCAI
	MAP2	PRH'COC'OVR2,@PRH'COC'OVR1	!Overlay for color code
					!=XRAI
	MAP3	PRH'COC'NO,S,1			!White/Light/Dark/etc
					!=RCAI
	MAP2	PRH'CGH'NO,B,2			!Group Number
					!=IR
					!\4
	MAP2	PRH'CRDT,B,2			!Date Part First Entered
					!=JIR
					!\4
	MAP2	PRH'CRID,S,6			!Created By
					!=RCAI
	MAP2	PRH'LSTDT,B,2			!Last Update Date
					!=JIR
					!\4
	MAP2	PRH'LSTID,S,6			!Last Updated By
					!=RA
	MAP2	PRH'IA'JULIAN,B,2		!Julian Date Went Inactive
					!=JIR
					!\4
	MAP2	PRH'MILL'STYLE,S,10		!Mill Style Number
					!=RA
	MAP2	PRH'COMM'YN,S,1			!Give Commissions On This Part
					!=RY
	MAP2	PRH'UDT,B,4			!Used In Reports As Tmp Field
					!=GIR
					!\8
	MAP2	PRH'XCLUDE'SKU,S,1		!Exclude Sku From Reports
					!=RCA
	MAP2	PRH'ALL'BITS,B,2		!Bits For The WB-ALL File
					!=IR
					!\4
	MAP2	PRH'BRAND,S,30			!Brand Name
					!=RPA
	MAP2	PRH'ASIN,S,10			!Amazon Standard Id Number
					!=RA
	MAP2	PRH'FILLER,S,178		!Filler
					!=RAI
!K:INPRH
!K:INPRHD
!K:INPRHP
!K:INPRHU
!K:INPRHC
!K:INPRHS
17777  MAP1    PRH'RCC,@PRH'REC
17778          MAP2    PRH'SPACE,S,512
18000  MAP1    PRH'AFR,S,512
