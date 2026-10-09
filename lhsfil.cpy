!CMP 
MAP1   LHS'REC
	MAP2	LHS'CNO,B,1			!Company Number
					!=IR
					!\2
	MAP2	LHS'XXX'OVR,X			!
					!=XRA
	MAP3	LHS'KOVR1,X			!
					!=XRA
	MAP4	LHS'DOC'TYPE,B,1		!Document Type
					!=IR
					!\1
	MAP4	LHS'HDR'LOC'NO,B,2		!Location Of Order Header
					!=IR
					!\4
	MAP3	LHS'KOVR2,@LHS'KOVR1		!LEAVE ALONE
					!=XRA
	MAP4	LHS'KEYFLD,B,3			!Keyfield Overlay
					!=IR
					!\7
	MAP3	LHS'HDR'NO,B,3			!Header Number
					!=IR
					!\7
	MAP3	LHS'USR'OQTY,B,5		!User Order QTY
					!=IR
					!\11
	MAP3	LHS'USR'BQTY,B,5		!User Back Order QTY
					!=IR
					!\11
	MAP3	LHS'USR'CQTY,B,5		!User Committed QTY
					!=IR
					!\11
	MAP3	LHS'ORDER'QTY,B,5		!Currently On Order
					!=IR
					!\11
	MAP3	LHS'COM'QTY,B,5			!Committed Quantity
					!=IR
					!\11
	MAP3	LHS'BO'QTY,B,5			!Back Order Quantity
					!=IR
					!\11
	MAP3	LHS'LENGTH,B,4			!Length
					!=IR
					!\9
	MAP3	LHS'WIDTH,B,4			!Width
					!=IR
					!\9
	MAP3	LHS'HEIGHT,B,4			!Height
					!=IR
					!\9
	MAP3	LHS'PROC'OV1,X			!
					!=XRAI
	MAP4	LHS'RPROC,B,1			!Return Processing
					!=IR
					!\1
	MAP3	LHS'PROC'OVR2,@LHS'PROC'OV1	!Overlay
					!=XRAI
	MAP4	LHS'LOG'SO,B,1			!0=No Ord,1=No SO, 2=Done
					!=IR
					!\1
	MAP3	LHS'KIT'BUILD,S,1		!Kit Build?
					!=BRN
					!|"N"
	MAP3	LHS'QFRACTION,B,1		!Quantity Precision
					!=IR
					!\1
	MAP3	LHS'LFRACTION,B,1		!Length Precision
					!=IR
					!\1
	MAP3	LHS'XFRACTION,B,1		!Width Precision
					!=IR
					!\1
	MAP3	LHS'HFRACTION,B,1		!Height Precision
					!=IR
					!\1
	MAP2	LHS'XXX'RCC,@LHS'XXX'OVR	!Overlay
					!=XRA
	MAP3	LHS'XXX'SPACE,S,54		!Clear area
					!=RPCA
	MAP2	LHS'LOC'NO,B,2			!Location Of Inventory
					!=IR
					!\4
	MAP2	LHS'PFRACTION,B,1		!Price Precision
					!=IR
					!\1
	MAP2	LHS'NO,B,2			!Line Item Number
					!=IR
					!\4
	MAP2	LHS'SHIPMENT,B,2		!Shipment Number
					!=IR
					!\4
	MAP2	LHS'SHIP'DTT,B,3		!Shipment Date
					!=DIR
					!\8
	MAP2	LHS'TRK'NO,B,3			!Tracking Number
					!=IR
					!\7
	MAP2	LHS'UOM'NO,B,2			!Units Of Measure
					!=IR
					!\4
	MAP2	LHS'CALC'PRICE,B,5		!Calculated Price
					!=IR
					!\11
	MAP2	LHS'PRICE,B,5			!Shipped Price
					!=IR
					!\11
	MAP2	LHS'CALC'DISC,B,5		!Calculated Discount
					!=IR
					!\11
	MAP2	LHS'DISC,B,5			!Discount
					!=IR
					!\11
	MAP2	LHS'PROM'DTT,B,3		!Promised Dated
					!=DBIR
					!\8
					!|STR'PROM'DTT
					!`E'LHS'PROM'DTT
	MAP2	LHS'PRI'OVR,X			!
					!=XRA
	MAP3	LHS'PRI'NO1,B,1			!Priority 1
					!=BIR
					!\2
					!|HDR'PRI'NO1
					!`E'PRI'NO1
	MAP3	LHS'PRI'NO2,B,1			!Priority 2
					!=BIR
					!\2
					!|HDR'PRI'NO2
					!`E'PRI'NO2
	MAP2	LHS'PRI'OVR2,@LHS'PRI'OVR	!
					!=XRAI
	MAP3	LHS'PRI'NO(2),B,1		!Priority Array
					!=BIR
					!\2
	MAP2	LHS'PACK'FLAG,B,1		!Packing records???
					!=BIR
					!\2
	MAP2	LHS'OVERIDE,B,1			!Overide Available? (1="Y")
					!=BIR
					!\1
	MAP2	LHS'HOLD'OVR,X			!
					!=XRA
	MAP3	LHS'HOLD'YN,S,1			!Hold Flag
					!=RA
	MAP2	LHS'HOLD'OVR2,@LHS'HOLD'OVR	!
					!=XRAI
	MAP3	LHS'BATCH'YN,S,1		!In Batch?
					!=RA
	MAP2	LHS'SHPPED'QTY,B,5		!Shipped QTY
					!=IR
					!\11
	MAP2	LHS'NET,B,5			!Net Line Price
					!=IR
					!\11
	MAP2	LHS'REQ'DTT,B,3			!Required Date
					!=DIR
					!\8
	MAP2	LHS'SUB'COUNT,B,2		!Sub Item counter
					!=IR
					!\4
	MAP2	LHS'COVR1,X			!
					!=XRA
	MAP3	LHS'CMG'NO,B,3			!Commission group number
					!=IR
					!\7
	MAP2	LHS'COVR2,@LHS'COVR1		!Overlay
					!=XRA
	MAP3	LHS'NAM'NO,B,3			!Customer - for key only
					!=IR
					!\7
	MAP2	LHS'YOVR1,X			!
					!=XRA
	MAP3	LHS'EXT'TRK'NO,B,3		!External Tracking Number
					!=IR
					!\7
	MAP2	LHS'YOVR2,@LHS'YOVR1		!LEAVE ALONE
					!=XRA
	MAP3	LHS'SQ'FEET,B,3			!Square feet
					!=IR
					!\7
	MAP2	LHS'PRV'LOC'NO,B,2		!Previous Location Number
					!=IR
					!\4
	MAP2	LHS'PRV'TYPE,B,1		!Previous Order Type
					!=IR
					!\1
	MAP2	LHS'PRV'HDR'NO,B,3		!Previous Order Header Number
					!=IR
					!\7
	MAP2	LHS'MSGNO,B,3			!Message Chain
					!=IR
					!\5
	MAP2	LHS'T'ONLY,X			!T-Shirt only data
					!=XRA
	MAP3	LHS'TPRICE(3),B,5		!T-Shirt Price
					!=IR
					!\11
	MAP3	LHS'TCPRICE(3),B,5		!T-Shirt Price
					!=IR
					!\11
	MAP3	LHS'UOM'FACTOR(3),F,6		!Unit Of Measure Factors
					!=IR-L
					!\11
	MAP3	LHS'TORDER'QTY(6),B,5		!T-Shirt Order Quantities
					!=IR
					!\11
	MAP3	LHS'TCOM'QTY(6),B,5		!T-Shirt Comitted Quantities
					!=IR
					!\11
	MAP3	LHS'KTD'TRK'NO,B,3		!Part Number For Kit Substitute
					!=IR
					!\7
	MAP3	LHS'SLIP'PRICE,S,1		!Slip price Used?
					!=BFRN
	MAP3	LHS'CAP'PCEPRC,B,3		!Cap piece price
					!=IR
					!\7
	MAP3	LHS'CAP'HPRICE,B,3		!Cap 4th column price
					!=IR
					!\7
	MAP3	LHS'CAP'HFAC,B,1		!Cap 4th column factor
					!=IR
					!\2
	MAP3	LHS'CAP'PCOL,B,1		!Pricing Column (1-4)
					!=IR
					!\2
	MAP3	LHS'QFREE'FRT,S,1		!Quote Free Freight
					!=RCA
	MAP3	LHS'PROMOS,B,1			!Promos for sizes (bits)
					!=IR
					!\2
	MAP3	LHS'LS'BITS,B,1			!Low or standard sales bits
					!=IR
					!\2
	MAP3	LHS'AIR'BITS,B,1		!On is Air picking low sales
					!=IR
					!\2
	MAP3	LHS'TEMP,S,1			!Used in cache oepack
					!=RCA
	MAP3	LHS'TEMP2,S,1			!Used in cache oepack
					!=RCA
	MAP3	LHS'TEMP3,S,1			!Used in cache oepack
					!=RCA
	MAP3	LHS'T'FILL,X,1			!Filler
					!=XRA
	MAP2	LHS'R'ONLY,@LHS'T'ONLY		!Retail Only
					!=XRA
	MAP3	LHS'SCMG'NO(3),B,3		!Split commission numbers
					!=IR
					!\7
	MAP3	LHS'TAXABLE,S,1			!Taxable item?
					!=RY
	MAP3	LHS'DLOC'NO,B,2			!Delivery location 
					!=IR
					!\4
	MAP3	LHS'CONFIRM'YN,S,1		!Delivery confirmed?
					!=RY
	MAP3	LHS'PO'NUMBER,B,3		!Line PO Reference Number
					!=IR
					!\7
	MAP3	LHS'NSI'PKG,S,10		!NSI Package Number
					!=RAI
	MAP3	LHS'WAR'NO,B,3			!Warranty Number
					!=IR
					!\7
	MAP3	LHS'TAX'LOC'S,S			!
					!=RAI
	MAP3	LHS'TAX'PCT,B,3			!Tax Percentage
					!=IR
					!\7
	MAP3	LHS'VIA'NO,B,2			!Line Ship Via
					!=IR
					!\4
	MAP3	LHS'R'FILL,S,90			!Filler
					!=RAI
	MAP2	LHS'F'ONLY,@LHS'T'ONLY		!Fullfillment Only
					!=XRA
	MAP3	LHS'LINE'ID,B,5			!Line ID
					!=IR
					!\10
	MAP3	LHS'MOV'STAT,S,1		!Movement Status
					!=RCA
	MAP3	LHS'PLC'UID,S,6			!Pick/Pack order?
					!=RPCAI
	MAP3	LHS'SQTY,B,5			!Scanned QTY
					!=IR
					!\11
	MAP3	LHS'SERRS,B,2			!Scanned errors
					!=IR
					!\4
	MAP3	LHS'PQTY,B,5			!Picked QTY
					!=IR
					!\11
	MAP3	LHS'F'FILL,S,104		!Filler
					!=RAI
!K:OELHS
!K:OELHSP
!K:OELHSC
!K:OELHSD
17777  MAP1    LHS'RCC,@LHS'REC
17778          MAP2    LHS'SPACE,S,256
18000  MAP1    LHS'AFR,S,256
