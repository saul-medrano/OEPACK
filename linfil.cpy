!CMP 
MAP1   LIN'REC
	MAP2	LIN'CNO,B,1			!Company Number
					!=IR
					!\2
	MAP2	LIN'XXX'OVR,X			!Xxx'ovr
					!=XRA
	MAP3	LIN'KOVR1,X			!
					!=XRA
	MAP4	LIN'DOC'TYPE,B,1		!Document Type
					!=IR
					!\1
	MAP4	LIN'HDR'LOC'NO,B,2		!Location Of Order Header
					!=IR
					!\4
	MAP3	LIN'KOVR2,@LIN'KOVR1		!LEAVE ALONE
					!=XRA
	MAP4	LIN'KEYFLD,B,3			!Keyfield Overlay
					!=IR
					!\7
	MAP3	LIN'HDR'NO,B,3			!Header Number
					!=IR
					!\7
	MAP3	LIN'USR'OQTY,B,5		!User Order QTY
					!=IR
					!\11
	MAP3	LIN'USR'BQTY,B,5		!User Back Order QTY
					!=IR
					!\11
	MAP3	LIN'USR'CQTY,B,5		!User Committed QTY
					!=IR
					!\11
	MAP3	LIN'ORDER'QTY,B,5		!Currently On Order
					!=IR
					!\11
	MAP3	LIN'COM'QTY,B,5			!Committed Quantity
					!=IR
					!\11
	MAP3	LIN'BO'QTY,B,5			!Back Order Quantity
					!=IR
					!\11
	MAP3	LIN'LENGTH,B,4			!Length
					!=IR
					!\9
	MAP3	LIN'WIDTH,B,4			!Width
					!=IR
					!\9
	MAP3	LIN'HEIGHT,B,4			!Height
					!=IR
					!\9
	MAP3	LIN'PROC'OV1,X			!
					!=XRAI
	MAP4	LIN'RPROC,B,1			!Return Processing
					!=IR
					!\1
	MAP3	LIN'PROC'OVR2,@LIN'PROC'OV1	!Overlay
					!=XRAI
	MAP4	LIN'LOG'SO,B,1			!0=No Ord,1=No SO, 2=Done
					!=IR
					!\1
	MAP3	LIN'KIT'BUILD,S,1		!Kit Build?
					!=BRN
					!|"N"
	MAP3	LIN'QFRACTION,B,1		!Quantity Precision
					!=IR
					!\1
	MAP3	LIN'LFRACTION,B,1		!Length Precision
					!=IR
					!\1
	MAP3	LIN'XFRACTION,B,1		!Width Precision
					!=IR
					!\1
	MAP3	LIN'HFRACTION,B,1		!Height Precision
					!=IR
					!\1
	MAP2	LIN'XXX'RCC,@LIN'XXX'OVR	!Overlay
					!=XRA
	MAP3	LIN'XXX'SPACE,S,54		!Clear area
					!=RPCA
	MAP2	LIN'LOC'NO,B,2			!Location Of Inventory
					!=IR
					!\4
	MAP2	LIN'PFRACTION,B,1		!Price Precision
					!=IR
					!\1
	MAP2	LIN'NO,B,2			!Line Item Number
					!=IR
					!\4
	MAP2	LIN'SHIPMENT,B,2		!Shipment Number
					!=IR
					!\4
	MAP2	LIN'SHIP'DTT,B,3		!Shipment Date
					!=DIR
					!\8
	MAP2	LIN'TRK'NO,B,3			!Tracking Number
					!=IR
					!\7
	MAP2	LIN'UOM'NO,B,2			!Units Of Measure
					!=IR
					!\4
	MAP2	LIN'CALC'PRICE,B,5		!Calculated Price (Sq YDS)
					!=IR
					!\11
	MAP2	LIN'PRICE,B,5			!Shipped Price (Sq FEET)
					!=IR
					!\11
	MAP2	LIN'CALC'DISC,B,5		!Calculated Discount
					!=IR
					!\11
	MAP2	LIN'DISC,B,5			!Discount
					!=IR
					!\11
	MAP2	LIN'PROM'DTT,B,3		!Promised Dated
					!=DBIR
					!\8
					!|STR'PROM'DTT
					!`E'LIN'PROM'DTT
	MAP2	LIN'PRI'OVR,X			!
					!=XRA
	MAP3	LIN'PRI'NO1,B,1			!Priority 1
					!=BIR
					!\2
					!|HDR'PRI'NO1
					!`E'PRI'NO1
	MAP3	LIN'PRI'NO2,B,1			!Priority 2
					!=BIR
					!\2
					!|HDR'PRI'NO2
					!`E'PRI'NO2
	MAP2	LIN'PRI'OVR2,@LIN'PRI'OVR	!
					!=XRAI
	MAP3	LIN'PRI'NO(2),B,1		!Priority Array
					!=BIR
					!\2
	MAP2	LIN'PACK'FLAG,B,1		!Packing records???
					!=BIR
					!\2
	MAP2	LIN'OVERIDE,B,1			!Overide Available? (1="Y")
					!=BIR
					!\1
	MAP2	LIN'HOLD'YN,S,1			!Hold Flag
					!=RA
	MAP2	LIN'SHPPED'QTY,B,5		!Shipped QTY
					!=IR
					!\11
	MAP2	LIN'NET,B,5			!Net Line Price
					!=IR
					!\11
	MAP2	LIN'REQ'DTT,B,3			!Required Date
					!=DIR
					!\8
	MAP2	LIN'SUB'COUNT,B,2		!Sub Item counter
					!=IR
					!\4
	MAP2	LIN'CMG'NO,B,3			!Commission group number
					!=IR
					!\7
	MAP2	LIN'YOVR1,X			!
					!=XRA
	MAP3	LIN'EXT'TRK'NO,B,3		!External Tracking Number
					!=IR
					!\7
	MAP2	LIN'YOVR2,@LIN'YOVR1		!LEAVE ALONE
					!=XRA
	MAP3	LIN'SQ'FEET,B,3			!Square feet
					!=IR
					!\7
	MAP2	LIN'PRV'LOC'NO,B,2		!Previous Location Number
					!=IR
					!\4
	MAP2	LIN'PRV'TYPE,B,1		!Previous Order Type
					!=IR
					!\1
	MAP2	LIN'PRV'HDR'NO,B,3		!Previous Order Header Number
					!=IR
					!\7
	MAP2	LIN'MSGNO,B,3			!Message Chain
					!=IR
					!\5
	MAP2	LIN'T'ONLY,X			!T-Shirt only data
					!=XRA
	MAP3	LIN'TPRICE(3),B,5		!T-Shirt Prices
					!=IR
					!\11
	MAP3	LIN'TCPRICE(3),B,5		!T-Shirt Calculated Prices
					!=IR
					!\11
	MAP3	LIN'UOM'FACTOR(3),F,6		!Unit Of Measure Factors
					!=IR-L
					!\11
	MAP3	LIN'TORDER'QTY(6),B,5		!T-Shirt Order Quantities
					!=IR
					!\11
	MAP3	LIN'TCOM'QTY(6),B,5		!T-Shirt Committed Quantities
					!=IR
					!\11
	MAP3	LIN'KTD'TRK'NO,B,3		!Part Number For Kit Substitute
					!=IR
					!\7
	MAP3	LIN'SLIP'PRICE,S,1		!Slip price Used?
					!=BFRN
	MAP3	LIN'CAP'PCEPRC,B,3		!Cap piece price
					!=IR
					!\7
	MAP3	LIN'CAP'HPRICE,B,3		!Cap 4th column price
					!=IR
					!\7
	MAP3	LIN'CAP'HFAC,B,1		!Cap 4th column factor
					!=IR
					!\2
	MAP3	LIN'CAP'PCOL,B,1		!Pricing Column (1-4)
					!=IR
					!\2
	MAP3	LIN'QFREE'FRT,S,1		!Quote Free Freight
					!=RCA
	MAP3	LIN'PROMOS,B,1			!Promos for sizes (bits)
					!=IR
					!\2
	MAP3	LIN'LS'BITS,B,1			!Low or standard sales bits
					!=IR
					!\2
	MAP3	LIN'AIR'BITS,B,1		!On is Air picking low sales
					!=IR
					!\2
	MAP3	LIN'TEMP,S,1			!Used in cache oepack
					!=RCA
	MAP3	LIN'TEMP2,S,1			!Used in cache oepack
					!=RCA
	MAP3	LIN'TEMP3,S,1			!Used in cache oepack
					!=RCA
	MAP3	LIN'T'FILL,X,1			!Filler
					!=XRA
	MAP2	LIN'R'ONLY,@LIN'T'ONLY		!Retail Only
					!=XRA
	MAP3	LIN'SCMG'NO(3),B,3		!Split commission numbers
					!=IR
					!\7
	MAP3	LIN'TAXABLE,S,1			!Taxable item?
					!=RY
	MAP3	LIN'DLOC'NO,B,2			!Delivery location 
					!=IR
					!\4
	MAP3	LIN'CONFIRM'YN,S,1		!Delivery confirmed?
					!=RN
	MAP3	LIN'PO'NUMBER,B,3		!Line PO Reference Number
					!=IR
					!\7
	MAP3	LIN'NSI'PKG,S,10		!NSI Package number
					!=RAI
	MAP3	LIN'WARN'NO,B,3			!Warranty Number
					!=IR
					!\7
	MAP3	LIN'TAX'LOC,S,4			!Taxing Location
					!=RAI
	MAP3	LIN'TAX'PCT,B,3			!Tax Percentage
					!=IR
					!\7
	MAP3	LIN'VIA'NO,B,2			!Line Ship Via
					!=IR
					!\4
	MAP3	LIN'R'FILL,S,90			!Filler
					!=RAI
	MAP2	LIN'F'ONLY,@LIN'T'ONLY		!Fullfillment Only
					!=XRA
	MAP3	LIN'LINE'ID,B,5			!Line ID
					!=IR
					!\10
	MAP3	LIN'MOV'STAT,S,1		!Movement Status
					!=RCA
	MAP3	LIN'PLC'UID,S,6			!Pick/Pack order?
					!=RPCAI
	MAP3	LIN'SQTY,B,5			!Scanned QTY
					!=IR
					!\11
	MAP3	LIN'SERRS,B,2			!Scanned errors
					!=IR
					!\4
	MAP3	LIN'PQTY,B,5			!Picked QTY
					!=IR
					!\11
	MAP3	LIN'F'FILL,S,104		!Filler
					!=RAI
!K:OELIN
!K:OELINB
!K:OELINP
!K:OELINL
17777  MAP1    LIN'RCC,@LIN'REC
17778          MAP2    LIN'SPACE,S,256
18000  MAP1    LIN'AFR,S,256
