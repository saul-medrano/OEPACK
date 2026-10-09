!CMP 
MAP1   SUB'REC
	MAP2	SUB'CNO,B,1			!Company Number
					!=IR
					!\2
	MAP2	SUB'XXX'OVR,X			!
					!=XRA
	MAP3	SUB'KOVR1,X			!
					!=XRA
	MAP4	SUB'DOC'TYPE,B,1		!Document Type
					!=IR
					!\1
	MAP4	SUB'HDR'LOC'NO,B,2		!Location Of Order Header
					!=IR
					!\4
	MAP3	SUB'KOVR2,@SUB'KOVR1		!LEAVE ALONE
					!=XRA
	MAP4	SUB'KEYFLD,B,3			!Keyfield Overlay
					!=IR
					!\7
	MAP3	SUB'HDR'NO,B,3			!Header Number
					!=IR
					!\7
	MAP3	SUB'USR'OQTY,B,5		!User Order QTY
					!=IR
					!\11
	MAP3	SUB'USR'BQTY,B,5		!User Back Order QTY
					!=IR
					!\11
	MAP3	SUB'USR'CQTY,B,5		!User Committed QTY
					!=IR
					!\11
	MAP3	SUB'ORDER'QTY,B,5		!Currently On Order
					!=IR
					!\11
	MAP3	SUB'COM'QTY,B,5			!Committed Quantity
					!=IR
					!\11
	MAP3	SUB'BO'QTY,B,5			!Back Order Quantity
					!=IR
					!\11
	MAP3	SUB'LENGTH,B,4			!Length
					!=IR
					!\9
	MAP3	SUB'WIDTH,B,4			!Width
					!=IR
					!\9
	MAP3	SUB'HEIGHT,B,4			!Height
					!=IR
					!\9
	MAP3	SUB'RPROC,B,1			!Return Processing
					!=IR
					!\1
	MAP3	SUB'KIT'BUILD,S,1		!Kit Build?
					!=BRN
					!|"N"
	MAP3	SUB'QFRACTION,B,1		!Quantity Precision
					!=IR
					!\1
	MAP3	SUB'LFRACTION,B,1		!Length Precision
					!=IR
					!\1
	MAP3	SUB'XFRACTION,B,1		!Width Precision
					!=IR
					!\1
	MAP3	SUB'HFRACTION,B,1		!Height Precision
					!=IR
					!\1
	MAP2	SUB'XXX'RCC,@SUB'XXX'OVR	!Overlay
					!=XRA
	MAP3	SUB'XXX'SPACE,S,54		!Clear area
					!=RPCA
	MAP2	SUB'LIN'NO,B,2			!Line Item Number
					!=IR
					!\4
	MAP2	SUB'NO,B,2			!Sub Line Item Number
					!=IR
					!\4
	MAP2	SUB'SHIPMENT,B,2		!Shipment Number
					!=IR
					!\4
	MAP2	SUB'CSOVR1,X			!
					!=XRA
	MAP3	SUB'SERIAL,S,20			!Serial Number
					!=RPCAI
	MAP2	SUB'CSOVR2,@SUB'CSOVR1		!OVERLAY
					!=XRAI
	MAP3	SUB'CSFILL,S,15			!Serial FILLER (must be 1st)
					!=RPCAI
	MAP3	SUB'CWIDTH,B,5			!Carpet Width
					!=IR
					!\11
	MAP2	SUB'ATT'OVR1,X			!Att'ovr1
					!=XRA
	MAP3	SUB'ATT'NO1,B,2			!Attribute 1
					!=IR
					!\4
	MAP3	SUB'ATT'NO2,B,2			!Attribute 2
					!=IR
					!\4
	MAP3	SUB'MATT'NO1,B,2		!Memo Attribute 1
					!=IR
					!\4
	MAP3	SUB'MATT'NO2,B,2		!Memo Attribute 2
					!=IR
					!\4
	MAP2	SUB'ATT'OVR2,@SUB'ATT'OVR1	!Att'ovr2
					!=XRAI
	MAP3	SUB'ATT'NO(4),B,2		!Attribute array overlay
					!=IR
					!\4
	MAP2	SUB'ATT'OVR3,@SUB'ATT'OVR1	!Att'ovr3
					!=XRAI
	MAP3	SUB'CUT'SIZE,B,2		!Cut Size
					!=IR
					!\4
	MAP3	SUB'CUT'FILLER,S,6		!Filler
					!=RAI
	MAP2	SUB'ATT'OVR4,@SUB'ATT'OVR1	!Att'ovr2
					!=XRAI
	MAP3	SUB'ATT'KEYFLD,B,4		!Attribute array overlay
					!=IR
					!\9
	MAP3	SUB'ATT'FILL,B,4		!Attribute array overlay
					!=IR
					!\9
	MAP2	SUB'BATCH'YN,S,1		!In Batch? (Archive only).
					!=RY
	MAP2	SUB'FILLER,S,9			!UNUSED FILLER
					!=RPCAI
	MAP2	SUB'MSGNO,B,3			!Message Chain
					!=IR
					!\7
!K:OESUB
17777  MAP1    SUB'RCC,@SUB'REC
17778          MAP2    SUB'SPACE,S,102
18000  MAP1    SUB'AFR,S,102
