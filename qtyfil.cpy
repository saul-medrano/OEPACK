!CMP 
MAP1   QTY'REC
	MAP2	QTY'CNO,B,1			!Company Number
					!=IR
					!\2
	MAP2	QTY'MARK,S,1			!Posting Mark
					!=RCAI
	MAP2	QTY'LOC'NO,B,2			!Location
					!=IR
					!\4
	MAP2	QTY'TRK'NO,B,3			!Tracking number
					!=IR
					!\7
	MAP2	QTY'ATT'OVR1,X			!
					!=XRA
	MAP3	QTY'ATT'NO1,B,2			!attribute 1
					!=IR
					!\4
	MAP3	QTY'ATT'NO2,B,2			!attribute 2
					!=IR
					!\4
	MAP2	QTY'ATT'OVR2,@QTY'ATT'OVR1	!
					!=XRAI
	MAP3	QTY'ATT'NO(2),B,2		!
					!=IR
					!\4
	MAP2	QTY'ATT'OVR3,@QTY'ATT'OVR1	!
					!=XRAI
	MAP3	QTY'CUT'SIZE,B,2		!Cut Size
					!=IR
					!\4
	MAP3	QTY'CUT'FILLER,S,2		!Filler
					!=RPCA
	MAP2	QTY'REC'OVR1,X			!
					!=XRA
	MAP3	QTY'ON'HAND,B,5			!On hand*
					!=IR
					!\11
	MAP3	QTY'ON'ORDER,B,5		!On order
					!=IR
					!\11
	MAP3	QTY'BACKORDER,B,5		!Backordered
					!=IR
					!\11
	MAP3	QTY'BO'OVR,@QTY'BACKORDER	!Backordered
					!=XRA
	MAP4	QTY'BO'QTY,B,5			!Backordered
					!=IR
					!\11
	MAP3	QTY'RESERVED,B,5		!Reserved
					!=IR
					!\11
	MAP3	QTY'COMMITTED,B,5		!Sales orders
					!=IR
					!\11
	MAP3	QTY'COM'OVR,@QTY'COMMITTED	!Sales orders
					!=XRA
	MAP4	QTY'COM'QTY,B,5			!Sales orders
					!=IR
					!\11
	MAP3	QTY'SAFETY,B,5			!Safety stock
					!=IR
					!\11
	MAP3	QTY'RAN,B,5			!Authorized Returns
					!=IR
					!\11
	MAP3	QTY'RETURN,B,5			!Quantity to be returned MFG*
					!=IR
					!\11
	MAP3	QTY'BAD,B,5			!Stock To Be Trashed*
					!=IR
					!\11
	MAP3	QTY'HOLD,B,5			!Returned Stock in Holding*
					!=IR
					!\11
	MAP3	QTY'NEW'STOCK,B,5		!New Stock Received
					!=IR
					!\11
	MAP3	QTY'OVR'SHORT,B,5		!Qty Over and Short*
					!=IR
					!\11
	MAP3	QTY'CYC'NO,B,1			!Cycle Count
					!=IR
					!\2
	MAP3	QTY'FILLERU,B,2			!Filler
					!=IR
					!\4
	MAP3	QTY'CGE'NO(2),B,2		!Warehouse Cage
					!=IR
					!\4
	MAP3	QTY'ASL'NO(2),B,2		!Warehouse Aisle
					!=IR
					!\4
	MAP3	QTY'SCT'NO(2),B,2		!Warehouse Section
					!=IR
					!\4
	MAP3	QTY'BIN'NO(2),B,2		!Warehouse Bin
					!=IR
					!\4
	MAP3	QTY'MSGNO,B,3			!Message pointer
					!=IR
					!\7
	MAP2	QTY'REC'OVR2,@QTY'REC'OVR1	!Qty Rec overlay 2
					!=XRA
	MAP3	QTY'REC'CLEAR,S,82		!If clear may delete
					!=RAI
	MAP2	QTY'CIN,B,1			!Cases In Bin
					!=IR
					!\1
	MAP2	QTY'FILLER,B,1			!Filler
					!=IR
					!\1
	MAP2	QTY'PQTY,B,5			!Quantity In Picking Bin
					!=IR
					!\11
	MAP2	QTY'BLWM,B,2			!Bulk Low Water Mark
					!=IR
					!\4
	MAP2	QTY'BHWM,B,2			!Bulk High Water Mark
					!=IR
					!\4
	MAP2	QTY'LAST'RCV,B,3		!Last Receipt Date From Archive
					!=DIR
					!\8
	MAP2	QTY'ACGE'NO(2),B,2		!Alt Warehouse Cage
					!=IR
					!\4
	MAP2	QTY'AASL'NO(2),B,2		!Alt Warehouse Aisle
					!=IR
					!\4
	MAP2	QTY'ASCT'NO(2),B,2		!Alt Warehouse Section
					!=IR
					!\4
	MAP2	QTY'ABIN'NO(2),B,2		!Alt Warehouse Bin
					!=IR
					!\4
	MAP2	QTY'BULK,S,1			!Pick from bulk?
					!=RCAI
	MAP2	QTY'LWM,B,2			!Low Water Mark
					!=IR
					!\4
	MAP2	QTY'HWM,B,2			!High Water Mark
					!=IR
					!\4
!K:INQTY
!K:INQTYT
17777  MAP1    QTY'RCC,@QTY'REC
17778          MAP2    QTY'SPACE,S,128
18000  MAP1    QTY'AFR,S,128
