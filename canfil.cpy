!CMP 
MAP1   CAN'REC
	MAP2	CAN'CNO,B,1			!Company #
					!=IR
					!\2
	MAP2	CAN'HHS'NO,B,3			!Order Number
					!=IR
					!\7
	MAP2	CAN'SHIPMENT,B,2		!Shipment
					!=IR
					!\4
	MAP2	CAN'LOC'NO,B,2			!Location
					!=IR
					!\4
	MAP2	CAN'DOC'TYPE,B,1		!Document Type
					!=IR
					!\1
	MAP2	CAN'UB'UID,S,6			!User Who Canceled
					!=RAI
	MAP2	CAN'DATE,B,3			!Cancel Date
					!=DIR
					!\8
	MAP2	CAN'EXTRA,S,14			!Extra
					!=RAI
	MAP2	CAN'NAM'NO,B,3			!Customer Number
					!=IR
					!\7
	MAP2	CAN'TYPE,S,1			!Deleted Or Cancelled
					!=RA
	MAP2	CAN'QUOTE'YN,S,1		!Quote Y/N
					!=RCA
	MAP2	CAN'PO,S,22			!Po Number
					!=RPA
	MAP2	CAN'REASON,S,60			!Reason
					!=RPA
	MAP2	CAN'MSGNO,B,3			!Message Chain
					!=IR
					!\5
!K:OECAN
!K:OECANA
!K:OECAND
15555           MAP2    CAN'FLL,X,6
17777  MAP1    CAN'RCC,@CAN'REC
17778          MAP2    CAN'SPACE,S,128
18000  MAP1    CAN'AFR,S,128
