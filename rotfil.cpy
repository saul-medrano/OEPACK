!CMP 
MAP1   ROT'REC
	MAP2	ROT'CNO,B,1			!Company Number
					!=IR
					!\2
	MAP2	ROT'CODE,S,20			!Root Code
					!=RPUAI
	MAP2	ROT'DESC,S,30			!Description/Contact
					!=RPUAI
	MAP2	ROT'ATHS,X			!
					!=XRAI
	MAP3	ROT'ATH'NO1,B,2			!Section Attribute Code 1
					!=IR
					!\4
	MAP3	ROT'ATH'NO2,B,2			!Section Attribute Code 2
					!=IR
					!\4
	MAP3	ROT'ATH'NO3,B,2			!Section Attribute Code 3
					!=IR
					!\4
	MAP3	ROT'ATH'NO4,B,2			!Section Attribute Code 4
					!=IR
					!\4
	MAP3	ROT'ATH'NO5,B,2			!Section Attribute Code 5
					!=IR
					!\4
	MAP3	ROT'ATH'NO6,B,2			!Section Attribute Code 6
					!=IR
					!\4
	MAP3	ROT'ATH'NO7,B,2			!Section Attribute Code 7
					!=IR
					!\4
	MAP3	ROT'ATH'NO8,B,2			!Section Attribute Code 8
					!=IR
					!\4
	MAP3	ROT'ATH'NO9,B,2			!Section Attribute Code 9
					!=IR
					!\4
	MAP3	ROT'ATH'NO10,B,2		!Section Attribute Code 10
					!=IR
					!\4
	MAP2	ROT'ATH'NOS,@ROT'ATHS		!
					!=XRAI
	MAP3	ROT'ATH'NO(10),B,2		!Section Attribute Codes
					!=IR
					!\4
	MAP2	ROT'O'ATH'NOS,S			!
					!=RA
	MAP3	ROT'O'ATH'NO(10),B,2		!Old Section Attribute Codes
					!=IR
					!\4
	MAP2	ROT'TAB'DESC,S,77		!Input desriptions
					!=RPCAI
	MAP2	ROT'FILLER,X,3			!Filler
					!=XRA
	MAP2	ROT'TAB'POS(11),B,1		!Desc Positions
					!=IR
					!\2
	MAP2	ROT'ATH'LEN(10),B,1		!Section Lengths
					!=IR
					!\2
	MAP2	ROT'PLN'NO,B,2			!Default G/L Prod Line
					!=IR
					!\4
	MAP2	ROT'SEC'NO,B,2			!Default Sales Prod Line
					!=IR
					!\4
	MAP2	ROT'MSGNO,B,3			!Message pointer
					!=IR
					!\7
	MAP2	ROT'ATH'VERIFY(10),S,1		!Verify Attributes
					!=RCA
	MAP2	ROT'DIV(4),B,1			!Printing divisions
					!=IR
					!\2
!K:INROT
15555           MAP2    ROT'FLL,X,43
17777  MAP1    ROT'RCC,@ROT'REC
17778          MAP2    ROT'SPACE,S,256
18000  MAP1    ROT'AFR,S,256
