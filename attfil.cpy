!CMP 
MAP1   ATT'REC
	MAP2	ATT'CNO,B,1			!Company Number
					!=IR
					!\2
	MAP2	ATT'ATH'NO,B,2			!Attribute Header Number
					!=IR
					!\4
	MAP2	ATT'NO,B,2			!Attribute Number
					!=IR
					!\4
	MAP2	ATT'CODE,S,19			!Attribute Code
					!=RPCAI
	MAP2	ATT'DESC,S,25			!Description
					!=RPUAI
	MAP2	ATT'GTIN,S,2			!GTIN Section For Size
					!=RAI
	MAP2	ATT'MSGNO,B,3			!Message #
					!=IR
					!\7
	MAP2	ATT'TS'CODE,S,2			!T-Shirt Code
					!=RCAI
	MAP2	ATT'CO'CODE,S,2			!Color Family Code
					!=RAI
	MAP2	ATT'HEX'CODE,S,6		!Hex Color
					!=RAI
!K:INATT
!K:INATTD
17777  MAP1    ATT'RCC,@ATT'REC
17778          MAP2    ATT'SPACE,S,64
18000  MAP1    ATT'AFR,S,64
