!CMP 
MAP1   HAZ'REC
	MAP2	HAZ'CNO,B,1			!Company Number
					!=IR
					!\2
	MAP2	HAZ'NO,B,1			!Hazardous Material Number
					!=IR
					!\2
	MAP2	HAZ'DESC,S,30			!Description
					!=RPUAI
	MAP2	HAZ'CODE,S,10			!Hazardous Material ID
					!=RPCAI
	MAP2	HAZ'LETTER,S,6			!Letter to Send
					!=RCAI
	MAP2	HAZ'SEND,S,1			!When to Send Letter	
					!=RCAI
	MAP2	HAZ'REMINDER,S,1		!Print reminder
					!=BRY
					!|"Y"
	MAP2	HAZ'MSGNO,B,3			!Message Pointer
					!=IR
					!\5
	MAP2	HAZ'DIVISION,S,4		!Hazard Class/Division
					!=RCA
	MAP2	HAZ'PKG'GROUP,S,3		!Packing Group Type
					!=RCA
	MAP2	HAZ'SHIP'NAME,S,124		!Hazard Shipping Name
					!=RPA
	MAP2	HAZ'PKG'TYPE,S,50		!Package Type
					!=RPA
	MAP2	HAZ'OUTSIDE'US,S,1		!Allow Shipping Outside US
					!=FBRN
					!|"N"
	MAP2	HAZ'NOT'ST(52),S,2		!States Not Allowed
					!=FRCAI
	MAP2	HAZ'ALLOW'AIR,S,1		!Allow air
					!=BFRN
					!|"N"
	MAP2	HAZ'ALLOW'GRND,S,1		!Allow Ground
					!=BFRN
					!|"N"
	MAP2	HAZ'ALLOW'USPS,S,1		!Allow usps
					!=BFRN
					!|"N"
	MAP2	HAZ'FILLER,S,170		!Filler
					!=RAI
!K:INHAZ
!K:INHAZD
17777  MAP1    HAZ'RCC,@HAZ'REC
17778          MAP2    HAZ'SPACE,S,512
18000  MAP1    HAZ'AFR,S,512
