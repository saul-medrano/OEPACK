!CMP 
MAP1   ASL'REC
	MAP2	ASL'CNO,B,1			!Company Number
					!=IR
					!\2
	MAP2	ASL'NO,B,2			!Aisle Number
					!=IR
					!\4
	MAP2	ASL'DESC,S,30			!Description
					!=RPUAI
	MAP2	ASL'MSGNO,B,3			!Message Pointer
					!=IR
					!\5
!K:INASL
!K:INASLD
17777  MAP1    ASL'RCC,@ASL'REC
17778          MAP2    ASL'SPACE,S,36
18000  MAP1    ASL'AFR,S,36
