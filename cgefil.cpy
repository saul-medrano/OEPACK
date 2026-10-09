!CMP 
MAP1   CGE'REC
	MAP2	CGE'CNO,B,1			!Company Number
					!=IR
					!\2
	MAP2	CGE'NO,B,2			!Cage Number
					!=IR
					!\4
	MAP2	CGE'DESC,S,30			!Description
					!=RPUAI
	MAP2	CGE'MSGNO,B,3			!Message Pointer
					!=IR
					!\5
!K:INCGE
!K:INCGED
17777  MAP1    CGE'RCC,@CGE'REC
17778          MAP2    CGE'SPACE,S,36
18000  MAP1    CGE'AFR,S,36
