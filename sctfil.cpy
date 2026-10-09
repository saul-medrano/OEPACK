!CMP 
MAP1   SCT'REC
	MAP2	SCT'CNO,B,1			!Company Number
					!=IR
					!\2
	MAP2	SCT'NO,B,2			!Section Number
					!=IR
					!\4
	MAP2	SCT'DESC,S,30			!Description
					!=RPUAI
	MAP2	SCT'MSGNO,B,3			!Message Pointer
					!=IR
					!\5
!K:INSCT
!K:INSCTD
17777  MAP1    SCT'RCC,@SCT'REC
17778          MAP2    SCT'SPACE,S,36
18000  MAP1    SCT'AFR,S,36
