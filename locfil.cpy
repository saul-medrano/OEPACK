!CMP 
MAP1   LOC'REC
	MAP2	LOC'CNO,B,1			!Company Number
					!=IR
					!\2
	MAP2	LOC'NO,B,3			!Location Number
					!=IR
					!\7
	MAP2	LOC'DESC,S,30			!Description
					!=RPUAI
	MAP2	LOC'LST'NO,B,3			!Lst File Number
					!=IR
					!\7
	MAP2	LOC'ALT'SEQ,B,2			!Alternate Sequence Number
					!=IR
					!\4
	MAP2	LOC'ACTIVE'YN,S,1		!Active Flag
					!=BRY
					!|"Y"
	MAP2	LOC'PTC,S,4			!G/L Profit Center
					!=RCAI
	MAP2	LOC'CST,S,4			!G/L Cost Center
					!=RCAI
	MAP2	LOC'TYP'DESC(9),S,15		!Invoice Type Descriptions
					!=RPUAI
	MAP2	LOC'INV'CNT'NO(9),B,1		!Invoice Type Counter Pointer
					!=BFIR
					!\1
					!|1
	MAP2	LOC'COUNTER(9),B,3		!Actual Type Counter
					!=IR
					!\6
	MAP2	LOC'FORM(9),S,6			!Form print program
					!=RPCAI
	MAP2	LOC'PRT'NO(9),B,1		!Printer Number
					!=BIR
					!\2
					!|CMP'PRTCOD
					!`E'PRT'NO
	MAP2	LOC'FORMA(9),S,6		!Form print program
					!=RPCAI
	MAP2	LOC'PRT'NOA(9),B,1		!Printer Number
					!=BIR
					!\2
					!|CMP'PRTCOD
					!`E'PRT'NO
	MAP2	LOC'FORMT(9),S,6		!Form print program
					!=RPCAI
	MAP2	LOC'PRT'NOT(9),B,1		!Printer Number
					!=BIR
					!\2
					!|CMP'PRTCOD
					!`E'PRT'NO
	MAP2	LOC'FORML(9),S,6		!Form print program
					!=RPCAI
	MAP2	LOC'PRT'NOL(9),B,1		!Printer Number
					!=BIR
					!\2
					!|CMP'PRTCOD
					!`E'PRT'NO
	MAP2	LOC'GL'CNO,B,1			!G/L Company
					!=IR
					!\2
	MAP2	LOC'CMP'CHG'YN,S,1		!Allow Company Change?
					!=BRY
					!|"Y"
	MAP2	LOC'SHP'LOC'NO,B,2		!Shipping Location Number
					!=IR
					!\4
	MAP2	LOC'MSGNO,B,3			!Message Chain
					!=IR
					!\5
	MAP2	LOC'ADD'COD(9),S,1		!Add to COD?
					!=BRN
					!|"N"
	MAP2	LOC'PICK'BGP,B,1		!Picking background priority
					!=IR
					!\1
					!|OEC'PICK'BGP
	MAP2	LOC'INV'BGP,B,1			!Invoice background priority 
					!=IR
					!\1
					!|OEC'INV'BGP
	MAP2	LOC'ORD'BGP,B,1			!Order background priority 
					!=IR
					!\1
					!|OEC'ORD'BGP
	MAP2	LOC'PACK'BGP,B,1		!Packing background priority
					!=IR
					!\1
					!|OEC'PACK'BGP
	MAP2	LOC'PFORM(9),S,6		!PO print program
					!=RPCAI
	MAP2	LOC'PPRT'NO(9),B,1		!PO Number
					!=BIR
					!\2
					!|CMP'PRTCOD
					!`E'PRT'NO
	MAP2	LOC'PFORMT(9),S,6		!Edit print program
					!=RPCAI
	MAP2	LOC'PPRT'NOT(9),B,1		!Edit Printer Number
					!=BIR
					!\2
					!|CMP'PRTCOD
					!`E'PRT'NO
	MAP2	LOC'PFORML(9),S,6		!Ack Form print program
					!=RPCAI
	MAP2	LOC'PPRT'NOL(9),B,1		!Ack Printer Number
					!=BIR
					!\2
					!|CMP'PRTCOD
					!`E'PRT'NO
	MAP2	LOC'PFORMR(9),S,6		!Rcv print program
					!=RPCAI
	MAP2	LOC'PPRT'NOR(9),B,1		!Rcv printer Number
					!=BIR
					!\2
					!|CMP'PRTCOD
					!`E'PRT'NO
	MAP2	LOC'PFORMV(9),S,6		!Vch print program
					!=RPCAI
	MAP2	LOC'PPRT'NOV(9),B,1		!Vch printer Number
					!=BIR
					!\2
					!|CMP'PRTCOD
					!`E'PRT'NO
	MAP2	LOC'PO'BGP,B,1			!PO background priority
					!=BIR
					!\2
					!|POC'PO'BGP
	MAP2	LOC'POACK'BGP,B,1		!Acknowledge background priorit
					!=IR
					!\2
					!|POC'POACK'BGP
	MAP2	LOC'POEDT'BGP,B,1		!Edit background priority
					!=IR
					!\2
					!|POC'POEDT'BGP
	MAP2	LOC'PORCV'BGP,B,1		!Rcv background priority
					!=IR
					!\2
					!|OEC'PORCV'BGP
	MAP2	LOC'POVCH'BGP,B,1		!Voucher background priority
					!=IR
					!\2
					!|OEC'POVCH'BGP
	MAP2	LOC'AR'APA'NO(9),B,2		!A/R APA No
					!=IR
					!\4
	MAP2	LOC'AP'APA'NO(9),B,2		!A/P APA No
					!=IR
					!\4
	MAP2	LOC'SDESC,S,4			!Short Description
					!=RPCAI
	MAP2	LOC'OC,S,1			!Open Closed
					!=RCAI
					!|"O"
					!`"=O,C"
	MAP2	LOC'AUTH'QID,B,4		!Authorization QID
					!=IR
					!\8
	MAP2	LOC'WEB'AVAIL,S,1		!Web Available
					!=RCA
	MAP2	LOC'TO'HDR'NO,B,2		!Totals only HDR number
					!=IR
					!\4
	MAP2	LOC'WL'YN,S,1			!Using Warehouse locator
					!=RY
					!|"Y"
	MAP2	LOC'SPLIT,S,1			!Splits allowed
					!=RY
					!|"Y"
	MAP2	LOC'SECURE,B,1			!Security Field
					!=IR
					!\2
	MAP2	LOC'MIN'BGP,B,1			!Smallest BGP QUEUE Allowed
					!=IR
					!\2
	MAP2	LOC'MAX'BGP,B,1			!Smallest BGP QUEUE Allowed
					!=IR
					!\2
	MAP2	LOC'PTYP'DESC(9),S,15		!PO Type Descriptions
					!=RPUAI
	MAP2	LOC'STX'FRT'YN,S,1		!Sales Tax On Freight?
					!=BFRN
					!|"N"
	MAP2	LOC'NO'100'ST,S,2		!No UPS 100 Wgt State
					!=RCA
	MAP2	LOC'PTC'CHG,S,1			!Ptc Change
					!=BFRY
					!|"Y"
	MAP2	LOC'CST'CHG,S,1			!Cst Change
					!=BFRY
					!|"Y"
	MAP2	LOC'PRT'NOP(9),B,1		!Printer Number
					!=BIR
					!\2
					!|CMP'PRTCOD
					!`E'PRT'NO
	MAP2	LOC'DEL'LOC'NO,B,2		!Delivery Location (Retail Sys)
					!=IR
					!\4
	MAP2	LOC'UNAPPL'COD,S,1		!Add Unapplies to COD?
					!=BRN
					!|"N"
	MAP2	LOC'APPLY'NETS,S,1		!Apply to Nets too?
					!=BRN
					!|"N"
	MAP2	LOC'REAL'YN,S,1			!Real location for reporting
					!=BFRY
					!|"Y"
	MAP2	LOC'OVR1,X			!Ovr1
					!=XRA
	MAP3	LOC'MGR'UID,S,6			!Manager User ID
					!=RPCAI
	MAP2	LOC'OVR2,@LOC'OVR1		!Ovr2
					!=RA
	MAP3	LOC'MILL'DIR,S,1		!Mill Direct Location
					!=RY
	MAP3	LOC'SHIP'COMP,S,1		!Ship Complete Always
					!=RY
	MAP3	LOC'FILLER,S,4			!Overlay Filler
					!=RAI
!K:INLOC
!K:INLOCD
17777  MAP1    LOC'RCC,@LOC'REC
17778          MAP2    LOC'SPACE,S,1024
18000  MAP1    LOC'AFR,S,1024
