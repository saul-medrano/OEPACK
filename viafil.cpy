!CMP 
MAP1   VIA'REC
	MAP2	VIA'CNO,B,1			!Company Number
					!=IR
					!\1
	MAP2	VIA'NO,B,2			!Ship Via Number
					!=IR
					!\4
	MAP2	VIA'DESC,S,25			!Description
					!=RA
	MAP2	VIA'ACTIVE,S,1			!Active
					!=BFRY
					!|"Y"
	MAP2	VIA'ALLOW,S,1			!Allowed Flag
					!=RA
	MAP2	VIA'MAN'NO,B,1			!Manifest Number
					!=IR
					!\2
	MAP2	VIA'CVIA'NO,B,2			!Charge Via Number
					!=IR
					!\4
	MAP2	VIA'UOW'NO,B,2			!Unit Of Weight Number
					!=IR
					!\4
	MAP2	VIA'OVR1,X			!Ovr1
					!=XRA
	MAP3	VIA'FTBL'NO,B,2			!Freight Table Number
					!=IR
					!\4
	MAP3	VIA'CTBL'NO,B,2			!Cod Table Number
					!=IR
					!\4
	MAP3	VIA'ITBL'NO,B,2			!Insurance Table Number
					!=IR
					!\4
	MAP3	VIA'M1TBL'NO,B,2		!Misc Table 1
					!=IR
					!\4
	MAP3	VIA'M2TBL'NO,B,2		!Misc Table 2
					!=IR
					!\4
	MAP3	VIA'M3TBL'NO,B,2		!Misc Table 3
					!=IR
					!\4
	MAP3	VIA'M4TBL'NO,B,2		!Misc Table 4
					!=IR
					!\4
	MAP2	VIA'OVR2,@VIA'OVR1		!Overlay
					!=XRAI
	MAP3	VIA'TBL'NO(7),B,2		!Table Numbers
					!=IR
					!\4
	MAP2	VIA'OVR3,X			!Ovr3
					!=XRA
	MAP3	VIA'MTDESC1,S,8			!Misc Desc 1
					!=RPUAI
	MAP3	VIA'MTDESC2,S,8			!Misc Desc 2
					!=RPUAI
	MAP3	VIA'MTDESC3,S,8			!Misc Desc 3
					!=RPUAI
	MAP3	VIA'MTDESC4,S,8			!Misc Desc 4
					!=RPUAI
	MAP2	VIA'OVR4,@VIA'OVR3		!Overlay
					!=XRAI
	MAP3	VIA'MTDESC(4),S,8		!Misc Descriptions
					!=RPUAI
	MAP2	VIA'CHG'OPT(7),B,1		!Freight Charge Option
					!=BFIR
					!\2
	MAP2	VIA'ALLOW'POB,S,1		!Allow Delivery To PO Box
					!=RA
	MAP2	VIA'OVR5,X			!Ovr5
					!=XRA
	MAP3	VIA'FTBL'FLAG,B,1		!Freight Table Flag
					!=BIR
					!\1
					!`"|TBLFLG(12)"
	MAP3	VIA'CTBL'FLAG,B,1		!COD Table Flag
					!=BIR
					!\1
					!`"|TBLFLG(12)"
	MAP3	VIA'ITBL'FLAG,B,1		!Insurance Table Flag
					!=BIR
					!\1
					!`"|TBLFLG(12)"
	MAP3	VIA'M1TBL'FLAG,B,1		!Misc Table Flag 1
					!=BIR
					!\1
					!`"|TBLFLG(12)"
	MAP3	VIA'M2TBL'FLAG,B,1		!Misc Table Flag 2
					!=BIR
					!\1
					!`"|TBLFLG(12)"
	MAP3	VIA'M3TBL'FLAG,B,1		!Misc Table Flag 3
					!=BIR
					!\1
					!`"|TBLFLG(12)"
	MAP3	VIA'M4TBL'FLAG,B,1		!Misc Table Flag 4
					!=BIR
					!\1
					!`"|TBLFLG(12)"
	MAP2	VIA'OVR6,@VIA'OVR5		!Overlay
					!=XRAI
	MAP3	VIA'TBL'FLAG(7),B,1		!Misc Table Flags
					!=BIR
					!\1
					!`"|TBLFLG(12)"
	MAP2	VIA'SHP'FLAG,B,1		!Shipping Flag
					!=BIR
					!\1
					!|OEC'SHP'FLAG
					!`"|SHPFLG(15)"
	MAP2	VIA'MAN'YN,S,1			!Manifest ?
					!=BRN
					!|"N"
	MAP2	VIA'FRT'NO,B,1			!Freight charge number
					!=IR
					!\1
	MAP2	VIA'AOD'NO,B,1			!AOD/Del Conf charge number
					!=IR
					!\1
	MAP2	VIA'COD'NO,B,1			!COD charge number
					!=IR
					!\1
	MAP2	VIA'INS'NO,B,1			!INS charge number
					!=IR
					!\1
	MAP2	VIA'AVIA'NO(5),B,2		!Alternate Ship Via Numbers
					!=IR
					!\4
	MAP2	VIA'AUTO'CMP(5),S,1		!Auto Compare Ship Via
					!=RY
	MAP2	VIA'MAX'WEIGHT,B,5		!Max weight To Pack
					!=IR
					!\11
	MAP2	VIA'ALLOW'ST,S,2		!Allow/Disallow State
					!=RCA
	MAP2	VIA'WB'YN,S,1			!WEB Allowed?
					!=BFRY
					!|"Y"
	MAP2	VIA'ALLOW'TYPE,B,1		!Allow type (COD Or Not)
					!=IR
					!\1
	MAP2	VIA'ALLOW'COD,S,1		!Allow cod?
					!=BFRY
	MAP2	VIA'UPS,X			!Overlay for UPS
					!=XRA
	MAP3	VIA'SAT'IND,S,1			!Sat Indicator?
					!=BFRN
					!|"N"
	MAP3	VIA'SERV'TYPE,S,2		!Service Type
					!=RA
	MAP3	VIA'CHG'TYPE,S,3		!Charge Type
					!=RCA
	MAP3	VIA'PKG'TYPE,S,2		!Packaging Type
					!=RCAI
	MAP3	VIA'RES'IND,S,1			!Residence?
					!=BFRN
					!|"N"
	MAP3	VIA'1Z'SERV,S,2			!1Z Service Code
					!=RCAI
	MAP3	VIA'PICKUP'IND,S,1		!Pickup Indicator
					!=BFRN
					!|"N"
	MAP3	VIA'DCIS'TYPE,S,1		!Delivery Confirmation Type
					!=RCAI
	MAP3	VIA'SVIA'NO,B,2			!Summary Ship Via Number
					!=IR
					!\4
	MAP3	VIA'DIM'YN,S,1			!Use Dimensional Weight?
					!=BFRY
					!|"Y"
	MAP3	VIA'UPS'FIL,S,6			!UPS Filler
					!=RAI
	MAP2	VIA'USPS,@VIA'UPS		!Usps
					!=XRA
	MAP3	VIA'INT'YN,S,1			!International?
					!=RA
	MAP3	VIA'USPS1,X			!Overlay for code
					!=XRA
	MAP4	VIA'INTER'CODE,S,2		!Inter Code (non-Mach)
					!=BFRY
	MAP3	VIA'USPS2,@VIA'USPS1		!Overlay for code
					!=XRA
	MAP4	VIA'RF'CODE,S,2			!Rate/Fee Code
					!=BFRY
	MAP3	VIA'INTRA'CODE,S,2		!Intra Code (non-Mach)
					!=BFRY
	MAP3	VIA'INTERM'COD,S,2		!Inter Code (Machinable)
					!=BFRY
	MAP3	VIA'INTRAM'COD,S,2		!Intra Code (Machinable)
					!=BFRY
	MAP3	VIA'SIZE,S,1			!Size Of Package
					!=BFRN
					!|"N"
	MAP3	VIA'HOLIDAY'YN,S,1		!Holiday Y/N
					!=RCA
	MAP3	VIA'WEEKEND'YN,S,1		!Weekend Y/N
					!=BFRN
					!|"N"
	MAP3	VIA'SIG'WAIVER,S,1		!Signature Waiver
					!=RCA
	MAP3	VIA'ADD'CORR,S,1		!Address Correction
					!=RUA
	MAP3	VIA'GDCIS'TYPE,S,1		!Delivery Confirmation Type
					!=RCAI
	MAP3	VIA'CERTIFIED,S,1		!Certified
					!=RCA
	MAP3	VIA'GCHG'TYPE,S,3		!Charge Type
					!=RCA
	MAP3	VIA'AM'DELIVER,S,1		!Morning Delivery
					!=RCA
	MAP3	VIA'USPS'FIL,S,2		!USPS Filler
					!=RAI
	MAP2	VIA'FEDX,@VIA'UPS		!Fed-X
					!=XRA
	MAP3	VIA'FSAT'IND,S,1		!Sat Indicator?
					!=BFRN
					!|"N"
	MAP3	VIA'FSERV'TYPE,S,2		!Service Type
					!=RA
	MAP3	VIA'FCHG'TYPE,S,3		!Charge Type
					!=RCA
	MAP3	VIA'FPKG'TYPE,S,2		!Packaging Type
					!=RCAI
	MAP3	VIA'FRES'IND,S,1		!Residence?
					!=BFRN
					!|"N"
	MAP3	VIA'F1Z'SERV,S,2		!1Z Service Code
					!=RCAI
	MAP3	VIA'FPICKUP'IN,S,1		!Pickup Indicator
					!=BFRN
					!|"N"
	MAP3	VIA'FDCIS'TYPE,S,1		!Delivery Confirmation Type
					!=RCAI
	MAP3	VIA'FSVIA'NO,B,2		!Summary Ship Via Number
					!=IR
					!\4
	MAP3	VIA'FDIM'YN,S,1			!Use Dimensional Weight?
					!=BFRY
					!|"Y"
	MAP3	VIA'FINT'YN,S,1			!Fedex International
					!=RA
					!|"N"
	MAP3	VIA'FALLOW'COD,S,1		!Allow cod?
					!=BFRY
	MAP3	VIA'FEDX'FIL,S,4		!Federal Express Fil
					!=RAI
	MAP2	VIA'PCL'NO,B,2			!Picking Class
					!=IR
					!\4
	MAP2	VIA'MAX'QTY,B,2			!Max qty To Pack
					!=IR
					!\4
	MAP2	VIA'RES'VIA'NO,B,2		!Opposite Residential Via
					!=IR
					!\4
	MAP2	VIA'1Z'COD,S,2			!1Z COD Service Code
					!=RCAI
	MAP2	VIA'SCAC'CODE,S,6		!SCAC Code
					!=RCAI
	MAP2	VIA'PACK'CASES,S,1		!Pack cases?
					!=RY
	MAP2	VIA'MTYPE,S,1			!Manifest Type
					!=RCAI
	MAP2	VIA'LABEL'SECS,B,1		!USPS Seconds For Label
					!=IR
					!\1
	MAP2	VIA'WEB'SECS,B,1		!USPS Seconds For Processing
					!=IR
					!\1
	MAP2	VIA'AD,S,1			!Allow/Disallow At Location
					!=RA
	MAP2	VIA'OLD'LOC'NO(20),B,3		!Allow/Disallow Locs (filler)
					!=IR
					!\5
	MAP2	VIA'PLOC'NO,B,3			!Pricing Location
					!=IR
					!\5
	MAP2	VIA'FREE'FRT,S,1		!Set Free Freight In O/E
					!=RCA
	MAP2	VIA'HI'TBL'NO,B,2		!International "high" Table
					!=IR
					!\4
	MAP2	VIA'C5'TBL'NO,B,2		!Canada 500 Weight Table
					!=IR
					!\4
	MAP2	VIA'C1'TBL'NO,B,2		!Canada 1000 Weight Table
					!=IR
					!\4
	MAP2	VIA'RANK,B,1			!Ship Via Ranking
					!=IR
					!\1
	MAP2	VIA'SHIP'LINE,S,3		!Shipping Line
					!=RCA
	MAP2	VIA'IMPORTER,S,1		!Non Resident Importer
					!=RCA
	MAP2	VIA'LETTER'S,S,1		!letter S for TAD schema
					!=RAI
	MAP2	VIA'BOX1'LANE,S,1		!Boxer 1 lane
					!=RCAI
	MAP2	VIA'LOC'NO(38),B,3		!Allow/Disallow Locations
					!=IR
					!\5
	MAP2	VIA'LOAD,S,30			!Load File Name
					!=RAI
	MAP2	VIA'UNLOAD,S,30			!Unload File Name
					!=RAI
	MAP2	VIA'CARRIER,S,7			!Malvern Carrier Code
					!=RCAI
	MAP2	VIA'FILLER2,S,86		!Filler2
					!=RA
!K:INVIA
!K:INVIAD
!K:INVIAM
17777  MAP1    VIA'RCC,@VIA'REC
17778          MAP2    VIA'SPACE,S,512
18000  MAP1    VIA'AFR,S,512
