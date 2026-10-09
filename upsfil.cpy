!CMP 
MAP1   UPS'REC
	MAP2	UPS'CNO,B,1			!Company #
					!=IR
					!\2
	MAP2	UPS'LOC'NO,B,2			!Location
					!=IR
					!\4
	MAP2	UPS'MTYPE,S,1			!Manifest Type
					!=RCAI
	MAP2	UPS'UPS,X			!Overlay for UPS
					!=XRA
	MAP3	UPS'SHIPPER,S,10		!Shipper #
					!=RPCAI
	MAP3	UPS'BOOK(4),B,4			!Book
					!=IR
					!\8
	MAP3	UPS'PAGE(4),B,1			!Page
					!=IR
					!\2
	MAP3	UPS'FILLER1,S,8			!Filler1
					!=RAI
	MAP3	UPS'CUR'BOOK,B,1		!Current Book Number
					!=IR
					!\1
	MAP3	UPS'NEXT'CNT,B,3		!Next Day Tracking Counter
					!=IR
					!\7
	MAP3	UPS'2ND'CNT,B,3			!2nd Day Tracking Counter
					!=IR
					!\7
	MAP3	UPS'GRN'CNT,B,3			!Ground Tracking Counter
					!=IR
					!\7
	MAP3	UPS'MISC'CNT,B,3		!Misc Tracking Counter
					!=IR
					!\7
	MAP2	UPS'USPS,@UPS'UPS		!Usps
					!=XRA
	MAP3	UPS'PERMIT'NO,S,4		!Permit #
					!=RPCAI
	MAP3	UPS'DUNS'NO,S,9			!Duns Number
					!=RAI
	MAP3	UPS'SOFT'VER,S,5		!Software Version
					!=RPAI
	MAP3	UPS'MAN'SEQ'NO,B,4		!Manifest Sequence Number
					!=IR
					!\8
	MAP3	UPS'OR'ZIPCODE,S,5		!Origin Zip Code
					!=RAI
	MAP3	UPS'EPERMIT'NO,S,6		!Express Permit #
					!=RPCAI
	MAP3	UPS'UFILLER1,S,18		!Filler1
					!=RAI
	MAP2	UPS'FEDX,@UPS'UPS		!Fedx
					!=XRA
	MAP3	UPS'FSHIPPER,S,10		!Shipper #
					!=RPCAI
	MAP3	UPS'FBOOK(4),B,4		!Book
					!=IR
					!\8
	MAP3	UPS'FPAGE(4),B,1		!Page
					!=IR
					!\2
	MAP3	UPS'DROP'TYP,S,1		!Drop Off Type
					!=RA
	MAP3	UPS'FHUB'ID,S,7			!Hub Id
					!=RAI
	MAP3	UPS'FCUR'BOOK,B,1		!Current Book Number
					!=IR
					!\1
	MAP3	UPS'FNEXT'CNT,B,3		!Next Day Tracking Counter
					!=IR
					!\7
	MAP3	UPS'F2ND'CNT,B,3		!2nd Day Tracking Counter
					!=IR
					!\7
	MAP3	UPS'FGRN'CNT,B,3		!Ground Tracking Counter
					!=IR
					!\7
	MAP3	UPS'FMISC'CNT,B,3		!Misc Tracking Counter
					!=IR
					!\7
	MAP2	UPS'CAOV,@UPS'UPS		!California Overnight
					!=XRA
	MAP3	UPS'CSHIPPER,S,10		!Shipper #
					!=RAI
	MAP3	UPS'CBOOK(4),B,4		!Book
					!=IR
					!\8
	MAP3	UPS'CPAGE(4),B,1		!Page
					!=IR
					!\2
	MAP3	UPS'CFILLER1,S,8		!Filler1
					!=RAI
	MAP3	UPS'CCUR'BOOK,B,1		!Current Book Number
					!=IR
					!\1
	MAP3	UPS'CSUN'CNT,B,3		!Sunrise Tracking Counter
					!=IR
					!\7
	MAP3	UPS'CGOLD'CNT,B,3		!Sunrise Gold Tracking Counter
					!=IR
					!\7
	MAP3	UPS'CCAL'CNT,B,3		!CalTrack Tracking Counter
					!=IR
					!\7
	MAP3	UPS'CMISC'CNT,B,3		!Misc Tracking Counter
					!=IR
					!\7
	MAP2	UPS'DHL,@UPS'UPS		!DHL
					!=XRA
	MAP3	UPS'UNIT'ID,S,5			!Unit ID #
					!=RPCAI
	MAP3	UPS'ORIGIN,B,4			!Origin
					!=IR
					!\8
	MAP3	UPS'DFILLER1,S,42		!Filler1
					!=RAI
	MAP2	UPS'SLOC'CNT,B,1		!Count Of Sub Locations
					!=IR
					!\2
	MAP2	UPS'SERV'AREA,S			!BMC Service Area USPS
					!=RAI
	MAP3	UPS'START(15),S,3		!Starting Zip
					!=RCAI
	MAP3	UPS'END(15),S,3			!Ending Zip Code
					!=RCAI
	MAP2	UPS'POSTOFFICE,S,20		!Origin Post Office
					!=RPUAI
	MAP2	UPS'BS'SHIPPER,S,6		!UPS Basic Urban/Remote Shipper
					!=RCA
	MAP2	UPS'BS'BOOK(2),B,4		!UPS Basic Urban/Remote Book
					!=IR
					!\8
	MAP2	UPS'BS'PAGE(2),B,1		!UPS Basic Urban/Remote Page
					!=IR
					!\2
	MAP2	UPS'BSP'SHIPPR,S,6		!UPS Basic Remote To PO Shipper
					!=RA
	MAP2	UPS'BSP'BOOK(2),B,4		!UPS Basic Remote To PO Book
					!=IR
					!\8
	MAP2	UPS'BSP'PAGE(2),B,1		!UPS Basic Remote To PO Page
					!=IR
					!\2
	MAP2	UPS'FILLER,S,58			!Filler
					!=RAI
!K:INUPS
17777  MAP1    UPS'RCC,@UPS'REC
17778          MAP2    UPS'SPACE,S,256
18000  MAP1    UPS'AFR,S,256
