!CMP 
MAP1   MEA'REC
	MAP2	MEA'CNO,B,1			!Company Number
					!=IR
					!\2
	MAP2	MEA'TRK'NO,B,3			!Part Tracking Number
					!=IR
					!\7
	MAP2	MEA'UOM'NO,B,2			!Unit of measure
					!=BIR
					!\4
	MAP2	MEA'DESC,S,20			!Physical Description
					!=RPUAI
	MAP2	MEA'FILLER,S,7			!Filler
					!=RAI
	MAP2	MEA'MADE'UP,S,1			!Made up Data
					!=FBRN
					!|"N"
	MAP2	MEA'PPC,B,2			!Peices Per Case
					!=IR
					!\4
	MAP2	MEA'PRH'OVR,X			!Overlay for PRH
					!=XRAI
	MAP3	MEA'UOW'NO,B,2			!Unit of Weight
					!=BIR
					!\4
					!|SEC'UOW'NO
	MAP3	MEA'OVR1,X			!Ovr1
					!=XRAI
	MAP4	MEA'SHP'WGT,B,3			!Gross Shipping Weight
					!=IR
					!\7
	MAP4	MEA'NET'WGT,B,3			!Net Shipping Weight
					!=IR
					!\7
	MAP4	MEA'CUBES,B,2			!Size in Cubes
					!=IR
					!\4
	MAP4	MEA'LENGTH,B,2			!Length
					!=IR
					!\4
	MAP4	MEA'WIDTH,B,2			!Width
					!=IR
					!\4
	MAP4	MEA'HEIGHT,B,2			!Height
					!=IR
					!\4
	MAP4	MEA'LWH'UOM'NO,B,2		!Unit of measure for LWH
					!=BIR
					!\4
	MAP4	MEA'FLLL,B,1			!Overlay filler
					!=IR
					!\2
	MAP3	MEA'OVR2,@MEA'OVR1		!Ovr2
					!=XRAI
	MAP4	MEA'WGT(6),B,2			!Garment weights
					!=IR
					!\4
	MAP4	MEA'CASE'TO'1,B,1		!Cases To Treat As 1
					!=IR
					!\2
	MAP4	MEA'TREAT'CASE,S,1		!Treat As A Case?
					!=BRY
					!|"Y"
	MAP4	MEA'PC'PER'PKG,B,1		!Pieces per package
					!=IR
					!\2
	MAP4	MEA'PKG'PER'CS,B,1		!Packages per case
					!=IR
					!\2
	MAP4	MEA'DIM'WGT,B,1			!Dimensional weight
					!=IR
					!\2
	MAP2	MEA'FREEFRT'YN,B,1		!Free Freight Scheme
					!=IR
					!\1
!K:INMEA
17777  MAP1    MEA'RCC,@MEA'REC
17778          MAP2    MEA'SPACE,S,56
18000  MAP1    MEA'AFR,S,56
