* OEPACK - Order Entry box packing subroutine (Pick/UniBasic style).
*
* Called from Order Entry to decide how to pack items into boxes based on
* weight and measurements. Full cases are packed first, then full dozens,
* then the left-over pieces are packed into as few boxes as possible.
*
* CALL OEPACK(ITEM.QTY, CASE.QTY, UNIT.WT, UNIT.VOL, BOX.MAX.WT, BOX.MAX.VOL, CASE.WT, CASE.VOL, BOXES, ERR)
*
*   ITEM.QTY   (in)  Dynamic array (attribute per order line): pieces ordered
*   CASE.QTY   (in)  Attribute per line: pieces in a full case (0 = no cases)
*   UNIT.WT    (in)  Attribute per line: weight of one piece
*   UNIT.VOL   (in)  Attribute per line: volume of one piece (L x W x H)
*   BOX.MAX.WT (in)  Maximum weight a standard box holds
*   BOX.MAX.VOL(in)  Maximum volume a standard box holds
*   CASE.WT    (in)  Unused placeholder kept for caller compatibility (set to 0)
*   CASE.VOL   (in)  Unused placeholder kept for caller compatibility (set to 0)
*   BOXES      (out) Attribute per box; values are line*pieces*weight*volume*type,
*                    separated by value marks; type is the 5th
*                    '*'-delimited field (C=case, D=dozen, L=left-over)
*   ERR        (out) Blank if OK, otherwise an error message
*
SUBROUTINE OEPACK(ITEM.QTY, CASE.QTY, UNIT.WT, UNIT.VOL, BOX.MAX.WT, BOX.MAX.VOL, CASE.WT, CASE.VOL, BOXES, ERR)
   BOXES = ''; ERR = ''
   IF BOX.MAX.WT <= 0 OR BOX.MAX.VOL <= 0 THEN ERR = 'Invalid box size'; RETURN
   NLINES = DCOUNT(ITEM.QTY, @AM)
   DIM LEFT(NLINES)
   BOXNO = 0

   * Pass 1: full cases, one box per case; Pass 2: full dozens, packed
   * as many per box as weight and volume allow.
   FOR L = 1 TO NLINES
      QTY = ITEM.QTY<L>; CQ = CASE.QTY<L>; W = UNIT.WT<L>; V = UNIT.VOL<L>
      IF W <= 0 OR V <= 0 THEN ERR = 'Line ':L:' has no weight/measurements'; RETURN
      IF QTY < 0 OR CQ < 0 THEN ERR = 'Line ':L:' has negative quantity'; RETURN
      IF V > BOX.MAX.VOL OR W > BOX.MAX.WT THEN ERR = 'Line ':L:' piece does not fit in a box'; RETURN
      IF CQ > 0 THEN
         NCASES = INT(QTY / CQ)
         FOR C = 1 TO NCASES
            BOXNO += 1
            BOXES<BOXNO> = L:'*':CQ:'*':CQ * W:'*':CQ * V:'*':'C'
         NEXT C
         QTY = QTY - NCASES * CQ
      END
      NDOZ = INT(QTY / 12)
      IF NDOZ > 0 THEN
         * dozens per box limited by weight and volume (at least one)
         PER = INT(BOX.MAX.WT / (12 * W)); P2 = INT(BOX.MAX.VOL / (12 * V))
         IF P2 < PER THEN PER = P2
         IF PER < 1 THEN PER = 1
         LOOP WHILE NDOZ > 0
            N = NDOZ; IF N > PER THEN N = PER
            BOXNO += 1
            BOXES<BOXNO> = L:'*':N * 12:'*':N * 12 * W:'*':N * 12 * V:'*':'D'
            NDOZ -= N
         REPEAT
         QTY = QTY - INT(QTY / 12) * 12
      END
      LEFT(L) = QTY
   NEXT L

   * Pass 3: left-over pieces, first-fit-decreasing by piece volume into
   * boxes limited by weight and volume.
   CURW = 0; CURV = 0; CUR = ''
   LOOP
      BEST = 0; BV = 0
      FOR L = 1 TO NLINES
         IF LEFT(L) > 0 AND UNIT.VOL<L> > BV THEN BEST = L; BV = UNIT.VOL<L>
      NEXT L
   WHILE BEST DO
      W = UNIT.WT<BEST>; V = UNIT.VOL<BEST>
      IF CURW + W > BOX.MAX.WT OR CURV + V > BOX.MAX.VOL THEN
         IF CUR # '' THEN BOXNO += 1; BOXES<BOXNO> = CUR
         CURW = 0; CURV = 0; CUR = ''
      END
      * add one piece (merge into current line entry if same line)
      N = 0
      LOOP WHILE LEFT(BEST) > N AND CURW + (N + 1) * W <= BOX.MAX.WT AND CURV + (N + 1) * V <= BOX.MAX.VOL
         N += 1
      REPEAT
      CURW += N * W; CURV += N * V; LEFT(BEST) -= N
      CUR<1,-1> = BEST:'*':N:'*':N * W:'*':N * V:'*':'L'
   REPEAT
   IF CUR # '' THEN BOXNO += 1; BOXES<BOXNO> = CUR
   RETURN
END
