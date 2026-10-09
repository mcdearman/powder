module Data.Ix (Ix (..)) where

class Ord a => Ix a where
  range :: (a, a) -> [a]
  index :: (a, a) -> a -> Int
  inRange :: (a, a) -> a -> Bool
  rangeSize :: (a, a) -> Int
  rangeSize b@(_, h) = if null (range b) then 0 else index b h + 1

outOfRange :: a
outOfRange = error "Ix.index: Index out of range."

instance Ix Char where
  range (m, n) = [m .. n]
  index b@(c, _) ci = if inRange b ci then fromEnum ci - fromEnum c else outOfRange
  inRange (c, c') i = c <= i && i <= c'

instance Ix Int where
  range (m, n) = [m .. n]
  index b@(m, _) i = if inRange b i then i - m else outOfRange
  inRange (m, n) i = m <= i && i <= n

instance Ix Integer where
  range (m, n) = [m .. n]
  index b@(m, _) i = if inRange b i then fromInteger (i - m) else outOfRange
  inRange (m, n) i = m <= i && i <= n

instance Ix Bool where
  range (m, n) = [m .. n]
  index b@(m, _) i = if inRange b i then fromEnum i - fromEnum m else outOfRange
  inRange (m, n) i = m <= i && i <= n

instance Ix Ordering where
  range (m, n) = [m .. n]
  index b@(m, _) i = if inRange b i then fromEnum i - fromEnum m else outOfRange
  inRange (m, n) i = m <= i && i <= n

instance Ix () where
  range _ = [()]
  index _ _ = 0
  inRange _ _ = True

instance (Ix t1, Ix t2) => Ix (t1, t2) where
  range ((l1, l2), (u1, u2)) = [(i1, i2) | i1 <- range (l1, u1), i2 <- range (l2, u2)]
  index ((l1, l2), (u1, u2)) (i1, i2) = (index (l1, u1) i1) * rangeSize (l2, u2) + index (l2, u2) i2
  inRange ((l1, l2), (u1, u2)) (i1, i2) = inRange (l1, u1) i1 && inRange (l2, u2) i2

instance (Ix t1, Ix t2, Ix t3) => Ix (t1, t2, t3) where
  range ((l1, l2, l3), (u1, u2, u3)) = [(i1, i2, i3) | i1 <- range (l1, u1), i2 <- range (l2, u2), i3 <- range (l3, u3)]
  index ((l1, l2, l3), (u1, u2, u3)) (i1, i2, i3) = ((index (l1, u1) i1) * rangeSize (l2, u2) + index (l2, u2) i2) * rangeSize (l3, u3) + index (l3, u3) i3
  inRange ((l1, l2, l3), (u1, u2, u3)) (i1, i2, i3) = inRange (l1, u1) i1 && inRange (l2, u2) i2 && inRange (l3, u3) i3

instance (Ix t1, Ix t2, Ix t3, Ix t4) => Ix (t1, t2, t3, t4) where
  range ((l1, l2, l3, l4), (u1, u2, u3, u4)) = [(i1, i2, i3, i4) | i1 <- range (l1, u1), i2 <- range (l2, u2), i3 <- range (l3, u3), i4 <- range (l4, u4)]
  index ((l1, l2, l3, l4), (u1, u2, u3, u4)) (i1, i2, i3, i4) = (((index (l1, u1) i1) * rangeSize (l2, u2) + index (l2, u2) i2) * rangeSize (l3, u3) + index (l3, u3) i3) * rangeSize (l4, u4) + index (l4, u4) i4
  inRange ((l1, l2, l3, l4), (u1, u2, u3, u4)) (i1, i2, i3, i4) = inRange (l1, u1) i1 && inRange (l2, u2) i2 && inRange (l3, u3) i3 && inRange (l4, u4) i4

instance (Ix t1, Ix t2, Ix t3, Ix t4, Ix t5) => Ix (t1, t2, t3, t4, t5) where
  range ((l1, l2, l3, l4, l5), (u1, u2, u3, u4, u5)) = [(i1, i2, i3, i4, i5) | i1 <- range (l1, u1), i2 <- range (l2, u2), i3 <- range (l3, u3), i4 <- range (l4, u4), i5 <- range (l5, u5)]
  index ((l1, l2, l3, l4, l5), (u1, u2, u3, u4, u5)) (i1, i2, i3, i4, i5) = ((((index (l1, u1) i1) * rangeSize (l2, u2) + index (l2, u2) i2) * rangeSize (l3, u3) + index (l3, u3) i3) * rangeSize (l4, u4) + index (l4, u4) i4) * rangeSize (l5, u5) + index (l5, u5) i5
  inRange ((l1, l2, l3, l4, l5), (u1, u2, u3, u4, u5)) (i1, i2, i3, i4, i5) = inRange (l1, u1) i1 && inRange (l2, u2) i2 && inRange (l3, u3) i3 && inRange (l4, u4) i4 && inRange (l5, u5) i5

instance (Ix t1, Ix t2, Ix t3, Ix t4, Ix t5, Ix t6) => Ix (t1, t2, t3, t4, t5, t6) where
  range ((l1, l2, l3, l4, l5, l6), (u1, u2, u3, u4, u5, u6)) = [(i1, i2, i3, i4, i5, i6) | i1 <- range (l1, u1), i2 <- range (l2, u2), i3 <- range (l3, u3), i4 <- range (l4, u4), i5 <- range (l5, u5), i6 <- range (l6, u6)]
  index ((l1, l2, l3, l4, l5, l6), (u1, u2, u3, u4, u5, u6)) (i1, i2, i3, i4, i5, i6) = (((((index (l1, u1) i1) * rangeSize (l2, u2) + index (l2, u2) i2) * rangeSize (l3, u3) + index (l3, u3) i3) * rangeSize (l4, u4) + index (l4, u4) i4) * rangeSize (l5, u5) + index (l5, u5) i5) * rangeSize (l6, u6) + index (l6, u6) i6
  inRange ((l1, l2, l3, l4, l5, l6), (u1, u2, u3, u4, u5, u6)) (i1, i2, i3, i4, i5, i6) = inRange (l1, u1) i1 && inRange (l2, u2) i2 && inRange (l3, u3) i3 && inRange (l4, u4) i4 && inRange (l5, u5) i5 && inRange (l6, u6) i6

instance (Ix t1, Ix t2, Ix t3, Ix t4, Ix t5, Ix t6, Ix t7) => Ix (t1, t2, t3, t4, t5, t6, t7) where
  range ((l1, l2, l3, l4, l5, l6, l7), (u1, u2, u3, u4, u5, u6, u7)) = [(i1, i2, i3, i4, i5, i6, i7) | i1 <- range (l1, u1), i2 <- range (l2, u2), i3 <- range (l3, u3), i4 <- range (l4, u4), i5 <- range (l5, u5), i6 <- range (l6, u6), i7 <- range (l7, u7)]
  index ((l1, l2, l3, l4, l5, l6, l7), (u1, u2, u3, u4, u5, u6, u7)) (i1, i2, i3, i4, i5, i6, i7) = ((((((index (l1, u1) i1) * rangeSize (l2, u2) + index (l2, u2) i2) * rangeSize (l3, u3) + index (l3, u3) i3) * rangeSize (l4, u4) + index (l4, u4) i4) * rangeSize (l5, u5) + index (l5, u5) i5) * rangeSize (l6, u6) + index (l6, u6) i6) * rangeSize (l7, u7) + index (l7, u7) i7
  inRange ((l1, l2, l3, l4, l5, l6, l7), (u1, u2, u3, u4, u5, u6, u7)) (i1, i2, i3, i4, i5, i6, i7) = inRange (l1, u1) i1 && inRange (l2, u2) i2 && inRange (l3, u3) i3 && inRange (l4, u4) i4 && inRange (l5, u5) i5 && inRange (l6, u6) i6 && inRange (l7, u7) i7

instance (Ix t1, Ix t2, Ix t3, Ix t4, Ix t5, Ix t6, Ix t7, Ix t8) => Ix (t1, t2, t3, t4, t5, t6, t7, t8) where
  range ((l1, l2, l3, l4, l5, l6, l7, l8), (u1, u2, u3, u4, u5, u6, u7, u8)) = [(i1, i2, i3, i4, i5, i6, i7, i8) | i1 <- range (l1, u1), i2 <- range (l2, u2), i3 <- range (l3, u3), i4 <- range (l4, u4), i5 <- range (l5, u5), i6 <- range (l6, u6), i7 <- range (l7, u7), i8 <- range (l8, u8)]
  index ((l1, l2, l3, l4, l5, l6, l7, l8), (u1, u2, u3, u4, u5, u6, u7, u8)) (i1, i2, i3, i4, i5, i6, i7, i8) = (((((((index (l1, u1) i1) * rangeSize (l2, u2) + index (l2, u2) i2) * rangeSize (l3, u3) + index (l3, u3) i3) * rangeSize (l4, u4) + index (l4, u4) i4) * rangeSize (l5, u5) + index (l5, u5) i5) * rangeSize (l6, u6) + index (l6, u6) i6) * rangeSize (l7, u7) + index (l7, u7) i7) * rangeSize (l8, u8) + index (l8, u8) i8
  inRange ((l1, l2, l3, l4, l5, l6, l7, l8), (u1, u2, u3, u4, u5, u6, u7, u8)) (i1, i2, i3, i4, i5, i6, i7, i8) = inRange (l1, u1) i1 && inRange (l2, u2) i2 && inRange (l3, u3) i3 && inRange (l4, u4) i4 && inRange (l5, u5) i5 && inRange (l6, u6) i6 && inRange (l7, u7) i7 && inRange (l8, u8) i8

instance (Ix t1, Ix t2, Ix t3, Ix t4, Ix t5, Ix t6, Ix t7, Ix t8, Ix t9) => Ix (t1, t2, t3, t4, t5, t6, t7, t8, t9) where
  range ((l1, l2, l3, l4, l5, l6, l7, l8, l9), (u1, u2, u3, u4, u5, u6, u7, u8, u9)) = [(i1, i2, i3, i4, i5, i6, i7, i8, i9) | i1 <- range (l1, u1), i2 <- range (l2, u2), i3 <- range (l3, u3), i4 <- range (l4, u4), i5 <- range (l5, u5), i6 <- range (l6, u6), i7 <- range (l7, u7), i8 <- range (l8, u8), i9 <- range (l9, u9)]
  index ((l1, l2, l3, l4, l5, l6, l7, l8, l9), (u1, u2, u3, u4, u5, u6, u7, u8, u9)) (i1, i2, i3, i4, i5, i6, i7, i8, i9) = ((((((((index (l1, u1) i1) * rangeSize (l2, u2) + index (l2, u2) i2) * rangeSize (l3, u3) + index (l3, u3) i3) * rangeSize (l4, u4) + index (l4, u4) i4) * rangeSize (l5, u5) + index (l5, u5) i5) * rangeSize (l6, u6) + index (l6, u6) i6) * rangeSize (l7, u7) + index (l7, u7) i7) * rangeSize (l8, u8) + index (l8, u8) i8) * rangeSize (l9, u9) + index (l9, u9) i9
  inRange ((l1, l2, l3, l4, l5, l6, l7, l8, l9), (u1, u2, u3, u4, u5, u6, u7, u8, u9)) (i1, i2, i3, i4, i5, i6, i7, i8, i9) = inRange (l1, u1) i1 && inRange (l2, u2) i2 && inRange (l3, u3) i3 && inRange (l4, u4) i4 && inRange (l5, u5) i5 && inRange (l6, u6) i6 && inRange (l7, u7) i7 && inRange (l8, u8) i8 && inRange (l9, u9) i9

instance (Ix t1, Ix t2, Ix t3, Ix t4, Ix t5, Ix t6, Ix t7, Ix t8, Ix t9, Ix t10) => Ix (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10) where
  range ((l1, l2, l3, l4, l5, l6, l7, l8, l9, l10), (u1, u2, u3, u4, u5, u6, u7, u8, u9, u10)) = [(i1, i2, i3, i4, i5, i6, i7, i8, i9, i10) | i1 <- range (l1, u1), i2 <- range (l2, u2), i3 <- range (l3, u3), i4 <- range (l4, u4), i5 <- range (l5, u5), i6 <- range (l6, u6), i7 <- range (l7, u7), i8 <- range (l8, u8), i9 <- range (l9, u9), i10 <- range (l10, u10)]
  index ((l1, l2, l3, l4, l5, l6, l7, l8, l9, l10), (u1, u2, u3, u4, u5, u6, u7, u8, u9, u10)) (i1, i2, i3, i4, i5, i6, i7, i8, i9, i10) = (((((((((index (l1, u1) i1) * rangeSize (l2, u2) + index (l2, u2) i2) * rangeSize (l3, u3) + index (l3, u3) i3) * rangeSize (l4, u4) + index (l4, u4) i4) * rangeSize (l5, u5) + index (l5, u5) i5) * rangeSize (l6, u6) + index (l6, u6) i6) * rangeSize (l7, u7) + index (l7, u7) i7) * rangeSize (l8, u8) + index (l8, u8) i8) * rangeSize (l9, u9) + index (l9, u9) i9) * rangeSize (l10, u10) + index (l10, u10) i10
  inRange ((l1, l2, l3, l4, l5, l6, l7, l8, l9, l10), (u1, u2, u3, u4, u5, u6, u7, u8, u9, u10)) (i1, i2, i3, i4, i5, i6, i7, i8, i9, i10) = inRange (l1, u1) i1 && inRange (l2, u2) i2 && inRange (l3, u3) i3 && inRange (l4, u4) i4 && inRange (l5, u5) i5 && inRange (l6, u6) i6 && inRange (l7, u7) i7 && inRange (l8, u8) i8 && inRange (l9, u9) i9 && inRange (l10, u10) i10

instance (Ix t1, Ix t2, Ix t3, Ix t4, Ix t5, Ix t6, Ix t7, Ix t8, Ix t9, Ix t10, Ix t11) => Ix (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11) where
  range ((l1, l2, l3, l4, l5, l6, l7, l8, l9, l10, l11), (u1, u2, u3, u4, u5, u6, u7, u8, u9, u10, u11)) = [(i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11) | i1 <- range (l1, u1), i2 <- range (l2, u2), i3 <- range (l3, u3), i4 <- range (l4, u4), i5 <- range (l5, u5), i6 <- range (l6, u6), i7 <- range (l7, u7), i8 <- range (l8, u8), i9 <- range (l9, u9), i10 <- range (l10, u10), i11 <- range (l11, u11)]
  index ((l1, l2, l3, l4, l5, l6, l7, l8, l9, l10, l11), (u1, u2, u3, u4, u5, u6, u7, u8, u9, u10, u11)) (i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11) = ((((((((((index (l1, u1) i1) * rangeSize (l2, u2) + index (l2, u2) i2) * rangeSize (l3, u3) + index (l3, u3) i3) * rangeSize (l4, u4) + index (l4, u4) i4) * rangeSize (l5, u5) + index (l5, u5) i5) * rangeSize (l6, u6) + index (l6, u6) i6) * rangeSize (l7, u7) + index (l7, u7) i7) * rangeSize (l8, u8) + index (l8, u8) i8) * rangeSize (l9, u9) + index (l9, u9) i9) * rangeSize (l10, u10) + index (l10, u10) i10) * rangeSize (l11, u11) + index (l11, u11) i11
  inRange ((l1, l2, l3, l4, l5, l6, l7, l8, l9, l10, l11), (u1, u2, u3, u4, u5, u6, u7, u8, u9, u10, u11)) (i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11) = inRange (l1, u1) i1 && inRange (l2, u2) i2 && inRange (l3, u3) i3 && inRange (l4, u4) i4 && inRange (l5, u5) i5 && inRange (l6, u6) i6 && inRange (l7, u7) i7 && inRange (l8, u8) i8 && inRange (l9, u9) i9 && inRange (l10, u10) i10 && inRange (l11, u11) i11

instance (Ix t1, Ix t2, Ix t3, Ix t4, Ix t5, Ix t6, Ix t7, Ix t8, Ix t9, Ix t10, Ix t11, Ix t12) => Ix (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12) where
  range ((l1, l2, l3, l4, l5, l6, l7, l8, l9, l10, l11, l12), (u1, u2, u3, u4, u5, u6, u7, u8, u9, u10, u11, u12)) = [(i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12) | i1 <- range (l1, u1), i2 <- range (l2, u2), i3 <- range (l3, u3), i4 <- range (l4, u4), i5 <- range (l5, u5), i6 <- range (l6, u6), i7 <- range (l7, u7), i8 <- range (l8, u8), i9 <- range (l9, u9), i10 <- range (l10, u10), i11 <- range (l11, u11), i12 <- range (l12, u12)]
  index ((l1, l2, l3, l4, l5, l6, l7, l8, l9, l10, l11, l12), (u1, u2, u3, u4, u5, u6, u7, u8, u9, u10, u11, u12)) (i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12) = (((((((((((index (l1, u1) i1) * rangeSize (l2, u2) + index (l2, u2) i2) * rangeSize (l3, u3) + index (l3, u3) i3) * rangeSize (l4, u4) + index (l4, u4) i4) * rangeSize (l5, u5) + index (l5, u5) i5) * rangeSize (l6, u6) + index (l6, u6) i6) * rangeSize (l7, u7) + index (l7, u7) i7) * rangeSize (l8, u8) + index (l8, u8) i8) * rangeSize (l9, u9) + index (l9, u9) i9) * rangeSize (l10, u10) + index (l10, u10) i10) * rangeSize (l11, u11) + index (l11, u11) i11) * rangeSize (l12, u12) + index (l12, u12) i12
  inRange ((l1, l2, l3, l4, l5, l6, l7, l8, l9, l10, l11, l12), (u1, u2, u3, u4, u5, u6, u7, u8, u9, u10, u11, u12)) (i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12) = inRange (l1, u1) i1 && inRange (l2, u2) i2 && inRange (l3, u3) i3 && inRange (l4, u4) i4 && inRange (l5, u5) i5 && inRange (l6, u6) i6 && inRange (l7, u7) i7 && inRange (l8, u8) i8 && inRange (l9, u9) i9 && inRange (l10, u10) i10 && inRange (l11, u11) i11 && inRange (l12, u12) i12

instance (Ix t1, Ix t2, Ix t3, Ix t4, Ix t5, Ix t6, Ix t7, Ix t8, Ix t9, Ix t10, Ix t11, Ix t12, Ix t13) => Ix (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13) where
  range ((l1, l2, l3, l4, l5, l6, l7, l8, l9, l10, l11, l12, l13), (u1, u2, u3, u4, u5, u6, u7, u8, u9, u10, u11, u12, u13)) = [(i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13) | i1 <- range (l1, u1), i2 <- range (l2, u2), i3 <- range (l3, u3), i4 <- range (l4, u4), i5 <- range (l5, u5), i6 <- range (l6, u6), i7 <- range (l7, u7), i8 <- range (l8, u8), i9 <- range (l9, u9), i10 <- range (l10, u10), i11 <- range (l11, u11), i12 <- range (l12, u12), i13 <- range (l13, u13)]
  index ((l1, l2, l3, l4, l5, l6, l7, l8, l9, l10, l11, l12, l13), (u1, u2, u3, u4, u5, u6, u7, u8, u9, u10, u11, u12, u13)) (i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13) = ((((((((((((index (l1, u1) i1) * rangeSize (l2, u2) + index (l2, u2) i2) * rangeSize (l3, u3) + index (l3, u3) i3) * rangeSize (l4, u4) + index (l4, u4) i4) * rangeSize (l5, u5) + index (l5, u5) i5) * rangeSize (l6, u6) + index (l6, u6) i6) * rangeSize (l7, u7) + index (l7, u7) i7) * rangeSize (l8, u8) + index (l8, u8) i8) * rangeSize (l9, u9) + index (l9, u9) i9) * rangeSize (l10, u10) + index (l10, u10) i10) * rangeSize (l11, u11) + index (l11, u11) i11) * rangeSize (l12, u12) + index (l12, u12) i12) * rangeSize (l13, u13) + index (l13, u13) i13
  inRange ((l1, l2, l3, l4, l5, l6, l7, l8, l9, l10, l11, l12, l13), (u1, u2, u3, u4, u5, u6, u7, u8, u9, u10, u11, u12, u13)) (i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13) = inRange (l1, u1) i1 && inRange (l2, u2) i2 && inRange (l3, u3) i3 && inRange (l4, u4) i4 && inRange (l5, u5) i5 && inRange (l6, u6) i6 && inRange (l7, u7) i7 && inRange (l8, u8) i8 && inRange (l9, u9) i9 && inRange (l10, u10) i10 && inRange (l11, u11) i11 && inRange (l12, u12) i12 && inRange (l13, u13) i13

instance (Ix t1, Ix t2, Ix t3, Ix t4, Ix t5, Ix t6, Ix t7, Ix t8, Ix t9, Ix t10, Ix t11, Ix t12, Ix t13, Ix t14) => Ix (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14) where
  range ((l1, l2, l3, l4, l5, l6, l7, l8, l9, l10, l11, l12, l13, l14), (u1, u2, u3, u4, u5, u6, u7, u8, u9, u10, u11, u12, u13, u14)) = [(i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14) | i1 <- range (l1, u1), i2 <- range (l2, u2), i3 <- range (l3, u3), i4 <- range (l4, u4), i5 <- range (l5, u5), i6 <- range (l6, u6), i7 <- range (l7, u7), i8 <- range (l8, u8), i9 <- range (l9, u9), i10 <- range (l10, u10), i11 <- range (l11, u11), i12 <- range (l12, u12), i13 <- range (l13, u13), i14 <- range (l14, u14)]
  index ((l1, l2, l3, l4, l5, l6, l7, l8, l9, l10, l11, l12, l13, l14), (u1, u2, u3, u4, u5, u6, u7, u8, u9, u10, u11, u12, u13, u14)) (i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14) = (((((((((((((index (l1, u1) i1) * rangeSize (l2, u2) + index (l2, u2) i2) * rangeSize (l3, u3) + index (l3, u3) i3) * rangeSize (l4, u4) + index (l4, u4) i4) * rangeSize (l5, u5) + index (l5, u5) i5) * rangeSize (l6, u6) + index (l6, u6) i6) * rangeSize (l7, u7) + index (l7, u7) i7) * rangeSize (l8, u8) + index (l8, u8) i8) * rangeSize (l9, u9) + index (l9, u9) i9) * rangeSize (l10, u10) + index (l10, u10) i10) * rangeSize (l11, u11) + index (l11, u11) i11) * rangeSize (l12, u12) + index (l12, u12) i12) * rangeSize (l13, u13) + index (l13, u13) i13) * rangeSize (l14, u14) + index (l14, u14) i14
  inRange ((l1, l2, l3, l4, l5, l6, l7, l8, l9, l10, l11, l12, l13, l14), (u1, u2, u3, u4, u5, u6, u7, u8, u9, u10, u11, u12, u13, u14)) (i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14) = inRange (l1, u1) i1 && inRange (l2, u2) i2 && inRange (l3, u3) i3 && inRange (l4, u4) i4 && inRange (l5, u5) i5 && inRange (l6, u6) i6 && inRange (l7, u7) i7 && inRange (l8, u8) i8 && inRange (l9, u9) i9 && inRange (l10, u10) i10 && inRange (l11, u11) i11 && inRange (l12, u12) i12 && inRange (l13, u13) i13 && inRange (l14, u14) i14

instance (Ix t1, Ix t2, Ix t3, Ix t4, Ix t5, Ix t6, Ix t7, Ix t8, Ix t9, Ix t10, Ix t11, Ix t12, Ix t13, Ix t14, Ix t15) => Ix (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15) where
  range ((l1, l2, l3, l4, l5, l6, l7, l8, l9, l10, l11, l12, l13, l14, l15), (u1, u2, u3, u4, u5, u6, u7, u8, u9, u10, u11, u12, u13, u14, u15)) = [(i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15) | i1 <- range (l1, u1), i2 <- range (l2, u2), i3 <- range (l3, u3), i4 <- range (l4, u4), i5 <- range (l5, u5), i6 <- range (l6, u6), i7 <- range (l7, u7), i8 <- range (l8, u8), i9 <- range (l9, u9), i10 <- range (l10, u10), i11 <- range (l11, u11), i12 <- range (l12, u12), i13 <- range (l13, u13), i14 <- range (l14, u14), i15 <- range (l15, u15)]
  index ((l1, l2, l3, l4, l5, l6, l7, l8, l9, l10, l11, l12, l13, l14, l15), (u1, u2, u3, u4, u5, u6, u7, u8, u9, u10, u11, u12, u13, u14, u15)) (i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15) = ((((((((((((((index (l1, u1) i1) * rangeSize (l2, u2) + index (l2, u2) i2) * rangeSize (l3, u3) + index (l3, u3) i3) * rangeSize (l4, u4) + index (l4, u4) i4) * rangeSize (l5, u5) + index (l5, u5) i5) * rangeSize (l6, u6) + index (l6, u6) i6) * rangeSize (l7, u7) + index (l7, u7) i7) * rangeSize (l8, u8) + index (l8, u8) i8) * rangeSize (l9, u9) + index (l9, u9) i9) * rangeSize (l10, u10) + index (l10, u10) i10) * rangeSize (l11, u11) + index (l11, u11) i11) * rangeSize (l12, u12) + index (l12, u12) i12) * rangeSize (l13, u13) + index (l13, u13) i13) * rangeSize (l14, u14) + index (l14, u14) i14) * rangeSize (l15, u15) + index (l15, u15) i15
  inRange ((l1, l2, l3, l4, l5, l6, l7, l8, l9, l10, l11, l12, l13, l14, l15), (u1, u2, u3, u4, u5, u6, u7, u8, u9, u10, u11, u12, u13, u14, u15)) (i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15) = inRange (l1, u1) i1 && inRange (l2, u2) i2 && inRange (l3, u3) i3 && inRange (l4, u4) i4 && inRange (l5, u5) i5 && inRange (l6, u6) i6 && inRange (l7, u7) i7 && inRange (l8, u8) i8 && inRange (l9, u9) i9 && inRange (l10, u10) i10 && inRange (l11, u11) i11 && inRange (l12, u12) i12 && inRange (l13, u13) i13 && inRange (l14, u14) i14 && inRange (l15, u15) i15
