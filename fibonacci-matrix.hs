newtype TBT a = TBT ((a,a),(a,a)) deriving (Eq,Show)

m :: TBT Integer
m = TBT ((1,1),(1,0))

