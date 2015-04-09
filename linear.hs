import Linear
import Linear.V
import Data.Vector

identity :: V 2 (V 2 Float)
identity = V (fromList [ V (fromList [1, 0]), V (fromList [0, 1]) ])

calculation :: V 2 (V 2 Float)
calculation = identity !*! identity