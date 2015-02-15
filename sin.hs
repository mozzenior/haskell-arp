{-# LANGUAGE ForeignFunctionInterface #-}

import Foreign.C -- get the C types
 
-- pure function
foreign import ccall "math.h sin" c_sin :: CDouble -> CDouble
xsin :: Double -> Double
xsin d = realToFrac (c_sin (realToFrac d))

main :: IO ()
main = do
    let x = xsin (3.14159 / 2)
    putStrLn $ show x
