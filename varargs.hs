{-# LANGUAGE ExistentialQuantification #-}

data ShowBox = forall a . Show a => SB a

instance Show ShowBox where
    show (SB s) = show s

printEverything :: [ShowBox] -> IO ()
printEverything sbs = mapM_ putStrLn $ map show sbs

main :: IO ()
main = printEverything [SB 1, SB "yes", SB True]
