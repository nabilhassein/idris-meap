-- 6.2 but using List instead of encoding recursion in Format
data Format = Number | Str | Lit String

PrintfType : List Format -> Type
PrintfType []               = String
PrintfType (Number :: fmt)  = Int -> PrintfType fmt
PrintfType (Str :: fmt)     = String -> PrintfType fmt
PrintfType ((Lit _) :: fmt) = PrintfType fmt

printfFmts : (fmts : List Format) -> String -> PrintfType fmts
printfFmts []                acc = acc
printfFmts (Number :: fmts)  acc = \i => printfFmts fmts (acc ++ show i)
printfFmts (Str :: fmts)     acc = \s => printfFmts fmts (acc ++ s)
printfFmts ((Lit s) :: fmts) acc = printfFmts fmts (acc ++ s)

toFormat : List Char -> List Format
toFormat []                    = []
toFormat ('%' :: 'd' :: chars) = Number  :: toFormat chars
toFormat ('%' :: 's' :: chars) = Str     :: toFormat chars
toFormat ('%' :: chars)        = Lit "%" :: toFormat chars
toFormat (c :: chars)          = case toFormat chars of
  Lit lit :: fmts => Lit (strCons c lit) :: fmts
  fmts            => Lit (strCons c "" ) :: fmts

printf : (fmt : String) -> (PrintfType . toFormat . unpack) fmt
printf _ = printfFmts _ ""