module ConversionUtils exposing (convertToInt)


convertToInt : String -> Int -> Int
convertToInt string fallback =
    case String.toInt string of
        Err msg ->
            fallback

        Ok val ->
            val


convertToFloat : String -> Float -> Float
convertToFloat string fallback =
    case String.toFloat string of
        Err msg ->
            fallback

        Ok val ->
            val
