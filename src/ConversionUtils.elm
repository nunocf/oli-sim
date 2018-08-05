module ConversionUtils exposing (convertToInt, convertToFloat)


convertToInt : String -> Int
convertToInt string =
    case String.toInt string of
        Err msg ->
            0

        Ok val ->
            val


convertToFloat : String -> Float
convertToFloat string =
    case String.toFloat string of
        Err msg ->
            0

        Ok val ->
            val
