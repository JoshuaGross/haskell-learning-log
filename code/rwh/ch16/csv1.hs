import Text.ParserCombinators.Parsec

{- A CSV contains 0 or more lines, each of which is terminated by the EOL character. -}
csvFile :: GenParser Char st [[String]]
csvFile =
  do result <- many line
     eof
     return result

-- Each line contains 1 or more cells, separated by a comma
line :: GenParser Char st [String]
line = 
  do result <- cells
     eol                    -- end of line
     return result

-- Build up a list of cells. Try to parse the first cell, then figure out what ends the cell.
cells :: GenParser Char st [String]
cells =
  do first <- cellContent
     next <- remainingCells
     return (first : next)

-- The cell either ends with a comma, indicating more cells; or ends with EOL indicating this is the last cell
remainingCells :: GenParser Char st [String]
remainingCells =
  (char ',' >> cells) -- found comma?
  <|> (return []) -- no comma? return [], no more cells

-- each cell contains 0 or more chars, which must not be comma or EOL
cellContent :: GenParser Char st String
cellContent =
  many (noneOf ",\n")

eol :: GenParser Char st Char
eol = char '\n'

parseCSV :: String -> Either ParseError [[String]]
parseCSV input = parse csvFile "(unknown)" input
