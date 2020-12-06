-- | Utility modules for working with AOC, such as reading lists from files and
-- whatnot
module Utils
  ( listFromFile,
    Problem (..),
  )
where

-- | Parse a line delimited list from an input file
--
-- A file of the form:
--
-- ```txt
-- a
-- b
-- ```
--
-- Will be parsed into `['a', 'b']`. This works with any type that implemented
-- `Read`. This uses the naive `readFile` method that reads the `String` type
-- and is lazy, and could be better.
listFromFile :: Read a => FilePath -> IO [a]
listFromFile filename = do
  contents <- readFile filename
  let lines' = lines contents
  pure $ read <$> lines'

-- | A structure representing an AOC problem
data Problem a = Problem
  { -- | The day corresponding to the problem
    day :: Int,
    -- | A brief name/description of the problem
    shortName :: String,
    -- | The solution to the problem. This must implement `Show` so that
    -- results can be printed to the terminal.
    result :: a
  }

instance Show a => Show (Problem a) where
  show (Problem d name res) =
    mconcat
      [ "Day ",
        show d,
        ": ",
        name,
        "\n",
        "result: ",
        show res
      ]
