module Movie where

import Data.Text (Text)

data Movie = Movie { title :: Text, movieUrl :: Text }
