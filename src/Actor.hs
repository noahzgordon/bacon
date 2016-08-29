module Actor where

import Data.Text (Text)

data Actor = Actor { name :: Text, actorUrl :: Text }
