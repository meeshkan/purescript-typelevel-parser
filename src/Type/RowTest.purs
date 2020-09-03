module RowTest where

import Prim.Row (class Lacks)

class Ugh (row :: # Type)

instance lacksR :: Lacks "Foo" c => Ugh c
