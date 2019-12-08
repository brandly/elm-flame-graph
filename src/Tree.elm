module Tree exposing (..)


type Tree a
    = Tree
        { value : a
        , children : List (Tree a)
        }


value : Tree a -> a
value (Tree { value }) =
    value


children : Tree a -> List (Tree a)
children (Tree { children }) =
    children
