module TreeZipper
    exposing
        ( TreeZipper(..)
        , down
        , downN
        , fromTree
        , left
        , right
        , toTree
        , up
        )

import Tree exposing (Tree(..))


{-| A 'TreeZipper' represents a focused point in a 'Tree'.

       ___ A ____
      |  |   |   |
      B  C  [D]  G
           |   |
           E   F

    The above diagram has:

      focus  = D

      parent = Just (the zipper focused on) A

      lefts  = [C, B] -- note: reverse order

      rights = [G]

-}
type TreeZipper a
    = TreeZipper
        { focus : Tree a
        , parent : Maybe (TreeZipper a)
        , lefts : List (Tree a)
        , rights : List (Tree a)
        }


{-| Create a 'TreeZipper' from a 'Tree'.
-}
fromTree : Tree a -> TreeZipper a
fromTree tree =
    TreeZipper
        { focus = tree
        , parent = Nothing
        , lefts = []
        , rights = []
        }


{-| Create a 'Tree' from a 'TreeZipper'.
-}
toTree : TreeZipper a -> Tree a
toTree (TreeZipper z) =
    z.focus


{-| Move down to the current focus's first child, if any.
-}
down : TreeZipper a -> Maybe (TreeZipper a)
down (TreeZipper z) =
    case Tree.children z.focus of
        [] ->
            Nothing

        t :: ts ->
            Just <|
                TreeZipper
                    { focus = t
                    , parent = Just (TreeZipper z)
                    , lefts = []
                    , rights = ts
                    }


{-| Move down to the current focus's nth child (0-indexed), if it exists.
-}
downN : Int -> TreeZipper a -> Maybe (TreeZipper a)
downN n0 z =
    let
        inward : Int -> TreeZipper a -> Maybe (TreeZipper a)
        inward n z =
            if n <= 0 then
                Just z
            else
                right z
                    |> Maybe.andThen (inward (n - 1))
    in
    down z
        |> Maybe.andThen (inward n0)


{-| Move up to the current focus's parent, if any.
-}
up : TreeZipper a -> Maybe (TreeZipper a)
up (TreeZipper { parent }) =
    parent


{-| Move left to the current focus's previous sibling, if any.
-}
left : TreeZipper a -> Maybe (TreeZipper a)
left (TreeZipper z) =
    case z.lefts of
        [] ->
            Nothing

        t :: ts ->
            Just <|
                TreeZipper
                    { focus = t
                    , parent = z.parent
                    , lefts = ts
                    , rights = z.focus :: z.rights
                    }


{-| Move right to the current focus's next sibling, if any.
-}
right : TreeZipper a -> Maybe (TreeZipper a)
right (TreeZipper z) =
    case z.rights of
        [] ->
            Nothing

        t :: ts ->
            Just <|
                TreeZipper
                    { focus = t
                    , parent = z.parent
                    , lefts = z.focus :: z.lefts
                    , rights = ts
                    }
