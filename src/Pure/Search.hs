{-# LANGUAGE PatternSynonyms, RecordWildCards, ExistentialQuantification #-}
module Pure.Search where

import Pure
import qualified Pure.Data.Txt.Search as Search
import Pure.Theme

import Data.Typeable

pattern Search :: (Search.Search a, Typeable a) => () => SearchTheme -> Txt -> [a] -> (a -> View) -> View -> View
pattern Search theme needle haystack renderer placeholder = View Search_ {..}

data SearchTheme = forall t. Themeable t => SearchTheme t

data Search a = Search_ 
    { theme       :: SearchTheme
    , needle      :: Txt 
    , haystack    :: [a] 
    , renderer    :: a -> View
    , placeholder :: View
    }

instance (Search.Search a, Typeable a) => Pure (Search a) where
    view = LibraryComponentIO $ \self -> 
        let search Search_ {..} = pure (filter (Search.contains needle) haystack)
        in
            def
                { construct = ask self >>= search
                , receive   = \np _ -> search np
                , render    = \Search_ {..} found -> 
                    case theme of
                        SearchTheme t ->
                            Div <| Theme t |>
                                case found of
                                    [] -> [ placeholder ]
                                    xs -> fmap renderer found
                }