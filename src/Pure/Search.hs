{-# LANGUAGE PatternSynonyms, RecordWildCards, ExistentialQuantification, MagicHash, ScopedTypeVariables #-}
module Pure.Search (pattern Search, SearchTheme(..), Search(), module Search) where

import Pure
import qualified Pure.Data.Txt.Search as Search
import Pure.Theme

import Data.Typeable

pattern Search :: (Search.Search a, Typeable a) => () => Search.SearchOptions -> SearchTheme -> Txt -> [a] -> (a -> View) -> View -> View
pattern Search options theme needle haystack renderer placeholder = View Search_ {..}

data SearchTheme = forall t. Themeable t => SearchTheme t

data Search a = Search_
    { options     :: Search.SearchOptions
    , theme       :: SearchTheme
    , needle      :: Txt
    , haystack    :: [a]
    , renderer    :: a -> View
    , placeholder :: View
    }

instance (Search.Search a, Typeable a) => Pure (Search a) where
    view = LibraryComponentIO $ \self ->
        let
            search Search_ {..} = Search.containing options needle haystack
        in
            def
                { construct = ask self >>= pure . search
                , receive   = \np _ -> pure (search np)
                , render    = \Search_ {..} found ->
                    case theme of
                        SearchTheme t ->
                            Div <| Theme t |>
                                case found of
                                    [] -> [ placeholder ]
                                    xs -> fmap renderer found
                }