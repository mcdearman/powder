-- | Language extensions and editions: what a module may be written in.
--
-- An edition is a set of extensions under one name. A module is written
-- in the edition its package says, or the one asked for where it is
-- compiled, and failing both in 'Powder2026'; with the extensions that
-- are asked for beside the edition turned on or off; and then with what
-- its own @LANGUAGE@ pragmas say, in order, each naming an extension to
-- turn on, one to turn off with @No@ before it, or an edition to start
-- again from.
--
-- Every extension of GHC's editions is known by name here, and some
-- beyond them. Not all are implemented. An edition turns on those of its
-- extensions that are; asking for one that is not, by name, is an error,
-- so that a program that needs it is not compiled as if it did not.
module Powder.Extensions
  ( Extension (..),
    Edition (..),
    Extensions,
    Settings (..),
    defaultSettings,
    haskell2010,
    implemented,
    reportPrelude,
    editionExtensions,
    editionNamed,
    change,
    extensionsOf,
    pragmaWords,
  )
where

import Data.Char (isSpace, toUpper)
import Data.Either (lefts, rights)
import Data.List (isPrefixOf)
import qualified Data.Set as Set
import Text.Read (readMaybe)

data Extension
  = BangPatterns
  | BinaryLiterals
  | BlockArguments
  | ConstrainedClassMethods
  | ConstraintKinds
  | DataKinds
  | DefaultSignatures
  | DeriveAnyClass
  | DeriveDataTypeable
  | DeriveFoldable
  | DeriveFunctor
  | DeriveGeneric
  | DeriveLift
  | DeriveTraversable
  | DerivingStrategies
  | DerivingVia
  | DisambiguateRecordFields
  | EmptyCase
  | EmptyDataDeriving
  | ExistentialQuantification
  | ExplicitForAll
  | ExplicitNamespaces
  | FlexibleContexts
  | FlexibleInstances
  | FunctionalDependencies
  | GADTSyntax
  | GADTs
  | GeneralisedNewtypeDeriving
  | HexFloatLiterals
  | ImportQualifiedPost
  | InstanceSigs
  | KindSignatures
  | LambdaCase
  | LexicalNegation
  | MonoLocalBinds
  | MultiParamTypeClasses
  | MultiWayIf
  | NamedFieldPuns
  | NamedWildCards
  | NumDecimals
  | NumericUnderscores
  | OverloadedStrings
  | PackageImports
  | ParallelListComp
  | PatternSynonyms
  | PolyKinds
  | PostfixOperators
  | RankNTypes
  | RecordWildCards
  | -- | Powder's own: the Prelude is the Report's names and no others.
    ReportPrelude
  | RoleAnnotations
  | ScopedTypeVariables
  | StandaloneDeriving
  | StandaloneKindSignatures
  | StrictData
  | TupleSections
  | TypeApplications
  | TypeFamilies
  | TypeOperators
  | TypeSynonymInstances
  | UndecidableInstances
  | UnicodeSyntax
  | ViewPatterns
  | AllowAmbiguousTypes
  | PartialTypeSignatures
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

-- | The extensions a module is written with.
type Extensions = Set.Set Extension

data Edition
  = -- | The Report's language, and of the Prelude only the names the
    -- Report gives it.
    Haskell2010
  | GHC2021
  | GHC2024
  | -- | Powder's own: the extensions it is thought a program should be
    -- written with today.
    Powder2026
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

-- | What is asked for where a module is compiled: an edition, and
-- extensions to turn on or off beside it.
data Settings = Settings {settingsEdition :: Edition, settingsChanges :: [(Extension, Bool)]}
  deriving (Eq, Show)

defaultSettings :: Settings
defaultSettings = Settings Powder2026 []

-- | The Report's language, as the libraries and the conformance suite
-- are written in.
haskell2010 :: Settings
haskell2010 = Settings Haskell2010 []

-- | The module that is the Prelude where 'ReportPrelude' is on.
reportPrelude :: String
reportPrelude = "Powder.Prelude2010"

-- | Whether the compiler does what an extension asks.
implemented :: Extension -> Bool
implemented =
  ( `elem`
      [ AllowAmbiguousTypes,
        BangPatterns,
        BinaryLiterals,
        BlockArguments,
        ConstrainedClassMethods,
        ConstraintKinds,
        DeriveAnyClass,
        DeriveFoldable,
        DeriveFunctor,
        DeriveTraversable,
        DerivingStrategies,
        DerivingVia,
        DisambiguateRecordFields,
        EmptyCase,
        EmptyDataDeriving,
        ExistentialQuantification,
        ExplicitForAll,
        ExplicitNamespaces,
        FlexibleContexts,
        FlexibleInstances,
        FunctionalDependencies,
        GADTSyntax,
        GADTs,
        GeneralisedNewtypeDeriving,
        HexFloatLiterals,
        ImportQualifiedPost,
        InstanceSigs,
        KindSignatures,
        LambdaCase,
        LexicalNegation,
        MonoLocalBinds,
        MultiParamTypeClasses,
        MultiWayIf,
        NamedFieldPuns,
        NamedWildCards,
        NumDecimals,
        NumericUnderscores,
        OverloadedStrings,
        PackageImports,
        ParallelListComp,
        PartialTypeSignatures,
        PatternSynonyms,
        PostfixOperators,
        RankNTypes,
        RecordWildCards,
        ReportPrelude,
        RoleAnnotations,
        ScopedTypeVariables,
        StandaloneDeriving,
        StandaloneKindSignatures,
        StrictData,
        TupleSections,
        TypeApplications,
        TypeOperators,
        TypeSynonymInstances,
        UndecidableInstances,
        UnicodeSyntax,
        ViewPatterns
      ]
  )

-- | The extensions of an edition, implemented or not. What the Report
-- itself has, which GHC counts as extensions too, is always there and is
-- not listed.
editionExtensions :: Edition -> [Extension]
editionExtensions = \case
  Haskell2010 -> [ReportPrelude]
  GHC2021 ->
    [ BangPatterns,
      BinaryLiterals,
      ConstrainedClassMethods,
      ConstraintKinds,
      DeriveDataTypeable,
      DeriveFoldable,
      DeriveFunctor,
      DeriveGeneric,
      DeriveLift,
      DeriveTraversable,
      EmptyCase,
      EmptyDataDeriving,
      ExistentialQuantification,
      ExplicitForAll,
      FlexibleContexts,
      FlexibleInstances,
      GADTSyntax,
      GeneralisedNewtypeDeriving,
      HexFloatLiterals,
      ImportQualifiedPost,
      InstanceSigs,
      KindSignatures,
      MultiParamTypeClasses,
      NamedFieldPuns,
      NamedWildCards,
      NumericUnderscores,
      PolyKinds,
      PostfixOperators,
      RankNTypes,
      ScopedTypeVariables,
      StandaloneDeriving,
      StandaloneKindSignatures,
      TupleSections,
      TypeApplications,
      TypeOperators,
      TypeSynonymInstances
    ]
  GHC2024 ->
    editionExtensions GHC2021
      ++ [DataKinds, DerivingStrategies, DisambiguateRecordFields, ExplicitNamespaces, GADTs, LambdaCase, MonoLocalBinds, RoleAnnotations]
  -- What is implemented of the extensions that only add syntax, each of
  -- which a program either uses or cannot tell is on. One that changes
  -- what a program already means, as StrictData, LexicalNegation and
  -- NumDecimals do, is left to be asked for.
  Powder2026 ->
    [ BangPatterns,
      BinaryLiterals,
      BlockArguments,
      ConstrainedClassMethods,
      ConstraintKinds,
      DeriveFoldable,
      DeriveFunctor,
      DeriveTraversable,
      DerivingStrategies,
      DerivingVia,
      DisambiguateRecordFields,
      EmptyCase,
      EmptyDataDeriving,
      ExistentialQuantification,
      ExplicitForAll,
      ExplicitNamespaces,
      FlexibleContexts,
      FlexibleInstances,
      FunctionalDependencies,
      GADTSyntax,
      GADTs,
      GeneralisedNewtypeDeriving,
      HexFloatLiterals,
      ImportQualifiedPost,
      InstanceSigs,
      KindSignatures,
      LambdaCase,
      MonoLocalBinds,
      MultiParamTypeClasses,
      MultiWayIf,
      NamedFieldPuns,
      NumericUnderscores,
      PackageImports,
      ParallelListComp,
      PatternSynonyms,
      PostfixOperators,
      RankNTypes,
      RecordWildCards,
      RoleAnnotations,
      ScopedTypeVariables,
      StandaloneDeriving,
      StandaloneKindSignatures,
      TupleSections,
      TypeApplications,
      TypeOperators,
      TypeSynonymInstances,
      UnicodeSyntax,
      ViewPatterns
    ]

editionNamed :: String -> Maybe Edition
editionNamed = readMaybe

-- | What GHC calls extensions that are the Report's own language here:
-- naming one changes nothing, and none can be turned off.
always :: [String]
always =
  [ "DoAndIfThenElse",
    "EmptyDataDecls",
    "FieldSelectors",
    "ForeignFunctionInterface",
    "ImplicitPrelude",
    "MonomorphismRestriction",
    "PatternGuards",
    "RelaxedPolyRec",
    "StarIsType",
    "TraditionalRecordSyntax",
    "ListTuplePuns",
    "DatatypeContexts"
  ]

-- | What a word of a pragma, or one given where a module is compiled,
-- asks: an extension on or off. Says what is wrong with it if it is not
-- one that can be done.
change :: String -> Either String (Maybe (Extension, Bool))
change word
  | word `elem` always = Right Nothing
  | "No" `isPrefixOf` word, drop 2 word `elem` always = Left (drop 2 word ++ " is part of the language here, and cannot be turned off")
  | Just extension <- named word = wanted extension True
  | "No" `isPrefixOf` word, Just extension <- named (drop 2 word) = Right (Just (extension, False))
  | otherwise = Left ("there is no language extension called " ++ word)
  where
    -- GHC spells one of them two ways.
    named w = readMaybe (if w == "GeneralizedNewtypeDeriving" then "GeneralisedNewtypeDeriving" else w)
    wanted extension on
      | implemented extension = Right (Just (extension, on))
      | otherwise = Left ("the extension " ++ show extension ++ " is not supported yet")

-- | The extensions a module is written with, given what was asked for
-- where it is compiled and the words of its pragmas; or what is wrong
-- with what was asked.
extensionsOf :: Settings -> [String] -> Either [String] Extensions
extensionsOf settings wordsOfPragmas = case lefts steps of
  [] -> Right (foldl apply (foldl (flip turn) (start (settingsEdition settings)) (settingsChanges settings)) (rights steps))
  wrong -> Left wrong
  where
    start edition = Set.fromList (filter implemented (editionExtensions edition))
    turn (extension, on) = if on then Set.insert extension else Set.delete extension
    steps = map step wordsOfPragmas
    step word = case editionNamed word of
      Just edition -> Right (Left edition)
      Nothing -> Right <$> change word
    apply set = \case
      Left edition -> start edition
      Right (Just turned) -> turn turned set
      Right Nothing -> set

-- | The words of a pragma that says what language a module is in:
-- @{-\# LANGUAGE A, B \#-}@ gives @A@ and @B@. Any other pragma gives
-- none.
pragmaWords :: String -> [String]
pragmaWords text = case words (map (\c -> if c == ',' then ' ' else c) inside) of
  keyword : rest | map toUpper keyword == "LANGUAGE" -> rest
  _ -> []
  where
    inside = reverse (drop 3 (dropWhile isSpace (reverse (drop 3 text))))
