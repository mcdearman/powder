{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

-- | The typed layer over the syntax tree.
--
-- The tree itself is homogeneous: any node can hold any children. This
-- layer says what is what. Each sort of syntax is a type wrapping a red
-- node, and each form of it is a pattern synonym over a view of that node,
-- with a @COMPLETE@ pragma saying which patterns between them cover the
-- type. The patterns are bundled with their type, so importing @Rhs (..)@
-- brings its pattern too. So code that reads syntax pattern-matches as it would on an
-- algebraic data type, and is warned about a case it leaves out, while the
-- tree underneath stays whole: 'syntax' gives the node back, with its
-- text, its position and its neighbours.
--
-- A sort with many forms has a module of its own, to be imported
-- qualified: "Powder.Syntax.AST.Decl", "Powder.Syntax.AST.Expr",
-- "Powder.Syntax.AST.Pat" and "Powder.Syntax.AST.Stmt". A variable is
-- @Expr.Var@ in an expression and @Pat.Var@ in a pattern.
module Powder.Syntax.AST
  ( Name,
    Ast (cast, syntax),
    Module,
    moduleDecls,
    moduleHeader,
    Listed (..),
    Subordinates (..),
    Decl,
    Constructor (Constructor),
    constructorFields,
    constructorInfix,
    constructorStrict,
    Rhs (Rhs),
    Type,
    Expr,
    Operator,
    operatorName,
    Alt (Alt),
    Stmt,
    Pat,
    Literal (..),
    fixityDecls,
  )
where

import Powder.Syntax.AST.Internal
import qualified Powder.Syntax.Kind as K
import qualified Data.Text as Text
import qualified Powder.Syntax.Red as Red

moduleDecls :: Module -> [Decl]
moduleDecls = children . syntax

-- | What a module's header says: its name, and what it exports if it
-- says. A module with no header is @Main@, exporting @main@.
moduleHeader :: Module -> Maybe (Name, Maybe [Listed])
moduleHeader m = case [h | h <- Red.nodes (syntax m), Red.kind h == K.ModuleHeader] of
  h : _ -> case drop 1 (spelled h) of
    (_, name) : (K.LParen, _) : rest -> Just (name, Just (itemsOf rest))
    (_, name) : _ -> Just (name, Nothing)
    [] -> Nothing
  [] -> Nothing

-- | Every fixity declaration in a module, at any depth.
fixityDecls :: Module -> [Decl]
fixityDecls m = [d | r <- Red.descendants (syntax m), Red.kind r == K.FixityDecl, Just d <- [cast r]]

-- | A constructor's name and the types of its fields.
pattern Constructor :: Name -> [Type] -> Constructor
pattern Constructor name fields <- (constructor -> (name, fields))

{-# COMPLETE Constructor #-}

constructor :: Constructor -> (Name, [Type])
constructor (syntax -> r) = (name, types)
  where
    -- Written between its fields, it is an operator.
    name = case [o | o <- Red.nodes r, Red.kind o == K.Operator] of
      o : _ -> nameOf o
      [] -> nameOf r
    types = case fieldDecls r of
      [] -> children r
      fs -> concat [replicate (length (fieldNames f)) (child 0 f) | f <- fs]

fieldDecls :: Red.Red -> [Red.Red]
fieldDecls r = [f | f <- Red.nodes r, Red.kind f == K.FieldDecl]

fieldNames :: Red.Red -> [Name]
fieldNames f = [x | (k, x) <- takeWhile ((/= K.DoubleColon) . fst) (spelled f), k `elem` [K.VarId, K.VarSym]]

-- | Which of a constructor's fields are marked strict, in order.
constructorStrict :: Constructor -> [Bool]
constructorStrict (syntax -> r) = case fieldDecls r of
  [] -> marked (Red.children r)
  fs -> concat [replicate (length (fieldNames f)) (or (marked (Red.children f))) | f <- fs]
  where
    -- Each type, with whether a mark came just before it.
    marked = go False
    go _ [] = []
    go flag (c : cs)
      | Red.kind c == K.Bang || (Red.kind c == K.VarSym && Red.text c == Text.pack "!") = go True cs
      | Red.isToken c = go flag cs
      | Red.kind c == K.Operator = go False cs
      | otherwise = flag : go False cs

-- | Whether a constructor was declared between its two fields.
constructorInfix :: Constructor -> Bool
constructorInfix (syntax -> r) = any ((== K.Operator) . Red.kind) (Red.nodes r)

-- | The names of a constructor's fields, in order, if it is a record's.
constructorFields :: Constructor -> [Name]
constructorFields = concatMap fieldNames . fieldDecls . syntax

-- | A right-hand side: its alternatives, each the qualifiers of its guard,
-- if it has one, and a body; and the declarations of its @where@.
pattern Rhs :: [([Stmt], Expr)] -> [Decl] -> Rhs
pattern Rhs alternatives bindings <- (rhs -> (alternatives, bindings))

{-# COMPLETE Rhs #-}

rhs :: Rhs -> ([([Stmt], Expr)], [Decl])
rhs (syntax -> r) = (alternatives, concatMap children wheres)
  where
    guarded = [g | g <- Red.nodes r, Red.kind g == K.GuardedRhs]
    wheres = [w | w <- Red.nodes r, Red.kind w == K.WhereClause]
    alternatives
      | null guarded = [([], child 0 r)]
      | otherwise = [(children g, child 0 g) | g <- guarded]

-- | A case alternative.
pattern Alt :: Pat -> Rhs -> Alt
pattern Alt pat body <- (alt -> (pat, body))

{-# COMPLETE Alt #-}

alt :: Alt -> (Pat, Rhs)
alt (syntax -> r) = (child 0 r, child 0 r)

-- | The name an operator spells: @+@, or @div@ for @`div`@.
operatorName :: Operator -> Name
operatorName = nameOf . syntax
