
module AST ( module AST ) where

import           Data.List   ( intercalate )
import           Data.String ( IsString(fromString) )

import           Text.Printf ( printf )

newtype MoonType = MoonType (String, [MoonType])

data Ident = Imm String | Mut String

instance IsString Ident where
  fromString = Imm

mut :: String -> Ident
mut = Mut

data Lit = Int Int | String String | Bool Bool

data Expr
    = Lit Lit
    | Ident String
    | Record [(String, Expr)]
    | FunCall String [Expr]
    | If Expr Expr Expr
    | OpAdd Expr Expr
    | OpEq Expr Expr
    | OpNeq Expr Expr
    | OpGe Expr Expr
    | OpMinus Expr Expr
    | ArrayIndex Expr Expr
    | Array [Expr]
    | Raw String

data Pat = Binder String | PatLit Lit | Constructor String [Pat] | Wildcard

data Decl
    = Var Ident MoonType Expr
    | Fn String [(String, MoonType)] MoonType [Decl]
    | Match Expr [(Pat, [Decl])]
    | Assign Expr Expr
    | Return Expr
    | Expr Expr
    | Block [Decl]

type_ :: (String, [MoonType]) -> MoonType
type_ = MoonType

call :: String -> [Expr] -> Expr
call = FunCall

decl :: Ident -> MoonType -> Expr -> Decl
decl = Var

fn :: String -> [(String, MoonType)] -> MoonType -> [Decl] -> Decl
fn = Fn

match :: Expr -> [(Pat, [Decl])] -> Decl
match = Match

assign :: Expr -> Expr -> Decl
assign = Assign

return_ :: Expr -> Decl
return_ = Return

array :: [Expr] -> Expr
array = Array

brace :: Int -> String -> String
brace i s = printf "{\n%s\n%s}" s (replicate i ' ')

commas :: [String] -> String
commas = intercalate ", "

ppPat :: Pat -> String
ppPat (Binder b) = b
ppPat (PatLit l) = ppLit l
ppPat (Constructor "Tuple" args) =
    printf "(%s)" (intercalate ", " (ppPat <$> args))
ppPat (Constructor ctr args) =
    printf "%s[%s]" ctr (intercalate ", " (ppPat <$> args))
ppPat Wildcard = "_"

ppType :: MoonType -> String
ppType (MoonType (ctr, [])) = ctr
ppType (MoonType ("Tuple", args)) =
    printf "(%s)" (intercalate ", " (ppType <$> args))
ppType (MoonType (ctr, args)) =
    printf "%s[%s]" ctr (intercalate ", " (ppType <$> args))

ppLit :: Lit -> String
ppLit = go
  where
    go (Int int) = show int
    go (String s) = show s
    go (Bool b) = if b then "true" else "false"

ppExpr :: Int -> Expr -> String
ppExpr i e = replicate i ' ' <> go e
  where
    go (Lit l) = ppLit l
    go (Ident s) = s
    go (Record r) =
        printf "{ %s }"
               (intercalate ", "
                            [ printf "%s : %s" binder (ppExpr 0 body)
                            | (binder, body) <- r
                            ])
    go (FunCall fun args) = printf "%s(%s)" fun (commas (ppExpr 0 <$> args))
    go (OpAdd a b) = printf "%s + %s" (ppExpr 0 a) (ppExpr 0 b)
    go (OpEq a b) = printf "%s == %s" (ppExpr 0 a) (ppExpr 0 b)
    go (OpNeq a b) = printf "%s != %s" (ppExpr 0 a) (ppExpr 0 b)
    go (OpGe a b) = printf "%s >= %s" (ppExpr 0 a) (ppExpr 0 b)
    go (OpMinus a b) = printf "%s - %s" (ppExpr 0 a) (ppExpr 0 b)
    go (ArrayIndex arr index) =
        printf "(%s)[%s]" (ppExpr 0 arr) (ppExpr 0 index)
    go (If cond then_ else_) = printf "if %s %s %s"
                                      (ppExpr 0 cond)
                                      (brace (i + 2) (ppExpr (i + 4) then_))
                                      (brace (i + 2) (ppExpr (i + 4) else_))
    go (Array arr) = printf "[%s]" (commas (ppExpr 0 <$> arr))
    go (Raw s) = s

ppDecl :: Int -> Decl -> String
ppDecl i d = replicate i ' ' <> go d
  where
    go (Var (Imm v) t e) = printf "let %s : %s = %s" v (ppType t) (ppExpr 0 e)
    go (Var (Mut v) t e) = printf "var %s : %s = %s" v (ppType t) (ppExpr 0 e)
    go (Fn f args t ds) =
        printf "fn %s(%s) -> %s %s"
               f
               (commas [ name <> " : " <> ppType ty | (name, ty) <- args ])
               (ppType t)
               (brace i (intercalate "\n" (ppDecl (i + 2) <$> ds)))
    go (Match e ps) =
        printf "match %s %s"
               (ppExpr 0 e)
               (brace i
                      (unlines [ ppPat p <> " => "
                                   <> brace (i + 2)
                                            (unlines (ppDecl (i + 2) <$> d'))
                               | (p, d') <- ps
                               ]))
    go (Assign b e) = printf "%s = %s" (ppExpr 0 b) (ppExpr (i + 2) e)
    go (Return e) = printf "return %s" (ppExpr (i + 2) e)
    go (Expr e) = ppExpr 0 e
    go (Block ds) = brace (i + 2) (unlines (ppDecl (i + 4) <$> ds))
