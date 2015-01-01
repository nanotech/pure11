module Language.PureScript.CodeGen.Go where

import Data.List
import Data.Char

import Language.PureScript.CodeGen.JS.AST
import Language.PureScript.Pretty.Common

unqual :: String -> String
unqual s = let indices = elemIndices '.' s in
	         if null indices then s else drop (last indices + 1) s

dotsTo :: Char -> String -> String
dotsTo chr = map (\c -> if c == '.' then chr else c)

anyType :: String
anyType  = "Any"

funcDecl :: String
funcDecl = "func "

anyFunc :: String
anyFunc  = funcDecl ++ parens (anyType) ++ withSpace anyType

noArgFunc :: String
noArgFunc  = funcDecl ++ parens [] ++ withSpace anyType

appFn :: String
appFn = "AppFn"

anyList :: String
anyList = "[]" ++ anyType

getSuper :: String
getSuper = funcDecl ++ parens "" ++ withSpace anyType

capitalize :: String -> String
capitalize (x:xs) | isLower x = (toUpper x : xs)
capitalize s = s

appFnDef :: [JS]
appFnDef = map JSRaw [
              funcDecl ++ appFn ++ parens ("f " ++ anyType ++ ", args ..." ++ anyType) ++ " " ++ anyType ++ " {"
            , "  count := len(args)"
            , "  if count == 0 {"
            , "    f = f." ++ parens (noArgFunc) ++ "()"
            , "  } else {"
            , "    for i := 0; i < count; i++ {"
            , "      f = f." ++ parens (anyFunc) ++ "(args[i])"
            , "    }"
            , "  }"
            , "  return f"
            , "}"
            ]

withSpace :: String -> String
withSpace [] = []
withSpace s = (' ' : s)

-- TODO: Should type casts be done in this module or in Pretty?
--

listLen :: JS -> JS
listLen l = JSApp' (JSVar "len") [JSAccessor (parens anyList) l]
