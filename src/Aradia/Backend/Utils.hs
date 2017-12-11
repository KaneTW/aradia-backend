module Aradia.Backend.Utils where

import Control.Lens
import Control.Monad
import Data.Aeson
import qualified Data.Aeson.Encoding as AE
import Data.Aeson.TH
import Data.Char
import Data.Scientific
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Data.List (sortOn)
import Data.List.Split

import Math.NumberTheory.Logarithms

-- adapted from https://hackage.haskell.org/package/api-field-json-th
deriveApiFieldJSON :: Options -> Name -> Q [Dec]
deriveApiFieldJSON options name = deriveJSON options {fieldLabelModifier = camelTo2 '_' . dropPrefix name} name

typeName :: String -> String
typeName = view _last . splitOn "."

dropPrefix :: Name -> String -> String
dropPrefix = drop . (1 + ) . length . typeName . show

discordTypes :: QuasiQuoter
discordTypes = QuasiQuoter { quoteExp = parseDiscordTypes
                           , quotePat = error "Can't parse as pattern"
                           , quoteType = error "Can't parse as type"
                           , quoteDec = error "Can't parse as dec" }
  where
    parseDiscordTypes expr = let res = map parseCon . filter (not . null) . map words $ lines expr
                             in [|res|]

    parseCon :: [String] -> (String, Integer)
    parseCon (ident:val:_) = (ident, read val)
    parseCon _ = error "Incorrect constructor format"

asBitfield :: [(String, Integer)] -> [(String, Integer)]
asBitfield = map toBit
  where
    toBit (s,n) = let bit = fromIntegral $ integerLog2 n
                  in if 2^bit /= n then error $ s ++ " sets more than one bit or is zero"
                     else (s, fromIntegral bit)

mkEnum :: String -> [(String, Integer)] -> Q [Dec]
mkEnum name conmap = do
  derivs <- sequence [[t|Show|], [t|Eq|], [t|Ord|]]
  let ddecl = DataD [] tyname [] Nothing cons [ DerivClause Nothing derivs ]
  let idecl = InstanceD Nothing [] (AppT (ConT ''Enum) (ConT tyname)) [mkFromEnum, mkToEnum]
  parseJSONDec <- mkParseJSON
  let fromJsonDecl = InstanceD Nothing [] (AppT (ConT ''FromJSON) (ConT tyname)) parseJSONDec
  toJSONDec <- mkToJSON
  let toJsonDecl = InstanceD Nothing [] (AppT (ConT ''ToJSON) (ConT tyname)) toJSONDec
  return [ddecl, idecl, fromJsonDecl, toJsonDecl]
  
  where
    tyname = mkName name
    cm = sortOn snd $ map (over _1 (mkName . (name++) . camelCase)) conmap
    cons = map (\x -> NormalC (fst x) []) cm

    mkFromEnum = FunD 'fromEnum $ map (\(n,v) ->  Clause [ConP n []] (NormalB . LitE $ IntegerL v) []) cm
    mkToEnum = FunD 'toEnum $ map (\(n,v) ->  Clause [LitP $ IntegerL v] (NormalB $ ConE n) []) cm

    

    mkParseJSONInternal splice = [d|
      $(varP 'parseJSON) = withScientific name $ \n ->
        case toBoundedInteger n :: Maybe Word of
          Just n -> $(splice) n
          _ -> fail "Expected a Word" |]

    mkParseJSON = let splice = lamCaseE $ matches ++
                        [match wildP (normalB $ [|fail $ "No such enum when parsing " ++ name|]) []]
                      matches = map (\(con, val) -> match
                                                    (litP $ integerL val)
                                                    (normalB $ appE (varE 'pure) (conE con))
                                                    []) cm
                  in mkParseJSONInternal splice

    mkToJSON = [d|
      $(varP 'toJSON) = toJSON . fromEnum
      $(varP 'toEncoding) = AE.int . fromEnum |]

    camelCase = concatMap (\(h:t) -> toUpper h : map toLower t) . splitOn "_"

fixedTypeFields :: LensRules
fixedTypeFields = defaultFieldRules & lensField .~ (\tyn fs f -> map fixDefName $ camelCaseNamer tyn fs f)
  where
    fixDefName (TopName n) = TopName (fixName n)
    fixDefName (MethodName c n) = MethodName c (fixName n)

    fixName n = mkName $ case nameBase n of
      "type" -> "type'"
      "id" -> "id'"
      s -> s
