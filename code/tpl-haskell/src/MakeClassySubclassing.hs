{-# LANGUAGE TemplateHaskell #-}

--module MakeClassySubclassing (makeClassySubclassing) where
module MakeClassySubclassing where

import           Control.Lens
import           Control.Lens.Internal.FieldTH
import           Language.Haskell.TH
import           Language.Haskell.TH.Ppr
import           Language.Haskell.TH.Syntax

-- \> $(stringE . show =<< reify (Name (OccName "TypeZ") NameS))

-- TODO: document
-- TODO: figure out how to get reified class instance spec
-- TODO: figure out how to bind implementation so we can pass it along to Lens
-- Assumed that the [Name] argument is like [''TypeX, ''TypeY]
--  where there are instances ''HasTypeX, ''HasTypeY that we can
--  assume are present.
makeClassySubclassing :: DecsQ -> [Name] -> DecsQ
makeClassySubclassing decs names = do
  decs' <- decs

  let namesStr = map (\x -> splitStr '.' $ show x) names
  let clsNamesStr = map (\x -> "Has" ++ (last x)) namesStr
  let clsNames = map (\x -> (Name (OccName x) NameS)) clsNamesStr

  reifiedNames <- reifyNames names
  reifiedClsNames <- reifyNames clsNames

  {-
  let (ClassI (ClassD _ clsName _ _ _) _) = reifiedClsNames !! 1
  let (TyConI (DataD _ dataName _ _ _)) = reifiedNames !! 1
  reifiedInstDecl <- reifyInstances clsName [ConT dataName]
  -}

  let zippedNames = zip names reifiedClsNames

  qRunIO $ do
    {-
    print names
    print namesStr
    print clsNamesStr
    print clsNames
    print decs'-}
    {-print "==============="
    print clsName
    print dataName
    print reifiedInstDecl
    print $ ppr $ reifiedInstDecl
    print "==============="-}
    print zippedNames
    print "reified class names head"
    print $ head reifiedClsNames
    print "reified class names head ppr"
    print $ ppr $ reifiedClsNames !! 0
    print "... end reified class names ppr"
    print $ reifiedNames !! 1
    print "... end reified names ppr"

  classyDataDecl decs' zippedNames

reifyNames :: [Name] -> Q [Info]
reifyNames (n:ns) = do
  n' <- reify n
  ns' <- reifyNames ns
  return (n' : ns')
reifyNames [] = return []

-- Given a [Dec], pull out the first DataD (should be the first member of
--  the list) and:
-- * annotate it with a field for all the classes it's instancing
-- * ...
classyDataDecl :: [Dec] -> [(Name, Info)] -> DecsQ
classyDataDecl (decl@(DataD _ _ _ _ _):ds) classes = do
  let newDec = (annotateDataDWithInstFields decl classes)
  -- TODO: figure out how to bind so we can classify it
  --newDecs <- makeFieldOpticsForDec classyRules newDec
  let newDecs = [newDec]
  {-
  qRunIO $ do
    print newDecs-}
  return $ newDecs ++ ds
classyDataDecl ds _ = return ds

annotateDataDWithInstFields :: Dec -> [(Name, Info)] -> Dec
annotateDataDWithInstFields decl@(DataD cxt name tyvb (con:cons) dNames) ((clsName, hasClsInst):cs) =
  annotateDataDWithInstFields decl cs
  where decl = (DataD cxt name tyvb (con':cons) dNames)
        con' = (addFieldForClassToCon con clsName hasClsInst)
annotateDataDWithInstFields d [] = d

-- Given a constructor and some HasX class, add field for HasX to
--  fields of constructor
-- We only support Record constructors for this reason.
addFieldForClassToCon :: Con -> Name -> Info -> Con
addFieldForClassToCon (RecC name vars) clsName hasClsInst =
  rec
  where rec = (RecC name (injected:vars))
  --where rec = (RecC name vars)
        injected = (clsName', NotStrict, ConT clsName)
        clsName' = fieldNameForClassName clsName
addFieldForClassToCon _ _ _ = undefined

fieldNameForClassName :: Name -> Name
fieldNameForClassName n = Name (OccName ("_fieldFor" ++ (removeDots $ show n))) NameS

--
-- String manipulation
--
strCase :: a -> String -> (Char -> String -> a) -> a
strCase d "" _ = d
strCase _ (s:ss) f = f s ss

splitStr :: Char -> String -> [String]
splitStr f "" = [""]
splitStr f str =
  let splitStr' f prev str' = strCase [prev] str' (\s ss -> if s == f then (prev : splitStr' f "" ss) else splitStr' f (prev ++ [s]) ss) in
  splitStr' f "" str

removeDots :: String -> String
removeDots = chReplace '.' '_'

chReplace :: Char -> Char -> String -> String
chReplace f r (s:ss) = (if s == f then r else s) : (chReplace f r ss)
chReplace _ _ [] = []

-- TODO: figure out what instance fields need to be implemented: should be (.) (parent implementation) (myFieldForClass)
-- TODO: inject instance implementing each type
-- TODO: sub-instances of the injected type

{-makeClassySubclassing decs names = do
  type1 <- reify $ head names
  d' <- decs
  qRunIO $ do
    print d'
    print "Printing type for name:"
    print $ head names
    print type1
  decs-}
