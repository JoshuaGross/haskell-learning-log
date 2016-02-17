{-# LANGUAGE TemplateHaskell #-}

--module MakeClassySubclassing (makeClassySubclassing) where
module MakeClassySubclassing where

import           Control.Lens
import           Control.Lens.Internal.FieldTH
import           Data.List
import           Data.Maybe
import           GHC.Err
import           Language.Haskell.TH
import           Language.Haskell.TH.Ppr
import           Language.Haskell.TH.Syntax

-- \> $(stringE . show =<< reify (Name (OccName "TypeZ") NameS))

-- Usage:
--
-- ```makeClassySubclassing [d| data TypeZ = TypeZ { _z :: Int } deriving Show |] [''TypeX, ''TypeY]```
--
-- Pass in a Template Haskell quotation of a new type with a single record constructor.
-- This function will add extra fields to it to store instances of the
--  classy types you want to "inherit" from.
-- Assumed that the [Name] argument is like [''TypeX, ''TypeY]
--  where there are instances ''HasTypeX, ''HasTypeY that we can
--  assume are present. Or, in simpler terms: TypeX and TypeY
--  should be Lens classes.
-- TODO: figure out how to get reified class instance spec (not possible currently?)
-- TODO: figure out how to bind implementation so we can pass it along to Lens
-- TODO: figure out what instance fields need to be implemented: should be (.) (parent implementation) (myFieldForClass)
-- TODO: inject instance implementing each type
-- TODO: sub-instances of the injected type
makeClassySubclassing :: DecsQ -> [Name] -> DecsQ
makeClassySubclassing decs typeNames = do
  decs' <- decs

  zippedNames <- typeToZippedTypeAndClass typeNames

  depsWithFields <- classyDataDecl decs' zippedNames

  return depsWithFields

-- Given a type name and a list of lens types to "inherit" from,
--  construct instance declarations for field access.
deriveClassyInstances :: Name -> [Name] -> DecsQ
deriveClassyInstances name typeNames = do
  zippedNames <- typeToZippedTypeAndClass typeNames

  qRunIO $ do
    print "BEFORE ISH"
    print zippedNames
    print $ map (\(x, y) -> ppr y) zippedNames
    print "AFTER ISH"

  depWithFields <- reify name

  insts <- declareInstances depWithFields zippedNames
  subInsts <- declareSubInstances depWithFields zippedNames
  return (insts ++ subInsts)

-- Given a list [''TypeX, ''TypeY, ...], return [(''TypeX, reified ''HasTypeX), ...]
typeToZippedTypeAndClass :: [Name] -> Q [(Name, Info)]
typeToZippedTypeAndClass typeNames = do
  let namesStr = map (\x -> splitStr '.' $ show x) typeNames
  let clsNamesStr = map (\x -> "Has" ++ (last x)) namesStr
  let clsNames = map (\x -> (Name (OccName x) NameS)) clsNamesStr

  reifiedClsNames <- reifyNames clsNames

  let zippedNames = zip typeNames reifiedClsNames
  return zippedNames

reifyNames :: [Name] -> Q [Info]
reifyNames (n:ns) = do
  n' <- reify n
  ns' <- reifyNames ns
  return (n' : ns')
reifyNames [] = return []

-- Given a main declaration, and zipped pairs of (type, instancetype),
--  declare instances of instancetype m => decl m.
declareInstances :: Info -> [(Name, Info)] -> DecsQ
declareInstances (TyConI (d@(DataD _ _ _ _ _))) classes = do
  mapM (declareInstance d) classes
declareInstances d [] = return []

-- First argument should be a DataD MyType, declaring a record-based data-type.
-- Second argument is a (TypeX, HasTypeX) Name,Info pair.
-- Will return an instance declaration of HasTypeX MyType.
declareInstance :: Dec -> (Name,Info) -> Q Dec
declareInstance (DataD _ name _ recs _) (typeName, cls) = do
  let clsType = getClsType cls
  let instancingType = AppT clsType (ConT name)
  let clsFields = (getClsFields cls)
  let clsFieldNames = map getFieldName clsFields
  dot <- runQ [| (.) |]
  let specialFieldTypeName = head clsFieldNames
  let specialFieldAccessor = (manipulateName stripFirstChar) $ fieldNameForClassName typeName
  let instanceFn = buildInstanceFn dot specialFieldTypeName undefined specialFieldAccessor (head clsFieldNames) 0
  return $ InstanceD [] instancingType [instanceFn]

-- Declare sub instances: we are an instance of TypeZ, and it
--  has a type constraint on TypeX and TypeY. We need to implement
--  the TypeX and TypeY interfaces as well.
declareSubInstances :: Info -> [(Name, Info)] -> DecsQ
declareSubInstances (TyConI (d@(DataD _ _ _ _ _))) classes = do
  decs <- mapM (declareSubInstancesOf' d) classes
  return $ concat decs
declareSubInstances d [] = return []

declareSubInstancesOf' :: Dec -> (Name, Info) -> DecsQ
declareSubInstancesOf' (DataD _ name _ _ _) (typeName, ClassI (ClassD cxt className _ _ _) _) = do
  typeReified <- reify typeName
  maybeDecs <- mapM (declareInstanceOfConstraint name typeName typeReified) cxt
  return $ catMaybes maybeDecs

  where

  -- Given the reified data type we're inheriting from (TypeX) and its
  --  constraint (HasTypeN => TypeX), figure out if TypeX has a field for
  --  TypeN.
  -- TODO: more than 1 obvious level of nesting
  declareInstanceOfConstraint :: Name -> Name -> Info -> Type -> Q (Maybe Dec)
  declareInstanceOfConstraint dataName typeName typeReified@(TyConI (DataD cxt _ _ [(RecC _ typeRecs)] _)) (AppT (ConT constraintName) (VarT _)) = do
    let constraintNameWithoutHas' = stripHas $ show constraintName
    let constraintNameWithoutHas = Name (OccName constraintNameWithoutHas') NameS
    let constraintGetterFieldName = fieldNameForClassNameWithDots constraintNameWithoutHas
    let typeHasFieldFor = (== 1) $ length $ filter (\(recName, _, _) -> (show recName) == (show constraintGetterFieldName)) typeRecs

    classTypeReified <- reify constraintName

    let clsType = getClsType classTypeReified
    let instancingType = AppT clsType (ConT dataName)
    let clsFields = (getClsFields classTypeReified)
    let clsFieldNames = map getFieldName clsFields
    dot <- runQ [| (.) |]
    let specialFieldTypeName = head clsFieldNames
    let specialFieldAccessor = (manipulateName stripFirstChar) $ fieldNameForClassName $ constraintNameWithoutHas
    let specialFieldParentAccessor = (manipulateName stripFirstChar) $ fieldNameForClassName typeName

    let instanceFn = buildInstanceFn dot specialFieldTypeName specialFieldParentAccessor specialFieldAccessor (head clsFieldNames) 1

    return $ if typeHasFieldFor then Just $ InstanceD [] instancingType [instanceFn] else Nothing
  declareInstanceOfConstraint _ _ _ _ = return Nothing

buildInstanceFn :: Exp -> Name -> Name -> Name -> Name -> Int -> Dec
buildInstanceFn dot specialFieldTypeName specialFieldAccessor specialFieldParentAccessor n nestLevel =
  FunD n [Clause [] (NormalB $ InfixE accessThis dot accessField) []]
  where
  accessThis = if nestLevel == 0 then id' else Just $ VarE $ specialFieldAccessor
  accessField = Just $ VarE $ specialFieldParentAccessor
  id' = Just $ VarE $ Name (OccName "id") NameS

getClsType :: Info -> Type
getClsType (ClassI (ClassD _ name _ _ _) _) = ConT name
getClsType _ = undefined

getClsFields :: Info -> [Dec]
getClsFields (ClassI (ClassD _ _ _ _ fields) _) = fields

-- Lens'd field types are pretty complicated. For now we're just
-- interested in pulling out the name.
getFieldName :: Dec -> Name
getFieldName (SigD name _) = name

-- Given a [Dec], pull out the first DataD (should be the first member of
--  the list) and annotate it with a field for all the classes it's instancing
classyDataDecl :: [Dec] -> [(Name, Info)] -> DecsQ
classyDataDecl (decl@(DataD _ _ _ _ _):ds) classes = do
  let newDec = (annotateDataDWithInstFields decl classes)
  let newDecs = [newDec]
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
        injected = (clsName', NotStrict, ConT clsName)
        clsName' = fieldNameForClassName clsName
addFieldForClassToCon _ _ _ = undefined

fieldNameForClassName' :: Bool -> Name -> Name
fieldNameForClassName' stripDots n =
  Name (OccName (dots ++ "_fieldFor" ++ (removeDots $ show n))) NameS
  where dots = if stripDots then "" else (++ ".") $ concat $ intersperse "." $ init $ splitStr '.' $ show n

fieldNameForClassName :: Name -> Name
fieldNameForClassName = fieldNameForClassName' True

fieldNameForClassNameWithDots :: Name -> Name
fieldNameForClassNameWithDots = fieldNameForClassName' False

manipulateName :: (String -> String) -> Name -> Name
manipulateName fn (Name (OccName s) NameS) = Name (OccName (fn s)) NameS

--
-- String manipulation
--
stripFirstChar :: String -> String
stripFirstChar (s:ss) = ss

-- TODO: don't use fromJust here
stripHas :: String -> String
stripHas ss = concat $ intersperse "." $ (first' ++ [last'])
  where split' = splitStr '.' ss
        first' = init split'
        last' = fromJust $ stripPrefix "Has" $ last split'

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
