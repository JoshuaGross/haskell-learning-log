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
import           MakeClassyConstraints

-- \> $(stringE . show =<< reify (Name (OccName "TypeZ") NameS))

-- makeClassyAwesome:
-- 1. Inject fields necessary to hold subclassed values
-- 2. makeClassy on new data type
-- 3. Derive instances for field accessors all the way up the "inheritance" chain
--
-- The type name passed in must end in a prime (').
--
-- Haskell doesn't seem to return the list of names that `n` is deriving, so it must be
--  provided as an argument.
makeClassyAwesome :: Name -> [Name] -> [Name] -> [Name] -> DecsQ
makeClassyAwesome n constraints derives extraConstraints = do
  let constraintsWithHas = map (nameWithoutModulePrefix . nameWithHasPrefix) constraints

  recordType'' <- reify n
  let recordType' = removePrimesFromTyCon recordType''
  recordType <- makeClassySubclassingForDec recordType' (constraints ++ extraConstraints)

  depWithConstraints <- makeClassyConstraintsForDec (head recordType) constraintsWithHas

  derivedInstances <- deriveClassyInstancesForDec (head recordType) (head depWithConstraints)

  return $ recordType ++ depWithConstraints ++ derivedInstances

  where

  removePrimesFromTyCon :: Info -> Dec
  removePrimesFromTyCon (TyConI (DataD a bName c [RecC dName fields] e)) =
    (DataD a bName' c [RecC dName' (map removePrimesFromField fields)] (e ++ derives))
    where bName' = removeTerminalPrime bName
          dName' = removeTerminalPrime dName
          removePrimesFromField (x,y,z) = (removeTerminalPrime x, y, z)
          removeTerminalPrime = nameWithoutModulePrefix . (manipulateName $ stripSuffix "'")
  removePrimesFromTyCon _ = undefined

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
makeClassySubclassingForDec :: Dec -> [Name] -> DecsQ
makeClassySubclassingForDec dec typeNames = do
  zippedNames <- typeToZippedTypeAndClass typeNames

  depsWithFields <- classyDataDecl [dec] zippedNames

  return depsWithFields

-- Given a type name and a list of lens types to "inherit" from,
--  construct instance declarations for field access.
deriveClassyInstancesForDec :: Dec -> Dec -> DecsQ
deriveClassyInstancesForDec depWithFields classDep = do
  -- get instances that we may be able to derive
  let constraints = map (nameWithoutHasPrefix . extractConstraintClass) (getConstraints classDep)
  zippedNames <- typeToZippedTypeAndClass constraints

  declareInstances depWithFields zippedNames

  where

  getConstraints :: Dec -> [Type]
  getConstraints (DataD cxt _ _ _ _) = cxt
  getConstraints (ClassD cxt _ _ _ _) = cxt
  getConstraints _ = undefined

-- Given a list [''TypeX, ''TypeY, ...], return [(''TypeX, reified ''HasTypeX), ...]
typeToZippedTypeAndClass :: [Name] -> Q [(Name, Info)]
typeToZippedTypeAndClass typeNames = do
  let clsNames = map (nameWithoutModulePrefix . nameWithHasPrefix) typeNames

  reifiedClsNames <- mapM reify clsNames

  let zippedNames = zip typeNames reifiedClsNames
  return zippedNames

-- Given a main declaration, and zipped pairs of (type, instancetype),
--  declare instances of instancetype m => decl m.
declareInstances :: Dec -> [(Name, Info)] -> DecsQ
declareInstances d@(DataD _ _ _ _ _) classes = do
  insts <- mapM (declareInstance d) classes
  subInsts <- mapM (declareSubInstancesOf' d) classes
  return (insts ++ (concat subInsts))
declareInstances d [] = return []

-- First argument should be a DataD MyType, declaring a record-based data-type.
-- Second argument is a (TypeX, HasTypeX) Name,Info pair.
-- Will return an instance declaration of HasTypeX MyType.
declareInstance :: Dec -> (Name,Info) -> Q Dec
declareInstance (DataD _ name _ _ _) (typeName, cls) = do
  let clsType = getClsType cls
  let instancingType = AppT clsType (ConT name)
  let clsFields = getClsFields cls
  let clsFieldNames = map getFieldName clsFields
  let specialFieldTypeName = head clsFieldNames
  let specialFieldAccessor = (manipulateName stripFirstChar) $ fieldNameForClassName typeName
  instanceFn <- buildInstanceFn specialFieldTypeName [specialFieldAccessor] (head clsFieldNames) 0
  return $ InstanceD [] instancingType [instanceFn]

-- Declare sub instance: we are an instance of TypeZ, and it
--  has a type constraint on TypeX and TypeY. We need to implement
--  the TypeX and TypeY interfaces as well.
declareSubInstancesOf' :: Dec -> (Name, Info) -> DecsQ
declareSubInstancesOf' d@(DataD _ name _ _ _) (typeName, ClassI (ClassD cxt className _ _ _) _) = do
  -- The "Lib.X" token will not be in scope, but "X" will be
  typeReified <- reify $ nameWithoutModulePrefix typeName

  decs <- mapM (declareInstanceOfConstraint name typeName typeReified) cxt

  return $ (concat decs)

  where

  -- Given the reified data type we're inheriting from (TypeX) and its
  --  constraint (HasTypeN => TypeX), figure out if TypeX has a field for
  --  TypeN.
  declareInstanceOfConstraint :: Name -> Name -> Info -> Type -> DecsQ
  declareInstanceOfConstraint dataName typeName (TyConI (DataD _ _ _ [(RecC _ typeRecs)] _)) (AppT (ConT constraintName) (VarT _)) = do
    let constraintNameWithoutHas = nameWithoutHasPrefix constraintName
    let constraintGetterFieldName = fieldNameForClassNameWithDots constraintNameWithoutHas
    let typeHasFieldFor = (== 1) $ length $ filter (\(recName, _, _) -> (show $ nameWithoutModulePrefix recName) == (show $ nameWithoutModulePrefix constraintGetterFieldName)) typeRecs

    classTypeReified <- reify constraintName

    -- recurse - get sub-sub classes, etc...
    subsubinsts <- declareSubInstancesOf' d (constraintNameWithoutHas, classTypeReified)

    let clsType = getClsType classTypeReified
    let instancingType = AppT clsType (ConT dataName)
    let clsFields = (getClsFields classTypeReified)
    let clsFieldNames = map getFieldName clsFields
    let specialFieldTypeName = head clsFieldNames
    let specialFieldAccessor = (manipulateName stripFirstChar) $ fieldNameForClassName $ constraintNameWithoutHas
    let specialFieldParentAccessor = (manipulateName stripFirstChar) $ fieldNameForClassName typeName

    instanceFn <- buildInstanceFn specialFieldTypeName [specialFieldParentAccessor, specialFieldAccessor] (head clsFieldNames) 1

    return $ (if typeHasFieldFor then [InstanceD [] instancingType [instanceFn]] else []) ++ subsubinsts
  declareInstanceOfConstraint _ _ _ _ = return []

buildInstanceFn :: Name -> [Name] -> Name -> Int -> Q Dec
buildInstanceFn specialFieldTypeName accessors n nestLevel = do
  dot <- runQ [| (.) |]
  let expr' = foldr (\e prev -> Just $ InfixE e dot prev) (last accessors') (init accessors')
  return $ FunD n [Clause [] (NormalB $ fromJust expr') []]
  where
  accessors' = map (Just . VarE) accessors

getClsType :: Info -> Type
getClsType (ClassI (ClassD _ name _ _ _) _) = ConT name
getClsType _ = undefined

getClsFields :: Info -> [Dec]
getClsFields (ClassI (ClassD _ _ _ _ fields) _) = fields
getClsFields _ = undefined

extractConstraintClass :: Type -> Name
extractConstraintClass (AppT (ConT cxtCls) _) = cxtCls
extractConstraintClass _ = undefined

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

--
-- String manipulation
--

-- TODO: ensure that module part is always in the name; sometimes it's not present, so names
--  are inconsistent, for now, unless we always strip module
fieldNameForClassName' :: Bool -> Name -> Name
fieldNameForClassName' stripDots n =
  --Name (OccName (dots ++ "_fieldFor" ++ (removeDots $ show n))) NameS
  Name (OccName (dots ++ "_fieldFor" ++ (show $ nameWithoutModulePrefix n))) NameS
  where dots = ""
    --dots = if stripDots then "" else (++ ".") $ concat $ intersperse "." $ init $ splitStr '.' $ show n

fieldNameForClassName :: Name -> Name
fieldNameForClassName = fieldNameForClassName' True

fieldNameForClassNameWithDots :: Name -> Name
fieldNameForClassNameWithDots = fieldNameForClassName' False

nameWithHasPrefix :: Name -> Name
nameWithHasPrefix = manipulateName (\s -> let s' = splitStr '.' s in concat $ intersperse "." $ (init s') ++ ["Has" ++ (last s')])

nameWithoutHasPrefix :: Name -> Name
nameWithoutHasPrefix = manipulateName (\s -> let s' = splitStr '.' s in concat $ intersperse "." $ (init s') ++ [stripHas (last s')])

nameWithoutModulePrefix :: Name -> Name
nameWithoutModulePrefix = manipulateName (\s -> let s' = splitStr '.' s in last s')

manipulateName :: (String -> String) -> Name -> Name
manipulateName fn n = Name (OccName (fn $ show n)) NameS

stripFirstChar :: String -> String
stripFirstChar (s:ss) = ss

stripSuffix :: String -> String -> String
stripSuffix suf str =
  let suf' = reverse suf in
  let str' = reverse str in
  case stripPrefix suf' str' of
    Nothing -> str
    Just res -> reverse res

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
