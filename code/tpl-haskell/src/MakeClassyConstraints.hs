{-# LANGUAGE TemplateHaskell #-}

module MakeClassyConstraints (makeClassyConstraints) where

import           Control.Lens
import           Language.Haskell.TH

makeClassyConstraints :: Name -> [Name] -> DecsQ
makeClassyConstraints n ns = do
  decs <- makeClassy n
  return (makeClassyConstraints' ns decs)

makeClassyConstraints' :: [Name] -> [Dec] -> [Dec]
makeClassyConstraints' ns (d:ds) = addConstraintsTo d ns : ds
makeClassyConstraints' _ ds = ds

addConstraintsTo :: Dec -> [Name] -> Dec
addConstraintsTo (ClassD constraints name typeVars f d) (n':ns) =
  addConstraintsTo cls ns
  where cls = (ClassD (constraint:constraints) name typeVars f d)
        constraint = (AppT (ConT n') (VarT cVarName))
        cVarName = getFirstTypeVar typeVars
addConstraintsTo decl@_ _ = decl

getFirstTypeVar :: [TyVarBndr] -> Name
getFirstTypeVar ((PlainTV n):ts) = n
getFirstTypeVar (_:ts) = getFirstTypeVar ts
getFirstTypeVar [] = undefined
