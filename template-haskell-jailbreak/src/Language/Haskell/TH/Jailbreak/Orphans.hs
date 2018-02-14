module Language.Haskell.TH.Jailbreak.Orphans where

import Data.Binary
import Language.Haskell.TH.Syntax

instance Binary OccName

instance Binary ModName

instance Binary NameSpace

instance Binary PkgName

instance Binary NameFlavour

instance Binary Name

instance Binary Lit

instance Binary TyVarBndr

instance Binary TyLit

instance Binary Type

instance Binary Pat

instance Binary Clause

instance Binary SourceUnpackedness

instance Binary SourceStrictness

instance Binary Bang

instance Binary Con

instance Binary DerivStrategy

instance Binary DerivClause

instance Binary FunDep

instance Binary Overlap

instance Binary Callconv

instance Binary Safety

instance Binary Foreign

instance Binary FixityDirection

instance Binary Fixity

instance Binary Inline

instance Binary RuleMatch

instance Binary Phases

instance Binary RuleBndr

instance Binary AnnTarget

instance Binary Pragma

instance Binary TySynEqn

instance Binary FamilyResultSig

instance Binary InjectivityAnn

instance Binary TypeFamilyHead

instance Binary Role

instance Binary PatSynArgs

instance Binary PatSynDir

instance Binary Dec

instance Binary Stmt

instance Binary Guard

instance Binary Body

instance Binary Match

instance Binary Range

instance Binary Exp
