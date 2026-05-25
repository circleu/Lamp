module Syntax where


data Type
    = DKAbstraction Kind Type
    | DTypeIdentifier Int
    | DTypePlaceHolder
    | DUniversalType Type
    | Error -- for typechecking
    | FunctionType Type Type
    | KAbstraction (Type, Kind) Type
    | KApplication Type Type
    | RecordType [(Expression, Type)]
    | TypeIdentifier String
    deriving (Show, Eq)
data Kind
    = Kind
    | FunctionKind Kind Kind
    deriving (Show, Eq)
type Delta = (Type, Kind)
type Gamma = (Expression, Type)

type Ast = [Statement]
data Statement
    = Expression Expression 
    | Define Expression Expression
    | TypeDefine Type Type
    deriving (Show, Eq)
data Expression
    = Abstraction (Expression, Type) Expression
    | Application Expression Expression
    | Bool Bool
    | DAbstraction Type Expression
    | DIdentifier Int
    | DPlaceHolder
    | DTypeAbstraction Kind Expression
    | Fix Expression
    | Identifier String
    | IfThenElse Expression Expression Expression
    | Integer Int
    | Let (Expression, Type) Expression Expression
    | Peek Type Expression
    | Poke Type Expression Expression
    | Record [(Expression, Type)]
    | TypeAbstraction (Type, Kind) Expression
    | TypeApplication Expression Type
    deriving (Show, Eq)