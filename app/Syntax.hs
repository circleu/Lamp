module Syntax where


data Mutability
    = Mutable
    | Immutable
    deriving (Show, Eq)
data Type
    = DKAbstraction Kind Type
    | DTypeIdentifier Int
    | DTypePlaceHolder
    | DUniversalType Kind Type
    | Error -- for typechecking
    | FunctionType Type Type
    | KAbstraction (Type, Kind) Type
    | KApplication Type Type
    | TypeIdentifier String
    | BoolType
    | IntegerType
    | U1
    | U2
    | U4
    | U8
    deriving (Show, Eq)
data Kind
    = Kind
    | FunctionKind Kind Kind
    deriving (Show, Eq)
type Delta = [(Type, Kind)]
type Gamma = [(Expression, Mutability, Type)]
type DContext = [Either Kind Type]

type Ast = [Statement]
data Statement
    = Define Expression Expression
    | Expression Expression 
    | Global Mutability Expression Expression 
    | TypeDefine Type Type
    deriving (Show, Eq)
data Expression
    = Abstraction (Expression, Type) Expression
    | Addition [Expression]
    | Alloc Expression
    | And [Expression]
    | Application Expression Expression
    | Assignment Expression Expression
    | Bool Bool
    | DAbstraction Type Expression
    | DIdentifier Int
    | Division [Expression]
    | DPlaceHolder
    | DTypeAbstraction Kind Expression
    | Equal [Expression]
    | Fix Expression
    | GreaterEqual [Expression]
    | GreaterThan [Expression]
    | Identifier String
    | IfThenElse Expression Expression Expression
    | Integer Int
    | LessEqual [Expression]
    | LessThan [Expression]
    | Let Mutability Expression Expression Expression
    | LogicalAnd [Expression]
    | LogicalNot Expression
    | LogicalOr [Expression]
    | Modulo [Expression]
    | Multiplication [Expression]
    | Not Expression
    | NotEqual [Expression]
    | Or [Expression]
    | Peek Type Expression
    | Poke Type Expression Expression
    | Subtraction [Expression]
    | TypeAbstraction (Type, Kind) Expression
    | TypeApplication Expression Type
    | TypeConversion Expression Type
    | Xor [Expression]
    deriving (Show, Eq)
