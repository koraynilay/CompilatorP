module Lib.Token where

-- TokenName            Pattern                         Nome
-- -------------------------------------------------------
-- Numeri           Costante numerica                   256
-- Identificatore   Lettera seguita da lettere e cifre  257
-- Print            print                               258
-- While            while                               259
-- Do               do                                  260
-- Conditional      conditional                         261
-- Case             case                                262
-- Break            break                               263
-- Default          default                             264
-- Input utente     user                                265
-- Assegnamento     :=                                  266
-- Congiunzione     &&                                  267
-- Disgiunzione     ||                                  268
-- Minore/uguale    <=                                  269
-- Maggiore/uguale  >=                                  270
-- Uguale a         ==                                  271
-- Diverso da       !=                                  272
-- Minore di        <                                   60
-- Maggiore di      >                                   62
-- Par. tonda (     (                                   40
-- Par. tonda )     )                                   41
-- Par. quadra [    [                                   91
-- Par. quadra ]    ]                                   93
-- Par. graffa {    {                                   123
-- Par. graffa }    }                                   125
-- Somma            +                                   43
-- Sottrazione      -                                   45
-- Moltiplicaz.     *                                   42
-- Divisione        /                                   47
-- Punto e virgola  ;                                   59
-- Virgola          ,                                   44
-- EOF              Fine input                          -1

data Token = Number Integer | Identifier String | Print        | While
           | Do             | Conditional       | Case         | Break
           | Default        | UserInput         | Assignment   | Conjunction
           | Disjunction    | LessEqual         | GreaterEqual | Equal
           | NotEqual       | LessThan          | GreaterThan  | ParenOpen
           | ParenClose     | BracketOpen       | BracketClose | CurlyOpen
           | CurlyClose     | Plus              | Minus        | Multiply
           | Divide         | Semicolon         | Comma        | EOF
             deriving (Show, Eq)

getTokenName :: Token -> Int
getTokenName (Number _)     = 256
getTokenName (Identifier _) = 257
getTokenName Print          = 258
getTokenName While          = 259
getTokenName Do             = 260
getTokenName Conditional    = 261
getTokenName Case           = 262
getTokenName Break          = 263
getTokenName Default        = 264
getTokenName UserInput      = 265
getTokenName Assignment     = 266
getTokenName Conjunction    = 267
getTokenName Disjunction    = 268
getTokenName LessEqual      = 269
getTokenName GreaterEqual   = 270
getTokenName Equal          = 271
getTokenName NotEqual       = 272
getTokenName LessThan       = 60
getTokenName GreaterThan    = 62
getTokenName ParenOpen      = 40
getTokenName ParenClose     = 41
getTokenName BracketOpen    = 91
getTokenName BracketClose   = 93
getTokenName CurlyOpen      = 123
getTokenName CurlyClose     = 125
getTokenName Plus           = 43
getTokenName Minus          = 45
getTokenName Multiply       = 42
getTokenName Divide         = 47
getTokenName Semicolon      = 59
getTokenName Comma          = 44
getTokenName EOF            = -1

tokenValue :: Token -> Maybe (Either String Integer)
tokenValue (Identifier s) = Just $ Left s
tokenValue (Number     n) = Just $ Right n
tokenValue _              = Nothing
