module OpilioCraft.ObjectPath.Parser

open FParsec

// narrow types
type Parser<'t> = Parser<'t, unit>

// lexical stuff
let pOpenBracket : Parser<_> = pchar '['
let pCloseBracket : Parser<_> = pchar ']'
let pDot : Parser<_> = pchar '.'

// elements
let pSlot : Parser<_> = 
    let isAsciiIdStart    = fun c -> isAsciiLetter c || c = '_'
    let isAsciiIdContinue = fun c -> isAsciiLetter c || isDigit c || c = '_'
    identifier (IdentifierOptions(isAsciiIdStart = isAsciiIdStart, isAsciiIdContinue = isAsciiIdContinue)) |>> ObjectPathElement.Property

let pDictionaryKey : Parser<_> =
    between pOpenBracket pCloseBracket (many1Satisfy (fun c -> c <> ']')) |>> ObjectPathElement.DictionaryKey 

// path expression
let pObjectPath : Parser<ObjectPath> =
    pstring "$." >>. ( sepBy (pSlot <|> pDictionaryKey) pDot )
