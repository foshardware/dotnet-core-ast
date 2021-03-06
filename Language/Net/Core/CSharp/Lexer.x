{
module Language.Net.Core.CSharp.Lexer
    ( Lexer (..)
    , Token (..)
    , lexer
    ) where

import Data.Char (ord)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Language.Net.Core.CSharp.Tokens
}

$any     = [.\n\r]
@newline = [\n\r] | \r\n
@comment = "/*" $any* "*/"
         | "//" .* @newline

@nul_eof = \0 $any*

@preprocessor = \# .* @newline

-- C# actually defines a letter to be any character (or escape sequence)
-- from the Unicode classes Lu, Ll, Lt, Lm, Lo or Nl. Identifiers must
-- start with a letter or an underscore, but can then also contain
-- characters from the classes Mn, Mc, Nd, Pc or Cf.
$ident_start = [a-zA-Z_\@]
$ident_part  = [a-zA-Z_0-9]
$const_part  = [A-Z_]

$digit     = [0-9]
$hex_digit = [0-9a-fA-F]
$sign      = [\+\-]

@int_suffix  = [uU][lL]? | [lL][uU]?
@real_suffix = [fFdDmM]
@exponent    = [eE] $sign? $digit+

@simple_escape  = \\ [0abfnrtv\'\"\\]
@hex_escape     = \\x $hex_digit{1,4}
@unicode_escape = \\u $hex_digit{4} | \\U $hex_digit{8}
@escapes        = @simple_escape | @hex_escape | @unicode_escape

@character          = [^\'\\] | @escapes
@string_character   = [^\"\\] | @escapes
@verbatim_character = $any # \" | \"\"

tokens :-

$white+       ;
@comment      ;
@nul_eof      ;
@preprocessor ;

-- Keywords
abstract   { constTok Tok_Abstract   }
as         { constTok Tok_As         }
async      { constTok Tok_Async      }
await      { constTok Tok_Await      }
base       { constTok Tok_Base       }
bool       { constTok Tok_Bool       }
break      { constTok Tok_Break      }
byte       { constTok Tok_Byte       }
case       { constTok Tok_Case       }
catch      { constTok Tok_Catch      }
char       { constTok Tok_Char       }
checked    { constTok Tok_Checked    }
class      { constTok Tok_Class      }
const      { constTok Tok_Const      }
continue   { constTok Tok_Continue   }
decimal    { constTok Tok_Decimal    }
default    { constTok Tok_Default    }
delegate   { constTok Tok_Delegate   }
do         { constTok Tok_Do         }
double     { constTok Tok_Double     }
dynamic    { constTok Tok_Dynamic    }
else       { constTok Tok_Else       }
enum       { constTok Tok_Enum       }
event      { constTok Tok_Event      }
explicit   { constTok Tok_Explicit   }
extern     { constTok Tok_Extern     }
false      { constTok Tok_False      }
finally    { constTok Tok_Finally    }
fixed      { constTok Tok_Fixed      }
float      { constTok Tok_Float      }
for        { constTok Tok_For        }
foreach    { constTok Tok_Foreach    }
get        { constTok Tok_Get        }
goto       { constTok Tok_Goto       }
if         { constTok Tok_If         }
implicit   { constTok Tok_Implicit   }
in         { constTok Tok_In         }
int        { constTok Tok_Int        }
interface  { constTok Tok_Interface  }
internal   { constTok Tok_Internal   }
is         { constTok Tok_Is         }
lock       { constTok Tok_Lock       }
long       { constTok Tok_Long       }
namespace  { constTok Tok_Namespace  }
new        { constTok Tok_New        }
null       { constTok Tok_Null       }
object     { constTok Tok_Object     }
operator   { constTok Tok_Operator   }
out        { constTok Tok_Out        }
override   { constTok Tok_Override   }
params     { constTok Tok_Params     }
partial    { constTok Tok_Partial    }
private    { constTok Tok_Private    }
protected  { constTok Tok_Protected  }
public     { constTok Tok_Public     }
readonly   { constTok Tok_Readonly   }
ref        { constTok Tok_Ref        }
return     { constTok Tok_Return     }
sbyte      { constTok Tok_Sbyte      }
sealed     { constTok Tok_Sealed     }
set        { constTok Tok_Set        }
short      { constTok Tok_Short      }
sizeof     { constTok Tok_Sizeof     }
stackalloc { constTok Tok_Stackalloc }
static     { constTok Tok_Static     }
string     { constTok Tok_String     }
struct     { constTok Tok_Struct     }
switch     { constTok Tok_Switch     }
this       { constTok Tok_This       }
throw      { constTok Tok_Throw      }
true       { constTok Tok_True       }
try        { constTok Tok_Try        }
typeof     { constTok Tok_Typeof     }
uint       { constTok Tok_UInt       }
ulong      { constTok Tok_ULong      }
unchecked  { constTok Tok_Unchecked  }
unsafe     { constTok Tok_Unsafe     }
ushort     { constTok Tok_UShort     }
using      { constTok Tok_Using      }
var        { constTok Tok_Var        }
virtual    { constTok Tok_Virtual    }
void       { constTok Tok_Void       }
volatile   { constTok Tok_Volatile   }
where      { constTok Tok_Where      }
while      { constTok Tok_While      }

-- Punctuators
\( { constTok Tok_LParen   }
\) { constTok Tok_RParen   }
\[ { constTok Tok_LBracket }
\] { constTok Tok_RBracket }
\{ { constTok Tok_LBrace   }
\} { constTok Tok_RBrace   }
\: { constTok Tok_Colon    }
\; { constTok Tok_Semi     }
\, { constTok Tok_Comma    }
\. { constTok Tok_Dot      }

-- Operators
\+     { constTok Tok_Plus       }
\-     { constTok Tok_Minus      }
\*     { constTok Tok_Star       }
\/     { constTok Tok_Slash      }
\%     { constTok Tok_Percent    }
\&     { constTok Tok_Amp        }
\|     { constTok Tok_Pipe       }
\^     { constTok Tok_Caret      }
\!     { constTok Tok_Not        }
\~     { constTok Tok_Tilde      }
\=     { constTok Tok_Assign     }
\?     { constTok Tok_Question   }
\?\.   { constTok Tok_MaybeDot   }
\?\[   { constTok Tok_MaybeBrack }
\?\?   { constTok Tok_Coalesce   }
\:\:   { constTok Tok_NameQual   }
\+\+   { constTok Tok_Increment  }
\-\-   { constTok Tok_Decrement  }
\&\&   { constTok Tok_And        }
\|\|   { constTok Tok_Or         }
\-\>   { constTok Tok_Arrow      }
\=\=   { constTok Tok_Eq         }
\!\=   { constTok Tok_NotEq      }
\<     { constTok Tok_Lt         }
\<\=   { constTok Tok_LtEq       }
\>     { constTok Tok_Gt         }
\>\=   { constTok Tok_GtEq       }
\+\=   { constTok Tok_AssPlus    }
\-\=   { constTok Tok_AssMinus   }
\*\=   { constTok Tok_AssStar    }
\/\=   { constTok Tok_AssSlash   }
\%\=   { constTok Tok_AssPercent }
\&\=   { constTok Tok_AssAmp     }
\|\=   { constTok Tok_AssPipe    }
\^\=   { constTok Tok_AssCaret   }
\<\<\= { constTok Tok_AssShiftL  }
\>\>\= { constTok Tok_AssShiftR  }
\=\>   { constTok Tok_Lambda     }

-- Integer literals
\-    $digit+     @int_suffix? { textTok Tok_IntLit }
      $digit+     @int_suffix? { textTok Tok_IntLit }
0[xX] $hex_digit+ @int_suffix? { textTok Tok_IntLit }

-- Real literals
$digit+ \. $digit+ @exponent? @real_suffix? { textTok Tok_RealLit }
        \. $digit+ @exponent? @real_suffix? { textTok Tok_RealLit }
           $digit+ @exponent  @real_suffix? { textTok Tok_RealLit }
           $digit+            @real_suffix  { textTok Tok_RealLit }

-- Character / String literals
\' @character \'             { textTok (Tok_CharLit . T.drop 1 . T.init)     }
\" @string_character* \"     { textTok (Tok_StringLit . T.drop 1 . T.init)   }
\@\" @verbatim_character* \" { textTok (Tok_VerbatimLit . T.drop 2 . T.init) }

-- Identifiers
$ident_start $ident_part*      { textTok Tok_Ident }
$const_part*                   { textTok Tok_Constant }
$ident_start $ident_part* \<   { textTok (Tok_HigherOrder . T.init)            }
$ident_start $ident_part* \?   { textTok (Tok_MaybeNull . T.init)              }
$ident_start $ident_part* \[\] { textTok (Tok_ArrayType . T.init . T.init)     }
$ident_start $ident_part* \+\+ { textTok (Tok_PostIncrement . T.init . T.init) }
$ident_start $ident_part* \-\- { textTok (Tok_PostDecrement . T.init . T.init) }
\+\+ $ident_start $ident_part* { textTok (Tok_PreIncrement . T.init . T.init)  }
\-\- $ident_start $ident_part* { textTok (Tok_PreDecrement . T.init . T.init)  }


{
wrap :: (str -> tok) -> AlexPosn -> str -> Lexer tok
wrap f (AlexPn _ line col) s = L (line, col) (f s)

constTok = wrap . const
bstrTok f = wrap (f . T.encodeUtf8)
textTok = wrap

lexer :: String -> T.Text -> [Lexer Token]
lexer file text = go (alexStartPos, '\n', text `T.snoc` '\n')
  where
    go inp@(pos, _, cs) = case {-# SCC "alexScan" #-} alexScan inp 0 of
        AlexEOF                -> []
        AlexError inp'         -> error (errMsg inp')
        AlexSkip  inp'   _     -> go inp'
        AlexToken inp' len act -> act pos (T.take len cs) : go inp'

    errMsg (AlexPn _ line col, _, cs) =
        file ++ ": lexical error (line " ++ show line ++ ", col " ++ show col ++ ")\n"
             ++ "    near " ++ show (T.unpack $ T.take 40 cs)

-----------------------------------------------------------

type AlexInput = (AlexPosn,     -- current position,
                  Char,         -- previous char
                  T.Text)       -- current input string

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar (_,c,_) = c

alexGetChar :: AlexInput -> Maybe (Char,AlexInput)
alexGetChar (p,_,cs) | T.null cs  = Nothing
                     | {-# SCC "alexSkip" #-} alexSkip c = alexGetChar (p', c, cs')
                     | otherwise  = p' `seq` cs' `seq` Just (c, (p', c, cs'))
  where
    c   = T.head cs
    cs' = T.tail cs
    p'  = alexMove p c

alexGetByte :: AlexInput -> Maybe (Int,AlexInput)
alexGetByte i = case alexGetChar i of
  Nothing -> Nothing
  Just (c, j) -> Just (ord c, j)

alexSkip :: Char -> Bool
alexSkip '\xFEFF' = True
alexSkip _        = False

-----------------------------------------------------------

data AlexPosn = AlexPn !Int !Int !Int
        deriving (Eq,Show)

alexStartPos :: AlexPosn
alexStartPos = AlexPn 0 1 1

alexMove :: AlexPosn -> Char -> AlexPosn
alexMove (AlexPn a l c) '\t' = AlexPn (a+1)  l     (((c+7) `div` 8)*8+1)
alexMove (AlexPn a l _) '\n' = AlexPn (a+1) (l+1)   1
alexMove (AlexPn a l c) _    = AlexPn (a+1)  l     (c+1)
}
