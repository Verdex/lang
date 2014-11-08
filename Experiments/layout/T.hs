
import Control.Applicative
import Scoper
import Tokenizer
import Parsing
import LangAst

i = "\nx = x\n\n" ++
    "y = y\ny\n"  ++
    "z = z\n"

t = tokenize i
s = scope t

