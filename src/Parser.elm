module Parser exposing (parse)

import Tokenizer exposing (tokenize)
import Lexer exposing (commands, Command)

parse : String -> List Command
parse = tokenize >> commands
