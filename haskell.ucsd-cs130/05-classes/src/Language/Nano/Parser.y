{
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Language.Nano.Parser (
    parseExpr
  , parseDefs
  , parseTokens
  ) where

import Language.Nano.Lexer
import Language.Nano.Types hiding (Nano (..))
import Control.Monad.Except
import Control.Exception

}

-- Entry point
%name top

-- Lexer structure
%tokentype { Token }

-- Parser monad
%monad { Except String } { (>>=) } { return }
%error { parseError }

-- Token Names
%token
    let   { LET _    }
    true  { TRUE _   }
    false { FALSE _  }
    in    { IN _     }
    if    { IF _     }
    then  { THEN _   }
    else  { ELSE _   }
    TNUM  { NUM _ $$ }
    ID    { ID _ $$  }
    '\\'  { LAM _    }
    '->'  { ARROW _  }
    '='   { EQB _    }
    '+'   { PLUS _   }
    '-'   { MINUS _  }
    '*'   { MUL _    }
    '&&'  { AND _    }
    '||'  { OR  _    }
    '=='  { EQL _    }
    '/='  { NEQ _    }
    '<'   { LESS _   }
    '<='  { LEQ _    }
    ':'   { COLON _  }
    '('   { LPAREN _ }
    ')'   { RPAREN _ }
    '['   { LBRAC _  }
    ']'   { RBRAC _  }
    ','   { COMMA _  }


-- Operators
%right in
%nonassoc '=' '==' '/=' '<' '<=' if then else
%right ':' '->'
%left '||' '&&'
%left '+' '-'
%left '*'
%nonassoc ID
%nonassoc TNUM
%nonassoc true
%nonassoc false
%right '('
%right ')'
%left APP
%%

Top  : Def                        { [$1] } 
     | Def ',' Top                { $1 : $3 }
     | Expr                       { [("", $1) ]}

Def  : ID '=' Expr                 { ($1, $3) }


Expr : TNUM                          { EInt $1 }
      | '\\' ID '->' Expr            { mkLam [$2] $4 }
      | if  Expr then Expr else Expr { EIf  $2 $4 $6}
      | let ID ids '=' Expr in Expr  { ELet $2 (mkLam $3 $5) $7}
      | '[' xs ']'    	             { $2 }

      | Expr Expr  %prec APP        { EApp $1 $2 }     
      | ID                          { EVar $1 }
      | true                        { EBool True }
      | false                       { EBool False }

      | Expr  '<=' Expr           { EBin Le $1 $3 }
      | Expr  '<' Expr            { EBin Lt  $1 $3 }
      | Expr  '==' Expr           { EBin Eq  $1 $3 }
      | Expr  '/=' Expr           { EBin Ne  $1 $3 }
      | Expr  '&&' Expr           { EBin And  $1 $3 }
      | Expr  '||' Expr           { EBin Or  $1 $3 } 

      | Expr  '*' Expr            { EBin Mul $1 $3 } 		
      | Expr  '+' Expr            { EBin Plus $1 $3 }
      | Expr  '-' Expr            { EBin Minus  $1 $3 }
      | Expr  ':' Expr            { EBin Cons $1 $3 }
      | '(' Expr ')'    	             { $2 }

xs :    {- empty -}             { ENil }
      | Expr                    { EBin Cons $1 ENil }
      | Expr ',' xs             { EBin Cons  $1 $3 }

Exprs : {- empty -}             { [] }
      | Expr Exprs             { $1 : $2 }

ids :   {- empty -}             { [] }
      | ID ids    { $1 : $2 }
{
mkLam :: [Id] -> Expr -> Expr
mkLam []     e = e
mkLam (x:xs) e = ELam x (mkLam xs e)

parseError :: [Token] -> Except String a
parseError (l:ls) = throwError (show l)
parseError []     = throwError "Unexpected end of Input"

parseExpr :: String -> Expr
parseExpr s = case parseDefs' s of
                Left msg         -> throw (Error ("parse error:" ++ msg))
                Right ((_,e):_)  -> e

parseDefs :: String -> [(Id, Expr)]
parseDefs s = case parseDefs' s of 
                Left msg -> throw (Error ("parse error:" ++ msg))
                Right e  -> e
                
parseDefs' input = runExcept $ do
   tokenStream <- scanTokens input
   top tokenStream

parseTokens :: String -> Either String [Token]
parseTokens = runExcept . scanTokens


}
