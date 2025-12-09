{
module Parser where

import Model
}

%name parser
%tokentype { Token }

%token
  x { Emptycommand }

%%

Program : { Program }

{

happyError _ = error "parse error"

}