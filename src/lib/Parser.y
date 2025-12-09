{
module Parser where

import Model
}

%name parser
%tokentype { Token }

%token
  arrow                   {Arrow}
  '.'                     {Dot}
  ','                     {Comma}
  go                      {Go}
  take                    {Take}
  mark                    {Mark}
  nothing                 {Emptycommand}
  turn                    {Turn}
  case                    {Case}
  of                      {Of}
  end                     {End}
  left                    {Lefttok}
  right                   {Righttok}
  front                   {Front}
  ';'                     {Semicolon}
  Empty                   {Emptytoken}
  Lambda                  {Lambdatoken}
  Debris                  {Debristoken}
  Asteroid                {Asteroidtoken}
  Boundary                {Boundarytoken}
  '_'                     {Underscore}
  ident                   {Ident $$}  

%%

Program : ident arrow Command { Program $2 $3}


{

happyError _ = error "parse error"

}