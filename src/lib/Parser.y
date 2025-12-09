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
  empty                   {Emptytoken}
  lambda                  {Lambdatoken}
  debris                  {Debristoken}
  asteroid                {Asteroidtoken}
  boundary                {Boundarytoken}
  '_'                     {Underscore}
  ident                   {Ident $$}  

%%

Program     : Program '.' Rule      { let (Program rs) = $1 in Program ($3 : rs) }
            | Rule                  { Program [$1] }

Rule        : ident arrow Commands  { Rule $1 $3 }

Commands    : Command               { Commands [$1] }
            | Commands ',' Command  { let (Commands cs) = $1 in Commands ($2 : cs)}

Command     : go                    { GoComm }
            | take                  { TakeComm }
            | mark                  { MarkComm }
            | nothing               { NothingComm }
            | turn Dir              { TurnComm $2 }
            | case Dir of Alts end  { CaseComm $2 $4}

Dir         : left                  { LeftDir }
            | right                 { RightDir }
            | front                 { FrontDir }

Alts        : Alt                   { Alts [$1]}
            | Alts ';' Alt          { let (Alts as) = $1 in Alts ($3 : $1) }

Alt         : Pat arrow Commands    { Alt $1 $3 }

Pat         : empty                 { EmptyPat }
            | lambda                { LambdaPat }
            | debris                { DebrisPat }
            | asteroid              { AsteroidPat }
            | boundary              { BoundaryPat }
            | '_'                   { UnderscorePat }

{

happyError _ = error "parse error"

}