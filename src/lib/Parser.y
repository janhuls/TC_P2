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

Program   : Rules                 { Program $1}

Rules     :                       { [] }
          | Rule '.' Rules        { $1 : $3 }
          | Rule '.'              { [$1] }

Rule      : ident arrow Commands  { Rule $1 $3 }


CommandList : Command             { [$1] }
            | Command ',' CommandList { $1 : $3 }

Commands  :                       { Commands [] }
          | CommandList           { Commands $1 }

Command   : go                    { GoComm }
          | take                  { TakeComm }
          | mark                  { MarkComm }
          | nothing               { NothingComm }
          | turn Dir              { TurnComm $2 }
          | case Dir of Alts end  { CaseComm $2 $4}
          | ident                 { CallComm $1 }

Dir       : left                  { LeftDir }
          | right                 { RightDir }
          | front                 { FrontDir }

AltList   : Alt                   { [$1] }
          | Alt ';' AltList       { $1 : $3 }

Alts      :                       { Alts [] }
          | AltList               { Alts $1 }

Alt       : Pat arrow Commands    { Alt $1 $3 }

Pat       : empty                 { EmptyPat }
          | lambda                { LambdaPat }
          | debris                { DebrisPat }
          | asteroid              { AsteroidPat }
          | boundary              { BoundaryPat }
          | '_'                   { UnderscorePat }

{

happyError _ = error "parse error"

}