{
type token =
  | HEADER of string
  | COMMENT of char * string
  | SEQUENCE of string
}

rule token = parse

| '>' ([^ '\n' ]* as h) '\n' { HEADER h }

| ([';' '#'] as c) ([^ '\n' ]* as t) '\n' { COMMENT (c, t) }

| ([^ '\n' ]* as s) '\n' { SEQUENCE s }
