# purescript-typelevel-parser

Turn DSLs into ASTs.

## Motivation

There are lots of DSLs in the world, like GraphQL and OpenAPI. These DSLs are usually ported to a language as an AST, ie GraphQL to [`purescript-graphql-parser`](https://github.com/meeshkan/purescript-graphql-parser).

The problem with this approach is that, if the DSL is supposed to describe the business logic of an application, then you need to do extra work to make sure that the implementation is conformant to the DSL. Think again of GraphQL - to make sure that resolvers are conformant to a spec, you either need code generation or runtime assertions. Both are clunky.

This library fixes that. By using a typelevel-parser, you can create a typelevel-AST from a DSL. Then, you can transform the AST to the correct type, ie resolvers for a GraphQL server. This guarantees that the code won't compile unless your implementation is conformant to the spec.

## Example

Let's take a simple query string, ie `foo=1&bar=2&baz=3`. Let's call it **NumberQL**. The AST to describe this is that the keys must be composed of lowercase letters and the values must be an integer. Each business has their own NumberQL spec - for example, one supports keys `gold&silver&bronze` and another supports keys `earth&wind&fire`. In code, we want to represent the spec as `{ key1 :: Int, key2 :: Int }`.

At Meeshkan, let's say our spec is `python&javascript&java`. We encode it like this.

```purescript
module Test.NumberQL where

import Prelude
import Prim.Row (class Cons)
import Type.Data.Row (RProxy(..))
import Type.Parser (class Parse,
  type (!:!), ConsPositiveParserResult, ListParser, ListParserResult,
  Lowercase, NilPositiveParserResult, SingletonMatcher', SingletonParserResult,
  SomeMatcher, Success, kind ParserResult)

-- our spec
type OurSpec
  = "python&java&javascript"

data Key

data Keys

-- here's our parser
type KeyList
  = ListParser ((SomeMatcher Lowercase) !:! Key) (SingletonMatcher' "&") Keys

-- Now, we create a class that turns our AST into a row.
-- This general pattern can be used to...
--   turn a GraphQL AST into a GraphQL resolver type
--   turn an OpenAPI spec into a REST server type
--   etc.
class NumberQLToRow (p :: ParserResult) (t :: # Type) | p -> t

instance nqlToRowNil :: NumberQLToRow (Success
   (ListParserResult NilPositiveParserResult Keys)) res

-- this is where we construct the row
instance nqlToRowCons ::
  ( NumberQLToRow (Success (ListParserResult y Keys)) out
  , Cons key Int out res
  ) =>
  NumberQLToRow
    ( Success
        ( ListParserResult
            ( ConsPositiveParserResult
                (SingletonParserResult key Key)
                y
            )
            Keys
        )
    )
    res

-- we construct the type
class SymbolToNumberQLType (s :: Symbol) (r :: # Type) | s -> r

instance symbolToNumberQLType :: (
   Parse KeyList s out
 , NumberQLToRow out r
) => SymbolToNumberQLType s r

-- this will validate that our spec is conformant
validator ::
  forall (c :: # Type).
  SymbolToNumberQLType OurSpec c =>
  Record c ->
  Record c
validator a = a

-- the validator validates that our type is conformant to the DSL!
languages :: { python :: Int, javascript :: Int, java :: Int }
languages =
  validator
    { python: 1
    , javascript: 2
    , java: 3
    }
```
