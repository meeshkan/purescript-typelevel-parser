module Test.Main where

import Prelude
import Effect (Effect)
import Effect.Class.Console (log)
import Prim.Row (class Cons)
import Type.Data.Peano (D3, Z)
import Type.Parser (class Match, class Parse, type (!:!), type (:$), type (:/), type (<<>>), AndMatcher, AtLeastMatcher, AtMostMatcher, ConcatMatcher, ConsParser, ConsPositiveParserResult, ConsSymbol, FailMatch, ListParser, ListParserResult, Lowercase, ManyMatcher, ManyMatcher', MatcherResultProxy(..), NMatcher, NMatcher', NilParser, NilPositiveParserResult, NilSymbol, NotMatcher, Ns, OrMatcher, ParserResultProxy(..), SepMatcher, SingletonMatcher, SingletonMatcher', SingletonParser, SingletonParserResult, SomeMatcher, SomeMatcher', Success, SuccessMatch, TupleParser, UnionParser, UnionParserResult, kind ParserResult)

testSingletonMatcherT0 :: MatcherResultProxy (SuccessMatch "bar")
testSingletonMatcherT0 =
  MatcherResultProxy ::
    forall c.
    Match
      ( SingletonMatcher (ConsSymbol "bar" NilSymbol)
      )
      "bar"
      c =>
    MatcherResultProxy c

testSingletonMatcherT00 :: MatcherResultProxy (SuccessMatch "bar")
testSingletonMatcherT00 =
  MatcherResultProxy ::
    forall c.
    Match
      ( SingletonMatcher' "bar"
      )
      "bar"
      c =>
    MatcherResultProxy c

testSingletonMatcherF0 :: MatcherResultProxy (FailMatch "baz" "")
testSingletonMatcherF0 =
  MatcherResultProxy ::
    forall c.
    Match
      ( SingletonMatcher (ConsSymbol "bar" NilSymbol)
      )
      "baz"
      c =>
    MatcherResultProxy c

testSingletonMatcherT1 :: MatcherResultProxy (SuccessMatch "baz")
testSingletonMatcherT1 =
  MatcherResultProxy ::
    forall c.
    Match
      ( SingletonMatcher (ConsSymbol "bar" (ConsSymbol "baz" NilSymbol))
      )
      "baz"
      c =>
    MatcherResultProxy c

testSingletonMatcherF1 :: MatcherResultProxy (FailMatch "bar" "")
testSingletonMatcherF1 =
  MatcherResultProxy ::
    forall c.
    Match
      ( SingletonMatcher NilSymbol
      )
      "bar"
      c =>
    MatcherResultProxy c

testNotMatcherT2 :: MatcherResultProxy (SuccessMatch "bar")
testNotMatcherT2 =
  MatcherResultProxy ::
    forall c.
    Match
      ( NotMatcher (ConsSymbol "baz" NilSymbol)
      )
      "bar"
      c =>
    MatcherResultProxy c

testNotMatcherF2 :: MatcherResultProxy (FailMatch "bar" "")
testNotMatcherF2 =
  MatcherResultProxy ::
    forall c.
    Match
      ( NotMatcher (ConsSymbol "bar" NilSymbol)
      )
      "bar"
      c =>
    MatcherResultProxy c

testAtMostMatcherT0 :: MatcherResultProxy (SuccessMatch "b")
testAtMostMatcherT0 =
  MatcherResultProxy ::
    forall c.
    Match
      ( AtMostMatcher D3 (ConsSymbol "b" NilSymbol)
      )
      "b"
      c =>
    MatcherResultProxy c

testAtMostMatcherT1 :: MatcherResultProxy (SuccessMatch "bb")
testAtMostMatcherT1 =
  MatcherResultProxy ::
    forall c.
    Match
      ( AtMostMatcher D3 (ConsSymbol "b" NilSymbol)
      )
      "bb"
      c =>
    MatcherResultProxy c

testAtMostMatcherT3 :: MatcherResultProxy (SuccessMatch "bbb")
testAtMostMatcherT3 =
  MatcherResultProxy ::
    forall c.
    Match
      ( AtMostMatcher D3 (ConsSymbol "b" NilSymbol)
      )
      "bbb"
      c =>
    MatcherResultProxy c

testAtMostMatcherF1 :: MatcherResultProxy (FailMatch "" "bbbb")
testAtMostMatcherF1 =
  MatcherResultProxy ::
    forall c.
    Match
      ( AtMostMatcher D3 (ConsSymbol "b" NilSymbol)
      )
      "bbbb"
      c =>
    MatcherResultProxy c

testNMatcherT0 :: MatcherResultProxy (FailMatch "bbb" "33")
testNMatcherT0 =
  MatcherResultProxy ::
    forall c.
    Match
      ( NMatcher D3 (ConsSymbol "b" NilSymbol)
      )
      "bbb33"
      c =>
    MatcherResultProxy c

testNMatcherT00 :: MatcherResultProxy (SuccessMatch "bbb")
testNMatcherT00 =
  MatcherResultProxy ::
    forall c.
    Match
      ( NMatcher D3 (ConsSymbol "b" NilSymbol)
      )
      "bbb"
      c =>
    MatcherResultProxy c

testNMatcherF0 :: MatcherResultProxy (FailMatch "bb3" "3")
testNMatcherF0 =
  MatcherResultProxy ::
    forall c.
    Match
      ( NMatcher D3 (ConsSymbol "b" NilSymbol)
      )
      "bb33"
      c =>
    MatcherResultProxy c

testNMatcherT1 :: MatcherResultProxy (FailMatch "" "qrs33")
testNMatcherT1 =
  MatcherResultProxy ::
    forall c.
    Match
      ( NMatcher Z (ConsSymbol "b" NilSymbol)
      )
      "qrs33"
      c =>
    MatcherResultProxy c

testNMatcherT2 :: MatcherResultProxy (FailMatch "qrs" "33")
testNMatcherT2 =
  MatcherResultProxy ::
    forall c.
    Match
      (NMatcher D3 ("s" :/ "r" :/ "q" :/ Ns))
      "qrs33"
      c =>
    MatcherResultProxy c

testNMatcherT3 :: MatcherResultProxy (SuccessMatch "rrs")
testNMatcherT3 =
  MatcherResultProxy ::
    forall c.
    Match
      ( NMatcher D3 (ConsSymbol "s" (ConsSymbol "r" (ConsSymbol "q" NilSymbol)))
      )
      "rrs"
      c =>
    MatcherResultProxy c

testNMatcherF3 :: MatcherResultProxy (FailMatch "rrb" "srq33")
testNMatcherF3 =
  MatcherResultProxy ::
    forall c.
    Match
      ( NMatcher D3 (ConsSymbol "s" (ConsSymbol "r" (ConsSymbol "q" NilSymbol)))
      )
      "rrbsrq33"
      c =>
    MatcherResultProxy c

testNMatcherT4 :: MatcherResultProxy (FailMatch "rrr" "q33")
testNMatcherT4 =
  MatcherResultProxy ::
    forall c.
    Match
      (NMatcher' D3 "r")
      "rrrq33"
      c =>
    MatcherResultProxy c

testManyMatcherT0 :: MatcherResultProxy (FailMatch "bbbbbb" "33")
testManyMatcherT0 =
  MatcherResultProxy ::
    forall c.
    Match
      ( ManyMatcher (ConsSymbol "b" NilSymbol)
      )
      "bbbbbb33"
      c =>
    MatcherResultProxy c

testManyMatcherT1 :: MatcherResultProxy (SuccessMatch "bbbbbb")
testManyMatcherT1 =
  MatcherResultProxy ::
    forall c.
    Match
      ( ManyMatcher (ConsSymbol "b" NilSymbol)
      )
      "bbbbbb"
      c =>
    MatcherResultProxy c

testManyMatcherT2 :: MatcherResultProxy (SuccessMatch "bbbababb")
testManyMatcherT2 =
  MatcherResultProxy ::
    forall c.
    Match
      ( ManyMatcher (ConsSymbol "b" (ConsSymbol "a" NilSymbol))
      )
      "bbbababb"
      c =>
    MatcherResultProxy c

testManyMatcherT3 :: MatcherResultProxy (FailMatch "" "qbbbababb")
testManyMatcherT3 =
  MatcherResultProxy ::
    forall c.
    Match
      ( ManyMatcher (ConsSymbol "b" (ConsSymbol "a" NilSymbol))
      )
      "qbbbababb"
      c =>
    MatcherResultProxy c

testManyMatcherT4 :: MatcherResultProxy (SuccessMatch "")
testManyMatcherT4 =
  MatcherResultProxy ::
    forall c.
    Match
      ( ManyMatcher (ConsSymbol "b" (ConsSymbol "a" NilSymbol))
      )
      ""
      c =>
    MatcherResultProxy c

testSomeMatcherT0 :: MatcherResultProxy (SuccessMatch "bbbbbb")
testSomeMatcherT0 =
  MatcherResultProxy ::
    forall c.
    Match
      ( SomeMatcher (ConsSymbol "b" NilSymbol)
      )
      "bbbbbb"
      c =>
    MatcherResultProxy c

testSomeMatcherT1 :: MatcherResultProxy (SuccessMatch "b")
testSomeMatcherT1 =
  MatcherResultProxy ::
    forall c.
    Match
      ( SomeMatcher (ConsSymbol "b" NilSymbol)
      )
      "b"
      c =>
    MatcherResultProxy c

testSomeMatcherF0 :: MatcherResultProxy (FailMatch "" "")
testSomeMatcherF0 =
  MatcherResultProxy ::
    forall c.
    Match
      ( SomeMatcher (ConsSymbol "b" NilSymbol)
      )
      ""
      c =>
    MatcherResultProxy c

testConcatMatcherT0 :: MatcherResultProxy (FailMatch "helloworld" "")
testConcatMatcherT0 =
  MatcherResultProxy ::
    forall c.
    Match
      ( ConcatMatcher
          (SingletonMatcher (ConsSymbol "hel" NilSymbol))
          (SingletonMatcher (ConsSymbol "lo" NilSymbol))
      )
      "helloworld"
      c =>
    MatcherResultProxy c

testConcatMatcherT1 :: MatcherResultProxy (SuccessMatch "hello")
testConcatMatcherT1 =
  MatcherResultProxy ::
    forall c.
    Match
      ( (ManyMatcher' "a")
          <<>> ( (SingletonMatcher (ConsSymbol "hel" NilSymbol))
              <<>> (SingletonMatcher (ConsSymbol "lo" NilSymbol))
          )
      )
      "hello"
      c =>
    MatcherResultProxy c

testConcatMatcherT2 :: MatcherResultProxy (SuccessMatch "aaahello")
testConcatMatcherT2 =
  MatcherResultProxy ::
    forall c.
    Match
      ( ConcatMatcher
          (ManyMatcher' "a")
          ( ConcatMatcher
              (SingletonMatcher (ConsSymbol "hel" NilSymbol))
              (SingletonMatcher (ConsSymbol "lo" NilSymbol))
          )
      )
      "aaahello"
      c =>
    MatcherResultProxy c

testConcatMatcherF2 :: MatcherResultProxy (FailMatch "aaabhelloworld" "")
testConcatMatcherF2 =
  MatcherResultProxy ::
    forall c.
    Match
      ( ConcatMatcher
          (ManyMatcher' "a")
          ( ConcatMatcher
              (SingletonMatcher (ConsSymbol "hel" NilSymbol))
              (SingletonMatcher (ConsSymbol "lo" NilSymbol))
          )
      )
      "aaabhelloworld"
      c =>
    MatcherResultProxy c

testConcatMatcherT3 :: MatcherResultProxy (SuccessMatch "aaabhello")
testConcatMatcherT3 =
  MatcherResultProxy ::
    forall c.
    Match
      ( ConcatMatcher
          (ManyMatcher' "a")
          ( ConcatMatcher
              (ManyMatcher' "b")
              ( ConcatMatcher
                  (SingletonMatcher (ConsSymbol "hel" NilSymbol))
                  (SingletonMatcher (ConsSymbol "lo" NilSymbol))
              )
          )
      )
      "aaabhello"
      c =>
    MatcherResultProxy c

testSepMatcherT00 :: MatcherResultProxy (SuccessMatch "aaa")
testSepMatcherT00 =
  MatcherResultProxy ::
    forall c.
    Match
      ( SepMatcher
          (ManyMatcher (ConsSymbol "a" (ConsSymbol "b" NilSymbol)))
          (SingletonMatcher' "-")
      )
      "aaa"
      c =>
    MatcherResultProxy c

testSepMatcherT01 :: MatcherResultProxy (SuccessMatch "aaba")
testSepMatcherT01 =
  MatcherResultProxy ::
    forall c.
    Match
      ( SepMatcher
          (ManyMatcher (ConsSymbol "a" (ConsSymbol "b" NilSymbol)))
          (SingletonMatcher' "-")
      )
      "aaba"
      c =>
    MatcherResultProxy c

testSepMatcherT02 :: MatcherResultProxy (SuccessMatch "a-a")
testSepMatcherT02 =
  MatcherResultProxy ::
    forall c.
    Match
      ( SepMatcher
          (SingletonMatcher' "a")
          (SingletonMatcher' "-")
      )
      "a-a"
      c =>
    MatcherResultProxy c

testSepMatcherT03 :: MatcherResultProxy (SuccessMatch "b-a")
testSepMatcherT03 =
  MatcherResultProxy ::
    forall c.
    Match
      ( SepMatcher
          (SingletonMatcher (ConsSymbol "a" (ConsSymbol "b" NilSymbol)))
          (SingletonMatcher' "-")
      )
      "b-a"
      c =>
    MatcherResultProxy c

testSepMatcherF04 :: MatcherResultProxy (FailMatch "c-a" "")
testSepMatcherF04 =
  MatcherResultProxy ::
    forall c.
    Match
      ( SepMatcher
          (SingletonMatcher (ConsSymbol "a" (ConsSymbol "b" NilSymbol)))
          (SingletonMatcher' "-")
      )
      "c-a"
      c =>
    MatcherResultProxy c

testSepMatcherT05 :: MatcherResultProxy (SuccessMatch "hello,,world")
testSepMatcherT05 =
  MatcherResultProxy ::
    forall c.
    Match
      ( SepMatcher
          (OrMatcher (SingletonMatcher' "hello") (SingletonMatcher' "world"))
          (SingletonMatcher' ",,")
      )
      "hello,,world"
      c =>
    MatcherResultProxy c

testSepMatcherF05 :: MatcherResultProxy (FailMatch "hello,,world" "")
testSepMatcherF05 =
  MatcherResultProxy ::
    forall c.
    Match
      ( SepMatcher
          (OrMatcher (SingletonMatcher' "hello") (SingletonMatcher' "world"))
          (SingletonMatcher' ",")
      )
      "hello,,world"
      c =>
    MatcherResultProxy c

testSepMatcherT06 :: MatcherResultProxy (SuccessMatch "hello,,a")
testSepMatcherT06 =
  MatcherResultProxy ::
    forall c.
    Match
      ( SepMatcher
          (OrMatcher (SingletonMatcher' "hello") (ManyMatcher' "a"))
          (SingletonMatcher' ",")
      )
      "hello,,a"
      c =>
    MatcherResultProxy c

testSepMatcherF06 :: MatcherResultProxy (FailMatch "hello,,a" "")
testSepMatcherF06 =
  MatcherResultProxy ::
    forall c.
    Match
      ( SepMatcher
          (OrMatcher (SingletonMatcher' "hello") (SomeMatcher' "a"))
          (SingletonMatcher' ",")
      )
      "hello,,a"
      c =>
    MatcherResultProxy c

testSepMatcherT07 :: MatcherResultProxy (SuccessMatch "hello,a,a,aaaa,hello")
testSepMatcherT07 =
  MatcherResultProxy ::
    forall c.
    Match
      ( SepMatcher
          (OrMatcher (SingletonMatcher' "hello") (SomeMatcher' "a"))
          (SingletonMatcher' ",")
      )
      "hello,a,a,aaaa,hello"
      c =>
    MatcherResultProxy c

testSepMatcherT08 :: MatcherResultProxy (SuccessMatch "")
testSepMatcherT08 =
  MatcherResultProxy ::
    forall c.
    Match
      ( SepMatcher
          (OrMatcher (SingletonMatcher' "hello") (SomeMatcher' "a"))
          (SingletonMatcher' ",")
      )
      ""
      c =>
    MatcherResultProxy c

-- and
testAndMatcherT0 :: MatcherResultProxy (SuccessMatch "aaaaa")
testAndMatcherT0 =
  MatcherResultProxy ::
    forall c.
    Match
      ( AndMatcher
          ( SomeMatcher (ConsSymbol "a" (ConsSymbol "b" NilSymbol))
          )
          ( SomeMatcher (ConsSymbol "q" (ConsSymbol "a" NilSymbol))
          )
      )
      "aaaaa"
      c =>
    MatcherResultProxy c

testAndMatcherF0 :: MatcherResultProxy (FailMatch "aaaaa" "")
testAndMatcherF0 =
  MatcherResultProxy ::
    forall c.
    Match
      ( AndMatcher
          ( SomeMatcher (ConsSymbol "a" (ConsSymbol "b" NilSymbol))
          )
          ( SomeMatcher (ConsSymbol "q" (ConsSymbol "r" NilSymbol))
          )
      )
      "aaaaa"
      c =>
    MatcherResultProxy c

-- or
testOrMatcherT0 :: MatcherResultProxy (SuccessMatch "aaaaa")
testOrMatcherT0 =
  MatcherResultProxy ::
    forall c.
    Match
      ( OrMatcher
          ( SomeMatcher (ConsSymbol "a" (ConsSymbol "b" NilSymbol))
          )
          ( SomeMatcher (ConsSymbol "q" (ConsSymbol "a" NilSymbol))
          )
      )
      "aaaaa"
      c =>
    MatcherResultProxy c

testOrMatcherF0 :: MatcherResultProxy (SuccessMatch "aaabaa")
testOrMatcherF0 =
  MatcherResultProxy ::
    forall c.
    Match
      ( OrMatcher
          ( SomeMatcher (ConsSymbol "a" (ConsSymbol "b" NilSymbol))
          )
          ( SomeMatcher (ConsSymbol "q" (ConsSymbol "r" NilSymbol))
          )
      )
      "aaabaa"
      c =>
    MatcherResultProxy c

testAndMatcherF1 :: MatcherResultProxy (FailMatch "aaaaa" "")
testAndMatcherF1 =
  MatcherResultProxy ::
    forall c.
    Match
      ( OrMatcher
          ( SomeMatcher (ConsSymbol "x" (ConsSymbol "y" NilSymbol))
          )
          ( SomeMatcher (ConsSymbol "q" (ConsSymbol "r" NilSymbol))
          )
      )
      "aaaaa"
      c =>
    MatcherResultProxy c

-- at least
testAtLeastMatcherT1 :: MatcherResultProxy (SuccessMatch "xyxyxxx")
testAtLeastMatcherT1 =
  MatcherResultProxy ::
    forall c.
    Match
      ( AtLeastMatcher D3 (ConsSymbol "x" (ConsSymbol "y" NilSymbol))
      )
      "xyxyxxx"
      c =>
    MatcherResultProxy c

testAtLeastMatcherF1 :: MatcherResultProxy (FailMatch "xyaaa" "")
testAtLeastMatcherF1 =
  MatcherResultProxy ::
    forall c.
    Match
      ( AtLeastMatcher D3 (ConsSymbol "x" (ConsSymbol "y" NilSymbol))
      )
      "xyaaa"
      c =>
    MatcherResultProxy c

-- singleton parser result
testSingletonParserT0 :: ParserResultProxy (Success (SingletonParserResult "xyyyy" Int))
testSingletonParserT0 =
  ParserResultProxy ::
    forall c.
    Parse
      ( SingletonParser
          ( AtLeastMatcher D3 (ConsSymbol "x" (ConsSymbol "y" NilSymbol))
          )
          Int
      )
      "xyyyy"
      c =>
    ParserResultProxy c

testParserUnionResultT3 ::
  ParserResultProxy
    ( Success
        ( UnionParserResult
            (SingletonParserResult "aaba" Number)
            Int
        )
    )
testParserUnionResultT3 =
  ParserResultProxy ::
    forall c.
    Parse
      ( UnionParser
          ( ConsParser
              ( SingletonParser
                  ( SomeMatcher (ConsSymbol "a" (ConsSymbol "b" NilSymbol))
                  )
                  Number
              )
              ( ConsParser
                  ( SingletonParser
                      ( SomeMatcher (ConsSymbol "c" NilSymbol)
                      )
                      Boolean
                  )
                  NilParser
              )
          )
          Int
      )
      "aaba"
      c =>
    ParserResultProxy c

testParserUnionResultT4 ::
  ParserResultProxy
    ( Success
        ( UnionParserResult
            (SingletonParserResult "cc" Boolean)
            Int
        )
    )
testParserUnionResultT4 =
  ParserResultProxy ::
    forall c.
    Parse
      ( UnionParser
          ( ConsParser
              ( SingletonParser
                  ( SomeMatcher (ConsSymbol "a" (ConsSymbol "b" NilSymbol))
                  )
                  Number
              )
              ( ConsParser
                  ( SingletonParser
                      ( SomeMatcher (ConsSymbol "c" NilSymbol)
                      )
                      Boolean
                  )
                  NilParser
              )
          )
          Int
      )
      "cc"
      c =>
    ParserResultProxy c

testParserListResultT0 :: ParserResultProxy (Success (ListParserResult NilPositiveParserResult Int))
testParserListResultT0 =
  ParserResultProxy ::
    forall c.
    Parse
      ( TupleParser
          NilParser
          (SingletonMatcher' ",")
          Int
      )
      ""
      c =>
    ParserResultProxy c

testParserListResultT1 ::
  ParserResultProxy
    ( Success
        ( ListParserResult
            ( ConsPositiveParserResult
                (SingletonParserResult "a" Number)
                NilPositiveParserResult
            )
            Int
        )
    )
testParserListResultT1 =
  ParserResultProxy ::
    forall c.
    Parse
      ( TupleParser
          ( ( SingletonParser
                ( SomeMatcher (ConsSymbol "a" (ConsSymbol "b" NilSymbol))
                )
                Number
            )
              :$ NilParser
          )
          (SingletonMatcher' ",")
          Int
      )
      "a"
      c =>
    ParserResultProxy c

testParserListResultT2 ::
  ParserResultProxy
    ( Success
        ( ListParserResult
            ( ConsPositiveParserResult
                (SingletonParserResult "aaba" Number)
                NilPositiveParserResult
            )
            Int
        )
    )
testParserListResultT2 =
  ParserResultProxy ::
    forall c.
    Parse
      ( TupleParser
          ( ConsParser
              ( SingletonParser
                  ( SomeMatcher (ConsSymbol "a" (ConsSymbol "b" NilSymbol))
                  )
                  Number
              )
              NilParser
          )
          (SingletonMatcher' ",")
          Int
      )
      "aaba"
      c =>
    ParserResultProxy c

testParserListResultT3 ::
  ParserResultProxy
    ( Success
        ( ListParserResult
            ( ConsPositiveParserResult
                (SingletonParserResult "aaba" Number)
                ( ConsPositiveParserResult
                    (SingletonParserResult "cc" Boolean)
                    NilPositiveParserResult
                )
            )
            Int
        )
    )
testParserListResultT3 =
  ParserResultProxy ::
    forall c.
    Parse
      ( TupleParser
          ( ConsParser
              ( SingletonParser
                  ( SomeMatcher (ConsSymbol "a" (ConsSymbol "b" NilSymbol))
                  )
                  Number
              )
              ( ConsParser
                  ( SingletonParser
                      ( SomeMatcher (ConsSymbol "c" NilSymbol)
                      )
                      Boolean
                  )
                  NilParser
              )
          )
          (SingletonMatcher' ",")
          Int
      )
      "aaba,cc"
      c =>
    ParserResultProxy c

testParserListResultT4 ::
  ParserResultProxy
    ( Success
        ( ListParserResult
            ( ConsPositiveParserResult
                (SingletonParserResult "aaba" Number)
                ( ConsPositiveParserResult
                    (SingletonParserResult "cc" Boolean)
                    NilPositiveParserResult
                )
            )
            Int
        )
    )
testParserListResultT4 =
  ParserResultProxy ::
    forall c.
    Parse
      ( TupleParser
          ( ConsParser
              ( SingletonParser
                  ( SomeMatcher (ConsSymbol "a" (ConsSymbol "b" NilSymbol))
                  )
                  Number
              )
              ( ConsParser
                  ( SingletonParser
                      ( SomeMatcher (ConsSymbol "c" NilSymbol)
                      )
                      Boolean
                  )
                  NilParser
              )
          )
          (SingletonMatcher' "")
          Int
      )
      "aabacc"
      c =>
    ParserResultProxy c

testParserListResultT5 ::
  ParserResultProxy
    ( Success
        ( ListParserResult
            ( ConsPositiveParserResult
                (SingletonParserResult "aaba" Number)
                ( ConsPositiveParserResult
                    (SingletonParserResult "aabb" Number)
                    ( ConsPositiveParserResult
                        (SingletonParserResult "bbab" Number)
                        NilPositiveParserResult
                    )
                )
            )
            Int
        )
    )
testParserListResultT5 =
  ParserResultProxy ::
    forall c.
    Parse
      ( ListParser
          ( SingletonParser
              ( SomeMatcher (ConsSymbol "a" (ConsSymbol "b" NilSymbol))
              )
              Number
          )
          (SingletonMatcher' "--")
          Int
      )
      "aaba--aabb--bbab"
      c =>
    ParserResultProxy c

testParserListResultT6 ::
  ParserResultProxy
    ( Success
        ( ListParserResult
            ( ConsPositiveParserResult
                (SingletonParserResult "aaba" Number)
                NilPositiveParserResult
            )
            Int
        )
    )
testParserListResultT6 =
  ParserResultProxy ::
    forall c.
    Parse
      ( ListParser
          ( SingletonParser
              ( SomeMatcher (ConsSymbol "a" (ConsSymbol "b" NilSymbol))
              )
              Number
          )
          (SingletonMatcher' "-")
          Int
      )
      "aaba"
      c =>
    ParserResultProxy c

testParserListResultT7 ::
  ParserResultProxy
    ( Success
        ( ListParserResult
            ( ConsPositiveParserResult
                (SingletonParserResult "a" Number)
                ( ConsPositiveParserResult
                    (SingletonParserResult "a" Number)
                    ( ConsPositiveParserResult
                        (SingletonParserResult "b" Number)
                        ( ConsPositiveParserResult
                            (SingletonParserResult "a" Number)
                            NilPositiveParserResult
                        )
                    )
                )
            )
            Int
        )
    )
testParserListResultT7 =
  ParserResultProxy ::
    forall c.
    Parse
      ( ListParser
          ( SingletonParser
              ( SomeMatcher (ConsSymbol "a" (ConsSymbol "b" NilSymbol))
              )
              Number
          )
          (SingletonMatcher' "")
          Int
      )
      "aaba"
      c =>
    ParserResultProxy c

testParserDeep ::
  ParserResultProxy
    ( Success
        ( ListParserResult
            ( ConsPositiveParserResult
                (SingletonParserResult "?!" Unit)
                ( ConsPositiveParserResult
                    ( ListParserResult
                        ( ConsPositiveParserResult
                            ( ListParserResult
                                ( ConsPositiveParserResult
                                    (SingletonParserResult "aaba" Number)
                                    ( ConsPositiveParserResult
                                        (SingletonParserResult "cc" Boolean)
                                        NilPositiveParserResult
                                    )
                                )
                                Int
                            )
                            ( ConsPositiveParserResult
                                (SingletonParserResult "qrs" Unit)
                                NilPositiveParserResult
                            )
                        )
                        Unit
                    )
                    NilPositiveParserResult
                )
            )
            Unit
        )
    )
testParserDeep =
  ParserResultProxy ::
    forall c.
    Parse
      ( TupleParser
          ( ConsParser
              ( SingletonParser (SingletonMatcher' "?!") Unit
              )
              ( ConsParser
                  ( TupleParser
                      ( ConsParser
                          ( TupleParser
                              ( ConsParser
                                  ( SingletonParser
                                      ( SomeMatcher (ConsSymbol "a" (ConsSymbol "b" NilSymbol))
                                      )
                                      Number
                                  )
                                  ( ConsParser
                                      ( SingletonParser
                                          ( SomeMatcher (ConsSymbol "c" NilSymbol)
                                          )
                                          Boolean
                                      )
                                      NilParser
                                  )
                              )
                              (SingletonMatcher' ",")
                              Int
                          )
                          ( ConsParser
                              ( SingletonParser (SingletonMatcher' "qrs") Unit
                              )
                              NilParser
                          )
                      )
                      (SingletonMatcher' "")
                      Unit
                  )
                  NilParser
              )
          )
          (SingletonMatcher' "")
          Unit
      )
      "?!aaba,ccqrs"
      c =>
    ParserResultProxy c

--- readme
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
class TypeQLToRow (p :: ParserResult) (i :: Type) (t :: # Type) | p i -> t

instance nqlToRowNil ::
  TypeQLToRow
    ( Success
        (ListParserResult NilPositiveParserResult Keys)
    )
    i
    res

-- this is where we construct the row
instance nqlToRowCons ::
  ( TypeQLToRow (Success (ListParserResult y Keys)) i out
  , Cons key i out res
  ) =>
  TypeQLToRow
    ( Success
        ( ListParserResult
            ( ConsPositiveParserResult
                (SingletonParserResult key Key)
                y
            )
            Keys
        )
    )
    i
    res

-- we construct the type
class SymbolToRow (s :: Symbol) (i :: Type) (r :: # Type) | s i -> r

instance symbolToTypeQLType ::
  ( Parse KeyList s out
  , TypeQLToRow out i r
  ) =>
  SymbolToRow s i r

-- this will validate that an object conforms to our spec and contains Ints
intValidator ::
  forall (c :: # Type).
  SymbolToRow OurSpec Int c =>
  Record c ->
  Record c
intValidator a = a

-- the validator validates that our type is conformant to the DSL!
languages :: { python :: Int, javascript :: Int, java :: Int }
languages =
  intValidator
    { python: 1
    , javascript: 2
    , java: 3
    }

main :: Effect Unit
main = do
  log "🍝"
  log "You should add some non-typelevel tests."
