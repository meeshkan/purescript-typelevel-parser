module Type.Parser where

import Prelude
import Prim.Boolean (kind Boolean, False, True)
import Prim.Ordering (EQ, GT, LT, kind Ordering)
import Prim.Symbol (class Append, class Compare, class Cons)
import Type.Data.Boolean (class And, class Not, class Or)
import Type.Data.Peano (Succ, Z, D3, kind Nat)

foreign import kind Matcher

foreign import kind SymbolList

foreign import kind MatcherList

foreign import kind MatcherResult

foreign import kind Parser

foreign import kind ParserList

foreign import kind KVParser

foreign import kind KVParserList

foreign import kind ParserResult

foreign import kind PositiveParserResult

foreign import kind PositiveParserResultList

foreign import kind PositiveParserResultIndexedList

-- lists
foreign import data NilSymbol :: SymbolList

foreign import data ConsSymbol :: Symbol -> SymbolList -> SymbolList

foreign import data NilMatcher :: MatcherList

foreign import data ConsMatcher :: Matcher -> MatcherList -> MatcherList

foreign import data NilParser :: ParserList

foreign import data ConsParser :: Parser -> ParserList -> ParserList

foreign import data NilKVParser :: KVParserList

foreign import data ConsKVParser :: KVParser -> KVParserList -> KVParserList

foreign import data NilPositiveParserResult :: PositiveParserResultList

foreign import data ConsPositiveParserResult :: PositiveParserResult -> PositiveParserResultList -> PositiveParserResultList

foreign import data NilIndexedPositiveParserResult :: PositiveParserResultIndexedList

foreign import data ConsIndexedPositiveParserResult :: Symbol -> PositiveParserResult -> PositiveParserResultIndexedList -> PositiveParserResultIndexedList

-- matcher
foreign import data EmptyMatcher :: Matcher

foreign import data AnyMatcher :: Matcher

foreign import data SingletonMatcher' :: Symbol -> Matcher

foreign import data SingletonMatcher :: SymbolList -> Matcher

foreign import data NMatcher' :: Nat -> Symbol -> Matcher

foreign import data NMatcher :: Nat -> SymbolList -> Matcher

foreign import data ManyMatcher' :: Symbol -> Matcher

foreign import data ManyMatcher :: SymbolList -> Matcher

foreign import data OptMatcher :: Matcher -> Matcher

foreign import data NotMatcher' :: Symbol -> Matcher

foreign import data NotMatcher :: SymbolList -> Matcher

foreign import data SepMatcher :: Matcher -> Matcher -> Matcher

foreign import data ConcatMatcher :: Matcher -> Matcher -> Matcher

foreign import data SomeMatcher' :: Symbol -> Matcher

foreign import data SomeMatcher :: SymbolList -> Matcher

foreign import data OrMatcher :: Matcher -> Matcher -> Matcher

foreign import data AndMatcher :: Matcher -> Matcher -> Matcher

foreign import data AtLeastMatcher' :: Nat -> Symbol -> Matcher

foreign import data AtLeastMatcher :: Nat -> SymbolList -> Matcher

foreign import data AtMostMatcher' :: Nat -> Symbol -> Matcher

foreign import data AtMostMatcher :: Nat -> SymbolList -> Matcher

-- parser
foreign import data KVParser :: Matcher -> Parser -> Matcher -> KVParser

foreign import data FailingParser :: Parser

foreign import data SingletonParser :: Matcher -> Type -> Parser

foreign import data ListParser :: Parser -> Matcher -> Type -> Parser

foreign import data IndexedListParser :: Matcher -> Parser -> Matcher -> Type -> Parser

foreign import data TupleParser :: ParserList -> Matcher -> Type -> Parser

foreign import data IndexedTupleParser :: KVParserList -> Matcher -> Type -> Parser

foreign import data UnionParser :: ParserList -> Type -> Parser

foreign import data IgnoreLParser :: Matcher -> Parser -> Parser

foreign import data IgnoreRParser :: Parser -> Matcher -> Parser

-- result
foreign import data SuccessMatch :: Symbol -> MatcherResult

foreign import data FailMatch :: Symbol -> Symbol -> MatcherResult

foreign import data SingletonParserResult :: Symbol -> Type -> PositiveParserResult

foreign import data ListParserResult :: PositiveParserResultList -> Type -> PositiveParserResult

foreign import data IndexedListParserResult :: PositiveParserResultIndexedList -> Type -> PositiveParserResult

foreign import data UnionParserResult :: PositiveParserResult -> Type -> PositiveParserResult

foreign import data Success :: PositiveParserResult -> ParserResult

foreign import data Failure :: Symbol -> ParserResult

-- proxy
data MatcherResultProxy (p :: MatcherResult)
  = MatcherResultProxy

data ParserResultProxy (p :: ParserResult)
  = ParserResultProxy

-- utility
class GatedSafeCons (a :: Boolean) (b :: Symbol) (c :: Symbol) (d :: Symbol) | a b c -> d, a d -> b c

instance gatedSafeConsTrue :: GatedSafeCons True "" "" ""

instance gatedSafeConsFalse :: (Cons b c d) => GatedSafeCons False b c d

class SafeCons (a :: Symbol) (b :: Symbol) (c :: Symbol) | a b -> c, c -> a b

instance safeCons :: (Compare c "" v, IsEQ v g, GatedSafeCons g a b c) => SafeCons a b c

class IsEQ (o :: Ordering) (b :: Boolean) | o -> b

instance isEQ :: IsEQ EQ True

instance isLT :: IsEQ LT False

instance isGT :: IsEQ GT False

class IsParserSuccess (r :: ParserResult) (b :: Boolean) | r -> b

instance isParserSuccessTrue :: IsParserSuccess (Success h) True

instance isParserSuccessFalse :: IsParserSuccess (Failure t) False

class IsMatcherSuccess (r :: MatcherResult) (b :: Boolean) | r -> b

instance isMatcherSuccessTrue :: IsMatcherSuccess (SuccessMatch h) True

instance isMatcherSuccessFalse :: IsMatcherSuccess (FailMatch h t) False

class BooleanGate (b :: Boolean) (s0 :: Boolean) (s1 :: Boolean) (r :: Boolean) | b s0 s1 -> r

instance booleanGateTrue :: BooleanGate True s0 s1 s0

instance booleanGateFalse :: BooleanGate False s0 s1 s1

class SymbolGate (b :: Boolean) (s0 :: Symbol) (s1 :: Symbol) (r :: Symbol) | b s0 s1 -> r

instance symbolGateTrue :: SymbolGate True s0 s1 s0

instance symbolGateFalse :: SymbolGate False s0 s1 s1

class MatcherGate (b :: Boolean) (s0 :: Matcher) (s1 :: Matcher) (r :: Matcher) | b s0 s1 -> r

instance matcherGateTrue :: MatcherGate True s0 s1 s0

instance matcherGateFalse :: MatcherGate False s0 s1 s1

class ParserGate (b :: Boolean) (s0 :: Parser) (s1 :: Parser) (r :: Parser) | b s0 s1 -> r

instance parserGateTrue :: ParserGate True s0 s1 s0

instance parserGateFalse :: ParserGate False s0 s1 s1

class ParserListGate (b :: Boolean) (s0 :: ParserList) (s1 :: ParserList) (r :: ParserList) | b s0 s1 -> r

instance parserlGateTrue :: ParserListGate True s0 s1 s0

instance parserlGateFalse :: ParserListGate False s0 s1 s1

class KVParserListGate (b :: Boolean) (s0 :: KVParserList) (s1 :: KVParserList) (r :: KVParserList) | b s0 s1 -> r

instance kvparserlGateTrue :: KVParserListGate True s0 s1 s0

instance kvparserlGateFalse :: KVParserListGate False s0 s1 s1

class PositiveParserResultIndexedListGate (b :: Boolean) (s0 :: PositiveParserResultIndexedList) (s1 :: PositiveParserResultIndexedList) (r :: PositiveParserResultIndexedList) | b s0 s1 -> r

instance rowListGateTrue :: PositiveParserResultIndexedListGate True s0 s1 s0

instance rowListGateFalse :: PositiveParserResultIndexedListGate False s0 s1 s1

class MatcherListGate (b :: Boolean) (s0 :: MatcherList) (s1 :: MatcherList) (r :: MatcherList) | b s0 s1 -> r

instance matcherListGateTrue :: MatcherListGate True s0 s1 s0

instance matcherListGateFalse :: MatcherListGate False s0 s1 s1

class MatcherResultGate (b :: Boolean) (s0 :: MatcherResult) (s1 :: MatcherResult) (r :: MatcherResult) | b s0 s1 -> r

instance matcherResultGateTrue :: MatcherResultGate True s0 s1 s0

instance matcherResultGateFalse :: MatcherResultGate False s0 s1 s1

class ParserResultGate (b :: Boolean) (s0 :: ParserResult) (s1 :: ParserResult) (r :: ParserResult) | b s0 s1 -> r

instance parserResultGateTrue :: ParserResultGate True s0 s1 s0

instance parserResultGateFalse :: ParserResultGate False s0 s1 s1

class PositiveParserResultListGate (b :: Boolean) (s0 :: PositiveParserResultList) (s1 :: PositiveParserResultList) (r :: PositiveParserResultList) | b s0 s1 -> r

instance pprlgt :: PositiveParserResultListGate True s0 s1 s0

instance pprlgf :: PositiveParserResultListGate False s0 s1 s1

class GetParserHead (pl :: ParserList) (p :: Parser) | pl -> p

instance getParserHeadCons :: GetParserHead (ConsParser h t) h

instance getParserHeadNil :: GetParserHead NilParser FailingParser

class GetParserTail (pl :: ParserList) (p :: ParserList) | pl -> p

instance getParserTailCons :: GetParserTail (ConsParser h t) t

instance getParserTailNil :: GetParserTail NilParser NilParser

foreign import data KVTag :: Type

foreign import data KVLeft :: Type

foreign import data KVSpace :: Type

class GetKVParserHead (pl :: KVParserList) (p :: Parser) | pl -> p

instance getKVParserHeadCons ::
  GetKVParserHead
    ( ConsKVParser (KVParser left right space) t
    )
    ( TupleParser
        ( ConsParser (SingletonParser left KVLeft)
            (ConsParser right NilParser)
        )
        space
        KVTag
    )

instance getKVParserHeadNil ::
  GetKVParserHead NilKVParser ( TupleParser
        ( ConsParser FailingParser
            (ConsParser FailingParser NilParser)
        )
        (EmptyMatcher)
        KVTag
    )

class GetKVParserTail (pl :: KVParserList) (p :: KVParserList) | pl -> p

instance getKVParserTailCons :: GetKVParserTail (ConsKVParser h t) t

instance getKVParserTailNil :: GetKVParserTail NilKVParser NilKVParser

class IsNilParserList (pl :: ParserList) (b :: Boolean) | pl -> b

instance isNilParserListTrue :: IsNilParserList NilParser True

instance isNilParserListFalse :: IsNilParserList (ConsParser a b) False

class IsNilKVParserList (pl :: KVParserList) (b :: Boolean) | pl -> b

instance isNilkvParserListTrue :: IsNilKVParserList NilKVParser True

instance isNilkvParserListFalse :: IsNilKVParserList (ConsKVParser a b) False

class PositiveResultHack (r :: ParserResult) (h :: PositiveParserResult) | r -> h

instance positiveReal :: PositiveResultHack (Success r) r

instance positiveHack :: PositiveResultHack (Failure s) (SingletonParserResult "" Void)

class AsPositiveParserResultList (r :: ParserResult) (l :: PositiveParserResultList) | r -> l

instance asPositiveParserResultListFailure :: AsPositiveParserResultList (Failure s) NilPositiveParserResult

instance asPositiveParserResultListSuccessSingleton :: AsPositiveParserResultList (Success (SingletonParserResult x y)) NilPositiveParserResult

instance asPositiveParserResultListSuccessPositiveParserResultList :: AsPositiveParserResultList (Success (ListParserResult x y)) x

instance asPositiveParserResultListSuccessPositiveParserResultIndexedList :: AsPositiveParserResultList (Success (IndexedListParserResult x y)) NilPositiveParserResult

instance asPositiveParserResultListSuccessUnion :: AsPositiveParserResultList (Success (UnionParserResult x y)) NilPositiveParserResult

class AsPositiveParserResultIndexedList (r :: ParserResult) (l :: PositiveParserResultIndexedList) | r -> l

instance asPositiveParserPositiveParserResultIndexedListFailure :: AsPositiveParserResultIndexedList (Failure s) NilIndexedPositiveParserResult

instance asPositiveParserPositiveParserResultIndexedListSuccessSingleton :: AsPositiveParserResultIndexedList (Success (SingletonParserResult x y)) NilIndexedPositiveParserResult

instance asPositiveParserPositiveParserResultIndexedListSuccessPositiveParserResultList :: AsPositiveParserResultIndexedList (Success (ListParserResult x y)) NilIndexedPositiveParserResult

instance asPositiveParserPositiveParserResultIndexedListSuccessPositiveParserResultIndexedList :: AsPositiveParserResultIndexedList (Success (IndexedListParserResult x y)) x

instance asPositiveParserPositiveParserResultIndexedListSuccessUnion :: AsPositiveParserResultIndexedList (Success (UnionParserResult x y)) NilIndexedPositiveParserResult

-- singleton
class SingletonMatcherGate (b :: Boolean) (l :: SymbolList) (s :: Symbol) (r :: MatcherResult) | b l s -> r

instance singletonMatcherGateTrue :: SingletonMatcherGate True l s (SuccessMatch s)

instance singletonMatcherGateFalseNil :: SingletonMatcherGate False NilSymbol s (FailMatch s "")

instance singletonMatcherGateFalseCons ::
  ( Compare h s v
  , IsEQ v b
  , SingletonMatcherGate b t s r
  ) =>
  SingletonMatcherGate False (ConsSymbol h t) s r

class NotMatcherGate (b :: Boolean) (l :: SymbolList) (s :: Symbol) (r :: MatcherResult) | b l s -> r

instance notMatcherGateTrue :: NotMatcherGate True l s (SuccessMatch s)

instance notMatcherGateFalseNil :: NotMatcherGate False NilSymbol s (FailMatch s "")

instance notMatcherGateFalseCons ::
  ( Compare h s v
  , IsEQ v bb
  , Not bb b
  , NotMatcherGate b t s r
  ) =>
  NotMatcherGate False (ConsSymbol h t) s r

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

-- at most
class AtMostMatcherGate (b :: Boolean) (n :: Nat) (l :: SymbolList) (s :: Symbol) (r :: MatcherResult) | b n l s -> r

instance atMostMatcherGateZ ::
  ( Compare s "" e
  , IsEQ e b
  , MatcherResultGate b (SuccessMatch "") (FailMatch "" s) r
  ) =>
  AtMostMatcherGate False Z l s r

instance atMostMatcherGateTrueZ :: AtMostMatcherGate True Z l s (SuccessMatch s)

instance atMostMatcherGateS ::
  ( Match (NMatcher (Succ n) l) s o
  , IsMatcherSuccess o b
  , AtMostMatcherGate b n l s r
  ) =>
  AtMostMatcherGate False (Succ n) l s r

instance atMostMatcherGateStop :: AtMostMatcherGate True (Succ n) l s (SuccessMatch s)

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

-- n 
class NMatcherGate (b :: Boolean) (n :: Nat) (l :: SymbolList) (h :: Symbol) (t :: Symbol) (r :: MatcherResult) | b n l h t -> r

instance nMatcherGateTrueZero ::
  ( Compare t "" e
  , IsEQ e b
  , MatcherResultGate b (SuccessMatch h) (FailMatch h t) v
  ) =>
  NMatcherGate True Z l h t v

instance nMatcherGateFalseZero :: NMatcherGate False Z l h t (FailMatch h t)

instance nMatcherGateFalseSucc :: NMatcherGate False (Succ n) l h t (FailMatch h t)

instance nMatcherGateTrueSucc ::
  ( SafeCons th tt t
  , Append h th nh
  , SingletonMatcherGate False l th r
  , IsMatcherSuccess r b
  , NMatcherGate b n l nh tt v
  ) =>
  NMatcherGate True (Succ n) l h t v

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
      ( NMatcher D3 (ConsSymbol "s" (ConsSymbol "r" (ConsSymbol "q" NilSymbol)))
      )
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

-- many 
class ManyMatcherTailGate (b :: Boolean) (l :: SymbolList) (ph :: Symbol) (pt :: Symbol) (h :: Symbol) (t :: Symbol) (r :: MatcherResult) | b l ph pt h t -> r

class ManyMatcherGate (b :: Boolean) (l :: SymbolList) (ph :: Symbol) (pt :: Symbol) (h :: Symbol) (t :: Symbol) (r :: MatcherResult) | b l ph pt h t -> r

instance manyMatcherGateFalse :: ManyMatcherGate False l ph pt h t (FailMatch ph pt)

instance manyMatcherTailGateFalse :: ManyMatcherTailGate False l ph pt h t (SuccessMatch ph)

instance manyMatcherTailGateTrue ::
  ( Cons th tt t
  , Append h th nh
  , SingletonMatcherGate False l th r
  , IsMatcherSuccess r b
  , ManyMatcherGate b l h t nh tt v
  ) =>
  ManyMatcherTailGate True l ph pt h t v

instance manyMatcherGateTrue ::
  ( Compare t "" c
  , IsEQ c bb
  , Not bb b
  , SymbolGate b ph h nph
  , SymbolGate b pt t npt
  , ManyMatcherTailGate b l nph npt h t v
  ) =>
  ManyMatcherGate True l ph pt h t v

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

-- opt
class OptMatcherGate (v :: MatcherResult) (s :: Symbol) (r :: MatcherResult) | v s -> r

instance optMatcherGateSuccess :: OptMatcherGate (SuccessMatch a) s (SuccessMatch a)

instance optMatcherGateFail :: OptMatcherGate (FailMatch a b) s (SuccessMatch "")

testOptMatcherT0 :: MatcherResultProxy (SuccessMatch "bar")
testOptMatcherT0 =
  MatcherResultProxy ::
    forall c.
    Match
      ( OptMatcher (SingletonMatcher (ConsSymbol "bar" NilSymbol))
      )
      "bar"
      c =>
    MatcherResultProxy c

testOptMatcherT1 :: MatcherResultProxy (SuccessMatch "")
testOptMatcherT1 =
  MatcherResultProxy ::
    forall c.
    Match
      ( OptMatcher (SingletonMatcher (ConsSymbol "bar" NilSymbol))
      )
      "baz"
      c =>
    MatcherResultProxy c

-- concat
class ConcatMatcherGate (continue :: Boolean) (left :: Matcher) (right :: Matcher) (ph :: Symbol) (pt :: Symbol) (h :: Symbol) (t :: Symbol) (r :: MatcherResult) | continue left right ph pt h t -> r

instance concatMatcherGateStop :: ConcatMatcherGate False left right ph pt h t (FailMatch ph pt)

instance concatMatcherGateGo ::
  ( Match left h headres
  , IsMatcherSuccess headres headParsed
  , Match right t tailres
  , IsMatcherSuccess tailres tailParsed
  , And headParsed tailParsed fullSuccess
  , Compare t "" tailToEmptySym
  , IsEQ tailToEmptySym tailEmpty
  , Not tailEmpty notTailEmpty
  , SafeCons th nt t
  , Append h th nh
  , Not fullSuccess failed
  , And failed notTailEmpty keepGoing
  , Append h t ht
  , ConcatMatcherGate keepGoing left right h t nh nt o
  , MatcherResultGate fullSuccess (SuccessMatch ht) o ooo
  ) =>
  ConcatMatcherGate True left right ph pt h t ooo

-- sep
class SepMatcherGate (continue :: Boolean) (onSeparator :: Boolean) (m :: Matcher) (sep :: Matcher) (ph :: Symbol) (pt :: Symbol) (h :: Symbol) (t :: Symbol) (r :: MatcherResult) | continue onSeparator m sep ph pt h t -> r

instance sepMatcherGateStop :: SepMatcherGate False onSep m sep ph pt h t (FailMatch ph pt)

instance sepMatcherGateGo ::
  ( MatcherGate onSep sep m toMatch
  , Match toMatch h headres
  , IsMatcherSuccess headres headParsed
  , Not onSep notOnSep
  , SepMatcherGate headParsed notOnSep m sep "" "" "" t tailres
  , IsMatcherSuccess tailres tailParsed
  , And headParsed tailParsed fullSuccess
  , SafeCons th nt t
  , Append h th nh
  , Compare t "" tailToEmptySym
  , IsEQ tailToEmptySym tailEmpty
  , And headParsed tailEmpty hpte
  , And hpte notOnSep endOfSymbol
  , Or fullSuccess endOfSymbol done
  , Not done notDone
  , Not tailEmpty notTailEmpty
  , And notDone notTailEmpty keepGoing
  , Append h t ht
  , SepMatcherGate keepGoing onSep m sep h t nh nt o
  , MatcherResultGate keepGoing o (FailMatch h t) oo
  , MatcherResultGate done (SuccessMatch ht) oo ooo
  ) =>
  SepMatcherGate True onSep m sep ph pt h t ooo

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
      ( ConcatMatcher
          (ManyMatcher' "a")
          ( ConcatMatcher
              (SingletonMatcher (ConsSymbol "hel" NilSymbol))
              (SingletonMatcher (ConsSymbol "lo" NilSymbol))
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

-- parser tuple
class TupleParserGate (continue :: Boolean) (onSeparator :: Boolean) (pl :: ParserList) (sep :: Matcher) (ph :: Symbol) (pt :: Symbol) (h :: Symbol) (t :: Symbol) (tag :: Type) (r :: ParserResult) | continue onSeparator pl sep ph pt h t tag -> r

instance tupleParserGateStop :: (Append ph pt phpt) => TupleParserGate False onSep pl sep ph pt h t tag (Failure phpt)

instance tupleParserGateGo ::
  ( GetParserHead pl parserHead -- get the head parser, or failure if no head
  , GetParserTail pl maybeParserTail -- get the tail parser
  , ParserGate onSep (SingletonParser sep Unit) parserHead toParse -- parse using the separator or the head
  , ParserListGate onSep pl maybeParserTail parserTail
  , Parse toParse h headres -- parse the head. will yield failure if pl was empty 
  , IsParserSuccess headres headParsed -- did the parsing succeed?
  , Not onSep notOnSep -- flip the separator
  , TupleParserGate headParsed notOnSep parserTail sep "" "" "" t tag tailres -- if the parsing succeeded, continue
  , IsParserSuccess tailres tailParsed -- did the tail parse as well
  , AsPositiveParserResultList tailres tailParserResults -- gets a list back, or nil if it's not a list 
  , And headParsed tailParsed fullSuccess -- did the whole thing succeed?
  , SafeCons th nt t -- new tail
  , Append h th nh -- new head
  , Compare t "" tailToEmptySym -- have we finished parsing the string?
  , IsEQ tailToEmptySym tailEmpty -- is the tail empty?
  , And headParsed tailEmpty hpte -- we successfully parsed the head and the tail's empty
  , And hpte notOnSep endOfSymbol -- the head parsed was something we want, so we are at the end of the symbol
  , IsNilParserList parserTail parserListEmpty -- there is nothing left in the parser list
  , And endOfSymbol parserListEmpty endOfSymbolAndPl -- we are at the end and there's nothing left to parse
  , Or fullSuccess endOfSymbolAndPl done -- either everything succeeded or nothing left to parse
  , And fullSuccess onSep fullSuccessAndOnStep -- we are on a stepping stage
  , PositiveResultHack headres headParsedHack -- extract a positive result or a dummy value
  , PositiveParserResultListGate
      endOfSymbolAndPl
      (ConsPositiveParserResult headParsedHack NilPositiveParserResult)
      NilPositiveParserResult
      step0
  , PositiveParserResultListGate fullSuccess (ConsPositiveParserResult headParsedHack tailParserResults) step0 step1
  , PositiveParserResultListGate fullSuccessAndOnStep tailParserResults step1 successfulOutput
  , Not done notDone
  , Not tailEmpty notTailEmpty
  , And notDone notTailEmpty keepGoing -- keep going if tail not empty, parser list not empty, and not done
  , Append h t ht -- for failure message if needed
  , TupleParserGate keepGoing onSep pl sep h t nh nt tag o -- shift the head and tail and try again
  , ParserResultGate keepGoing o (Failure ht) oo
  , ParserResultGate done (Success (ListParserResult successfulOutput tag)) oo ooo
  ) =>
  TupleParserGate True onSep pl sep ph pt h t tag ooo

class ParseKV (p :: Parser) (s :: Symbol) (k :: Symbol) (v :: ParserResult) | p s -> k v

class ExtractFirst (pr :: ParserResult) (s :: Symbol) | pr -> s

instance extractFirstFailure :: ExtractFirst (Failure x) ""

instance extractFirstSuccess :: ExtractFirst (Success (ListParserResult (ConsPositiveParserResult (SingletonParserResult x tp) y) tp2)) x

class ExtractSecond (pr :: ParserResult) (s :: ParserResult) | pr -> s

instance extractSecondFailure :: ExtractSecond (Failure x) (Failure x)

instance extractSecondSuccess :: ExtractSecond (Success (ListParserResult (ConsPositiveParserResult x ((ConsPositiveParserResult y z))) tp)) (Success y)

instance parseKV ::
  ( Parse
      ( TupleParser
          ( ConsParser left
              (ConsParser right NilParser)
          )
          space
          KVTag
      )
      s
      o
  , ExtractFirst o k
  , ExtractSecond o v
  ) =>
  ParseKV
    ( TupleParser
        ( ConsParser left
            (ConsParser right NilParser)
        )
        space
        KVTag
    )
    s
    k
    v

class IndexedTupleParserGate (continue :: Boolean) (onSeparator :: Boolean) (pl :: KVParserList) (sep :: Matcher) (ph :: Symbol) (pt :: Symbol) (h :: Symbol) (t :: Symbol) (tag :: Type) (r :: ParserResult) | continue onSeparator pl sep ph pt h t tag -> r

instance iTupleParserGateStop :: (Append ph pt phpt) => IndexedTupleParserGate False onSep pl sep ph pt h t tag (Failure phpt)

instance iTupleParserGateGo ::
  ( GetKVParserHead pl parserHead -- get the head parser, or failure if no head
  , GetKVParserTail pl maybeParserTail -- get the tail parser
  , KVParserListGate onSep pl maybeParserTail parserTail
  , ParseKV parserHead h key val -- parse the head as if it were a kv parser
  , Parse (SingletonParser sep Unit) h headresASSep -- parse the head as if it were a separator
  , IsParserSuccess val headParsedAsKV -- did the parsing succeed?
  , IsParserSuccess headresASSep headParsedAsSep -- did the parsing succeed?
  , Not onSep notOnSep -- flip the separator
  , BooleanGate onSep headParsedAsSep headParsedAsKV headParsed
  , IndexedTupleParserGate headParsed notOnSep parserTail sep "" "" "" t tag tailres -- if the parsing succeeded, continue
  , IsParserSuccess tailres tailParsed -- did the tail parse as well
  , AsPositiveParserResultIndexedList tailres tailParserResults -- gets a list back, or nil if it's not a list 
  , And headParsed tailParsed fullSuccess -- did the whole thing succeed?
  , SafeCons th nt t -- new tail
  , Append h th nh -- new head
  , Compare t "" tailToEmptySym -- have we finished parsing the string?
  , IsEQ tailToEmptySym tailEmpty -- is the tail empty?
  , And headParsed tailEmpty hpte -- we successfully parsed the head and the tail's empty
  , And hpte notOnSep endOfSymbol -- the head parsed was something we want, so we are at the end of the symbol
  , IsNilKVParserList parserTail parserListEmpty -- there is nothing left in the parser list
  , And endOfSymbol parserListEmpty endOfSymbolAndPl -- we are at the end and there's nothing left to parse
  , Or fullSuccess endOfSymbolAndPl done -- either everything succeeded or nothing left to parse
  , And fullSuccess onSep fullSuccessAndOnStep -- we are on a stepping stage
  , PositiveResultHack val valHack -- extract a positive result or a dummy value
  , PositiveParserResultIndexedListGate
      endOfSymbolAndPl
      (ConsIndexedPositiveParserResult key valHack NilIndexedPositiveParserResult)
      NilIndexedPositiveParserResult
      step0
  , PositiveParserResultIndexedListGate fullSuccess (ConsIndexedPositiveParserResult key valHack tailParserResults) step0 step1
  , PositiveParserResultIndexedListGate fullSuccessAndOnStep tailParserResults step1 successfulOutput
  , Not done notDone
  , Not tailEmpty notTailEmpty
  , And notDone notTailEmpty keepGoing -- keep going if tail not empty, parser list not empty, and not done
  , Append h t ht -- for failure message if needed
  , IndexedTupleParserGate keepGoing onSep pl sep h t nh nt tag o -- shift the head and tail and try again
  , ParserResultGate keepGoing o (Failure ht) oo
  , ParserResultGate done (Success (IndexedListParserResult successfulOutput tag)) oo ooo
  ) =>
  IndexedTupleParserGate True onSep pl sep ph pt h t tag ooo

-- Match
class Match (p :: Matcher) (s :: Symbol) (r :: MatcherResult) | p s -> r

instance matchEmptyMatcher :: Match EmptyMatcher s (SuccessMatch "")

instance matchAnyMatcher :: Match AnyMatcher s (SuccessMatch s)

instance matchSingletonMatcher ::
  SingletonMatcherGate False l s r =>
  Match (SingletonMatcher l) s r

instance matchSingletonMatcher' ::
  SingletonMatcherGate
      False
      ( ConsSymbol v NilSymbol
      )
      s
      r =>
  Match (SingletonMatcher' v) s r

instance matchNotMatcher ::
  NotMatcherGate False l s r =>
  Match (NotMatcher l) s r

instance matchNotMatcher' ::
  NotMatcherGate
      False
      ( ConsSymbol v NilSymbol
      )
      s
      r =>
  Match (NotMatcher' v) s r

instance matchNMatcher ::
  NMatcherGate True n l "" s r =>
  Match (NMatcher n l) s r

instance matchNMatcher' ::
  NMatcherGate True n (ConsSymbol v NilSymbol) "" s r =>
  Match (NMatcher' n v) s r

instance matchManyMatcher ::
  ManyMatcherGate True l "" "" "" s r =>
  Match (ManyMatcher l) s r

instance matchManyMatcher' ::
  ManyMatcherGate True (ConsSymbol v NilSymbol) "" "" "" s r =>
  Match (ManyMatcher' v) s r

instance matchOptMatcher ::
  (Match o s v, OptMatcherGate v s r) =>
  Match (OptMatcher o) s r

instance matchSepMatcher ::
  ( Compare s "" eq
  , IsEQ eq b
  , Not b runComp
  , SepMatcherGate runComp False m sep "" "" "" s r
  , MatcherResultGate b (SuccessMatch "") r rr
  ) =>
  Match (SepMatcher m sep) s rr

instance matchConcatMatcher ::
  (ConcatMatcherGate True left right "" "" "" s r) =>
  Match (ConcatMatcher left right) s r

instance matchAndMatcher ::
  ( Match a s r0
  , Match b s r1
  , IsMatcherSuccess r0 s0
  , IsMatcherSuccess r1 s1
  , And s0 s1 yes
  , MatcherResultGate s0 r1 r0 fm
  , MatcherResultGate yes (SuccessMatch s) fm r
  ) =>
  Match (AndMatcher a b) s r

instance matchOrMatcher ::
  ( Match a s r0
  , Match b s r1
  , IsMatcherSuccess r0 s0
  , IsMatcherSuccess r1 s1
  , Or s0 s1 yes
  , MatcherResultGate yes (SuccessMatch s) r0 r
  ) =>
  Match (OrMatcher a b) s r

instance matchSomeMatcher ::
  ( Match
        ( ConcatMatcher
            (SingletonMatcher l)
            (ManyMatcher l)
        )
        s
        r
    ) =>
  Match (SomeMatcher l) s r

instance matchSomeMatcher' ::
  ( Match (SomeMatcher (ConsSymbol v NilSymbol)) s r
    ) =>
  Match (SomeMatcher' v) s r

instance matchAtLeastMatcher ::
  ( Match
        ( ConcatMatcher
            (NMatcher n l)
            (ManyMatcher l)
        )
        s
        r
    ) =>
  Match (AtLeastMatcher n l) s r

instance matchAtLeastMatcher' ::
  Match (AtLeastMatcher n (ConsSymbol v NilSymbol)) s r =>
  Match (AtLeastMatcher' n v) s r

instance matchAtMostMatcher ::
  (AtMostMatcherGate False n l s r) =>
  Match (AtMostMatcher n l) s r

instance matchAtMostMatcher' ::
  Match (AtMostMatcher n (ConsSymbol v NilSymbol)) s r =>
  Match (AtMostMatcher' n v) s r

class Parse (p :: Parser) (s :: Symbol) (r :: ParserResult) | p s -> r

instance parseFailure :: Parse FailingParser s (Failure s)

instance parseSingleton ::
  ( Match m s x
  , IsMatcherSuccess x b
  , ParserResultGate b (Success (SingletonParserResult s t)) (Failure s) r
  ) =>
  Parse (SingletonParser m t) s r

instance parseTuple ::
  ( Compare s "" eq
  , IsEQ eq b
  , Not b runComp
  , TupleParserGate runComp False pl m "" "" "" s tag r
  , ParserResultGate b (Success (ListParserResult NilPositiveParserResult tag)) r rr
  ) =>
  Parse (TupleParser pl m tag) s rr
