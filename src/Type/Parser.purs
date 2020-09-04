module Type.Parser where

import Prelude
import Prim.Boolean (kind Boolean, False, True)
import Prim.Ordering (EQ, GT, LT, kind Ordering)
import Prim.Symbol (class Append, class Compare, class Cons)
import Type.Data.Boolean (class And, class Not, class Or)
import Type.Data.Peano (Succ, Z, kind Nat)

foreign import kind Matcher

foreign import kind SymbolList

foreign import kind MatcherList

foreign import kind MatcherResult

foreign import kind Parser

foreign import kind ParserList

foreign import kind ParserUList

foreign import kind ParserResult

foreign import kind PositiveParserResult

foreign import kind PositiveParserResultList

-- lists
foreign import data NilSymbol :: SymbolList

foreign import data ConsSymbol :: Symbol -> SymbolList -> SymbolList

foreign import data NilMatcher :: MatcherList

foreign import data ConsMatcher :: Matcher -> MatcherList -> MatcherList

foreign import data NilParser :: ParserList

foreign import data ConsParser :: Parser -> ParserList -> ParserList

foreign import data OptConsParser :: Parser -> ParserList -> ParserList

foreign import data NilUParser :: ParserUList

foreign import data ConsUParser :: Parser -> ParserUList -> ParserUList

foreign import data NilPositiveParserResult :: PositiveParserResultList

foreign import data ConsPositiveParserResult :: PositiveParserResult -> PositiveParserResultList -> PositiveParserResultList

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
foreign import data FailingParser :: Parser

foreign import data SingletonParser :: Matcher -> Type -> Parser

foreign import data ListParser :: Parser -> Matcher -> Type -> Parser

foreign import data TupleParser :: ParserList -> Matcher -> Type -> Parser

foreign import data UnionParser :: ParserUList -> Type -> Parser

-- result
foreign import data SuccessMatch :: Symbol -> MatcherResult

foreign import data FailMatch :: Symbol -> Symbol -> MatcherResult

foreign import data OptionalParserResult :: Type -> PositiveParserResult

foreign import data SingletonParserResult :: Symbol -> Type -> PositiveParserResult

foreign import data ListParserResult :: PositiveParserResultList -> Type -> PositiveParserResult

foreign import data UnionParserResult :: PositiveParserResult -> Type -> PositiveParserResult

foreign import data Success :: PositiveParserResult -> ParserResult

foreign import data Failure :: Symbol -> ParserResult

-- raccourcis
type UP
  = UnionParser

type LP
  = ListParser

type TP
  = TupleParser

type SP
  = SingletonParser

type FP
  = FailingParser

type Ns
  = NilSymbol

infixr 4 type ConcatMatcher as <<>>

infixr 4 type ConsSymbol as :/

type Nm
  = NilMatcher

infixr 4 type ConsMatcher as :-

type Np
  = NilParser

type Nup
  = NilUParser

infixr 4 type ConsParser as :$

infixr 4 type ConsUParser as :%

infixr 4 type OptConsParser as :$?

infix 4 type SingletonParser as !:!

type Digits
  = "0" :/ "1" :/ "2" :/ "3" :/ "4" :/ "5" :/ "6" :/ "7" :/ "8" :/ "9" :/ Ns

type Lowercase
  = "a"
      :/ "b"
      :/ "c"
      :/ "d"
      :/ "e"
      :/ "f"
      :/ "g"
      :/ "h"
      :/ "i"
      :/ "j"
      :/ "k"
      :/ "l"
      :/ "m"
      :/ "n"
      :/ "o"
      :/ "p"
      :/ "q"
      :/ "r"
      :/ "s"
      :/ "t"
      :/ "u"
      :/ "v"
      :/ "w"
      :/ "x"
      :/ "y"
      :/ "z"
      :/ Ns

type Uppercase
  = "A"
      :/ "B"
      :/ "C"
      :/ "D"
      :/ "E"
      :/ "F"
      :/ "G"
      :/ "H"
      :/ "I"
      :/ "J"
      :/ "K"
      :/ "L"
      :/ "M"
      :/ "N"
      :/ "O"
      :/ "P"
      :/ "Q"
      :/ "R"
      :/ "S"
      :/ "T"
      :/ "U"
      :/ "V"
      :/ "W"
      :/ "X"
      :/ "Y"
      :/ "Z"
      :/ Ns

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

class IsParserOptional (r :: ParserList) (b :: Boolean) | r -> b

instance isParserOptionalCons :: IsParserOptional (ConsParser x y) False

instance isParserOptionalNil :: IsParserOptional NilParser False

instance isParserOptionalOpt :: IsParserOptional (OptConsParser x y) True

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

class PositiveParserResultGate (b :: Boolean) (s0 :: PositiveParserResult) (s1 :: PositiveParserResult) (r :: PositiveParserResult) | b s0 s1 -> r

instance pprgt :: PositiveParserResultGate True s0 s1 s0

instance pprgf :: PositiveParserResultGate False s0 s1 s1

class GetParserType (p :: Parser) (t :: Type) | p -> p

instance getParserTypeFailingParser :: GetParserType FailingParser Void

instance getParserTypeSingletonParser :: GetParserType (SingletonParser m t) t

instance getParserTypeListParser :: GetParserType (ListParser p m t) t

instance getParserTypeTupleParser :: GetParserType (TupleParser pl m t) t

instance getParserTypeUnionParser :: GetParserType (UnionParser pul t) t

class GetParserHead (pl :: ParserList) (p :: Parser) | pl -> p

instance getParserHeadCons :: GetParserHead (ConsParser h t) h

instance getParserHeadOptCons :: GetParserHead (OptConsParser h t) h

instance getParserHeadNil :: GetParserHead NilParser FailingParser

class GetParserTail (pl :: ParserList) (p :: ParserList) | pl -> p

instance getParserTailCons :: GetParserTail (ConsParser h t) t

instance getParserTailOptCons :: GetParserTail (OptConsParser h t) t

instance getParserTailNil :: GetParserTail NilParser NilParser

class IsNilParserList (pl :: ParserList) (b :: Boolean) | pl -> b

instance isNilParserListNil :: IsNilParserList NilParser True

instance isNilParserListCons :: IsNilParserList (ConsParser a b) False

instance isNilParserListOptCons :: IsNilParserList (OptConsParser a b) False

class PositiveResultHack (r :: ParserResult) (h :: PositiveParserResult) | r -> h

instance positiveReal :: PositiveResultHack (Success r) r

instance positiveHack :: PositiveResultHack (Failure s) (SingletonParserResult "" Void)

class AsPositiveParserResultList (r :: ParserResult) (l :: PositiveParserResultList) | r -> l

instance asPositiveParserResultListFailure :: AsPositiveParserResultList (Failure s) NilPositiveParserResult

instance asPositiveParserResultListSuccessSingleton :: AsPositiveParserResultList (Success (SingletonParserResult x y)) NilPositiveParserResult

instance asPositiveParserResultListSuccessPositiveParserResultList :: AsPositiveParserResultList (Success (ListParserResult x y)) x

instance asPositiveParserResultListSuccessUnion :: AsPositiveParserResultList (Success (UnionParserResult x y)) NilPositiveParserResult

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
  , Append h t ht -- for failure message if needed
  ----------------- IsParserOptional pl isOptional
  ----------------- Or isOptional headParsed doNextStep
  ----------------- SymbolGate headParsed t ht tailForNextStep
  , Not onSep notOnSep -- flip the separator
  , TupleParserGate headParsed notOnSep parserTail sep "" "" "" t tag tailres -- if the parsing succeeded, continue
  , IsParserSuccess tailres tailParsed -- did the tail parse as well
  , AsPositiveParserResultList tailres tailParserResults -- gets a list back, or nil if it's not a list 
  ------------- And doNextStep tailParsed fullSuccess
  , And headParsed tailParsed fullSuccess -- did the whole thing succeed?
  , SafeCons th nt t -- new tail
  , Append h th nh -- new head
  , Compare t "" tailToEmptySym -- have we finished parsing the string?
  , IsEQ tailToEmptySym tailEmpty -- is the tail empty?
  ----------------- And doNextStep tailEmpty hpte
  , And headParsed tailEmpty hpte -- we successfully parsed the head and the tail's empty
  , And hpte notOnSep endOfSymbol -- the head parsed was something we want, so we are at the end of the symbol
  , IsNilParserList parserTail parserListEmpty -- there is nothing left in the parser list
  , And endOfSymbol parserListEmpty endOfSymbolAndPl -- we are at the end and there's nothing left to parse
  , Or fullSuccess endOfSymbolAndPl done -- either everything succeeded or nothing left to parse
  , And fullSuccess onSep fullSuccessAndOnStep -- we are on a stepping stage
  , PositiveResultHack headres headParsedHack -- extract a positive result or a dummy value
  ----------------- GetParserType parserHead parserHeadType
  ----------------- PositiveParserResultGate isOptional headParsedHack (OptionalResult parserHeadType) realHeadParsedHack
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
  , TupleParserGate keepGoing onSep pl sep h t nh nt tag o -- shift the head and tail and try again
  , ParserResultGate keepGoing o (Failure ht) oo
  , ParserResultGate done (Success (ListParserResult successfulOutput tag)) oo ooo
  ) =>
  TupleParserGate True onSep pl sep ph pt h t tag ooo

class ListParserGate (continue :: Boolean) (onSeparator :: Boolean) (p :: Parser) (sep :: Matcher) (ph :: Symbol) (pt :: Symbol) (h :: Symbol) (t :: Symbol) (tag :: Type) (r :: ParserResult) | continue onSeparator p sep ph pt h t tag -> r

instance listParserGateStop :: (Append ph pt phpt) => ListParserGate False onSep p sep ph pt h t tag (Failure phpt)

instance listParserGateGo ::
  ( ParserGate onSep (SingletonParser sep Unit) p toParse -- parse using the separator or the head
  , Parse toParse h headres -- parse the head. will yield failure if pl was empty 
  , IsParserSuccess headres headParsed -- did the parsing succeed?
  , Not onSep notOnSep -- flip the separator
  , ListParserGate headParsed notOnSep p sep "" "" "" t tag tailres -- if the parsing succeeded, continue
  , IsParserSuccess tailres tailParsed -- did the tail parse as well
  , AsPositiveParserResultList tailres tailParserResults -- gets a list back, or nil if it's not a list 
  , And headParsed tailParsed fullSuccess -- did the whole thing succeed?
  , SafeCons th nt t -- new tail
  , Append h th nh -- new head
  , Compare t "" tailToEmptySym -- have we finished parsing the string?
  , IsEQ tailToEmptySym tailEmpty -- is the tail empty?
  , And headParsed tailEmpty hpte -- we successfully parsed the head and the tail's empty
  , And hpte notOnSep endOfSymbol -- the head parsed was something we want, so we are at the end of the symbol
  , Or fullSuccess endOfSymbol done -- either everything succeeded or nothing left to parse
  , And fullSuccess onSep fullSuccessAndOnStep -- we are on a stepping stage
  , PositiveResultHack headres headParsedHack -- extract a positive result or a dummy value
  , PositiveParserResultListGate
      endOfSymbol
      (ConsPositiveParserResult headParsedHack NilPositiveParserResult)
      NilPositiveParserResult
      step0
  , PositiveParserResultListGate fullSuccess (ConsPositiveParserResult headParsedHack tailParserResults) step0 step1
  , PositiveParserResultListGate fullSuccessAndOnStep tailParserResults step1 successfulOutput
  , Not done notDone
  , Not tailEmpty notTailEmpty
  , And notDone notTailEmpty keepGoing -- keep going if tail not empty, parser list not empty, and not done
  , Append h t ht -- for failure message if needed
  , ListParserGate keepGoing onSep p sep h t nh nt tag o -- shift the head and tail and try again
  , ParserResultGate keepGoing o (Failure ht) oo
  , ParserResultGate done (Success (ListParserResult successfulOutput tag)) oo ooo
  ) =>
  ListParserGate True onSep p sep ph pt h t tag ooo

-- Union parser
class UnionParserRunner (b :: Boolean) (d :: ParserResult) (l :: ParserUList) (s :: Symbol) (r :: ParserResult) | b d l s -> r

instance uprNilF :: UnionParserRunner False d NilUParser s d

instance uprNilT :: UnionParserRunner True d NilUParser s (Failure s)

instance uprConsT ::
  ( Parse x s r
  , IsParserSuccess r b
  , Not b go
  , UnionParserRunner go d y s v
  , ParserResultGate b r v rr
  ) =>
  UnionParserRunner True d (ConsUParser x y) s rr

instance uprConsF :: UnionParserRunner False d (ConsUParser x y) s d

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

instance parseList ::
  ( Compare s "" eq
  , IsEQ eq b
  , Not b runComp
  , ListParserGate runComp False p m "" "" "" s tag r
  , ParserResultGate b (Success (ListParserResult NilPositiveParserResult tag)) r rr
  ) =>
  Parse (ListParser p m tag) s rr

instance parseUnion ::
  ( UnionParserRunner True (Failure s) (ConsUParser x y) s r
  , IsParserSuccess r b
  , PositiveResultHack r insideR
  , ParserResultGate b (Success (UnionParserResult insideR tag)) r rr
  ) =>
  Parse (UnionParser (ConsUParser x y) tag) s rr

instance parseUnionN :: Parse (UnionParser NilUParser tag) s (Failure s)
