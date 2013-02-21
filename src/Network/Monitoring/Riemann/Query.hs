{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Network.Monitoring.Riemann.Query where

import Data.Text (Text)
import Data.Text (pack, unpack)
import Data.Monoid

data Comparator = Approximately | NotEqual | Equal | Lesser | LesserEqual | Greater | GreaterEqual deriving (Eq, Ord, Show)

data Field = Host | Service | State | Description | MetricF | Metric | TTL | Time deriving (Eq, Ord, Show)

data Value = VString String | VBool Bool | VNil | VInt Int | VFloat Float deriving (Eq, Ord, Show)

class RiemannValued a where
  toRiemannValue :: a -> Value

instance RiemannValued a => RiemannValued (Maybe a) where
  toRiemannValue Nothing = VNil
  toRiemannValue (Just a) = toRiemannValue a

instance RiemannValued String where toRiemannValue = VString
instance RiemannValued Bool   where toRiemannValue = VBool
instance RiemannValued Int    where toRiemannValue = VInt
instance RiemannValued Float  where toRiemannValue = VFloat

data Query = And Query Query
           | Or Query Query
           | Not Query
           | Comparison Comparator Field Value
           | Tagged String
     deriving (Eq, Ord)

toText :: Query -> Text
toText q = ttU q
  where ttQ q = "(" <> ttU q <> ")"
        ttU (And q1 q2) = ttQ q1 <> " and " <> ttQ q2
        ttU (Or q1 q2)  = ttQ q1 <> " or " <> ttQ q2
        ttU (Not q)     = "not " <> ttQ q
        ttU (Comparison cmp f v) = ttField f <> " " <> ttComp cmp <> " " <> ttVal v
        ttU (Tagged s) = "tagged " <> pack s

        ttField Host = "host"
        ttField Service = "service"
        ttField State = "state"
        ttField Description = "description"
        ttField MetricF = "metric_f"
        ttField Metric = "metric"
        ttField TTL = "ttl"
        ttField Time = "time"

        ttComp Approximately = "=~"
        ttComp NotEqual      = "!="
        ttComp Equal         = "=="
        ttComp Lesser        = "<"
        ttComp LesserEqual   = "<="
        ttComp Greater       = ">"
        ttComp GreaterEqual  = ">="

        ttVal (VString s)   = pack (show s)
        ttVal (VBool True)  = "true"
        ttVal (VBool False) = "false"
        ttVal VNil          = "nil"
        ttVal (VInt i)      = pack (show i)
        ttVal (VFloat f)    = pack (show f)

instance Show Query where show = unpack . toText
  
tagged :: String -> Query
tagged = Tagged

infix 3 &&#
infix 2 ||#
infix 6 !#
infix 4 ==#
infix 4 /=#
infix 4 <#
infix 4 >#
infix 4 <=#
infix 4 >=#
infix 4 =~#

(&&#) :: Query -> Query -> Query
(&&#) = And

(||#) :: Query -> Query -> Query
(||#) = Or

(!#) :: Query -> Query
(!#) = Not

(==#) :: RiemannValued a => Field -> a -> Query
f ==# v = Comparison Equal f (toRiemannValue v)

(/=#) :: RiemannValued a => Field -> a -> Query
f /=# v = Comparison NotEqual f (toRiemannValue v)

(<#) :: RiemannValued a => Field -> a -> Query
f <# v = Comparison Lesser f (toRiemannValue v)

(<=#) :: RiemannValued a => Field -> a -> Query
f <=# v = Comparison LesserEqual f (toRiemannValue v)

(>#) :: RiemannValued a => Field -> a -> Query
f ># v = Comparison Greater f (toRiemannValue v)

(>=#) :: RiemannValued a => Field -> a -> Query
f >=# v = Comparison GreaterEqual f (toRiemannValue v)

(=~#) :: RiemannValued a => Field -> a -> Query
f =~# v = Comparison Approximately f (toRiemannValue v)
