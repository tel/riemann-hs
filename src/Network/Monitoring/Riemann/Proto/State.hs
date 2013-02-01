{-# LANGUAGE BangPatterns, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}
module Network.Monitoring.Riemann.Proto.State (State(..)) where
import Prelude ((+), (/))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
 
data State = State{time :: !(P'.Maybe P'.Int64), state :: !(P'.Maybe P'.Utf8), service :: !(P'.Maybe P'.Utf8),
                   host :: !(P'.Maybe P'.Utf8), description :: !(P'.Maybe P'.Utf8), once :: !(P'.Maybe P'.Bool),
                   tags :: !(P'.Seq P'.Utf8), ttl :: !(P'.Maybe P'.Float)}
           deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data)
 
instance P'.Mergeable State where
  mergeAppend (State x'1 x'2 x'3 x'4 x'5 x'6 x'7 x'8) (State y'1 y'2 y'3 y'4 y'5 y'6 y'7 y'8)
   = State (P'.mergeAppend x'1 y'1) (P'.mergeAppend x'2 y'2) (P'.mergeAppend x'3 y'3) (P'.mergeAppend x'4 y'4)
      (P'.mergeAppend x'5 y'5)
      (P'.mergeAppend x'6 y'6)
      (P'.mergeAppend x'7 y'7)
      (P'.mergeAppend x'8 y'8)
 
instance P'.Default State where
  defaultValue
   = State P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue
      P'.defaultValue
 
instance P'.Wire State where
  wireSize ft' self'@(State x'1 x'2 x'3 x'4 x'5 x'6 x'7 x'8)
   = case ft' of
       10 -> calc'Size
       11 -> P'.prependMessageSize calc'Size
       _ -> P'.wireSizeErr ft' self'
    where
        calc'Size
         = (P'.wireSizeOpt 1 3 x'1 + P'.wireSizeOpt 1 9 x'2 + P'.wireSizeOpt 1 9 x'3 + P'.wireSizeOpt 1 9 x'4 +
             P'.wireSizeOpt 1 9 x'5
             + P'.wireSizeOpt 1 8 x'6
             + P'.wireSizeRep 1 9 x'7
             + P'.wireSizeOpt 1 2 x'8)
  wirePut ft' self'@(State x'1 x'2 x'3 x'4 x'5 x'6 x'7 x'8)
   = case ft' of
       10 -> put'Fields
       11 -> do
               P'.putSize (P'.wireSize 10 self')
               put'Fields
       _ -> P'.wirePutErr ft' self'
    where
        put'Fields
         = do
             P'.wirePutOpt 8 3 x'1
             P'.wirePutOpt 18 9 x'2
             P'.wirePutOpt 26 9 x'3
             P'.wirePutOpt 34 9 x'4
             P'.wirePutOpt 42 9 x'5
             P'.wirePutOpt 48 8 x'6
             P'.wirePutRep 58 9 x'7
             P'.wirePutOpt 69 2 x'8
  wireGet ft'
   = case ft' of
       10 -> P'.getBareMessageWith update'Self
       11 -> P'.getMessageWith update'Self
       _ -> P'.wireGetErr ft'
    where
        update'Self wire'Tag old'Self
         = case wire'Tag of
             8 -> Prelude'.fmap (\ !new'Field -> old'Self{time = Prelude'.Just new'Field}) (P'.wireGet 3)
             18 -> Prelude'.fmap (\ !new'Field -> old'Self{state = Prelude'.Just new'Field}) (P'.wireGet 9)
             26 -> Prelude'.fmap (\ !new'Field -> old'Self{service = Prelude'.Just new'Field}) (P'.wireGet 9)
             34 -> Prelude'.fmap (\ !new'Field -> old'Self{host = Prelude'.Just new'Field}) (P'.wireGet 9)
             42 -> Prelude'.fmap (\ !new'Field -> old'Self{description = Prelude'.Just new'Field}) (P'.wireGet 9)
             48 -> Prelude'.fmap (\ !new'Field -> old'Self{once = Prelude'.Just new'Field}) (P'.wireGet 8)
             58 -> Prelude'.fmap (\ !new'Field -> old'Self{tags = P'.append (tags old'Self) new'Field}) (P'.wireGet 9)
             69 -> Prelude'.fmap (\ !new'Field -> old'Self{ttl = Prelude'.Just new'Field}) (P'.wireGet 2)
             _ -> let (field'Number, wire'Type) = P'.splitWireTag wire'Tag in P'.unknown field'Number wire'Type old'Self
 
instance P'.MessageAPI msg' (msg' -> State) State where
  getVal m' f' = f' m'
 
instance P'.GPB State
 
instance P'.ReflectDescriptor State where
  getMessageInfo _ = P'.GetMessageInfo (P'.fromDistinctAscList []) (P'.fromDistinctAscList [8, 18, 26, 34, 42, 48, 58, 69])
  reflectDescriptorInfo _
   = Prelude'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".ExtraRiemann.State\", haskellPrefix = [MName \"Network\",MName \"Monitoring\",MName \"Riemann\"], parentModule = [MName \"Proto\"], baseName = MName \"State\"}, descFilePath = [\"Network\",\"Monitoring\",\"Riemann\",\"Proto\",\"State.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".ExtraRiemann.State.time\", haskellPrefix' = [MName \"Network\",MName \"Monitoring\",MName \"Riemann\"], parentModule' = [MName \"Proto\",MName \"State\"], baseName' = FName \"time\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 8}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 3}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".ExtraRiemann.State.state\", haskellPrefix' = [MName \"Network\",MName \"Monitoring\",MName \"Riemann\"], parentModule' = [MName \"Proto\",MName \"State\"], baseName' = FName \"state\"}, fieldNumber = FieldId {getFieldId = 2}, wireTag = WireTag {getWireTag = 18}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 9}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".ExtraRiemann.State.service\", haskellPrefix' = [MName \"Network\",MName \"Monitoring\",MName \"Riemann\"], parentModule' = [MName \"Proto\",MName \"State\"], baseName' = FName \"service\"}, fieldNumber = FieldId {getFieldId = 3}, wireTag = WireTag {getWireTag = 26}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 9}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".ExtraRiemann.State.host\", haskellPrefix' = [MName \"Network\",MName \"Monitoring\",MName \"Riemann\"], parentModule' = [MName \"Proto\",MName \"State\"], baseName' = FName \"host\"}, fieldNumber = FieldId {getFieldId = 4}, wireTag = WireTag {getWireTag = 34}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 9}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".ExtraRiemann.State.description\", haskellPrefix' = [MName \"Network\",MName \"Monitoring\",MName \"Riemann\"], parentModule' = [MName \"Proto\",MName \"State\"], baseName' = FName \"description\"}, fieldNumber = FieldId {getFieldId = 5}, wireTag = WireTag {getWireTag = 42}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 9}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".ExtraRiemann.State.once\", haskellPrefix' = [MName \"Network\",MName \"Monitoring\",MName \"Riemann\"], parentModule' = [MName \"Proto\",MName \"State\"], baseName' = FName \"once\"}, fieldNumber = FieldId {getFieldId = 6}, wireTag = WireTag {getWireTag = 48}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 8}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".ExtraRiemann.State.tags\", haskellPrefix' = [MName \"Network\",MName \"Monitoring\",MName \"Riemann\"], parentModule' = [MName \"Proto\",MName \"State\"], baseName' = FName \"tags\"}, fieldNumber = FieldId {getFieldId = 7}, wireTag = WireTag {getWireTag = 58}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = True, mightPack = False, typeCode = FieldType {getFieldType = 9}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".ExtraRiemann.State.ttl\", haskellPrefix' = [MName \"Network\",MName \"Monitoring\",MName \"Riemann\"], parentModule' = [MName \"Proto\",MName \"State\"], baseName' = FName \"ttl\"}, fieldNumber = FieldId {getFieldId = 8}, wireTag = WireTag {getWireTag = 69}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 2}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing}], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = False, lazyFields = False}"