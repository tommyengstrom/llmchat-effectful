module Rake.TypesSpec where

import Data.Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.OpenApi
import Data.OpenApi.Optics ()
import Data.UUID qualified as UUID
import Optics.Core ((?~))
import Rake
import Relude
import StructuredSchemaTestTypes
import Test.Hspec

data ClosedRecord = ClosedRecord
    { name :: Text
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON, ToSchema)

data OpenMapPayload = OpenMapPayload
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)

instance ToSchema OpenMapPayload where
    declareNamedSchema _ =
        pure $
            NamedSchema (Just "OpenMapPayload") $
                mempty
                    & #type ?~ OpenApiObject
                    & #additionalProperties ?~ AdditionalPropertiesAllowed True

spec :: Spec
spec = describe "Rake.Types" $ do
    describe "typed structured schema rendering" $ do
        it "renders non-nullary sums as top-level oneOf" $ do
            case jsonSchemaFormat @NonNullarySum of
                JsonSchema schemaValue ->
                    schemaKeywordPaths "oneOf" schemaValue `shouldBe` ["$.oneOf"]
                _ ->
                    expectationFailure "Expected JsonSchema from jsonSchemaFormat"

        it "renders Maybe fields as nullable anyOf" $ do
            case jsonSchemaFormat @RecordWithMaybe of
                JsonSchema schemaValue ->
                    schemaKeywordPaths "anyOf" schemaValue `shouldBe` ["$.properties.toolCall.anyOf"]
                _ ->
                    expectationFailure "Expected JsonSchema from jsonSchemaFormat"

        it "renders nullary enums as plain enum strings without union keywords" $ do
            case jsonSchemaFormat @NullaryEnum of
                JsonSchema schemaValue -> do
                    schemaKeywordPaths "oneOf" schemaValue `shouldBe` []
                    schemaKeywordPaths "anyOf" schemaValue `shouldBe` []
                    lookupField "enum" schemaValue
                        `shouldBe` Just (toJSON (["Alpha", "Beta"] :: [Text]))
                _ ->
                    expectationFailure "Expected JsonSchema from jsonSchemaFormat"

        it "renders Maybe-bearing typed tool arguments with nullable anyOf" $ do
            let tool = defineToolWithArgument @RecordWithMaybe "lookup" "lookup tool" (\_ -> pure (Right "ok"))
                ToolDef{parameterSchema = parameterSchemaValue} = tool
            case parameterSchemaValue of
                Just schemaValue ->
                    schemaKeywordPaths "anyOf" schemaValue `shouldBe` ["$.properties.toolCall.anyOf"]
                Nothing ->
                    expectationFailure "Expected parameterSchema from defineToolWithArgument"

    describe "jsonSchemaFormat" $ do
        it "adds additionalProperties false when the typed schema leaves it absent" $ do
            case jsonSchemaFormat @ClosedRecord of
                JsonSchema schemaValue ->
                    lookupField "additionalProperties" schemaValue `shouldBe` Just (Bool False)
                _ ->
                    expectationFailure "Expected JsonSchema from jsonSchemaFormat"

        it "preserves explicit additionalProperties from typed schemas" $ do
            case jsonSchemaFormat @OpenMapPayload of
                JsonSchema schemaValue ->
                    lookupField "additionalProperties" schemaValue `shouldBe` Just (Bool True)
                _ ->
                    expectationFailure "Expected JsonSchema from jsonSchemaFormat"

        it "makes optional typed fields required and nullable" $ do
            case jsonSchemaFormat @RecordWithMaybe of
                JsonSchema schemaValue -> do
                    sort (fromMaybe [] (requiredFieldNames schemaValue))
                        `shouldBe` sort ["response", "translated_response", "toolCall"]
                    lookupPath ["properties", "toolCall"] schemaValue
                        `shouldBe` Just (nullableSchema exampleToolCallSchema)
                _ ->
                    expectationFailure "Expected JsonSchema from jsonSchemaFormat"

        it "normalizes nested optional object fields" $ do
            case jsonSchemaFormat @NestedRecord of
                JsonSchema schemaValue -> do
                    sort (fromMaybe [] (requiredFieldNames schemaValue))
                        `shouldBe` sort ["label", "child"]
                    lookupPath ["properties", "child"] schemaValue
                        `shouldBe` Just (nullableSchema nestedChildSchema)
                _ ->
                    expectationFailure "Expected JsonSchema from jsonSchemaFormat"

    describe "defineToolWithArgument" $ do
        it "preserves explicit additionalProperties from typed tool schemas" $ do
            let tool = defineToolWithArgument @OpenMapPayload "open_map" "open map tool" (\_ -> pure (Right "ok"))
                ToolDef{parameterSchema = parameterSchemaValue} = tool
            (parameterSchemaValue >>= lookupField "additionalProperties") `shouldBe` Just (Bool True)

        it "applies the same normalization to typed tool argument schemas" $ do
            let tool = defineToolWithArgument @RecordWithMaybe "lookup" "lookup tool" (\_ -> pure (Right "ok"))
                ToolDef{parameterSchema = parameterSchemaValue} = tool
            sort (maybe [] id (parameterSchemaValue >>= requiredFieldNames))
                `shouldBe` sort ["response", "translated_response", "toolCall"]
            (parameterSchemaValue >>= lookupPath ["properties", "toolCall"])
                `shouldBe` Just (nullableSchema exampleToolCallSchema)

    describe "HistoryItem JSON" $ do
        it "encodes completed lifecycle for local items without a schemaVersion envelope" $ do
            let encoded = toJSON (user "hello")
            lookupField "schemaVersion" encoded `shouldBe` Nothing
            lookupField "itemLifecycle" encoded `shouldBe` Just (toJSON ItemCompleted)
            fromJSON encoded `shouldBe` Success (user "hello")

        it "round-trips embedded history item ids" $ do
            let itemId = historyItemIdAt 7
                item = setHistoryItemId (Just itemId) (user "hello")
                encoded = toJSON item
            lookupField "historyItemIdField" encoded `shouldBe` Just (toJSON itemId)
            fromJSON encoded `shouldBe` Success item

        it "round-trips non-portable provider-backed items" $ do
            let item =
                    nonPortableHistoryItem
                        ItemPending
                        ProviderItem
                            { apiFamily = ProviderGeminiInteractions
                            , exchangeId = Nothing
                            , nativeItemId = Just "native-thought"
                            , payload = object ["type" .= ("thought" :: Text)]
                            , availableLocalTools = []
                            }
            fromJSON (toJSON item) `shouldBe` Success item

        it "round-trips provider item available local tools" $ do
            let item =
                    nonPortableHistoryItem
                        ItemPending
                        ProviderItem
                            { apiFamily = ProviderOpenAIResponses
                            , exchangeId = Just "response-openai"
                            , nativeItemId = Just "native-tool-call"
                            , payload = object ["type" .= ("function_call" :: Text)]
                            , availableLocalTools =
                                [ ToolDeclaration
                                    { name = "lookup"
                                    , description = "lookup tool"
                                    , parameterSchema = Just (object ["type" .= ("object" :: Text)])
                                    }
                                ]
                            }
            fromJSON (toJSON item) `shouldBe` Success item

        it "round-trips tool calls with continuation attachments" $ do
            let item =
                    toolCallWithContinuations
                        "tool-call-1"
                        "lookup"
                        (fromList [("name", String "Ada")])
                        [ ToolCallContinuation
                            { continuationProviderFamily = ProviderGeminiInteractions
                            , continuationPayload = object ["type" .= ("thought" :: Text), "signature" .= ("thought-1" :: Text)]
                            }
                        ]
            fromJSON (toJSON item) `shouldBe` Success item

        it "round-trips reset control envelopes" $ do
            let item = resetTo (historyItemIdAt 2)
            fromJSON (toJSON item) `shouldBe` Success item

        it "round-trips replay barrier control envelopes" $ do
            let item =
                    HistoryItem
                        { historyItemIdField = Nothing
                        , itemLifecycle = ItemCompleted
                        , genericItem = GenericReplayBarrier{reason = "Responses response status was failed"}
                        , providerItem = Nothing
                        }
            fromJSON (toJSON item) `shouldBe` Success item

        it "round-trips reset-to-start control envelopes" $ do
            let item = resetToStart
            fromJSON (toJSON item) `shouldBe` Success item

        it "round-trips multipart text messages" $ do
            let item = userParts [textPart "hello", textPart " world"]
            fromJSON (toJSON item) `shouldBe` Success item

        it "round-trips refusal and media message parts" $ do
            let item =
                    assistantParts
                        [ refusalPart "I can't help with that"
                        , imagePart "blob-image-1" (Just "image/png") (Just "diagram")
                        , audioPart "blob-audio-1" (Just "audio/mpeg") (Just "spoken note")
                        , filePart "blob-file-1" Nothing (Just "notes.txt")
                        ]
            fromJSON (toJSON item) `shouldBe` Success item

        it "round-trips JSON tool responses" $ do
            forM_
                ( [ String "ok"
                  , Number 123
                  , Bool True
                  , Null
                  , toJSON ([1 :: Int, 2 :: Int] :: [Int])
                  , object ["answer" .= (4 :: Int)]
                  ]
                    :: [Value]
                )
                $ \jsonValue -> do
                    let item = toolResultJson "tool-call-1" jsonValue
                    fromJSON (toJSON item) `shouldBe` Success item
  where
    lookupField :: Text -> Value -> Maybe Value
    lookupField fieldName = \case
        Object objectValue ->
            KM.lookup (Key.fromText fieldName) objectValue
        _ ->
            Nothing

    lookupPath :: [Text] -> Value -> Maybe Value
    lookupPath [] currentValue = Just currentValue
    lookupPath (fieldName : rest) currentValue = case currentValue of
        Object currentObject ->
            KM.lookup (Key.fromText fieldName) currentObject >>= lookupPath rest
        _ ->
            Nothing

    schemaKeywordPaths :: Text -> Value -> [Text]
    schemaKeywordPaths keyword = go "$"
      where
        go currentPath = \case
            Object objectValue ->
                let keywordMatches =
                        [ currentPath <> "." <> keyword
                        | KM.member (Key.fromText keyword) objectValue
                        ]
                    nestedMatches =
                        concatMap
                            (uncurry (goObjectField currentPath))
                            (KM.toList objectValue)
                 in keywordMatches <> nestedMatches
            Array values ->
                concatMap (go (currentPath <> "[]")) (toList values)
            _ ->
                []

        goObjectField currentPath fieldName fieldValue =
            go (currentPath <> "." <> Key.toText fieldName) fieldValue

    requiredFieldNames :: Value -> Maybe [Text]
    requiredFieldNames schemaValue = do
        Array requiredFields <- lookupField "required" schemaValue
        traverse requiredFieldName (toList requiredFields)

    requiredFieldName :: Value -> Maybe Text
    requiredFieldName = \case
        String fieldName ->
            Just fieldName
        _ ->
            Nothing

    nullableSchema :: Value -> Value
    nullableSchema schemaValue =
        object
            [ "anyOf" .= ([schemaValue, nullSchema] :: [Value])
            ]

    nullSchema :: Value
    nullSchema =
        object
            [ "type" .= ("null" :: Text)
            ]

    historyItemIdAt :: Word32 -> HistoryItemId
    historyItemIdAt suffix =
        HistoryItemId (UUID.fromWords 0 0 0 suffix)

    exampleToolCallSchema :: Value
    exampleToolCallSchema =
        object
            [ "type" .= ("object" :: Text)
            , "properties"
                .= object
                    [ "tool" .= object ["type" .= ("string" :: Text)]
                    ]
            , "required" .= (["tool" :: Text] :: [Text])
            , "additionalProperties" .= False
            ]

    nestedChildSchema :: Value
    nestedChildSchema =
        object
            [ "type" .= ("object" :: Text)
            , "properties"
                .= object
                    [ "maybeText" .= nullableSchema (object ["type" .= ("string" :: Text)])
                    ]
            , "required" .= (["maybeText" :: Text] :: [Text])
            , "additionalProperties" .= False
            ]
