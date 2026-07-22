module Rake.Providers.Chat.Projection
    ( classifyResponsesPayload
    , classifyGeminiPayload
    , classifyGeminiPayloads
    ) where

import Data.Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.Map qualified as Map
import Data.Text qualified as T
import Data.Vector qualified as Vector
import Rake.Types
import Relude

classifyResponsesPayload :: ProviderApiFamily -> Maybe Text -> Value -> ([Text], GenericItem, [MediaProviderReference])
classifyResponsesPayload providerFamily exchangeId payload =
    case payload of
        Object itemObject ->
            case (lookupText "type" itemObject, lookupText "role" itemObject) of
                (Just "function_call", _) ->
                    case parseResponsesToolCall itemObject of
                        Nothing ->
                            (["Dropped malformed function_call item during canonical projection"], GenericNonPortable, [])
                        Just genericToolCall' ->
                            ([], GenericToolCall{toolCall = genericToolCall'}, [])
                (Just "function_call_output", _) ->
                    case parseResponsesToolResult itemObject of
                        Nothing ->
                            (["Dropped malformed function_call_output item during canonical projection"], GenericNonPortable, [])
                        Just genericToolResult' ->
                            ([], GenericToolResult{toolResult = genericToolResult'}, [])
                (Just "message", _) ->
                    responsesMessageObjectToGeneric providerFamily exchangeId itemObject
                (Nothing, Just _) ->
                    responsesMessageObjectToGeneric providerFamily exchangeId itemObject
                (Just unsupportedType, _) ->
                    (["Persisted unused response item type: " <> unsupportedType], GenericNonPortable, [])
                _ ->
                    (["Persisted malformed unused response item"], GenericNonPortable, [])
        _ ->
            (["Persisted non-object unused response item"], GenericNonPortable, [])

responsesMessageObjectToGeneric :: ProviderApiFamily -> Maybe Text -> Object -> ([Text], GenericItem, [MediaProviderReference])
responsesMessageObjectToGeneric providerFamily exchangeId itemObject =
    case roleTextToGenericRole =<< lookupText "role" itemObject of
        Nothing ->
            (["Persisted unused response message item with unsupported role"], GenericNonPortable, [])
        Just role ->
            case extractResponsesContentParts providerFamily exchangeId (lookupText "id" itemObject) (KM.lookup "content" itemObject) of
                (notes, [], mediaReferences) ->
                    (notes <> ["Persisted unused response message item without supported content parts"], GenericNonPortable, mediaReferences)
                (notes, parts, mediaReferences) ->
                    (notes, GenericMessage{role, parts}, mediaReferences)

extractResponsesContentParts :: ProviderApiFamily -> Maybe Text -> Maybe Text -> Maybe Value -> ([Text], [MessagePart], [MediaProviderReference])
extractResponsesContentParts providerFamily exchangeId nativeItemId = \case
    Nothing ->
        (["Missing content field"], [], [])
    Just (String content) ->
        ([], [PartText content], [])
    Just (Array contentParts) ->
        foldMap partToGeneric (zip [0 ..] (Vector.toList contentParts))
    Just _ ->
        (["Unsupported message content format"], [], [])
  where
    partToGeneric :: (Int, Value) -> ([Text], [MessagePart], [MediaProviderReference])
    partToGeneric (partIndex, partValue) = case partValue of
        String content ->
            ([], [PartText content], [])
        Object partObject ->
            responsesMessagePartObjectToGeneric providerFamily exchangeId nativeItemId partIndex partObject
        _ ->
            (["Dropped unsupported message part"], [], [])

responsesMessagePartObjectToGeneric :: ProviderApiFamily -> Maybe Text -> Maybe Text -> Int -> Object -> ([Text], [MessagePart], [MediaProviderReference])
responsesMessagePartObjectToGeneric providerFamily exchangeId nativeItemId partIndex partObject =
    case lookupText "type" partObject of
        Just "input_text" ->
            parseTextPart
        Just "output_text" ->
            parseTextPart
        Just "text" ->
            parseTextPart
        Just "refusal" ->
            parseRefusalPart
        Just "input_image" ->
            parseImagePart
        Just "input_audio" ->
            parseAudioPart "input_audio"
        Just "output_audio" ->
            parseAudioPart "output_audio"
        Just "input_file" ->
            parseFilePart
        Just unsupportedType ->
            (["Dropped unsupported message part type: " <> unsupportedType], [], [])
        Nothing ->
            case lookupText "text" partObject of
                Just text ->
                    ([], [PartText text], [])
                Nothing ->
                    case lookupText "refusal" partObject of
                        Just refusal ->
                            ([], [PartRefusal refusal], [])
                        Nothing ->
                            case lookupText "image_url" partObject of
                                Just _imageUrl ->
                                    parseImagePart
                                Nothing ->
                                    case lookupText "file_id" partObject <|> lookupText "filename" partObject of
                                        Just _fileMarker ->
                                            parseFilePart
                                        Nothing ->
                                            (["Dropped unsupported message part"], [], [])
  where
    parseTextPart =
        case lookupText "text" partObject of
            Just text ->
                ([], [PartText text], [])
            Nothing ->
                (["Dropped malformed text message part"], [], [])

    parseRefusalPart =
        case lookupText "refusal" partObject <|> lookupText "text" partObject of
            Just text ->
                ([], [PartRefusal text], [])
            Nothing ->
                (["Dropped malformed refusal message part"], [], [])

    parseImagePart =
        case mediaBlobId providerFamily exchangeId nativeItemId partIndex of
            Just blobId ->
                ( [], [PartImage{blobId, mimeType = Nothing, altText = Nothing}]
                , [mediaProviderReference blobId providerFamily (canonicalizeResponsesMediaPart "input_image" partObject)]
                )
            Nothing ->
                (["Dropped image message part because the containing response item was missing an id"], [], [])

    parseAudioPart partType =
        case mediaBlobId providerFamily exchangeId nativeItemId partIndex of
            Just blobId ->
                ( [], [PartAudio{blobId, mimeType = Nothing, transcript = lookupText "transcript" partObject}]
                , [mediaProviderReference blobId providerFamily (canonicalizeResponsesMediaPart partType partObject)]
                )
            Nothing ->
                (["Dropped audio message part because the containing response item was missing an id"], [], [])

    parseFilePart =
        case mediaBlobId providerFamily exchangeId nativeItemId partIndex of
            Just blobId ->
                ( [], [PartFile{blobId, mimeType = Nothing, fileName = lookupText "filename" partObject}]
                , [mediaProviderReference blobId providerFamily (canonicalizeResponsesMediaPart "input_file" partObject)]
                )
            Nothing ->
                (["Dropped file message part because the containing response item was missing an id"], [], [])

mediaProviderReference :: MediaBlobId -> ProviderApiFamily -> Value -> MediaProviderReference
mediaProviderReference blobRef providerFamily providerRequestPart =
    MediaProviderReference{mediaBlobId = blobRef, providerFamily, providerRequestPart}

canonicalizeResponsesMediaPart :: Text -> Object -> Value
canonicalizeResponsesMediaPart partType partObject =
    Object (KM.insert (Key.fromText "type") (String partType) partObject)

mediaBlobId :: ProviderApiFamily -> Maybe Text -> Maybe Text -> Int -> Maybe MediaBlobId
mediaBlobId providerFamily exchangeId nativeItemId partIndex = do
    itemId <- nativeItemId
    pure $
        MediaBlobId
            ( maybe
                (providerApiFamilyText providerFamily)
                (\responseId -> providerApiFamilyText providerFamily <> "-" <> responseId)
                exchangeId
                <> "-"
                <> itemId
                <> "-"
                <> T.pack (show partIndex)
            )

parseResponsesToolCall :: Object -> Maybe ToolCall
parseResponsesToolCall itemObject = do
    toolCallId <- ToolCallId <$> lookupText "call_id" itemObject
    toolName <- lookupText "name" itemObject
    toolArgs <- parseToolArgs =<< KM.lookup "arguments" itemObject
    pure ToolCall{toolCallId, toolName, toolArgs, continuationAttachments = []}

parseResponsesToolResult :: Object -> Maybe ToolResult
parseResponsesToolResult itemObject = do
    toolCallId <- ToolCallId <$> lookupText "call_id" itemObject
    outputValue <- KM.lookup "output" itemObject
    pure ToolResult{toolCallId, toolResponse = parseToolResponse outputValue}

classifyGeminiPayload :: Value -> ([Text], GenericItem)
classifyGeminiPayload = \case
    Object itemObject ->
        case lookupText "type" itemObject of
            Just "model_output" ->
                geminiModelOutputToGeneric itemObject
            Just "text" ->
                case lookupText "text" itemObject of
                    Just text ->
                        ([], GenericMessage{role = GenericAssistant, parts = [PartText text]})
                    Nothing ->
                        ( ["Dropped malformed Gemini text content during canonical projection"]
                        , GenericNonPortable
                        )
            Just "function_call" ->
                case parseGeminiToolCall itemObject of
                    Just genericToolCall' ->
                        ([], GenericToolCall{toolCall = genericToolCall'})
                    Nothing ->
                        ( ["Dropped malformed Gemini function_call content during canonical projection"]
                        , GenericNonPortable
                        )
            Just "function_result" ->
                case parseGeminiToolResult itemObject of
                    Just genericToolResult' ->
                        ([], GenericToolResult{toolResult = genericToolResult'})
                    Nothing ->
                        ( ["Dropped malformed Gemini function_result content during canonical projection"]
                        , GenericNonPortable
                        )
            Just "thought" ->
                ([], GenericNonPortable)
            Just unsupportedType ->
                (["Persisted unused Gemini content type: " <> unsupportedType], GenericNonPortable)
            Nothing ->
                case lookupText "text" itemObject of
                    Just text ->
                        ([], GenericMessage{role = GenericAssistant, parts = [PartText text]})
                    Nothing ->
                        (["Persisted malformed unused Gemini item"], GenericNonPortable)
    _ ->
        (["Persisted non-object unused Gemini item"], GenericNonPortable)

geminiModelOutputToGeneric :: Object -> ([Text], GenericItem)
geminiModelOutputToGeneric itemObject =
    case extractGeminiModelOutputParts (KM.lookup "content" itemObject) of
        (notes, []) ->
            ( notes <> ["Persisted unused Gemini model_output item without supported content parts"]
            , GenericNonPortable
            )
        (notes, parts) ->
            (notes, GenericMessage{role = GenericAssistant, parts})

extractGeminiModelOutputParts :: Maybe Value -> ([Text], [MessagePart])
extractGeminiModelOutputParts = \case
    Nothing ->
        (["Gemini model_output item was missing content"], [])
    Just (String text) ->
        ([], [PartText text])
    Just (Array contentParts) ->
        foldMap geminiModelOutputPart (Vector.toList contentParts)
    Just _ ->
        (["Gemini model_output content was neither text nor an array"], [])

geminiModelOutputPart :: Value -> ([Text], [MessagePart])
geminiModelOutputPart = \case
    Object partObject ->
        case lookupText "type" partObject of
            Just "text" ->
                case lookupText "text" partObject of
                    Just text ->
                        ([], [PartText text])
                    Nothing ->
                        (["Dropped malformed Gemini text content during canonical projection"], [])
            Just "refusal" ->
                case lookupText "refusal" partObject <|> lookupText "text" partObject of
                    Just text ->
                        ([], [PartRefusal text])
                    Nothing ->
                        (["Dropped malformed Gemini refusal content during canonical projection"], [])
            Just unsupportedType ->
                (["Persisted unused Gemini model_output content type: " <> unsupportedType], [])
            Nothing ->
                case lookupText "text" partObject of
                    Just text ->
                        ([], [PartText text])
                    Nothing ->
                        (["Persisted malformed unused Gemini model_output content"], [])
    _ ->
        (["Persisted non-object unused Gemini model_output content"], [])

classifyGeminiPayloads :: [Value] -> [(Value, [Text], GenericItem)]
classifyGeminiPayloads =
    go []
  where
    go pendingThoughts [] =
        map (\payload -> (payload, [], GenericNonPortable)) pendingThoughts
    go pendingThoughts (payload : remainingPayloads) =
        case payload of
            Object payloadObject
                | lookupText "type" payloadObject == Just "thought" ->
                    go (pendingThoughts <> [payload]) remainingPayloads
                | Just toolCallValue <- parseGeminiToolCallWithContinuations pendingThoughts payloadObject ->
                    (payload, [], GenericToolCall{toolCall = toolCallValue}) : go [] remainingPayloads
            _ ->
                flushPendingThoughts <> [(payload, notes, classifiedItem)] <> go [] remainingPayloads
              where
                flushPendingThoughts =
                    map (\thoughtPayload -> (thoughtPayload, [], GenericNonPortable)) pendingThoughts
                (notes, classifiedItem) =
                    classifyGeminiPayload payload

parseGeminiToolCallWithContinuations :: [Value] -> Object -> Maybe ToolCall
parseGeminiToolCallWithContinuations pendingThoughtPayloads itemObject = do
    toolCallValue@ToolCall{continuationAttachments = existingAttachments} <- parseGeminiToolCall itemObject
    pure
        toolCallValue
            { continuationAttachments =
                existingAttachments
                    <> map geminiThoughtContinuation pendingThoughtPayloads
            }
  where
    geminiThoughtContinuation payload =
        ToolCallContinuation
            { continuationProviderFamily = ProviderGeminiInteractions
            , continuationPayload = payload
            }

parseGeminiToolCall :: Object -> Maybe ToolCall
parseGeminiToolCall itemObject = do
    toolCallId <-
        ToolCallId
            <$> ( lookupText "id" itemObject
                    <|> lookupText "call_id" itemObject
                )
    toolName <- lookupText "name" itemObject
    toolArgs <- parseToolArgs =<< KM.lookup "arguments" itemObject
    pure ToolCall{toolCallId, toolName, toolArgs, continuationAttachments = []}

parseGeminiToolResult :: Object -> Maybe ToolResult
parseGeminiToolResult itemObject = do
    toolCallId <-
        ToolCallId
            <$> ( lookupText "call_id" itemObject
                    <|> lookupText "id" itemObject
                )
    resultValue <- KM.lookup "result" itemObject
    pure ToolResult{toolCallId, toolResponse = parseGeminiToolResponse resultValue}

parseGeminiToolResponse :: Value -> ToolResponse
parseGeminiToolResponse = \case
    Array contentParts ->
        case traverse geminiTextContent (Vector.toList contentParts) of
            Just texts ->
                parseToolResponse (String (mconcat texts))
            Nothing ->
                ToolResponseJson (Array contentParts)
    other ->
        parseToolResponse other
  where
    geminiTextContent = \case
        Object partObject ->
            case lookupText "type" partObject of
                Just "text" ->
                    lookupText "text" partObject
                Nothing ->
                    lookupText "text" partObject
                _ ->
                    Nothing
        _ ->
            Nothing

parseToolResponse :: Value -> ToolResponse
parseToolResponse = \case
    String text ->
        fromMaybe
            (ToolResponseText text)
            (ToolResponseJson <$> decodeStrictText text)
    other ->
        ToolResponseJson other

parseToolArgs :: Value -> Maybe (Map Text Value)
parseToolArgs = \case
    Object arguments ->
        Just (Map.fromList [(Key.toText key, value) | (key, value) <- KM.toList arguments])
    String argumentsText ->
        case decodeStrictText argumentsText of
            Just (Object arguments) ->
                Just (Map.fromList [(Key.toText key, value) | (key, value) <- KM.toList arguments])
            _ ->
                Nothing
    _ ->
        Nothing

lookupText :: Text -> Object -> Maybe Text
lookupText fieldName obj =
    KM.lookup (Key.fromText fieldName) obj >>= \case
        String text ->
            Just text
        _ ->
            Nothing

roleTextToGenericRole :: Text -> Maybe GenericRole
roleTextToGenericRole = \case
    "system" ->
        Just GenericSystem
    "developer" ->
        Just GenericSystem
    "user" ->
        Just GenericUser
    "assistant" ->
        Just GenericAssistant
    _ ->
        Nothing
