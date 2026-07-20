module Rake.Providers.Gemini.Chat
    ( GeminiChatSettings (..)
    , defaultGeminiChatSettings
    , GeminiGenerationConfig (..)
    , defaultGeminiGenerationConfig
    , GeminiThinkingLevel (..)
    , GeminiThinkingSummaries (..)
    , GeminiToolChoice (..)
    , GeminiAllowedTools (..)
    , GeminiToolChoiceMode (..)
    , GeminiSearchType (..)
    , GeminiTool (..)
    , GeminiComputerUseConfig (..)
    , GeminiComputerUseEnvironment (..)
    , GeminiMcpServerConfig (..)
    , GeminiFileSearchConfig (..)
    , GeminiGoogleMapsConfig (..)
    , decodeGeminiResponse
    , runRakeGeminiChat
    ) where

import Data.Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString qualified as BS
import Data.IORef qualified as IORef
import Data.Map qualified as Map
import Data.Text.Encoding qualified as TextEncoding
import Data.Vector qualified as Vector
import Effectful
import Effectful.Error.Static
import Network.HTTP.Client qualified as HttpClient
import Network.HTTP.Client.TLS (newTlsManagerWith, tlsManagerSettings)
import Rake.Effect
import Rake.Internal.Sse
import Rake.MediaStorage.Effect
import Rake.Providers.Chat.Projection (classifyGeminiPayloads)
import Rake.Providers.Internal
    ( defaultWarningLogger
    , protectStreamingInternalAction
    , runChatProvider
    , runStreamingSseRequest
    , valueToCompactText
    )
import Rake.Types
import Relude
import Servant.API (Header, JSON, Post, ReqBody)
import Servant.API qualified as Servant
import Servant.Client

data GeminiChatSettings es = GeminiChatSettings
    { apiKey :: Text
    , model :: Text
    , baseUrl :: Text
    , systemInstruction :: Maybe Text
    , providerTools :: [GeminiTool]
    , generationConfig :: GeminiGenerationConfig
    , requestLogger :: NativeMsgFormat -> Eff es ()
    }

defaultGeminiChatSettings :: Text -> GeminiChatSettings es
defaultGeminiChatSettings apiKey =
    GeminiChatSettings
        { apiKey
        , model = "gemini-3-flash-preview"
        , baseUrl = "https://generativelanguage.googleapis.com"
        , systemInstruction = Nothing
        , providerTools = []
        , generationConfig = defaultGeminiGenerationConfig
        , requestLogger = defaultWarningLogger "gemini.chat"
        }

data GeminiGenerationConfig = GeminiGenerationConfig
    { temperature :: Maybe Double
    , topP :: Maybe Double
    , seed :: Maybe Int
    , stopSequences :: [Text]
    , thinkingLevel :: Maybe GeminiThinkingLevel
    , thinkingSummaries :: Maybe GeminiThinkingSummaries
    , maxOutputTokens :: Maybe Int
    , toolChoice :: Maybe GeminiToolChoice
    }
    deriving stock (Show, Eq, Generic)

defaultGeminiGenerationConfig :: GeminiGenerationConfig
defaultGeminiGenerationConfig =
    GeminiGenerationConfig
        { temperature = Nothing
        , topP = Nothing
        , seed = Nothing
        , stopSequences = []
        , thinkingLevel = Nothing
        , thinkingSummaries = Nothing
        , maxOutputTokens = Nothing
        , toolChoice = Nothing
        }

data GeminiThinkingLevel
    = GeminiThinkingMinimal
    | GeminiThinkingLow
    | GeminiThinkingMedium
    | GeminiThinkingHigh
    deriving stock (Show, Eq, Generic)

data GeminiThinkingSummaries
    = GeminiThinkingSummariesAuto
    | GeminiThinkingSummariesNone
    deriving stock (Show, Eq, Generic)

data GeminiToolChoiceMode
    = GeminiToolChoiceAuto
    | GeminiToolChoiceAny
    | GeminiToolChoiceNone
    | GeminiToolChoiceValidated
    deriving stock (Show, Eq, Generic)

data GeminiAllowedTools = GeminiAllowedTools
    { mode :: Maybe GeminiToolChoiceMode
    , tools :: [Text]
    }
    deriving stock (Show, Eq, Generic)

data GeminiToolChoice
    = GeminiToolChoiceModeOnly GeminiToolChoiceMode
    | GeminiToolChoiceAllowedTools GeminiAllowedTools
    deriving stock (Show, Eq, Generic)

data GeminiSearchType
    = GeminiWebSearch
    | GeminiImageSearch
    deriving stock (Show, Eq, Generic)

data GeminiComputerUseEnvironment
    = GeminiComputerUseBrowser
    deriving stock (Show, Eq, Generic)

data GeminiComputerUseConfig = GeminiComputerUseConfig
    { environment :: Maybe GeminiComputerUseEnvironment
    , excludedPredefinedFunctions :: [Text]
    }
    deriving stock (Show, Eq, Generic)

data GeminiMcpServerConfig = GeminiMcpServerConfig
    { name :: Maybe Text
    , url :: Maybe Text
    , headers :: Maybe Value
    , allowedTools :: Maybe GeminiAllowedTools
    }
    deriving stock (Show, Eq, Generic)

data GeminiFileSearchConfig = GeminiFileSearchConfig
    { fileSearchStoreNames :: [Text]
    , topK :: Maybe Int
    , metadataFilter :: Maybe Text
    }
    deriving stock (Show, Eq, Generic)

data GeminiGoogleMapsConfig = GeminiGoogleMapsConfig
    { enableWidget :: Maybe Bool
    , latitude :: Maybe Double
    , longitude :: Maybe Double
    }
    deriving stock (Show, Eq, Generic)

data GeminiTool
    = GeminiGoogleSearchTool [GeminiSearchType]
    | GeminiCodeExecutionTool
    | GeminiUrlContextTool
    | GeminiComputerUseTool GeminiComputerUseConfig
    | GeminiMcpServerTool GeminiMcpServerConfig
    | GeminiFileSearchTool GeminiFileSearchConfig
    | GeminiGoogleMapsTool GeminiGoogleMapsConfig
    deriving stock (Show, Eq, Generic)

instance ToJSON GeminiThinkingLevel where
    toJSON = String . \case
        GeminiThinkingMinimal -> "minimal"
        GeminiThinkingLow -> "low"
        GeminiThinkingMedium -> "medium"
        GeminiThinkingHigh -> "high"

instance ToJSON GeminiThinkingSummaries where
    toJSON = String . \case
        GeminiThinkingSummariesAuto -> "auto"
        GeminiThinkingSummariesNone -> "none"

instance ToJSON GeminiToolChoiceMode where
    toJSON = String . \case
        GeminiToolChoiceAuto -> "auto"
        GeminiToolChoiceAny -> "any"
        GeminiToolChoiceNone -> "none"
        GeminiToolChoiceValidated -> "validated"

instance ToJSON GeminiAllowedTools where
    toJSON GeminiAllowedTools{mode, tools} =
        object $
            catMaybes
                [ ("mode" .=) <$> mode
                , Just ("tools" .= tools)
                ]

instance ToJSON GeminiToolChoice where
    toJSON = \case
        GeminiToolChoiceModeOnly mode ->
            toJSON mode
        GeminiToolChoiceAllowedTools allowedTools ->
            object ["allowed_tools" .= allowedTools]

instance ToJSON GeminiSearchType where
    toJSON = String . \case
        GeminiWebSearch -> "web_search"
        GeminiImageSearch -> "image_search"

instance ToJSON GeminiComputerUseEnvironment where
    toJSON = String . \case
        GeminiComputerUseBrowser -> "browser"

instance ToJSON GeminiTool where
    toJSON = \case
        GeminiGoogleSearchTool searchTypes ->
            object $
                ["type" .= ("google_search" :: Text)]
                    <> [ "search_types" .= searchTypes
                       | not (null searchTypes)
                       ]
        GeminiCodeExecutionTool ->
            object ["type" .= ("code_execution" :: Text)]
        GeminiUrlContextTool ->
            object ["type" .= ("url_context" :: Text)]
        GeminiComputerUseTool GeminiComputerUseConfig{environment, excludedPredefinedFunctions} ->
            object $
                [ "type" .= ("computer_use" :: Text)
                ]
                    <> catMaybes
                        [ ("environment" .=) <$> environment
                        , if null excludedPredefinedFunctions
                            then Nothing
                            else Just ("excludedPredefinedFunctions" .= excludedPredefinedFunctions)
                        ]
        GeminiMcpServerTool GeminiMcpServerConfig{name, url, headers, allowedTools} ->
            object $
                [ "type" .= ("mcp_server" :: Text)
                ]
                    <> catMaybes
                        [ ("name" .=) <$> name
                        , ("url" .=) <$> url
                        , ("headers" .=) <$> headers
                        , ("allowed_tools" .=) <$> allowedTools
                        ]
        GeminiFileSearchTool GeminiFileSearchConfig{fileSearchStoreNames, topK, metadataFilter} ->
            object $
                [ "type" .= ("file_search" :: Text)
                ]
                    <> catMaybes
                        [ if null fileSearchStoreNames
                            then Nothing
                            else Just ("file_search_store_names" .= fileSearchStoreNames)
                        , ("top_k" .=) <$> topK
                        , ("metadata_filter" .=) <$> metadataFilter
                        ]
        GeminiGoogleMapsTool GeminiGoogleMapsConfig{enableWidget, latitude, longitude} ->
            object $
                [ "type" .= ("google_maps" :: Text)
                ]
                    <> catMaybes
                        [ ("enable_widget" .=) <$> enableWidget
                        , ("latitude" .=) <$> latitude
                        , ("longitude" .=) <$> longitude
                        ]

type GeminiInteractionsAPI =
    "v1beta"
        Servant.:> "interactions"
        Servant.:> Header "x-goog-api-key" Text
        Servant.:> ReqBody '[JSON] Value
        Servant.:> Post '[JSON] Value

geminiInteractionsApi :: Proxy GeminiInteractionsAPI
geminiInteractionsApi = Proxy

runRakeGeminiChat
    :: forall es a
     . ( IOE :> es
       , Error RakeError :> es
       , RakeMediaStorage :> es
       )
    => GeminiChatSettings es
    -> Eff (Rake ': es) a
    -> Eff es a
runRakeGeminiChat settings@GeminiChatSettings{apiKey, baseUrl, requestLogger} eff = do
    manager <-
        liftIO $
            newTlsManagerWith
                tlsManagerSettings{HttpClient.managerResponseTimeout = HttpClient.responseTimeoutNone}
    parsedBaseUrl <- either (throwError . invalidBaseUrl) pure $ parseBaseUrl (toString baseUrl)
    let clientEnv = mkClientEnv manager parsedBaseUrl
        postInteraction = client geminiInteractionsApi

    runChatProvider
        (\tools responseFormat samplingOptions history -> do
            requestBody <- buildGeminiRequestBody settings tools responseFormat samplingOptions history
            requestLogger (NativeMsgOut requestBody)
            responseValue <-
                liftIO
                    (runClientM (postInteraction (Just apiKey) requestBody) clientEnv)
                    >>= \case
                        Left err -> do
                            requestLogger (NativeRequestFailure err)
                            throwError (LlmClientError err)
                        Right response ->
                            pure response
            requestLogger (NativeMsgIn responseValue)
            either throwError pure (decodeGeminiResponse responseValue)
        )
        (\streamCallbacks tools responseFormat samplingOptions history -> do
            requestBody <- buildGeminiRequestBody settings tools responseFormat samplingOptions history
            let streamingRequestBody = enableStreamingRequestBody requestBody
            requestLogger (NativeMsgOut streamingRequestBody)
            streamingRequest <- liftIO (buildGeminiStreamingRequest baseUrl apiKey streamingRequestBody)
            streamStateRef <- liftIO (IORef.newIORef initialGeminiStreamState)
            maybeFinalResponseValue <-
                runStreamingSseRequest
                    parsedBaseUrl
                    manager
                    streamingRequest
                    (\clientErr -> requestLogger (NativeRequestFailure clientErr))
                    (handleGeminiStreamEvent requestLogger streamCallbacks streamStateRef)
            finalResponseValue <-
                maybe
                    (throwError (LlmExpectationError "Gemini stream ended without a terminal interaction event"))
                    pure
                    maybeFinalResponseValue
            either throwError pure (decodeGeminiResponse finalResponseValue)
        )
        eff
  where
    invalidBaseUrl err =
        LlmExpectationError ("Invalid base URL: " <> show err)

buildGeminiStreamingRequest :: Text -> Text -> Value -> IO HttpClient.Request
buildGeminiStreamingRequest baseUrl apiKey requestBody = do
    request <- HttpClient.parseRequest (toString baseUrl <> "/v1beta/interactions?alt=sse")
    pure
        request
            { HttpClient.method = "POST"
            , HttpClient.requestHeaders =
                [ ("x-goog-api-key", TextEncoding.encodeUtf8 apiKey)
                , ("Content-Type", "application/json")
                , ("Accept", "text/event-stream")
                ]
            , HttpClient.requestBody = HttpClient.RequestBodyLBS (encode requestBody)
            , HttpClient.responseTimeout = HttpClient.responseTimeoutNone
            }

enableStreamingRequestBody :: Value -> Value
enableStreamingRequestBody = \case
    Object requestObject ->
        Object (KM.insert "stream" (Bool True) requestObject)
    otherValue ->
        otherValue

handleGeminiStreamEvent
    :: ( IOE :> es
       , Error RakeError :> es
       )
    => (NativeMsgFormat -> Eff es ())
    -> StreamCallbacks es
    -> IORef.IORef GeminiStreamState
    -> Maybe Text
    -> BS.ByteString
    -> Eff es (SseStep Value)
handleGeminiStreamEvent requestLogger streamCallbacks streamStateRef maybeEventName payload
    | payload == "[DONE]" =
        pure SseStop
    | otherwise =
        case eitherDecodeStrict' payload of
            Left err ->
                throwError
                    ( LlmExpectationError
                        ( "Gemini stream event was not valid JSON: "
                            <> err
                        )
                    )
            Right eventValue -> do
                protectStreamingInternalAction
                    (RequestLoggerFailed . ("gemini: " <>))
                    (requestLogger (NativeMsgIn eventValue))
                currentStreamState <- liftIO (IORef.readIORef streamStateRef)
                (nextStreamState, maybeTerminalInteraction) <-
                    either
                        throwError
                        pure
                        (applyGeminiStreamEvent maybeEventName currentStreamState eventValue)
                liftIO (IORef.writeIORef streamStateRef nextStreamState)
                emitGeminiStreamDelta streamCallbacks nextStreamState maybeEventName eventValue
                pure (maybe SseContinue SseFinish maybeTerminalInteraction)

data GeminiStreamState = GeminiStreamState
    { createdInteraction :: Maybe Object
    , streamSteps :: Map Int GeminiStreamStep
    }

data GeminiStreamStep = GeminiStreamStep
    { startedStep :: Object
    , stepDeltasRev :: [Object]
    , stepStopped :: Bool
    }

initialGeminiStreamState :: GeminiStreamState
initialGeminiStreamState =
    GeminiStreamState
        { createdInteraction = Nothing
        , streamSteps = mempty
        }

applyGeminiStreamEvent
    :: Maybe Text
    -> GeminiStreamState
    -> Value
    -> Either RakeError (GeminiStreamState, Maybe Value)
applyGeminiStreamEvent maybeEventName streamState@GeminiStreamState{streamSteps} eventValue =
    case eventValue of
        Object eventObject ->
            case geminiStreamEventType maybeEventName eventObject of
                Just "interaction.created" ->
                    case KM.lookup "interaction" eventObject of
                        Just (Object interactionObject) ->
                            Right (streamState{createdInteraction = Just interactionObject}, Nothing)
                        _ ->
                            Left (malformedGeminiStreamEvent "interaction.created was missing an interaction object")
                Just "step.start" -> do
                    stepIndex <- geminiStreamEventIndex "step.start" eventObject
                    stepObject <- geminiStreamEventObject "step.start.step" "step" eventObject
                    let streamStep =
                            GeminiStreamStep
                                { startedStep = stepObject
                                , stepDeltasRev = []
                                , stepStopped = False
                                }
                    Right (streamState{streamSteps = Map.insert stepIndex streamStep streamSteps}, Nothing)
                Just "step.delta" -> do
                    stepIndex <- geminiStreamEventIndex "step.delta" eventObject
                    deltaObject <- geminiStreamEventObject "step.delta.delta" "delta" eventObject
                    streamStep <-
                        maybe
                            (Left (malformedGeminiStreamEvent "step.delta referred to an unknown step index"))
                            Right
                            (Map.lookup stepIndex streamSteps)
                    let GeminiStreamStep{stepDeltasRev} = streamStep
                        nextStep = streamStep{stepDeltasRev = deltaObject : stepDeltasRev}
                    Right (streamState{streamSteps = Map.insert stepIndex nextStep streamSteps}, Nothing)
                Just "step.stop" -> do
                    stepIndex <- geminiStreamEventIndex "step.stop" eventObject
                    streamStep <-
                        maybe
                            (Left (malformedGeminiStreamEvent "step.stop referred to an unknown step index"))
                            Right
                            (Map.lookup stepIndex streamSteps)
                    let nextStep = streamStep{stepStopped = True}
                    Right (streamState{streamSteps = Map.insert stepIndex nextStep streamSteps}, Nothing)
                Just "interaction.completed" ->
                    (,)
                        streamState
                        . Just
                        <$> completedGeminiStreamInteraction streamState eventObject
                Just "interaction.complete" ->
                    case KM.lookup "interaction" eventObject of
                        Just interactionValue ->
                            Right (streamState, Just interactionValue)
                        Nothing ->
                            Left (malformedGeminiStreamEvent "interaction.complete was missing an interaction object")
                Just "error" ->
                    Right (streamState, Just (geminiStreamErrorInteraction eventObject))
                maybeTerminalEventType ->
                    (,)
                        streamState
                        <$> geminiTerminalInteraction streamState maybeTerminalEventType eventObject
        _ ->
            Right (streamState, Nothing)

geminiStreamEventType :: Maybe Text -> Object -> Maybe Text
geminiStreamEventType maybeEventName eventObject =
    lookupText "event_type" eventObject
        <|> lookupText "type" eventObject
        <|> maybeEventName

geminiStreamEventIndex :: Text -> Object -> Either RakeError Int
geminiStreamEventIndex eventName eventObject =
    case KM.lookup "index" eventObject >>= jsonInt of
        Just stepIndex ->
            Right stepIndex
        Nothing ->
            Left (malformedGeminiStreamEvent (eventName <> " was missing an integer index"))
  where
    jsonInt value = case fromJSON value of
        Success intValue ->
            Just intValue
        Error _ ->
            Nothing

geminiStreamEventObject :: Text -> Key.Key -> Object -> Either RakeError Object
geminiStreamEventObject label key eventObject =
    case KM.lookup key eventObject of
        Just (Object objectValue) ->
            Right objectValue
        _ ->
            Left (malformedGeminiStreamEvent (label <> " was not an object"))

malformedGeminiStreamEvent :: Text -> RakeError
malformedGeminiStreamEvent detail =
    LlmExpectationError ("Malformed Gemini stream event: " <> toString detail)

completedGeminiStreamInteraction :: GeminiStreamState -> Object -> Either RakeError Value
completedGeminiStreamInteraction GeminiStreamState{createdInteraction, streamSteps} eventObject = do
    terminalInteraction <-
        geminiStreamEventObject "interaction.completed.interaction" "interaction" eventObject
    assembleGeminiTerminalInteraction createdInteraction streamSteps terminalInteraction

assembleGeminiTerminalInteraction
    :: Maybe Object
    -> Map Int GeminiStreamStep
    -> Object
    -> Either RakeError Value
assembleGeminiTerminalInteraction createdInteraction streamSteps terminalInteraction = do
    let interactionObject =
            KM.union terminalInteraction (fromMaybe mempty createdInteraction)
    if KM.member "steps" interactionObject
        || KM.member "outputs" interactionObject
        || geminiInteractionFailed interactionObject
        then Right (Object interactionObject)
        else do
            assembledSteps <- traverse (assembleGeminiStreamStep . snd) (Map.toAscList streamSteps)
            Right (Object (KM.insert "steps" (toJSON assembledSteps) interactionObject))

geminiInteractionFailed :: Object -> Bool
geminiInteractionFailed interactionObject =
    maybe False (`elem` failedStatuses) (lookupText "status" interactionObject)
  where
    failedStatuses :: [Text]
    failedStatuses =
        [ "failed"
        , "cancelled"
        , "canceled"
        , "expired"
        ]

assembleGeminiStreamStep :: GeminiStreamStep -> Either RakeError Value
assembleGeminiStreamStep GeminiStreamStep{startedStep, stepDeltasRev} = do
    let deltas = reverse stepDeltasRev
        argumentFragments = mapMaybe geminiArgumentFragment deltas
        nonArgumentDeltas = filter (isNothing . geminiArgumentFragment) deltas
        assembledStep = foldl' applyGeminiStepDelta startedStep nonArgumentDeltas
    finalStep <-
        if null argumentFragments
            then Right assembledStep
            else do
                argumentsValue <-
                    maybe
                        (Left (malformedGeminiStreamEvent "function-call argument deltas did not form valid JSON"))
                        Right
                        (decodeStrict' (TextEncoding.encodeUtf8 (mconcat argumentFragments)))
                case argumentsValue of
                    Object argumentsObject ->
                        Right (KM.insert "arguments" (Object argumentsObject) assembledStep)
                    _ ->
                        Left
                            (malformedGeminiStreamEvent "function-call argument deltas did not form a JSON object")
    Right (Object finalStep)

geminiArgumentFragment :: Object -> Maybe Text
geminiArgumentFragment deltaObject
    | lookupText "type" deltaObject == Just "arguments_delta" =
        lookupText "arguments" deltaObject
    | lookupText "type" deltaObject == Just "arguments" =
        lookupText "partial_arguments" deltaObject <|> lookupText "arguments" deltaObject
    | otherwise =
        Nothing

applyGeminiStepDelta :: Object -> Object -> Object
applyGeminiStepDelta stepObject deltaObject =
    case lookupText "type" deltaObject of
        Just "text" ->
            appendGeminiStepArrayValue "content" (Object deltaObject) stepObject
        Just "image" ->
            appendGeminiStepArrayValue "content" (Object deltaObject) stepObject
        Just "audio" ->
            appendGeminiStepArrayValue "content" (Object deltaObject) stepObject
        Just "video" ->
            appendGeminiStepArrayValue "content" (Object deltaObject) stepObject
        Just "document" ->
            appendGeminiStepArrayValue "content" (Object deltaObject) stepObject
        Just "refusal" ->
            appendGeminiStepArrayValue "content" (Object deltaObject) stepObject
        Just "thought_signature"
            | Just signature <- lookupText "signature" deltaObject ->
                KM.insert "signature" (String signature) stepObject
        Just "thought_summary"
            | Just summaryContent <- KM.lookup "content" deltaObject ->
                appendGeminiStepArrayValue "summary" summaryContent stepObject
        Nothing
            | Just deltaText <- lookupText "text" deltaObject ->
                appendGeminiStepArrayValue "content" (textContentValue deltaText) stepObject
            | Just signature <- lookupText "signature" deltaObject ->
                KM.insert "signature" (String signature) stepObject
        _ ->
            KM.union (KM.delete "type" deltaObject) stepObject

appendGeminiStepArrayValue :: Key.Key -> Value -> Object -> Object
appendGeminiStepArrayValue key newValue stepObject =
    KM.insert key nextValue stepObject
  where
    nextValue = case KM.lookup key stepObject of
        Just (Array existingValues) ->
            Array (existingValues <> Vector.singleton newValue)
        Just existingValue ->
            toJSON ([existingValue, newValue] :: [Value])
        Nothing ->
            toJSON ([newValue] :: [Value])

emitGeminiStreamDelta
    :: StreamCallbacks es
    -> GeminiStreamState
    -> Maybe Text
    -> Value
    -> Eff es ()
emitGeminiStreamDelta callbacks GeminiStreamState{streamSteps} maybeEventName = \case
    Object eventObject ->
        case geminiStreamEventType maybeEventName eventObject of
            Just "step.start"
                | Just stepIndex <- KM.lookup "index" eventObject >>= jsonInt
                , Just GeminiStreamStep{startedStep} <- Map.lookup stepIndex streamSteps
                , lookupText "type" startedStep == Just "model_output" ->
                    emitGeminiInitialModelOutput callbacks startedStep
            Just "step.delta"
                | Just stepIndex <- KM.lookup "index" eventObject >>= jsonInt
                , Just GeminiStreamStep{startedStep} <- Map.lookup stepIndex streamSteps
                , lookupText "type" startedStep == Just "model_output"
                , Just (Object deltaObject) <- KM.lookup "delta" eventObject ->
                    emitGeminiContentDelta callbacks deltaObject
            Just "content.delta"
                | Just (Object deltaObject) <- KM.lookup "delta" eventObject ->
                    emitGeminiContentDelta callbacks deltaObject
            _ ->
                pure ()
    _ ->
        pure ()
  where
    jsonInt value = case fromJSON value of
        Success intValue ->
            Just intValue
        Error _ ->
            Nothing

emitGeminiInitialModelOutput :: StreamCallbacks es -> Object -> Eff es ()
emitGeminiInitialModelOutput callbacks@StreamCallbacks{onAssistantTextDelta} stepObject =
    case KM.lookup "content" stepObject of
        Just (Array contentParts) ->
            traverse_ (emitContentPart callbacks) (Vector.toList contentParts)
        Just contentPart ->
            emitContentPart callbacks contentPart
        Nothing ->
            pure ()
  where
    emitContentPart streamCallbacks = \case
        Object contentObject ->
            emitGeminiContentDelta streamCallbacks contentObject
        String text ->
            onAssistantTextDelta text
        _ ->
            pure ()

emitGeminiContentDelta :: StreamCallbacks es -> Object -> Eff es ()
emitGeminiContentDelta StreamCallbacks{onAssistantTextDelta, onAssistantRefusalDelta} deltaObject =
    case lookupText "type" deltaObject of
        Just "text"
            | Just deltaText <- lookupText "text" deltaObject ->
                onAssistantTextDelta deltaText
        Just "refusal"
            | Just refusalText <- lookupText "refusal" deltaObject <|> lookupText "text" deltaObject ->
                onAssistantRefusalDelta refusalText
        Nothing
            | Just deltaText <- lookupText "text" deltaObject ->
                onAssistantTextDelta deltaText
        _ ->
            pure ()

geminiStreamErrorInteraction :: Object -> Value
geminiStreamErrorInteraction eventObject =
    object
        [ "status" .= ("failed" :: Text)
        , "error" .= fromMaybe (Object eventObject) (KM.lookup "error" eventObject)
        ]

geminiTerminalInteraction
    :: GeminiStreamState
    -> Maybe Text
    -> Object
    -> Either RakeError (Maybe Value)
geminiTerminalInteraction GeminiStreamState{createdInteraction, streamSteps} maybeEventType eventObject =
    case KM.lookup "interaction" eventObject of
        Just (Object interactionObject)
            | maybe False (`elem` terminalStatuses) (lookupText "status" interactionObject) ->
                Just
                    <$> assembleGeminiTerminalInteraction createdInteraction streamSteps interactionObject
        _
            | Just terminalStatus <- maybeEventType >>= interactionEventTerminalStatus ->
                let terminalInteraction =
                        KM.insert "status" (String terminalStatus) (fromMaybe mempty createdInteraction)
                 in Just
                        <$> assembleGeminiTerminalInteraction createdInteraction streamSteps terminalInteraction
        _ ->
            Right Nothing
  where
    terminalStatuses :: [Text]
    terminalStatuses =
        [ "completed"
        , "requires_action"
        , "incomplete"
        , "failed"
        , "cancelled"
        , "canceled"
        , "expired"
        ]

interactionEventTerminalStatus :: Text -> Maybe Text
interactionEventTerminalStatus = \case
    "interaction.requires_action" ->
        Just "requires_action"
    "interaction.incomplete" ->
        Just "incomplete"
    "interaction.failed" ->
        Just "failed"
    "interaction.cancelled" ->
        Just "cancelled"
    "interaction.canceled" ->
        Just "canceled"
    "interaction.expired" ->
        Just "expired"
    _ ->
        Nothing

buildGeminiRequestBody
    :: ( Error RakeError :> es
       , RakeMediaStorage :> es
       )
    => GeminiChatSettings es
    -> [ToolDeclaration]
    -> ResponseFormat
    -> SamplingOptions
    -> [HistoryItem]
    -> Eff es Value
buildGeminiRequestBody
    GeminiChatSettings
        { model
        , providerTools
        , systemInstruction
        , generationConfig
        , requestLogger = _requestLogger
        }
    tools
    responseFormat
    samplingOptions
    history = do
        -- Gemini also gets one effective system instruction. We therefore collapse
        -- GenericSystem to the latest snapshot for compatibility with the shared
        -- portable semantics used across providers.
        let (maybeSystemSnapshot, chronologicalHistory) = splitRenderableGeminiHistory history
        renderedHistory <- renderGeminiHistory chronologicalHistory
        renderedSystemInstruction <- traverse historyItemSystemInstruction maybeSystemSnapshot
        let RenderedGeminiHistory
                { renderedSteps = renderedStepValues
                } = renderedHistory
            maybeResponseFormat = geminiResponseFormatSchema responseFormat
        pure
            $ object
            $ [ "model" .= model
              , "input" .= reverse renderedStepValues
              , "store" .= False
              ]
            <> catMaybes
                [ ("system_instruction" .=)
                    <$> combinedSystemInstruction systemInstruction renderedSystemInstruction
                , if null allTools
                    then Nothing
                    else Just ("tools" .= allTools)
                , ("response_format" .=) <$> maybeResponseFormat
                , ("generation_config" .=) <$> generationConfigValue generationConfig samplingOptions
                ]
      where
        allTools =
            map localToolDeclarationToGeminiTool tools
                <> fmap toJSON providerTools

renderGeminiHistory
    :: ( Error RakeError :> es
       , RakeMediaStorage :> es
       )
    => [HistoryItem]
    -> Eff es RenderedGeminiHistory
renderGeminiHistory =
    foldlM renderGeminiHistoryItem initialRenderedGeminiHistory

renderGeminiHistoryItem
    :: ( Error RakeError :> es
       , RakeMediaStorage :> es
       )
    => RenderedGeminiHistory
    -> HistoryItem
    -> Eff es RenderedGeminiHistory
renderGeminiHistoryItem renderedHistory historyEntry =
    renderGeminiCanonicalHistoryItem renderedHistory historyEntry

geminiMessageStepValue :: Text -> [Value] -> Value
geminiMessageStepValue stepType content =
    object
        [ "type" .= stepType
        , "content" .= content
        ]

data RenderedGeminiHistory = RenderedGeminiHistory
    { renderedSteps :: [Value]
    , toolCallNames :: Map Text Text
    }

initialRenderedGeminiHistory :: RenderedGeminiHistory
initialRenderedGeminiHistory =
    RenderedGeminiHistory
        { renderedSteps = []
        , toolCallNames = mempty
        }

combinedSystemInstruction :: Maybe Text -> Maybe Text -> Maybe Text
combinedSystemInstruction settingsInstruction historyInstruction =
    if null allBlocks
        then Nothing
        else Just (mconcat (intersperse "\n\n" allBlocks))
  where
    allBlocks = catMaybes [settingsInstruction, historyInstruction]

renderGeminiCanonicalHistoryItem
    :: ( Error RakeError :> es
       , RakeMediaStorage :> es
       )
    => RenderedGeminiHistory
    -> HistoryItem
    -> Eff es RenderedGeminiHistory
renderGeminiCanonicalHistoryItem
    renderedHistory
    HistoryItem
        { itemLifecycle = lifecycle
        , genericItem = genericHistoryItem
        , providerItem = maybeProviderItem
        } =
        case genericHistoryItem of
            GenericMessage{role = GenericUser, parts} -> do
                renderedContentParts <- messagePartsToGeminiContentParts ProviderGeminiInteractions parts
                pure
                    $ appendGeminiStep
                        (geminiMessageStepValue "user_input" renderedContentParts)
                        renderedHistory
            GenericMessage{role = GenericAssistant, parts} -> do
                case completedGeminiProviderStep "model_output" lifecycle maybeProviderItem of
                    Just providerStep ->
                        pure (appendGeminiStep providerStep renderedHistory)
                    Nothing -> do
                        renderedContentParts <- messagePartsToGeminiContentParts ProviderGeminiInteractions parts
                        pure
                            $ appendGeminiStep
                                ( geminiMessageStepValue
                                    "model_output"
                                    (annotatePendingAssistantContentParts lifecycle renderedContentParts)
                                )
                                renderedHistory
            GenericMessage{role = GenericSystem} ->
                pure renderedHistory
            GenericToolCall
                { toolCall =
                    ToolCall{toolCallId = ToolCallId toolCallId, toolName, toolArgs, continuationAttachments}
                } ->
                    let geminiContinuationPayloads =
                            [ continuationPayload
                            | ToolCallContinuation
                                { continuationProviderFamily = ProviderGeminiInteractions
                                , continuationPayload
                                } <-
                                continuationAttachments
                            ]
                        renderedHistoryWithContinuations =
                            foldl' (flip appendGeminiStep) renderedHistory geminiContinuationPayloads
                        functionCallStep =
                            fromMaybe
                                (genericGeminiFunctionCallValue toolCallId toolName toolArgs)
                                (geminiProviderStep "function_call" maybeProviderItem)
                     in pure
                            $ appendGeminiStep
                                functionCallStep
                                (registerToolCall toolCallId toolName renderedHistoryWithContinuations)
            GenericToolResult
                { toolResult = ToolResult{toolCallId = ToolCallId toolCallId, toolResponse}
                } -> do
                    let RenderedGeminiHistory{toolCallNames} = renderedHistory
                    toolName <-
                        maybe
                            ( throwError
                                (LlmExpectationError "Gemini function_result requires the preceding tool call name")
                            )
                            pure
                            (Map.lookup toolCallId toolCallNames)
                    pure
                        $ appendGeminiStep
                            ( object
                                [ "type" .= ("function_result" :: Text)
                                , "name" .= toolName
                                , "call_id" .= toolCallId
                                , "result" .= geminiToolResultValue toolResponse
                                ]
                            )
                            renderedHistory
            GenericResetTo{} ->
                pure renderedHistory
            GenericReplayBarrier{} ->
                pure renderedHistory
            GenericNonPortable ->
                pure
                    $ case maybeProviderItem of
                        Just ProviderItem{apiFamily, payload}
                            | lifecycle == ItemCompleted
                            , apiFamily == ProviderGeminiInteractions ->
                                appendGeminiStep payload renderedHistory
                        _ ->
                            renderedHistory

appendGeminiStep :: Value -> RenderedGeminiHistory -> RenderedGeminiHistory
appendGeminiStep step renderedHistory@RenderedGeminiHistory{renderedSteps} =
    renderedHistory{renderedSteps = step : renderedSteps}

completedGeminiProviderStep :: Text -> ItemLifecycle -> Maybe ProviderItem -> Maybe Value
completedGeminiProviderStep expectedType lifecycle maybeProviderItem = do
    guard (lifecycle == ItemCompleted)
    geminiProviderStep expectedType maybeProviderItem

geminiProviderStep :: Text -> Maybe ProviderItem -> Maybe Value
geminiProviderStep expectedType = \case
    Just
        ProviderItem{apiFamily = ProviderGeminiInteractions, payload = step@(Object stepObject)}
            | lookupText "type" stepObject == Just expectedType ->
                Just step
    _ ->
        Nothing

genericGeminiFunctionCallValue :: Text -> Text -> Map Text Value -> Value
genericGeminiFunctionCallValue toolCallId toolName toolArgs =
    object
        [ "type" .= ("function_call" :: Text)
        , "id" .= toolCallId
        , "name" .= toolName
        , "arguments" .= toolArgs
        ]

registerToolCall :: Text -> Text -> RenderedGeminiHistory -> RenderedGeminiHistory
registerToolCall toolCallId toolName renderedHistory@RenderedGeminiHistory{toolCallNames} =
    renderedHistory{toolCallNames = Map.insert toolCallId toolName toolCallNames}

splitRenderableGeminiHistory :: [HistoryItem] -> (Maybe HistoryItem, [HistoryItem])
splitRenderableGeminiHistory history =
    ( latestGeminiSystemSnapshot history
    , filter (not . isGeminiSystemHistoryItem) history
    )

latestGeminiSystemSnapshot :: [HistoryItem] -> Maybe HistoryItem
latestGeminiSystemSnapshot =
    viaNonEmpty last . filter isGeminiSystemHistoryItem

isGeminiSystemHistoryItem :: HistoryItem -> Bool
isGeminiSystemHistoryItem HistoryItem{genericItem = GenericMessage{role = GenericSystem}} =
    True
isGeminiSystemHistoryItem _ =
    False

historyItemSystemInstruction
    :: Error RakeError :> es
    => HistoryItem
    -> Eff es Text
historyItemSystemInstruction HistoryItem{genericItem = GenericMessage{role = GenericSystem, parts}} =
    messagePartsToText parts
historyItemSystemInstruction _ =
    throwError (LlmExpectationError "Gemini system instruction can only be rendered from a system message")

messagePartsToText
    :: Error RakeError :> es
    => [MessagePart]
    -> Eff es Text
messagePartsToText =
    fmap mconcat . traverse partText
  where
    partText = \case
        PartText{text} ->
            pure text
        PartRefusal{text} ->
            pure text
        PartImage{} ->
            throwError (LlmExpectationError "Generic media message parts require a blob resolver before they can be rendered to provider input")
        PartAudio{} ->
            throwError (LlmExpectationError "Generic media message parts require a blob resolver before they can be rendered to provider input")
        PartFile{} ->
            throwError (LlmExpectationError "Generic media message parts require a blob resolver before they can be rendered to provider input")

messagePartsToGeminiContentParts
    :: ( Error RakeError :> es
       , RakeMediaStorage :> es
       )
    => ProviderApiFamily
    -> [MessagePart]
    -> Eff es [Value]
messagePartsToGeminiContentParts providerFamily =
    traverse (messagePartToGeminiContentPart providerFamily)
  where
    messagePartToGeminiContentPart targetProviderFamily = \case
        PartText{text} ->
            pure (textContentValue text)
        PartRefusal{text} ->
            pure (textContentValue text)
        PartImage{blobId} ->
            resolveMediaContentPart targetProviderFamily blobId
        PartAudio{blobId} ->
            resolveMediaContentPart targetProviderFamily blobId
        PartFile{blobId} ->
            resolveMediaContentPart targetProviderFamily blobId

    resolveMediaContentPart targetProviderFamily blobId = do
        maybeRequestPart <- lookupMediaReference targetProviderFamily blobId
        case maybeRequestPart of
            Just requestPart ->
                pure requestPart
            Nothing ->
                let MediaBlobId blobIdText = blobId
                 in
                throwError
                    ( LlmExpectationError
                        ( "No stored media reference is available for blob "
                            <> toString blobIdText
                            <> " when rendering "
                            <> toString (providerApiFamilyText targetProviderFamily)
                        )
                    )

annotatePendingAssistantContentParts :: ItemLifecycle -> [Value] -> [Value]
annotatePendingAssistantContentParts lifecycle contentParts
    | lifecycle == ItemPending =
        textContentValue "[INCOMPLETE ASSISTANT MESSAGE FROM PREVIOUS PROVIDER]\n" : contentParts
    | otherwise =
        contentParts

textContentValue :: Text -> Value
textContentValue text =
    object
        [ "type" .= ("text" :: Text)
        , "text" .= text
        ]

geminiToolResultValue :: ToolResponse -> Value
geminiToolResultValue = \case
    ToolResponseText{text} ->
        geminiToolResultTextParts text
    ToolResponseJson{json} ->
        geminiToolResultTextParts (valueToCompactText json)

geminiToolResultTextParts :: Text -> Value
geminiToolResultTextParts text =
    toJSON
        ( [ object
                [ "type" .= ("text" :: Text)
                , "text" .= text
                ]
          ]
            :: [Value]
        )

localToolDeclarationToGeminiTool :: ToolDeclaration -> Value
localToolDeclarationToGeminiTool ToolDeclaration{name, description, parameterSchema} =
    object $
        [ "type" .= ("function" :: Text)
        , "name" .= name
        , "description" .= description
        , "parameters" .= fromMaybe emptyToolParametersSchema parameterSchema
        ]

emptyToolParametersSchema :: Value
emptyToolParametersSchema =
    object []

geminiResponseFormatSchema :: ResponseFormat -> Maybe Value
geminiResponseFormatSchema = \case
    Unstructured ->
        Nothing
    JsonValue ->
        Just
            . geminiJsonResponseFormat
            $ object
                [ "type" .= ("object" :: Text)
                , "additionalProperties" .= True
                ]
    JsonSchema schema ->
        Just (geminiJsonResponseFormat (toGeminiStructuredSchema schema))

geminiJsonResponseFormat :: Value -> Value
geminiJsonResponseFormat schema =
    object
        [ "type" .= ("text" :: Text)
        , "mime_type" .= ("application/json" :: Text)
        , "schema" .= schema
        ]

toGeminiStructuredSchema :: Value -> Value
toGeminiStructuredSchema = \case
    Object objectValue
        | Just nullableSchema <- nullableAnyOfSchema objectValue ->
            makeGeminiSchemaNullable (toGeminiStructuredSchema nullableSchema)
        | Just flattenedObject <- flattenObjectOneOfSchema objectValue ->
            toGeminiStructuredSchema (Object flattenedObject)
        | otherwise ->
            Object (ensureGeminiSchemaType normalizedObject)
      where
        normalizedObject = KM.mapWithKey normalizeGeminiSchemaField objectValue
    Array values ->
        Array (toGeminiStructuredSchema <$> values)
    other ->
        other

normalizeGeminiSchemaField :: Key.Key -> Value -> Value
normalizeGeminiSchemaField fieldName fieldValue
    | fieldName `elem` geminiSchemaMapFields =
        normalizeGeminiSchemaMap fieldValue
    | otherwise =
        toGeminiStructuredSchema fieldValue

normalizeGeminiSchemaMap :: Value -> Value
normalizeGeminiSchemaMap = \case
    Object objectValue ->
        Object (KM.map toGeminiStructuredSchema objectValue)
    other ->
        toGeminiStructuredSchema other

nullableAnyOfSchema :: KM.KeyMap Value -> Maybe Value
nullableAnyOfSchema objectValue = do
    Array alternatives <- KM.lookup "anyOf" objectValue
    let alternativeValues = toList alternatives
        nonNullAlternatives = filter (not . isNullSchema) alternativeValues
    guard (length nonNullAlternatives == 1 && any isNullSchema alternativeValues)
    viaNonEmpty head nonNullAlternatives

flattenObjectOneOfSchema :: KM.KeyMap Value -> Maybe (KM.KeyMap Value)
flattenObjectOneOfSchema objectValue = do
    Array alternatives <- KM.lookup "oneOf" objectValue
    alternativeObjects <- traverse objectSchema (toList alternatives)
    firstObject : restObjects <- pure alternativeObjects
    let alternativeProperties = objectProperties <$> alternativeObjects
        mergedProperties = foldl' mergeProperties mempty alternativeProperties
        commonRequired =
            foldl'
                intersectRequired
                (requiredPropertyNames firstObject)
                (requiredPropertyNames <$> restObjects)
    pure $
        KM.fromList
            [ ("type", String "object")
            , ("properties", Object mergedProperties)
            , ("required", toJSON commonRequired)
            , ("additionalProperties", Bool False)
            ]

objectSchema :: Value -> Maybe (KM.KeyMap Value)
objectSchema = \case
    Object objectValue
        | KM.lookup "type" objectValue == Just (String "object") || KM.member "properties" objectValue ->
            Just objectValue
    _ ->
        Nothing

objectProperties :: KM.KeyMap Value -> KM.KeyMap Value
objectProperties objectValue = case KM.lookup "properties" objectValue of
    Just (Object propertiesObject) ->
        propertiesObject
    _ ->
        mempty

requiredPropertyNames :: KM.KeyMap Value -> [Text]
requiredPropertyNames objectValue = case KM.lookup "required" objectValue of
    Just (Array values) ->
        mapMaybe requiredPropertyName (toList values)
    _ ->
        []

requiredPropertyName :: Value -> Maybe Text
requiredPropertyName = \case
    String fieldName ->
        Just fieldName
    _ ->
        Nothing

intersectRequired :: [Text] -> [Text] -> [Text]
intersectRequired left right =
    filter (`elem` right) left

mergeProperties :: KM.KeyMap Value -> KM.KeyMap Value -> KM.KeyMap Value
mergeProperties =
    KM.unionWith mergePropertySchema

mergePropertySchema :: Value -> Value -> Value
mergePropertySchema left@(Object leftObject) (Object rightObject)
    | Just mergedEnum <- mergeEnumValues leftObject rightObject =
        Object (ensureGeminiSchemaType (KM.insert "enum" mergedEnum leftObject))
    | otherwise =
        left
mergePropertySchema left _ =
    left

mergeEnumValues :: KM.KeyMap Value -> KM.KeyMap Value -> Maybe Value
mergeEnumValues leftObject rightObject = do
    Array leftEnum <- KM.lookup "enum" leftObject
    Array rightEnum <- KM.lookup "enum" rightObject
    pure . Array . fromList $ ordNub (toList leftEnum <> toList rightEnum)

isNullSchema :: Value -> Bool
isNullSchema = \case
    Object objectValue ->
        KM.lookup "type" objectValue == Just (String "null")
    _ ->
        False

makeGeminiSchemaNullable :: Value -> Value
makeGeminiSchemaNullable = \case
    Object objectValue ->
        Object (KM.insert "type" (nullableTypeValue (KM.lookup "type" objectValue)) objectValue)
    schemaValue ->
        object
            [ "type" .= (["null"] :: [Text])
            , "anyOf" .= ([schemaValue] :: [Value])
            ]

nullableTypeValue :: Maybe Value -> Value
nullableTypeValue = \case
    Just (String typeName) ->
        toJSON ([typeName, "null"] :: [Text])
    Just (Array typeNames)
        | any (== String "null") typeNames ->
            Array typeNames
        | otherwise ->
            Array (typeNames <> Vector.singleton (String "null"))
    _ ->
        toJSON (["null"] :: [Text])

ensureGeminiSchemaType :: KM.KeyMap Value -> KM.KeyMap Value
ensureGeminiSchemaType objectValue
    | KM.member "type" objectValue = objectValue
    | otherwise =
        case inferGeminiSchemaType objectValue of
            Nothing ->
                objectValue
            Just typeValue ->
                KM.insert "type" typeValue objectValue

inferGeminiSchemaType :: KM.KeyMap Value -> Maybe Value
inferGeminiSchemaType objectValue
    | KM.member "properties" objectValue =
        Just (String "object")
    | KM.member "oneOf" objectValue =
        Just (String "object")
    | KM.member "items" objectValue =
        Just (String "array")
    | otherwise =
        inferEnumSchemaType =<< KM.lookup "enum" objectValue

inferEnumSchemaType :: Value -> Maybe Value
inferEnumSchemaType = \case
    Array values ->
        enumTypeValue (filter (/= Null) (toList values))
    _ ->
        Nothing

enumTypeValue :: [Value] -> Maybe Value
enumTypeValue values
    | null values = Nothing
    | all isString values = Just (String "string")
    | all isNumber values = Just (String "number")
    | all isBool values = Just (String "boolean")
    | otherwise = Nothing
  where
    isString = \case
        String{} -> True
        _ -> False

    isNumber = \case
        Number{} -> True
        _ -> False

    isBool = \case
        Bool{} -> True
        _ -> False

geminiSchemaMapFields :: [Key.Key]
geminiSchemaMapFields =
    [ "$defs"
    , "definitions"
    , "dependentSchemas"
    , "patternProperties"
    , "properties"
    ]

generationConfigValue :: GeminiGenerationConfig -> SamplingOptions -> Maybe Value
generationConfigValue GeminiGenerationConfig{temperature, topP, seed, stopSequences, thinkingLevel, thinkingSummaries, maxOutputTokens, toolChoice} SamplingOptions{temperature = samplingTemperature, topP = samplingTopP} =
    if null generationFields
        then Nothing
        else Just (object generationFields)
  where
    generationFields =
        catMaybes
            [ ("temperature" .=) <$> (samplingTemperature <|> temperature)
            , ("top_p" .=) <$> (samplingTopP <|> topP)
            , ("seed" .=) <$> seed
            , if null stopSequences
                then Nothing
                else Just ("stop_sequences" .= stopSequences)
            , ("thinking_level" .=) <$> thinkingLevel
            , ("thinking_summaries" .=) <$> thinkingSummaries
            , ("max_output_tokens" .=) <$> maxOutputTokens
            , ("tool_choice" .=) <$> toolChoice
            ]

decodeGeminiResponse :: Value -> Either RakeError ProviderRound
decodeGeminiResponse responseValue = do
    responseObject <- expectObject "interaction" responseValue
    let interactionStatus = lookupText "status" responseObject
        interactionFailureDetail = providerFailureDetail responseObject
    steps <- case KM.lookup "steps" responseObject <|> KM.lookup "outputs" responseObject of
        Just stepsValue ->
            expectArray "interaction.steps" stepsValue
        Nothing ->
            Right Vector.empty
    let outputPayloads = Vector.toList steps
        interactionExchangeId =
            lookupText "id" responseObject
                <|> geminiFallbackExchangeId outputPayloads
        classifiedOutputItems =
            classifyGeminiPayloads outputPayloads
        projectionNotes =
            concatMap (\(_, notes, _) -> notes) classifiedOutputItems
        projectedItems =
            [ canonicalItem
            | (_, _, canonicalItem) <- classifiedOutputItems
            ]
        toolCalls = collectToolCalls projectedItems
        roundAction =
            geminiRoundAction
                interactionStatus
                interactionFailureDetail
                projectedItems
                toolCalls
                projectionNotes
        roundItemLifecycle = providerRoundItemLifecycle roundAction
    roundItems <- forM classifiedOutputItems $ \(payload, _, canonicalItem) -> do
        let rawProviderItem =
                ProviderItem
                    { apiFamily = ProviderGeminiInteractions
                    , exchangeId = interactionExchangeId
                    , nativeItemId =
                        case payload of
                            Object payloadObject ->
                                geminiNativeItemId payloadObject
                            _ ->
                                Nothing
                    , payload
                    , availableLocalTools = []
                    }
        pure
            $ case canonicalItem of
                GenericNonPortable ->
                    nonPortableHistoryItem roundItemLifecycle rawProviderItem
                _ ->
                    HistoryItem
                        { historyItemIdField = Nothing
                        , itemLifecycle = roundItemLifecycle
                        , genericItem = canonicalItem
                        , providerItem = Just rawProviderItem
                        }
    pure ProviderRound{roundItems, mediaReferences = [], action = roundAction}

geminiRoundAction
    :: Maybe Text
    -> Maybe Text
    -> [GenericItem]
    -> [ToolCall]
    -> [Text]
    -> ProviderRoundAction
geminiRoundAction interactionStatus interactionFailureDetail projectedItems toolCalls projectionNotes
    | Just failureAction <- interactionStatus >>= (`geminiTerminalAction` interactionFailureDetail) =
        failureAction
    | not (null toolCalls) =
        ProviderRoundNeedsLocalTools toolCalls
    | Just "requires_action" <- interactionStatus =
        ProviderRoundFailed $
            FailureContract $
                appendProjectionNotes
                    "Gemini interaction required local tools but returned no projected tool calls"
                    projectionNotes
    | Just statusAction <- geminiStatusAction =<< interactionStatus =
        statusAction "Gemini interaction status was "
    | hasAssistantMessage projectedItems =
        ProviderRoundDone
    | otherwise =
        ProviderRoundFailed $
            FailureContract $
                appendProjectionNotes
                    "Gemini interaction completed without tool calls or assistant message"
                    projectionNotes

geminiTerminalAction :: Text -> Maybe Text -> Maybe ProviderRoundAction
geminiTerminalAction interactionStatus maybeDetail = case interactionStatus of
    "failed" ->
        Just (ProviderRoundFailed (FailureProvider (appendProviderFailureDetail ("Gemini interaction status was " <> interactionStatus) maybeDetail)))
    "cancelled" ->
        Just (ProviderRoundFailed (FailureProvider (appendProviderFailureDetail ("Gemini interaction status was " <> interactionStatus) maybeDetail)))
    "canceled" ->
        Just (ProviderRoundFailed (FailureProvider (appendProviderFailureDetail ("Gemini interaction status was " <> interactionStatus) maybeDetail)))
    "expired" ->
        Just (ProviderRoundFailed (FailureProvider (appendProviderFailureDetail ("Gemini interaction status was " <> interactionStatus) maybeDetail)))
    _ ->
        Nothing

geminiStatusAction :: Text -> Maybe (Text -> ProviderRoundAction)
geminiStatusAction = \case
    "completed" ->
        Nothing
    "requires_action" ->
        Nothing
    "incomplete" ->
        Just (ProviderRoundPaused . PauseIncomplete . (<> "incomplete"))
    "queued" ->
        Just (ProviderRoundPaused . PauseProviderWaiting . (<> "queued"))
    "in_progress" ->
        Just (ProviderRoundPaused . PauseProviderWaiting . (<> "in_progress"))
    "pending" ->
        Just (ProviderRoundPaused . PauseProviderWaiting . (<> "pending"))
    "processing" ->
        Just (ProviderRoundPaused . PauseProviderWaiting . (<> "processing"))
    "failed" ->
        Just (ProviderRoundFailed . FailureProvider . (<> "failed"))
    "cancelled" ->
        Just (ProviderRoundFailed . FailureProvider . (<> "cancelled"))
    "canceled" ->
        Just (ProviderRoundFailed . FailureProvider . (<> "canceled"))
    "expired" ->
        Just (ProviderRoundFailed . FailureProvider . (<> "expired"))
    otherStatus ->
        Just (\_ -> ProviderRoundFailed (FailureContract ("Unsupported Gemini interaction status: " <> otherStatus)))

providerRoundItemLifecycle :: ProviderRoundAction -> ItemLifecycle
providerRoundItemLifecycle = \case
    ProviderRoundDone ->
        ItemCompleted
    ProviderRoundNeedsLocalTools{} ->
        ItemPending
    ProviderRoundPaused{} ->
        ItemPending
    ProviderRoundFailed{} ->
        ItemPending

collectToolCalls :: [GenericItem] -> [ToolCall]
collectToolCalls genericItems =
    [ genericToolCall'
    | GenericToolCall{toolCall = genericToolCall'} <- genericItems
    ]

hasAssistantMessage :: [GenericItem] -> Bool
hasAssistantMessage =
    any \case
        GenericMessage{role = GenericAssistant} ->
            True
        _ ->
            False

appendProjectionNotes :: Text -> [Text] -> Text
appendProjectionNotes base notes =
    if null notes
        then base
        else base <> ". Projection notes: " <> mconcat (intersperse "; " notes)

appendProviderFailureDetail :: Text -> Maybe Text -> Text
appendProviderFailureDetail base maybeDetail =
    maybe base ((base <> ": ") <>) maybeDetail

providerFailureDetail :: Object -> Maybe Text
providerFailureDetail objectValue =
    (KM.lookup "error" objectValue >>= providerErrorValueDetail)
        <|> lookupText "message" objectValue
        <|> lookupText "failure_reason" objectValue

providerErrorValueDetail :: Value -> Maybe Text
providerErrorValueDetail = \case
    Object errorObject ->
        combineErrorCodeAndMessage
            (lookupText "code" errorObject)
            (lookupText "message" errorObject)
            <|> Just (valueToCompactText (Object errorObject))
    String errorText ->
        Just errorText
    otherValue ->
        Just (valueToCompactText otherValue)

combineErrorCodeAndMessage :: Maybe Text -> Maybe Text -> Maybe Text
combineErrorCodeAndMessage maybeCode maybeMessage =
    case (maybeCode, maybeMessage) of
        (Just code, Just message) ->
            Just (code <> ": " <> message)
        (Just code, Nothing) ->
            Just code
        (Nothing, Just message) ->
            Just message
        (Nothing, Nothing) ->
            Nothing

geminiNativeItemId :: Object -> Maybe Text
geminiNativeItemId payloadObject =
    lookupText "id" payloadObject
        <|> lookupText "call_id" payloadObject
        <|> lookupText "signature" payloadObject

geminiFallbackExchangeId :: [Value] -> Maybe Text
geminiFallbackExchangeId =
    viaNonEmpty head . mapMaybe payloadNativeItemId
  where
    payloadNativeItemId = \case
        Object payloadObject ->
            geminiNativeItemId payloadObject
        _ ->
            Nothing

expectObject :: Text -> Value -> Either RakeError Object
expectObject label = \case
    Object objectValue ->
        Right objectValue
    _ ->
        Left (LlmExpectationError ("Expected " <> toString label <> " to be an object"))

expectArray :: Text -> Value -> Either RakeError (Vector.Vector Value)
expectArray label = \case
    Array values ->
        Right values
    _ ->
        Left (LlmExpectationError ("Expected " <> toString label <> " to be an array"))

lookupText :: Key.Key -> Object -> Maybe Text
lookupText key objectValue = KM.lookup key objectValue >>= \case
    String text ->
        Just text
    _ ->
        Nothing
