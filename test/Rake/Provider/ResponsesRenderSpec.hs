module Rake.Provider.ResponsesRenderSpec where

import Control.Concurrent qualified as Concurrent
import Control.Exception (ErrorCall (ErrorCall), bracket, finally)
import Data.Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BC8
import Data.ByteString.Lazy qualified as LBS
import Data.IORef qualified as IORef
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Effectful
import Effectful.Error.Static
import GHC.IO.Handle (hDuplicate, hDuplicateTo)
import Network.HTTP.Types.Status (accepted202)
import Network.HTTP.Types.Version (http11)
import Network.Socket qualified as Socket
import Network.Socket.ByteString qualified as SocketBS
import Rake
import Rake.MediaStorage.InMemory
import Rake.Providers.Gemini.Chat
import Rake.Providers.OpenAI.Chat
import Rake.Providers.OpenAI.Chat qualified as OpenAI
import Rake.Providers.XAI.Chat
import Rake.Providers.XAI.Imagine
import Relude
import Servant.Client (BaseUrl (..), ClientError (..), ResponseF (..), Scheme (..))
import Servant.Client.Core.Request (RequestF (..))
import StructuredSchemaTestTypes
import System.Directory (removeFile)
import System.IO qualified as IO
import Test.Hspec

spec :: Spec
spec = describe "Responses request rendering" $ do
    describe "schema preservation" $ do
        it "preserves raw JsonSchema values verbatim" $ do
            let rawSchema =
                    object
                        [ "type" .= ("object" :: Text)
                        , "additionalProperties" .= True
                        ]

            requestBody <-
                captureOpenAIRequestBody
                    (withResponseFormat (JsonSchema rawSchema) defaultChatConfig)
                    [user "hello"]

            lookupPath ["text", "format", "schema"] requestBody `shouldBe` Just rawSchema
            lookupPath ["text", "format", "strict"] requestBody `shouldBe` Nothing

        it "preserves custom raw tool parameterSchema values verbatim" $ do
            let rawSchema =
                    object
                        [ "type" .= ("object" :: Text)
                        , "additionalProperties" .= True
                        ]
                tool =
                    ToolDef
                        { name = "raw_tool"
                        , description = "Raw schema tool"
                        , parameterSchema = Just rawSchema
                        , executeFunction = \_ -> pure (Right "ok")
                        }

            requestBody <-
                captureOpenAIRequestBody
                    (withTools [tool] defaultChatConfig)
                    [user "hello"]

            firstToolParameters requestBody `shouldBe` Just rawSchema

        it "renders the no-argument tool fallback as a closed object with required []" $ do
            let tool =
                    defineToolNoArgument "noop" "No-op tool" (pure (Right "ok"))

            requestBody <-
                captureOpenAIRequestBody
                    (withTools [tool] defaultChatConfig)
                    [user "hello"]

            firstToolParameters requestBody
                `shouldBe` Just
                    ( object
                        [ "type" .= ("object" :: Text)
                        , "properties" .= object []
                        , "required" .= ([] :: [Text])
                        , "additionalProperties" .= False
                        ]
                    )

        it "renders Gemini no-argument tools with an empty parameters object" $ do
            let tool =
                    defineToolNoArgument "noop" "No-op tool" (pure (Right "ok"))

            requestBody <-
                captureGeminiRequestBody
                    (withTools [tool] defaultChatConfig)
                    [user "hello"]

            firstToolParameters requestBody `shouldBe` Just (object [])

        forM_ ([2, excessiveToolCount] :: [Int]) $ \toolCountToRender ->
            it (toString ("renders " <> show toolCountToRender <> " tools for OpenAI, xAI, and Gemini" :: Text)) $ do
                let chatConfig =
                        withTools (generatedTools toolCountToRender) defaultChatConfig

                openAIRequestBody <- captureOpenAIRequestBody chatConfig [user "hello"]
                xaiRequestBody <- captureXAIRequestBody chatConfig [user "hello"]
                geminiRequestBody <- captureGeminiRequestBody chatConfig [user "hello"]

                toolCount openAIRequestBody `shouldBe` Just toolCountToRender
                toolCount xaiRequestBody `shouldBe` Just toolCountToRender
                toolCount geminiRequestBody `shouldBe` Just toolCountToRender

        it "renders Gemini structured response schemas in the current response_format envelope" $ do
            requestBody <-
                captureGeminiRequestBody
                    (withResponseFormat (jsonSchemaFormat @RecordWithMaybe) defaultChatConfig)
                    [user "hello"]

            lookupPath ["response_mime_type"] requestBody `shouldBe` Nothing
            lookupPath ["response_format", "type"] requestBody `shouldBe` Just (String "text")
            lookupPath ["response_format", "mime_type"] requestBody `shouldBe` Just (String "application/json")
            lookupPath ["response_format", "schema", "type"] requestBody `shouldBe` Just (String "object")
            lookupPath ["response_format", "schema", "properties", "toolCall", "type"] requestBody
                `shouldBe` Just (toJSON (["object", "null"] :: [Text]))
            lookupPath ["response_format", "schema", "properties", "toolCall", "anyOf"] requestBody `shouldBe` Nothing

        it "adds Gemini response_format types for enum and sum schemas" $ do
            enumRequestBody <-
                captureGeminiRequestBody
                    (withResponseFormat (jsonSchemaFormat @NullaryEnum) defaultChatConfig)
                    [user "hello"]
            sumRequestBody <-
                captureGeminiRequestBody
                    (withResponseFormat (jsonSchemaFormat @NonNullarySum) defaultChatConfig)
                    [user "hello"]

            lookupPath ["response_format", "schema", "type"] enumRequestBody `shouldBe` Just (String "string")
            lookupPath ["response_format", "schema", "type"] sumRequestBody `shouldBe` Just (String "object")
            lookupPath ["response_format", "schema", "oneOf"] sumRequestBody `shouldBe` Nothing
            lookupPath ["response_format", "schema", "properties", "tag", "enum"] sumRequestBody
                `shouldBe` Just (toJSON (["SumText", "SumCount"] :: [Text]))

    describe "sampling options" $ do
        it "renders temperature when explicitly configured" $ do
            requestBody <-
                captureOpenAIRequestBody
                    (withSampling (withTemperature (Just 0) defaultSamplingOptions) defaultChatConfig)
                    [user "hello"]

            lookupPath ["temperature"] requestBody `shouldBe` Just (Number 0)

        it "omits temperature when not configured" $ do
            requestBody <-
                captureOpenAIRequestBody
                    defaultChatConfig
                    [user "hello"]

            lookupPath ["temperature"] requestBody `shouldBe` Nothing

        it "renders top_p when explicitly configured" $ do
            requestBody <-
                captureOpenAIRequestBody
                    (withSampling (withTopP (Just 0.1) defaultSamplingOptions) defaultChatConfig)
                    [user "hello"]

            lookupPath ["top_p"] requestBody `shouldBe` Just (Number 0.1)

        it "omits top_p when not configured" $ do
            requestBody <-
                captureOpenAIRequestBody
                    defaultChatConfig
                    [user "hello"]

            lookupPath ["top_p"] requestBody `shouldBe` Nothing

    describe "OpenAI reasoning effort" $ do
        it "omits reasoning from default OpenAI requests" $ do
            requestBody <- captureOpenAIRequestBody defaultChatConfig [user "hello"]

            lookupPath ["reasoning"] requestBody `shouldBe` Nothing

        forM_ openAIReasoningEffortCases $ \(reasoningEffort, encodedEffort) ->
            it (toString ("encodes " <> show reasoningEffort <> " as " <> encodedEffort)) $ do
                requestBody <-
                    captureOpenAIRequestBodyWithReasoningEffort
                        reasoningEffort
                        defaultChatConfig
                        [user "hello"]

                lookupPath ["reasoning", "effort"] requestBody
                    `shouldBe` Just (String encodedEffort)

        it "renders OpenAIReasoningNone as the complete reasoning object" $ do
            requestBody <-
                captureOpenAIRequestBodyWithReasoningEffort
                    OpenAIReasoningNone
                    defaultChatConfig
                    [user "hello"]

            lookupPath ["reasoning"] requestBody
                `shouldBe` Just (object ["effort" .= ("none" :: Text)])

        it "keeps shared sampling fields unchanged and reasoning out of xAI requests" $ do
            let samplingOptions =
                    withTopP (Just 0.75)
                        $ withTemperature (Just 0.25) defaultSamplingOptions
                chatConfig = withSampling samplingOptions defaultChatConfig

            openAIRequestBody <-
                captureOpenAIRequestBodyWithReasoningEffort
                    OpenAIReasoningMedium
                    chatConfig
                    [user "hello"]
            xaiRequestBody <- captureXAIRequestBody chatConfig [user "hello"]

            forM_ ([openAIRequestBody, xaiRequestBody] :: [Value]) $ \requestBody -> do
                lookupPath ["temperature"] requestBody `shouldBe` Just (Number 0.25)
                lookupPath ["top_p"] requestBody `shouldBe` Just (Number 0.75)
                lookupPath ["store"] requestBody `shouldBe` Just (Bool False)
            lookupPath ["reasoning", "effort"] openAIRequestBody
                `shouldBe` Just (String "medium")
            lookupPath ["reasoning"] xaiRequestBody `shouldBe` Nothing

    describe "native history rendering" $ do
        it "projects OpenAI-native items into canonical OpenAI input" $ do
            requestBody <-
                captureOpenAIRequestBody
                    defaultChatConfig
                    [openAiNativeItem nativeResponsesAssistantPayload]

            lookupPath ["input"] requestBody `shouldBe` Just (toJSON ([projectedAssistantMessage] :: [Value]))

        it "projects xAI-native items into generic input for OpenAI requests" $ do
            requestBody <-
                captureOpenAIRequestBody
                    defaultChatConfig
                    [xaiNativeItem nativeResponsesAssistantPayload]

            lookupPath ["input"] requestBody `shouldBe` Just (toJSON ([projectedAssistantMessage] :: [Value]))

        it "projects xAI-native items into canonical xAI input" $ do
            requestBody <-
                captureXAIRequestBody
                    defaultChatConfig
                    [xaiNativeItem nativeResponsesAssistantPayload]

            lookupPath ["input"] requestBody `shouldBe` Just (toJSON ([projectedAssistantMessage] :: [Value]))

        it "projects OpenAI-native items into generic input for xAI requests" $ do
            requestBody <-
                captureXAIRequestBody
                    defaultChatConfig
                    [openAiNativeItem nativeResponsesAssistantPayload]

            lookupPath ["input"] requestBody `shouldBe` Just (toJSON ([projectedAssistantMessage] :: [Value]))

        it "projects native developer messages into the effective system snapshot" $ do
            let nativeDeveloperPayload =
                    object
                        [ "role" .= ("developer" :: Text)
                        , "content" .= ("authoritative state" :: Text)
                        ]
            requestBody <-
                captureXAIRequestBody
                    defaultChatConfig
                    [ openAiNativeItem nativeDeveloperPayload
                    , user "hello"
                    ]

            lookupPath ["input"] requestBody
                `shouldBe` Just
                    ( toJSON
                        ( [ object
                                [ "role" .= ("system" :: Text)
                                , "content" .= ("authoritative state" :: Text)
                                ]
                          , object
                                [ "role" .= ("user" :: Text)
                                , "content" .= ("hello" :: Text)
                                ]
                          ]
                            :: [Value]
                        )
                    )

        it "replays completed OpenAI non-portable items verbatim for later OpenAI requests" $ do
            requestBody <-
                captureOpenAIRequestBody
                    defaultChatConfig
                    [openAiNativeItem nativeResponsesReasoningPayload]

            lookupPath ["input"] requestBody
                `shouldBe` Just (toJSON ([nativeResponsesReasoningPayload] :: [Value]))

        it "drops completed OpenAI non-portable items when rendering xAI requests" $ do
            requestBody <-
                captureXAIRequestBody
                    defaultChatConfig
                    [openAiNativeItem nativeResponsesReasoningPayload]

            lookupPath ["input"] requestBody `shouldBe` Just (toJSON ([] :: [Value]))

        it "drops pending OpenAI-native assistant text from replay for OpenAI requests" $ do
            (requestBody, notes) <-
                captureOpenAIRender
                    defaultChatConfig
                    [pendingOpenAiNativeItem nativeResponsesAssistantPayload]

            notes `shouldBe` []
            lookupPath ["input"] requestBody `shouldBe` Just (toJSON ([] :: [Value]))

        it "drops pending OpenAI-native assistant text without a type field" $ do
            (requestBody, notes) <-
                captureOpenAIRender
                    defaultChatConfig
                    [pendingOpenAiNativeItem legacyResponsesAssistantPayload]

            notes `shouldBe` []
            lookupPath ["input"] requestBody `shouldBe` Just (toJSON ([] :: [Value]))

        it "renders refusal parts as assistant text for OpenAI requests" $ do
            requestBody <-
                captureOpenAIRequestBody
                    defaultChatConfig
                    [assistantParts [refusalPart "I can't help with that"]]

            lookupPath ["input"] requestBody
                `shouldBe` Just
                    ( toJSON
                        ( [ object
                                [ "role" .= ("assistant" :: Text)
                                , "content" .= ("I can't help with that" :: Text)
                                ]
                          ]
                            :: [Value]
                        )
                    )

        it "fails fast on OpenAI renders that contain generic image parts" $ do
            result <-
                runOpenAIRenderResult
                    defaultChatConfig
                    [userParts [imagePart "blob-image-1" (Just "image/png") (Just "diagram")]]

            result
                `shouldBe` Left
                    (LlmExpectationError "No stored media reference is available for blob blob-image-1 when rendering openai.responses")

        it "replays canonical OpenAI image history in-process when media refs were registered" $ do
            let payload =
                    responsesAssistantPayloadWithContent
                        "item-openai-image"
                        [ object
                            [ "type" .= ("input_image" :: Text)
                            , "image_url" .= ("https://example.com/cat.png" :: Text)
                            ]
                        ]
            decodedRound <-
                case decodeOpenAIResponse (responsesResponse "response-openai" "completed" [payload]) of
                    Left err -> expectationFailure ("Expected OpenAI response to decode: " <> show err) >> fail "unreachable"
                    Right roundValue -> pure roundValue

            let ProviderRound{roundItems, mediaReferences} = decodedRound
            requestBody <-
                captureOpenAIRequestBodyWithMediaReferences
                    mediaReferences
                    defaultChatConfig
                    (roundItems <> [user "what next?"])

            lookupPath ["input"] requestBody
                `shouldBe` Just
                    ( toJSON
                        ( [ object
                                [ "role" .= ("assistant" :: Text)
                                , "content"
                                    .= ( [ object
                                                [ "type" .= ("input_image" :: Text)
                                                , "image_url" .= ("https://example.com/cat.png" :: Text)
                                                ]
                                           ]
                                            :: [Value]
                                       )
                                ]
                          , object
                                [ "role" .= ("user" :: Text)
                                , "content" .= ("what next?" :: Text)
                                ]
                          ]
                            :: [Value]
                        )
                    )

        it "fails clearly on a fresh media store when replaying canonical OpenAI image history" $ do
            let payload =
                    responsesAssistantPayloadWithContent
                        "item-openai-image"
                        [ object
                            [ "type" .= ("input_image" :: Text)
                            , "image_url" .= ("https://example.com/cat.png" :: Text)
                            ]
                        ]
            decodedRound <-
                case decodeOpenAIResponse (responsesResponse "response-openai" "completed" [payload]) of
                    Left err -> expectationFailure ("Expected OpenAI response to decode: " <> show err) >> fail "unreachable"
                    Right roundValue -> pure roundValue

            let ProviderRound{roundItems} = decodedRound
            result <-
                runOpenAIRenderResult
                    defaultChatConfig
                    (roundItems <> [user "what next?"])

            result
                `shouldBe` Left
                    (LlmExpectationError "No stored media reference is available for blob openai.responses-response-openai-item-openai-image-0 when rendering openai.responses")

        it "replays generic audio history in-process when media refs were registered" $ do
            let requestPart =
                    object
                        [ "type" .= ("input_audio" :: Text)
                        , "input_audio"
                            .= object
                                [ "data" .= ("UklGRg==" :: Text)
                                , "format" .= ("wav" :: Text)
                                ]
                        ]
            requestBody <-
                captureOpenAIRequestBodyWithMediaReferences
                    [mediaReference "blob-audio-1" ProviderOpenAIResponses requestPart]
                    defaultChatConfig
                    [userParts [audioPart "blob-audio-1" (Just "audio/wav") (Just "spoken note")]]

            lookupPath ["input"] requestBody
                `shouldBe` Just
                    ( toJSON
                        ( [ object
                                [ "role" .= ("user" :: Text)
                                , "content" .= ([requestPart] :: [Value])
                                ]
                          ]
                            :: [Value]
                        )
                    )

        it "keeps media refs distinct when different Responses rounds reuse the same item id" $ do
            let firstPayload =
                    responsesAssistantPayloadWithContent
                        "shared-item"
                        [ object
                            [ "type" .= ("input_image" :: Text)
                            , "image_url" .= ("https://example.com/first.png" :: Text)
                            ]
                        ]
                secondPayload =
                    responsesAssistantPayloadWithContent
                        "shared-item"
                        [ object
                            [ "type" .= ("input_image" :: Text)
                            , "image_url" .= ("https://example.com/second.png" :: Text)
                            ]
                        ]
            firstRound <-
                case decodeOpenAIResponse (responsesResponse "response-openai-1" "completed" [firstPayload]) of
                    Left err -> expectationFailure ("Expected first OpenAI response to decode: " <> show err) >> fail "unreachable"
                    Right roundValue -> pure roundValue
            secondRound <-
                case decodeOpenAIResponse (responsesResponse "response-openai-2" "completed" [secondPayload]) of
                    Left err -> expectationFailure ("Expected second OpenAI response to decode: " <> show err) >> fail "unreachable"
                    Right roundValue -> pure roundValue

            let ProviderRound{roundItems = firstItems, mediaReferences = firstMediaReferences} = firstRound
                ProviderRound{roundItems = secondItems, mediaReferences = secondMediaReferences} = secondRound
                allMediaReferences = firstMediaReferences <> secondMediaReferences

            firstRequestBody <-
                captureOpenAIRequestBodyWithMediaReferences
                    allMediaReferences
                    defaultChatConfig
                    (firstItems <> [user "what next?"])
            secondRequestBody <-
                captureOpenAIRequestBodyWithMediaReferences
                    allMediaReferences
                    defaultChatConfig
                    (secondItems <> [user "what next?"])

            lookupPath ["input"] firstRequestBody
                `shouldBe` Just
                    ( toJSON
                        ( [ object
                                [ "role" .= ("assistant" :: Text)
                                , "content"
                                    .= ( [ object
                                                [ "type" .= ("input_image" :: Text)
                                                , "image_url" .= ("https://example.com/first.png" :: Text)
                                                ]
                                           ]
                                            :: [Value]
                                       )
                                ]
                          , object
                                [ "role" .= ("user" :: Text)
                                , "content" .= ("what next?" :: Text)
                                ]
                          ]
                            :: [Value]
                        )
                    )
            lookupPath ["input"] secondRequestBody
                `shouldBe` Just
                    ( toJSON
                        ( [ object
                                [ "role" .= ("assistant" :: Text)
                                , "content"
                                    .= ( [ object
                                                [ "type" .= ("input_image" :: Text)
                                                , "image_url" .= ("https://example.com/second.png" :: Text)
                                                ]
                                           ]
                                            :: [Value]
                                       )
                                ]
                          , object
                                [ "role" .= ("user" :: Text)
                                , "content" .= ("what next?" :: Text)
                                ]
                          ]
                            :: [Value]
                        )
                    )

        it "projects Gemini-native items into canonical Gemini input" $ do
            requestBody <-
                captureGeminiRequestBody
                    defaultChatConfig
                    [geminiNativeItem nativeGeminiTextPayload]

            lookupPath ["input"] requestBody
                `shouldBe` Just
                    ( toJSON
                        ( [ object
                                [ "id" .= ("item-gemini" :: Text)
                                , "type" .= ("model_output" :: Text)
                                , "content"
                                    .= ( [ object
                                                [ "type" .= ("text" :: Text)
                                                , "text" .= ("native assistant text" :: Text)
                                                ]
                                           ]
                                            :: [Value]
                                       )
                                ]
                          ]
                            :: [Value]
                        )
                    )

        it "projects Gemini-native items into generic input for OpenAI requests" $ do
            requestBody <-
                captureOpenAIRequestBody
                    defaultChatConfig
                    [geminiNativeItem nativeGeminiTextPayload]

            lookupPath ["input"] requestBody `shouldBe` Just (toJSON ([projectedAssistantMessage] :: [Value]))

        it "drops pending Gemini-native assistant text when projecting into OpenAI requests" $ do
            (requestBody, notes) <-
                captureOpenAIRender
                    defaultChatConfig
                    [pendingGeminiNativeItem nativeGeminiTextPayload]

            notes `shouldBe` []
            lookupPath ["input"] requestBody `shouldBe` Just (toJSON ([] :: [Value]))

        it "drops pending Gemini-native assistant text even for Gemini requests" $ do
            requestBody <-
                captureGeminiRequestBody
                    defaultChatConfig
                    [pendingGeminiNativeItem nativeGeminiTextPayload]

            lookupPath ["input"] requestBody `shouldBe` Just (toJSON ([] :: [Value]))

        it "replays completed Gemini non-portable artifacts on later Gemini requests" $ do
            (requestBody, notes) <-
                captureGeminiRender
                    defaultChatConfig
                    [geminiNativeItem (geminiThoughtPayload "thought-1")]

            lookupPath ["input"] requestBody
                `shouldBe` Just
                    ( toJSON
                        ( [ object
                                [ "signature" .= ("thought-1" :: Text)
                                , "type" .= ("thought" :: Text)
                                ]
                          ]
                            :: [Value]
                        )
                    )
            notes `shouldBe` []

        it "drops completed Gemini non-portable artifacts when rendering OpenAI requests" $ do
            requestBody <-
                captureOpenAIRequestBody
                    defaultChatConfig
                    [geminiNativeItem (geminiThoughtPayload "thought-1")]

            lookupPath ["input"] requestBody `shouldBe` Just (toJSON ([] :: [Value]))

        it "drops pending Gemini-native assistant text without a type field even for Gemini requests" $ do
            requestBody <-
                captureGeminiRequestBody
                    defaultChatConfig
                    [pendingGeminiNativeItem legacyGeminiTextPayload]

            lookupPath ["input"] requestBody `shouldBe` Just (toJSON ([] :: [Value]))

        it "fails fast on Gemini renders that contain generic audio parts" $ do
            result <-
                runGeminiRenderResult
                    defaultChatConfig
                    [userParts [audioPart "blob-audio-1" (Just "audio/mpeg") (Just "spoken note")]]

            result
                `shouldBe` Left
                    (LlmExpectationError "No stored media reference is available for blob blob-audio-1 when rendering gemini.interactions")

        it "renders Gemini continuation attachments alongside resolved tool calls" $ do
            let pendingToolCall = pendingGeminiToolCallWithThoughtItem "John Snow"

            requestBody <-
                captureGeminiRequestBody
                    defaultChatConfig
                    [ pendingToolCall
                    , toolResultText "tool-call-1" "Contacts:\n- John Snow"
                    ]

            lookupPath ["input"] requestBody
                `shouldBe` Just
                    ( toJSON
                        ( [ object
                                [ "signature" .= ("thought-1" :: Text)
                                , "type" .= ("thought" :: Text)
                                ]
                          , geminiFunctionCallPayload "tool-call-1" "lookup" (object ["name" .= ("John Snow" :: Text)])
                          , object
                                [ "type" .= ("function_result" :: Text)
                                , "name" .= ("lookup" :: Text)
                                , "call_id" .= ("tool-call-1" :: Text)
                                , "result"
                                    .= ( [ object
                                                [ "type" .= ("text" :: Text)
                                                , "text" .= ("Contacts:\n- John Snow" :: Text)
                                                ]
                                           ]
                                            :: [Value]
                                       )
                                ]
                          ]
                            :: [Value]
                        )
                    )

        it "renders foreign tool continuations without an invalid thought signature" $ do
            let pendingForeignToolCall =
                    nativeHistoryItem
                        ProviderOpenAIResponses
                        ItemPending
                        "response-openai"
                        (Just "item-openai-tool")
                        (responsesToolCallPayload "item-openai-tool" "tool-call-1" "lookup" (object []) "pending")

            requestBody <-
                captureGeminiRequestBody
                    defaultChatConfig
                    [ pendingForeignToolCall
                    , toolResultText "tool-call-1" "Contacts:\n- Ada"
                    ]

            lookupPath ["input"] requestBody
                `shouldBe` Just
                    ( toJSON
                        ( [ object
                                [ "type" .= ("function_call" :: Text)
                                , "id" .= ("tool-call-1" :: Text)
                                , "name" .= ("lookup" :: Text)
                                , "arguments" .= object []
                                ]
                          , object
                                [ "type" .= ("function_result" :: Text)
                                , "name" .= ("lookup" :: Text)
                                , "call_id" .= ("tool-call-1" :: Text)
                                , "result"
                                    .= ( [ object
                                                [ "type" .= ("text" :: Text)
                                                , "text" .= ("Contacts:\n- Ada" :: Text)
                                                ]
                                           ]
                                            :: [Value]
                                       )
                                ]
                          ]
                            :: [Value]
                        )
                    )

        it "drops Gemini continuation attachments but keeps portable Gemini tool continuations for OpenAI requests" $ do
            let pendingToolCall = pendingGeminiToolCallWithThoughtItem "John Snow"
                expectedInput =
                    [ object
                        [ "type" .= ("function_call" :: Text)
                        , "call_id" .= ("tool-call-1" :: Text)
                        , "name" .= ("lookup" :: Text)
                        , "arguments" .= ("{\"name\":\"John Snow\"}" :: Text)
                        ]
                    , object
                        [ "type" .= ("function_call_output" :: Text)
                        , "call_id" .= ("tool-call-1" :: Text)
                        , "output" .= ("Contacts:\n- John Snow" :: Text)
                        ]
                    ]

            requestBody <-
                captureOpenAIRequestBody
                    defaultChatConfig
                    [ pendingToolCall
                    , toolResultText "tool-call-1" "Contacts:\n- John Snow"
                    ]

            lookupPath ["input"] requestBody `shouldBe` Just (toJSON (expectedInput :: [Value]))

        it "drops Gemini continuation attachments but keeps portable Gemini tool continuations for xAI requests" $ do
            let pendingToolCall = pendingGeminiToolCallWithThoughtItem "John Snow"
                expectedInput =
                    [ object
                        [ "type" .= ("function_call" :: Text)
                        , "call_id" .= ("tool-call-1" :: Text)
                        , "name" .= ("lookup" :: Text)
                        , "arguments" .= ("{\"name\":\"John Snow\"}" :: Text)
                        ]
                    , object
                        [ "type" .= ("function_call_output" :: Text)
                        , "call_id" .= ("tool-call-1" :: Text)
                        , "output" .= ("Contacts:\n- John Snow" :: Text)
                        ]
                    ]

            requestBody <-
                captureXAIRequestBody
                    defaultChatConfig
                    [ pendingToolCall
                    , toolResultText "tool-call-1" "Contacts:\n- John Snow"
                    ]

            lookupPath ["input"] requestBody `shouldBe` Just (toJSON (expectedInput :: [Value]))

    describe "multipart shared rendering" $ do
        it "renders shared history without conversion notes for OpenAI requests" $ do
            (requestBody, notes) <-
                captureOpenAIRender
                    defaultChatConfig
                    sharedHistory

            notes `shouldBe` []
            lookupPath ["input"] requestBody `shouldBe` Just (toJSON sharedHistoryRequest)

        it "renders shared history without conversion notes for xAI requests" $ do
            (requestBody, notes) <-
                captureXAIRender
                    defaultChatConfig
                    sharedHistory

            notes `shouldBe` []
            lookupPath ["input"] requestBody `shouldBe` Just (toJSON sharedHistoryRequest)

        it "renders shared history for Gemini with instruction collapsing" $ do
            (requestBody, notes) <-
                captureGeminiRender
                    defaultChatConfig
                    sharedHistory

            notes `shouldBe` []
            lookupPath ["system_instruction"] requestBody `shouldBe` Just (String sharedGeminiSystemInstruction)
            lookupPath ["input"] requestBody `shouldBe` Just (toJSON sharedGeminiHistoryRequest)

        it "renders only the latest system snapshot for OpenAI requests" $ do
            requestBody <-
                captureOpenAIRequestBody
                    defaultChatConfig
                    [ system "old system snapshot"
                    , user "hello"
                    , assistantText "working"
                    , system "new system snapshot"
                    ]

            lookupPath ["input"] requestBody
                `shouldBe` Just
                    ( toJSON
                        ( [ object
                                [ "role" .= ("system" :: Text)
                                , "content" .= ("new system snapshot" :: Text)
                                ]
                          , object
                                [ "role" .= ("user" :: Text)
                                , "content" .= ("hello" :: Text)
                                ]
                          , object
                                [ "role" .= ("assistant" :: Text)
                                , "content" .= ("working" :: Text)
                                ]
                          ]
                            :: [Value]
                        )
                    )
            countSystemRoleMessages requestBody `shouldBe` Just 1

        it "renders only the latest system snapshot for xAI requests" $ do
            requestBody <-
                captureXAIRequestBody
                    defaultChatConfig
                    [ system "old system snapshot"
                    , user "hello"
                    , assistantText "working"
                    , system "new system snapshot"
                    ]

            lookupPath ["input"] requestBody
                `shouldBe` Just
                    ( toJSON
                        ( [ object
                                [ "role" .= ("system" :: Text)
                                , "content" .= ("new system snapshot" :: Text)
                                ]
                          , object
                                [ "role" .= ("user" :: Text)
                                , "content" .= ("hello" :: Text)
                                ]
                          , object
                                [ "role" .= ("assistant" :: Text)
                                , "content" .= ("working" :: Text)
                                ]
                          ]
                            :: [Value]
                        )
                    )

        it "renders only the latest system snapshot for Gemini requests" $ do
            (requestBody, notes) <-
                captureGeminiRender
                    defaultChatConfig
                    [ system "old system snapshot"
                    , user "hello"
                    , system "new system snapshot"
                    ]

            notes `shouldBe` []
            lookupPath ["system_instruction"] requestBody `shouldBe` Just (String "new system snapshot")
            lookupPath ["input"] requestBody
                `shouldBe` Just
                    ( toJSON
                        ( [ object
                                [ "type" .= ("user_input" :: Text)
                                , "content"
                                    .= ( [ object
                                                [ "type" .= ("text" :: Text)
                                                , "text" .= ("hello" :: Text)
                                                ]
                                           ]
                                            :: [Value]
                                       )
                                ]
                          ]
                            :: [Value]
                        )
                    )

        it "renders JSON tool results as JSON text for completed tool exchanges" $ do
            requestBody <-
                captureOpenAIRequestBody
                    defaultChatConfig
                    [ toolCall "tool-call-1" "lookup" (fromList [("name", String "Ada")])
                    , toolResultJson "tool-call-1" (String "ok")
                    , toolCall "tool-call-2" "lookup" (fromList [("name", String "Grace")])
                    , toolResultJson "tool-call-2" (object ["answer" .= (4 :: Int)])
                    ]

            lookupPath ["input"] requestBody
                `shouldBe` Just
                    ( toJSON
                        ( [ object
                                [ "type" .= ("function_call" :: Text)
                                , "call_id" .= ("tool-call-1" :: Text)
                                , "name" .= ("lookup" :: Text)
                                , "arguments" .= ("{\"name\":\"Ada\"}" :: Text)
                                ]
                          , object
                                [ "type" .= ("function_call_output" :: Text)
                                , "call_id" .= ("tool-call-1" :: Text)
                                , "output" .= ("\"ok\"" :: Text)
                                ]
                          , object
                                [ "type" .= ("function_call" :: Text)
                                , "call_id" .= ("tool-call-2" :: Text)
                                , "name" .= ("lookup" :: Text)
                                , "arguments" .= ("{\"name\":\"Grace\"}" :: Text)
                                ]
                          , object
                                [ "type" .= ("function_call_output" :: Text)
                                , "call_id" .= ("tool-call-2" :: Text)
                                , "output" .= ("{\"answer\":4}" :: Text)
                                ]
                          ]
                            :: [Value]
                        )
                    )

    describe "tool response parsing" $ do
        it "parses any valid JSON tool output string as JSON" $ do
            forM_ scalarAndCompositeJsonCases $ \outputText ->
                itemTexts (openAiNativeItem (nativeToolResultPayload outputText))
                    `shouldBe` []

        it "keeps non-JSON tool output strings as text" $ do
            itemTexts (openAiNativeItem (nativeToolResultPayload "hello"))
                `shouldBe` ["hello"]

        it "parses Gemini function result strings as text" $ do
            itemTexts (geminiNativeItem (nativeGeminiToolResultPayload "hello"))
                `shouldBe` ["hello"]

    describe "response decoding" $ do
        it "decodes completed OpenAI assistant responses as completed rounds" $ do
            let payload = responsesAssistantPayload "item-openai" "native assistant text"

            decodeOpenAIResponse (responsesResponse "response-openai" "completed" [payload])
                `shouldBe` Right
                    (providerRound [nativeHistoryItem ProviderOpenAIResponses ItemCompleted "response-openai" (Just "item-openai") payload] [] ProviderRoundDone)

        it "decodes completed xAI assistant responses as completed rounds" $ do
            let payload = responsesAssistantPayload "item-xai" "native assistant text"

            decodeXAIResponse (responsesResponse "response-xai" "completed" [payload])
                `shouldBe` Right
                    (providerRound [nativeHistoryItem ProviderXAIResponses ItemCompleted "response-xai" (Just "item-xai") payload] [] ProviderRoundDone)

        it "decodes OpenAI refusal message parts into canonical refusal parts" $ do
            let payload =
                    object
                        [ "id" .= ("item-openai-refusal" :: Text)
                        , "type" .= ("message" :: Text)
                        , "role" .= ("assistant" :: Text)
                        , "content"
                            .= ( [ object
                                        [ "type" .= ("refusal" :: Text)
                                        , "refusal" .= ("I can't help with that" :: Text)
                                        ]
                                   ]
                                    :: [Value]
                               )
                        ]

            decodeOpenAIResponse (responsesResponse "response-openai" "completed" [payload])
                `shouldBe` Right
                    (providerRound [nativeHistoryItem ProviderOpenAIResponses ItemCompleted "response-openai" (Just "item-openai-refusal") payload] [] ProviderRoundDone)

        it "decodes completed OpenAI image-only assistant responses into canonical image parts" $ do
            let payload =
                    responsesAssistantPayloadWithContent
                        "item-openai-image"
                        [ object
                            [ "type" .= ("input_image" :: Text)
                            , "image_url" .= ("https://example.com/cat.png" :: Text)
                            ]
                        ]
                expectedItem =
                    HistoryItem
                        { historyItemIdField = Nothing
                        , itemLifecycle = ItemCompleted
                        , genericItem =
                            GenericMessage
                                { role = GenericAssistant
                                , parts = [imagePart "openai.responses-response-openai-item-openai-image-0" Nothing Nothing]
                                }
                        , providerItem =
                            Just
                                ProviderItem
                                    { apiFamily = ProviderOpenAIResponses
                                    , exchangeId = Just "response-openai"
                                    , nativeItemId = Just "item-openai-image"
                                    , payload
                                    , availableLocalTools = []
                                    }
                        }

            decodeOpenAIResponse (responsesResponse "response-openai" "completed" [payload])
                `shouldBe` Right
                    ( providerRound
                        [expectedItem]
                        [ mediaReference
                            "openai.responses-response-openai-item-openai-image-0"
                            ProviderOpenAIResponses
                            ( object
                                [ "type" .= ("input_image" :: Text)
                                , "image_url" .= ("https://example.com/cat.png" :: Text)
                                ]
                            )
                        ]
                        ProviderRoundDone
                    )

        it "decodes completed OpenAI audio-only assistant responses into canonical audio parts" $ do
            let payload =
                    responsesAssistantPayloadWithContent
                        "item-openai-audio"
                        [ object
                            [ "type" .= ("input_audio" :: Text)
                            , "input_audio"
                                .= object
                                    [ "data" .= ("UklGRg==" :: Text)
                                    , "format" .= ("wav" :: Text)
                                    ]
                            , "transcript" .= ("spoken note" :: Text)
                            ]
                        ]
                expectedItem =
                    HistoryItem
                        { historyItemIdField = Nothing
                        , itemLifecycle = ItemCompleted
                        , genericItem =
                            GenericMessage
                                { role = GenericAssistant
                                , parts = [audioPart "openai.responses-response-openai-item-openai-audio-0" Nothing (Just "spoken note")]
                                }
                        , providerItem =
                            Just
                                ProviderItem
                                    { apiFamily = ProviderOpenAIResponses
                                    , exchangeId = Just "response-openai"
                                    , nativeItemId = Just "item-openai-audio"
                                    , payload
                                    , availableLocalTools = []
                                    }
                        }

            decodeOpenAIResponse (responsesResponse "response-openai" "completed" [payload])
                `shouldBe` Right
                    ( providerRound
                        [expectedItem]
                        [ mediaReference
                            "openai.responses-response-openai-item-openai-audio-0"
                            ProviderOpenAIResponses
                            ( object
                                [ "type" .= ("input_audio" :: Text)
                                , "input_audio"
                                    .= object
                                        [ "data" .= ("UklGRg==" :: Text)
                                        , "format" .= ("wav" :: Text)
                                        ]
                                , "transcript" .= ("spoken note" :: Text)
                                ]
                            )
                        ]
                        ProviderRoundDone
                    )

        it "decodes completed xAI file-only assistant responses into canonical file parts" $ do
            let payload =
                    responsesAssistantPayloadWithContent
                        "item-xai-file"
                        [ object
                            [ "type" .= ("input_file" :: Text)
                            , "file_id" .= ("file-123" :: Text)
                            , "filename" .= ("report.pdf" :: Text)
                            ]
                        ]
                expectedItem =
                    HistoryItem
                        { historyItemIdField = Nothing
                        , itemLifecycle = ItemCompleted
                        , genericItem =
                            GenericMessage
                                { role = GenericAssistant
                                , parts = [filePart "xai.responses-response-xai-item-xai-file-0" Nothing (Just "report.pdf")]
                                }
                        , providerItem =
                            Just
                                ProviderItem
                                    { apiFamily = ProviderXAIResponses
                                    , exchangeId = Just "response-xai"
                                    , nativeItemId = Just "item-xai-file"
                                    , payload
                                    , availableLocalTools = []
                                    }
                        }

            decodeXAIResponse (responsesResponse "response-xai" "completed" [payload])
                `shouldBe` Right
                    ( providerRound
                        [expectedItem]
                        [ mediaReference
                            "xai.responses-response-xai-item-xai-file-0"
                            ProviderXAIResponses
                            ( object
                                [ "type" .= ("input_file" :: Text)
                                , "file_id" .= ("file-123" :: Text)
                                , "filename" .= ("report.pdf" :: Text)
                                ]
                            )
                        ]
                        ProviderRoundDone
                    )

        it "decodes completed xAI audio-only assistant responses into canonical audio parts" $ do
            let payload =
                    responsesAssistantPayloadWithContent
                        "item-xai-audio"
                        [ object
                            [ "type" .= ("output_audio" :: Text)
                            , "audio_url" .= ("https://example.com/audio.mp3" :: Text)
                            , "transcript" .= ("spoken note" :: Text)
                            ]
                        ]
                expectedItem =
                    HistoryItem
                        { historyItemIdField = Nothing
                        , itemLifecycle = ItemCompleted
                        , genericItem =
                            GenericMessage
                                { role = GenericAssistant
                                , parts = [audioPart "xai.responses-response-xai-item-xai-audio-0" Nothing (Just "spoken note")]
                                }
                        , providerItem =
                            Just
                                ProviderItem
                                    { apiFamily = ProviderXAIResponses
                                    , exchangeId = Just "response-xai"
                                    , nativeItemId = Just "item-xai-audio"
                                    , payload
                                    , availableLocalTools = []
                                    }
                        }

            decodeXAIResponse (responsesResponse "response-xai" "completed" [payload])
                `shouldBe` Right
                    ( providerRound
                        [expectedItem]
                        [ mediaReference
                            "xai.responses-response-xai-item-xai-audio-0"
                            ProviderXAIResponses
                            ( object
                                [ "type" .= ("output_audio" :: Text)
                                , "audio_url" .= ("https://example.com/audio.mp3" :: Text)
                                , "transcript" .= ("spoken note" :: Text)
                                ]
                            )
                        ]
                        ProviderRoundDone
                    )

        it "canonicalizes untyped OpenAI image parts before storing replay refs" $ do
            let payload =
                    responsesAssistantPayloadWithContent
                        "item-openai-untyped-image"
                        [ object
                            [ "image_url" .= ("https://example.com/cat.png" :: Text)
                            ]
                        ]
                expectedItem =
                    HistoryItem
                        { historyItemIdField = Nothing
                        , itemLifecycle = ItemCompleted
                        , genericItem =
                            GenericMessage
                                { role = GenericAssistant
                                , parts = [imagePart "openai.responses-response-openai-item-openai-untyped-image-0" Nothing Nothing]
                                }
                        , providerItem =
                            Just
                                ProviderItem
                                    { apiFamily = ProviderOpenAIResponses
                                    , exchangeId = Just "response-openai"
                                    , nativeItemId = Just "item-openai-untyped-image"
                                    , payload
                                    , availableLocalTools = []
                                    }
                        }

            decodeOpenAIResponse (responsesResponse "response-openai" "completed" [payload])
                `shouldBe` Right
                    ( providerRound
                        [expectedItem]
                        [ mediaReference
                            "openai.responses-response-openai-item-openai-untyped-image-0"
                            ProviderOpenAIResponses
                            ( object
                                [ "type" .= ("input_image" :: Text)
                                , "image_url" .= ("https://example.com/cat.png" :: Text)
                                ]
                            )
                        ]
                        ProviderRoundDone
                    )

        it "canonicalizes untyped xAI file parts before storing replay refs" $ do
            let payload =
                    responsesAssistantPayloadWithContent
                        "item-xai-untyped-file"
                        [ object
                            [ "file_id" .= ("file-123" :: Text)
                            , "filename" .= ("report.pdf" :: Text)
                            ]
                        ]
                expectedItem =
                    HistoryItem
                        { historyItemIdField = Nothing
                        , itemLifecycle = ItemCompleted
                        , genericItem =
                            GenericMessage
                                { role = GenericAssistant
                                , parts = [filePart "xai.responses-response-xai-item-xai-untyped-file-0" Nothing (Just "report.pdf")]
                                }
                        , providerItem =
                            Just
                                ProviderItem
                                    { apiFamily = ProviderXAIResponses
                                    , exchangeId = Just "response-xai"
                                    , nativeItemId = Just "item-xai-untyped-file"
                                    , payload
                                    , availableLocalTools = []
                                    }
                        }

            decodeXAIResponse (responsesResponse "response-xai" "completed" [payload])
                `shouldBe` Right
                    ( providerRound
                        [expectedItem]
                        [ mediaReference
                            "xai.responses-response-xai-item-xai-untyped-file-0"
                            ProviderXAIResponses
                            ( object
                                [ "type" .= ("input_file" :: Text)
                                , "file_id" .= ("file-123" :: Text)
                                , "filename" .= ("report.pdf" :: Text)
                                ]
                            )
                        ]
                        ProviderRoundDone
                    )

        it "preserves mixed OpenAI assistant text and audio parts in canonical order" $ do
            let payload =
                    responsesAssistantPayloadWithContent
                        "item-openai-mixed-audio"
                        [ object
                            [ "type" .= ("output_text" :: Text)
                            , "text" .= ("before" :: Text)
                            ]
                        , object
                            [ "type" .= ("input_audio" :: Text)
                            , "input_audio"
                                .= object
                                    [ "data" .= ("UklGRg==" :: Text)
                                    , "format" .= ("wav" :: Text)
                                    ]
                            , "transcript" .= ("spoken note" :: Text)
                            ]
                        , object
                            [ "type" .= ("output_text" :: Text)
                            , "text" .= ("after" :: Text)
                            ]
                        ]
                expectedItem =
                    HistoryItem
                        { historyItemIdField = Nothing
                        , itemLifecycle = ItemCompleted
                        , genericItem =
                            GenericMessage
                                { role = GenericAssistant
                                , parts =
                                    [ textPart "before"
                                    , audioPart "openai.responses-response-openai-item-openai-mixed-audio-1" Nothing (Just "spoken note")
                                    , textPart "after"
                                    ]
                                }
                        , providerItem =
                            Just
                                ProviderItem
                                    { apiFamily = ProviderOpenAIResponses
                                    , exchangeId = Just "response-openai"
                                    , nativeItemId = Just "item-openai-mixed-audio"
                                    , payload
                                    , availableLocalTools = []
                                    }
                        }

            decodeOpenAIResponse (responsesResponse "response-openai" "completed" [payload])
                `shouldBe` Right
                    ( providerRound
                        [expectedItem]
                        [ mediaReference
                            "openai.responses-response-openai-item-openai-mixed-audio-1"
                            ProviderOpenAIResponses
                            ( object
                                [ "type" .= ("input_audio" :: Text)
                                , "input_audio"
                                    .= object
                                        [ "data" .= ("UklGRg==" :: Text)
                                        , "format" .= ("wav" :: Text)
                                        ]
                                , "transcript" .= ("spoken note" :: Text)
                                ]
                            )
                        ]
                        ProviderRoundDone
                    )

        it "preserves mixed OpenAI assistant message parts in canonical order" $ do
            let payload =
                    responsesAssistantPayloadWithContent
                        "item-openai-mixed"
                        [ object
                            [ "type" .= ("output_text" :: Text)
                            , "text" .= ("before" :: Text)
                            ]
                        , object
                            [ "type" .= ("input_image" :: Text)
                            , "image_url" .= ("https://example.com/diagram.png" :: Text)
                            ]
                        , object
                            [ "type" .= ("refusal" :: Text)
                            , "refusal" .= ("I can't help with that part" :: Text)
                            ]
                        , object
                            [ "type" .= ("input_file" :: Text)
                            , "file_id" .= ("file-456" :: Text)
                            , "filename" .= ("appendix.txt" :: Text)
                            ]
                        ]
                expectedItem =
                    HistoryItem
                        { historyItemIdField = Nothing
                        , itemLifecycle = ItemCompleted
                        , genericItem =
                            GenericMessage
                                { role = GenericAssistant
                                , parts =
                                    [ textPart "before"
                                    , imagePart "openai.responses-response-openai-item-openai-mixed-1" Nothing Nothing
                                    , refusalPart "I can't help with that part"
                                    , filePart "openai.responses-response-openai-item-openai-mixed-3" Nothing (Just "appendix.txt")
                                    ]
                                }
                        , providerItem =
                            Just
                                ProviderItem
                                    { apiFamily = ProviderOpenAIResponses
                                    , exchangeId = Just "response-openai"
                                    , nativeItemId = Just "item-openai-mixed"
                                    , payload
                                    , availableLocalTools = []
                                    }
                        }

            decodeOpenAIResponse (responsesResponse "response-openai" "completed" [payload])
                `shouldBe` Right
                    ( providerRound
                        [expectedItem]
                        [ mediaReference
                            "openai.responses-response-openai-item-openai-mixed-1"
                            ProviderOpenAIResponses
                            ( object
                                [ "type" .= ("input_image" :: Text)
                                , "image_url" .= ("https://example.com/diagram.png" :: Text)
                                ]
                            )
                        , mediaReference
                            "openai.responses-response-openai-item-openai-mixed-3"
                            ProviderOpenAIResponses
                            ( object
                                [ "type" .= ("input_file" :: Text)
                                , "file_id" .= ("file-456" :: Text)
                                , "filename" .= ("appendix.txt" :: Text)
                                ]
                            )
                        ]
                        ProviderRoundDone
                    )

        it "marks OpenAI tool handoff rounds as pending even when the response is still in progress" $ do
            let payload =
                    responsesToolCallPayload
                        "item-openai-tool"
                        "tool-call-1"
                        "lookup"
                        (object ["name" .= ("Ada" :: Text)])
                        "in_progress"
                expectedToolCall =
                    ToolCall
                        { toolCallId = "tool-call-1"
                        , toolName = "lookup"
                        , toolArgs = fromList [("name", String "Ada")]
                        , continuationAttachments = []
                        }

            decodeOpenAIResponse (responsesResponse "response-openai" "in_progress" [payload])
                `shouldBe` Right
                    (providerRound [nativeHistoryItem ProviderOpenAIResponses ItemPending "response-openai" (Just "item-openai-tool") payload] [] (ProviderRoundNeedsLocalTools [expectedToolCall]))

        it "marks xAI tool handoff rounds as pending even when the item status is still in progress" $ do
            let payload =
                    responsesToolCallPayload
                        "item-xai-tool"
                        "tool-call-1"
                        "lookup"
                        (object ["name" .= ("Ada" :: Text)])
                        "in_progress"
                expectedToolCall =
                    ToolCall
                        { toolCallId = "tool-call-1"
                        , toolName = "lookup"
                        , toolArgs = fromList [("name", String "Ada")]
                        , continuationAttachments = []
                        }

            decodeXAIResponse (responsesResponse "response-xai" "completed" [payload])
                `shouldBe` Right
                    (providerRound [nativeHistoryItem ProviderXAIResponses ItemPending "response-xai" (Just "item-xai-tool") payload] [] (ProviderRoundNeedsLocalTools [expectedToolCall]))

        it "keeps incomplete OpenAI responses in pending history instead of completing them" $ do
            let payload = responsesAssistantPayload "item-openai" "working on it"

            decodeOpenAIResponse (responsesResponse "response-openai" "incomplete" [payload])
                `shouldBe` Right
                    ( providerRound
                        [nativeHistoryItem ProviderOpenAIResponses ItemPending "response-openai" (Just "item-openai") payload]
                        []
                        (ProviderRoundPaused (PauseIncomplete "Responses response status was incomplete"))
                    )

        it "fails terminal xAI responses" $ do
            decodeXAIResponse (responsesResponse "response-xai" "failed" [])
                `shouldBe` Right
                    (providerRound [] [] (ProviderRoundFailed (FailureProvider "Responses response status was failed")))

        it "propagates top-level Responses failure details" $ do
            let response =
                    object
                        [ "id" .= ("response-openai" :: Text)
                        , "status" .= ("failed" :: Text)
                        , "error"
                            .= object
                                [ "code" .= ("too_many_tools" :: Text)
                                , "message" .= ("The request contains too many tools." :: Text)
                                ]
                        , "output" .= ([] :: [Value])
                        ]

            decodeOpenAIResponse response
                `shouldBe` Right
                    ( providerRound
                        []
                        []
                        ( ProviderRoundFailed
                            ( FailureProvider
                                "Responses response status was failed: too_many_tools: The request contains too many tools."
                            )
                        )
                    )

        it "fails mixed Responses rounds when any item has terminal failure status, even if a tool call is present" $ do
            let toolPayload =
                    responsesToolCallPayload
                        "item-openai-tool"
                        "tool-call-1"
                        "lookup"
                        (object ["name" .= ("Ada" :: Text)])
                        "completed"
                failedPayload =
                    responsesAssistantPayloadWithStatus
                        "item-openai-message"
                        "native assistant text"
                        "failed"

            decodeOpenAIResponse (responsesResponse "response-openai" "completed" [toolPayload, failedPayload])
                `shouldBe` Right
                    ( providerRound
                        [ nativeHistoryItem ProviderOpenAIResponses ItemPending "response-openai" (Just "item-openai-tool") toolPayload
                        , nativeHistoryItem ProviderOpenAIResponses ItemPending "response-openai" (Just "item-openai-message") failedPayload
                        ]
                        []
                        (ProviderRoundFailed (FailureProvider "Responses output item status was failed"))
                    )

        it "propagates Responses output item failure details" $ do
            let failedPayload =
                    object
                        [ "id" .= ("item-openai-message" :: Text)
                        , "type" .= ("message" :: Text)
                        , "status" .= ("failed" :: Text)
                        , "error"
                            .= object
                                [ "code" .= ("tool_schema_invalid" :: Text)
                                , "message" .= ("Tool schema is invalid." :: Text)
                                ]
                        ]

            decodeOpenAIResponse (responsesResponse "response-openai" "completed" [failedPayload])
                `shouldBe` Right
                    ( providerRound
                        [nativeHistoryItem ProviderOpenAIResponses ItemPending "response-openai" (Just "item-openai-message") failedPayload]
                        []
                        ( ProviderRoundFailed
                            ( FailureProvider
                                "Responses output item status was failed: tool_schema_invalid: Tool schema is invalid."
                            )
                        )
                    )

        it "decodes completed Gemini assistant responses as completed rounds" $ do
            let payload = geminiTextPayloadWithId "item-gemini" "native assistant text"

            decodeGeminiResponse (geminiResponse "interaction-gemini" "completed" [payload])
                `shouldBe` Right
                    (providerRound [nativeHistoryItem ProviderGeminiInteractions ItemCompleted "interaction-gemini" (Just "item-gemini") payload] [] ProviderRoundDone)

        it "continues to decode legacy Gemini outputs and flat text payloads" $ do
            let payload =
                    object
                        [ "id" .= ("item-gemini" :: Text)
                        , "type" .= ("text" :: Text)
                        , "text" .= ("legacy assistant text" :: Text)
                        ]
                response =
                    object
                        [ "id" .= ("interaction-gemini" :: Text)
                        , "status" .= ("completed" :: Text)
                        , "outputs" .= ([payload] :: [Value])
                        ]

            decodeGeminiResponse response
                `shouldBe` Right
                    (providerRound [nativeHistoryItem ProviderGeminiInteractions ItemCompleted "interaction-gemini" (Just "item-gemini") payload] [] ProviderRoundDone)

        it "decodes Gemini requires_action rounds as pending tool handoff" $ do
            let payload =
                    geminiFunctionCallPayload
                        "item-gemini-tool"
                        "lookup"
                        (object ["name" .= ("Ada" :: Text)])
                expectedToolCall =
                    ToolCall
                        { toolCallId = "item-gemini-tool"
                        , toolName = "lookup"
                        , toolArgs = fromList [("name", String "Ada")]
                        , continuationAttachments = []
                        }

            decodeGeminiResponse (geminiResponse "interaction-gemini" "requires_action" [payload])
                `shouldBe` Right
                    (providerRound [nativeHistoryItem ProviderGeminiInteractions ItemPending "interaction-gemini" (Just "item-gemini-tool") payload] [] (ProviderRoundNeedsLocalTools [expectedToolCall]))

        it "falls back to the first step identifier as the Gemini exchange id when store=false omits it" $ do
            let thoughtPayload = geminiThoughtPayload "thought-1"
                toolPayload =
                    geminiFunctionCallPayload
                        "item-gemini-tool"
                        "lookup"
                        (object ["name" .= ("Ada" :: Text)])
                rawResponse =
                    object
                        [ "status" .= ("requires_action" :: Text)
                        , "steps" .= ([thoughtPayload, toolPayload] :: [Value])
                        ]
                expectedToolCall =
                    ToolCall
                        { toolCallId = "item-gemini-tool"
                        , toolName = "lookup"
                        , toolArgs = fromList [("name", String "Ada")]
                        , continuationAttachments =
                            [ ToolCallContinuation
                                { continuationProviderFamily = ProviderGeminiInteractions
                                , continuationPayload = thoughtPayload
                                }
                            ]
                        }

            decodeGeminiResponse rawResponse
                `shouldBe` Right
                    ( providerRound
                        [pendingGeminiToolCallWithThoughtItemAndExchangeId "Ada" "thought-1" "item-gemini-tool"]
                        []
                        (ProviderRoundNeedsLocalTools [expectedToolCall])
                    )

        it "pauses Gemini in-progress rounds" $ do
            let payload = geminiTextPayloadWithId "item-gemini" "working on it"

            decodeGeminiResponse (geminiResponse "interaction-gemini" "in_progress" [payload])
                `shouldBe` Right
                    ( providerRound
                        [nativeHistoryItem ProviderGeminiInteractions ItemPending "interaction-gemini" (Just "item-gemini") payload]
                        []
                        (ProviderRoundPaused (PauseProviderWaiting "Gemini interaction status was in_progress"))
                    )

        it "fails terminal Gemini responses" $ do
            decodeGeminiResponse (geminiResponse "interaction-gemini" "failed" [])
                `shouldBe` Right
                    (providerRound [] [] (ProviderRoundFailed (FailureProvider "Gemini interaction status was failed")))

        it "propagates Gemini failure details" $ do
            let response =
                    object
                        [ "id" .= ("interaction-gemini" :: Text)
                        , "status" .= ("failed" :: Text)
                        , "error"
                            .= object
                                [ "code" .= ("too_many_tools" :: Text)
                                , "message" .= ("Too many function declarations." :: Text)
                                ]
                        , "steps" .= ([] :: [Value])
                        ]

            decodeGeminiResponse response
                `shouldBe` Right
                    ( providerRound
                        []
                        []
                        ( ProviderRoundFailed
                            ( FailureProvider
                                "Gemini interaction status was failed: too_many_tools: Too many function declarations."
                            )
                        )
                    )

        it "fails completed Gemini thought-only rounds as contract errors" $ do
            let payload = geminiThoughtPayload "thought-1"

            decodeGeminiResponse (geminiResponse "interaction-gemini" "completed" [payload])
                `shouldBe` Right
                    ( providerRound
                        [nativeHistoryItem ProviderGeminiInteractions ItemPending "interaction-gemini" (Just "thought-1") payload]
                        []
                        ( ProviderRoundFailed
                            ( FailureContract
                                "Gemini interaction completed without tool calls or assistant message"
                            )
                        )
                    )

    describe "Gemini current streaming schema" $ do
        it "accumulates thought and model-output steps when completion omits steps" $ do
            streamedTextsRef <- IORef.newIORef ([] :: [Text])
            result <-
                runGeminiSseFixture
                    [ geminiStreamCreatedEvent
                    , object
                        [ "event_type" .= ("step.start" :: Text)
                        , "index" .= (0 :: Int)
                        , "step" .= object ["type" .= ("thought" :: Text)]
                        ]
                    , object
                        [ "event_type" .= ("step.delta" :: Text)
                        , "index" .= (0 :: Int)
                        , "delta"
                            .= object
                                [ "type" .= ("thought_signature" :: Text)
                                , "signature" .= ("thought-signature" :: Text)
                                ]
                        ]
                    , geminiStepStopEvent 0
                    , object
                        [ "event_type" .= ("step.start" :: Text)
                        , "index" .= (1 :: Int)
                        , "step"
                            .= object
                                [ "type" .= ("model_output" :: Text)
                                , "content"
                                    .= ( [ object
                                                [ "type" .= ("text" :: Text)
                                                , "text" .= ("Hello " :: Text)
                                                ]
                                           ]
                                            :: [Value]
                                       )
                                ]
                        ]
                    , object
                        [ "event_type" .= ("step.delta" :: Text)
                        , "index" .= (1 :: Int)
                        , "delta"
                            .= object
                                [ "type" .= ("text" :: Text)
                                , "text" .= ("world" :: Text)
                                ]
                        ]
                    , geminiStepStopEvent 1
                    , geminiStreamTerminalEvent "interaction.completed" "completed"
                    ]
                    defaultStreamCallbacks
                        { onAssistantTextDelta = \deltaText ->
                            liftIO (IORef.modifyIORef' streamedTextsRef (<> [deltaText]))
                        }
                    defaultChatConfig{llmCallTimeout = Just 5}

            IORef.readIORef streamedTextsRef `shouldReturn` ["Hello ", "world"]
            case result of
                Right ChatFinished{appendedItems} -> do
                    concatMap itemTexts appendedItems `shouldBe` ["Hello ", "world"]
                    geminiProviderPayloads appendedItems
                        `shouldContain` [object ["type" .= ("thought" :: Text), "signature" .= ("thought-signature" :: Text)]]
                other ->
                    expectationFailure ("Unexpected Gemini streamed text result: " <> show other)

        it "accumulates fragmented function-call arguments for requires_action" $ do
            result <-
                runGeminiSseFixture
                    [ geminiStreamCreatedEvent
                    , object
                        [ "event_type" .= ("step.start" :: Text)
                        , "index" .= (0 :: Int)
                        , "step" .= object ["type" .= ("thought" :: Text)]
                        ]
                    , object
                        [ "event_type" .= ("step.delta" :: Text)
                        , "index" .= (0 :: Int)
                        , "delta"
                            .= object
                                [ "type" .= ("thought_signature" :: Text)
                                , "signature" .= ("tool-thought" :: Text)
                                ]
                        ]
                    , geminiStepStopEvent 0
                    , object
                        [ "event_type" .= ("step.start" :: Text)
                        , "index" .= (1 :: Int)
                        , "step"
                            .= object
                                [ "type" .= ("function_call" :: Text)
                                , "id" .= ("tool-call-1" :: Text)
                                , "name" .= ("lookup" :: Text)
                                , "arguments" .= object []
                                ]
                        ]
                    , geminiArgumentsDeltaEvent 1 "{\"name\":\""
                    , geminiArgumentsDeltaEvent 1 "Ada\"}"
                    , geminiStepStopEvent 1
                    , object ["event_type" .= ("interaction.requires_action" :: Text)]
                    ]
                    defaultStreamCallbacks
                    defaultChatConfig
                        { maxToolRounds = 0
                        , llmCallTimeout = Just 5
                        }

            case result of
                Right
                    ChatPaused
                        { appendedItems =
                            [ HistoryItem
                                { genericItem =
                                    GenericToolCall
                                        { toolCall = ToolCall{toolCallId, toolName, toolArgs, continuationAttachments}
                                        }
                                }
                            ]
                        , pauseReason = PauseToolLoopLimit 0
                        } -> do
                            toolCallId `shouldBe` "tool-call-1"
                            toolName `shouldBe` "lookup"
                            toolArgs `shouldBe` fromList [("name", String "Ada")]
                            continuationAttachments
                                `shouldBe` [ToolCallContinuation ProviderGeminiInteractions (object ["type" .= ("thought" :: Text), "signature" .= ("tool-thought" :: Text)])]
                other ->
                    expectationFailure ("Unexpected Gemini streamed tool result: " <> show other)

    describe "default logging" $ do
        it "warns on OpenAI conversion notes and request failures" $ do
            output <-
                captureStderrText do
                    let settings :: OpenAIChatSettings '[IOE]
                        settings = defaultOpenAIChatSettings "test-api-key"
                        OpenAIChatSettings{requestLogger = logger} = settings
                    runEff do
                        logger (NativeConversionNote "Dropped unsupported item")
                        logger (NativeRequestFailure (ConnectionError (toException (ErrorCall "boom"))))

            output `shouldSatisfy` T.isInfixOf "[ai-rake:openai.chat] Dropped unsupported item"
            output `shouldSatisfy` T.isInfixOf "[ai-rake:openai.chat] Provider connection failed:"

        it "warns on xAI conversion notes" $ do
            output <-
                captureStderrText do
                    let settings :: XAIChatSettings '[IOE]
                        settings = defaultXAIChatSettings "test-api-key"
                        XAIChatSettings{requestLogger = logger} = settings
                    runEff $
                        logger (NativeConversionNote "Dropped unsupported item")

            output `shouldSatisfy` T.isInfixOf "[ai-rake:xai.chat] Dropped unsupported item"

        it "renders xAI Imagine request failures tersely" $ do
            output <-
                captureStderrText do
                    let settings :: XAIImagineSettings '[IOE]
                        settings = defaultXAIImagineSettings "test-api-key"
                        XAIImagineSettings{requestLogger = logger} = settings
                    runEff $
                        logger (NativeRequestFailure pendingVideoFailureResponse)

            T.strip output
                `shouldBe` "[ai-rake:xai.imagine] Provider request failed (HTTP 202 Accepted): status=pending, progress=1"

        it "keeps raw request and response bodies silent by default" $ do
            output <-
                captureStderrText do
                    let openAiSettings :: OpenAIChatSettings '[IOE]
                        openAiSettings = defaultOpenAIChatSettings "test-api-key"
                        xaiSettings :: XAIChatSettings '[IOE]
                        xaiSettings = defaultXAIChatSettings "test-api-key"
                        OpenAIChatSettings{requestLogger = openAiLogger} = openAiSettings
                        XAIChatSettings{requestLogger = xaiLogger} = xaiSettings
                    runEff do
                        openAiLogger (NativeMsgOut (object ["hello" .= ("world" :: Text)]))
                        openAiLogger (NativeMsgIn (object ["ok" .= True]))
                        xaiLogger (NativeMsgOut (object ["hello" .= ("world" :: Text)]))
                        xaiLogger (NativeMsgIn (object ["ok" .= True]))

            T.strip output `shouldBe` ""

withTools :: [ToolDef es] -> ChatConfig es -> ChatConfig es
withTools tools ChatConfig{responseFormat, sampling, onItem, maxToolRounds, llmCallTimeout} =
    ChatConfig{tools, responseFormat, sampling, onItem, maxToolRounds, llmCallTimeout}

withResponseFormat :: ResponseFormat -> ChatConfig es -> ChatConfig es
withResponseFormat responseFormat ChatConfig{tools, sampling, onItem, maxToolRounds, llmCallTimeout} =
    ChatConfig{tools, responseFormat, sampling, onItem, maxToolRounds, llmCallTimeout}

withSampling :: SamplingOptions -> ChatConfig es -> ChatConfig es
withSampling sampling ChatConfig{tools, responseFormat, onItem, maxToolRounds, llmCallTimeout} =
    ChatConfig{tools, responseFormat, sampling, onItem, maxToolRounds, llmCallTimeout}

withTemperature :: Maybe Double -> SamplingOptions -> SamplingOptions
withTemperature temperature SamplingOptions{topP} =
    SamplingOptions{temperature, topP}

withTopP :: Maybe Double -> SamplingOptions -> SamplingOptions
withTopP topP SamplingOptions{temperature} =
    SamplingOptions{temperature, topP}

excessiveToolCount :: Int
excessiveToolCount = 5000

generatedTools :: Int -> [ToolDef es]
generatedTools count =
    [ defineToolNoArgument
        ("generated_tool_" <> show toolIndex)
        ("Generated no-op tool " <> show toolIndex)
        (pure (Right "ok"))
    | toolIndex <- [1 .. count]
    ]

openAIReasoningEffortCases :: [(OpenAIReasoningEffort, Text)]
openAIReasoningEffortCases =
    [ (OpenAIReasoningNone, "none")
    , (OpenAIReasoningLow, "low")
    , (OpenAIReasoningMedium, "medium")
    , (OpenAIReasoningHigh, "high")
    , (OpenAIReasoningXHigh, "xhigh")
    , (OpenAIReasoningMax, "max")
    ]

captureOpenAIRequestBody
    :: ChatConfig '[Rake, RakeMediaStorage, Error RakeError, IOE]
    -> [HistoryItem]
    -> IO Value
captureOpenAIRequestBody chatConfig history = do
    requestBody <- captureOpenAIRequestBodyWithMediaReferences [] chatConfig history
    pure requestBody

captureOpenAIRequestBodyWithReasoningEffort
    :: OpenAIReasoningEffort
    -> ChatConfig '[Rake, RakeMediaStorage, Error RakeError, IOE]
    -> [HistoryItem]
    -> IO Value
captureOpenAIRequestBodyWithReasoningEffort reasoningEffort chatConfig history = do
    (requestBody, _) <-
        captureOpenAIRenderWithMediaReferencesAndReasoningEffort
            []
            (Just reasoningEffort)
            chatConfig
            history
    pure requestBody

captureOpenAIRequestBodyWithMediaReferences
    :: [MediaProviderReference]
    -> ChatConfig '[Rake, RakeMediaStorage, Error RakeError, IOE]
    -> [HistoryItem]
    -> IO Value
captureOpenAIRequestBodyWithMediaReferences mediaReferences chatConfig history = do
    (requestBody, _) <-
        captureOpenAIRenderWithMediaReferences mediaReferences chatConfig history
    pure requestBody

captureOpenAIRender
    :: ChatConfig '[Rake, RakeMediaStorage, Error RakeError, IOE]
    -> [HistoryItem]
    -> IO (Value, [Text])
captureOpenAIRender =
    captureOpenAIRenderWithMediaReferences []

captureOpenAIRenderWithMediaReferences
    :: [MediaProviderReference]
    -> ChatConfig '[Rake, RakeMediaStorage, Error RakeError, IOE]
    -> [HistoryItem]
    -> IO (Value, [Text])
captureOpenAIRenderWithMediaReferences mediaReferences =
    captureOpenAIRenderWithMediaReferencesAndReasoningEffort mediaReferences Nothing

captureOpenAIRenderWithMediaReferencesAndReasoningEffort
    :: [MediaProviderReference]
    -> Maybe OpenAIReasoningEffort
    -> ChatConfig '[Rake, RakeMediaStorage, Error RakeError, IOE]
    -> [HistoryItem]
    -> IO (Value, [Text])
captureOpenAIRenderWithMediaReferencesAndReasoningEffort mediaReferences configuredReasoningEffort chatConfig history = do
    requestRef <- IORef.newIORef Nothing
    notesRef <- IORef.newIORef []
    let settings :: OpenAIChatSettings '[RakeMediaStorage, Error RakeError, IOE]
        settings =
            (defaultOpenAIChatSettings "test-api-key")
                { OpenAI.baseUrl = unreachableBaseUrl
                , OpenAI.reasoningEffort = configuredReasoningEffort
                , OpenAI.requestLogger = recordRequestAndNotes requestRef notesRef
                }

    result <-
        runEff
            . runErrorNoCallStack
            . runRakeMediaStorageInMemory
            $ do
                saveMediaReferences mediaReferences
                runRakeOpenAIChat settings
                    $ void
                    $ chatOutcome chatConfig history

    result `shouldSatisfy` isLeft
    requestBody <- readRequest requestRef
    notes <- IORef.readIORef notesRef
    pure (requestBody, notes)

runOpenAIRenderResult
    :: ChatConfig '[Rake, RakeMediaStorage, Error RakeError, IOE]
    -> [HistoryItem]
    -> IO (Either RakeError ())
runOpenAIRenderResult =
    runOpenAIRenderResultWithMediaReferences []

runOpenAIRenderResultWithMediaReferences
    :: [MediaProviderReference]
    -> ChatConfig '[Rake, RakeMediaStorage, Error RakeError, IOE]
    -> [HistoryItem]
    -> IO (Either RakeError ())
runOpenAIRenderResultWithMediaReferences mediaReferences chatConfig history = do
    let settings :: OpenAIChatSettings '[RakeMediaStorage, Error RakeError, IOE]
        settings =
            (defaultOpenAIChatSettings "test-api-key")
                { OpenAI.baseUrl = unreachableBaseUrl
                , OpenAI.requestLogger = \_ -> pure ()
                }

    runEff
        . runErrorNoCallStack
        . runRakeMediaStorageInMemory
        $ do
            saveMediaReferences mediaReferences
            runRakeOpenAIChat settings
                $ void
                $ chatOutcome chatConfig history

captureXAIRequestBody
    :: ChatConfig '[Rake, RakeMediaStorage, Error RakeError, IOE]
    -> [HistoryItem]
    -> IO Value
captureXAIRequestBody chatConfig history = do
    (requestBody, _) <- captureXAIRender chatConfig history
    pure requestBody

captureXAIRender
    :: ChatConfig '[Rake, RakeMediaStorage, Error RakeError, IOE]
    -> [HistoryItem]
    -> IO (Value, [Text])
captureXAIRender chatConfig history = do
    requestRef <- IORef.newIORef Nothing
    notesRef <- IORef.newIORef []
    let XAIChatSettings
            { apiKey = defaultApiKey
            , model = defaultModel
            } = defaultXAIChatSettings "test-api-key"
        settings :: XAIChatSettings '[RakeMediaStorage, Error RakeError, IOE]
        settings =
            XAIChatSettings
                { apiKey = defaultApiKey
                , model = defaultModel
                , baseUrl = unreachableBaseUrl
                , requestLogger = recordRequestAndNotes requestRef notesRef
                }

    result <-
        runEff
            . runErrorNoCallStack
            . runRakeMediaStorageInMemory
            $ runRakeXAIChat settings
            $ void
            $ chatOutcome chatConfig history

    result `shouldSatisfy` isLeft
    requestBody <- readRequest requestRef
    notes <- IORef.readIORef notesRef
    pure (requestBody, notes)

captureGeminiRequestBody
    :: ChatConfig '[Rake, RakeMediaStorage, Error RakeError, IOE]
    -> [HistoryItem]
    -> IO Value
captureGeminiRequestBody chatConfig history = do
    (requestBody, _) <- captureGeminiRender chatConfig history
    pure requestBody

captureGeminiRender
    :: ChatConfig '[Rake, RakeMediaStorage, Error RakeError, IOE]
    -> [HistoryItem]
    -> IO (Value, [Text])
captureGeminiRender chatConfig history = do
    requestRef <- IORef.newIORef Nothing
    notesRef <- IORef.newIORef []
    let GeminiChatSettings
            { apiKey = defaultApiKey
            , model = defaultModel
            , providerTools = defaultProviderTools
            , generationConfig = defaultGenerationConfig
            } = defaultGeminiChatSettings "test-api-key"
        settings :: GeminiChatSettings '[RakeMediaStorage, Error RakeError, IOE]
        settings =
            GeminiChatSettings
                { apiKey = defaultApiKey
                , model = defaultModel
                , baseUrl = unreachableBaseUrl
                , systemInstruction = Nothing
                , providerTools = defaultProviderTools
                , generationConfig = defaultGenerationConfig
                , requestLogger = recordRequestAndNotes requestRef notesRef
                }

    result <-
        runEff
            . runErrorNoCallStack
            . runRakeMediaStorageInMemory
            $ runRakeGeminiChat settings
            $ void
            $ chatOutcome chatConfig history

    result `shouldSatisfy` isLeft
    requestBody <- readRequest requestRef
    notes <- IORef.readIORef notesRef
    pure (requestBody, notes)

runGeminiRenderResult
    :: ChatConfig '[Rake, RakeMediaStorage, Error RakeError, IOE]
    -> [HistoryItem]
    -> IO (Either RakeError ())
runGeminiRenderResult chatConfig history = do
    let GeminiChatSettings
            { apiKey = defaultApiKey
            , model = defaultModel
            , providerTools = defaultProviderTools
            , generationConfig = defaultGenerationConfig
            } = defaultGeminiChatSettings "test-api-key"
        settings :: GeminiChatSettings '[RakeMediaStorage, Error RakeError, IOE]
        settings =
            GeminiChatSettings
                { apiKey = defaultApiKey
                , model = defaultModel
                , baseUrl = unreachableBaseUrl
                , systemInstruction = Nothing
                , providerTools = defaultProviderTools
                , generationConfig = defaultGenerationConfig
                , requestLogger = \_ -> pure ()
                }

    runEff
        . runErrorNoCallStack
        . runRakeMediaStorageInMemory
        $ runRakeGeminiChat settings
        $ void
        $ chatOutcome chatConfig history

runGeminiSseFixture
    :: [Value]
    -> StreamCallbacks '[Rake, RakeMediaStorage, Error RakeError, IOE]
    -> ChatConfig '[Rake, RakeMediaStorage, Error RakeError, IOE]
    -> IO (Either RakeError ChatOutcome)
runGeminiSseFixture streamEvents streamCallbacks chatConfig =
    withGeminiSseServer streamEvents $ \fixtureBaseUrl -> do
        let GeminiChatSettings
                { apiKey = defaultApiKey
                , model = defaultModel
                , systemInstruction = defaultSystemInstruction
                , providerTools = defaultProviderTools
                , generationConfig = defaultGenerationConfig
                } = defaultGeminiChatSettings "test-api-key"
            settings :: GeminiChatSettings '[RakeMediaStorage, Error RakeError, IOE]
            settings =
                GeminiChatSettings
                    { apiKey = defaultApiKey
                    , model = defaultModel
                    , baseUrl = fixtureBaseUrl
                    , systemInstruction = defaultSystemInstruction
                    , providerTools = defaultProviderTools
                    , generationConfig = defaultGenerationConfig
                    , requestLogger = \_ -> pure ()
                    }
        runEff
            . runErrorNoCallStack
            . runRakeMediaStorageInMemory
            $ runRakeGeminiChat settings
            $ streamChatOutcome streamCallbacks chatConfig [user "hello"]

withGeminiSseServer :: [Value] -> (Text -> IO a) -> IO a
withGeminiSseServer streamEvents action =
    Socket.withSocketsDo
        $ bracket openListener Socket.close
        $ \listener -> do
            listenerAddress <- Socket.getSocketName listener
            fixturePort <- case listenerAddress of
                Socket.SockAddrInet port _ ->
                    pure port
                otherAddress ->
                    error ("Unexpected Gemini SSE fixture address: " <> show otherAddress)
            serverThread <- Concurrent.forkIO (serveGeminiSseFixture listener streamEvents)
            action ("http://127.0.0.1:" <> show fixturePort)
                `finally` Concurrent.killThread serverThread
  where
    openListener = do
        listener <- Socket.socket Socket.AF_INET Socket.Stream Socket.defaultProtocol
        Socket.setSocketOption listener Socket.ReuseAddr 1
        Socket.bind listener (Socket.SockAddrInet 0 (Socket.tupleToHostAddress (127, 0, 0, 1)))
        Socket.listen listener 1
        pure listener

serveGeminiSseFixture :: Socket.Socket -> [Value] -> IO ()
serveGeminiSseFixture listener streamEvents =
    bracket (fst <$> Socket.accept listener) Socket.close $ \clientSocket -> do
        receiveHttpRequest clientSocket
        SocketBS.sendAll clientSocket (geminiSseHttpResponse streamEvents)

receiveHttpRequest :: Socket.Socket -> IO ()
receiveHttpRequest clientSocket = do
    (headerBytes, initialBodyBytes) <- receiveHeaders BS.empty
    let contentLength = httpContentLength headerBytes
        remainingBodyBytes = max 0 (contentLength - BS.length initialBodyBytes)
    drainBody remainingBodyBytes
  where
    receiveHeaders bufferedBytes =
        case BS.breakSubstring "\r\n\r\n" bufferedBytes of
            (headerBytes, separatorAndBody)
                | not (BS.null separatorAndBody) ->
                    pure (headerBytes, BS.drop 4 separatorAndBody)
            _ -> do
                nextBytes <- SocketBS.recv clientSocket 4096
                if BS.null nextBytes
                    then pure (bufferedBytes, BS.empty)
                    else receiveHeaders (bufferedBytes <> nextBytes)

    drainBody remainingBytes
        | remainingBytes <= 0 =
            pure ()
        | otherwise = do
            nextBytes <- SocketBS.recv clientSocket (min 4096 remainingBytes)
            unless (BS.null nextBytes) (drainBody (remainingBytes - BS.length nextBytes))

httpContentLength :: BS.ByteString -> Int
httpContentLength headerBytes =
    fromMaybe 0
        $ viaNonEmpty
            head
            [ contentLength
            | headerLine <- BC8.lines headerBytes
            , Just rawContentLength <- [BC8.stripPrefix "Content-Length:" headerLine]
            , Just contentLength <- [readMaybe (BC8.unpack (BC8.strip rawContentLength))]
            ]

geminiSseHttpResponse :: [Value] -> BS.ByteString
geminiSseHttpResponse streamEvents =
    "HTTP/1.1 200 OK\r\nContent-Type: text/event-stream\r\nConnection: close\r\n\r\n"
        <> foldMap geminiSseEvent streamEvents

geminiSseEvent :: Value -> BS.ByteString
geminiSseEvent eventValue =
    "data: " <> LBS.toStrict (encode eventValue) <> "\n\n"

recordRequest :: IOE :> es => IORef.IORef (Maybe Value) -> NativeMsgFormat -> Eff es ()
recordRequest requestRef = \case
    NativeMsgOut requestBody ->
        liftIO $ IORef.writeIORef requestRef (Just requestBody)
    _ ->
        pure ()

recordRequestAndNotes
    :: IOE :> es
    => IORef.IORef (Maybe Value)
    -> IORef.IORef [Text]
    -> NativeMsgFormat
    -> Eff es ()
recordRequestAndNotes requestRef notesRef = \case
    NativeMsgOut requestBody ->
        liftIO $ IORef.writeIORef requestRef (Just requestBody)
    NativeConversionNote note ->
        liftIO $ IORef.modifyIORef' notesRef (<> [note])
    _ ->
        pure ()

readRequest :: IORef.IORef (Maybe Value) -> IO Value
readRequest requestRef = do
    maybeRequest <- IORef.readIORef requestRef
    case maybeRequest of
        Just requestBody ->
            pure requestBody
        Nothing -> do
            expectationFailure "Expected request body to be captured before the HTTP failure"
            fail "request body not captured"

captureStderrText :: IO a -> IO Text
captureStderrText action = do
    originalStderr <- hDuplicate IO.stderr
    (tempPath, tempHandle) <- IO.openTempFile "/tmp" "ai-rake-stderr"

    hDuplicateTo tempHandle IO.stderr
    _ <-
        action `finally` do
            IO.hFlush IO.stderr
            hDuplicateTo originalStderr IO.stderr
            IO.hClose originalStderr
            IO.hClose tempHandle

    output <- TIO.readFile tempPath
    removeFile tempPath
    pure output

unreachableBaseUrl :: Text
unreachableBaseUrl = "http://127.0.0.1:1"

nativeResponsesAssistantPayload :: Value
nativeResponsesAssistantPayload =
    object
        [ "id" .= ("native-message-1" :: Text)
        , "type" .= ("message" :: Text)
        , "role" .= ("assistant" :: Text)
        , "content"
            .= ( [ object
                        [ "type" .= ("output_text" :: Text)
                        , "text" .= ("native assistant text" :: Text)
                        ]
                   ]
                    :: [Value]
               )
        ]

legacyResponsesAssistantPayload :: Value
legacyResponsesAssistantPayload =
    object
        [ "id" .= ("native-message-legacy" :: Text)
        , "role" .= ("assistant" :: Text)
        , "content" .= ("native assistant text" :: Text)
        ]

nativeResponsesReasoningPayload :: Value
nativeResponsesReasoningPayload =
    object
        [ "id" .= ("native-reasoning-1" :: Text)
        , "type" .= ("reasoning" :: Text)
        , "encrypted_content" .= ("opaque-reasoning-state" :: Text)
        ]

projectedAssistantMessage :: Value
projectedAssistantMessage =
    object
        [ "role" .= ("assistant" :: Text)
        , "content" .= ("native assistant text" :: Text)
        ]

nativeGeminiTextPayload :: Value
nativeGeminiTextPayload =
    object
        [ "id" .= ("item-gemini" :: Text)
        , "type" .= ("model_output" :: Text)
        , "content"
            .= ( [ object
                    [ "type" .= ("text" :: Text)
                    , "text" .= ("native assistant text" :: Text)
                    ]
                 ]
                    :: [Value]
               )
        ]

legacyGeminiTextPayload :: Value
legacyGeminiTextPayload =
    object
        [ "text" .= ("native assistant text" :: Text)
        ]

openAiNativeItem :: Value -> HistoryItem
openAiNativeItem =
    nativeHistoryItem ProviderOpenAIResponses ItemCompleted "response-openai" (Just "item-openai")

pendingOpenAiNativeItem :: Value -> HistoryItem
pendingOpenAiNativeItem =
    nativeHistoryItem ProviderOpenAIResponses ItemPending "response-openai" (Just "item-openai")

xaiNativeItem :: Value -> HistoryItem
xaiNativeItem =
    nativeHistoryItem ProviderXAIResponses ItemCompleted "response-xai" (Just "item-xai")

geminiNativeItem :: Value -> HistoryItem
geminiNativeItem =
    nativeHistoryItem ProviderGeminiInteractions ItemCompleted "interaction-gemini" (Just "item-gemini")

pendingGeminiNativeItem :: Value -> HistoryItem
pendingGeminiNativeItem =
    nativeHistoryItem ProviderGeminiInteractions ItemPending "interaction-gemini" (Just "item-gemini")

pendingGeminiToolCallWithThoughtItem :: Text -> HistoryItem
pendingGeminiToolCallWithThoughtItem contactName =
    pendingGeminiToolCallWithThoughtItemAndExchangeId contactName "interaction-gemini" "tool-call-1"

pendingGeminiToolCallWithThoughtItemAndExchangeId :: Text -> Text -> Text -> HistoryItem
pendingGeminiToolCallWithThoughtItemAndExchangeId contactName exchangeId toolCallId =
    HistoryItem
        { historyItemIdField = Nothing
        , itemLifecycle = ItemPending
        , genericItem =
            GenericToolCall
                { toolCall =
                    ToolCall
                        { toolCallId = ToolCallId toolCallId
                        , toolName = "lookup"
                        , toolArgs = fromList [("name", String contactName)]
                        , continuationAttachments =
                            [ ToolCallContinuation
                                { continuationProviderFamily = ProviderGeminiInteractions
                                , continuationPayload = geminiThoughtPayload "thought-1"
                                }
                            ]
                        }
                }
        , providerItem =
            Just
                ProviderItem
                    { apiFamily = ProviderGeminiInteractions
                    , exchangeId = Just exchangeId
                    , nativeItemId = Just toolCallId
                    , payload = geminiFunctionCallPayload toolCallId "lookup" (object ["name" .= contactName])
                    , availableLocalTools = []
                    }
        }

nativeHistoryItem :: ProviderApiFamily -> ItemLifecycle -> Text -> Maybe Text -> Value -> HistoryItem
nativeHistoryItem apiFamily lifecycle exchangeId nativeItemId payload =
    HistoryItem
        { historyItemIdField = Nothing
        , itemLifecycle = lifecycle
        , genericItem = classifiedItem
        , providerItem =
            Just
                ProviderItem
                    { apiFamily
                    , exchangeId = Just exchangeId
                    , nativeItemId
                    , payload
                    , availableLocalTools = []
                    }
        }
  where
    classifiedItem =
        classifyNativePayload apiFamily payload

classifyNativePayload :: ProviderApiFamily -> Value -> GenericItem
classifyNativePayload apiFamily payload =
    case apiFamily of
        ProviderOpenAIResponses ->
            classifiedRoundItem $
                decodeOpenAIResponse (responsesResponse "response-openai" "completed" [payload])
        ProviderXAIResponses ->
            classifiedRoundItem $
                decodeXAIResponse (responsesResponse "response-xai" "completed" [payload])
        ProviderGeminiInteractions ->
            classifiedRoundItem $
                decodeGeminiResponse (geminiResponse "interaction-gemini" "completed" [payload])
        ProviderApiFamily{} ->
            GenericNonPortable
  where
    classifiedRoundItem = \case
        Right ProviderRound{roundItems = HistoryItem{genericItem = canonicalItem} : _} ->
            canonicalItem
        _ ->
            GenericNonPortable

providerRound :: [HistoryItem] -> [MediaProviderReference] -> ProviderRoundAction -> ProviderRound
providerRound roundItems mediaReferences action =
    ProviderRound{roundItems, mediaReferences, action}

mediaReference :: MediaBlobId -> ProviderApiFamily -> Value -> MediaProviderReference
mediaReference mediaBlobId providerFamily providerRequestPart =
    MediaProviderReference{mediaBlobId, providerFamily, providerRequestPart}

responsesResponse :: Text -> Text -> [Value] -> Value
responsesResponse responseId status output =
    object
        [ "id" .= responseId
        , "status" .= status
        , "output" .= output
        ]

responsesAssistantPayload :: Text -> Text -> Value
responsesAssistantPayload itemId textValue =
    responsesAssistantPayloadWithContentAndStatus
        itemId
        [ object
            [ "type" .= ("output_text" :: Text)
            , "text" .= textValue
            ]
        ]
        "completed"

responsesAssistantPayloadWithStatus :: Text -> Text -> Text -> Value
responsesAssistantPayloadWithStatus itemId textValue status =
    responsesAssistantPayloadWithContentAndStatus
        itemId
        [ object
            [ "type" .= ("output_text" :: Text)
            , "text" .= textValue
            ]
        ]
        status

responsesAssistantPayloadWithContent :: Text -> [Value] -> Value
responsesAssistantPayloadWithContent itemId content =
    responsesAssistantPayloadWithContentAndStatus itemId content "completed"

responsesAssistantPayloadWithContentAndStatus :: Text -> [Value] -> Text -> Value
responsesAssistantPayloadWithContentAndStatus itemId content status =
    object
        [ "id" .= itemId
        , "type" .= ("message" :: Text)
        , "role" .= ("assistant" :: Text)
        , "status" .= status
        , "content" .= content
        ]

responsesToolCallPayload :: Text -> Text -> Text -> Value -> Text -> Value
responsesToolCallPayload itemId callId name arguments status =
    object
        [ "id" .= itemId
        , "type" .= ("function_call" :: Text)
        , "call_id" .= callId
        , "name" .= name
        , "arguments" .= arguments
        , "status" .= status
        ]

geminiResponse :: Text -> Text -> [Value] -> Value
geminiResponse interactionId status steps =
    object
        [ "id" .= interactionId
        , "status" .= status
        , "steps" .= steps
        ]

geminiStreamCreatedEvent :: Value
geminiStreamCreatedEvent =
    object
        [ "event_type" .= ("interaction.created" :: Text)
        , "interaction"
            .= object
                [ "id" .= ("interaction-stream" :: Text)
                , "status" .= ("in_progress" :: Text)
                ]
        ]

geminiStepStopEvent :: Int -> Value
geminiStepStopEvent stepIndex =
    object
        [ "event_type" .= ("step.stop" :: Text)
        , "index" .= stepIndex
        ]

geminiArgumentsDeltaEvent :: Int -> Text -> Value
geminiArgumentsDeltaEvent stepIndex argumentsFragment =
    object
        [ "event_type" .= ("step.delta" :: Text)
        , "index" .= stepIndex
        , "delta"
            .= object
                [ "type" .= ("arguments_delta" :: Text)
                , "arguments" .= argumentsFragment
                ]
        ]

geminiStreamTerminalEvent :: Text -> Text -> Value
geminiStreamTerminalEvent eventType status =
    object
        [ "type" .= eventType
        , "interaction"
            .= object
                [ "id" .= ("interaction-stream" :: Text)
                , "status" .= status
                ]
        ]

geminiTextPayloadWithId :: Text -> Text -> Value
geminiTextPayloadWithId itemId textValue =
    object
        [ "id" .= itemId
        , "type" .= ("model_output" :: Text)
        , "content"
            .= ( [ object
                    [ "type" .= ("text" :: Text)
                    , "text" .= textValue
                    ]
                 ]
                    :: [Value]
               )
        ]

geminiFunctionCallPayload :: Text -> Text -> Value -> Value
geminiFunctionCallPayload itemId name arguments =
    object
        [ "id" .= itemId
        , "type" .= ("function_call" :: Text)
        , "name" .= name
        , "arguments" .= arguments
        ]

geminiThoughtPayload :: Text -> Value
geminiThoughtPayload itemId =
    object
        [ "signature" .= itemId
        , "type" .= ("thought" :: Text)
        ]

geminiProviderPayloads :: [HistoryItem] -> [Value]
geminiProviderPayloads historyItems =
    [ payload
    | HistoryItem
        { providerItem =
            Just
                ProviderItem
                    { apiFamily = ProviderGeminiInteractions
                    , payload
                    }
        } <-
        historyItems
    ]

firstToolParameters :: Value -> Maybe Value
firstToolParameters requestBody = do
    Array tools <- lookupPath ["tools"] requestBody
    toolValue <- viaNonEmpty head (toList tools)
    lookupPath ["parameters"] toolValue

toolCount :: Value -> Maybe Int
toolCount requestBody = do
    Array tools <- lookupPath ["tools"] requestBody
    pure (length tools)

lookupPath :: [Text] -> Value -> Maybe Value
lookupPath [] value = Just value
lookupPath (fieldName : rest) value = case value of
    Object objectValue ->
        KM.lookup (Key.fromText fieldName) objectValue >>= lookupPath rest
    _ ->
        Nothing

countSystemRoleMessages :: Value -> Maybe Int
countSystemRoleMessages requestBody = do
    Array inputItems <- lookupPath ["input"] requestBody
    pure $
        length
            [ ()
            | Object itemObject <- toList inputItems
            , KM.lookup "role" itemObject == Just (String "system")
            ]

sharedHistory :: [HistoryItem]
sharedHistory =
    [ systemParts [textPart "sys", textPart "tem"]
    , userText "hello"
    , assistantParts [textPart "partial ", textPart "answer"]
    , toolCall "tool-call-1" "lookup" (fromList [("name", String "John Snow")])
    , toolResultJson "tool-call-1" (String "ok")
    ]

sharedHistoryRequest :: [Value]
sharedHistoryRequest =
    [ object
        [ "role" .= ("system" :: Text)
        , "content"
            .= ( [ object
                        [ "type" .= ("input_text" :: Text)
                        , "text" .= ("sys" :: Text)
                        ]
                   , object
                        [ "type" .= ("input_text" :: Text)
                        , "text" .= ("tem" :: Text)
                        ]
                   ]
                    :: [Value]
               )
        ]
    , object
        [ "role" .= ("user" :: Text)
        , "content" .= ("hello" :: Text)
        ]
    , object
        [ "role" .= ("assistant" :: Text)
        , "content"
            .= ( [ object
                        [ "type" .= ("output_text" :: Text)
                        , "text" .= ("partial " :: Text)
                        ]
                   , object
                        [ "type" .= ("output_text" :: Text)
                        , "text" .= ("answer" :: Text)
                        ]
                   ]
                    :: [Value]
               )
        ]
    , object
        [ "type" .= ("function_call" :: Text)
        , "call_id" .= ("tool-call-1" :: Text)
        , "name" .= ("lookup" :: Text)
        , "arguments" .= ("{\"name\":\"John Snow\"}" :: Text)
        ]
    , object
        [ "type" .= ("function_call_output" :: Text)
        , "call_id" .= ("tool-call-1" :: Text)
        , "output" .= ("\"ok\"" :: Text)
        ]
    ]

sharedGeminiSystemInstruction :: Text
sharedGeminiSystemInstruction =
    "system"

sharedGeminiHistoryRequest :: [Value]
sharedGeminiHistoryRequest =
    [ object
        [ "type" .= ("user_input" :: Text)
        , "content"
            .= ( [ object
                    [ "type" .= ("text" :: Text)
                    , "text" .= ("hello" :: Text)
                    ]
                 ]
                    :: [Value]
               )
        ]
    , object
        [ "type" .= ("model_output" :: Text)
        , "content"
            .= ( [ object
                    [ "type" .= ("text" :: Text)
                    , "text" .= ("partial " :: Text)
                    ]
                 , object
                    [ "type" .= ("text" :: Text)
                    , "text" .= ("answer" :: Text)
                    ]
                 ]
                    :: [Value]
               )
        ]
    , object
        [ "type" .= ("function_call" :: Text)
        , "id" .= ("tool-call-1" :: Text)
        , "name" .= ("lookup" :: Text)
        , "arguments" .= object ["name" .= ("John Snow" :: Text)]
        ]
    , object
        [ "type" .= ("function_result" :: Text)
        , "name" .= ("lookup" :: Text)
        , "call_id" .= ("tool-call-1" :: Text)
        , "result"
            .= ( [ object
                    [ "type" .= ("text" :: Text)
                    , "text" .= ("\"ok\"" :: Text)
                    ]
                 ]
                    :: [Value]
               )
        ]
    ]

nativeToolResultPayload :: Text -> Value
nativeToolResultPayload outputText =
    object
        [ "type" .= ("function_call_output" :: Text)
        , "call_id" .= ("tool-call-1" :: Text)
        , "output" .= outputText
        ]

nativeGeminiToolResultPayload :: Text -> Value
nativeGeminiToolResultPayload resultText =
    object
        [ "type" .= ("function_result" :: Text)
        , "call_id" .= ("tool-call-1" :: Text)
        , "result"
            .= ( [ object
                        [ "type" .= ("text" :: Text)
                        , "text" .= resultText
                        ]
                   ]
                    :: [Value]
               )
        ]

scalarAndCompositeJsonCases :: [Text]
scalarAndCompositeJsonCases =
    [ "123"
    , "true"
    , "null"
    , "\"ok\""
    , "[1,2]"
    , "{\"answer\":4}"
    ]

pendingVideoFailureResponse :: ClientError
pendingVideoFailureResponse =
    FailureResponse pendingVideoRequest pendingVideoResponse

pendingVideoRequest :: RequestF () (BaseUrl, ByteString)
pendingVideoRequest =
    Request
        { requestPath = (BaseUrl Https "example.test" 443 "", "/v1/videos")
        , requestQueryString = mempty
        , requestBody = Nothing
        , requestAccept = mempty
        , requestHeaders = mempty
        , requestHttpVersion = http11
        , requestMethod = "POST"
        }

pendingVideoResponse :: ResponseF LByteString
pendingVideoResponse =
    Response
        { responseStatusCode = accepted202
        , responseHeaders = mempty
        , responseHttpVersion = http11
        , responseBody = encode (object ["status" .= ("pending" :: Text), "progress" .= (1 :: Int)])
        }
