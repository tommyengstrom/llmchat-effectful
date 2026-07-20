{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Rake.ReplaySpec where

import Control.Exception (finally)
import Data.Aeson
import Data.IORef qualified as IORef
import Data.UUID qualified as UUID
import Data.UUID.V4 (nextRandom)
import Effectful
import Effectful.Concurrent (Concurrent, runConcurrent)
import Effectful.Error.Static
import Effectful.Time (Time, runTime)
import Rake
import Rake.ChatSpec qualified as Chat
import Rake.MediaStorage.Directory (runRakeMediaStorageDirectory)
import Rake.MediaStorage.InMemory (runRakeMediaStorageInMemory)
import Rake.Provider.ResponsesRenderSpec qualified as Render
import Rake.Providers.Gemini.Chat
    ( GeminiChatSettings (..)
    , defaultGeminiChatSettings
    , runRakeGeminiChat
    )
import Rake.Providers.OpenAI.Chat
    ( OpenAIChatSettings (..)
    , decodeOpenAIResponse
    , defaultOpenAIChatSettings
    , runRakeOpenAIChat
    )
import Rake.Providers.XAI.Chat
    ( XAIChatSettings (..)
    , defaultXAIChatSettings
    , runRakeXAIChat
    )
import Rake.Storage.InMemory (runRakeStorageInMemory)
import Relude
import System.Directory qualified as Directory
import System.FilePath ((</>))
import Test.Hspec

spec :: Spec
spec = describe "Rake.Replay" $ do
    describe "conversationReplayState" $ do
        it "tracks the active logical branch across resets" $ do
            let start = withHistoryId 1 (user "start")
                stale = withHistoryId 2 (assistantText "stale")
                again = withHistoryId 3 (user "again")
                history =
                    [ start
                    , stale
                    , resetTo (itemIdOf start)
                    , again
                    ]
                ReplayState{activeHistory, replayHistory, resumableToolCalls, pendingArtifacts, blocked} =
                    conversationReplayState history

            activeHistory `shouldBe` [start, again]
            replayHistory `shouldBe` [start, again]
            resumableToolCalls `shouldBe` []
            pendingArtifacts `shouldBe` []
            blocked `shouldBe` Nothing

        it "keeps resolved tool exchanges replayable while suppressing pending assistant artifacts" $ do
            let pendingAssistant = Chat.responsesAssistantItem ItemPending "working on it"
                pendingCall = Chat.loopToolCall "loop-1"
                pendingCallItem = Chat.responsesToolCallItem ItemPending pendingCall
                replayState =
                    conversationReplayState
                        [ user "start"
                        , pendingAssistant
                        , pendingCallItem
                        , toolResult "loop-1" "looped"
                        ]
                ReplayState{replayHistory, resumableToolCalls, pendingArtifacts, blocked} = replayState

            replayHistory
                `shouldBe`
                    [ user "start"
                    , pendingCallItem
                    , toolResult "loop-1" "looped"
                    ]
            resumableToolCalls `shouldBe` []
            pendingArtifacts `shouldBe` [pendingAssistant]
            blocked `shouldBe` Nothing

        it "does not let an earlier stray tool result resolve a later pending tool call with the same id" $ do
            let pendingCall = Chat.responsesToolCallItem ItemPending (Chat.loopToolCall "loop-1")
                replayState =
                    conversationReplayState
                        [ user "start"
                        , toolResultText "loop-1" "stale result"
                        , pendingCall
                        ]
                ReplayState{replayHistory, resumableToolCalls, pendingArtifacts, blocked} = replayState

            replayHistory `shouldBe` [user "start"]
            resumableToolCalls `shouldBe` [Chat.loopToolCall "loop-1"]
            pendingArtifacts `shouldBe` [pendingCall]
            blocked `shouldBe` Nothing

        it "blocks replay when the active branch contains duplicate unresolved tool call ids" $ do
            let firstPendingCall = Chat.responsesToolCallItem ItemPending (Chat.loopToolCall "loop-1")
                secondPendingCall = Chat.responsesToolCallItem ItemPending (Chat.loopToolCall "loop-1")
                replayState =
                    conversationReplayState
                        [ user "start"
                        , firstPendingCall
                        , secondPendingCall
                        ]
                ReplayState{blocked} = replayState

            blocked
                `shouldBe` Just
                    (ReplayBlocked "Active conversation contains multiple unresolved tool calls with id loop-1. Append resetTo to rewind before continuing.")

        it "keeps stray tool results in the log but excludes them from replay and pending artifacts" $ do
            let strayToolResult = toolResultText "loop-1" "stale result"
                replayState =
                    conversationReplayState
                        [ user "start"
                        , strayToolResult
                        ]
                ReplayState{activeHistory, replayHistory, resumableToolCalls, pendingArtifacts, blocked} = replayState

            activeHistory `shouldBe` [user "start", strayToolResult]
            replayHistory `shouldBe` [user "start"]
            resumableToolCalls `shouldBe` []
            pendingArtifacts `shouldBe` []
            blocked `shouldBe` Nothing

        it "replays Gemini continuation attachments for resolved same-provider tool continuations" $ do
            let pendingCallItem = pendingGeminiToolCallWithThoughtItem "John Snow"
                replayState =
                    conversationReplayState
                        [ pendingCallItem
                        , toolResultText "tool-call-1" "Contacts:\n- John Snow"
                        ]
                ReplayState{replayHistory, resumableToolCalls, pendingArtifacts, blocked} = replayState

            replayHistory
                `shouldBe`
                    [ pendingCallItem
                    , toolResultText "tool-call-1" "Contacts:\n- John Snow"
                    ]
            resumableToolCalls `shouldBe` []
            pendingArtifacts `shouldBe` []
            blocked `shouldBe` Nothing

        it "surfaces invalid reset checkpoints instead of silently accepting them" $ do
            let start = withHistoryId 1 (user "start")
                missingItemId = historyItemIdAt 99
                history =
                    [ start
                    , resetTo missingItemId
                    ]
                ReplayState{blocked} =
                    conversationReplayState history

            blocked `shouldBe` Just (ReplayInvalidReset (ResetToItem missingItemId))
            latestValidCheckpoint history `shouldBe` Just (ResetToItem (itemIdOf start))
            resetToLatestValidCheckpoint history `shouldBe` Just (resetTo (itemIdOf start))

    describe "system snapshots" $ do
        it "uses the latest replayed system snapshot and drops all system messages from chronology" $ do
            let history =
                    [ system "old system"
                    , user "hello"
                    , system "new system"
                    , assistantText "done"
                    ]
                (maybeSystemSnapshot, chronology) = renderableReplayHistory history

            effectiveSystemSnapshot history `shouldBe` Just (system "new system")
            maybeSystemSnapshot `shouldBe` Just (system "new system")
            chronology `shouldBe` [user "hello", assistantText "done"]

        it "takes the latest system snapshot from the active replay branch after resets" $ do
            let staleSystem = withHistoryId 1 (system "stale system")
                freshSystem = withHistoryId 2 (system "fresh system")
                history =
                    [ staleSystem
                    , user "before reset"
                    , resetToStart
                    , freshSystem
                    , user "after reset"
                    ]

            effectiveSystemSnapshot history `shouldBe` Just freshSystem
            snd (renderableReplayHistory history) `shouldBe` [user "after reset"]

    describe "reset checkpoints" $ do
        it "reports the stable active prefixes and the latest reset point" $ do
            let start = withHistoryId 1 (user "start")
                done = withHistoryId 2 (assistantText "done")
                pendingAssistant = withHistoryId 3 (Chat.responsesAssistantItem ItemPending "working on it")
                barrierMessage = "Responses response status was failed"
                history =
                    [ start
                    , done
                    , pendingAssistant
                    , Chat.replayBarrierItem barrierMessage
                    ]

            validResetCheckpoints history
                `shouldBe` [ResetToStart, ResetToItem (itemIdOf start), ResetToItem (itemIdOf done)]
            latestValidCheckpoint history `shouldBe` Just (ResetToItem (itemIdOf done))
            resetToLatestValidCheckpoint history `shouldBe` Just (resetTo (itemIdOf done))

        it "treats pending non-portable artifacts as unfinished when computing reset checkpoints" $ do
            let start = withHistoryId 1 (user "start")
                done = withHistoryId 2 (assistantText "done")
                pendingThought = withHistoryId 3 pendingGeminiUnusedThoughtItem
                history = [start, done, pendingThought]
                ReplayState{replayHistory, pendingArtifacts, blocked} = conversationReplayState history

            replayHistory `shouldBe` [start, done]
            pendingArtifacts `shouldBe` [pendingThought]
            blocked `shouldBe` Nothing
            validResetCheckpoints history
                `shouldBe` [ResetToStart, ResetToItem (itemIdOf start), ResetToItem (itemIdOf done)]
            latestValidCheckpoint history `shouldBe` Just (ResetToItem (itemIdOf done))
            resetToLatestValidCheckpoint history `shouldBe` Just (resetTo (itemIdOf done))

        it "returns Nothing when the current active branch is already a valid checkpoint" $ do
            let start = withHistoryId 1 (user "start")
                done = withHistoryId 2 (assistantText "done")
            resetToLatestValidCheckpoint [start, done]
                `shouldBe` Nothing

        it "does not suggest ResetToStart for stable id-less histories" $ do
            resetToLatestValidCheckpoint [user "start", assistantText "done"]
                `shouldBe` Nothing

    describe "shared restart semantics" $ do
        it "drops unfinished assistant output but keeps resumable tool work when restarting a paused round" $ do
            historyRef <- IORef.newIORef []
            let pendingAssistant = Chat.responsesAssistantItem ItemPending "working on it"
                pendingCall = Chat.loopToolCall "loop-1"
                pendingCallItem = Chat.responsesToolCallItem ItemPending pendingCall
                finalAssistant = Chat.responsesAssistantItem ItemCompleted "done"

            result <-
                runStorageReplayTest historyRef
                    [ Chat.pausedProviderRound
                        (PauseProviderWaiting "Responses response status was in_progress")
                        [pendingAssistant, pendingCallItem]
                    , Chat.finalProviderRound [finalAssistant]
                    ]
                    do
                        convId <- createConversation
                        appendItems convId [user "start"]
                        _ <-
                            withStorageBy
                                chatOutcomeAppendedItems
                                ( chatOutcome
                                    defaultChatConfig
                                        { tools = [Chat.loopTool]
                                        }
                                )
                                convId
                        _ <-
                            withStorageBy
                                chatOutcomeAppendedItems
                                ( chatOutcome
                                    defaultChatConfig
                                        { tools = [Chat.loopTool]
                                        }
                                )
                                convId
                        pure ()

            result `shouldBe` Right ()
            recordedHistories <- IORef.readIORef historyRef
            recordedHistories
                `shouldBe`
                    [ [user "start"]
                    , [ user "start"
                      , Chat.withAvailableLocalTools [Chat.loopTool] pendingCallItem
                      , toolResult "loop-1" "looped"
                      ]
                    ]

        it "requires an explicit reset before retrying a failed round" $ do
            historyRef <- IORef.newIORef []
            let pendingCall = Chat.loopToolCall "loop-1"
                pendingCallItem = Chat.responsesToolCallItem ItemPending pendingCall
                finalAssistant = Chat.responsesAssistantItem ItemCompleted "done"

            result <-
                runStorageReplayTest historyRef
                    [ Chat.failedProviderRound
                        (FailureProvider "Responses response status was failed")
                        [pendingCallItem]
                    , Chat.finalProviderRound [finalAssistant]
                    ]
                    do
                        convId <- createConversation
                        appendItems convId [user "start"]
                        _ <-
                            withStorageBy
                                chatOutcomeAppendedItems
                                ( chatOutcome
                                    defaultChatConfig
                                        { tools = [Chat.loopTool]
                                        }
                                )
                                convId
                        storedHistory <- getConversation convId
                        appendItems convId (maybe [] pure (resetToLatestValidCheckpoint storedHistory))
                        _ <-
                            withStorageBy
                                chatOutcomeAppendedItems
                                ( chatOutcome
                                    defaultChatConfig
                                        { tools = [Chat.loopTool]
                                        }
                                )
                                convId
                        pure ()

            result `shouldBe` Right ()
            recordedHistories <- IORef.readIORef historyRef
            recordedHistories
                `shouldBe`
                    [ [user "start"]
                    , [user "start"]
                    ]

        it "synthesizes a Tool not found result for historical pending tools and continues" $ do
            historyRef <- IORef.newIORef []
            let pendingCall = Chat.loopToolCall "loop-1"
                pendingCallItem = Chat.responsesToolCallItem ItemPending pendingCall
                finalAssistant = Chat.responsesAssistantItem ItemCompleted "done"

            result <-
                runStorageReplayTest historyRef [Chat.finalProviderRound [finalAssistant]] do
                    convId <- createConversation
                    appendItems convId [user "start", pendingCallItem]
                    outcome <-
                        withStorageBy
                            chatOutcomeAppendedItems
                            (chatOutcome defaultChatConfig)
                            convId
                    storedHistory <- getConversation convId
                    pure (outcome, storedHistory)

            fmap
                ( \(outcome, storedHistory) ->
                    ( Chat.stripHistoryItemIdsFromOutcome outcome
                    , Chat.stripHistoryItemIds storedHistory
                    )
                )
                result
                `shouldBe` Right
                    ( Chat.stripHistoryItemIdsFromOutcome
                        (ChatFinished
                            { appendedItems =
                                [ toolResultText "loop-1" "Tool not found: loop_tool"
                                , finalAssistant
                                ]
                            })
                    , Chat.stripHistoryItemIds
                        [ user "start"
                        , pendingCallItem
                        , toolResultText "loop-1" "Tool not found: loop_tool"
                        , finalAssistant
                        ]
                    )
            recordedHistories <- IORef.readIORef historyRef
            recordedHistories
                `shouldBe`
                    [ [ user "start"
                      , pendingCallItem
                      , toolResultText "loop-1" "Tool not found: loop_tool"
                      ]
                    ]

        it "loses same-provider media replay after restart with the default ephemeral media store" $ do
            let payload =
                    Render.responsesAssistantPayloadWithContent
                        "item-openai-image"
                        [ object
                            [ "type" .= ("input_image" :: Text)
                            , "image_url" .= ("https://example.com/cat.png" :: Text)
                            ]
                        ]
            decodedRound <-
                case decodeOpenAIResponse (Render.responsesResponse "response-openai" "completed" [payload]) of
                    Left err -> expectationFailure ("Expected OpenAI response to decode: " <> show err) >> fail "unreachable"
                    Right roundValue -> pure roundValue

            result <-
                runEff
                    . runConcurrent
                    . runTime
                    . runErrorNoCallStackWith @ChatStorageError (error . show)
                    . runRakeStorageInMemory
                    $ do
                        let OpenAIChatSettings
                                { apiKey = defaultApiKey
                                , model = defaultModel
                                , organizationId = defaultOrganizationId
                                , projectId = defaultProjectId
                                } = defaultOpenAIChatSettings "test-api-key"
                            settings =
                                OpenAIChatSettings
                                    { apiKey = defaultApiKey
                                    , model = defaultModel
                                    , baseUrl = Render.unreachableBaseUrl
                                    , organizationId = defaultOrganizationId
                                    , projectId = defaultProjectId
                                    , requestLogger = \_ -> pure ()
                                    }
                        convId <- createConversation
                        appendItems convId [user "show me a cat"]

                        firstResult <-
                            runErrorNoCallStack @RakeError
                                $ runRakeMediaStorageInMemory
                                $ Chat.runMockRake [decodedRound]
                                $ withResumableChat defaultChatConfig convId
                        _ <- either (error . show) pure firstResult

                        appendItems convId [user "what next?"]

                        runErrorNoCallStack @RakeError
                            $ runRakeMediaStorageInMemory
                            $ runRakeOpenAIChat settings
                            $ withResumableChat defaultChatConfig convId

            result
                `shouldBe` Left
                    (LlmExpectationError "No stored media reference is available for blob openai.responses-response-openai-item-openai-image-0 when rendering openai.responses")

        it "replays same-provider media after restart when using the directory media store" $
            withTempMediaDirectory \mediaDirectory -> do
                let payload =
                        Render.responsesAssistantPayloadWithContent
                            "item-openai-image"
                            [ object
                                [ "type" .= ("input_image" :: Text)
                                , "image_url" .= ("https://example.com/cat.png" :: Text)
                                ]
                            ]
                decodedRound <-
                    case decodeOpenAIResponse (Render.responsesResponse "response-openai" "completed" [payload]) of
                        Left err -> expectationFailure ("Expected OpenAI response to decode: " <> show err) >> fail "unreachable"
                        Right roundValue -> pure roundValue

                requestRef <- IORef.newIORef Nothing

                result <-
                    runEff
                        . runConcurrent
                        . runTime
                        . runErrorNoCallStackWith @ChatStorageError (error . show)
                        . runRakeStorageInMemory
                        $ do
                            let OpenAIChatSettings
                                    { apiKey = defaultApiKey
                                    , model = defaultModel
                                    , organizationId = defaultOrganizationId
                                    , projectId = defaultProjectId
                                    } = defaultOpenAIChatSettings "test-api-key"
                                settings =
                                    OpenAIChatSettings
                                        { apiKey = defaultApiKey
                                        , model = defaultModel
                                        , baseUrl = Render.unreachableBaseUrl
                                        , organizationId = defaultOrganizationId
                                        , projectId = defaultProjectId
                                        , requestLogger = Render.recordRequest requestRef
                                        }
                            convId <- createConversation
                            appendItems convId [user "show me a cat"]

                            firstResult <-
                                runErrorNoCallStack @RakeError
                                    $ runRakeMediaStorageDirectory mediaDirectory
                                    $ Chat.runMockRake [decodedRound]
                                    $ withResumableChat defaultChatConfig convId
                            _ <- either (error . show) pure firstResult

                            appendItems convId [user "what next?"]

                            runErrorNoCallStack @RakeError
                                $ runRakeMediaStorageDirectory mediaDirectory
                                $ runRakeOpenAIChat settings
                                $ withResumableChat defaultChatConfig convId

                result `shouldSatisfy` isLeft
                requestBody <- Render.readRequest requestRef
                Render.lookupPath ["input"] requestBody
                    `shouldBe` Just
                        ( toJSON
                            ( [ object
                                    [ "role" .= ("user" :: Text)
                                    , "content" .= ("show me a cat" :: Text)
                                    ]
                              , object
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

    describe "stored generic media replay" $ do
        forM_ storedMediaRenderCases $ \StoredMediaRenderCase{caseName, providerFamily, captureStoredRequestBody} -> do
            it ("re-renders stored generic image history for " <> caseName) $ do
                let blobId = "blob-image-1"
                    requestPart = imageRequestPart providerFamily
                requestBody <-
                    captureStoredRequestBody
                        [MediaProviderReference{mediaBlobId = blobId, providerFamily, providerRequestPart = requestPart}]
                        [userParts [imagePart blobId (Just "image/png") (Just "diagram")]]

                Render.lookupPath ["input"] requestBody
                    `shouldBe` Just
                        ( toJSON
                            ( [storedUserInputStep providerFamily requestPart]
                                :: [Value]
                            )
                        )

            it ("re-renders stored generic audio history for " <> caseName) $ do
                let blobId = "blob-audio-1"
                    requestPart = audioRequestPart providerFamily
                requestBody <-
                    captureStoredRequestBody
                        [MediaProviderReference{mediaBlobId = blobId, providerFamily, providerRequestPart = requestPart}]
                        [userParts [audioPart blobId (Just "audio/wav") (Just "spoken note")]]

                Render.lookupPath ["input"] requestBody
                    `shouldBe` Just
                        ( toJSON
                            ( [storedUserInputStep providerFamily requestPart]
                                :: [Value]
                            )
                        )

    describe "provider replay normalization" $ do
        it "turns unresolved pending OpenAI tool calls into Tool not found results before replay" $ do
            historyRef <- IORef.newIORef []
            result <-
                Chat.runHistoryRecordedMockChatOutcome historyRef [] $
                    chatOutcome defaultChatConfig [pendingOpenAiToolCallItem]

            result
                `shouldBe` Right
                    (ChatFailed
                        { appendedItems =
                            [ toolResultText "tool-call-1" "Tool not found: lookup"
                            , Chat.replayBarrierItem "Unexpected provider round request in test"
                            ]
                        , failureReason = FailureContract "Unexpected provider round request in test"
                        }
                    )
            recordedHistories <- IORef.readIORef historyRef
            recordedHistories
                `shouldBe`
                    [ [ pendingOpenAiToolCallItem
                      , toolResultText "tool-call-1" "Tool not found: lookup"
                      ]
                    ]

        it "turns unresolved pending xAI tool calls into Tool not found results before replay" $ do
            historyRef <- IORef.newIORef []
            result <-
                Chat.runHistoryRecordedMockChatOutcome historyRef [] $
                    chatOutcome defaultChatConfig [pendingXaiToolCallItem]

            result
                `shouldBe` Right
                    (ChatFailed
                        { appendedItems =
                            [ toolResultText "tool-call-1" "Tool not found: lookup"
                            , Chat.replayBarrierItem "Unexpected provider round request in test"
                            ]
                        , failureReason = FailureContract "Unexpected provider round request in test"
                        }
                    )
            recordedHistories <- IORef.readIORef historyRef
            recordedHistories
                `shouldBe`
                    [ [ pendingXaiToolCallItem
                      , toolResultText "tool-call-1" "Tool not found: lookup"
                      ]
                    ]

        it "normalizes unresolved pending Responses tool calls before provider switching" $ do
            historyRef <- IORef.newIORef []
            result <-
                Chat.runHistoryRecordedMockChatOutcome historyRef [] $
                    chatOutcome defaultChatConfig [pendingOpenAiToolCallItem]

            result
                `shouldBe` Right
                    (ChatFailed
                        { appendedItems =
                            [ toolResultText "tool-call-1" "Tool not found: lookup"
                            , Chat.replayBarrierItem "Unexpected provider round request in test"
                            ]
                        , failureReason = FailureContract "Unexpected provider round request in test"
                        }
                    )
            recordedHistories <- IORef.readIORef historyRef
            recordedHistories
                `shouldBe`
                    [ [ pendingOpenAiToolCallItem
                      , toolResultText "tool-call-1" "Tool not found: lookup"
                      ]
                    ]

        it "does not replay pending Gemini function_result items as provider input" $ do
            requestBody <-
                Render.captureGeminiRequestBody
                    defaultChatConfig
                    [pendingGeminiFunctionResultItem]

            Render.lookupPath ["input"] requestBody
                `shouldBe` Just (toJSON ([] :: [Value]))

        it "does not replay Gemini thought artifacts on same-provider restart" $ do
            requestBody <-
                Render.captureGeminiRequestBody
                    defaultChatConfig
                    [pendingGeminiUnusedThoughtItem]

            Render.lookupPath ["input"] requestBody
                `shouldBe` Just (toJSON ([] :: [Value]))

        it "keeps non-portable Gemini thought artifacts out of replay while still marking them pending" $ do
            let replayState =
                    conversationReplayState
                        [ pendingGeminiUnusedThoughtItem
                        , toolResultText "tool-call-1" "hello"
                        , pendingGeminiToolCallItem
                        ]
                ReplayState{replayHistory, resumableToolCalls, pendingArtifacts, blocked} = replayState

            replayHistory `shouldBe` []
            resumableToolCalls
                `shouldBe`
                    [ ToolCall
                        { toolCallId = "tool-call-1"
                        , toolName = "lookup"
                        , toolArgs = fromList [("name", String "Ada")]
                        , continuationAttachments = []
                        }
                    ]
            pendingArtifacts `shouldBe` [pendingGeminiUnusedThoughtItem, pendingGeminiToolCallItem]
            blocked `shouldBe` Nothing

        it "normalizes unresolved pending Responses tool calls before switching into Gemini" $ do
            historyRef <- IORef.newIORef []
            result <-
                Chat.runHistoryRecordedMockChatOutcome historyRef [] $
                    chatOutcome defaultChatConfig [pendingOpenAiToolCallItem]

            result
                `shouldBe` Right
                    (ChatFailed
                        { appendedItems =
                            [ toolResultText "tool-call-1" "Tool not found: lookup"
                            , Chat.replayBarrierItem "Unexpected provider round request in test"
                            ]
                        , failureReason = FailureContract "Unexpected provider round request in test"
                        }
                    )
            recordedHistories <- IORef.readIORef historyRef
            recordedHistories
                `shouldBe`
                    [ [ pendingOpenAiToolCallItem
                      , toolResultText "tool-call-1" "Tool not found: lookup"
                      ]
                    ]

        it "keeps resolved tool exchanges replayable after the tool disappears" $ do
            let toolResultItem = toolResultText "tool-call-1" "Contacts:\n- Ada"
                expectedInput =
                    [ object
                        [ "type" .= ("function_call" :: Text)
                        , "call_id" .= ("tool-call-1" :: Text)
                        , "name" .= ("lookup" :: Text)
                        , "arguments" .= ("{\"name\":\"Ada\"}" :: Text)
                        ]
                    , object
                        [ "type" .= ("function_call_output" :: Text)
                        , "call_id" .= ("tool-call-1" :: Text)
                        , "output" .= ("Contacts:\n- Ada" :: Text)
                        ]
                    ]

            requestBody <-
                Render.captureOpenAIRequestBody
                    defaultChatConfig
                    [pendingOpenAiToolCallItem, toolResultItem]

            Render.lookupPath ["input"] requestBody
                `shouldBe` Just (toJSON (expectedInput :: [Value]))

runStorageReplayTest
    :: IORef.IORef [[HistoryItem]]
    -> [ProviderRound]
    -> Eff
        '[ Rake
         , Error RakeError
         , RakeStorage
         , Error ChatStorageError
         , RakeMediaStorage
         , Time
         , Concurrent
         , IOE
         ]
        a
    -> IO (Either ChatStorageError a)
runStorageReplayTest historyRef plannedRounds action =
    runEff
        . runConcurrent
        . runTime
        . runRakeMediaStorageInMemory
        . runErrorNoCallStack
        . runRakeStorageInMemory
        $ do
            result <- runErrorNoCallStack @RakeError (Chat.runHistoryRecordedMockRake historyRef plannedRounds action)
            either (error . show) pure result

data StoredMediaRenderCase = StoredMediaRenderCase
    { caseName :: String
    , providerFamily :: ProviderApiFamily
    , captureStoredRequestBody :: [MediaProviderReference] -> [HistoryItem] -> IO Value
    }

storedMediaRenderCases :: [StoredMediaRenderCase]
storedMediaRenderCases =
    [ StoredMediaRenderCase
        { caseName = "OpenAI"
        , providerFamily = ProviderOpenAIResponses
        , captureStoredRequestBody = captureStoredOpenAIRequestBody
        }
    , StoredMediaRenderCase
        { caseName = "xAI"
        , providerFamily = ProviderXAIResponses
        , captureStoredRequestBody = captureStoredXAIRequestBody
        }
    , StoredMediaRenderCase
        { caseName = "Gemini"
        , providerFamily = ProviderGeminiInteractions
        , captureStoredRequestBody = captureStoredGeminiRequestBody
        }
    ]

captureStoredOpenAIRequestBody :: [MediaProviderReference] -> [HistoryItem] -> IO Value
captureStoredOpenAIRequestBody mediaReferences history = do
    requestRef <- IORef.newIORef Nothing
    let OpenAIChatSettings
            { apiKey = defaultApiKey
            , model = defaultModel
            , organizationId = defaultOrganizationId
            , projectId = defaultProjectId
            } = defaultOpenAIChatSettings "test-api-key"
        settings =
            OpenAIChatSettings
                { apiKey = defaultApiKey
                , model = defaultModel
                , baseUrl = Render.unreachableBaseUrl
                , organizationId = defaultOrganizationId
                , projectId = defaultProjectId
                , requestLogger = Render.recordRequest requestRef
                }

    result <-
        runEff
            . runConcurrent
            . runTime
            . runErrorNoCallStackWith @ChatStorageError (error . show)
            . runRakeStorageInMemory
            $ do
                convId <- createConversation
                appendItems convId history
                runErrorNoCallStack @RakeError
                    $ runRakeMediaStorageInMemory
                    $ do
                        saveMediaReferences mediaReferences
                        runRakeOpenAIChat settings
                            $ void
                            $ withResumableChat defaultChatConfig convId

    result `shouldSatisfy` isLeft
    Render.readRequest requestRef

captureStoredXAIRequestBody :: [MediaProviderReference] -> [HistoryItem] -> IO Value
captureStoredXAIRequestBody mediaReferences history = do
    requestRef <- IORef.newIORef Nothing
    let XAIChatSettings
            { apiKey = defaultApiKey
            , model = defaultModel
            } = defaultXAIChatSettings "test-api-key"
        settings =
            XAIChatSettings
                { apiKey = defaultApiKey
                , model = defaultModel
                , baseUrl = Render.unreachableBaseUrl
                , requestLogger = Render.recordRequest requestRef
                }

    result <-
        runEff
            . runConcurrent
            . runTime
            . runErrorNoCallStackWith @ChatStorageError (error . show)
            . runRakeStorageInMemory
            $ do
                convId <- createConversation
                appendItems convId history
                runErrorNoCallStack @RakeError
                    $ runRakeMediaStorageInMemory
                    $ do
                        saveMediaReferences mediaReferences
                        runRakeXAIChat settings
                            $ void
                            $ withResumableChat defaultChatConfig convId

    result `shouldSatisfy` isLeft
    Render.readRequest requestRef

captureStoredGeminiRequestBody :: [MediaProviderReference] -> [HistoryItem] -> IO Value
captureStoredGeminiRequestBody mediaReferences history = do
    requestRef <- IORef.newIORef Nothing
    let GeminiChatSettings
            { apiKey = defaultApiKey
            , model = defaultModel
            , providerTools = defaultProviderTools
            , generationConfig = defaultGenerationConfig
            } = defaultGeminiChatSettings "test-api-key"
        settings =
            GeminiChatSettings
                { apiKey = defaultApiKey
                , model = defaultModel
                , baseUrl = Render.unreachableBaseUrl
                , systemInstruction = Nothing
                , providerTools = defaultProviderTools
                , generationConfig = defaultGenerationConfig
                , requestLogger = Render.recordRequest requestRef
                }

    result <-
        runEff
            . runConcurrent
            . runTime
            . runErrorNoCallStackWith @ChatStorageError (error . show)
            . runRakeStorageInMemory
            $ do
                convId <- createConversation
                appendItems convId history
                runErrorNoCallStack @RakeError
                    $ runRakeMediaStorageInMemory
                    $ do
                        saveMediaReferences mediaReferences
                        runRakeGeminiChat settings
                            $ void
                            $ withResumableChat defaultChatConfig convId

    result `shouldSatisfy` isLeft
    Render.readRequest requestRef

storedUserInputStep :: ProviderApiFamily -> Value -> Value
storedUserInputStep providerFamily requestPart =
    case providerFamily of
        ProviderGeminiInteractions ->
            object
                [ "type" .= ("user_input" :: Text)
                , "content" .= ([requestPart] :: [Value])
                ]
        _ ->
            object
                [ "role" .= ("user" :: Text)
                , "content" .= ([requestPart] :: [Value])
                ]

imageRequestPart :: ProviderApiFamily -> Value
imageRequestPart = \case
    ProviderOpenAIResponses ->
        responsesImageRequestPart
    ProviderXAIResponses ->
        responsesImageRequestPart
    ProviderGeminiInteractions ->
        object
            [ "type" .= ("image" :: Text)
            , "uri"
                .= ( "https://raw.githubusercontent.com/philschmid/gemini-samples/refs/heads/main/assets/cats-and-dogs.jpg"
                        :: Text
                   )
            ]
    ProviderApiFamily{} ->
        error "Unexpected provider family in ReplaySpec imageRequestPart"
  where
    responsesImageRequestPart =
        object
            [ "type" .= ("input_image" :: Text)
            , "image_url"
                .= ( "https://raw.githubusercontent.com/philschmid/gemini-samples/refs/heads/main/assets/cats-and-dogs.jpg"
                        :: Text
                   )
            ]

audioRequestPart :: ProviderApiFamily -> Value
audioRequestPart = \case
    ProviderOpenAIResponses ->
        responsesAudioRequestPart
    ProviderXAIResponses ->
        responsesAudioRequestPart
    ProviderGeminiInteractions ->
        geminiInlineMediaRequestPart "audio" "audio/wav" "UklGRg=="
    ProviderApiFamily{} ->
        error "Unexpected provider family in ReplaySpec audioRequestPart"
  where
    responsesAudioRequestPart =
        object
            [ "type" .= ("input_audio" :: Text)
            , "input_audio"
                .= object
                    [ "data" .= ("UklGRg==" :: Text)
                    , "format" .= ("wav" :: Text)
                    ]
            ]

geminiInlineMediaRequestPart :: Text -> Text -> Text -> Value
geminiInlineMediaRequestPart mediaType mimeType base64Data =
    object
        [ "type" .= mediaType
        , "mime_type" .= mimeType
        , "data" .= base64Data
        ]

pendingOpenAiToolCallItem :: HistoryItem
pendingOpenAiToolCallItem =
    Render.nativeHistoryItem
        ProviderOpenAIResponses
        ItemPending
        "response-openai"
        (Just "item-openai-tool")
        ( pendingResponsesToolCallPayload
            "item-openai-tool"
            "tool-call-1"
            "lookup"
        )

pendingXaiToolCallItem :: HistoryItem
pendingXaiToolCallItem =
    Render.nativeHistoryItem
        ProviderXAIResponses
        ItemPending
        "response-xai"
        (Just "item-xai-tool")
        ( pendingResponsesToolCallPayload
            "item-xai-tool"
            "tool-call-1"
            "lookup"
        )

pendingGeminiFunctionResultItem :: HistoryItem
pendingGeminiFunctionResultItem =
    Render.nativeHistoryItem
        ProviderGeminiInteractions
        ItemPending
        "interaction-gemini"
        (Just "tool-call-1")
        (Render.nativeGeminiToolResultPayload "hello")

pendingGeminiUnusedThoughtItem :: HistoryItem
pendingGeminiUnusedThoughtItem =
    nonPortableHistoryItem
        ItemPending
        ProviderItem
            { apiFamily = ProviderGeminiInteractions
            , exchangeId = Just "interaction-gemini"
            , nativeItemId = Just "thought-1"
            , payload = Render.geminiThoughtPayload "thought-1"
            , availableLocalTools = []
            }

pendingGeminiToolCallItem :: HistoryItem
pendingGeminiToolCallItem =
    Render.nativeHistoryItem
        ProviderGeminiInteractions
        ItemPending
        "interaction-gemini"
        (Just "tool-call-1")
        (Render.geminiFunctionCallPayload "tool-call-1" "lookup" (object ["name" .= ("Ada" :: Text)]))

pendingGeminiToolCallWithThoughtItem :: Text -> HistoryItem
pendingGeminiToolCallWithThoughtItem contactName =
    HistoryItem
        { historyItemIdField = Nothing
        , itemLifecycle = ItemPending
        , genericItem =
            GenericToolCall
                { toolCall =
                    ToolCall
                        { toolCallId = "tool-call-1"
                        , toolName = "lookup"
                        , toolArgs = fromList [("name", String contactName)]
                        , continuationAttachments =
                            [ ToolCallContinuation
                                { continuationProviderFamily = ProviderGeminiInteractions
                                , continuationPayload = Render.geminiThoughtPayload "thought-1"
                                }
                            ]
                        }
                }
        , providerItem =
            Just
                ProviderItem
                    { apiFamily = ProviderGeminiInteractions
                    , exchangeId = Just "interaction-gemini"
                    , nativeItemId = Just "tool-call-1"
                    , payload = Render.geminiFunctionCallPayload "tool-call-1" "lookup" (object ["name" .= contactName])
                    , availableLocalTools = []
                    }
        }

pendingResponsesToolCallPayload :: Text -> Text -> Text -> Value
pendingResponsesToolCallPayload itemId callId toolName =
    Render.responsesToolCallPayload
        itemId
        callId
        toolName
        (object ["name" .= ("Ada" :: Text)])
        "in_progress"

withHistoryId :: Word32 -> HistoryItem -> HistoryItem
withHistoryId suffix =
    setHistoryItemId (Just (historyItemIdAt suffix))

historyItemIdAt :: Word32 -> HistoryItemId
historyItemIdAt suffix =
    HistoryItemId (UUID.fromWords 0 0 0 suffix)

itemIdOf :: HistoryItem -> HistoryItemId
itemIdOf historyItem =
    fromMaybe
        (error "Expected HistoryItem to have an embedded id in ReplaySpec")
        (historyItemId historyItem)

withTempMediaDirectory :: (FilePath -> IO a) -> IO a
withTempMediaDirectory action = do
    tempRoot <- Directory.getTemporaryDirectory
    uuid <- nextRandom
    let mediaDirectory = tempRoot </> ("ai-rake-media-" <> show uuid)
    Directory.createDirectoryIfMissing True mediaDirectory
    action mediaDirectory
        `finally` Directory.removePathForcibly mediaDirectory
