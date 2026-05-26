# ai-rake

A Haskell library for provider-agnostic LLM chat with local tool execution, Effectful integration, and storage backends.

## Basic Usage

```haskell
{-# LANGUAGE DataKinds #-}

import Effectful
import Effectful.Concurrent
import Effectful.Error.Static
import Rake
import Rake.Error (renderRakeError)
import Rake.Providers.OpenAI.Chat
import Relude
import System.Environment (getEnv)

main :: IO ()
main = do
  apiKey <- toText <$> getEnv "OPENAI_API_KEY"
  runEff
    . runConcurrent
    . runErrorNoCallStackWith @RakeError (error . toString . renderRakeError)
    $ runRakeOpenAIChat (defaultOpenAIChatSettings apiKey) do
        outcome <-
          chatOutcome
            defaultChatConfig{maxToolRounds = 8}
            [ system "You are a helpful assistant."
            , user "What is 2 + 2?"
            ]

        case outcome of
          ChatFinished{appendedItems} ->
            print (lastAssistantTextsStrict appendedItems)
          other ->
            print other
```

## Current Scope

- Canonical conversations are `[HistoryItem]`
- Generic chat entrypoint is `chatOutcome`
- `withResumableChat` is the recommended durable wrapper for agent loops that need persistence
- `streamChatOutcome` and `withResumableStreamingChat` add ephemeral shared streaming on top of the same durable history model
- Supported chat providers are OpenAI Chat, xAI Chat, and Gemini Chat
- Shared/local history items cover text messages, tool calls, and tool results
- Provider-native history is preserved for OpenAI Responses, xAI Responses, and Gemini Interactions items
- Local storage backends support in-memory and PostgreSQL persistence

The old `ChatMsg` API and the `openai` package dependency have been removed.

Notes:

- `lastAssistantTexts` and `decodeLastAssistant` are best-effort helpers.
- `lastAssistantTextsStrict` and `decodeLastAssistantStrict` only look at the latest contiguous assistant tail.
- Canonical conversations are append-only agent logs. Provider-native items can be marked `ItemPending` or `ItemCompleted` inside persisted history.
- Anything named `appendedItems` is the newly returned append-only suffix, not the full conversation history.
- History items get stable `HistoryItemId`s before replay and persistence. When storage is used, the embedded item id matches the storage row id.
- Provider-backed history items store the local `ToolDeclaration`s that were available to the model for the request that produced the provider item. This is debug/provenance data only; replay and tool execution still use the current `ChatConfig.tools`.
- `chatOutcome` and `withResumableChat` require `IOE` because the library assigns `HistoryItemId`s before replay and persistence.
- `streamChatOutcome` and `withResumableStreamingChat` also require `IOE`; they surface live assistant text/refusal deltas through `StreamCallbacks` but still only persist finalized `HistoryItem` suffixes.
- Set `llmCallTimeout = Just 60` on `ChatConfig` to bound each individual provider LLM round. This is separate from `maxToolRounds`, which only bounds local tool-loop recursion.
- `chatOutcome` returns an append-only canonical suffix plus explicit pause/failure state. Failed outcomes append a durable `ReplayBarrier` control item into the returned suffix.
- `withResumableChat` is the recommended durable wrapper for append-only loops. It persists paused and failed suffixes through `chatOutcome`.
- Shared streaming is text-first today. Provider-native reasoning deltas, partial tool arguments, and partial multimodal outputs stay out of the shared API.
- Stored unresolved tool calls are resumed locally before the next provider request instead of being replayed back at the model.
- Historical unresolved tool calls whose local tool no longer exists are resumed into a synthetic `"Tool not found"` result and the loop continues.
- `chatOutcome` throws `ConversationBlocked` for blocked histories and includes the latest valid reset checkpoint when the supplied history already has stable item ids.
- `validResetCheckpoints`, `latestValidCheckpoint`, and `resetToLatestValidCheckpoint` work with `HistoryItemId` checkpoints, not raw log offsets. Use `resetTo` for item checkpoints and `resetToStart` to rewind to the beginning.
- `withStorage` and `withStorageBy` are snapshot-based helpers; they load the current history, run the action, and append the returned suffix.
- `withStorageBy chatOutcomeAppendedItems (chatOutcome config)` is the advanced low-level storage pattern for resumable agent loops, but `withResumableChat` is the preferred public entrypoint.
- `modifyConversationAtomic` is the short-lived mutation helper to use when concurrent writers matter.
- `renderRakeError` gives user-facing text for blocked conversations, including when no concrete reset checkpoint can be suggested for id-less direct histories.
- Shared/local multipart content is text-only for now; richer media remains provider-native until both adapters support the same generic representation.
- Provider-switch conversion warnings are logged to stderr by default when native items lose information during projection.
- OpenAI and xAI chat adapters target the Responses API; Gemini chat targets the Interactions API.
- Gemini-specific built-in Interactions tools are available through `GeminiChatSettings.providerTools`, while local function tools still flow through the shared chat loop.
- Stored provider item tool provenance covers local Rake tools only; Gemini provider-native `providerTools` are not included in `availableLocalTools`.
- Switching away from Gemini keeps generic unresolved tool state portable. Gemini-only pending `thought` metadata is reused only for same-provider Gemini continuation and is dropped when replaying into OpenAI/xAI.
- Gemini image generation, OpenAI image generation, and xAI Grok Imagine image/video generation are available through provider-specific helper modules instead of the shared chat API.
- Standalone TTS helpers are available for OpenAI and xAI through the shared `tts` and `ttsStreaming` entrypoints, plus provider-specific modules when you need provider-native settings.

## Persistence

PostgreSQL storage now uses validated identifiers instead of raw table-name `Text`:

```haskell
{-# LANGUAGE DataKinds #-}

import Database.PostgreSQL.Simple (close, connectPostgreSQL)
import Effectful
import Effectful.PostgreSQL.Connection.Pool
import Rake
import Rake.Storage.Postgres
import Relude
import UnliftIO.Pool (mkDefaultPoolConfig, newPool)

main :: IO ()
main = do
  prefix <- either (error . toString) pure (mkPgIdentifier "ai_rake")
  tables <- either (error . toString) pure (conversationTablesFromPrefix prefix)
  config <- mkDefaultPoolConfig (connectPostgreSQL "dbname=chatcompletion-test") close 60 5
  pool <- newPool config

  runEff
    . runWithConnectionPool pool
    $ setupConversationTables tables
```

Use `withResumableChat config conversationId` for resumable append-only agent logs, `withStorageBy chatOutcomeAppendedItems (chatOutcome config)` for advanced manual suffix persistence, and `modifyConversationAtomic` for short-lived read-modify-write mutations that must serialize correctly under concurrent access.

`chatOutcome` is the durable path for failed rounds. If a run returns `ChatFailed`, the returned `appendedItems` suffix includes a `ReplayBarrier`, and later `chatOutcome`/`withResumableChat` calls will refuse to continue until you append a reset control item that rewinds before the blocked suffix.

PostgreSQL item ids are unique per conversation, not globally. `setupConversationTables` also performs a best-effort migration away from the library's older global `item_id` uniqueness constraint; if an older installation renamed that legacy constraint manually, drop it yourself and rerun setup.

Minimal resumable loop:

```haskell
outcome <-
  withResumableChat
    defaultChatConfig{tools = [myTool]}
    conversationId

case outcome of
  ChatFinished{appendedItems} ->
    print (lastAssistantTextsStrict appendedItems)

  ChatPaused{pauseReason} ->
    print pauseReason

  ChatFailed{failureReason} ->
    print failureReason
```

Recover from a blocked append-only history:

```haskell
history <- getConversation conversationId

case resetToLatestValidCheckpoint history of
  Nothing ->
    pure ()
  Just rewindItem ->
    appendItems conversationId [rewindItem]
```

If you need to present all reset options to a user, inspect `validResetCheckpoints history`. The checkpoints target `HistoryItemId`s from the active conversation branch, not append-log offsets.
Compute those checkpoints from the full stored conversation, not from `appendedItems`.

## Media Generation

```haskell
{-# LANGUAGE DataKinds #-}

import Effectful
import Effectful.Error.Static
import Rake
import Rake.Error (renderRakeError)
import Rake.Providers.Gemini.Images
import Rake.Providers.Gemini.Videos
import Rake.Providers.OpenAI.Images
import Rake.Providers.XAI.Imagine
import Relude
import System.Environment (getEnv)

main :: IO ()
main = do
  geminiKey <- toText <$> getEnv "GEMINI_API_KEY"
  openAiKey <- toText <$> getEnv "OPENAI_API_KEY"
  xaiKey <- toText <$> getEnv "XAI_API_KEY"
  runEff
    . runErrorNoCallStackWith @RakeError (error . toString . renderRakeError)
    $ do
        geminiImage <-
          generateGeminiImage
            (defaultGeminiImagesSettings geminiKey)
            (defaultGeminiImageRequest "A tiny watercolor postcard of a lighthouse")

        openAiImage <-
          generateOpenAIImage
            (defaultOpenAIImagesSettings openAiKey)
            (defaultOpenAIImageRequest "A clean product photo of a ceramic mug on linen")

        xaiVideo <-
          generateXAIVideo
            (defaultXAIImagineSettings xaiKey)
            ( defaultXAIImagineVideoRequest
                "Animate this still into a gentle dusk timelapse"
            ){imageUrl = Just "https://example.com/still.png", duration = Just 8}

        veoVideo <-
          generateGeminiVideo
            (defaultGeminiVideoSettings geminiKey)
            ( defaultGeminiVideoRequest
                "A cinematic shot of a ghostly swing moving from the first frame to the last"
            )

        print geminiImage
        print openAiImage
        print xaiVideo
        print veoVideo
```

Validation notes:

- OpenAI `mask` and `inputFidelity` require at least one input image.
- OpenAI `gpt-image-2` does not support transparent backgrounds or explicit `inputFidelity`.
- xAI `videoUrl` requests are edit operations and cannot be combined with `duration`, `aspectRatio`, or `resolution`.
- xAI video requests may set `imageUrl` or `videoUrl`, but not both.
- Gemini Veo `image` requests animate one still image as the first frame; `lastFrame` must also set `image` and is only for first-frame/last-frame interpolation.

## Text To Speech

```haskell
{-# LANGUAGE DataKinds #-}

import Data.ByteString qualified as BS
import Effectful
import Effectful.Error.Static
import Rake
import Rake.Error (renderRakeError)
import Rake.Providers.OpenAI.TTS
import Relude
import System.Environment (getEnv)

main :: IO ()
main = do
  apiKey <- toText <$> getEnv "OPENAI_API_KEY"
  runEff
    . runErrorNoCallStackWith @RakeError (error . toString . renderRakeError)
    $ do
        audio <-
          tts
            (TTSOpenAI (defaultOpenAITTSSettings apiKey))
            "Hello from ai-rake."

        liftIO (BS.writeFile "hello.mp3" audioBytes)
```

Use `ttsStreaming` when you want chunk callbacks during generation and the final aggregated `Audio` value at the end:

```haskell
audio <-
  ttsStreaming
    TTSStreamCallbacks
      { onAudioChunk = \chunk ->
          liftIO (putStrLn ("received " <> show (BS.length chunk) <> " bytes"))
      }
    (TTSStreamingXAI (defaultXAITTSStreamingSettings xaiKey))
    "Hello from ai-rake."
```

## CLIs

Build and run the image CLI with:

```bash
cabal run rake-image -- xai "a man riding a horse on the moon"
```

Build and run the video CLI with:

```bash
cabal run rake-video -- xai "She walk away" --image girl.jpg
cabal run rake-video -- xai --extend clip.mp4 "continue the scene for 5 more seconds"
cabal run rake-video -- veo --image still.png "animate this still frame"
cabal run rake-video -- veo --image start.png --last-frame end.png "transition between these frames"
```

Build and run the speech CLI with:

```bash
cabal run rake-tts -- openai "Hello from ai-rake."
cabal run rake-tts -- xai --codec=wav --sample-rate=24000 -o update.wav "A short status update"
```

Defaults:

- `rake-image gptimage ...` uses OpenAI `gpt-image-2`
- `rake-image xai ...` uses xAI Grok Imagine image generation
- `rake-image banana2 ...` uses Gemini `gemini-2.5-flash-image`
- `rake-video xai ...` uses xAI Grok Imagine video generation
- `rake-video veo ...` uses Google Veo with default model `veo-3.1-generate-preview`
- `rake-tts openai ...` uses OpenAI TTS with default model `gpt-4o-mini-tts`
- `rake-tts xai ...` uses xAI TTS with default voice `eve`
- No `--output` writes to `./generated/<timestamp>-<slug>.png` for images
- No `--output` writes to `./generated/<timestamp>-<slug>.mp4` for videos
- No `--output` plays speech locally by default; local playback looks for `ffplay`, `mpv`, or `afplay`
- `rake-tts --output PATH ...` saves speech to `PATH` instead of playing it
- `rake-video --extend ...` performs a true append by extracting the last frame locally, generating a continuation from that frame, and concatenating the clips; it requires local `ffmpeg` and `ffprobe`

The CLIs read `OPENAI_API_KEY` for `gptimage` and `rake-tts openai`, `XAI_API_KEY` for `rake-image xai`, `rake-video xai`, and `rake-tts xai`, and `GEMINI_API_KEY` for `banana2` and `rake-video veo`.

Use provider-specific help to see all available controls:

```bash
cabal run rake-image -- --help
cabal run rake-image -- gptimage --help
cabal run rake-image -- xai --help
cabal run rake-image -- banana2 --help
cabal run rake-video -- --help
cabal run rake-video -- xai --help
cabal run rake-video -- veo --help
cabal run rake-tts -- --help
cabal run rake-tts -- openai --help
cabal run rake-tts -- xai --help
```
