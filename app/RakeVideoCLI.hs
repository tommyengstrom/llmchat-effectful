module RakeVideoCLI
    ( GenVideoProvider (..)
    , GenVideoHelpTopic (..)
    , CommonGenVideoOptions (..)
    , XAIGenVideoOptions (..)
    , VeoGenVideoOptions (..)
    , GenVideoOptions (..)
    , ParseGenVideoArgsResult (..)
    , parseGenVideoArgs
    , renderGenVideoHelp
    , runGenVideoCli
    , veoAspectRatioForImageDimensions
    ) where

import Data.ByteString qualified as BS
import Effectful
import Effectful.Error.Static
import RakeCliSupport
import Data.Text qualified as T
import Rake
import Rake.Providers.Gemini.Videos
import Rake.Providers.XAI.Imagine
import Relude hiding (exitFailure, getArgs, lookupEnv)
import System.Directory
import System.Environment (getArgs, getProgName, lookupEnv)
import System.Exit (ExitCode (..), exitFailure)
import System.IO qualified as IO
import System.Process (readProcessWithExitCode)

data GenVideoProvider
    = GenVideoProviderXAI
    | GenVideoProviderVeo
    deriving stock (Show, Eq)

data GenVideoHelpTopic
    = GenVideoHelpGeneral
    | GenVideoHelpXAI
    | GenVideoHelpVeo
    deriving stock (Show, Eq)

data CommonGenVideoOptions = CommonGenVideoOptions
    { commonVideoPromptText :: Text
    , commonVideoOutputPath :: Maybe FilePath
    }
    deriving stock (Show, Eq)

data XAIGenVideoOptions = XAIGenVideoOptions
    { xaiVideoCommonOptions :: CommonGenVideoOptions
    , xaiVideoModel :: Text
    , xaiVideoImageSource :: Maybe Text
    , xaiVideoEditSource :: Maybe Text
    , xaiVideoExtendSource :: Maybe Text
    , xaiVideoDuration :: Maybe Int
    , xaiVideoAspectRatio :: Maybe Text
    , xaiVideoResolution :: Maybe Text
    , xaiVideoPollIntervalMilliseconds :: Int
    , xaiVideoMaxPollAttempts :: Int
    }
    deriving stock (Show, Eq)

data VeoGenVideoOptions = VeoGenVideoOptions
    { veoVideoCommonOptions :: CommonGenVideoOptions
    , veoVideoModel :: Text
    , veoVideoImageSource :: Maybe Text
    , veoVideoLastFrameSource :: Maybe Text
    , veoVideoDuration :: Maybe Int
    , veoVideoAspectRatio :: Maybe Text
    , veoVideoResolution :: Maybe Text
    , veoVideoPersonGeneration :: Maybe Text
    , veoVideoSeed :: Maybe Int
    , veoVideoPollIntervalMilliseconds :: Int
    , veoVideoMaxPollAttempts :: Int
    }
    deriving stock (Show, Eq)

data GenVideoOptions
    = GenVideoXAI XAIGenVideoOptions
    | GenVideoVeo VeoGenVideoOptions
    deriving stock (Show, Eq)

data ParseGenVideoArgsResult
    = ParseGenVideoArgsHelp GenVideoHelpTopic
    | ParseGenVideoArgsError Text GenVideoHelpTopic
    | ParseGenVideoArgsSuccess GenVideoOptions
    deriving stock (Show, Eq)

data CommonParseState = CommonParseState
    { parseOutputPath :: Maybe FilePath
    , parsePromptParts :: [Text]
    }

defaultCommonParseState :: CommonParseState
defaultCommonParseState =
    CommonParseState
        { parseOutputPath = Nothing
        , parsePromptParts = []
        }

defaultXAIGenVideoOptions :: XAIGenVideoOptions
defaultXAIGenVideoOptions =
    XAIGenVideoOptions
        { xaiVideoCommonOptions =
            CommonGenVideoOptions
                { commonVideoPromptText = ""
                , commonVideoOutputPath = Nothing
                }
        , xaiVideoModel = "grok-imagine-video"
        , xaiVideoImageSource = Nothing
        , xaiVideoEditSource = Nothing
        , xaiVideoExtendSource = Nothing
        , xaiVideoDuration = Nothing
        , xaiVideoAspectRatio = Nothing
        , xaiVideoResolution = Nothing
        , xaiVideoPollIntervalMilliseconds = 5000
        , xaiVideoMaxPollAttempts = 120
        }

defaultVeoGenVideoOptions :: VeoGenVideoOptions
defaultVeoGenVideoOptions =
    VeoGenVideoOptions
        { veoVideoCommonOptions =
            CommonGenVideoOptions
                { commonVideoPromptText = ""
                , commonVideoOutputPath = Nothing
                }
        , veoVideoModel = "veo-3.1-generate-preview"
        , veoVideoImageSource = Nothing
        , veoVideoLastFrameSource = Nothing
        , veoVideoDuration = Nothing
        , veoVideoAspectRatio = Nothing
        , veoVideoResolution = Nothing
        , veoVideoPersonGeneration = Nothing
        , veoVideoSeed = Nothing
        , veoVideoPollIntervalMilliseconds = 5000
        , veoVideoMaxPollAttempts = 120
        }

runGenVideoCli :: IO ()
runGenVideoCli = do
    progName <- getProgName
    args <- getArgs
    case parseGenVideoArgs (toText <$> args) of
        ParseGenVideoArgsHelp helpTopic ->
            putTextLn (renderGenVideoHelp progName helpTopic)
        ParseGenVideoArgsError err helpTopic -> do
            IO.hPutStrLn IO.stderr (toString err)
            putTextLn (renderGenVideoHelp progName helpTopic)
            exitFailure
        ParseGenVideoArgsSuccess options -> do
            result <- runGenVideo options
            case result of
                Left err -> do
                    IO.hPutStrLn IO.stderr (toString err)
                    exitFailure
                Right savedPaths ->
                    traverse_ (putTextLn . ("Saved video: " <>) . toText) savedPaths

runGenVideo :: GenVideoOptions -> IO (Either Text [FilePath])
runGenVideo = \case
    GenVideoXAI XAIGenVideoOptions
        { xaiVideoCommonOptions = CommonGenVideoOptions{commonVideoPromptText, commonVideoOutputPath}
        , xaiVideoModel
        , xaiVideoImageSource
        , xaiVideoEditSource
        , xaiVideoExtendSource
        , xaiVideoDuration
        , xaiVideoAspectRatio
        , xaiVideoResolution
        , xaiVideoPollIntervalMilliseconds
        , xaiVideoMaxPollAttempts
        } -> do
            maybeApiKey <- lookupEnv "XAI_API_KEY"
            case maybeApiKey of
                Nothing ->
                    pure (Left "XAI_API_KEY is required for xai video generation")
                Just apiKey -> do
                    resolvedImageSource <- traverse resolveMediaSource xaiVideoImageSource
                    resolvedEditSource <- traverse resolveMediaSource xaiVideoEditSource
                    case (sequence resolvedImageSource, sequence resolvedEditSource) of
                        (Left err, _) ->
                            pure (Left err)
                        (_, Left err) ->
                            pure (Left err)
                        (Right maybeImageSource, Right maybeEditSource) ->
                            case (maybeImageSource, maybeEditSource, xaiVideoExtendSource) of
                                (Nothing, Nothing, Nothing) -> do
                                    responseResult <-
                                        runProvider
                                            ( generateXAIVideo
                                                (videoSettings (toText apiKey) xaiVideoPollIntervalMilliseconds xaiVideoMaxPollAttempts)
                                                ( XAIImagineVideoRequest
                                                    { model = xaiVideoModel
                                                    , prompt = commonVideoPromptText
                                                    , imageUrl = Nothing
                                                    , videoUrl = Nothing
                                                    , duration = xaiVideoDuration
                                                    , aspectRatio = xaiVideoAspectRatio
                                                    , resolution = xaiVideoResolution
                                                    }
                                                )
                                            )
                                    handleVideoResponse commonVideoOutputPath commonVideoPromptText responseResult
                                (Just imageSource, Nothing, Nothing) -> do
                                    responseResult <-
                                        runProvider
                                            ( generateXAIVideo
                                                (videoSettings (toText apiKey) xaiVideoPollIntervalMilliseconds xaiVideoMaxPollAttempts)
                                                ( XAIImagineVideoRequest
                                                    { model = xaiVideoModel
                                                    , prompt = commonVideoPromptText
                                                    , imageUrl = Just imageSource
                                                    , videoUrl = Nothing
                                                    , duration = xaiVideoDuration
                                                    , aspectRatio = xaiVideoAspectRatio
                                                    , resolution = xaiVideoResolution
                                                    }
                                                )
                                            )
                                    handleVideoResponse commonVideoOutputPath commonVideoPromptText responseResult
                                (Nothing, Just editSource, Nothing) -> do
                                    responseResult <-
                                        runProvider
                                            ( generateXAIVideo
                                                (videoSettings (toText apiKey) xaiVideoPollIntervalMilliseconds xaiVideoMaxPollAttempts)
                                                ( XAIImagineVideoRequest
                                                    { model = xaiVideoModel
                                                    , prompt = commonVideoPromptText
                                                    , imageUrl = Nothing
                                                    , videoUrl = Just editSource
                                                    , duration = xaiVideoDuration
                                                    , aspectRatio = xaiVideoAspectRatio
                                                    , resolution = xaiVideoResolution
                                                    }
                                                )
                                            )
                                    handleVideoResponse commonVideoOutputPath commonVideoPromptText responseResult
                                (Nothing, Nothing, Just extendSource) ->
                                    extendVideoFromEnd
                                        (toText apiKey)
                                        xaiVideoPollIntervalMilliseconds
                                        xaiVideoMaxPollAttempts
                                        xaiVideoModel
                                        commonVideoPromptText
                                        commonVideoOutputPath
                                        extendSource
                                        xaiVideoDuration
                                        xaiVideoAspectRatio
                                        xaiVideoResolution
                                _ ->
                                    pure (Left "Use at most one of --image, --edit/--video, or --extend.")
    GenVideoVeo VeoGenVideoOptions
        { veoVideoCommonOptions = CommonGenVideoOptions{commonVideoPromptText, commonVideoOutputPath}
        , veoVideoModel
        , veoVideoImageSource
        , veoVideoLastFrameSource
        , veoVideoDuration
        , veoVideoAspectRatio
        , veoVideoResolution
        , veoVideoPersonGeneration
        , veoVideoSeed
        , veoVideoPollIntervalMilliseconds
        , veoVideoMaxPollAttempts
        } -> do
            maybeApiKey <- lookupEnv "GEMINI_API_KEY"
            case maybeApiKey of
                Nothing ->
                    pure (Left "GEMINI_API_KEY is required for Veo video generation")
                Just apiKey -> do
                    resolvedImageSource <- traverse resolveInlineImageSourceWithMetadata veoVideoImageSource
                    resolvedLastFrameSource <- traverse resolveInlineImageSource veoVideoLastFrameSource
                    case (sequence resolvedImageSource, sequence resolvedLastFrameSource) of
                        (Left err, _) ->
                            pure (Left err)
                        (_, Left err) ->
                            pure (Left err)
                        (Right maybeImageSource, Right maybeLastFrameSource) -> do
                            let settings =
                                    veoSettings
                                        (toText apiKey)
                                        veoVideoPollIntervalMilliseconds
                                        veoVideoMaxPollAttempts
                                maybeInlineImageSource =
                                    ( \ResolvedInlineImageSource{inlineImage} ->
                                        inlineImage
                                    )
                                        <$> maybeImageSource
                                inferredAspectRatio = do
                                    ResolvedInlineImageSource{imageDimensions} <- maybeImageSource
                                    veoAspectRatioForImageDimensions <$> imageDimensions
                                effectiveAspectRatio =
                                    veoVideoAspectRatio <|> inferredAspectRatio
                                request :: GeminiVideoRequest
                                request =
                                    GeminiVideoRequest
                                        { model = veoVideoModel
                                        , prompt = commonVideoPromptText
                                        , image = maybeInlineImageSource
                                        , lastFrame = maybeLastFrameSource
                                        , durationSeconds = veoVideoDuration
                                        , aspectRatio = effectiveAspectRatio
                                        , resolution = veoVideoResolution
                                        , personGeneration = veoVideoPersonGeneration
                                        , seed = veoVideoSeed
                                        }
                            responseResult <- runProvider (generateGeminiVideo settings request)
                            handleGeminiVideoResponse settings commonVideoOutputPath commonVideoPromptText responseResult
  where
    videoSettings :: Text -> Int -> Int -> XAIImagineSettings '[Error RakeError, IOE]
    videoSettings apiKey pollIntervalMilliseconds' maxPollAttempts' =
        case defaultXAIImagineSettings apiKey of
            XAIImagineSettings
                { apiKey = settingsApiKey
                , baseUrl = settingsBaseUrl
                , requestLogger = settingsRequestLogger
                } ->
                    XAIImagineSettings
                        { apiKey = settingsApiKey
                        , baseUrl = settingsBaseUrl
                        , pollIntervalMilliseconds = pollIntervalMilliseconds'
                        , maxPollAttempts = maxPollAttempts'
                        , requestLogger = settingsRequestLogger
                        }

    veoSettings :: Text -> Int -> Int -> GeminiVideoSettings '[Error RakeError, IOE]
    veoSettings apiKey pollIntervalMilliseconds' maxPollAttempts' =
        case defaultGeminiVideoSettings apiKey of
            GeminiVideoSettings
                { apiKey = settingsApiKey
                , baseUrl = settingsBaseUrl
                , requestLogger = settingsRequestLogger
                } ->
                    GeminiVideoSettings
                        { apiKey = settingsApiKey
                        , baseUrl = settingsBaseUrl
                        , pollIntervalMilliseconds = pollIntervalMilliseconds'
                        , maxPollAttempts = maxPollAttempts'
                        , requestLogger = settingsRequestLogger
                        }

    runProvider
        :: Eff '[Error RakeError, IOE] a
        -> IO (Either Text a)
    runProvider action = do
        result <-
            runEff
                . runErrorNoCallStack
                $ action
        pure (first renderRakeError result)

    handleVideoResponse maybeOutput promptText responseResult =
        case responseResult of
            Left err ->
                pure (Left err)
            Right XAIVideoResponse{status = XAIVideoDone, video = Just generatedVideo} ->
                saveGeneratedVideo maybeOutput promptText generatedVideo
            Right XAIVideoResponse{status} ->
                pure (Left ("The provider did not return a completed video. Final status: " <> renderVideoStatus status))

    handleGeminiVideoResponse settings maybeOutput promptText responseResult =
        case responseResult of
            Left err ->
                pure (Left err)
            Right operation@GeminiVideoOperation{response = Just videoResponse} ->
                case videoResponse of
                    GeminiGenerateVideoResponse{generatedVideos = generatedVideo : _} ->
                        saveGeminiGeneratedVideo settings maybeOutput promptText generatedVideo
                    GeminiGenerateVideoResponse{generatedVideos = []} ->
                        reportGeminiVideoFailure operation
            Right operation ->
                reportGeminiVideoFailure operation

    saveGeminiGeneratedVideo settings maybeOutput promptText generatedVideo@GeminiGeneratedVideo{uri = maybeVideoUri} = do
        traverse_ (\videoUri -> putTextLn ("Provider video URI: " <> videoUri)) maybeVideoUri
        downloadResult <- runProvider (downloadGeminiVideo settings generatedVideo)
        case downloadResult of
            Left err ->
                pure (Left err)
            Right videoBytes -> do
                outputPaths <- buildOutputPaths "video" maybeOutput promptText [".mp4"]
                traverse_ (`writeBinaryFile` videoBytes) outputPaths
                pure (Right outputPaths)

    reportGeminiVideoFailure operation@GeminiVideoOperation{done} =
        case geminiVideoOperationFailureMessage operation of
            Just failureMessage ->
                pure (Left ("Gemini Veo video generation failed: " <> failureMessage))
            Nothing ->
                pure (Left ("The provider did not return a completed Veo video operation. done=" <> show done))

    extendVideoFromEnd apiKey pollIntervalMilliseconds' maxPollAttempts' modelName promptText maybeOutput sourceVideo duration aspectRatio resolution = do
        ffmpegPathResult <- requireExecutable "ffmpeg"
        ffprobePathResult <- requireExecutable "ffprobe"
        case (ffmpegPathResult, ffprobePathResult) of
            (Left err, _) ->
                pure (Left err)
            (_, Left err) ->
                pure (Left err)
            (Right ffmpegPath, Right ffprobePath) ->
                withPreparedVideoSource sourceVideo $ \sourceVideoPath ->
                    withTempFilePath "gen-video-last-frame" $ \framePath ->
                        withTempFilePath "gen-video-continuation" $ \continuationPath -> do
                            extractFrameResult <- extractLastFrame ffmpegPath sourceVideoPath framePath
                            case extractFrameResult of
                                Left err ->
                                    pure (Left err)
                                Right () -> do
                                    resolvedFrame <- resolveMediaSource (toText framePath)
                                    case resolvedFrame of
                                        Left err ->
                                            pure (Left err)
                                        Right frameSource -> do
                                            responseResult <-
                                                runProvider
                                                    ( generateXAIVideo
                                                        (videoSettings apiKey pollIntervalMilliseconds' maxPollAttempts')
                                                        ( XAIImagineVideoRequest
                                                            { model = modelName
                                                            , prompt = promptText
                                                            , imageUrl = Just frameSource
                                                            , videoUrl = Nothing
                                                            , duration = duration
                                                            , aspectRatio = aspectRatio
                                                            , resolution = resolution
                                                            }
                                                        )
                                                    )
                                            case responseResult of
                                                Left err ->
                                                    pure (Left err)
                                                Right XAIVideoResponse{status = XAIVideoDone, video = Just generatedVideo} -> do
                                                    announceGeneratedVideoUrl "Provider continuation URL" generatedVideo
                                                    downloadedVideo <- downloadGeneratedVideo generatedVideo
                                                    case downloadedVideo of
                                                        Left err ->
                                                            pure (Left err)
                                                        Right continuationBytes -> do
                                                            writeBinaryFile continuationPath continuationBytes
                                                            outputPaths <- buildOutputPaths "video" maybeOutput promptText [".mp4"]
                                                            case outputPaths of
                                                                [outputPath] -> do
                                                                    concatResult <-
                                                                        appendContinuation
                                                                            ffmpegPath
                                                                            ffprobePath
                                                                            sourceVideoPath
                                                                            continuationPath
                                                                            outputPath
                                                                    case concatResult of
                                                                        Left err ->
                                                                            pure (Left err)
                                                                        Right () ->
                                                                            pure (Right [outputPath])
                                                                _ ->
                                                                    pure (Left "Expected exactly one output path for video extension.")
                                                Right XAIVideoResponse{status} ->
                                                    pure (Left ("The provider did not return a completed continuation video. Final status: " <> renderVideoStatus status))

veoAspectRatioForImageDimensions :: ImageDimensions -> Text
veoAspectRatioForImageDimensions ImageDimensions{imageWidth, imageHeight}
    | imageWidth < imageHeight =
        "9:16"
    | otherwise =
        "16:9"

parseGenVideoArgs :: [Text] -> ParseGenVideoArgsResult
parseGenVideoArgs = \case
    [] ->
        ParseGenVideoArgsError "A provider is required. Use `xai` or `veo`." GenVideoHelpGeneral
    "--help" : _ ->
        ParseGenVideoArgsHelp GenVideoHelpGeneral
    providerArg : rest ->
        case parseProvider providerArg of
            Nothing ->
                ParseGenVideoArgsError
                    ("Unknown provider: " <> providerArg <> ". Use `xai` or `veo`.")
                    GenVideoHelpGeneral
            Just GenVideoProviderXAI ->
                parseXAIArgs rest
            Just GenVideoProviderVeo ->
                parseVeoArgs rest

parseProvider :: Text -> Maybe GenVideoProvider
parseProvider = \case
    "xai" ->
        Just GenVideoProviderXAI
    "veo" ->
        Just GenVideoProviderVeo
    _ ->
        Nothing

parseXAIArgs :: [Text] -> ParseGenVideoArgsResult
parseXAIArgs =
    go defaultCommonParseState defaultXAIGenVideoOptions
  where
    go commonState xaiOptions = \case
        [] ->
            finalize commonState xaiOptions
        "--help" : _ ->
            ParseGenVideoArgsHelp GenVideoHelpXAI
        "--" : rest ->
            finalize
                (appendPromptParts commonState rest)
                xaiOptions
        "--output" : path : rest ->
            go commonState{parseOutputPath = Just (toString path)} xaiOptions rest
        "-o" : path : rest ->
            go commonState{parseOutputPath = Just (toString path)} xaiOptions rest
        "--model" : modelName : rest ->
            go commonState xaiOptions{xaiVideoModel = modelName} rest
        "--image" : imageSource : rest ->
            go commonState xaiOptions{xaiVideoImageSource = Just imageSource} rest
        "--edit" : videoSource : rest ->
            go commonState xaiOptions{xaiVideoEditSource = Just videoSource} rest
        "--extend" : videoSource : rest ->
            go commonState xaiOptions{xaiVideoExtendSource = Just videoSource} rest
        "--video" : videoSource : rest ->
            go commonState xaiOptions{xaiVideoEditSource = Just videoSource} rest
        "--duration" : rawDuration : rest ->
            case parsePositiveIntOption "--duration" rawDuration of
                Left err ->
                    ParseGenVideoArgsError err GenVideoHelpXAI
                Right durationSeconds ->
                    go commonState xaiOptions{xaiVideoDuration = Just durationSeconds} rest
        "--aspect-ratio" : aspectRatioValue : rest ->
            go commonState xaiOptions{xaiVideoAspectRatio = Just aspectRatioValue} rest
        "--resolution" : resolutionValue : rest ->
            go commonState xaiOptions{xaiVideoResolution = Just resolutionValue} rest
        "--poll-interval-ms" : rawPollInterval : rest ->
            case parsePositiveIntOption "--poll-interval-ms" rawPollInterval of
                Left err ->
                    ParseGenVideoArgsError err GenVideoHelpXAI
                Right pollIntervalMilliseconds ->
                    go commonState xaiOptions{xaiVideoPollIntervalMilliseconds = pollIntervalMilliseconds} rest
        "--max-poll-attempts" : rawMaxPollAttempts : rest ->
            case parsePositiveIntOption "--max-poll-attempts" rawMaxPollAttempts of
                Left err ->
                    ParseGenVideoArgsError err GenVideoHelpXAI
                Right maxPollAttempts ->
                    go commonState xaiOptions{xaiVideoMaxPollAttempts = maxPollAttempts} rest
        arg : rest
            | Just path <- T.stripPrefix "--output=" arg ->
                go commonState{parseOutputPath = Just (toString path)} xaiOptions rest
            | Just modelName <- T.stripPrefix "--model=" arg ->
                go commonState xaiOptions{xaiVideoModel = modelName} rest
            | Just imageSource <- T.stripPrefix "--image=" arg ->
                go commonState xaiOptions{xaiVideoImageSource = Just imageSource} rest
            | Just videoSource <- T.stripPrefix "--edit=" arg ->
                go commonState xaiOptions{xaiVideoEditSource = Just videoSource} rest
            | Just videoSource <- T.stripPrefix "--extend=" arg ->
                go commonState xaiOptions{xaiVideoExtendSource = Just videoSource} rest
            | Just videoSource <- T.stripPrefix "--video=" arg ->
                go commonState xaiOptions{xaiVideoEditSource = Just videoSource} rest
            | Just rawDuration <- T.stripPrefix "--duration=" arg ->
                case parsePositiveIntOption "--duration" rawDuration of
                    Left err ->
                        ParseGenVideoArgsError err GenVideoHelpXAI
                    Right durationSeconds ->
                        go commonState xaiOptions{xaiVideoDuration = Just durationSeconds} rest
            | Just aspectRatioValue <- T.stripPrefix "--aspect-ratio=" arg ->
                go commonState xaiOptions{xaiVideoAspectRatio = Just aspectRatioValue} rest
            | Just resolutionValue <- T.stripPrefix "--resolution=" arg ->
                go commonState xaiOptions{xaiVideoResolution = Just resolutionValue} rest
            | Just rawPollInterval <- T.stripPrefix "--poll-interval-ms=" arg ->
                case parsePositiveIntOption "--poll-interval-ms" rawPollInterval of
                    Left err ->
                        ParseGenVideoArgsError err GenVideoHelpXAI
                    Right pollIntervalMilliseconds ->
                        go commonState xaiOptions{xaiVideoPollIntervalMilliseconds = pollIntervalMilliseconds} rest
            | Just rawMaxPollAttempts <- T.stripPrefix "--max-poll-attempts=" arg ->
                case parsePositiveIntOption "--max-poll-attempts" rawMaxPollAttempts of
                    Left err ->
                        ParseGenVideoArgsError err GenVideoHelpXAI
                    Right maxPollAttempts ->
                        go commonState xaiOptions{xaiVideoMaxPollAttempts = maxPollAttempts} rest
            | "-" `T.isPrefixOf` arg ->
                ParseGenVideoArgsError ("Unknown xai option: " <> arg) GenVideoHelpXAI
            | otherwise ->
                go (appendPromptParts commonState [arg]) xaiOptions rest

    finalize CommonParseState{parseOutputPath, parsePromptParts} xaiOptions
        | null parsePromptParts =
            ParseGenVideoArgsError "A prompt is required." GenVideoHelpXAI
        | hasXAIVideoSourceConflict xaiOptions =
            ParseGenVideoArgsError "Use exactly one of --image, --edit/--video, or --extend." GenVideoHelpXAI
        | otherwise =
            ParseGenVideoArgsSuccess $
                GenVideoXAI
                    xaiOptions
                        { xaiVideoCommonOptions =
                            CommonGenVideoOptions
                                { commonVideoPromptText = T.unwords parsePromptParts
                                , commonVideoOutputPath = parseOutputPath
                                }
                        }

parseVeoArgs :: [Text] -> ParseGenVideoArgsResult
parseVeoArgs =
    go defaultCommonParseState defaultVeoGenVideoOptions
  where
    go commonState veoOptions = \case
        [] ->
            finalize commonState veoOptions
        "--help" : _ ->
            ParseGenVideoArgsHelp GenVideoHelpVeo
        "--" : rest ->
            finalize
                (appendPromptParts commonState rest)
                veoOptions
        "--output" : path : rest ->
            go commonState{parseOutputPath = Just (toString path)} veoOptions rest
        "-o" : path : rest ->
            go commonState{parseOutputPath = Just (toString path)} veoOptions rest
        "--model" : modelName : rest ->
            go commonState veoOptions{veoVideoModel = modelName} rest
        "--image" : imageSource : rest ->
            go commonState veoOptions{veoVideoImageSource = Just imageSource} rest
        "--last-frame" : imageSource : rest ->
            go commonState veoOptions{veoVideoLastFrameSource = Just imageSource} rest
        "--duration" : rawDuration : rest ->
            case parsePositiveIntOption "--duration" rawDuration of
                Left err ->
                    ParseGenVideoArgsError err GenVideoHelpVeo
                Right durationSeconds ->
                    go commonState veoOptions{veoVideoDuration = Just durationSeconds} rest
        "--aspect-ratio" : aspectRatioValue : rest ->
            go commonState veoOptions{veoVideoAspectRatio = Just aspectRatioValue} rest
        "--resolution" : resolutionValue : rest ->
            go commonState veoOptions{veoVideoResolution = Just resolutionValue} rest
        "--person-generation" : personGenerationValue : rest ->
            go commonState veoOptions{veoVideoPersonGeneration = Just personGenerationValue} rest
        "--seed" : rawSeed : rest ->
            case parseIntOption "--seed" rawSeed of
                Left err ->
                    ParseGenVideoArgsError err GenVideoHelpVeo
                Right seedValue ->
                    go commonState veoOptions{veoVideoSeed = Just seedValue} rest
        "--poll-interval-ms" : rawPollInterval : rest ->
            case parsePositiveIntOption "--poll-interval-ms" rawPollInterval of
                Left err ->
                    ParseGenVideoArgsError err GenVideoHelpVeo
                Right pollIntervalMilliseconds ->
                    go commonState veoOptions{veoVideoPollIntervalMilliseconds = pollIntervalMilliseconds} rest
        "--max-poll-attempts" : rawMaxPollAttempts : rest ->
            case parsePositiveIntOption "--max-poll-attempts" rawMaxPollAttempts of
                Left err ->
                    ParseGenVideoArgsError err GenVideoHelpVeo
                Right maxPollAttempts ->
                    go commonState veoOptions{veoVideoMaxPollAttempts = maxPollAttempts} rest
        arg : rest
            | Just path <- T.stripPrefix "--output=" arg ->
                go commonState{parseOutputPath = Just (toString path)} veoOptions rest
            | Just modelName <- T.stripPrefix "--model=" arg ->
                go commonState veoOptions{veoVideoModel = modelName} rest
            | Just imageSource <- T.stripPrefix "--image=" arg ->
                go commonState veoOptions{veoVideoImageSource = Just imageSource} rest
            | Just imageSource <- T.stripPrefix "--last-frame=" arg ->
                go commonState veoOptions{veoVideoLastFrameSource = Just imageSource} rest
            | Just rawDuration <- T.stripPrefix "--duration=" arg ->
                case parsePositiveIntOption "--duration" rawDuration of
                    Left err ->
                        ParseGenVideoArgsError err GenVideoHelpVeo
                    Right durationSeconds ->
                        go commonState veoOptions{veoVideoDuration = Just durationSeconds} rest
            | Just aspectRatioValue <- T.stripPrefix "--aspect-ratio=" arg ->
                go commonState veoOptions{veoVideoAspectRatio = Just aspectRatioValue} rest
            | Just resolutionValue <- T.stripPrefix "--resolution=" arg ->
                go commonState veoOptions{veoVideoResolution = Just resolutionValue} rest
            | Just personGenerationValue <- T.stripPrefix "--person-generation=" arg ->
                go commonState veoOptions{veoVideoPersonGeneration = Just personGenerationValue} rest
            | Just rawSeed <- T.stripPrefix "--seed=" arg ->
                case parseIntOption "--seed" rawSeed of
                    Left err ->
                        ParseGenVideoArgsError err GenVideoHelpVeo
                    Right seedValue ->
                        go commonState veoOptions{veoVideoSeed = Just seedValue} rest
            | Just rawPollInterval <- T.stripPrefix "--poll-interval-ms=" arg ->
                case parsePositiveIntOption "--poll-interval-ms" rawPollInterval of
                    Left err ->
                        ParseGenVideoArgsError err GenVideoHelpVeo
                    Right pollIntervalMilliseconds ->
                        go commonState veoOptions{veoVideoPollIntervalMilliseconds = pollIntervalMilliseconds} rest
            | Just rawMaxPollAttempts <- T.stripPrefix "--max-poll-attempts=" arg ->
                case parsePositiveIntOption "--max-poll-attempts" rawMaxPollAttempts of
                    Left err ->
                        ParseGenVideoArgsError err GenVideoHelpVeo
                    Right maxPollAttempts ->
                        go commonState veoOptions{veoVideoMaxPollAttempts = maxPollAttempts} rest
            | "-" `T.isPrefixOf` arg ->
                ParseGenVideoArgsError ("Unknown veo option: " <> arg) GenVideoHelpVeo
            | otherwise ->
                go (appendPromptParts commonState [arg]) veoOptions rest

    finalize CommonParseState{parseOutputPath, parsePromptParts} veoOptions
        | null parsePromptParts =
            ParseGenVideoArgsError "A prompt is required." GenVideoHelpVeo
        | hasVeoLastFrameWithoutImage veoOptions =
            ParseGenVideoArgsError
                "veo --last-frame is only for first/last frame interpolation and requires --image. To animate one still image, use --image SOURCE."
                GenVideoHelpVeo
        | otherwise =
            ParseGenVideoArgsSuccess $
                GenVideoVeo
                    veoOptions
                        { veoVideoCommonOptions =
                            CommonGenVideoOptions
                                { commonVideoPromptText = T.unwords parsePromptParts
                                , commonVideoOutputPath = parseOutputPath
                                }
                        }

parsePositiveIntOption :: Text -> Text -> Either Text Int
parsePositiveIntOption optionName rawValue =
    case readMaybe @Int (toString rawValue) of
        Nothing ->
            Left ("Invalid value for " <> optionName <> ": " <> rawValue)
        Just value
            | value < 1 ->
                Left (optionName <> " must be at least 1")
            | otherwise ->
                Right value

parseIntOption :: Text -> Text -> Either Text Int
parseIntOption optionName rawValue =
    case readMaybe @Int (toString rawValue) of
        Nothing ->
            Left ("Invalid value for " <> optionName <> ": " <> rawValue)
        Just value ->
            Right value

appendPromptParts :: CommonParseState -> [Text] -> CommonParseState
appendPromptParts commonState@CommonParseState{parsePromptParts} extraPromptParts =
    commonState{parsePromptParts = parsePromptParts <> extraPromptParts}

hasXAIVideoSourceConflict :: XAIGenVideoOptions -> Bool
hasXAIVideoSourceConflict XAIGenVideoOptions{xaiVideoImageSource, xaiVideoEditSource, xaiVideoExtendSource} =
    length (catMaybes [xaiVideoImageSource, xaiVideoEditSource, xaiVideoExtendSource]) > 1

hasVeoLastFrameWithoutImage :: VeoGenVideoOptions -> Bool
hasVeoLastFrameWithoutImage VeoGenVideoOptions{veoVideoImageSource, veoVideoLastFrameSource} =
    isNothing veoVideoImageSource && isJust veoVideoLastFrameSource

renderGenVideoHelp :: String -> GenVideoHelpTopic -> Text
renderGenVideoHelp progName = \case
    GenVideoHelpGeneral ->
        T.unlines $
            [ "Usage:"
            , "  " <> toText progName <> " xai [OPTIONS] PROMPT"
            , "  " <> toText progName <> " veo [OPTIONS] PROMPT"
            , ""
            , "Providers:"
            , "  xai     Generate videos with xAI Grok Imagine. Default model: grok-imagine-video"
            , "  veo     Generate videos with Google Veo. Default model: veo-3.1-generate-preview"
            , ""
            , "Common options:"
            ]
                <> commonOptionLines
                <> [ ""
                   , "xai options:"
                   ]
                <> xaiOptionLines
                <> [ ""
                   , "veo options:"
                   ]
                <> veoOptionLines
                <> [ ""
                   , "Notes:"
                   , "  SOURCE can be a URL, a data URL, or a local file path."
                   , "  Use `" <> toText progName <> " xai --help` or `" <> toText progName <> " veo --help` for focused help."
                   , "  With no source option, the CLI sends a text-to-video request."
                   , "  Use `--image SOURCE` for image-to-video generation."
                   , "  Use `veo --image START --last-frame END` for first/last frame interpolation."
                   , "  Use `--edit SOURCE` to update an existing video."
                   , "  Use `--extend SOURCE` to append a continuation to the end of a video."
                   , "  `--video SOURCE` is kept as a compatible alias for `--edit`."
                   , "  `--duration`, `--aspect-ratio`, and `--resolution` apply to text-to-video and image-to-video; xAI also accepts them for extend."
                   , "  `--extend` requires local `ffmpeg` and `ffprobe`."
                   , "  `--extend` currently supports local files and URLs, not data URLs."
                   , "  XAI_API_KEY is required."
                   , "  GEMINI_API_KEY is required for `veo`."
                   , ""
                   , "Examples:"
                   , "  " <> toText progName <> " xai \"A paper crane unfolds into a bird and flies away\""
                   , "  " <> toText progName <> " xai \"She walk away\" --image girl.jpg"
                   , "  " <> toText progName <> " xai --image=girl.jpg --duration=8 \"She walk away\""
                   , "  " <> toText progName <> " xai --edit=clip.mp4 \"make the lighting moodier\""
                   , "  " <> toText progName <> " xai --extend=clip.mp4 \"continue the scene for 5 more seconds\""
                   , "  " <> toText progName <> " veo --image=still.png \"animate this still frame\""
                   , "  " <> toText progName <> " veo --image=start.png --last-frame=end.png \"transition between these frames\""
                   ]
    GenVideoHelpXAI ->
        T.unlines $
            [ "Usage:"
            , "  " <> toText progName <> " xai [OPTIONS] PROMPT"
            , ""
            , "Default model: grok-imagine-video"
            , ""
            , "Common options:"
            ]
                <> commonOptionLines
                <> [ ""
                   , "xai options:"
                   ]
                <> xaiOptionLines
                <> [ ""
                   , "Notes:"
                   , "  SOURCE can be a URL, a data URL, or a local file path."
                   , "  With no source option, the CLI sends a text-to-video request."
                   , "  Use `--image SOURCE` for image-to-video generation."
                   , "  Use `--edit SOURCE` to update an existing video."
                   , "  Use `--extend SOURCE` to append a continuation to the end of a video."
                   , "  `--video SOURCE` is kept as a compatible alias for `--edit`."
                   , "  `--duration`, `--aspect-ratio`, and `--resolution` apply to text-to-video, image-to-video, and extend."
                   , "  `--extend` requires local `ffmpeg` and `ffprobe`."
                   , "  `--extend` currently supports local files and URLs, not data URLs."
                   , "  XAI_API_KEY is required."
                   , ""
                   , "Examples:"
                   , "  " <> toText progName <> " xai \"A paper crane unfolds into a bird and flies away\""
                   , "  " <> toText progName <> " xai \"She walk away\" --image girl.jpg"
                   , "  " <> toText progName <> " xai --image=girl.jpg --duration=8 \"She walk away\""
                   , "  " <> toText progName <> " xai --edit=clip.mp4 \"make the lighting moodier\""
                   , "  " <> toText progName <> " xai --extend=clip.mp4 \"continue the scene for 5 more seconds\""
                   ]
    GenVideoHelpVeo ->
        T.unlines $
            [ "Usage:"
            , "  " <> toText progName <> " veo [OPTIONS] PROMPT"
            , ""
            , "Default model: veo-3.1-generate-preview"
            , ""
            , "Common options:"
            ]
                <> commonOptionLines
                <> [ ""
                   , "veo options:"
                   ]
                <> veoOptionLines
                <> [ ""
                   , "Notes:"
                   , "  SOURCE can be a URL, a data URL, or a local image path."
                   , "  With no source option, the CLI sends a text-to-video request."
                   , "  Use `--image SOURCE` to animate one still image as the first frame."
                   , "  Use `--image START --last-frame END` for first/last frame interpolation."
                   , "  If `--aspect-ratio` is omitted for a Veo image request, source image dimensions are used to pick 16:9 or 9:16."
                   , "  GEMINI_API_KEY is required."
                   , ""
                   , "Examples:"
                   , "  " <> toText progName <> " veo \"A cinematic shot of a lion in the savannah\""
                   , "  " <> toText progName <> " veo --image=start.png \"animate this still frame\""
                   , "  " <> toText progName <> " veo --image=start.png --last-frame=end.png --duration=8 \"move from the first frame to the final frame\""
                   ]
  where
    commonOptionLines =
        [ "  -o, --output PATH            Output file path or filename prefix."
        , "  --help                       Show help."
        ]

    xaiOptionLines =
        [ "  --model MODEL                Override the xAI video model."
        , "  --image SOURCE               Input still image URL or local file."
        , "  --edit SOURCE                Update an existing video URL or local file."
        , "  --extend SOURCE              Append a continuation to the end of a video."
        , "  --video SOURCE               Alias for --edit."
        , "  --duration SECONDS           Duration hint for image-to-video or extend."
        , "  --aspect-ratio RATIO         Aspect ratio hint, for example 16:9."
        , "  --resolution RESOLUTION      Resolution hint, for example 720p."
        , "  --poll-interval-ms N         Poll interval in milliseconds. Default: 5000"
        , "  --max-poll-attempts N        Maximum poll attempts. Default: 120"
        ]

    veoOptionLines =
        [ "  --model MODEL                Override the Veo model."
        , "  --image SOURCE               First frame image URL, data URL, or local file."
        , "  --last-frame SOURCE          Final frame image URL, data URL, or local file."
        , "  --duration SECONDS           Duration hint, for example 4, 6, or 8; first/last frame interpolation requires 8."
        , "  --aspect-ratio RATIO         Aspect ratio hint, for example 16:9 or 9:16."
        , "  --resolution RESOLUTION      Resolution hint, for example 720p, 1080p, or 4k."
        , "  --person-generation VALUE    Person generation setting, for example allow_adult."
        , "  --seed N                     Provider seed hint."
        , "  --poll-interval-ms N         Poll interval in milliseconds. Default: 5000"
        , "  --max-poll-attempts N        Maximum poll attempts. Default: 120"
        ]

saveGeneratedVideo :: Maybe FilePath -> Text -> GeneratedVideo -> IO (Either Text [FilePath])
saveGeneratedVideo maybeOutput promptText GeneratedVideo{url = maybeUrl} =
    case maybeUrl of
        Nothing ->
            pure (Left "Video payload has no url")
        Just videoUrl -> do
            putTextLn ("Provider video URL: " <> videoUrl)
            downloadResult <- downloadBinary videoUrl
            case downloadResult of
                Left err ->
                    pure (Left err)
                Right videoBytes -> do
                    outputPaths <-
                        buildOutputPaths
                            "video"
                            maybeOutput
                            promptText
                            [fromMaybe ".mp4" (urlExtension videoUrl)]
                    traverse_
                        (\path -> writeBinaryFile path videoBytes)
                        outputPaths
                    pure (Right outputPaths)

downloadGeneratedVideo :: GeneratedVideo -> IO (Either Text BS.ByteString)
downloadGeneratedVideo GeneratedVideo{url = maybeUrl} =
    case maybeUrl of
        Nothing ->
            pure (Left "Video payload has no url")
        Just videoUrl ->
            downloadBinary videoUrl

announceGeneratedVideoUrl :: Text -> GeneratedVideo -> IO ()
announceGeneratedVideoUrl label GeneratedVideo{url = maybeUrl} =
    traverse_ (\videoUrl -> putTextLn (label <> ": " <> videoUrl)) maybeUrl

requireExecutable :: FilePath -> IO (Either Text FilePath)
requireExecutable executableName =
    findExecutable executableName <&> \case
        Nothing ->
            Left
                ( toText executableName
                    <> " is required for true video extension. xAI's public API supports video edits, but not append-style continuation, so `--extend` uses local "
                    <> toText executableName
                    <> " tooling to stitch the original clip and the generated continuation together."
                )
        Just executablePath ->
            Right executablePath

withPreparedVideoSource :: Text -> (FilePath -> IO (Either Text a)) -> IO (Either Text a)
withPreparedVideoSource source useSource
    | "http://" `T.isPrefixOf` source || "https://" `T.isPrefixOf` source = do
        downloadResult <- downloadBinary source
        case downloadResult of
            Left err ->
                pure (Left err)
            Right videoBytes ->
                withTempBinaryFile "gen-video-source" videoBytes useSource
    | "data:" `T.isPrefixOf` source =
        pure (Left "True video extension does not support data URLs yet. Write the source video to a file first.")
    | otherwise = do
        let sourcePath = toString source
        exists <- doesFileExist sourcePath
        if exists
            then useSource sourcePath
            else pure (Left ("Video source is not a URL or existing file: " <> source))

withTempBinaryFile :: String -> BS.ByteString -> (FilePath -> IO (Either Text a)) -> IO (Either Text a)
withTempBinaryFile template bytes useFile =
    withTempFilePath template $ \path -> do
        writeBinaryFile path bytes
        useFile path

withTempFilePath :: String -> (FilePath -> IO (Either Text a)) -> IO (Either Text a)
withTempFilePath template useFile = do
    tempDir <- getTemporaryDirectory
    (path, handle) <- IO.openBinaryTempFile tempDir template
    IO.hClose handle
    result <- useFile path
    ignoreMissingFile path
    pure result

appendContinuation
    :: FilePath
    -> FilePath
    -> FilePath
    -> FilePath
    -> FilePath
    -> IO (Either Text ())
appendContinuation ffmpegPath ffprobePath sourceVideoPath continuationPath outputPath = do
    originalVideoSize <- probeVideoSize ffprobePath sourceVideoPath
    originalHasAudio <- probeHasAudio ffprobePath sourceVideoPath
    continuationHasAudio <- probeHasAudio ffprobePath continuationPath
    case (originalVideoSize, originalHasAudio, continuationHasAudio) of
        (Left err, _, _) ->
            pure (Left err)
        (_, Left err, _) ->
            pure (Left err)
        (_, _, Left err) ->
            pure (Left err)
        (Right (videoWidth, videoHeight), Right hasOriginalAudio, Right hasContinuationAudio) ->
            runExternalCommand ffmpegPath (concatCommandArgs videoWidth videoHeight hasOriginalAudio hasContinuationAudio)
  where
    concatCommandArgs videoWidth videoHeight hasOriginalAudio hasContinuationAudio
        | hasOriginalAudio && hasContinuationAudio =
            [ "-y"
            , "-i"
            , sourceVideoPath
            , "-i"
            , continuationPath
            , "-filter_complex"
            , concatFilter videoWidth videoHeight True
            , "-map"
            , "[outv]"
            , "-map"
            , "[outa]"
            , "-c:v"
            , "libx264"
            , "-pix_fmt"
            , "yuv420p"
            , "-c:a"
            , "aac"
            , "-movflags"
            , "+faststart"
            , outputPath
            ]
        | otherwise =
            [ "-y"
            , "-i"
            , sourceVideoPath
            , "-i"
            , continuationPath
            , "-filter_complex"
            , concatFilter videoWidth videoHeight False
            , "-map"
            , "[outv]"
            , "-c:v"
            , "libx264"
            , "-pix_fmt"
            , "yuv420p"
            , "-movflags"
            , "+faststart"
            , outputPath
            ]

concatFilter :: Int -> Int -> Bool -> String
concatFilter videoWidth videoHeight includeAudio =
    if includeAudio
        then
            videoPrelude
                <> "[0:a]aformat=sample_rates=48000:channel_layouts=stereo[a0];"
                <> "[1:a]aformat=sample_rates=48000:channel_layouts=stereo[a1];"
                <> "[v0][a0][v1][a1]concat=n=2:v=1:a=1[outv][outa]"
        else
            videoPrelude
                <> "[v0][v1]concat=n=2:v=1:a=0[outv]"
  where
    scaleAndPad :: Int -> String -> String
    scaleAndPad inputIndex outputLabel =
        "["
            <> show inputIndex
            <> ":v]fps=30,scale="
            <> show videoWidth
            <> ":"
            <> show videoHeight
            <> ":force_original_aspect_ratio=decrease,pad="
            <> show videoWidth
            <> ":"
            <> show videoHeight
            <> ":(ow-iw)/2:(oh-ih)/2,setsar=1["
            <> outputLabel
            <> "];"

    videoPrelude :: String
    videoPrelude =
        scaleAndPad (0 :: Int) "v0"
            <> scaleAndPad (1 :: Int) "v1"

probeVideoSize :: FilePath -> FilePath -> IO (Either Text (Int, Int))
probeVideoSize ffprobePath videoPath = do
    probeResult <-
        runExternalCommandWithOutput
            ffprobePath
            [ "-v"
            , "error"
            , "-select_streams"
            , "v:0"
            , "-show_entries"
            , "stream=width,height"
            , "-of"
            , "csv=s=x:p=0"
            , videoPath
            ]
    pure $
        probeResult >>= \rawDimensions ->
            case break (== 'x') (toString (T.strip rawDimensions)) of
                (rawWidth, 'x' : rawHeight) ->
                    case (readMaybe @Int rawWidth, readMaybe @Int rawHeight) of
                        (Just videoWidth, Just videoHeight) ->
                            Right (videoWidth, videoHeight)
                        _ ->
                            Left ("Could not parse video dimensions from ffprobe output: " <> rawDimensions)
                _ ->
                    Left ("Could not parse video dimensions from ffprobe output: " <> rawDimensions)

probeHasAudio :: FilePath -> FilePath -> IO (Either Text Bool)
probeHasAudio ffprobePath videoPath =
    runExternalCommandWithOutput
        ffprobePath
        [ "-v"
        , "error"
        , "-select_streams"
        , "a:0"
        , "-show_entries"
        , "stream=index"
        , "-of"
        , "csv=p=0"
        , videoPath
        ]
        <&> fmap (not . T.null . T.strip)

extractLastFrame :: FilePath -> FilePath -> FilePath -> IO (Either Text ())
extractLastFrame ffmpegPath sourceVideoPath framePath =
    runExternalCommand
        ffmpegPath
        [ "-y"
        , "-sseof"
        , "-0.1"
        , "-i"
        , sourceVideoPath
        , "-frames:v"
        , "1"
        , "-f"
        , "image2"
        , "-vcodec"
        , "png"
        , framePath
        ]

runExternalCommand :: FilePath -> [String] -> IO (Either Text ())
runExternalCommand executablePath args =
    runExternalCommandWithOutput executablePath args <&> void

runExternalCommandWithOutput :: FilePath -> [String] -> IO (Either Text Text)
runExternalCommandWithOutput executablePath args = do
    (exitCode, stdoutText, stderrText) <- readProcessWithExitCode executablePath args ""
    pure $
        case exitCode of
            ExitSuccess ->
                Right (T.strip (toText stdoutText))
            ExitFailure exitCodeValue ->
                Left
                    ( toText executablePath
                        <> " failed with exit code "
                        <> show exitCodeValue
                        <> renderProcessErrorDetails stdoutText stderrText
                    )

renderProcessErrorDetails :: String -> String -> Text
renderProcessErrorDetails stdoutText stderrText =
    case T.strip preferredDetail of
        "" ->
            ""
        detail ->
            ": " <> detail
  where
    preferredDetail
        | null stderrText =
            toText stdoutText
        | otherwise =
            toText stderrText

ignoreMissingFile :: FilePath -> IO ()
ignoreMissingFile path =
    do
        exists <- doesFileExist path
        when exists (removeFile path)

renderVideoStatus :: XAIVideoStatus -> Text
renderVideoStatus = \case
    XAIVideoPending ->
        "pending"
    XAIVideoDone ->
        "done"
    XAIVideoExpired ->
        "expired"
    XAIVideoFailed ->
        "failed"
    XAIVideoUnknown other ->
        other
