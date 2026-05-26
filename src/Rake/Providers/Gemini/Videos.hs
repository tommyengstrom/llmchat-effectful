module Rake.Providers.Gemini.Videos
    ( GeminiVideoSettings (..)
    , defaultGeminiVideoSettings
    , GeminiInlineImage (..)
    , GeminiVideoRequest (..)
    , defaultGeminiVideoRequest
    , GeminiVideoOperationName (..)
    , GeminiVideoError (..)
    , GeminiGeneratedVideo (..)
    , GeminiGenerateVideoResponse (..)
    , GeminiVideoOperation (..)
    , startGeminiVideo
    , getGeminiVideo
    , generateGeminiVideo
    , downloadGeminiVideo
    ) where

import Control.Concurrent (threadDelay)
import Data.Aeson
import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T
import Data.Text.Encoding qualified as TextEncoding
import Effectful
import Effectful.Error.Static
import Network.HTTP.Client qualified as HttpClient
import Network.HTTP.Client (managerResponseTimeout, responseTimeoutMicro)
import Network.HTTP.Client.TLS (newTlsManagerWith, tlsManagerSettings)
import Rake.Effect
import Rake.Providers.Gemini.Images (GeminiInlineImage (..))
import Rake.Providers.Internal (defaultWarningLogger)
import Relude
import Servant.API (Capture, CaptureAll, Get, Header, JSON, Post, ReqBody)
import Servant.API qualified as Servant
import Servant.Client

data GeminiVideoSettings es = GeminiVideoSettings
    { apiKey :: Text
    , baseUrl :: Text
    , pollIntervalMilliseconds :: Int
    , maxPollAttempts :: Int
    , requestLogger :: NativeMsgFormat -> Eff es ()
    }

defaultGeminiVideoSettings :: Text -> GeminiVideoSettings es
defaultGeminiVideoSettings apiKey =
    GeminiVideoSettings
        { apiKey
        , baseUrl = "https://generativelanguage.googleapis.com"
        , pollIntervalMilliseconds = 5000
        , maxPollAttempts = 120
        , requestLogger = defaultWarningLogger "gemini.videos"
        }

data GeminiVideoRequest = GeminiVideoRequest
    { model :: Text
    , prompt :: Text
    , image :: Maybe GeminiInlineImage
    , lastFrame :: Maybe GeminiInlineImage
    , durationSeconds :: Maybe Int
    , aspectRatio :: Maybe Text
    , resolution :: Maybe Text
    , personGeneration :: Maybe Text
    , seed :: Maybe Int
    }
    deriving stock (Show, Eq, Generic)

defaultGeminiVideoRequest :: Text -> GeminiVideoRequest
defaultGeminiVideoRequest prompt =
    GeminiVideoRequest
        { model = "veo-3.1-generate-preview"
        , prompt
        , image = Nothing
        , lastFrame = Nothing
        , durationSeconds = Nothing
        , aspectRatio = Nothing
        , resolution = Nothing
        , personGeneration = Nothing
        , seed = Nothing
        }

instance ToJSON GeminiVideoRequest where
    toJSON GeminiVideoRequest{prompt, image, lastFrame, durationSeconds, aspectRatio, resolution, personGeneration, seed} =
        object $
            [ "instances"
                .= ( [ object $
                        [ "prompt" .= prompt
                        ]
                            <> catMaybes
                                [ ("image" .=) <$> (veoImageValue <$> image)
                                , ("lastFrame" .=) <$> (veoImageValue <$> lastFrame)
                                ]
                     ]
                        :: [Value]
                   )
            ]
                <> parametersField
      where
        parametersField =
            case catMaybes
                [ ("durationSeconds" .=) <$> durationSeconds
                , ("aspectRatio" .=) <$> aspectRatio
                , ("resolution" .=) <$> resolution
                , ("personGeneration" .=) <$> personGeneration
                , ("seed" .=) <$> seed
                ] of
                [] ->
                    []
                fields ->
                    ["parameters" .= object fields]

newtype GeminiVideoOperationName = GeminiVideoOperationName Text
    deriving stock (Show, Eq, Ord, Generic)
    deriving newtype (FromJSON, ToJSON)

data GeminiVideoError = GeminiVideoError
    { code :: Maybe Int
    , message :: Maybe Text
    , status :: Maybe Text
    }
    deriving stock (Show, Eq, Generic)

instance FromJSON GeminiVideoError where
    parseJSON = withObject "GeminiVideoError" $ \obj ->
        GeminiVideoError
            <$> obj
            .:? "code"
            <*> obj
            .:? "message"
            <*> obj
            .:? "status"

instance ToJSON GeminiVideoError where
    toJSON GeminiVideoError{code, message, status} =
        object $
            catMaybes
                [ ("code" .=) <$> code
                , ("message" .=) <$> message
                , ("status" .=) <$> status
                ]

data GeminiGeneratedVideo = GeminiGeneratedVideo
    { uri :: Maybe Text
    , mimeType :: Maybe Text
    , videoBytes :: Maybe Text
    }
    deriving stock (Show, Eq, Generic)

instance FromJSON GeminiGeneratedVideo where
    parseJSON value =
        parseSample value <|> parseVideo value
      where
        parseSample =
            withObject "GeminiGeneratedVideoSample" $ \obj ->
                obj .: "video" >>= parseVideo

        parseVideo =
            withObject "GeminiGeneratedVideo" $ \obj ->
                let firstPresent =
                        listToMaybe . catMaybes
                 in do
                        mimeTypeCamel <- obj .:? "mimeType"
                        mimeTypeSnake <- obj .:? "mime_type"
                        videoBytesCamel <- obj .:? "videoBytes"
                        videoBytesSnake <- obj .:? "video_bytes"
                        GeminiGeneratedVideo
                            <$> obj
                            .:? "uri"
                            <*> pure (firstPresent [mimeTypeCamel, mimeTypeSnake])
                            <*> pure (firstPresent [videoBytesCamel, videoBytesSnake])

instance ToJSON GeminiGeneratedVideo where
    toJSON GeminiGeneratedVideo{uri, mimeType, videoBytes} =
        object $
            catMaybes
                [ ("uri" .=) <$> uri
                , ("mimeType" .=) <$> mimeType
                , ("videoBytes" .=) <$> videoBytes
                ]

newtype GeminiGenerateVideoResponse = GeminiGenerateVideoResponse
    { generatedVideos :: [GeminiGeneratedVideo]
    }
    deriving stock (Show, Eq, Generic)

instance FromJSON GeminiGenerateVideoResponse where
    parseJSON value =
        parseNestedGenerateVideoResponse value <|> parseGenerateVideoResponse value
      where
        parseNestedGenerateVideoResponse =
            withObject "GeminiOperationResponse" $ \obj ->
                obj .: "generateVideoResponse" >>= parseGenerateVideoResponse

        parseGenerateVideoResponse =
            withObject "GeminiGenerateVideoResponse" $ \obj ->
                let firstPresent =
                        listToMaybe . catMaybes
                 in do
                        generatedVideosCamel <- obj .:? "generatedVideos"
                        generatedVideosSnake <- obj .:? "generated_videos"
                        generatedSamplesCamel <- obj .:? "generatedSamples"
                        generatedSamplesSnake <- obj .:? "generated_samples"
                        pure
                            GeminiGenerateVideoResponse
                                { generatedVideos =
                                    fromMaybe
                                        []
                                        ( firstPresent
                                            [ generatedVideosCamel
                                            , generatedVideosSnake
                                            , generatedSamplesCamel
                                            , generatedSamplesSnake
                                            ]
                                        )
                                }

instance ToJSON GeminiGenerateVideoResponse where
    toJSON GeminiGenerateVideoResponse{generatedVideos} =
        object ["generatedVideos" .= generatedVideos]

data GeminiVideoOperation = GeminiVideoOperation
    { name :: GeminiVideoOperationName
    , done :: Bool
    , response :: Maybe GeminiGenerateVideoResponse
    , error :: Maybe GeminiVideoError
    }
    deriving stock (Show, Eq, Generic)

instance FromJSON GeminiVideoOperation where
    parseJSON = withObject "GeminiVideoOperation" $ \obj ->
        GeminiVideoOperation
            <$> obj
            .: "name"
            <*> obj
            .:? "done"
            .!= False
            <*> obj
            .:? "response"
            <*> obj
            .:? "error"

instance ToJSON GeminiVideoOperation where
    toJSON GeminiVideoOperation{name, done, response, error = maybeError} =
        object $
            [ "name" .= name
            , "done" .= done
            ]
                <> catMaybes
                    [ ("response" .=) <$> response
                    , ("error" .=) <$> maybeError
                    ]

type GeminiVideosAPI =
    "v1beta"
        Servant.:> ( "models"
                        Servant.:> Capture "model_action" Text
                        Servant.:> Header "x-goog-api-key" Text
                        Servant.:> ReqBody '[JSON] Value
                        Servant.:> Post '[JSON] Value
                  Servant.:<|> CaptureAll "operation_path" Text
                        Servant.:> Header "x-goog-api-key" Text
                        Servant.:> Get '[JSON] Value
                  )

geminiVideosApi :: Proxy GeminiVideosAPI
geminiVideosApi = Proxy

startGeminiVideo
    :: forall es
     . ( IOE :> es
       , Error RakeError :> es
       )
    => GeminiVideoSettings es
    -> GeminiVideoRequest
    -> Eff es GeminiVideoOperation
startGeminiVideo settings request@GeminiVideoRequest{model} = do
    either throwError pure (validateGeminiVideoRequest request)
    let requestBody = toJSON request
    responseValue <-
        runVideoStartRequest
            settings
            requestBody
            (model <> ":predictLongRunning")
    decodeResponse "Gemini video start response" responseValue

getGeminiVideo
    :: forall es
     . ( IOE :> es
       , Error RakeError :> es
       )
    => GeminiVideoSettings es
    -> GeminiVideoOperationName
    -> Eff es GeminiVideoOperation
getGeminiVideo settings operationName@(GeminiVideoOperationName operationNameText) = do
    let requestEnvelope =
            object
                [ "operation" .= ("get_gemini_video" :: Text)
                , "name" .= operationNameText
                ]
    responseValue <-
        runVideoStatusRequest
            settings
            requestEnvelope
            (operationNamePath operationName)
    decodeResponse "Gemini video operation response" responseValue

generateGeminiVideo
    :: forall es
     . ( IOE :> es
       , Error RakeError :> es
       )
    => GeminiVideoSettings es
    -> GeminiVideoRequest
    -> Eff es GeminiVideoOperation
generateGeminiVideo settings@GeminiVideoSettings{pollIntervalMilliseconds, maxPollAttempts} request = do
    operation@GeminiVideoOperation{name, done} <- startGeminiVideo settings request
    if done
        then pure operation
        else pollUntilReady 0 name
  where
    pollUntilReady attempts operationName = do
        when (attempts >= maxPollAttempts) $
            throwError (LlmExpectationError "Gemini video generation exceeded the configured poll limit")
        operation@GeminiVideoOperation{done} <- getGeminiVideo settings operationName
        if done
            then pure operation
            else do
                liftIO (threadDelay (pollIntervalMilliseconds * 1000))
                pollUntilReady (attempts + 1) operationName

downloadGeminiVideo
    :: forall es
     . ( IOE :> es
       , Error RakeError :> es
       )
    => GeminiVideoSettings es
    -> GeminiGeneratedVideo
    -> Eff es ByteString
downloadGeminiVideo GeminiVideoSettings{apiKey, baseUrl, requestLogger} GeminiGeneratedVideo{uri = maybeUri} =
    case maybeUri of
        Nothing ->
            throwError (LlmExpectationError "Gemini video payload has no uri")
        Just videoUri -> do
            manager <- liftIO $ newTlsManagerWith tlsManagerSettings{managerResponseTimeout = responseTimeoutMicro 180_000_000}
            request <- liftIO $ HttpClient.parseRequest (toString (absoluteVideoUri videoUri))
            let requestWithApiKey =
                    request
                        { HttpClient.requestHeaders =
                            ("x-goog-api-key", TextEncoding.encodeUtf8 apiKey)
                                : HttpClient.requestHeaders request
                        }
                requestEnvelope =
                    object
                        [ "operation" .= ("download_gemini_video" :: Text)
                        , "uri" .= videoUri
                        ]
            requestLogger (NativeMsgOut requestEnvelope)
            response <- liftIO (HttpClient.httpLbs requestWithApiKey manager)
            pure (LBS.toStrict (HttpClient.responseBody response))
  where
    absoluteVideoUri videoUri
        | "http://" `T.isPrefixOf` videoUri || "https://" `T.isPrefixOf` videoUri =
            videoUri
        | otherwise =
            T.dropWhileEnd (== '/') baseUrl
                <> "/"
                <> T.dropWhile (== '/') videoUri

validateGeminiVideoRequest :: GeminiVideoRequest -> Either RakeError ()
validateGeminiVideoRequest GeminiVideoRequest{image, lastFrame, durationSeconds}
    | isNothing image && isJust lastFrame =
        Left (LlmExpectationError "Gemini Veo lastFrame requires image")
    | isJust lastFrame && durationSeconds == Just 8 =
        Right ()
    | isJust lastFrame && isJust durationSeconds =
        Left (LlmExpectationError "Gemini Veo first/last frame interpolation requires durationSeconds=8")
    | otherwise =
        Right ()

runVideoStartRequest
    :: forall es
     . ( IOE :> es
       , Error RakeError :> es
       )
    => GeminiVideoSettings es
    -> Value
    -> Text
    -> Eff es Value
runVideoStartRequest settings@GeminiVideoSettings{apiKey} requestBody modelAction = do
    let postVideo Servant.:<|> _ = client geminiVideosApi
        clientCall =
            postVideo modelAction (Just apiKey) requestBody
    runClientCall settings requestBody clientCall

runVideoStatusRequest
    :: forall es
     . ( IOE :> es
       , Error RakeError :> es
       )
    => GeminiVideoSettings es
    -> Value
    -> [Text]
    -> Eff es Value
runVideoStatusRequest settings@GeminiVideoSettings{apiKey} requestEnvelope operationPath = do
    let _ Servant.:<|> getOperation = client geminiVideosApi
        clientCall =
            getOperation operationPath (Just apiKey)
    runClientCall settings requestEnvelope clientCall

runClientCall
    :: forall es
     . ( IOE :> es
       , Error RakeError :> es
       )
    => GeminiVideoSettings es
    -> Value
    -> ClientM Value
    -> Eff es Value
runClientCall GeminiVideoSettings{apiKey = _, baseUrl, requestLogger, pollIntervalMilliseconds = _, maxPollAttempts = _} requestEnvelope clientCall = do
    manager <- liftIO $ newTlsManagerWith tlsManagerSettings{managerResponseTimeout = responseTimeoutMicro 180_000_000}
    parsedBaseUrl <- either (throwError . invalidBaseUrl) pure $ parseBaseUrl (toString baseUrl)
    let clientEnv = mkClientEnv manager parsedBaseUrl

    requestLogger (NativeMsgOut requestEnvelope)
    liftIO (runClientM clientCall clientEnv) >>= \case
        Left err -> do
            requestLogger (NativeRequestFailure err)
            throwError (LlmClientError err)
        Right responseValue -> do
            requestLogger (NativeMsgIn responseValue)
            pure responseValue
  where
    invalidBaseUrl err =
        LlmExpectationError ("Invalid base URL: " <> show err)

decodeResponse
    :: forall a es
     . ( Error RakeError :> es
       , FromJSON a
       )
    => String
    -> Value
    -> Eff es a
decodeResponse label responseValue =
    case fromJSON responseValue of
        Error err ->
            throwError (LlmExpectationError ("Failed to decode " <> label <> ": " <> err))
        Success response ->
            pure response

operationNamePath :: GeminiVideoOperationName -> [Text]
operationNamePath (GeminiVideoOperationName operationName) =
    filter (not . T.null)
        . dropV1BetaPrefix
        . T.splitOn "/"
        . T.dropWhile (== '/')
        $ operationName
  where
    dropV1BetaPrefix ("v1beta" : rest) =
        rest
    dropV1BetaPrefix parts =
        parts

veoImageValue :: GeminiInlineImage -> Value
veoImageValue GeminiInlineImage{mimeType, base64Data} =
    object
        [ "bytesBase64Encoded" .= base64Data
        , "mimeType" .= mimeType
        ]
