module Rake.Providers.OpenAI.Chat
    ( OpenAIReasoningEffort (..)
    , OpenAIChatSettings (..)
    , defaultOpenAIChatSettings
    , decodeOpenAIResponse
    , runRakeOpenAIChat
    ) where

import Data.Aeson (ToJSON (..), Value (..), object, (.=))
import Effectful
import Effectful.Error.Static
import Rake.Effect
import Rake.MediaStorage.Effect
import Rake.Providers.Chat.Responses
import Rake.Providers.Internal (defaultWarningLogger)
import Rake.Types (ProviderRound)
import Relude

data OpenAIReasoningEffort
    = OpenAIReasoningNone
    | OpenAIReasoningLow
    | OpenAIReasoningMedium
    | OpenAIReasoningHigh
    | OpenAIReasoningXHigh
    | OpenAIReasoningMax
    deriving stock (Show, Eq)

instance ToJSON OpenAIReasoningEffort where
    toJSON =
        String . \case
            OpenAIReasoningNone -> "none"
            OpenAIReasoningLow -> "low"
            OpenAIReasoningMedium -> "medium"
            OpenAIReasoningHigh -> "high"
            OpenAIReasoningXHigh -> "xhigh"
            OpenAIReasoningMax -> "max"

data OpenAIChatSettings es = OpenAIChatSettings
    { apiKey :: Text
    , model :: Text
    , baseUrl :: Text
    , organizationId :: Maybe Text
    , projectId :: Maybe Text
    , reasoningEffort :: Maybe OpenAIReasoningEffort
    , requestLogger :: NativeMsgFormat -> Eff es ()
    }

defaultOpenAIChatSettings :: Text -> OpenAIChatSettings es
defaultOpenAIChatSettings apiKey =
    OpenAIChatSettings
        { apiKey
        , model = "gpt-4.1-mini"
        , baseUrl = "https://api.openai.com"
        , organizationId = Nothing
        , projectId = Nothing
        , reasoningEffort = Nothing
        , requestLogger = defaultWarningLogger "openai.chat"
        }

runRakeOpenAIChat
    :: forall es a
     . ( IOE :> es
       , Error RakeError :> es
       , RakeMediaStorage :> es
       )
    => OpenAIChatSettings es
    -> Eff (Rake ': es) a
    -> Eff es a
runRakeOpenAIChat OpenAIChatSettings{..} =
    runResponsesChatProvider
        ResponsesProviderConfig
            { providerTag = ResponsesProviderOpenAI
            , apiKey
            , model
            , baseUrl
            , organizationId
            , projectId
            , reasoningConfig = (\effort -> object ["effort" .= effort]) <$> reasoningEffort
            , requestLogger
            }

decodeOpenAIResponse :: Value -> Either RakeError ProviderRound
decodeOpenAIResponse =
    decodeResponsesResponse ResponsesProviderOpenAI
