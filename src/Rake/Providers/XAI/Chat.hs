module Rake.Providers.XAI.Chat
    ( XAIChatSettings (..)
    , defaultXAIChatSettings
    , decodeXAIResponse
    , runRakeXAIChat
    ) where

import Data.Aeson (Value)
import Effectful
import Effectful.Error.Static
import Rake.Effect
import Rake.MediaStorage.Effect
import Rake.Providers.Chat.Responses
import Rake.Providers.Internal (defaultWarningLogger)
import Rake.Types (ProviderRound)
import Relude

data XAIChatSettings es = XAIChatSettings
    { apiKey :: Text
    , model :: Text
    , baseUrl :: Text
    , requestLogger :: NativeMsgFormat -> Eff es ()
    }

defaultXAIChatSettings :: Text -> XAIChatSettings es
defaultXAIChatSettings apiKey =
    XAIChatSettings
        { apiKey
        , model = "grok-4-fast-non-reasoning"
        , baseUrl = "https://api.x.ai"
        , requestLogger = defaultWarningLogger "xai.chat"
        }

runRakeXAIChat
    :: forall es a
     . ( IOE :> es
       , Error RakeError :> es
       , RakeMediaStorage :> es
       )
    => XAIChatSettings es
    -> Eff (Rake ': es) a
    -> Eff es a
runRakeXAIChat XAIChatSettings{..} =
    runResponsesChatProvider
        ResponsesProviderConfig
            { providerTag = ResponsesProviderXAI
            , apiKey
            , model
            , baseUrl
            , organizationId = Nothing
            , projectId = Nothing
            , reasoningConfig = Nothing
            , requestLogger
            }

decodeXAIResponse :: Value -> Either RakeError ProviderRound
decodeXAIResponse =
    decodeResponsesResponse ResponsesProviderXAI
