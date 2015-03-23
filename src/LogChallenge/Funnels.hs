module LogChallenge.Funnels where

import ClassyPrelude
import LogChallenge.Parsing
import qualified Data.Map.Strict as Map
import Data.IP (IPv4, toIPv4)



data FunnelStep = FunnelStep
            { stepName :: String
            , matchesLog :: LogSuccess -> Bool
            }

data Funnel = Funnel
            { funnelName :: String
            , steps :: [FunnelStep]
            }

type FunnelPosition = Integer

data FunnelData = FunnelData
            { funnel :: Funnel
            , userToStep :: Map IPv4 FunnelPosition
            }

-- updateWithLog :: LogSuccess -> FunnelData -> FunnelData
-- updateWithLog = 

initFunnelData :: Funnel -> FunnelData
initFunnelData funnel = FunnelData funnel Map.empty 

allFunnels :: [Funnel]
allFunnels = 
    [ publisherFrontPageFunnel
    , developerImportGameFunnel
    , developerNewInventoryItemFunnel
    , developerUploadGameSimpleFunnel
    ]

-- Funnel Definitions

publisherFrontPageFunnel :: Funnel
publisherFrontPageFunnel = Funnel "Publishers Front Page Funnel" 
    [ frontPageFunnelStep
    , FunnelStep "New Site"   (matchesControllerAndMethod "PublishersController" "new_site")
    , FunnelStep "Get Embed"  (matchesControllerAndMethod "PublishersController" "get_embed")
    ]

developerImportGameFunnel :: Funnel
developerImportGameFunnel = Funnel "Developer Front Page (Import Game) Funnel"
    (developerFunnelPartial ++ [FunnelStep "Import Games" (matchesControllerAndMethod "DevelopersController" "import_games")])

developerNewInventoryItemFunnel :: Funnel
developerNewInventoryItemFunnel = Funnel "Developer Front Page (New Inventory Item) Funnel"
    (developerFunnelPartial ++ [FunnelStep "New Inventory Item" (matchesControllerAndMethod "DevelopersController" "new_inventory_item")])

developerUploadGameSimpleFunnel :: Funnel
developerUploadGameSimpleFunnel = Funnel "Developer Front Page (Upload Game) Funnel"
    (developerFunnelPartial ++ [FunnelStep "Upload Game" (matchesControllerAndMethod "DevelopersController" "upload_game_simple")])


-- Util

matchesControllerAndMethod :: Text -> Text -> (LogSuccess -> Bool)
matchesControllerAndMethod controllerName methodName = (\log -> controller log == controllerName
                                                             && method  log == methodName)

developerFunnelPartial :: [FunnelStep]
developerFunnelPartial = 
    [ frontPageFunnelStep
    , FunnelStep "Developers Page" (matchesControllerAndMethod "DevelopersController" "index")
    , FunnelStep "New Game Page" (matchesControllerAndMethod "DevelopersController" "new_game")
    ]

frontPageFunnelStep :: FunnelStep
frontPageFunnelStep = FunnelStep "Front Page" (matchesControllerAndMethod "HomeController" "index")
