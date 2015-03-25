module LogChallenge.Funnels where

import ClassyPrelude
import LogChallenge.Parsing
import qualified Data.Map.Strict as Map
import Data.IP (IPv4)
import Data.Maybe (fromJust)



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

infixr 4 `orElse`

-- | flipped version of @fromMaybe@.
orElse :: Maybe a -> a -> a
(Just x) `orElse` _ = x
Nothing  `orElse` y = y


showFunnelData :: FunnelData -> String
showFunnelData funnelData = 
    "FunnelData for "
    ++ funnelName (funnel funnelData)
    ++ " is "
    ++ show (funnelResults funnelData)

funnelResults :: FunnelData -> [(String, Integer)]
funnelResults funnelData =
    map formatStep
        $ Map.toList 
        $ ofoldr incrementSteps Map.empty (userToStep funnelData)
    where
        incrementSteps :: Integer -> Map Integer Integer -> Map Integer Integer
        incrementSteps stepInt stepMap = ofoldr incrementIndex stepMap [stepInt - 1, (stepInt - 2)..0]

        incrementIndex :: Integer -> Map Integer Integer -> Map Integer Integer
        incrementIndex idx intMap = let oldValue = lookup idx intMap `orElse` 0
                                    in insertMap idx (oldValue + 1) intMap

        formatStep :: (Integer, Integer) -> (String, Integer)
        formatStep (k,v) = (stepName $ fromJust $ index (steps $ funnel funnelData) (fromIntegral k), v)



updateFunnelWithLog :: LogSuccess -> FunnelData -> FunnelData
updateFunnelWithLog logSuccess funnelData = 
    let oldMap = userToStep funnelData
        userIP = ipAddress logSuccess
        currentStep = lookup userIP oldMap `orElse` 0
        maybeCurrentStep = index (steps (funnel funnelData)) (fromIntegral currentStep)
        maybeCurrentStepFn = fmap matchesLog maybeCurrentStep
    in case maybeCurrentStepFn <*> pure logSuccess of
        Just True -> funnelData { userToStep = insertMap userIP (currentStep + 1) oldMap }
        Just False -> funnelData
        Nothing -> funnelData

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

matchesControllerAndMethod :: Text -> Text -> LogSuccess -> Bool
matchesControllerAndMethod controllerName methodName theLog = controller theLog == controllerName
                                                           && method theLog == methodName

developerFunnelPartial :: [FunnelStep]
developerFunnelPartial = 
    [ frontPageFunnelStep
    , FunnelStep "Developers Page" (matchesControllerAndMethod "DevelopersController" "index")
    , FunnelStep "New Game Page" (matchesControllerAndMethod "DevelopersController" "new_game")
    ]

frontPageFunnelStep :: FunnelStep
frontPageFunnelStep = FunnelStep "Front Page" (matchesControllerAndMethod "HomeController" "index")
