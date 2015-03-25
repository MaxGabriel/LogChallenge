module LogChallenge.Counters where

import ClassyPrelude
import Data.Set (Set)
import qualified Data.Set as Set
import Data.IP (IPv4, toIPv4)
import LogChallenge.Parsing


data Counter = Counter
            { name :: String
            , matchesLog :: LogSuccess -> Bool
            }

data CounterData = CounterData
                { counter :: Counter
                , totalHits :: Integer
                , uniqueIPs :: Set IPv4
                }

showCounterData :: CounterData -> String
showCounterData cd = "CounterData for " 
                    ++ name (counter cd)
                    ++ ": totalHits = " 
                    ++ show (totalHits cd)
                    ++ "; uniques count = " 
                    ++ show (Set.size (uniqueIPs cd))

updateWithLog :: LogSuccess -> CounterData -> CounterData
updateWithLog log cd = let oldSet = uniqueIPs cd
                           oldHits = totalHits cd
                           ip = ipAddress log
                           matches = matchesLog (counter cd) log
                        in 
                            if matches then
                                cd { totalHits = oldHits + 1, uniqueIPs = Set.insert ip oldSet }
                            else 
                                cd



initCounterData :: Counter -> CounterData
initCounterData c = CounterData c 0 Set.empty

allCounters :: [Counter]
allCounters = [frontPageCounter, paymentsPageCounter, paymentsGetItemCounter]

-- The spec specifies front page hit as being "http://heyzap.com", but instead of looking for the exact URL,
-- I use the controller + method. This avoids issues of multiple URLs resolving to that same page
-- e.g. URLs with trailing slashes, URLs like "http://heyzap.com/home", "http://heyzap.com/"" (note the trailing quotation mark). etc.
frontPageCounter :: Counter
frontPageCounter = counterForNameControllerAndMethod "Front Page" "HomeController" "index"

paymentsPageCounter :: Counter
paymentsPageCounter = counterForNameControllerAndMethod "Payments Page" "PaymentsController" "index"

paymentsGetItemCounter :: Counter
paymentsGetItemCounter = counterForNameControllerAndMethod "Payments Get Item Page" "PaymentsController" "get_item"


counterForNameControllerAndMethod :: String -> Text -> Text -> Counter
counterForNameControllerAndMethod name controllerName methodName = Counter name (\log -> controller log == controllerName && method log == methodName)
-- Ugh. The URL doesn't always have a permalink parameter, sometimes its in the path of the URL, so I need to parse out the Parameters section

-- weeblyCounter :: Counter
-- weeblyCounter = Counter "Weebly Game Plays" (\log ->
--                     (controller log) == "Heyzap"
--                     && (action log) == "index"
--                     && lookup "embed_key" params == Just "12affbbace"
--                     && lookup "permalink" params /= Nothing

--                         where 
--                             params = uriParameter (uri log)
--                     )

