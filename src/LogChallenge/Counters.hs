module LogChallenge.Counters where

import ClassyPrelude
import Data.Set (Set)
import qualified Data.Set as Set
import Data.IP (IPv4, toIPv4)
import LogChallenge.Parsing


data Counter = Counter
            { name :: String
            -- , totalHits :: Integer
            -- , uniqueIPs :: Set IPv4
            , matchesLog :: LogSuccess -> Bool
            }

data CounterData = CounterData
                { counter :: Counter
                , totalHits :: Integer
                , uniqueIPs :: Set IPv4
                }

showCounterData :: CounterData -> String
showCounterData cd = "CounterData for " 
                    ++ (name (counter cd)) 
                    ++ ": totalHits = " 
                    ++ (show (totalHits cd)) 
                    ++ "; uniques count = " 
                    ++ (show (Set.size (uniqueIPs cd)))

updateWithLog :: LogSuccess -> CounterData -> CounterData
updateWithLog log cd = let oldSet = uniqueIPs cd
                           oldHits = totalHits cd
                           ip = ipAddress log
                           matches = matchesLog (counter cd) $ log
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
-- frontPageCounter :: Counter
-- frontPageCounter = Counter "Front Page" (\log -> (controller log) == "HomeController" && (method log) == "index")
paymentsPageCounter :: Counter
paymentsPageCounter = counterForNameControllerAndMethod "Payments Page" "PaymentsController" "index"

-- paymentsPageCounter :: Counter
-- paymentsPageCounter = Counter "Payments Page" (\log -> (controller log) == "PaymentsController" && (method log) == "index")

paymentsGetItemCounter :: Counter
paymentsGetItemCounter = counterForNameControllerAndMethod "Payments Get Item Page" "PaymentsController" "get_item"

-- paymentsGetItemCounter :: Counter
-- paymentsGetItemCounter = Counter "Payments Get Item Page" (\log -> (controller log) == "PaymentsController" && (method log) == "get_item")



counterForNameControllerAndMethod :: String -> Text -> Text -> Counter
counterForNameControllerAndMethod name controllerName methodName = Counter name (\log -> (controller log) == controllerName && (method log) == methodName)
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


-- data FunnelStep = Heyzap | Developers | 


-- funnelStep = [[a],[b],[c],[d,e,f]] -- potential way to do OR for funnel steps

-- Arbitrary funnel step logic?
-- e.g.
-- LogSuccess -> PrevFunnelStep -> NewFunnelStep
-- That seems like a good idea, with helper functions for "simple step" with Controller + Method
-- Then just need a way to store funnel state 

-- funnelStep = 1

-- Why not just have 3 funnels?
-- e.g. [a,b,c], [a,b,d], [a,b,e] 
-- I don't really see why you can't do both funnels
-- I suppose you might want to answer questions like, "what % of unique users completed 1 of these 3 funnels"
-- Really you could ask for arbitrarily complex logic though, so I don't think there's a 

-- Tree consists of a 
-- storing the path to the current node will be kind of a pain
    -- I guess its only a list of indexes, e.g. 0, 0, 1, 
    -- node needs to have a function mapping from (\tree -> )

-- though in practice, its ok to go A B []


-- would like to factor out the idea of having been to the previous step.
-- It could be like a tree -> 


--

-- Recursive data structure?
-- like, [Funnel Step ]
-- like a tree, basically
-- it could be like a tree, and I store the path to the current node