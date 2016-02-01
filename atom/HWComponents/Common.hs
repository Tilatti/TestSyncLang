module HWComponents.Common
(
 nodeName
) where

nodeName :: String -> Integer -> String
nodeName s i = s ++ (show i)
