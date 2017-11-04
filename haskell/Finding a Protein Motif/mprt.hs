#!/usr/bin/env stack
-- stack --install-ghc runghc --package http-conduit

module Main where

import Data.List
import Text.Printf
import Data.Aeson
import Network.HTTP.Client
import qualified Data.ByteString.Lazy.Char8 as C
import Control.Arrow
import qualified Text.Regex as R
import Text.Regex.PCRE
import Data.Array ((!))

subRegexWith re f s =
  case R.matchRegexAll re s of
    Nothing -> s
    Just (before, matched, after, _) ->
      before ++ f matched ++ (subRegexWith re f after)

firstLast xs@(_:_) = tail (init xs)
firstLast _ = []

pattern :: String -> String
pattern =
  let handleOr = (\x -> "[" ++ intersperse '|' (firstLast x) ++ "]")
      handleAnd = (printf "[^%s]") . firstLast
      processOr = subRegexWith (R.mkRegex "\\[(.+)\\]") handleOr
      processAnd = subRegexWith (R.mkRegex "\\{(.{1})\\}") handleAnd
      processOverlapping = printf "(?=%s)([A-Z]{1})"
  in processOverlapping . processAnd . processOr

withGivenIds :: [String] -> [String] -> [(String, String)]
withGivenIds = zipWith (curry (second skipFirst))
  where skipFirst = concat . drop 1 . lines

httpGet :: String -> IO String
httpGet s = do
    manager <- newManager defaultManagerSettings
    request <- parseRequest s
    response <- httpLbs request manager
    return $ (C.unpack . responseBody) response

fastaUrlById :: String -> String
fastaUrlById = printf "http://www.uniprot.org/uniprot/%s.fasta"

occurences :: String -> String -> [Int]
occurences p s =
  let re = makeRegex p :: Regex
  in map ((+1) . fst . (!0)) $ matchAll re s

solve :: String -> [(String, String)] -> [(String, [Int])]
solve p = map (second $ occurences p')
   where p' = pattern p

main :: IO ()
main = do
  let ids = ["A2Z669", "B5ZC00", "P07204_TRBM_HUMAN", "P20840_SAG1_YEAST"]
  downloaded <- mapM httpGet (map fastaUrlById ids)
  let fastaWithIds = withGivenIds ids downloaded
  let result = solve "N{P}[ST]{P}" fastaWithIds
  print result
