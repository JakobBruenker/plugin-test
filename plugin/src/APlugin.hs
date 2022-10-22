{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MonoLocalBinds #-}

module APlugin (plugin) where

import GHC.Plugins hiding (Expr, Let, empty)
import GHC.Hs
import Debug.Trace
import Data.Data
import Data.Maybe

plugin :: Plugin
plugin = defaultPlugin
  { parsedResultAction = replaceBangs
  }
replaceBangs :: [CommandLineOption] -> ModSummary -> ParsedResult -> Hsc ParsedResult
replaceBangs _ _ (ParsedResult (HsParsedModule res files) msgs) = do
  pure . flip ParsedResult msgs . flip HsParsedModule files . trace "Plugin is running...\n" $ replacePuStrLn res
  where
    replacePuStrLn :: Data a => a -> a
    replacePuStrLn e = fromMaybe (gmapT replacePuStrLn e) (tryOccName e)

    tryOccName :: forall a . Data a => a -> Maybe a
    tryOccName e = do
      case eqT @a @OccName of
        Just Refl | occNameString e == "puStrLn" -> Just $ mkVarOcc "putStrLn"
        _ -> Nothing
