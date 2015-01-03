{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.Data.Internal.JSON
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.Data.Internal.JSON
    (
    -- * FromJSON
      FromJSON (..)
    , parseJSONText
    , eitherDecode'
    , pprint
    -- ** Parser a
    , withObject
    , (.:)
    , (.:?)
    , (.!=)
    -- ** Either String a
    , (.:>)
    , (.:?>)

    -- * ToJSON
    , ToJSON   (..)
    , toJSONText
    , object
    , (.=)
    ) where

import           Data.Aeson                           (eitherDecode', json)
import           Data.Aeson.Encode.Pretty             (encodePretty', defConfig)
import           Data.Aeson.Types
import qualified Data.Attoparsec.ByteString.Lazy      as ABS
import qualified Data.HashMap.Strict                  as Map
import           Data.Text                            (Text)
import           Network.AWS.Data.Internal.ByteString
import           Network.AWS.Data.Internal.Text

-- | Return pretty printed LBS if parseable JSON, otherwise return
-- original LBS.
--
pprint :: LazyByteString -> LazyByteString
pprint lbs = either (const lbs)
                    (encodePretty' defConfig)
                    (ABS.eitherResult $ ABS.parse json lbs)

parseJSONText :: FromText a => String -> Value -> Parser a
parseJSONText n = withText n (either fail return . fromText)
{-# INLINE parseJSONText #-}

toJSONText :: ToText a => a -> Value
toJSONText = String . toText
{-# INLINE toJSONText #-}

(.:>) :: FromJSON a => Object -> Text -> Either String a
(.:>) o k =
    case Map.lookup k o of
        Nothing -> Left $ "key " ++ show k ++ " not present"
        Just v  -> parseEither parseJSON v
{-# INLINE (.:>) #-}

(.:?>) :: FromJSON a => Object -> Text -> Either String (Maybe a)
(.:?>) o k =
    case Map.lookup k o of
        Nothing -> Right Nothing
        Just v  -> parseEither parseJSON v
{-# INLINE (.:?>) #-}
