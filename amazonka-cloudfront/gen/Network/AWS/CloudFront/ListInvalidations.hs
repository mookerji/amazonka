{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudFront.ListInvalidations
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | List invalidation batches.
--
-- <http://docs.aws.amazon.com/AmazonCloudFront/latest/APIReference/ListInvalidations.html>
module Network.AWS.CloudFront.ListInvalidations
    (
    -- * Request
      ListInvalidations
    -- ** Request constructor
    , listInvalidations
    -- ** Request lenses
    , liDistributionId
    , liMarker
    , liMaxItems

    -- * Response
    , ListInvalidationsResponse
    -- ** Response constructor
    , listInvalidationsResponse
    -- ** Response lenses
    , lirInvalidationList
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.RestXML
import Network.AWS.CloudFront.Types
import qualified GHC.Exts

data ListInvalidations = ListInvalidations
    { _liDistributionId :: Text
    , _liMarker         :: Maybe Text
    , _liMaxItems       :: Maybe Text
    } deriving (Eq, Ord, Show)

-- | 'ListInvalidations' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'liDistributionId' @::@ 'Text'
--
-- * 'liMarker' @::@ 'Maybe' 'Text'
--
-- * 'liMaxItems' @::@ 'Maybe' 'Text'
--
listInvalidations :: Text -- ^ 'liDistributionId'
                  -> ListInvalidations
listInvalidations p1 = ListInvalidations
    { _liDistributionId = p1
    , _liMarker         = Nothing
    , _liMaxItems       = Nothing
    }

-- | The distribution's id.
liDistributionId :: Lens' ListInvalidations Text
liDistributionId = lens _liDistributionId (\s a -> s { _liDistributionId = a })

-- | Use this parameter when paginating results to indicate where to begin in your
-- list of invalidation batches. Because the results are returned in decreasing
-- order from most recent to oldest, the most recent results are on the first
-- page, the second page will contain earlier results, and so on. To get the
-- next page of results, set the Marker to the value of the NextMarker from the
-- current page's response. This value is the same as the ID of the last
-- invalidation batch on that page.
liMarker :: Lens' ListInvalidations (Maybe Text)
liMarker = lens _liMarker (\s a -> s { _liMarker = a })

-- | The maximum number of invalidation batches you want in the response body.
liMaxItems :: Lens' ListInvalidations (Maybe Text)
liMaxItems = lens _liMaxItems (\s a -> s { _liMaxItems = a })

newtype ListInvalidationsResponse = ListInvalidationsResponse
    { _lirInvalidationList :: InvalidationList
    } deriving (Eq, Show)

-- | 'ListInvalidationsResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lirInvalidationList' @::@ 'InvalidationList'
--
listInvalidationsResponse :: InvalidationList -- ^ 'lirInvalidationList'
                          -> ListInvalidationsResponse
listInvalidationsResponse p1 = ListInvalidationsResponse
    { _lirInvalidationList = p1
    }

-- | Information about invalidation batches.
lirInvalidationList :: Lens' ListInvalidationsResponse InvalidationList
lirInvalidationList =
    lens _lirInvalidationList (\s a -> s { _lirInvalidationList = a })

instance ToPath ListInvalidations where
    toPath ListInvalidations{..} = mconcat
        [ "/2014-10-21/distribution/"
        , toText _liDistributionId
        , "/invalidation"
        ]

instance ToQuery ListInvalidations where
    toQuery ListInvalidations{..} = mconcat
        [ "Marker"   =? _liMarker
        , "MaxItems" =? _liMaxItems
        ]

instance ToHeaders ListInvalidations

instance ToXMLRoot ListInvalidations where
    toXMLRoot = const (namespaced ns "ListInvalidations" [])

instance ToXML ListInvalidations

instance AWSRequest ListInvalidations where
    type Sv ListInvalidations = CloudFront
    type Rs ListInvalidations = ListInvalidationsResponse

    request  = get
    response = xmlResponse

instance FromXML ListInvalidationsResponse where
    parseXML x = ListInvalidationsResponse
        <$> x .@  "InvalidationList"

instance AWSPager ListInvalidations where
    page rq rs
        | stop (rs ^. lirInvalidationList . ilIsTruncated) = Nothing
        | otherwise = Just $ rq
            & liMarker .~ rs ^. lirInvalidationList . ilNextMarker
