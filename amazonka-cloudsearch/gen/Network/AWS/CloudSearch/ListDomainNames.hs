{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudSearch.ListDomainNames
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Lists all search domains owned by an account.
module Network.AWS.CloudSearch.ListDomainNames
    (
    -- * Request
      ListDomainNames
    -- ** Request constructor
    , listDomainNames
    -- * Response
    , ListDomainNamesResponse
    -- ** Response constructor
    , listDomainNamesResponse
    -- ** Response lenses
    , ldnrDomainNames
    ) where

import Network.AWS.Request.Query
import Network.AWS.CloudSearch.Types
import Network.AWS.Prelude

data ListDomainNames = ListDomainNames
    deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ListDomainNames' request.
listDomainNames :: ListDomainNames
listDomainNames = ListDomainNames

instance ToQuery ListDomainNames where
    toQuery = genericQuery def

-- | The result of a ListDomainNames request. Contains a list of the domains
-- owned by an account.
newtype ListDomainNamesResponse = ListDomainNamesResponse
    { _ldnrDomainNames :: Map Text Text
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ListDomainNamesResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @DomainNames ::@ @Map Text Text@
--
listDomainNamesResponse :: ListDomainNamesResponse
listDomainNamesResponse = ListDomainNamesResponse
    { _ldnrDomainNames = mempty
    }

-- | The names of the search domains owned by an account.
ldnrDomainNames :: Lens' ListDomainNamesResponse (Map Text Text)
ldnrDomainNames = lens _ldnrDomainNames (\s a -> s { _ldnrDomainNames = a })

instance FromXML ListDomainNamesResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest ListDomainNames where
    type Sv ListDomainNames = CloudSearch
    type Rs ListDomainNames = ListDomainNamesResponse

    request = post "ListDomainNames"
    response _ = xmlResponse
