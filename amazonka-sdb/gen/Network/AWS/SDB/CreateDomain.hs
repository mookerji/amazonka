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

-- Module      : Network.AWS.SDB.CreateDomain
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

-- | The 'CreateDomain' operation creates a new domain. The domain name should be
-- unique among the domains associated with the Access Key ID provided in the
-- request. The 'CreateDomain' operation may take 10 or more seconds to complete.
--
-- The client can create up to 100 domains per account.
--
-- If the client requires additional domains, go to <http://aws.amazon.com/contact-us/simpledb-limit-request/ http://aws.amazon.com/contact-us/simpledb-limit-request/>.
--
-- <http://docs.aws.amazon.com/AmazonSimpleDB/latest/DeveloperGuide/SDB_API_CreateDomain.html>
module Network.AWS.SDB.CreateDomain
    (
    -- * Request
      CreateDomain
    -- ** Request constructor
    , createDomain
    -- ** Request lenses
    , cdDomainName

    -- * Response
    , CreateDomainResponse
    -- ** Response constructor
    , createDomainResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.SDB.Types
import qualified GHC.Exts

newtype CreateDomain = CreateDomain
    { _cdDomainName :: Text
    } deriving (Eq, Ord, Show, Monoid, IsString)

-- | 'CreateDomain' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cdDomainName' @::@ 'Text'
--
createDomain :: Text -- ^ 'cdDomainName'
             -> CreateDomain
createDomain p1 = CreateDomain
    { _cdDomainName = p1
    }

-- | The name of the domain to create. The name can range between 3 and 255
-- characters and can contain the following characters: a-z, A-Z, 0-9, '_', '-',
-- and '.'.
cdDomainName :: Lens' CreateDomain Text
cdDomainName = lens _cdDomainName (\s a -> s { _cdDomainName = a })

data CreateDomainResponse = CreateDomainResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'CreateDomainResponse' constructor.
createDomainResponse :: CreateDomainResponse
createDomainResponse = CreateDomainResponse

instance ToPath CreateDomain where
    toPath = const "/"

instance ToQuery CreateDomain where
    toQuery CreateDomain{..} = mconcat
        [ "DomainName" =? _cdDomainName
        ]

instance ToHeaders CreateDomain

instance AWSRequest CreateDomain where
    type Sv CreateDomain = SDB
    type Rs CreateDomain = CreateDomainResponse

    request  = post "CreateDomain"
    response = nullResponse CreateDomainResponse
