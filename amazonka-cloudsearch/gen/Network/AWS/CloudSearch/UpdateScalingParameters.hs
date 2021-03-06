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

-- Module      : Network.AWS.CloudSearch.UpdateScalingParameters
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

-- | Configures scaling parameters for a domain. A domain's scaling parameters
-- specify the desired search instance type and replication count. Amazon
-- CloudSearch will still automatically scale your domain based on the volume of
-- data and traffic, but not below the desired instance type and replication
-- count. If the Multi-AZ option is enabled, these values control the resources
-- used per Availability Zone. For more information, see Configuring Scaling
-- Options in the /Amazon CloudSearch Developer Guide/.
--
-- <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/API_UpdateScalingParameters.html>
module Network.AWS.CloudSearch.UpdateScalingParameters
    (
    -- * Request
      UpdateScalingParameters
    -- ** Request constructor
    , updateScalingParameters
    -- ** Request lenses
    , uspDomainName
    , uspScalingParameters

    -- * Response
    , UpdateScalingParametersResponse
    -- ** Response constructor
    , updateScalingParametersResponse
    -- ** Response lenses
    , usprScalingParameters
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.CloudSearch.Types
import qualified GHC.Exts

data UpdateScalingParameters = UpdateScalingParameters
    { _uspDomainName        :: Text
    , _uspScalingParameters :: ScalingParameters
    } deriving (Eq, Show)

-- | 'UpdateScalingParameters' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'uspDomainName' @::@ 'Text'
--
-- * 'uspScalingParameters' @::@ 'ScalingParameters'
--
updateScalingParameters :: Text -- ^ 'uspDomainName'
                        -> ScalingParameters -- ^ 'uspScalingParameters'
                        -> UpdateScalingParameters
updateScalingParameters p1 p2 = UpdateScalingParameters
    { _uspDomainName        = p1
    , _uspScalingParameters = p2
    }

uspDomainName :: Lens' UpdateScalingParameters Text
uspDomainName = lens _uspDomainName (\s a -> s { _uspDomainName = a })

uspScalingParameters :: Lens' UpdateScalingParameters ScalingParameters
uspScalingParameters =
    lens _uspScalingParameters (\s a -> s { _uspScalingParameters = a })

newtype UpdateScalingParametersResponse = UpdateScalingParametersResponse
    { _usprScalingParameters :: ScalingParametersStatus
    } deriving (Eq, Show)

-- | 'UpdateScalingParametersResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'usprScalingParameters' @::@ 'ScalingParametersStatus'
--
updateScalingParametersResponse :: ScalingParametersStatus -- ^ 'usprScalingParameters'
                                -> UpdateScalingParametersResponse
updateScalingParametersResponse p1 = UpdateScalingParametersResponse
    { _usprScalingParameters = p1
    }

usprScalingParameters :: Lens' UpdateScalingParametersResponse ScalingParametersStatus
usprScalingParameters =
    lens _usprScalingParameters (\s a -> s { _usprScalingParameters = a })

instance ToPath UpdateScalingParameters where
    toPath = const "/"

instance ToQuery UpdateScalingParameters where
    toQuery UpdateScalingParameters{..} = mconcat
        [ "DomainName"        =? _uspDomainName
        , "ScalingParameters" =? _uspScalingParameters
        ]

instance ToHeaders UpdateScalingParameters

instance AWSRequest UpdateScalingParameters where
    type Sv UpdateScalingParameters = CloudSearch
    type Rs UpdateScalingParameters = UpdateScalingParametersResponse

    request  = post "UpdateScalingParameters"
    response = xmlResponse

instance FromXML UpdateScalingParametersResponse where
    parseXML = withElement "UpdateScalingParametersResult" $ \x -> UpdateScalingParametersResponse
        <$> x .@  "ScalingParameters"
