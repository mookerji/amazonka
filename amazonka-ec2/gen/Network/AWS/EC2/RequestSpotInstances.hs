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

-- Module      : Network.AWS.EC2.RequestSpotInstances
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

-- | Creates a Spot Instance request. Spot Instances are instances that Amazon EC2
-- starts on your behalf when the maximum price that you specify exceeds the
-- current Spot Price. Amazon EC2 periodically sets the Spot Price based on
-- available Spot Instance capacity and current Spot Instance requests. For more
-- information about Spot Instances, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/using-spot-instances.html Spot Instances> in the /Amazon ElasticCompute Cloud User Guide/.
--
-- Users must be subscribed to the required product to run an instance with AWS
-- Marketplace product codes.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-RequestSpotInstances.html>
module Network.AWS.EC2.RequestSpotInstances
    (
    -- * Request
      RequestSpotInstances
    -- ** Request constructor
    , requestSpotInstances
    -- ** Request lenses
    , rsiAvailabilityZoneGroup
    , rsiDryRun
    , rsiInstanceCount
    , rsiLaunchGroup
    , rsiLaunchSpecification
    , rsiSpotPrice
    , rsiType
    , rsiValidFrom
    , rsiValidUntil

    -- * Response
    , RequestSpotInstancesResponse
    -- ** Response constructor
    , requestSpotInstancesResponse
    -- ** Response lenses
    , rsirSpotInstanceRequests
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import qualified GHC.Exts

data RequestSpotInstances = RequestSpotInstances
    { _rsiAvailabilityZoneGroup :: Maybe Text
    , _rsiDryRun                :: Maybe Bool
    , _rsiInstanceCount         :: Maybe Int
    , _rsiLaunchGroup           :: Maybe Text
    , _rsiLaunchSpecification   :: Maybe RequestSpotLaunchSpecification
    , _rsiSpotPrice             :: Text
    , _rsiType                  :: Maybe SpotInstanceType
    , _rsiValidFrom             :: Maybe ISO8601
    , _rsiValidUntil            :: Maybe ISO8601
    } deriving (Eq, Show)

-- | 'RequestSpotInstances' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rsiAvailabilityZoneGroup' @::@ 'Maybe' 'Text'
--
-- * 'rsiDryRun' @::@ 'Maybe' 'Bool'
--
-- * 'rsiInstanceCount' @::@ 'Maybe' 'Int'
--
-- * 'rsiLaunchGroup' @::@ 'Maybe' 'Text'
--
-- * 'rsiLaunchSpecification' @::@ 'Maybe' 'RequestSpotLaunchSpecification'
--
-- * 'rsiSpotPrice' @::@ 'Text'
--
-- * 'rsiType' @::@ 'Maybe' 'SpotInstanceType'
--
-- * 'rsiValidFrom' @::@ 'Maybe' 'UTCTime'
--
-- * 'rsiValidUntil' @::@ 'Maybe' 'UTCTime'
--
requestSpotInstances :: Text -- ^ 'rsiSpotPrice'
                     -> RequestSpotInstances
requestSpotInstances p1 = RequestSpotInstances
    { _rsiSpotPrice             = p1
    , _rsiDryRun                = Nothing
    , _rsiInstanceCount         = Nothing
    , _rsiType                  = Nothing
    , _rsiValidFrom             = Nothing
    , _rsiValidUntil            = Nothing
    , _rsiLaunchGroup           = Nothing
    , _rsiAvailabilityZoneGroup = Nothing
    , _rsiLaunchSpecification   = Nothing
    }

-- | The user-specified name for a logical grouping of bids.
--
-- When you specify an Availability Zone group in a Spot Instance request, all
-- Spot Instances in the request are launched in the same Availability Zone.
-- Instance proximity is maintained with this parameter, but the choice of
-- Availability Zone is not. The group applies only to bids for Spot Instances
-- of the same instance type. Any additional Spot Instance requests that are
-- specified with the same Availability Zone group name are launched in that
-- same Availability Zone, as long as at least one instance from the group is
-- still active.
--
-- If there is no active instance running in the Availability Zone group that
-- you specify for a new Spot Instance request (all instances are terminated,
-- the bid is expired, or the bid falls below current market), then Amazon EC2
-- launches the instance in any Availability Zone where the constraint can be
-- met. Consequently, the subsequent set of Spot Instances could be placed in a
-- different zone from the original request, even if you specified the same
-- Availability Zone group.
--
-- Default: Instances are launched in any available Availability Zone.
rsiAvailabilityZoneGroup :: Lens' RequestSpotInstances (Maybe Text)
rsiAvailabilityZoneGroup =
    lens _rsiAvailabilityZoneGroup
        (\s a -> s { _rsiAvailabilityZoneGroup = a })

rsiDryRun :: Lens' RequestSpotInstances (Maybe Bool)
rsiDryRun = lens _rsiDryRun (\s a -> s { _rsiDryRun = a })

-- | The maximum number of Spot Instances to launch.
--
-- Default: 1
rsiInstanceCount :: Lens' RequestSpotInstances (Maybe Int)
rsiInstanceCount = lens _rsiInstanceCount (\s a -> s { _rsiInstanceCount = a })

-- | The instance launch group. Launch groups are Spot Instances that launch
-- together and terminate together.
--
-- Default: Instances are launched and terminated individually
rsiLaunchGroup :: Lens' RequestSpotInstances (Maybe Text)
rsiLaunchGroup = lens _rsiLaunchGroup (\s a -> s { _rsiLaunchGroup = a })

rsiLaunchSpecification :: Lens' RequestSpotInstances (Maybe RequestSpotLaunchSpecification)
rsiLaunchSpecification =
    lens _rsiLaunchSpecification (\s a -> s { _rsiLaunchSpecification = a })

-- | The maximum hourly price for any Spot Instance launched to fulfill the
-- request.
rsiSpotPrice :: Lens' RequestSpotInstances Text
rsiSpotPrice = lens _rsiSpotPrice (\s a -> s { _rsiSpotPrice = a })

-- | The Spot Instance request type.
--
-- Default: 'one-time'
rsiType :: Lens' RequestSpotInstances (Maybe SpotInstanceType)
rsiType = lens _rsiType (\s a -> s { _rsiType = a })

-- | The start date of the request. If this is a one-time request, the request
-- becomes active at this date and time and remains active until all instances
-- launch, the request expires, or the request is canceled. If the request is
-- persistent, the request becomes active at this date and time and remains
-- active until it expires or is canceled.
--
-- Default: The request is effective indefinitely.
rsiValidFrom :: Lens' RequestSpotInstances (Maybe UTCTime)
rsiValidFrom = lens _rsiValidFrom (\s a -> s { _rsiValidFrom = a }) . mapping _Time

-- | The end date of the request. If this is a one-time request, the request
-- remains active until all instances launch, the request is canceled, or this
-- date is reached. If the request is persistent, it remains active until it is
-- canceled or this date and time is reached.
--
-- Default: The request is effective indefinitely.
rsiValidUntil :: Lens' RequestSpotInstances (Maybe UTCTime)
rsiValidUntil = lens _rsiValidUntil (\s a -> s { _rsiValidUntil = a }) . mapping _Time

newtype RequestSpotInstancesResponse = RequestSpotInstancesResponse
    { _rsirSpotInstanceRequests :: List "item" SpotInstanceRequest
    } deriving (Eq, Show, Monoid, Semigroup)

-- | 'RequestSpotInstancesResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rsirSpotInstanceRequests' @::@ ['SpotInstanceRequest']
--
requestSpotInstancesResponse :: RequestSpotInstancesResponse
requestSpotInstancesResponse = RequestSpotInstancesResponse
    { _rsirSpotInstanceRequests = mempty
    }

-- | Information about the Spot Instance request.
rsirSpotInstanceRequests :: Lens' RequestSpotInstancesResponse [SpotInstanceRequest]
rsirSpotInstanceRequests =
    lens _rsirSpotInstanceRequests
        (\s a -> s { _rsirSpotInstanceRequests = a })
            . _List

instance ToPath RequestSpotInstances where
    toPath = const "/"

instance ToQuery RequestSpotInstances where
    toQuery RequestSpotInstances{..} = mconcat
        [ "availabilityZoneGroup" =? _rsiAvailabilityZoneGroup
        , "dryRun"                =? _rsiDryRun
        , "instanceCount"         =? _rsiInstanceCount
        , "launchGroup"           =? _rsiLaunchGroup
        , "LaunchSpecification"   =? _rsiLaunchSpecification
        , "spotPrice"             =? _rsiSpotPrice
        , "type"                  =? _rsiType
        , "validFrom"             =? _rsiValidFrom
        , "validUntil"            =? _rsiValidUntil
        ]

instance ToHeaders RequestSpotInstances

instance AWSRequest RequestSpotInstances where
    type Sv RequestSpotInstances = EC2
    type Rs RequestSpotInstances = RequestSpotInstancesResponse

    request  = post "RequestSpotInstances"
    response = xmlResponse

instance FromXML RequestSpotInstancesResponse where
    parseXML x = RequestSpotInstancesResponse
        <$> x .@? "spotInstanceRequestSet" .!@ mempty
