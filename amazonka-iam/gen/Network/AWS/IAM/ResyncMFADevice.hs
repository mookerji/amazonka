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

-- Module      : Network.AWS.IAM.ResyncMFADevice
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

-- | Synchronizes the specified MFA device with AWS servers.
--
-- For more information about creating and working with virtual MFA devices, go
-- to <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_VirtualMFA.html Using a Virtual MFA Device> in the /Using IAM/ guide.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_ResyncMFADevice.html>
module Network.AWS.IAM.ResyncMFADevice
    (
    -- * Request
      ResyncMFADevice
    -- ** Request constructor
    , resyncMFADevice
    -- ** Request lenses
    , rmfadAuthenticationCode1
    , rmfadAuthenticationCode2
    , rmfadSerialNumber
    , rmfadUserName

    -- * Response
    , ResyncMFADeviceResponse
    -- ** Response constructor
    , resyncMFADeviceResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.IAM.Types
import qualified GHC.Exts

data ResyncMFADevice = ResyncMFADevice
    { _rmfadAuthenticationCode1 :: Text
    , _rmfadAuthenticationCode2 :: Text
    , _rmfadSerialNumber        :: Text
    , _rmfadUserName            :: Text
    } deriving (Eq, Ord, Show)

-- | 'ResyncMFADevice' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rmfadAuthenticationCode1' @::@ 'Text'
--
-- * 'rmfadAuthenticationCode2' @::@ 'Text'
--
-- * 'rmfadSerialNumber' @::@ 'Text'
--
-- * 'rmfadUserName' @::@ 'Text'
--
resyncMFADevice :: Text -- ^ 'rmfadUserName'
                -> Text -- ^ 'rmfadSerialNumber'
                -> Text -- ^ 'rmfadAuthenticationCode1'
                -> Text -- ^ 'rmfadAuthenticationCode2'
                -> ResyncMFADevice
resyncMFADevice p1 p2 p3 p4 = ResyncMFADevice
    { _rmfadUserName            = p1
    , _rmfadSerialNumber        = p2
    , _rmfadAuthenticationCode1 = p3
    , _rmfadAuthenticationCode2 = p4
    }

-- | An authentication code emitted by the device.
rmfadAuthenticationCode1 :: Lens' ResyncMFADevice Text
rmfadAuthenticationCode1 =
    lens _rmfadAuthenticationCode1
        (\s a -> s { _rmfadAuthenticationCode1 = a })

-- | A subsequent authentication code emitted by the device.
rmfadAuthenticationCode2 :: Lens' ResyncMFADevice Text
rmfadAuthenticationCode2 =
    lens _rmfadAuthenticationCode2
        (\s a -> s { _rmfadAuthenticationCode2 = a })

-- | Serial number that uniquely identifies the MFA device.
rmfadSerialNumber :: Lens' ResyncMFADevice Text
rmfadSerialNumber =
    lens _rmfadSerialNumber (\s a -> s { _rmfadSerialNumber = a })

-- | The name of the user whose MFA device you want to resynchronize.
rmfadUserName :: Lens' ResyncMFADevice Text
rmfadUserName = lens _rmfadUserName (\s a -> s { _rmfadUserName = a })

data ResyncMFADeviceResponse = ResyncMFADeviceResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'ResyncMFADeviceResponse' constructor.
resyncMFADeviceResponse :: ResyncMFADeviceResponse
resyncMFADeviceResponse = ResyncMFADeviceResponse

instance ToPath ResyncMFADevice where
    toPath = const "/"

instance ToQuery ResyncMFADevice where
    toQuery ResyncMFADevice{..} = mconcat
        [ "AuthenticationCode1" =? _rmfadAuthenticationCode1
        , "AuthenticationCode2" =? _rmfadAuthenticationCode2
        , "SerialNumber"        =? _rmfadSerialNumber
        , "UserName"            =? _rmfadUserName
        ]

instance ToHeaders ResyncMFADevice

instance AWSRequest ResyncMFADevice where
    type Sv ResyncMFADevice = IAM
    type Rs ResyncMFADevice = ResyncMFADeviceResponse

    request  = post "ResyncMFADevice"
    response = nullResponse ResyncMFADeviceResponse
