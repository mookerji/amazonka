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

-- Module      : Network.AWS.IAM.GetInstanceProfile
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

-- | Retrieves information about the specified instance profile, including the
-- instance profile's path, GUID, ARN, and role. For more information about
-- instance profiles, go to <http://docs.aws.amazon.com/IAM/latest/UserGuide/AboutInstanceProfiles.html About Instance Profiles>. For more information about
-- ARNs, go to <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html#Identifiers_ARNs ARNs>.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_GetInstanceProfile.html>
module Network.AWS.IAM.GetInstanceProfile
    (
    -- * Request
      GetInstanceProfile
    -- ** Request constructor
    , getInstanceProfile
    -- ** Request lenses
    , gipInstanceProfileName

    -- * Response
    , GetInstanceProfileResponse
    -- ** Response constructor
    , getInstanceProfileResponse
    -- ** Response lenses
    , giprInstanceProfile
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.IAM.Types
import qualified GHC.Exts

newtype GetInstanceProfile = GetInstanceProfile
    { _gipInstanceProfileName :: Text
    } deriving (Eq, Ord, Show, Monoid, IsString)

-- | 'GetInstanceProfile' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gipInstanceProfileName' @::@ 'Text'
--
getInstanceProfile :: Text -- ^ 'gipInstanceProfileName'
                   -> GetInstanceProfile
getInstanceProfile p1 = GetInstanceProfile
    { _gipInstanceProfileName = p1
    }

-- | The name of the instance profile to get information about.
gipInstanceProfileName :: Lens' GetInstanceProfile Text
gipInstanceProfileName =
    lens _gipInstanceProfileName (\s a -> s { _gipInstanceProfileName = a })

newtype GetInstanceProfileResponse = GetInstanceProfileResponse
    { _giprInstanceProfile :: InstanceProfile
    } deriving (Eq, Show)

-- | 'GetInstanceProfileResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'giprInstanceProfile' @::@ 'InstanceProfile'
--
getInstanceProfileResponse :: InstanceProfile -- ^ 'giprInstanceProfile'
                           -> GetInstanceProfileResponse
getInstanceProfileResponse p1 = GetInstanceProfileResponse
    { _giprInstanceProfile = p1
    }

-- | Information about the instance profile.
giprInstanceProfile :: Lens' GetInstanceProfileResponse InstanceProfile
giprInstanceProfile =
    lens _giprInstanceProfile (\s a -> s { _giprInstanceProfile = a })

instance ToPath GetInstanceProfile where
    toPath = const "/"

instance ToQuery GetInstanceProfile where
    toQuery GetInstanceProfile{..} = mconcat
        [ "InstanceProfileName" =? _gipInstanceProfileName
        ]

instance ToHeaders GetInstanceProfile

instance AWSRequest GetInstanceProfile where
    type Sv GetInstanceProfile = IAM
    type Rs GetInstanceProfile = GetInstanceProfileResponse

    request  = post "GetInstanceProfile"
    response = xmlResponse

instance FromXML GetInstanceProfileResponse where
    parseXML = withElement "GetInstanceProfileResult" $ \x -> GetInstanceProfileResponse
        <$> x .@  "InstanceProfile"
