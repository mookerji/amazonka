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

-- Module      : Network.AWS.IAM.GetRole
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

-- | Retrieves information about the specified role, including the role's path,
-- GUID, ARN, and the policy granting permission to assume the role. For more
-- information about ARNs, go to <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html#Identifiers_ARNs ARNs>. For more information about roles, go to <http://docs.aws.amazon.com/IAM/latest/UserGuide/WorkingWithRoles.html Working with Roles>.
--
-- The returned policy is URL-encoded according to RFC 3986. For more
-- information about RFC 3986, go to <http://www.faqs.org/rfcs/rfc3986.html http://www.faqs.org/rfcs/rfc3986.html>.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_GetRole.html>
module Network.AWS.IAM.GetRole
    (
    -- * Request
      GetRole
    -- ** Request constructor
    , getRole
    -- ** Request lenses
    , grRoleName

    -- * Response
    , GetRoleResponse
    -- ** Response constructor
    , getRoleResponse
    -- ** Response lenses
    , grrRole
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.IAM.Types
import qualified GHC.Exts

newtype GetRole = GetRole
    { _grRoleName :: Text
    } deriving (Eq, Ord, Show, Monoid, IsString)

-- | 'GetRole' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'grRoleName' @::@ 'Text'
--
getRole :: Text -- ^ 'grRoleName'
        -> GetRole
getRole p1 = GetRole
    { _grRoleName = p1
    }

-- | The name of the role to get information about.
grRoleName :: Lens' GetRole Text
grRoleName = lens _grRoleName (\s a -> s { _grRoleName = a })

newtype GetRoleResponse = GetRoleResponse
    { _grrRole :: Role
    } deriving (Eq, Show)

-- | 'GetRoleResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'grrRole' @::@ 'Role'
--
getRoleResponse :: Role -- ^ 'grrRole'
                -> GetRoleResponse
getRoleResponse p1 = GetRoleResponse
    { _grrRole = p1
    }

-- | Information about the role.
grrRole :: Lens' GetRoleResponse Role
grrRole = lens _grrRole (\s a -> s { _grrRole = a })

instance ToPath GetRole where
    toPath = const "/"

instance ToQuery GetRole where
    toQuery GetRole{..} = mconcat
        [ "RoleName" =? _grRoleName
        ]

instance ToHeaders GetRole

instance AWSRequest GetRole where
    type Sv GetRole = IAM
    type Rs GetRole = GetRoleResponse

    request  = post "GetRole"
    response = xmlResponse

instance FromXML GetRoleResponse where
    parseXML = withElement "GetRoleResult" $ \x -> GetRoleResponse
        <$> x .@  "Role"
