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

-- Module      : Network.AWS.IAM.CreateRole
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

-- | Creates a new role for your AWS account. For more information about roles,
-- go to <http://docs.aws.amazon.com/IAM/latest/UserGuide/WorkingWithRoles.html Working with Roles>. For information about limitations on role names and
-- the number of roles you can create, go to <http://docs.aws.amazon.com/IAM/latest/UserGuide/LimitationsOnEntities.html Limitations on IAM Entities> in the /Using IAM/ guide.
--
-- The example policy grants permission to an EC2 instance to assume the role.
-- The policy is URL-encoded according to RFC 3986. For more information about
-- RFC 3986, go to <http://www.faqs.org/rfcs/rfc3986.html http://www.faqs.org/rfcs/rfc3986.html>.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_CreateRole.html>
module Network.AWS.IAM.CreateRole
    (
    -- * Request
      CreateRole
    -- ** Request constructor
    , createRole
    -- ** Request lenses
    , crAssumeRolePolicyDocument
    , crPath
    , crRoleName

    -- * Response
    , CreateRoleResponse
    -- ** Response constructor
    , createRoleResponse
    -- ** Response lenses
    , crrRole
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.IAM.Types
import qualified GHC.Exts

data CreateRole = CreateRole
    { _crAssumeRolePolicyDocument :: Text
    , _crPath                     :: Maybe Text
    , _crRoleName                 :: Text
    } deriving (Eq, Ord, Show)

-- | 'CreateRole' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'crAssumeRolePolicyDocument' @::@ 'Text'
--
-- * 'crPath' @::@ 'Maybe' 'Text'
--
-- * 'crRoleName' @::@ 'Text'
--
createRole :: Text -- ^ 'crRoleName'
           -> Text -- ^ 'crAssumeRolePolicyDocument'
           -> CreateRole
createRole p1 p2 = CreateRole
    { _crRoleName                 = p1
    , _crAssumeRolePolicyDocument = p2
    , _crPath                     = Nothing
    }

-- | The policy that grants an entity permission to assume the role.
crAssumeRolePolicyDocument :: Lens' CreateRole Text
crAssumeRolePolicyDocument =
    lens _crAssumeRolePolicyDocument
        (\s a -> s { _crAssumeRolePolicyDocument = a })

-- | The path to the role. For more information about paths, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers>
-- in the /Using IAM/ guide.
--
-- This parameter is optional. If it is not included, it defaults to a slash
-- (/).
crPath :: Lens' CreateRole (Maybe Text)
crPath = lens _crPath (\s a -> s { _crPath = a })

-- | The name of the role to create.
crRoleName :: Lens' CreateRole Text
crRoleName = lens _crRoleName (\s a -> s { _crRoleName = a })

newtype CreateRoleResponse = CreateRoleResponse
    { _crrRole :: Role
    } deriving (Eq, Show)

-- | 'CreateRoleResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'crrRole' @::@ 'Role'
--
createRoleResponse :: Role -- ^ 'crrRole'
                   -> CreateRoleResponse
createRoleResponse p1 = CreateRoleResponse
    { _crrRole = p1
    }

-- | Information about the role.
crrRole :: Lens' CreateRoleResponse Role
crrRole = lens _crrRole (\s a -> s { _crrRole = a })

instance ToPath CreateRole where
    toPath = const "/"

instance ToQuery CreateRole where
    toQuery CreateRole{..} = mconcat
        [ "AssumeRolePolicyDocument" =? _crAssumeRolePolicyDocument
        , "Path"                     =? _crPath
        , "RoleName"                 =? _crRoleName
        ]

instance ToHeaders CreateRole

instance AWSRequest CreateRole where
    type Sv CreateRole = IAM
    type Rs CreateRole = CreateRoleResponse

    request  = post "CreateRole"
    response = xmlResponse

instance FromXML CreateRoleResponse where
    parseXML = withElement "CreateRoleResult" $ \x -> CreateRoleResponse
        <$> x .@  "Role"
