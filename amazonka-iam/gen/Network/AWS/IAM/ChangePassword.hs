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

-- Module      : Network.AWS.IAM.ChangePassword
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

-- | Changes the password of the IAM user who is calling this action. The root
-- account password is not affected by this action.
--
-- To change the password for a different user, see 'UpdateLoginProfile'. For
-- more information about modifying passwords, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_ManagingLogins.html Managing Passwords> in the /Using IAM/ guide.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_ChangePassword.html>
module Network.AWS.IAM.ChangePassword
    (
    -- * Request
      ChangePassword
    -- ** Request constructor
    , changePassword
    -- ** Request lenses
    , cpNewPassword
    , cpOldPassword

    -- * Response
    , ChangePasswordResponse
    -- ** Response constructor
    , changePasswordResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.IAM.Types
import qualified GHC.Exts

data ChangePassword = ChangePassword
    { _cpNewPassword :: Sensitive Text
    , _cpOldPassword :: Sensitive Text
    } deriving (Eq, Ord, Show)

-- | 'ChangePassword' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cpNewPassword' @::@ 'Text'
--
-- * 'cpOldPassword' @::@ 'Text'
--
changePassword :: Text -- ^ 'cpOldPassword'
               -> Text -- ^ 'cpNewPassword'
               -> ChangePassword
changePassword p1 p2 = ChangePassword
    { _cpOldPassword = withIso _Sensitive (const id) p1
    , _cpNewPassword = withIso _Sensitive (const id) p2
    }

-- | The new password. The new password must conform to the AWS account's password
-- policy, if one exists.
cpNewPassword :: Lens' ChangePassword Text
cpNewPassword = lens _cpNewPassword (\s a -> s { _cpNewPassword = a }) . _Sensitive

-- | The IAM user's current password.
cpOldPassword :: Lens' ChangePassword Text
cpOldPassword = lens _cpOldPassword (\s a -> s { _cpOldPassword = a }) . _Sensitive

data ChangePasswordResponse = ChangePasswordResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'ChangePasswordResponse' constructor.
changePasswordResponse :: ChangePasswordResponse
changePasswordResponse = ChangePasswordResponse

instance ToPath ChangePassword where
    toPath = const "/"

instance ToQuery ChangePassword where
    toQuery ChangePassword{..} = mconcat
        [ "NewPassword" =? _cpNewPassword
        , "OldPassword" =? _cpOldPassword
        ]

instance ToHeaders ChangePassword

instance AWSRequest ChangePassword where
    type Sv ChangePassword = IAM
    type Rs ChangePassword = ChangePasswordResponse

    request  = post "ChangePassword"
    response = nullResponse ChangePasswordResponse
