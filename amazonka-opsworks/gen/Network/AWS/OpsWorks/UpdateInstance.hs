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

-- Module      : Network.AWS.OpsWorks.UpdateInstance
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

-- | Updates a specified instance.
--
-- Required Permissions: To use this action, an IAM user must have a Manage
-- permissions level for the stack, or an attached policy that explicitly grants
-- permissions. For more information on user permissions, see <http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing UserPermissions>.
--
-- <http://docs.aws.amazon.com/opsworks/latest/APIReference/API_UpdateInstance.html>
module Network.AWS.OpsWorks.UpdateInstance
    (
    -- * Request
      UpdateInstance
    -- ** Request constructor
    , updateInstance
    -- ** Request lenses
    , uiAmiId
    , uiArchitecture
    , uiAutoScalingType
    , uiEbsOptimized
    , uiHostname
    , uiInstallUpdatesOnBoot
    , uiInstanceId
    , uiInstanceType
    , uiLayerIds
    , uiOs
    , uiSshKeyName

    -- * Response
    , UpdateInstanceResponse
    -- ** Response constructor
    , updateInstanceResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.OpsWorks.Types
import qualified GHC.Exts

data UpdateInstance = UpdateInstance
    { _uiAmiId                :: Maybe Text
    , _uiArchitecture         :: Maybe Architecture
    , _uiAutoScalingType      :: Maybe AutoScalingType
    , _uiEbsOptimized         :: Maybe Bool
    , _uiHostname             :: Maybe Text
    , _uiInstallUpdatesOnBoot :: Maybe Bool
    , _uiInstanceId           :: Text
    , _uiInstanceType         :: Maybe Text
    , _uiLayerIds             :: List "LayerIds" Text
    , _uiOs                   :: Maybe Text
    , _uiSshKeyName           :: Maybe Text
    } deriving (Eq, Show)

-- | 'UpdateInstance' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'uiAmiId' @::@ 'Maybe' 'Text'
--
-- * 'uiArchitecture' @::@ 'Maybe' 'Architecture'
--
-- * 'uiAutoScalingType' @::@ 'Maybe' 'AutoScalingType'
--
-- * 'uiEbsOptimized' @::@ 'Maybe' 'Bool'
--
-- * 'uiHostname' @::@ 'Maybe' 'Text'
--
-- * 'uiInstallUpdatesOnBoot' @::@ 'Maybe' 'Bool'
--
-- * 'uiInstanceId' @::@ 'Text'
--
-- * 'uiInstanceType' @::@ 'Maybe' 'Text'
--
-- * 'uiLayerIds' @::@ ['Text']
--
-- * 'uiOs' @::@ 'Maybe' 'Text'
--
-- * 'uiSshKeyName' @::@ 'Maybe' 'Text'
--
updateInstance :: Text -- ^ 'uiInstanceId'
               -> UpdateInstance
updateInstance p1 = UpdateInstance
    { _uiInstanceId           = p1
    , _uiLayerIds             = mempty
    , _uiInstanceType         = Nothing
    , _uiAutoScalingType      = Nothing
    , _uiHostname             = Nothing
    , _uiOs                   = Nothing
    , _uiAmiId                = Nothing
    , _uiSshKeyName           = Nothing
    , _uiArchitecture         = Nothing
    , _uiInstallUpdatesOnBoot = Nothing
    , _uiEbsOptimized         = Nothing
    }

-- | A custom AMI ID to be used to create the instance. The AMI should be based on
-- one of the standard AWS OpsWorks AMIs: Amazon Linux, Ubuntu 12.04 LTS, or
-- Ubuntu 14.04 LTS. For more information, see <http://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances.html Instances>
--
-- If you specify a custom AMI, you must set 'Os' to 'Custom'.
uiAmiId :: Lens' UpdateInstance (Maybe Text)
uiAmiId = lens _uiAmiId (\s a -> s { _uiAmiId = a })

-- | The instance architecture. Instance types do not necessarily support both
-- architectures. For a list of the architectures that are supported by the
-- different instance types, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html Instance Families and Types>.
uiArchitecture :: Lens' UpdateInstance (Maybe Architecture)
uiArchitecture = lens _uiArchitecture (\s a -> s { _uiArchitecture = a })

-- | For load-based or time-based instances, the type.
uiAutoScalingType :: Lens' UpdateInstance (Maybe AutoScalingType)
uiAutoScalingType =
    lens _uiAutoScalingType (\s a -> s { _uiAutoScalingType = a })

-- | Whether this is an Amazon EBS-optimized instance.
uiEbsOptimized :: Lens' UpdateInstance (Maybe Bool)
uiEbsOptimized = lens _uiEbsOptimized (\s a -> s { _uiEbsOptimized = a })

-- | The instance host name.
uiHostname :: Lens' UpdateInstance (Maybe Text)
uiHostname = lens _uiHostname (\s a -> s { _uiHostname = a })

-- | Whether to install operating system and package updates when the instance
-- boots. The default value is 'true'. To control when updates are installed, set
-- this value to 'false'. You must then update your instances manually by using 'CreateDeployment' to run the 'update_dependencies' stack command or manually running 'yum' (Amazon
-- Linux) or 'apt-get' (Ubuntu) on the instances.
--
-- We strongly recommend using the default value of 'true', to ensure that your
-- instances have the latest security updates.
--
--
uiInstallUpdatesOnBoot :: Lens' UpdateInstance (Maybe Bool)
uiInstallUpdatesOnBoot =
    lens _uiInstallUpdatesOnBoot (\s a -> s { _uiInstallUpdatesOnBoot = a })

-- | The instance ID.
uiInstanceId :: Lens' UpdateInstance Text
uiInstanceId = lens _uiInstanceId (\s a -> s { _uiInstanceId = a })

-- | The instance type. AWS OpsWorks supports all instance types except Cluster
-- Compute, Cluster GPU, and High Memory Cluster. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html Instance Families and Types>. The parameter values that you use to specify the various types are in the
-- API Name column of the Available Instance Types table.
uiInstanceType :: Lens' UpdateInstance (Maybe Text)
uiInstanceType = lens _uiInstanceType (\s a -> s { _uiInstanceType = a })

-- | The instance's layer IDs.
uiLayerIds :: Lens' UpdateInstance [Text]
uiLayerIds = lens _uiLayerIds (\s a -> s { _uiLayerIds = a }) . _List

-- | The instance's operating system, which must be set to one of the following.
--
-- Standard operating systems: An Amazon Linux version such as 'Amazon Linux2014.09', 'Ubuntu 12.04 LTS', or 'Ubuntu 14.04 LTS'. Custom AMIs: 'Custom'   The
-- default option is the current Amazon Linux version, such as 'Amazon Linux2014.09'. If you set this parameter to 'Custom', you must use the 'CreateInstance'
-- action's AmiId parameter to specify the custom AMI that you want to use. For
-- more information on the standard operating systems, see <http://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-os.html Operating Systems>For
-- more information on how to use custom AMIs with OpsWorks, see <http://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-custom-ami.html Using CustomAMIs>.
uiOs :: Lens' UpdateInstance (Maybe Text)
uiOs = lens _uiOs (\s a -> s { _uiOs = a })

-- | The instance SSH key name.
uiSshKeyName :: Lens' UpdateInstance (Maybe Text)
uiSshKeyName = lens _uiSshKeyName (\s a -> s { _uiSshKeyName = a })

data UpdateInstanceResponse = UpdateInstanceResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'UpdateInstanceResponse' constructor.
updateInstanceResponse :: UpdateInstanceResponse
updateInstanceResponse = UpdateInstanceResponse

instance ToPath UpdateInstance where
    toPath = const "/"

instance ToQuery UpdateInstance where
    toQuery = const mempty

instance ToHeaders UpdateInstance

instance ToJSON UpdateInstance where
    toJSON UpdateInstance{..} = object
        [ "InstanceId"           .= _uiInstanceId
        , "LayerIds"             .= _uiLayerIds
        , "InstanceType"         .= _uiInstanceType
        , "AutoScalingType"      .= _uiAutoScalingType
        , "Hostname"             .= _uiHostname
        , "Os"                   .= _uiOs
        , "AmiId"                .= _uiAmiId
        , "SshKeyName"           .= _uiSshKeyName
        , "Architecture"         .= _uiArchitecture
        , "InstallUpdatesOnBoot" .= _uiInstallUpdatesOnBoot
        , "EbsOptimized"         .= _uiEbsOptimized
        ]

instance AWSRequest UpdateInstance where
    type Sv UpdateInstance = OpsWorks
    type Rs UpdateInstance = UpdateInstanceResponse

    request  = post "UpdateInstance"
    response = nullResponse UpdateInstanceResponse
