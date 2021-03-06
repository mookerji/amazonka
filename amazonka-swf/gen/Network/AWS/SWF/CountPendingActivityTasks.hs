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

-- Module      : Network.AWS.SWF.CountPendingActivityTasks
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

-- | Returns the estimated number of activity tasks in the specified task list.
-- The count returned is an approximation and is not guaranteed to be exact. If
-- you specify a task list that no activity task was ever scheduled in then 0
-- will be returned.
--
-- Access Control
--
-- You can use IAM policies to control this action's access to Amazon SWF
-- resources as follows:
--
-- Use a 'Resource' element with the domain name to limit the action to only
-- specified domains. Use an 'Action' element to allow or deny permission to call
-- this action. Constrain the 'taskList.name' parameter by using a Condition
-- element with the 'swf:taskList.name' key to allow the action to access only
-- certain task lists.  If the caller does not have sufficient permissions to
-- invoke the action, or the parameter values fall outside the specified
-- constraints, the action fails. The associated event attribute's cause
-- parameter will be set to OPERATION_NOT_PERMITTED. For details and example IAM
-- policies, see <http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html Using IAM to Manage Access to Amazon SWF Workflows>.
--
-- <http://docs.aws.amazon.com/amazonswf/latest/apireference/API_CountPendingActivityTasks.html>
module Network.AWS.SWF.CountPendingActivityTasks
    (
    -- * Request
      CountPendingActivityTasks
    -- ** Request constructor
    , countPendingActivityTasks
    -- ** Request lenses
    , cpatDomain
    , cpatTaskList

    -- * Response
    , CountPendingActivityTasksResponse
    -- ** Response constructor
    , countPendingActivityTasksResponse
    -- ** Response lenses
    , cpatrCount
    , cpatrTruncated
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.SWF.Types
import qualified GHC.Exts

data CountPendingActivityTasks = CountPendingActivityTasks
    { _cpatDomain   :: Text
    , _cpatTaskList :: TaskList
    } deriving (Eq, Show)

-- | 'CountPendingActivityTasks' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cpatDomain' @::@ 'Text'
--
-- * 'cpatTaskList' @::@ 'TaskList'
--
countPendingActivityTasks :: Text -- ^ 'cpatDomain'
                          -> TaskList -- ^ 'cpatTaskList'
                          -> CountPendingActivityTasks
countPendingActivityTasks p1 p2 = CountPendingActivityTasks
    { _cpatDomain   = p1
    , _cpatTaskList = p2
    }

-- | The name of the domain that contains the task list.
cpatDomain :: Lens' CountPendingActivityTasks Text
cpatDomain = lens _cpatDomain (\s a -> s { _cpatDomain = a })

-- | The name of the task list.
cpatTaskList :: Lens' CountPendingActivityTasks TaskList
cpatTaskList = lens _cpatTaskList (\s a -> s { _cpatTaskList = a })

data CountPendingActivityTasksResponse = CountPendingActivityTasksResponse
    { _cpatrCount     :: Nat
    , _cpatrTruncated :: Maybe Bool
    } deriving (Eq, Ord, Show)

-- | 'CountPendingActivityTasksResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cpatrCount' @::@ 'Natural'
--
-- * 'cpatrTruncated' @::@ 'Maybe' 'Bool'
--
countPendingActivityTasksResponse :: Natural -- ^ 'cpatrCount'
                                  -> CountPendingActivityTasksResponse
countPendingActivityTasksResponse p1 = CountPendingActivityTasksResponse
    { _cpatrCount     = withIso _Nat (const id) p1
    , _cpatrTruncated = Nothing
    }

-- | The number of tasks in the task list.
cpatrCount :: Lens' CountPendingActivityTasksResponse Natural
cpatrCount = lens _cpatrCount (\s a -> s { _cpatrCount = a }) . _Nat

-- | If set to true, indicates that the actual count was more than the maximum
-- supported by this API and the count returned is the truncated value.
cpatrTruncated :: Lens' CountPendingActivityTasksResponse (Maybe Bool)
cpatrTruncated = lens _cpatrTruncated (\s a -> s { _cpatrTruncated = a })

instance ToPath CountPendingActivityTasks where
    toPath = const "/"

instance ToQuery CountPendingActivityTasks where
    toQuery = const mempty

instance ToHeaders CountPendingActivityTasks

instance ToJSON CountPendingActivityTasks where
    toJSON CountPendingActivityTasks{..} = object
        [ "domain"   .= _cpatDomain
        , "taskList" .= _cpatTaskList
        ]

instance AWSRequest CountPendingActivityTasks where
    type Sv CountPendingActivityTasks = SWF
    type Rs CountPendingActivityTasks = CountPendingActivityTasksResponse

    request  = post "CountPendingActivityTasks"
    response = jsonResponse

instance FromJSON CountPendingActivityTasksResponse where
    parseJSON = withObject "CountPendingActivityTasksResponse" $ \o -> CountPendingActivityTasksResponse
        <$> o .:  "count"
        <*> o .:? "truncated"
