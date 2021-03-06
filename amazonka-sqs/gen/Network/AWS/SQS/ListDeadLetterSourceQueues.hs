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

-- Module      : Network.AWS.SQS.ListDeadLetterSourceQueues
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

-- | Returns a list of your queues that have the RedrivePolicy queue attribute
-- configured with a dead letter queue.
--
-- For more information about using dead letter queues, see <http://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/SQSDeadLetterQueue.html Using Amazon SQSDead Letter Queues>.
--
-- <http://docs.aws.amazon.com/AWSSimpleQueueService/latest/APIReference/API_ListDeadLetterSourceQueues.html>
module Network.AWS.SQS.ListDeadLetterSourceQueues
    (
    -- * Request
      ListDeadLetterSourceQueues
    -- ** Request constructor
    , listDeadLetterSourceQueues
    -- ** Request lenses
    , ldlsqQueueUrl

    -- * Response
    , ListDeadLetterSourceQueuesResponse
    -- ** Response constructor
    , listDeadLetterSourceQueuesResponse
    -- ** Response lenses
    , ldlsqrQueueUrls
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.SQS.Types
import qualified GHC.Exts

newtype ListDeadLetterSourceQueues = ListDeadLetterSourceQueues
    { _ldlsqQueueUrl :: Text
    } deriving (Eq, Ord, Show, Monoid, IsString)

-- | 'ListDeadLetterSourceQueues' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ldlsqQueueUrl' @::@ 'Text'
--
listDeadLetterSourceQueues :: Text -- ^ 'ldlsqQueueUrl'
                           -> ListDeadLetterSourceQueues
listDeadLetterSourceQueues p1 = ListDeadLetterSourceQueues
    { _ldlsqQueueUrl = p1
    }

-- | The queue URL of a dead letter queue.
ldlsqQueueUrl :: Lens' ListDeadLetterSourceQueues Text
ldlsqQueueUrl = lens _ldlsqQueueUrl (\s a -> s { _ldlsqQueueUrl = a })

newtype ListDeadLetterSourceQueuesResponse = ListDeadLetterSourceQueuesResponse
    { _ldlsqrQueueUrls :: List "member" Text
    } deriving (Eq, Ord, Show, Monoid, Semigroup)

-- | 'ListDeadLetterSourceQueuesResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ldlsqrQueueUrls' @::@ ['Text']
--
listDeadLetterSourceQueuesResponse :: ListDeadLetterSourceQueuesResponse
listDeadLetterSourceQueuesResponse = ListDeadLetterSourceQueuesResponse
    { _ldlsqrQueueUrls = mempty
    }

-- | A list of source queue URLs that have the RedrivePolicy queue attribute
-- configured with a dead letter queue.
ldlsqrQueueUrls :: Lens' ListDeadLetterSourceQueuesResponse [Text]
ldlsqrQueueUrls = lens _ldlsqrQueueUrls (\s a -> s { _ldlsqrQueueUrls = a }) . _List

instance ToPath ListDeadLetterSourceQueues where
    toPath = const "/"

instance ToQuery ListDeadLetterSourceQueues where
    toQuery ListDeadLetterSourceQueues{..} = mconcat
        [ "QueueUrl" =? _ldlsqQueueUrl
        ]

instance ToHeaders ListDeadLetterSourceQueues

instance AWSRequest ListDeadLetterSourceQueues where
    type Sv ListDeadLetterSourceQueues = SQS
    type Rs ListDeadLetterSourceQueues = ListDeadLetterSourceQueuesResponse

    request  = post "ListDeadLetterSourceQueues"
    response = xmlResponse

instance FromXML ListDeadLetterSourceQueuesResponse where
    parseXML = withElement "ListDeadLetterSourceQueuesResult" $ \x -> ListDeadLetterSourceQueuesResponse
        <$> parseXML x
