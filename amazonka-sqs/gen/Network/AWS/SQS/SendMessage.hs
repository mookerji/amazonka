{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SQS.SendMessage
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Delivers a message to the specified queue. With Amazon SQS, you now have
-- the ability to send large payload messages that are up to 256KB (262,144
-- bytes) in size. To send large payloads, you must use an AWS SDK that
-- supports SigV4 signing. To verify whether SigV4 is supported for an AWS
-- SDK, check the SDK release notes. The following list shows the characters
-- (in Unicode) allowed in your message, according to the W3C XML
-- specification. For more information, go to
-- http://www.w3.org/TR/REC-xml/#charsets If you send any characters not
-- included in the list, your request will be rejected. #x9 | #xA | #xD |
-- [#x20 to #xD7FF] | [#xE000 to #xFFFD] | [#x10000 to #x10FFFF] The following
-- example SendMessage request sends a message containing "This is a test
-- message" to the queue. You must URL encode the entire URL; however, we've
-- URL encoded only the message body to make the example easier for you to
-- read. The following example response includes the MD5 digest for "This is a
-- test message". http://sqs.us-east-1.amazonaws.com/123456789012/testQueue/
-- ?Action=SendMessage &MessageBody=This+is+a+test+message
-- &MessageAttribute.1.Name=test_attribute_name_1
-- &MessageAttribute.1.Value.StringValue=test_attribute_value_1
-- &MessageAttribute.1.Value.DataType=String
-- &MessageAttribute.2.Name=test_attribute_name_2
-- &MessageAttribute.2.Value.StringValue=test_attribute_value_2
-- &MessageAttribute.2.Value.DataType=String &Version=2012-11-05
-- &SignatureMethod=HmacSHA256 &Expires=2014-05-05T22%3A52%3A43PST
-- &AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE &SignatureVersion=2
-- &Signature=Dqlp3Sd6ljTUA9Uf6SGtEExwUQEXAMPLE
-- fafb00f5732ab283681e124bf8747ed1 3ae8f24a165a8cedc005670c81a27295
-- 5fea7756-0ea4-451a-a703-a558b933e274 27daac76-34dd-47df-bd01-1f6e873584a0.
module Network.AWS.SQS.SendMessage
    (
    -- * Request
      SendMessage
    -- ** Request constructor
    , sendMessage
    -- ** Request lenses
    , smQueueUrl
    , smMessageBody
    , smDelaySeconds
    , smMessageAttributes

    -- * Response
    , SendMessageResponse
    -- ** Response constructor
    , sendMessageResponse
    -- ** Response lenses
    , smrMD5OfMessageBody
    , smrMD5OfMessageAttributes
    , smrMessageId
    ) where

import Network.AWS.Request.Query
import Network.AWS.SQS.Types
import Network.AWS.Prelude

data SendMessage = SendMessage
    { _smQueueUrl :: Text
    , _smMessageBody :: Text
    , _smDelaySeconds :: Maybe Integer
    , _smMessageAttributes :: Map Text MessageAttributeValue
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'SendMessage' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @QueueUrl ::@ @Text@
--
-- * @MessageBody ::@ @Text@
--
-- * @DelaySeconds ::@ @Maybe Integer@
--
-- * @MessageAttributes ::@ @Map Text MessageAttributeValue@
--
sendMessage :: Text -- ^ 'smQueueUrl'
            -> Text -- ^ 'smMessageBody'
            -> SendMessage
sendMessage p1 p2 = SendMessage
    { _smQueueUrl = p1
    , _smMessageBody = p2
    , _smDelaySeconds = Nothing
    , _smMessageAttributes = mempty
    }

-- | The URL of the Amazon SQS queue to take action on.
smQueueUrl :: Lens' SendMessage Text
smQueueUrl = lens _smQueueUrl (\s a -> s { _smQueueUrl = a })

-- | The message to send. String maximum 256 KB in size. For a list of allowed
-- characters, see the preceding important note.
smMessageBody :: Lens' SendMessage Text
smMessageBody = lens _smMessageBody (\s a -> s { _smMessageBody = a })

-- | The number of seconds (0 to 900 - 15 minutes) to delay a specific message.
-- Messages with a positive DelaySeconds value become available for processing
-- after the delay time is finished. If you don't specify a value, the default
-- value for the queue applies.
smDelaySeconds :: Lens' SendMessage (Maybe Integer)
smDelaySeconds = lens _smDelaySeconds (\s a -> s { _smDelaySeconds = a })

-- | Each message attribute consists of a Name, Type, and Value. For more
-- information, see Message Attribute Items.
smMessageAttributes :: Lens' SendMessage (Map Text MessageAttributeValue)
smMessageAttributes =
    lens _smMessageAttributes (\s a -> s { _smMessageAttributes = a })

instance ToQuery SendMessage where
    toQuery = genericQuery def

-- | The MD5OfMessageBody and MessageId elements.
data SendMessageResponse = SendMessageResponse
    { _smrMD5OfMessageBody :: Maybe Text
    , _smrMD5OfMessageAttributes :: Maybe Text
    , _smrMessageId :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'SendMessageResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @MD5OfMessageBody ::@ @Maybe Text@
--
-- * @MD5OfMessageAttributes ::@ @Maybe Text@
--
-- * @MessageId ::@ @Maybe Text@
--
sendMessageResponse :: SendMessageResponse
sendMessageResponse = SendMessageResponse
    { _smrMD5OfMessageBody = Nothing
    , _smrMD5OfMessageAttributes = Nothing
    , _smrMessageId = Nothing
    }

-- | An MD5 digest of the non-URL-encoded message body string. This can be used
-- to verify that Amazon SQS received the message correctly. Amazon SQS first
-- URL decodes the message before creating the MD5 digest. For information
-- about MD5, go to http://www.faqs.org/rfcs/rfc1321.html.
smrMD5OfMessageBody :: Lens' SendMessageResponse (Maybe Text)
smrMD5OfMessageBody =
    lens _smrMD5OfMessageBody (\s a -> s { _smrMD5OfMessageBody = a })

-- | An MD5 digest of the non-URL-encoded message attribute string. This can be
-- used to verify that Amazon SQS received the message correctly. Amazon SQS
-- first URL decodes the message before creating the MD5 digest. For
-- information about MD5, go to http://www.faqs.org/rfcs/rfc1321.html.
smrMD5OfMessageAttributes :: Lens' SendMessageResponse (Maybe Text)
smrMD5OfMessageAttributes =
    lens _smrMD5OfMessageAttributes
         (\s a -> s { _smrMD5OfMessageAttributes = a })

-- | An element containing the message ID of the message sent to the queue. For
-- more information, see Queue and Message Identifiers in the Amazon SQS
-- Developer Guide.
smrMessageId :: Lens' SendMessageResponse (Maybe Text)
smrMessageId = lens _smrMessageId (\s a -> s { _smrMessageId = a })

instance FromXML SendMessageResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest SendMessage where
    type Sv SendMessage = SQS
    type Rs SendMessage = SendMessageResponse

    request = post "SendMessage"
    response _ = xmlResponse
