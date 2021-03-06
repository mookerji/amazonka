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

-- Module      : Network.AWS.Route53Domains.UpdateDomainContactPrivacy
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

-- | This operation updates the specified domain contact's privacy setting. When
-- the privacy option is enabled, personal information such as postal or email
-- address is hidden from the results of a public WHOIS query. The privacy
-- services are provided by the AWS registrar, Gandi. For more information, see
-- the <http://www.gandi.net/domain/whois/?currency=USD&amp;lang=en Gandi privacy features>.
--
-- This operation only affects the privacy of the specified contact type
-- (registrant, administrator, or tech). Successful acceptance returns an
-- operation ID that you can use with GetOperationDetail to track the progress
-- and completion of the action. If the request is not completed successfully,
-- the domain registrant will be notified by email.
--
-- <http://docs.aws.amazon.com/Route53/latest/APIReference/api-UpdateDomainContactPrivacy.html>
module Network.AWS.Route53Domains.UpdateDomainContactPrivacy
    (
    -- * Request
      UpdateDomainContactPrivacy
    -- ** Request constructor
    , updateDomainContactPrivacy
    -- ** Request lenses
    , udcpAdminPrivacy
    , udcpDomainName
    , udcpRegistrantPrivacy
    , udcpTechPrivacy

    -- * Response
    , UpdateDomainContactPrivacyResponse
    -- ** Response constructor
    , updateDomainContactPrivacyResponse
    -- ** Response lenses
    , udcprOperationId
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.Route53Domains.Types
import qualified GHC.Exts

data UpdateDomainContactPrivacy = UpdateDomainContactPrivacy
    { _udcpAdminPrivacy      :: Maybe Bool
    , _udcpDomainName        :: Text
    , _udcpRegistrantPrivacy :: Maybe Bool
    , _udcpTechPrivacy       :: Maybe Bool
    } deriving (Eq, Ord, Show)

-- | 'UpdateDomainContactPrivacy' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'udcpAdminPrivacy' @::@ 'Maybe' 'Bool'
--
-- * 'udcpDomainName' @::@ 'Text'
--
-- * 'udcpRegistrantPrivacy' @::@ 'Maybe' 'Bool'
--
-- * 'udcpTechPrivacy' @::@ 'Maybe' 'Bool'
--
updateDomainContactPrivacy :: Text -- ^ 'udcpDomainName'
                           -> UpdateDomainContactPrivacy
updateDomainContactPrivacy p1 = UpdateDomainContactPrivacy
    { _udcpDomainName        = p1
    , _udcpAdminPrivacy      = Nothing
    , _udcpRegistrantPrivacy = Nothing
    , _udcpTechPrivacy       = Nothing
    }

-- | Whether you want to conceal contact information from WHOIS queries. If you
-- specify true, WHOIS ("who is") queries will return contact information for
-- our registrar partner, Gandi, instead of the contact information that you
-- enter.
--
-- Type: Boolean
--
-- Default: None
--
-- Valid values: 'true' | 'false'
--
-- Required: No
udcpAdminPrivacy :: Lens' UpdateDomainContactPrivacy (Maybe Bool)
udcpAdminPrivacy = lens _udcpAdminPrivacy (\s a -> s { _udcpAdminPrivacy = a })

-- | The name of a domain.
--
-- Type: String
--
-- Default: None
--
-- Constraints: The domain name can contain only the letters a through z, the
-- numbers 0 through 9, and hyphen (-). Internationalized Domain Names are not
-- supported.
--
-- Required: Yes
udcpDomainName :: Lens' UpdateDomainContactPrivacy Text
udcpDomainName = lens _udcpDomainName (\s a -> s { _udcpDomainName = a })

-- | Whether you want to conceal contact information from WHOIS queries. If you
-- specify true, WHOIS ("who is") queries will return contact information for
-- our registrar partner, Gandi, instead of the contact information that you
-- enter.
--
-- Type: Boolean
--
-- Default: None
--
-- Valid values: 'true' | 'false'
--
-- Required: No
udcpRegistrantPrivacy :: Lens' UpdateDomainContactPrivacy (Maybe Bool)
udcpRegistrantPrivacy =
    lens _udcpRegistrantPrivacy (\s a -> s { _udcpRegistrantPrivacy = a })

-- | Whether you want to conceal contact information from WHOIS queries. If you
-- specify true, WHOIS ("who is") queries will return contact information for
-- our registrar partner, Gandi, instead of the contact information that you
-- enter.
--
-- Type: Boolean
--
-- Default: None
--
-- Valid values: 'true' | 'false'
--
-- Required: No
udcpTechPrivacy :: Lens' UpdateDomainContactPrivacy (Maybe Bool)
udcpTechPrivacy = lens _udcpTechPrivacy (\s a -> s { _udcpTechPrivacy = a })

newtype UpdateDomainContactPrivacyResponse = UpdateDomainContactPrivacyResponse
    { _udcprOperationId :: Text
    } deriving (Eq, Ord, Show, Monoid, IsString)

-- | 'UpdateDomainContactPrivacyResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'udcprOperationId' @::@ 'Text'
--
updateDomainContactPrivacyResponse :: Text -- ^ 'udcprOperationId'
                                   -> UpdateDomainContactPrivacyResponse
updateDomainContactPrivacyResponse p1 = UpdateDomainContactPrivacyResponse
    { _udcprOperationId = p1
    }

-- | Identifier for tracking the progress of the request. To use this ID to query
-- the operation status, use GetOperationDetail.
--
-- Type: String
--
-- Default: None
--
-- Constraints: Maximum 255 characters.
udcprOperationId :: Lens' UpdateDomainContactPrivacyResponse Text
udcprOperationId = lens _udcprOperationId (\s a -> s { _udcprOperationId = a })

instance ToPath UpdateDomainContactPrivacy where
    toPath = const "/"

instance ToQuery UpdateDomainContactPrivacy where
    toQuery = const mempty

instance ToHeaders UpdateDomainContactPrivacy

instance ToJSON UpdateDomainContactPrivacy where
    toJSON UpdateDomainContactPrivacy{..} = object
        [ "DomainName"        .= _udcpDomainName
        , "AdminPrivacy"      .= _udcpAdminPrivacy
        , "RegistrantPrivacy" .= _udcpRegistrantPrivacy
        , "TechPrivacy"       .= _udcpTechPrivacy
        ]

instance AWSRequest UpdateDomainContactPrivacy where
    type Sv UpdateDomainContactPrivacy = Route53Domains
    type Rs UpdateDomainContactPrivacy = UpdateDomainContactPrivacyResponse

    request  = post "UpdateDomainContactPrivacy"
    response = jsonResponse

instance FromJSON UpdateDomainContactPrivacyResponse where
    parseJSON = withObject "UpdateDomainContactPrivacyResponse" $ \o -> UpdateDomainContactPrivacyResponse
        <$> o .:  "OperationId"
