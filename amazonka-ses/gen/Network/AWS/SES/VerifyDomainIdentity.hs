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

-- Module      : Network.AWS.SES.VerifyDomainIdentity
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

-- | Verifies a domain.
--
-- This action is throttled at one request per second.
--
-- <http://docs.aws.amazon.com/ses/latest/APIReference/API_VerifyDomainIdentity.html>
module Network.AWS.SES.VerifyDomainIdentity
    (
    -- * Request
      VerifyDomainIdentity
    -- ** Request constructor
    , verifyDomainIdentity
    -- ** Request lenses
    , vdiDomain

    -- * Response
    , VerifyDomainIdentityResponse
    -- ** Response constructor
    , verifyDomainIdentityResponse
    -- ** Response lenses
    , vdirVerificationToken
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.SES.Types
import qualified GHC.Exts

newtype VerifyDomainIdentity = VerifyDomainIdentity
    { _vdiDomain :: Text
    } deriving (Eq, Ord, Show, Monoid, IsString)

-- | 'VerifyDomainIdentity' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'vdiDomain' @::@ 'Text'
--
verifyDomainIdentity :: Text -- ^ 'vdiDomain'
                     -> VerifyDomainIdentity
verifyDomainIdentity p1 = VerifyDomainIdentity
    { _vdiDomain = p1
    }

-- | The domain to be verified.
vdiDomain :: Lens' VerifyDomainIdentity Text
vdiDomain = lens _vdiDomain (\s a -> s { _vdiDomain = a })

newtype VerifyDomainIdentityResponse = VerifyDomainIdentityResponse
    { _vdirVerificationToken :: Text
    } deriving (Eq, Ord, Show, Monoid, IsString)

-- | 'VerifyDomainIdentityResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'vdirVerificationToken' @::@ 'Text'
--
verifyDomainIdentityResponse :: Text -- ^ 'vdirVerificationToken'
                             -> VerifyDomainIdentityResponse
verifyDomainIdentityResponse p1 = VerifyDomainIdentityResponse
    { _vdirVerificationToken = p1
    }

-- | A TXT record that must be placed in the DNS settings for the domain, in order
-- to complete domain verification.
vdirVerificationToken :: Lens' VerifyDomainIdentityResponse Text
vdirVerificationToken =
    lens _vdirVerificationToken (\s a -> s { _vdirVerificationToken = a })

instance ToPath VerifyDomainIdentity where
    toPath = const "/"

instance ToQuery VerifyDomainIdentity where
    toQuery VerifyDomainIdentity{..} = mconcat
        [ "Domain" =? _vdiDomain
        ]

instance ToHeaders VerifyDomainIdentity

instance AWSRequest VerifyDomainIdentity where
    type Sv VerifyDomainIdentity = SES
    type Rs VerifyDomainIdentity = VerifyDomainIdentityResponse

    request  = post "VerifyDomainIdentity"
    response = xmlResponse

instance FromXML VerifyDomainIdentityResponse where
    parseXML = withElement "VerifyDomainIdentityResult" $ \x -> VerifyDomainIdentityResponse
        <$> x .@  "VerificationToken"
