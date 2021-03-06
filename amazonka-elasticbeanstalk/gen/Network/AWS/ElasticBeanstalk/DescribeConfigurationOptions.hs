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

-- Module      : Network.AWS.ElasticBeanstalk.DescribeConfigurationOptions
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

-- | Describes the configuration options that are used in a particular
-- configuration template or environment, or that a specified solution stack
-- defines. The description includes the values the options, their default
-- values, and an indication of the required action on a running environment if
-- an option value is changed.
--
-- <http://docs.aws.amazon.com/elasticbeanstalk/latest/api/API_DescribeConfigurationOptions.html>
module Network.AWS.ElasticBeanstalk.DescribeConfigurationOptions
    (
    -- * Request
      DescribeConfigurationOptions
    -- ** Request constructor
    , describeConfigurationOptions
    -- ** Request lenses
    , dcoApplicationName
    , dcoEnvironmentName
    , dcoOptions
    , dcoSolutionStackName
    , dcoTemplateName

    -- * Response
    , DescribeConfigurationOptionsResponse
    -- ** Response constructor
    , describeConfigurationOptionsResponse
    -- ** Response lenses
    , dcorOptions
    , dcorSolutionStackName
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.ElasticBeanstalk.Types
import qualified GHC.Exts

data DescribeConfigurationOptions = DescribeConfigurationOptions
    { _dcoApplicationName   :: Maybe Text
    , _dcoEnvironmentName   :: Maybe Text
    , _dcoOptions           :: List "member" OptionSpecification
    , _dcoSolutionStackName :: Maybe Text
    , _dcoTemplateName      :: Maybe Text
    } deriving (Eq, Show)

-- | 'DescribeConfigurationOptions' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dcoApplicationName' @::@ 'Maybe' 'Text'
--
-- * 'dcoEnvironmentName' @::@ 'Maybe' 'Text'
--
-- * 'dcoOptions' @::@ ['OptionSpecification']
--
-- * 'dcoSolutionStackName' @::@ 'Maybe' 'Text'
--
-- * 'dcoTemplateName' @::@ 'Maybe' 'Text'
--
describeConfigurationOptions :: DescribeConfigurationOptions
describeConfigurationOptions = DescribeConfigurationOptions
    { _dcoApplicationName   = Nothing
    , _dcoTemplateName      = Nothing
    , _dcoEnvironmentName   = Nothing
    , _dcoSolutionStackName = Nothing
    , _dcoOptions           = mempty
    }

-- | The name of the application associated with the configuration template or
-- environment. Only needed if you want to describe the configuration options
-- associated with either the configuration template or environment.
dcoApplicationName :: Lens' DescribeConfigurationOptions (Maybe Text)
dcoApplicationName =
    lens _dcoApplicationName (\s a -> s { _dcoApplicationName = a })

-- | The name of the environment whose configuration options you want to
-- describe.
dcoEnvironmentName :: Lens' DescribeConfigurationOptions (Maybe Text)
dcoEnvironmentName =
    lens _dcoEnvironmentName (\s a -> s { _dcoEnvironmentName = a })

-- | If specified, restricts the descriptions to only the specified options.
dcoOptions :: Lens' DescribeConfigurationOptions [OptionSpecification]
dcoOptions = lens _dcoOptions (\s a -> s { _dcoOptions = a }) . _List

-- | The name of the solution stack whose configuration options you want to
-- describe.
dcoSolutionStackName :: Lens' DescribeConfigurationOptions (Maybe Text)
dcoSolutionStackName =
    lens _dcoSolutionStackName (\s a -> s { _dcoSolutionStackName = a })

-- | The name of the configuration template whose configuration options you want
-- to describe.
dcoTemplateName :: Lens' DescribeConfigurationOptions (Maybe Text)
dcoTemplateName = lens _dcoTemplateName (\s a -> s { _dcoTemplateName = a })

data DescribeConfigurationOptionsResponse = DescribeConfigurationOptionsResponse
    { _dcorOptions           :: List "member" ConfigurationOptionDescription
    , _dcorSolutionStackName :: Maybe Text
    } deriving (Eq, Show)

-- | 'DescribeConfigurationOptionsResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dcorOptions' @::@ ['ConfigurationOptionDescription']
--
-- * 'dcorSolutionStackName' @::@ 'Maybe' 'Text'
--
describeConfigurationOptionsResponse :: DescribeConfigurationOptionsResponse
describeConfigurationOptionsResponse = DescribeConfigurationOptionsResponse
    { _dcorSolutionStackName = Nothing
    , _dcorOptions           = mempty
    }

-- | A list of 'ConfigurationOptionDescription'.
dcorOptions :: Lens' DescribeConfigurationOptionsResponse [ConfigurationOptionDescription]
dcorOptions = lens _dcorOptions (\s a -> s { _dcorOptions = a }) . _List

-- | The name of the solution stack these configuration options belong to.
dcorSolutionStackName :: Lens' DescribeConfigurationOptionsResponse (Maybe Text)
dcorSolutionStackName =
    lens _dcorSolutionStackName (\s a -> s { _dcorSolutionStackName = a })

instance ToPath DescribeConfigurationOptions where
    toPath = const "/"

instance ToQuery DescribeConfigurationOptions where
    toQuery DescribeConfigurationOptions{..} = mconcat
        [ "ApplicationName"   =? _dcoApplicationName
        , "EnvironmentName"   =? _dcoEnvironmentName
        , "Options"           =? _dcoOptions
        , "SolutionStackName" =? _dcoSolutionStackName
        , "TemplateName"      =? _dcoTemplateName
        ]

instance ToHeaders DescribeConfigurationOptions

instance AWSRequest DescribeConfigurationOptions where
    type Sv DescribeConfigurationOptions = ElasticBeanstalk
    type Rs DescribeConfigurationOptions = DescribeConfigurationOptionsResponse

    request  = post "DescribeConfigurationOptions"
    response = xmlResponse

instance FromXML DescribeConfigurationOptionsResponse where
    parseXML = withElement "DescribeConfigurationOptionsResult" $ \x -> DescribeConfigurationOptionsResponse
        <$> x .@? "Options" .!@ mempty
        <*> x .@? "SolutionStackName"
