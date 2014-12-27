-- Module      : DynamoDB
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module DynamoDB where

import           Control.Applicative
import           Control.Lens
import           Control.Monad.Trans.AWS
import           Data.Monoid
import           Data.Text (Text)
import           Data.Time.Clock.POSIX
import           Network.AWS.Data
import           Network.AWS.DynamoDB
import           Network.AWS.DynamoDB.Types
import           Network.AWS.Prelude
import           System.IO
import qualified Data.ByteString.Builder as Build
import qualified Data.Text as Text
import qualified System.Environment as Env

tableName :: Text
tableName = "AmazonikaTestTable"

throughput :: ProvisionedThroughput
throughput = provisionedThroughput 1 1

subjectSchema :: NonEmpty KeySchemaElement
subjectSchema = keySchemaElement "ForumName" Hash :| [keySchemaElement "Subject" Range]

lastPostSchema :: NonEmpty KeySchemaElement
lastPostSchema = keySchemaElement "ForumName" Hash :| [keySchemaElement "LastPostDateTime" Range]

firstPostSchema :: NonEmpty KeySchemaElement
firstPostSchema = keySchemaElement "ForumName" Hash :| [keySchemaElement "FirstPostDateTime" Range]

attributeDefs :: [AttributeDefinition]
attributeDefs = [ attributeDefinition "ForumName" S
                , attributeDefinition "Subject" S
                , attributeDefinition "LastPostDateTime" S
                , attributeDefinition "FirstPostDateTime" S
                ]

firstPostProj :: Projection
firstPostProj = (projection $ "FirstPostDateTime" :| []) & pProjectionType .~ Just Include

lastPostProj :: Projection
lastPostProj = (projection $ "LastPostDateTime" :| []) & pProjectionType .~ Just Include

localIndex :: [LocalSecondaryIndex]
localIndex = [localSecondaryIndex "LastPostIndex" lastPostSchema firstPostProj]

globalIndex :: [GlobalSecondaryIndex]
globalIndex = [globalSecondaryIndex "FirstPostIndex" firstPostSchema lastPostProj throughput]

example :: IO (Either Error ())
example = do
    lgr <- newLogger Trace stdout
    reg <- Env.getEnv "AWS_DEFAULT_REGION"
    region <- either fail return (fromText $ Text.pack reg)
    env <- getEnv region (FromEnv "AWS_ACCESS_KEY_ID" "AWS_SECRET_ACCESS_KEY") <&> envLogger .~ lgr
    runAWST env $ do
         say "Creating DynamoDB table" tableName
         -- _ <- send $ describeTable tableName
         -- r <- send $ createTable tableName subjectSchema throughput
         --   & ctAttributeDefinitions .~ attributeDefs
         --   & ctLocalSecondaryIndexes .~ localIndex
         --   & ctGlobalSecondaryIndexes .~ globalIndex
         -- say "Describing DynamoDB table" tableName
         -- r <- send $ describeTable tableName
         say "Deleting DynamoDB table" tableName
         r <- send $ deleteTable tableName
         return ()

example1 :: IO (Either Error ())
example1 = do
    lgr <- newLogger Trace stdout
    env <- getEnv Oregon (FromEnv "AWS_ACCESS_KEY_ID" "AWS_SECRET_ACCESS_KEY") <&> envLogger .~ lgr
    let tableName = "AmazonikaTestTable"
        throughput = provisionedThroughput 1 1
        schema = keySchemaElement "ForumName" Hash :| [keySchemaElement "Subject" Range]
        schema2 = keySchemaElement "ForumName" Hash :| [keySchemaElement "LastPostDateTime" Range]
        schema3 = keySchemaElement "ForumName" Hash :| [keySchemaElement "FirstPostDateTime" Range]
        attributeDefs = [ attributeDefinition "ForumName" S
                        , attributeDefinition "Subject" S
                        , attributeDefinition "LastPostDateTime" S
                        , attributeDefinition "FirstPostDateTime" S
                        ]
        proj = (projection $ "FirstPostDateTime" :| []) & pProjectionType .~ Just Include
        proj2 = (projection $ "LastPostDateTime" :| []) & pProjectionType .~ Just Include
    runAWST env $ do
         r <- send $ createTable tableName schema throughput
           & ctAttributeDefinitions .~ attributeDefs
           & ctLocalSecondaryIndexes .~ [localSecondaryIndex "LastPostIndex" schema2 proj ]
           -- & ctGlobalSecondaryIndexes .~ [globalSecondaryIndex "FirstPostIndex" schema3 proj2 throughput ]
         return ()

-- Real world usage.
-- Idempotency of actions.
-- Cleanup and teardown of created resources.
-- Region agnostic .
-- Avoid or mitigate related AWS costs.

-- example :: IO (Either Error CreateTableResponse)
-- example = do
--     lgr <- newLogger Debug stdout
--     env <- getEnv NorthVirginia Discover <&> envLogger .~ lgr
--     ts  <- Text.pack . show <$> getTimestamp
--     let tableName = "AmazonikaTestTable"
--         throughput = provisionedThroughput 1 1
--         schema = keySchemaElement
--     runAWST env $ do
--         say "Creating DynamoDB table " tableName
--         response <- send $ createTable tableName schema throughput
--         say "Listing DynamoDB tables " g
--         response <- send $ listTables
--         say "Describing DynamoDB table " g
--         response <- send $ describeTable
--         say "Authorizing SSH on SecurityGroup " g
--         response <- send $ getItem
--         say "Authorizing SSH on SecurityGroup " g
--         response <- send $ putItem
--         say "Authorizing SSH on SecurityGroup " g
--         response <- send $ getItem
--         say "Authorizing SSH on SecurityGroup " g
--         response <- send $ deleteItem
--         say "Authorizing SSH on SecurityGroup " g
--         response <- send $ batchWriteItem
--         say "Authorizing SSH on SecurityGroup " g
--         response <- send $ batchGetItem
--         say "Authorizing SSH on SecurityGroup " g
--         response <- send $ updateItem
--         say "Authorizing SSH on SecurityGroup " g
--         response <- send $ updateTable
--         say "Authorizing SSH on SecurityGroup " g
--         response <- send $ query
--         say "Authorizing SSH on SecurityGroup " g
--         response <- send $ scan
--         say "Authorizing SSH on SecurityGroup " g
--         response <- send $ deleteTable

say :: Show a => Build.Builder -> a -> AWST IO ()
say msg = info . mappend msg . Build.stringUtf8 . show

getTimestamp :: IO Integer
getTimestamp = truncate <$> getPOSIXTime
