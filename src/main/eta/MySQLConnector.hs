{-# LANGUAGE OverloadedStrings #-}
{-| Connector working in top of library MySQL-Haskell https://github.com/winterland1989/mysql-haskell
    The version of MySQL-Haskell 0.8.3.0 only works properly with MySQL server 5.5-}
module MySQLConnector where

import Database.MySQL.Base
import qualified System.IO.Streams as Streams
import Data.Int
import ModelTypes
import qualified Data.Text as T
import Data.List
import Control.Monad.IO.Class
import System.IO.Streams (InputStream)

selectAllQuery = "SELECT * FROM haskell_users"
selectByIdQuery = "SELECT * FROM haskell_users WHERE userId=(?)"
selectByNameQuery = "SELECT * FROM haskell_users WHERE userName=(?)"
deleteByIdQuery = "DELETE FROM haskell_users WHERE userId=(?)"
insertUserQuery = "INSERT INTO mysql.haskell_users (userId,userName) VALUES(?,?)"
updateUserQuery = "UPDATE mysql.haskell_users SET userId=(?),userName=(?) WHERE userId=(?)"

-- | MySQL CRUD
-- -------------
getAllUsers :: IO [User]
getAllUsers = do
    liftIO $ print ("Preparing connection:")
    conn <- createConnection
    liftIO $ print ("Connection ready to be used:")
    s <- prepareStmt conn selectAllQuery
    (_, inputStream) <- queryStmt conn s [MySQLInt32U 18]
    maybeUsers <- return (Streams.toList inputStream)
    users <- liftIO $ transformMySQLValueArrayToUsers <$> maybeUsers
    return users

{-| Function for select. We select we use [query] operator followed by the connection, query and a QueryParam-}
getUserById :: Int -> IO User
getUserById id = let userId = id in do
            conn <- createConnection
            (columnDef, inputStream) <- querySelectById userId conn
            maybeMySQLValue <- readInputStream inputStream
            return (transformMaybeMySQLValueToUser maybeMySQLValue)

{-| Function for select. We use [query] operator followed by the connection, query and a QueryParam-}
getUserByUserName :: String -> IO User
getUserByUserName _name = let name = _name in do
            conn <- createConnection
            (columnDef, inputStream) <- querySelectByUserName name conn
            maybeMySQLValue <- readInputStream inputStream
            return (transformMaybeMySQLValueToUser maybeMySQLValue)

{-|Function for insert. We use [execute] operator followed by the connection, query and an array of QueryParam-}
insertUser :: User -> IO OK
insertUser _user = let user = _user in do
            conn <- createConnection
            status <- executeCreateQuery user conn
            return status

{-| Function for delete. We use [query] operator followed by the connection, query and a QueryParam-}
deleteUserById :: Int -> IO OK
deleteUserById id = let userId = id in do
              conn <- createConnection
              status <- executeDeleteQuery userId conn
              return status

{-| Function for update. we use [execute] operator followed by the connection,update query and an array of QueryParam with data to update and filter-}
updateUserById :: User -> IO OK
updateUserById _user = let user = _user in do
            conn <- createConnection
            status <- executeUpdateQuery user conn
            return status

{-| Function to  Query the select by id query-}
querySelectById :: Int -> MySQLConn -> IO ([ColumnDef], InputStream [MySQLValue])
querySelectById userId  conn = query conn selectByIdQuery [One $ MySQLInt32 (intToInt32 userId)]

{-| Function to  Query the select by name query-}
querySelectByUserName :: String -> MySQLConn -> IO ([ColumnDef], InputStream [MySQLValue])
querySelectByUserName name  conn = query conn selectByNameQuery [One $ MySQLText (T.pack $ name)]

{-| Function to  Execute the create query-}
executeCreateQuery :: User -> MySQLConn -> IO OK
executeCreateQuery user  conn = execute conn insertUserQuery [MySQLInt32 (intToInt32 $ getUserId user), MySQLText (T.pack $ getUserName user)]

{-| Function to  Execute the delete query-}
executeDeleteQuery :: Int -> MySQLConn -> IO OK
executeDeleteQuery userId  conn = execute conn  deleteByIdQuery [One $ MySQLInt32 (intToInt32 userId)]

{-| Function to  Execute the update query-}
executeUpdateQuery :: User -> MySQLConn -> IO OK
executeUpdateQuery user  conn = execute conn  updateUserQuery [MySQLInt32 (intToInt32 $ getUserId user), MySQLText (T.pack $ getUserName user),MySQLInt32 (intToInt32 $ getUserId user)]

{-| Function to extract the MySQLValue from Maybe and transform into User calling another function-}
transformMaybeMySQLValueToUser :: Maybe [MySQLValue] -> User
transformMaybeMySQLValueToUser maybeMySQLValue = case maybeMySQLValue of
                                            Just mysqlValue -> transformToUser mysqlValue
                                            Nothing -> User 0 "default User"

{-| Function that take an array of [[MySQLValue]] and transform every element into [User]-}
transformMySQLValueArrayToUsers :: [[MySQLValue]] -> [User]
transformMySQLValueArrayToUsers mysqlValues = map (\mysqlValue -> transformToUser mysqlValue) mysqlValues

{-| Function to receive the row [MySQLValue] and we define the fields of the row to be extracted, and after change
    format of types using some utils functions we create the User instance.
    In order to transform from Text to String we just need to use the operator [unpack] to extract the String -}
transformToUser :: [MySQLValue] -> User
transformToUser [MySQLInt32 row_userId, MySQLText row_userName] = User (int32ToInt row_userId) (T.unpack row_userName)

{-| Transform from Int to Int32 format-}
intToInt32 :: Int -> Int32
intToInt32 userId = fromIntegral (userId :: Int) :: Int32

int32ToInt :: Int32 -> Int
int32ToInt userId = fromIntegral (userId :: Int32) :: Int

{-| We use [connect] operator together with [defaultConnectInfo] with the info to connect to the MySQL Server-}
createConnection :: IO MySQLConn
createConnection = connect defaultConnectInfo {ciUser = "root", ciPassword = "root", ciDatabase = "mysql"}

{-| Using mysql-haskell [Streams.read] operator we able to transform the inputStream into Maybe[Type]-}
readInputStream :: InputStream a -> IO (Maybe a)
readInputStream inputStream = Streams.read inputStream