import Database.HaskellDB
import Database.HaskellDB.HDBC
import Database.HaskellDB.Sql.Default
import Database.HaskellDB.Database
import Database.HDBC.MySQL

import DBInfo.Posts

cdchawthSQLConnectInfo :: MySQLConnectInfo
cdchawthSQLConnectInfo = defaultMySQLConnectInfo
    { mysqlDatabase = "cdchawth"
    , mysqlUser = "cdchawth"
    , mysqlPassword = "TP5UdOzR4PmdVvUu1fxc"
    }

withDB :: (Database -> IO ()) -> IO ()
--withDB :: (MonadIO m0) => (Database -> m0 ()) -> m0 ()
withDB = hdbcConnect defaultSqlGenerator (connectMySQL cdchawthSQLConnectInfo)


printAndPerformQuery :: (Show vr, GetRec er vr) => Query (Rel er) -> Database -> IO ()
printAndPerformQuery q db =
    do putStrLn "Query:"
       print q
       
       result <- query db q
       putStrLn "Results:"
       mapM_ print result


foo :: Database -> IO ()
foo db = 
    do
        results <- query db $ table posts
        putStrLn "Results:"
        let ids = map (!xid) results
        mapM_ print ids
