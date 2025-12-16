{-# LANGUAGE OverloadedStrings #-}

module Models.FreeAccess
    ( FreeAccess(..)
    ) where

import Database.MySQL.Base
import Database.MySQL.Protocol.MySQLValue
import Database.MySQL.Protocol.Packet (OK(..))
import qualified System.IO.Streams as Streams
import Models.Types
import Data.Time (Day, TimeOfDay)
import Data.Maybe (mapMaybe)

-- | Запис вільного доступу
data FreeAccess = FreeAccess
    { accessId      :: Maybe Int
    , labId         :: Int
    , userId        :: Int
    , workstationId :: Int
    , accessDate    :: Day
    , startTime     :: TimeOfDay
    , endTime       :: TimeOfDay
    } deriving (Show, Eq)

-- | Instance для DatabaseEntity
instance DatabaseEntity FreeAccess where
    tableName _ = "free_access"

    selectAll conn = do
        (_, stream) <- query_ conn
            "SELECT access_id, lab_id, user_id, workstation_id, date, start_time, end_time FROM free_access ORDER BY date DESC, start_time"
        rows <- Streams.toList stream
        return $ mapMaybe fromMySQLValues rows

    selectById conn aid = do
        (_, stream) <- query conn
            "SELECT access_id, lab_id, user_id, workstation_id, date, start_time, end_time FROM free_access WHERE access_id = ?"
            [MySQLInt32 $ fromIntegral aid]
        rows <- Streams.toList stream
        case rows of
            (r:_) -> return $ fromMySQLValues r
            []    -> return Nothing

    fromMySQLValues [aid, lid, uid, wid, date, stime, etime] =
        case (aid, lid, uid, wid, date, stime, etime) of
            (MySQLInt32 aid', MySQLInt32 lid', MySQLInt32 uid', MySQLInt32 wid', MySQLDate date', MySQLTime _ stime', MySQLTime _ etime') ->
                Just $ FreeAccess (Just $ fromIntegral aid') (fromIntegral lid') (fromIntegral uid') (fromIntegral wid') date' stime' etime'
            _ -> Nothing
    fromMySQLValues _ = Nothing

-- | Instance для CrudOperations
instance CrudOperations FreeAccess where
    insert conn fa = do
        ok <- execute conn
            "INSERT INTO free_access (lab_id, user_id, workstation_id, date, start_time, end_time) VALUES (?, ?, ?, ?, ?, ?)"
            [MySQLInt32 $ fromIntegral (labId fa), MySQLInt32 $ fromIntegral (userId fa), MySQLInt32 $ fromIntegral (workstationId fa), MySQLDate (accessDate fa), MySQLTime 0 (startTime fa), MySQLTime 0 (endTime fa)]
        return $ fromIntegral $ okLastInsertID ok

    update conn fa = case accessId fa of
        Just aid -> do
            _ <- execute conn
                "UPDATE free_access SET lab_id = ?, user_id = ?, workstation_id = ?, date = ?, start_time = ?, end_time = ? WHERE access_id = ?"
                [MySQLInt32 $ fromIntegral (labId fa), MySQLInt32 $ fromIntegral (userId fa), MySQLInt32 $ fromIntegral (workstationId fa), MySQLDate (accessDate fa), MySQLTime 0 (startTime fa), MySQLTime 0 (endTime fa), MySQLInt32 $ fromIntegral aid]
            return ()
        Nothing -> putStrLn "Помилка: немає ID для оновлення"

    delete conn aid = do
        _ <- execute conn "DELETE FROM free_access WHERE access_id = ?" [MySQLInt32 $ fromIntegral aid]
        return ()

    display fa = do
        putStrLn $ "ID: " ++ show (accessId fa)
        putStrLn $ "ID класу: " ++ show (labId fa)
        putStrLn $ "ID користувача: " ++ show (userId fa)
        putStrLn $ "ID робочого місця: " ++ show (workstationId fa)
        putStrLn $ "Дата: " ++ show (accessDate fa)
        putStrLn $ "Час початку: " ++ show (startTime fa)
        putStrLn $ "Час закінчення: " ++ show (endTime fa)
        putStrLn "---"

    toMySQLValues fa =
        [MySQLInt32 $ fromIntegral (labId fa), MySQLInt32 $ fromIntegral (userId fa), MySQLInt32 $ fromIntegral (workstationId fa), MySQLDate (accessDate fa), MySQLTime 0 (startTime fa), MySQLTime 0 (endTime fa)]
