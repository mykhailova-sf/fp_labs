{-# LANGUAGE OverloadedStrings #-}

module Models.Workstation
    ( Workstation(..)
    ) where

import Database.MySQL.Base
import Database.MySQL.Protocol.MySQLValue
import Database.MySQL.Protocol.Packet (OK(..))
import qualified System.IO.Streams as Streams
import Models.Types
import Data.Maybe (mapMaybe)

-- | Робоче місце (комп'ютер)
data Workstation = Workstation
    { workstationId     :: Maybe Int
    , labId             :: Int
    , workstationNumber :: Int
    , isWorking         :: Bool
    } deriving (Show, Eq)

-- | Instance для DatabaseEntity
instance DatabaseEntity Workstation where
    tableName _ = "workstations"

    selectAll conn = do
        (_, stream) <- query_ conn
            "SELECT workstation_id, lab_id, workstation_number, is_working FROM workstations ORDER BY lab_id, workstation_number"
        rows <- Streams.toList stream
        return $ mapMaybe fromMySQLValues rows

    selectById conn wid = do
        (_, stream) <- query conn
            "SELECT workstation_id, lab_id, workstation_number, is_working FROM workstations WHERE workstation_id = ?"
            [MySQLInt32 $ fromIntegral wid]
        rows <- Streams.toList stream
        case rows of
            (r:_) -> return $ fromMySQLValues r
            []    -> return Nothing

    fromMySQLValues [wid, lid, wnum, working] =
        case (wid, lid, wnum, working) of
            (MySQLInt32 wid', MySQLInt32 lid', MySQLInt32 wnum', MySQLInt8 working') ->
                Just $ Workstation (Just $ fromIntegral wid') (fromIntegral lid') (fromIntegral wnum') (working' /= 0)
            _ -> Nothing
    fromMySQLValues _ = Nothing

-- | Instance для CrudOperations
instance CrudOperations Workstation where
    insert conn ws = do
        ok <- execute conn
            "INSERT INTO workstations (lab_id, workstation_number, is_working) VALUES (?, ?, ?)"
            [MySQLInt32 $ fromIntegral (labId ws), MySQLInt32 $ fromIntegral (workstationNumber ws), MySQLInt8 $ if isWorking ws then 1 else 0]
        return $ fromIntegral $ okLastInsertID ok

    update conn ws = case workstationId ws of
        Just wid -> do
            _ <- execute conn
                "UPDATE workstations SET lab_id = ?, workstation_number = ?, is_working = ? WHERE workstation_id = ?"
                [MySQLInt32 $ fromIntegral (labId ws), MySQLInt32 $ fromIntegral (workstationNumber ws), MySQLInt8 $ if isWorking ws then 1 else 0, MySQLInt32 $ fromIntegral wid]
            return ()
        Nothing -> putStrLn "Помилка: немає ID для оновлення"

    delete conn wid = do
        _ <- execute conn "DELETE FROM workstations WHERE workstation_id = ?" [MySQLInt32 $ fromIntegral wid]
        return ()

    display ws = do
        putStrLn $ "ID: " ++ show (workstationId ws)
        putStrLn $ "ID класу: " ++ show (labId ws)
        putStrLn $ "Номер робочого місця: " ++ show (workstationNumber ws)
        putStrLn $ "Статус: " ++ if isWorking ws then "Працює" else "Не працює"
        putStrLn "---"

    toMySQLValues ws =
        [MySQLInt32 $ fromIntegral (labId ws), MySQLInt32 $ fromIntegral (workstationNumber ws), MySQLInt8 $ if isWorking ws then 1 else 0]
