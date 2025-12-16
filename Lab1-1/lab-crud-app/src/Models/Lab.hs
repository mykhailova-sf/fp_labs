{-# LANGUAGE OverloadedStrings #-}

module Models.Lab
    ( Lab(..)
    ) where

import Database.MySQL.Base
import Database.MySQL.Protocol.MySQLValue
import Database.MySQL.Protocol.Packet (OK(..))
import qualified System.IO.Streams as Streams
import Models.Types
import Data.Text (Text)
import qualified Data.Text as T
import Data.Maybe (mapMaybe)

-- | Дисплейний клас
data Lab = Lab
    { labId              :: Maybe Int
    , labName            :: Text
    , building           :: Text
    , roomNumber         :: Text
    , totalWorkstations  :: Int
    } deriving (Show, Eq)

-- | Instance для DatabaseEntity
instance DatabaseEntity Lab where
    tableName _ = "labs"

    selectAll conn = do
        (_, stream) <- query_ conn
            "SELECT lab_id, lab_name, building, room_number, total_workstations FROM labs ORDER BY lab_id"
        rows <- Streams.toList stream
        return $ mapMaybe fromMySQLValues rows

    selectById conn lid = do
        (_, stream) <- query conn
            "SELECT lab_id, lab_name, building, room_number, total_workstations FROM labs WHERE lab_id = ?"
            [MySQLInt32 $ fromIntegral lid]
        rows <- Streams.toList stream
        case rows of
            (r:_) -> return $ fromMySQLValues r
            []    -> return Nothing

    fromMySQLValues [lid, name, bldg, room, total] =
        case (lid, name, bldg, room, total) of
            (MySQLInt32 lid', MySQLText name', MySQLText bldg', MySQLText room', MySQLInt32 total') ->
                Just $ Lab (Just $ fromIntegral lid') name' bldg' room' (fromIntegral total')
            _ -> Nothing
    fromMySQLValues _ = Nothing

-- | Instance для CrudOperations
instance CrudOperations Lab where
    insert conn lab = do
        ok <- execute conn
            "INSERT INTO labs (lab_name, building, room_number, total_workstations) VALUES (?, ?, ?, ?)"
            [MySQLText (labName lab), MySQLText (building lab), MySQLText (roomNumber lab), MySQLInt32 $ fromIntegral (totalWorkstations lab)]
        return $ fromIntegral $ okLastInsertID ok

    update conn lab = case labId lab of
        Just lid -> do
            _ <- execute conn
                "UPDATE labs SET lab_name = ?, building = ?, room_number = ?, total_workstations = ? WHERE lab_id = ?"
                [MySQLText (labName lab), MySQLText (building lab), MySQLText (roomNumber lab), MySQLInt32 $ fromIntegral (totalWorkstations lab), MySQLInt32 $ fromIntegral lid]
            return ()
        Nothing -> putStrLn "Помилка: немає ID для оновлення"

    delete conn lid = do
        _ <- execute conn "DELETE FROM labs WHERE lab_id = ?" [MySQLInt32 $ fromIntegral lid]
        return ()

    display lab = do
        putStrLn $ "ID: " ++ show (labId lab)
        putStrLn $ "Назва: " ++ T.unpack (labName lab)
        putStrLn $ "Будівля: " ++ T.unpack (building lab)
        putStrLn $ "Кімната: " ++ T.unpack (roomNumber lab)
        putStrLn $ "Кількість робочих місць: " ++ show (totalWorkstations lab)
        putStrLn "---"

    toMySQLValues lab =
        [MySQLText (labName lab), MySQLText (building lab), MySQLText (roomNumber lab), MySQLInt32 $ fromIntegral (totalWorkstations lab)]
