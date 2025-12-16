{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Models.Types
    ( DatabaseEntity(..)
    , CrudOperations(..)
    , MySQLConn
    , MySQLValue(..)
    ) where

import Database.MySQL.Base
import Database.MySQL.Protocol.MySQLValue
import qualified System.IO.Streams as Streams
import Data.Text (Text)

-- | Type class для сутностей бази даних
-- Визначає основні операції для роботи з таблицями
class DatabaseEntity a where
    -- | Назва таблиці в базі даних
    tableName :: a -> String

    -- | Отримати список всіх записів
    selectAll :: MySQLConn -> IO [a]

    -- | Отримати запис за ідентифікатором
    selectById :: MySQLConn -> Int -> IO (Maybe a)

    -- | Розпарсити запис з списку MySQLValue
    fromMySQLValues :: [MySQLValue] -> Maybe a

-- | Type class для CRUD операцій
-- Розширює DatabaseEntity додатковими операціями створення, оновлення та видалення
class DatabaseEntity a => CrudOperations a where
    -- | Створити новий запис
    insert :: MySQLConn -> a -> IO Int

    -- | Оновити існуючий запис
    update :: MySQLConn -> a -> IO ()

    -- | Видалити запис за ідентифікатором
    delete :: MySQLConn -> Int -> IO ()

    -- | Відобразити запис в зручному форматі
    display :: a -> IO ()

    -- | Конвертувати запис в список MySQLValue
    toMySQLValues :: a -> [MySQLValue]
