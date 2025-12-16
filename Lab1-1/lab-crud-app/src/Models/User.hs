{-# LANGUAGE OverloadedStrings #-}

module Models.User
    ( User(..)
    , UserType(..)
    ) where

import Database.MySQL.Base
import Database.MySQL.Protocol.MySQLValue
import Database.MySQL.Protocol.Packet (OK(..))
import qualified System.IO.Streams as Streams
import Models.Types
import Data.Text (Text)
import qualified Data.Text as T
import Data.Maybe (mapMaybe)

-- | Тип користувача
data UserType = Student | Instructor
    deriving (Show, Eq)

-- | Користувач (студент або викладач)
data User = User
    { userId    :: Maybe Int
    , fullName  :: Text
    , userType  :: UserType
    , email     :: Maybe Text
    , phone     :: Maybe Text
    } deriving (Show, Eq)

-- | Instance для DatabaseEntity
instance DatabaseEntity User where
    tableName _ = "users"

    selectAll conn = do
        (_, stream) <- query_ conn
            "SELECT user_id, full_name, user_type, email, phone FROM users ORDER BY user_id"
        rows <- Streams.toList stream
        return $ mapMaybe fromMySQLValues rows

    selectById conn uid = do
        (_, stream) <- query conn
            "SELECT user_id, full_name, user_type, email, phone FROM users WHERE user_id = ?"
            [MySQLInt32 $ fromIntegral uid]
        rows <- Streams.toList stream
        case rows of
            (r:_) -> return $ fromMySQLValues r
            []    -> return Nothing

    fromMySQLValues [uid, name, utype, emailVal, phoneVal] =
        case (uid, name, utype) of
            (MySQLInt32 uid', MySQLText name', MySQLText utype') ->
                let parsedType = case utype' of
                        "student"    -> Student
                        "instructor" -> Instructor
                        _            -> Student
                    parseEmail = case emailVal of
                        MySQLNull -> Nothing
                        MySQLText e -> Just e
                        _ -> Nothing
                    parsePhone = case phoneVal of
                        MySQLNull -> Nothing
                        MySQLText p -> Just p
                        _ -> Nothing
                in Just $ User (Just $ fromIntegral uid') name' parsedType parseEmail parsePhone
            _ -> Nothing
    fromMySQLValues _ = Nothing

-- | Instance для CrudOperations
instance CrudOperations User where
    insert conn user = do
        let utypeStr = case userType user of
                Student    -> MySQLText "student"
                Instructor -> MySQLText "instructor"
            emailVal = maybe MySQLNull MySQLText (email user)
            phoneVal = maybe MySQLNull MySQLText (phone user)
        ok <- execute conn
            "INSERT INTO users (full_name, user_type, email, phone) VALUES (?, ?, ?, ?)"
            [MySQLText (fullName user), utypeStr, emailVal, phoneVal]
        return $ fromIntegral $ okLastInsertID ok

    update conn user = case userId user of
        Just uid -> do
            let utypeStr = case userType user of
                    Student    -> MySQLText "student"
                    Instructor -> MySQLText "instructor"
                emailVal = maybe MySQLNull MySQLText (email user)
                phoneVal = maybe MySQLNull MySQLText (phone user)
            _ <- execute conn
                "UPDATE users SET full_name = ?, user_type = ?, email = ?, phone = ? WHERE user_id = ?"
                [MySQLText (fullName user), utypeStr, emailVal, phoneVal, MySQLInt32 $ fromIntegral uid]
            return ()
        Nothing -> putStrLn "Помилка: немає ID для оновлення"

    delete conn uid = do
        _ <- execute conn "DELETE FROM users WHERE user_id = ?" [MySQLInt32 $ fromIntegral uid]
        return ()

    display user = do
        putStrLn $ "ID: " ++ show (userId user)
        putStrLn $ "Ім'я: " ++ T.unpack (fullName user)
        putStrLn $ "Тип: " ++ case userType user of
            Student    -> "Студент"
            Instructor -> "Викладач"
        putStrLn $ "Email: " ++ maybe "не вказано" T.unpack (email user)
        putStrLn $ "Телефон: " ++ maybe "не вказано" T.unpack (phone user)
        putStrLn "---"

    toMySQLValues user =
        let utypeStr = case userType user of
                Student    -> MySQLText "student"
                Instructor -> MySQLText "instructor"
            emailVal = maybe MySQLNull MySQLText (email user)
            phoneVal = maybe MySQLNull MySQLText (phone user)
        in [MySQLText (fullName user), utypeStr, emailVal, phoneVal]
