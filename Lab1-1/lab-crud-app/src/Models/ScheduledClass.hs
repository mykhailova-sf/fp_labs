{-# LANGUAGE OverloadedStrings #-}

module Models.ScheduledClass
    ( ScheduledClass(..)
    , DayOfWeek(..)
    ) where

import Database.MySQL.Base
import Database.MySQL.Protocol.MySQLValue
import Database.MySQL.Protocol.Packet (OK(..))
import qualified System.IO.Streams as Streams
import Models.Types
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (TimeOfDay)
import Data.Maybe (mapMaybe)

-- | День тижня
data DayOfWeek = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday
    deriving (Show, Eq)

-- | Заплановане заняття
data ScheduledClass = ScheduledClass
    { classId      :: Maybe Int
    , labId        :: Int
    , instructorId :: Int
    , courseName   :: Text
    , dayOfWeek    :: DayOfWeek
    , startTime    :: TimeOfDay
    , endTime      :: TimeOfDay
    } deriving (Show, Eq)

-- | Instance для DatabaseEntity
instance DatabaseEntity ScheduledClass where
    tableName _ = "scheduled_classes"

    selectAll conn = do
        (_, stream) <- query_ conn
            "SELECT class_id, lab_id, instructor_id, course_name, day_of_week, start_time, end_time FROM scheduled_classes ORDER BY day_of_week, start_time"
        rows <- Streams.toList stream
        return $ mapMaybe fromMySQLValues rows

    selectById conn cid = do
        (_, stream) <- query conn
            "SELECT class_id, lab_id, instructor_id, course_name, day_of_week, start_time, end_time FROM scheduled_classes WHERE class_id = ?"
            [MySQLInt32 $ fromIntegral cid]
        rows <- Streams.toList stream
        case rows of
            (r:_) -> return $ fromMySQLValues r
            []    -> return Nothing

    fromMySQLValues [cid, lid, iid, cname, day, stime, etime] =
        case (cid, lid, iid, cname, day, stime, etime) of
            (MySQLInt32 cid', MySQLInt32 lid', MySQLInt32 iid', MySQLText cname', MySQLText day', MySQLTime _ stime', MySQLTime _ etime') ->
                let parsedDay = case day' of
                        "Monday"    -> Monday
                        "Tuesday"   -> Tuesday
                        "Wednesday" -> Wednesday
                        "Thursday"  -> Thursday
                        "Friday"    -> Friday
                        "Saturday"  -> Saturday
                        _           -> Monday
                in Just $ ScheduledClass (Just $ fromIntegral cid') (fromIntegral lid') (fromIntegral iid') cname' parsedDay stime' etime'
            _ -> Nothing
    fromMySQLValues _ = Nothing

-- | Instance для CrudOperations
instance CrudOperations ScheduledClass where
    insert conn sc = do
        let dayStr = case dayOfWeek sc of
                Monday    -> MySQLText "Monday"
                Tuesday   -> MySQLText "Tuesday"
                Wednesday -> MySQLText "Wednesday"
                Thursday  -> MySQLText "Thursday"
                Friday    -> MySQLText "Friday"
                Saturday  -> MySQLText "Saturday"
        ok <- execute conn
            "INSERT INTO scheduled_classes (lab_id, instructor_id, course_name, day_of_week, start_time, end_time) VALUES (?, ?, ?, ?, ?, ?)"
            [MySQLInt32 $ fromIntegral (labId sc), MySQLInt32 $ fromIntegral (instructorId sc), MySQLText (courseName sc), dayStr, MySQLTime 0 (startTime sc), MySQLTime 0 (endTime sc)]
        return $ fromIntegral $ okLastInsertID ok

    update conn sc = case classId sc of
        Just cid -> do
            let dayStr = case dayOfWeek sc of
                    Monday    -> MySQLText "Monday"
                    Tuesday   -> MySQLText "Tuesday"
                    Wednesday -> MySQLText "Wednesday"
                    Thursday  -> MySQLText "Thursday"
                    Friday    -> MySQLText "Friday"
                    Saturday  -> MySQLText "Saturday"
            _ <- execute conn
                "UPDATE scheduled_classes SET lab_id = ?, instructor_id = ?, course_name = ?, day_of_week = ?, start_time = ?, end_time = ? WHERE class_id = ?"
                [MySQLInt32 $ fromIntegral (labId sc), MySQLInt32 $ fromIntegral (instructorId sc), MySQLText (courseName sc), dayStr, MySQLTime 0 (startTime sc), MySQLTime 0 (endTime sc), MySQLInt32 $ fromIntegral cid]
            return ()
        Nothing -> putStrLn "Помилка: немає ID для оновлення"

    delete conn cid = do
        _ <- execute conn "DELETE FROM scheduled_classes WHERE class_id = ?" [MySQLInt32 $ fromIntegral cid]
        return ()

    display sc = do
        putStrLn $ "ID: " ++ show (classId sc)
        putStrLn $ "ID класу: " ++ show (labId sc)
        putStrLn $ "ID викладача: " ++ show (instructorId sc)
        putStrLn $ "Назва курсу: " ++ T.unpack (courseName sc)
        putStrLn $ "День тижня: " ++ show (dayOfWeek sc)
        putStrLn $ "Час початку: " ++ show (startTime sc)
        putStrLn $ "Час закінчення: " ++ show (endTime sc)
        putStrLn "---"

    toMySQLValues sc =
        let dayStr = case dayOfWeek sc of
                Monday    -> MySQLText "Monday"
                Tuesday   -> MySQLText "Tuesday"
                Wednesday -> MySQLText "Wednesday"
                Thursday  -> MySQLText "Thursday"
                Friday    -> MySQLText "Friday"
                Saturday  -> MySQLText "Saturday"
        in [MySQLInt32 $ fromIntegral (labId sc), MySQLInt32 $ fromIntegral (instructorId sc), MySQLText (courseName sc), dayStr, MySQLTime 0 (startTime sc), MySQLTime 0 (endTime sc)]
