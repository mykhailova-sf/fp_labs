{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Database.MySQL.Base (close)
import Database.Connection
import Models.Lab
import Models.Workstation
import Models.User
import Models.ScheduledClass
import Models.FreeAccess
import Models.Types
import Operations.CRUD
import System.IO (hFlush, stdout, stdin, hSetEncoding, utf8)
import Data.Text (Text, pack)
import Data.Time (Day, TimeOfDay, fromGregorian, makeTimeOfDayValid)
import Text.Read (readMaybe)
import Control.Monad (when)

main :: IO ()
main = do
    -- Set UTF-8 encoding for Windows console
    hSetEncoding stdout utf8
    hSetEncoding stdin utf8

    putStrLn "========================================="
    putStrLn "     Computer Lab Management System     "
    putStrLn "========================================="
    putStrLn ""

    conn <- getConnection
    putStrLn "Connected to database"
    putStrLn ""

    mainMenu conn

    close conn
    putStrLn "Thank you for using the system!"

-- | Main menu
mainMenu :: MySQLConn -> IO ()
mainMenu conn = do
    putStrLn "\n=== MAIN MENU ==="
    putStrLn "1. Manage Labs"
    putStrLn "2. Manage Workstations"
    putStrLn "3. Manage Users"
    putStrLn "4. Manage Scheduled Classes"
    putStrLn "5. Manage Free Access"
    putStrLn "0. Exit"
    putStr "\nChoose option: "
    hFlush stdout

    choice <- getLine
    case choice of
        "1" -> labMenu conn >> mainMenu conn
        "2" -> workstationMenu conn >> mainMenu conn
        "3" -> userMenu conn >> mainMenu conn
        "4" -> scheduledClassMenu conn >> mainMenu conn
        "5" -> freeAccessMenu conn >> mainMenu conn
        "0" -> return ()
        _   -> do
            putStrLn "Invalid choice. Try again."
            mainMenu conn

-- | Lab management menu
labMenu :: MySQLConn -> IO ()
labMenu conn = do
    putStrLn "\n=== LAB MANAGEMENT ==="
    putStrLn "1. Show all labs"
    putStrLn "2. Find lab by ID"
    putStrLn "3. Add new lab"
    putStrLn "4. Update lab"
    putStrLn "5. Delete lab"
    putStrLn "0. Back"
    putStr "\nChoose option: "
    hFlush stdout

    choice <- getLine
    case choice of
        "1" -> performReadAll conn (selectAll :: MySQLConn -> IO [Lab]) >> labMenu conn
        "2" -> do
            putStr "Enter lab ID: "
            hFlush stdout
            idStr <- getLine
            case readMaybe idStr of
                Just lid -> performReadById conn (selectById :: MySQLConn -> Int -> IO (Maybe Lab)) lid
                Nothing -> putStrLn "Invalid ID"
            labMenu conn
        "3" -> createLab conn >> labMenu conn
        "4" -> updateLab conn >> labMenu conn
        "5" -> do
            putStr "Enter lab ID to delete: "
            hFlush stdout
            idStr <- getLine
            case readMaybe idStr of
                Just lid -> performDelete conn (delete @Lab) lid
                Nothing -> putStrLn "Invalid ID"
            labMenu conn
        "0" -> return ()
        _   -> do
            putStrLn "Invalid choice"
            labMenu conn

-- | Create new lab
createLab :: MySQLConn -> IO ()
createLab conn = do
    putStrLn "\n--- Creating new lab ---"

    putStr "Lab name: "
    hFlush stdout
    name <- pack <$> getLine

    putStr "Building: "
    hFlush stdout
    bldg <- pack <$> getLine

    putStr "Room number: "
    hFlush stdout
    room <- pack <$> getLine

    putStr "Total workstations: "
    hFlush stdout
    capacityStr <- getLine

    case readMaybe capacityStr of
        Just capacity -> do
            let lab = Lab Nothing name bldg room capacity
            performCreate conn lab
        Nothing -> putStrLn "Invalid quantity value"

-- | Update lab
updateLab :: MySQLConn -> IO ()
updateLab conn = do
    putStrLn "\n--- Updating lab ---"

    putStr "Enter lab ID: "
    hFlush stdout
    idStr <- getLine

    case readMaybe idStr of
        Just lid -> do
            maybeLab <- selectById conn lid :: IO (Maybe Lab)
            case maybeLab of
                Just _ -> do
                    putStr "New lab name: "
                    hFlush stdout
                    name <- pack <$> getLine

                    putStr "New building: "
                    hFlush stdout
                    bldg <- pack <$> getLine

                    putStr "New room number: "
                    hFlush stdout
                    room <- pack <$> getLine

                    putStr "New total workstations: "
                    hFlush stdout
                    capacityStr <- getLine

                    case readMaybe capacityStr of
                        Just capacity -> do
                            let lab = Lab (Just lid) name bldg room capacity
                            performUpdate conn lab
                        Nothing -> putStrLn "Invalid quantity value"
                Nothing -> putStrLn "Lab not found"
        Nothing -> putStrLn "Invalid ID"

-- | Workstation management menu
workstationMenu :: MySQLConn -> IO ()
workstationMenu conn = do
    putStrLn "\n=== WORKSTATION MANAGEMENT ==="
    putStrLn "1. Show all workstations"
    putStrLn "2. Find workstation by ID"
    putStrLn "3. Add new workstation"
    putStrLn "4. Update workstation"
    putStrLn "5. Delete workstation"
    putStrLn "0. Back"
    putStr "\nChoose option: "
    hFlush stdout

    choice <- getLine
    case choice of
        "1" -> performReadAll conn (selectAll :: MySQLConn -> IO [Workstation]) >> workstationMenu conn
        "2" -> do
            putStr "Enter workstation ID: "
            hFlush stdout
            idStr <- getLine
            case readMaybe idStr of
                Just wid -> performReadById conn (selectById :: MySQLConn -> Int -> IO (Maybe Workstation)) wid
                Nothing -> putStrLn "Invalid ID"
            workstationMenu conn
        "3" -> createWorkstation conn >> workstationMenu conn
        "4" -> updateWorkstation conn >> workstationMenu conn
        "5" -> do
            putStr "Enter workstation ID to delete: "
            hFlush stdout
            idStr <- getLine
            case readMaybe idStr of
                Just wid -> performDelete conn (delete @Workstation) wid
                Nothing -> putStrLn "Invalid ID"
            workstationMenu conn
        "0" -> return ()
        _   -> do
            putStrLn "Invalid choice"
            workstationMenu conn

-- | Create new workstation
createWorkstation :: MySQLConn -> IO ()
createWorkstation conn = do
    putStrLn "\n--- Creating new workstation ---"

    putStr "Lab ID: "
    hFlush stdout
    labIdStr <- getLine

    putStr "Workstation number: "
    hFlush stdout
    numStr <- getLine

    putStr "Working? (1-yes, 0-no): "
    hFlush stdout
    statusStr <- getLine

    case (readMaybe labIdStr, readMaybe numStr) of
        (Just lid, Just num) -> do
            let working = statusStr == "1"
            let ws = Workstation Nothing lid num working
            performCreate conn ws
        _ -> putStrLn "Invalid data"

-- | Update workstation
updateWorkstation :: MySQLConn -> IO ()
updateWorkstation conn = do
    putStrLn "\n--- Updating workstation ---"

    putStr "Enter workstation ID: "
    hFlush stdout
    idStr <- getLine

    case readMaybe idStr of
        Just wid -> do
            maybeWs <- selectById conn wid :: IO (Maybe Workstation)
            case maybeWs of
                Just _ -> do
                    putStr "New lab ID: "
                    hFlush stdout
                    labIdStr <- getLine

                    putStr "New workstation number: "
                    hFlush stdout
                    numStr <- getLine

                    putStr "Working? (1-yes, 0-no): "
                    hFlush stdout
                    statusStr <- getLine

                    case (readMaybe labIdStr, readMaybe numStr) of
                        (Just lid, Just num) -> do
                            let working = statusStr == "1"
                            let ws = Workstation (Just wid) lid num working
                            performUpdate conn ws
                        _ -> putStrLn "Invalid data"
                Nothing -> putStrLn "Workstation not found"
        Nothing -> putStrLn "Invalid ID"

-- | User management menu
userMenu :: MySQLConn -> IO ()
userMenu conn = do
    putStrLn "\n=== USER MANAGEMENT ==="
    putStrLn "1. Show all users"
    putStrLn "2. Find user by ID"
    putStrLn "3. Add new user"
    putStrLn "4. Update user"
    putStrLn "5. Delete user"
    putStrLn "0. Back"
    putStr "\nChoose option: "
    hFlush stdout

    choice <- getLine
    case choice of
        "1" -> performReadAll conn (selectAll :: MySQLConn -> IO [User]) >> userMenu conn
        "2" -> do
            putStr "Enter user ID: "
            hFlush stdout
            idStr <- getLine
            case readMaybe idStr of
                Just uid -> performReadById conn (selectById :: MySQLConn -> Int -> IO (Maybe User)) uid
                Nothing -> putStrLn "Invalid ID"
            userMenu conn
        "3" -> createUser conn >> userMenu conn
        "4" -> updateUser conn >> userMenu conn
        "5" -> do
            putStr "Enter user ID to delete: "
            hFlush stdout
            idStr <- getLine
            case readMaybe idStr of
                Just uid -> performDelete conn (delete @User) uid
                Nothing -> putStrLn "Invalid ID"
            userMenu conn
        "0" -> return ()
        _   -> do
            putStrLn "Invalid choice"
            userMenu conn

-- | Create new user
createUser :: MySQLConn -> IO ()
createUser conn = do
    putStrLn "\n--- Creating new user ---"

    putStr "Full name: "
    hFlush stdout
    name <- pack <$> getLine

    putStr "Type (1-student, 2-instructor): "
    hFlush stdout
    typeStr <- getLine

    putStr "Email (or Enter to skip): "
    hFlush stdout
    emailStr <- getLine

    putStr "Phone (or Enter to skip): "
    hFlush stdout
    phoneStr <- getLine

    let utype = if typeStr == "1" then Student else Instructor
    let emailVal = if null emailStr then Nothing else Just (pack emailStr)
    let phoneVal = if null phoneStr then Nothing else Just (pack phoneStr)

    let user = User Nothing name utype emailVal phoneVal
    performCreate conn user

-- | Update user
updateUser :: MySQLConn -> IO ()
updateUser conn = do
    putStrLn "\n--- Updating user ---"

    putStr "Enter user ID: "
    hFlush stdout
    idStr <- getLine

    case readMaybe idStr of
        Just uid -> do
            maybeUser <- selectById conn uid :: IO (Maybe User)
            case maybeUser of
                Just _ -> do
                    putStr "New full name: "
                    hFlush stdout
                    name <- pack <$> getLine

                    putStr "New type (1-student, 2-instructor): "
                    hFlush stdout
                    typeStr <- getLine

                    putStr "New email (or Enter to skip): "
                    hFlush stdout
                    emailStr <- getLine

                    putStr "New phone (or Enter to skip): "
                    hFlush stdout
                    phoneStr <- getLine

                    let utype = if typeStr == "1" then Student else Instructor
                    let emailVal = if null emailStr then Nothing else Just (pack emailStr)
                    let phoneVal = if null phoneStr then Nothing else Just (pack phoneStr)

                    let user = User (Just uid) name utype emailVal phoneVal
                    performUpdate conn user
                Nothing -> putStrLn "User not found"
        Nothing -> putStrLn "Invalid ID"

-- | Scheduled classes management menu
scheduledClassMenu :: MySQLConn -> IO ()
scheduledClassMenu conn = do
    putStrLn "\n=== SCHEDULED CLASSES MANAGEMENT ==="
    putStrLn "1. Show all schedule"
    putStrLn "2. Find class by ID"
    putStrLn "3. Add new class"
    putStrLn "4. Update class"
    putStrLn "5. Delete class"
    putStrLn "0. Back"
    putStr "\nChoose option: "
    hFlush stdout

    choice <- getLine
    case choice of
        "1" -> performReadAll conn (selectAll :: MySQLConn -> IO [ScheduledClass]) >> scheduledClassMenu conn
        "2" -> do
            putStr "Enter class ID: "
            hFlush stdout
            idStr <- getLine
            case readMaybe idStr of
                Just cid -> performReadById conn (selectById :: MySQLConn -> Int -> IO (Maybe ScheduledClass)) cid
                Nothing -> putStrLn "Invalid ID"
            scheduledClassMenu conn
        "3" -> createScheduledClass conn >> scheduledClassMenu conn
        "4" -> updateScheduledClass conn >> scheduledClassMenu conn
        "5" -> do
            putStr "Enter class ID to delete: "
            hFlush stdout
            idStr <- getLine
            case readMaybe idStr of
                Just cid -> performDelete conn (delete @ScheduledClass) cid
                Nothing -> putStrLn "Invalid ID"
            scheduledClassMenu conn
        "0" -> return ()
        _   -> do
            putStrLn "Invalid choice"
            scheduledClassMenu conn

-- | Create new scheduled class
createScheduledClass :: MySQLConn -> IO ()
createScheduledClass conn = do
    putStrLn "\n--- Creating new class ---"

    putStr "Lab ID: "
    hFlush stdout
    labIdStr <- getLine

    putStr "Instructor ID: "
    hFlush stdout
    instrIdStr <- getLine

    putStr "Course name: "
    hFlush stdout
    courseName <- pack <$> getLine

    putStrLn "Day of week (1-Monday, 2-Tuesday, 3-Wednesday, 4-Thursday, 5-Friday, 6-Saturday): "
    putStr "Choose: "
    hFlush stdout
    dayStr <- getLine

    let day = case dayStr of
            "1" -> Monday
            "2" -> Tuesday
            "3" -> Wednesday
            "4" -> Thursday
            "5" -> Friday
            "6" -> Saturday
            _   -> Monday

    putStr "Start time (hours): "
    hFlush stdout
    startHourStr <- getLine

    putStr "Start time (minutes): "
    hFlush stdout
    startMinStr <- getLine

    putStr "End time (hours): "
    hFlush stdout
    endHourStr <- getLine

    putStr "End time (minutes): "
    hFlush stdout
    endMinStr <- getLine

    case (readMaybe labIdStr, readMaybe instrIdStr,
          readMaybe startHourStr, readMaybe startMinStr,
          readMaybe endHourStr, readMaybe endMinStr) of
        (Just lid, Just iid, Just sh, Just sm, Just eh, Just em) ->
            case (makeTimeOfDayValid sh sm 0, makeTimeOfDayValid eh em 0) of
                (Just stime, Just etime) -> do
                    let sc = ScheduledClass Nothing lid iid courseName day stime etime
                    performCreate conn sc
                _ -> putStrLn "Invalid time"
        _ -> putStrLn "Invalid data"

-- | Update scheduled class
updateScheduledClass :: MySQLConn -> IO ()
updateScheduledClass conn = do
    putStrLn "\n--- Updating class ---"
    putStr "Enter class ID: "
    hFlush stdout
    idStr <- getLine

    case readMaybe idStr of
        Just cid -> do
            maybeSc <- selectById conn cid :: IO (Maybe ScheduledClass)
            case maybeSc of
                Just _ -> do
                    putStr "New lab ID: "
                    hFlush stdout
                    labIdStr <- getLine

                    putStr "New instructor ID: "
                    hFlush stdout
                    instrIdStr <- getLine

                    putStr "New course name: "
                    hFlush stdout
                    courseName <- pack <$> getLine

                    putStrLn "New day of week (1-Monday, 2-Tuesday, 3-Wednesday, 4-Thursday, 5-Friday, 6-Saturday): "
                    putStr "Choose: "
                    hFlush stdout
                    dayStr <- getLine

                    let day = case dayStr of
                            "1" -> Monday
                            "2" -> Tuesday
                            "3" -> Wednesday
                            "4" -> Thursday
                            "5" -> Friday
                            "6" -> Saturday
                            _   -> Monday

                    putStr "New start time (hours): "
                    hFlush stdout
                    startHourStr <- getLine

                    putStr "New start time (minutes): "
                    hFlush stdout
                    startMinStr <- getLine

                    putStr "New end time (hours): "
                    hFlush stdout
                    endHourStr <- getLine

                    putStr "New end time (minutes): "
                    hFlush stdout
                    endMinStr <- getLine

                    case (readMaybe labIdStr, readMaybe instrIdStr,
                          readMaybe startHourStr, readMaybe startMinStr,
                          readMaybe endHourStr, readMaybe endMinStr) of
                        (Just lid, Just iid, Just sh, Just sm, Just eh, Just em) ->
                            case (makeTimeOfDayValid sh sm 0, makeTimeOfDayValid eh em 0) of
                                (Just stime, Just etime) -> do
                                    let sc = ScheduledClass (Just cid) lid iid courseName day stime etime
                                    performUpdate conn sc
                                _ -> putStrLn "Invalid time"
                        _ -> putStrLn "Invalid data"
                Nothing -> putStrLn "Class not found"
        Nothing -> putStrLn "Invalid ID"

-- | Free access management menu
freeAccessMenu :: MySQLConn -> IO ()
freeAccessMenu conn = do
    putStrLn "\n=== FREE ACCESS MANAGEMENT ==="
    putStrLn "1. Show all records"
    putStrLn "2. Find record by ID"
    putStrLn "3. Add new record"
    putStrLn "4. Update record"
    putStrLn "5. Delete record"
    putStrLn "0. Back"
    putStr "\nChoose option: "
    hFlush stdout

    choice <- getLine
    case choice of
        "1" -> performReadAll conn (selectAll :: MySQLConn -> IO [FreeAccess]) >> freeAccessMenu conn
        "2" -> do
            putStr "Enter record ID: "
            hFlush stdout
            idStr <- getLine
            case readMaybe idStr of
                Just aid -> performReadById conn (selectById :: MySQLConn -> Int -> IO (Maybe FreeAccess)) aid
                Nothing -> putStrLn "Invalid ID"
            freeAccessMenu conn
        "3" -> createFreeAccess conn >> freeAccessMenu conn
        "4" -> updateFreeAccess conn >> freeAccessMenu conn
        "5" -> do
            putStr "Enter record ID to delete: "
            hFlush stdout
            idStr <- getLine
            case readMaybe idStr of
                Just aid -> performDelete conn (delete @FreeAccess) aid
                Nothing -> putStrLn "Invalid ID"
            freeAccessMenu conn
        "0" -> return ()
        _   -> do
            putStrLn "Invalid choice"
            freeAccessMenu conn

-- | Create new free access record
createFreeAccess :: MySQLConn -> IO ()
createFreeAccess conn = do
    putStrLn "\n--- Creating new free access record ---"

    putStr "Lab ID: "
    hFlush stdout
    labIdStr <- getLine

    putStr "User ID: "
    hFlush stdout
    userIdStr <- getLine

    putStr "Workstation ID: "
    hFlush stdout
    wsIdStr <- getLine

    putStr "Date (year): "
    hFlush stdout
    yearStr <- getLine

    putStr "Date (month): "
    hFlush stdout
    monthStr <- getLine

    putStr "Date (day): "
    hFlush stdout
    dayStr <- getLine

    putStr "Start time (hours): "
    hFlush stdout
    startHourStr <- getLine

    putStr "Start time (minutes): "
    hFlush stdout
    startMinStr <- getLine

    putStr "End time (hours): "
    hFlush stdout
    endHourStr <- getLine

    putStr "End time (minutes): "
    hFlush stdout
    endMinStr <- getLine

    case (readMaybe labIdStr, readMaybe userIdStr, readMaybe wsIdStr,
          readMaybe yearStr, readMaybe monthStr, readMaybe dayStr,
          readMaybe startHourStr, readMaybe startMinStr,
          readMaybe endHourStr, readMaybe endMinStr) of
        (Just lid, Just uid, Just wid, Just y, Just m, Just d,
         Just sh, Just sm, Just eh, Just em) -> do
            let date = fromGregorian (fromIntegral y) m d
            case (makeTimeOfDayValid sh sm 0, makeTimeOfDayValid eh em 0) of
                (Just stime, Just etime) -> do
                    let fa = FreeAccess Nothing lid uid wid date stime etime
                    performCreate conn fa
                _ -> putStrLn "Invalid time"
        _ -> putStrLn "Invalid data"

-- | Update free access record
updateFreeAccess :: MySQLConn -> IO ()
updateFreeAccess conn = do
    putStrLn "\n--- Updating free access record ---"

    putStr "Enter record ID: "
    hFlush stdout
    idStr <- getLine

    case readMaybe idStr of
        Just aid -> do
            maybeFa <- selectById conn aid :: IO (Maybe FreeAccess)
            case maybeFa of
                Just _ -> do
                    putStr "New lab ID: "
                    hFlush stdout
                    labIdStr <- getLine

                    putStr "New user ID: "
                    hFlush stdout
                    userIdStr <- getLine

                    putStr "New workstation ID: "
                    hFlush stdout
                    wsIdStr <- getLine

                    putStr "New date (year): "
                    hFlush stdout
                    yearStr <- getLine

                    putStr "New date (month): "
                    hFlush stdout
                    monthStr <- getLine

                    putStr "New date (day): "
                    hFlush stdout
                    dayStr <- getLine

                    putStr "New start time (hours): "
                    hFlush stdout
                    startHourStr <- getLine

                    putStr "New start time (minutes): "
                    hFlush stdout
                    startMinStr <- getLine

                    putStr "New end time (hours): "
                    hFlush stdout
                    endHourStr <- getLine

                    putStr "New end time (minutes): "
                    hFlush stdout
                    endMinStr <- getLine

                    case (readMaybe labIdStr, readMaybe userIdStr, readMaybe wsIdStr,
                          readMaybe yearStr, readMaybe monthStr, readMaybe dayStr,
                          readMaybe startHourStr, readMaybe startMinStr,
                          readMaybe endHourStr, readMaybe endMinStr) of
                        (Just lid, Just uid, Just wid, Just y, Just m, Just d,
                         Just sh, Just sm, Just eh, Just em) -> do
                            let date = fromGregorian (fromIntegral y) m d
                            case (makeTimeOfDayValid sh sm 0, makeTimeOfDayValid eh em 0) of
                                (Just stime, Just etime) -> do
                                    let fa = FreeAccess (Just aid) lid uid wid date stime etime
                                    performUpdate conn fa
                                _ -> putStrLn "Invalid time"
                        _ -> putStrLn "Invalid data"
                Nothing -> putStrLn "Record not found"
        Nothing -> putStrLn "Invalid ID"
