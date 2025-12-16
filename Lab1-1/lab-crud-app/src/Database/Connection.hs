{-# LANGUAGE OverloadedStrings #-}

module Database.Connection
    ( getConnection
    , MySQLConn
    , ConnectInfo(..)
    ) where

import Database.MySQL.Base
import Network.Socket (PortNumber)


getConnection :: IO MySQLConn
getConnection = connect defaultConnectInfoMB4
    { ciHost     = "localhost"
    , ciPort     = 3306 :: PortNumber
    , ciUser     = "root"
    , ciPassword = "password"
    , ciDatabase = "lab_schedule"
    , ciCharset  = 224  -- utf8mb4_unicode_ci for full Unicode support
    }
