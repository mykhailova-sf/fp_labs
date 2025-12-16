{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Operations.CRUD
    ( performCreate
    , performReadAll
    , performReadById
    , performUpdate
    , performDelete
    ) where

import Models.Types
import Control.Exception (catch, SomeException)

-- | Виконати операцію створення з обробкою помилок
performCreate :: CrudOperations a => MySQLConn -> a -> IO ()
performCreate conn entity = do
    result <- catch
        (do
            newId <- insert conn entity
            putStrLn $ "Успішно створено запис з ID: " ++ show newId
            return ()
        )
        (\(e :: SomeException) -> do
            putStrLn $ "Помилка при створенні: " ++ show e
            return ()
        )
    return result

-- | Виконати операцію читання всіх записів
performReadAll :: CrudOperations a => MySQLConn -> (MySQLConn -> IO [a]) -> IO ()
performReadAll conn selectAllFunc = do
    result <- catch
        (do
            entities <- selectAllFunc conn
            if null entities
                then putStrLn "Записи не знайдено"
                else do
                    putStrLn $ "Знайдено записів: " ++ show (length entities)
                    mapM_ display entities
        )
        (\(e :: SomeException) -> do
            putStrLn $ "Помилка при читанні: " ++ show e
        )
    return result

-- | Виконати операцію читання за ID
performReadById :: CrudOperations a => MySQLConn -> (MySQLConn -> Int -> IO (Maybe a)) -> Int -> IO ()
performReadById conn selectByIdFunc entityId = do
    result <- catch
        (do
            maybeEntity <- selectByIdFunc conn entityId
            case maybeEntity of
                Just entity -> do
                    putStrLn "Запис знайдено:"
                    display entity
                Nothing -> putStrLn "Запис не знайдено"
        )
        (\(e :: SomeException) -> do
            putStrLn $ "Помилка при читанні: " ++ show e
        )
    return result

-- | Виконати операцію оновлення
performUpdate :: CrudOperations a => MySQLConn -> a -> IO ()
performUpdate conn entity = do
    result <- catch
        (do
            update conn entity
            putStrLn "Запис успішно оновлено"
        )
        (\(e :: SomeException) -> do
            putStrLn $ "Помилка при оновленні: " ++ show e
        )
    return result

-- | Виконати операцію видалення
performDelete :: MySQLConn -> (MySQLConn -> Int -> IO ()) -> Int -> IO ()
performDelete conn deleteFunc entityId = do
    result <- catch
        (do
            deleteFunc conn entityId
            putStrLn "Запис успішно видалено"
        )
        (\(e :: SomeException) -> do
            putStrLn $ "Помилка при видаленні: " ++ show e
        )
    return result
