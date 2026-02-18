{-# LANGUAGE MultiParamTypeClasses #-}

module TaskCrud (TaskServiceConfig(..), defaultTaskServiceConfig) where

import Crud (CRUDEngine(..), DiskFileStorageConfig(..))
import qualified CrudStorage
import Model (TaskContent(..))

newtype TaskServiceConfig = TaskServiceConfig String

instance DiskFileStorageConfig TaskServiceConfig where
  rootPath (TaskServiceConfig path) = path

instance CRUDEngine TaskServiceConfig TaskContent where
  getItems = CrudStorage.getAllItems
  postItem = CrudStorage.createItem
  delItem = CrudStorage.deleteItem
  putItem = CrudStorage.modifyItem
  crudTypeDenomination _ = "task"

defaultTaskServiceConfig :: TaskServiceConfig
defaultTaskServiceConfig = TaskServiceConfig "data/task"
