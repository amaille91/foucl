{-# LANGUAGE MultiParamTypeClasses #-}

module ChecklistCrud (ChecklistServiceConfig(..), defaultChecklistServiceConfig) where

import           Crud        (CRUDEngine (..), DiskFileStorageConfig (..))
import           Model       (ChecklistContent (..))
import qualified CrudStorage as CrudStorage

data ChecklistServiceConfig = ChecklistServiceConfig String

instance DiskFileStorageConfig ChecklistServiceConfig where
    rootPath (ChecklistServiceConfig path) = path

instance CRUDEngine ChecklistServiceConfig ChecklistContent where
  getItems = CrudStorage.getAllItems
  postItem = CrudStorage.createItem
  delItem = CrudStorage.deleteItem
  putItem = CrudStorage.modifyItem
  crudTypeDenomination _ = "checklist"

defaultChecklistServiceConfig = ChecklistServiceConfig ".foucl/data/checklist"
