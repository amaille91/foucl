{-# LANGUAGE MultiParamTypeClasses #-}

module NoteCrud (NoteServiceConfig(..), defaultNoteServiceConfig) where

import           Crud        (CRUDEngine (..), DiskFileStorageConfig (..))
import           Model       (NoteContent (..))
import qualified CrudStorage as CrudStorage

data NoteServiceConfig = NoteServiceConfig String

instance DiskFileStorageConfig NoteServiceConfig where
    rootPath (NoteServiceConfig path) = path

instance CRUDEngine NoteServiceConfig NoteContent where
  getItems = CrudStorage.getAllItems
  postItem = CrudStorage.createItem
  delItem = CrudStorage.deleteItem
  putItem = CrudStorage.modifyItem
  crudTypeDenomination _ = "note"

defaultNoteServiceConfig = NoteServiceConfig ".foucl/data/note"
