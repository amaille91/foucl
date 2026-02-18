{-# LANGUAGE MultiParamTypeClasses #-}

module AgendaCrud (AgendaServiceConfig(..), defaultAgendaServiceConfig) where

import Crud (CRUDEngine(..), DiskFileStorageConfig(..))
import qualified CrudStorage
import Model (AgendaContent(..))

newtype AgendaServiceConfig = AgendaServiceConfig String

instance DiskFileStorageConfig AgendaServiceConfig where
  rootPath (AgendaServiceConfig path) = path

instance CRUDEngine AgendaServiceConfig AgendaContent where
  getItems = CrudStorage.getAllItems
  postItem = CrudStorage.createItem
  delItem = CrudStorage.deleteItem
  putItem = CrudStorage.modifyItem
  crudTypeDenomination _ = "agenda"

defaultAgendaServiceConfig :: AgendaServiceConfig
defaultAgendaServiceConfig = AgendaServiceConfig "data/agenda"
