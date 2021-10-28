{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module IntegrationTests (runIntegrationTests) where

import Prelude hiding (id)
import           Data.Aeson
import           Data.ByteString       (ByteString)
import           Data.ByteString.UTF8
import           Data.Functor.Identity
import           GHC.Exts
import           GHC.Generics          (Generic)
import           Network.HTTP.Simple
import           Test.Hspec
import           Test.HUnit
import Model

-- ===================== Constants ==============================

noteEndpoint = "/note"
checklistEndpoint = "/checklist"

runIntegrationTests :: IO ()
runIntegrationTests = hspec $ do
  describe "Integration Tests" $ do
    it "should satisfy the basics, in one session, of the Very First User's needs, note-wise" $ do
      let
        firstNoteContent = NoteContent { title = Just "First note", noteContent = "First note content" }
        firstNoteNewContent = NoteContent { title = Just "First note new title", noteContent = "This is a new content for the first note" }
      assertNoItemAtEndpoint NoteEndpoint
      createNewContent NoteEndpoint firstNoteContent
      [firstNote :: Identifiable NoteContent] <- assertGetWithContent NoteEndpoint firstNoteContent
      modifyItem NoteEndpoint $ Identifiable (storageId firstNote) firstNoteNewContent
      [modifiedNote] <- assertGetWithContent NoteEndpoint firstNoteNewContent
      deleteItem NoteEndpoint $ (id.storageId) modifiedNote
      assertNoItemAtEndpoint NoteEndpoint
    it "should satisfy the basics, in one session of the Very Firsr User's needs, checklist-wise" $ do
      assertNoItemAtEndpoint ChecklistEndpoint
      createNewContent ChecklistEndpoint firstChecklistContent
      [firstChecklist :: Identifiable ChecklistContent] <- assertGetWithContent ChecklistEndpoint firstChecklistContent
      modifyItem ChecklistEndpoint $ Identifiable (storageId firstChecklist) firstChecklistNewContent
      [modifiedChecklist :: Identifiable ChecklistContent] <- assertGetWithContent ChecklistEndpoint firstChecklistNewContent
      deleteItem ChecklistEndpoint $ (id.storageId) modifiedChecklist
        where
        firstChecklistContent    = ChecklistContent { name = "First checklist"
                                                     , items = [ ChecklistItem { label = "First item label unchecked", checked = False }
                                                               , ChecklistItem { label = "Second item label checked", checked = True }
                                                               ]
                                                     }
        firstChecklistNewContent = ChecklistContent { name = "new checklist"
                                                    , items = [ ChecklistItem { label = "Third item label checked", checked = True }
                                                              , ChecklistItem { label = "Fourth item label checked", checked = True }
                                                              ]
                                                    }

assertNoItemAtEndpoint :: (Content contentType, RequestType GET endpointType () [Identifiable contentType]) => endpointType -> Expectation
assertNoItemAtEndpoint endpoint = do
  getResponse :: Response [Identifiable contentType] <- sendRequestWithJSONBodyImpl GET endpoint ()
  assertNoNoteInResponse "Failed to start with an empty server" getResponse

createNewContent :: (Content contentType, RequestType POST endpointType contentType StorageId) => endpointType -> contentType -> Expectation
createNewContent endpoint content = do
  postResponse :: Response StorageId <- sendRequestWithJSONBodyImpl POST endpoint content
  assertStatusCode200 ("Failed to create item" ++ show content) postResponse

assertGetWithContent :: (Content contentType, RequestType GET endpointType () [Identifiable contentType]) => endpointType -> contentType -> IO [Identifiable contentType]
assertGetWithContent endpoint expectedContent = do
  getResponse <- sendRequestWithJSONBodyImpl GET endpoint ()
  assertWithFoundContent ("Failed to retrieve created content " ++ show expectedContent) [expectedContent] getResponse

modifyItem :: (Content contentType, RequestType PUT endpointType (Identifiable contentType) StorageId) => endpointType -> Identifiable contentType -> Expectation
modifyItem endpoint update = do
  putResponse :: Response StorageId <- sendRequestWithJSONBodyImpl PUT endpoint update
  assertStatusCode200 ("Failed to apply modification " ++ show update) putResponse

deleteItem :: Endpoint a => a -> String -> Expectation
deleteItem endpoint idToDelete = do
  deleteResponse <- parseRequest ("DELETE http://localhost:8081" ++ getEndpoint endpoint ++ "/" ++ idToDelete) >>= httpBS
  assertStatusCode200 ("Failed to delete item" ++ show idToDelete) deleteResponse

assertWithFoundContent :: Content a => String -> [a] -> Response [Identifiable a] -> IO [Identifiable a]
assertWithFoundContent errorPrefix expectedContents response = do
  assertStatusCode200 errorPrefix response
  assertEqual (errorPrefix ++ "\n\tExpected notes with contents:\n\t" ++ show expectedContents) expectedContents  (map content responseItems)
  return responseItems
  where
    responseItems = getResponseBody response

sendRequestWithJSONBodyImpl :: (RequestType methodType endpointType requestType responseType) =>
  methodType -> endpointType -> requestType -> IO (Response responseType)
sendRequestWithJSONBodyImpl method endpoint body = do
    req <- parseRequest ("http://localhost:8081" ++ getEndpoint endpoint) 
    httpJSON $ (setRequestMethod (getMethod method) . (setRequestHeader "Content-Type" ["application/json"]) . (setRequestBodyJSON body)) req


-- sendRequestWithJSONBodyImpl endpoint method body = httpJSON <$>
--   setRequestBodyJSON body <$>
--     setRequestHeader "Content-Type" ["application/json"] <$>
--       setRequestMethod method <$> parseRequest ("http://localhost:8081" ++ endpoint) 
-- 
-- sendRequestSimple :: (ToJSON requestType) => ByteString -> requestType -> IO (Response ByteString)
-- sendRequestSimple method body = httpBS $
--   setRequestBodyJSON body $
--     setRequestHeader "Content-Type" ["application/json"] $
--       setRequestMethod method "http://localhost:8081/note"

assertNoNoteInResponse errorPrefix response = do
  assertStatusCode200 errorPrefix response
  assertEqual (errorPrefix ++ "Expected no note in response"     ) []  (toList $ getResponseBody response)

assertStatusCode200 :: String -> Response a -> Assertion
assertStatusCode200 errorPrefix response = assertEqual (errorPrefix ++ "Expected 200 response status code") 200 (getResponseStatusCode response)

modifyNote :: Content a => a -> Identifiable a -> Identifiable a
modifyNote newContent previousNote = Identifiable (storageId previousNote) newContent

class Method a where
    getMethod :: a -> ByteString

data GET = GET
data POST = POST
data PUT = PUT
data DELETE = DELETE

instance Method GET where
    getMethod GET = "GET"

instance Method POST where
    getMethod POST = "POST"

instance Method DELETE where
    getMethod DELETE = "DELETE"

instance Method PUT where
    getMethod PUT = "PUT"

class Endpoint a where
    getEndpoint :: a -> String

data NoteEndpoint = NoteEndpoint
instance Endpoint NoteEndpoint where
    getEndpoint NoteEndpoint = "/note"

data ChecklistEndpoint = ChecklistEndpoint
instance Endpoint ChecklistEndpoint where
    getEndpoint ChecklistEndpoint = "/checklist"

class (ToJSON requestType, FromJSON responseType, Endpoint endpoint, Method methodType) => RequestType methodType endpoint requestType responseType | endpoint methodType -> requestType, endpoint methodType requestType -> responseType where
    sendRequestWithJSONBody :: endpoint -> methodType -> requestType -> IO (Response responseType)

instance RequestType GET NoteEndpoint () [Identifiable NoteContent] where
    sendRequestWithJSONBody endpoint _ req = sendRequestWithJSONBodyImpl GET endpoint req

instance RequestType POST NoteEndpoint NoteContent StorageId where
    sendRequestWithJSONBody endpoint _ req = sendRequestWithJSONBodyImpl POST endpoint req

instance RequestType PUT NoteEndpoint (Identifiable NoteContent) StorageId where
    sendRequestWithJSONBody endpoint _ req = sendRequestWithJSONBodyImpl PUT endpoint req

instance RequestType GET ChecklistEndpoint () [Identifiable ChecklistContent] where
    sendRequestWithJSONBody endpoint _ req = sendRequestWithJSONBodyImpl GET endpoint req

instance RequestType POST ChecklistEndpoint ChecklistContent StorageId where
    sendRequestWithJSONBody endpoint _ req = sendRequestWithJSONBodyImpl POST endpoint req

instance RequestType PUT ChecklistEndpoint (Identifiable ChecklistContent) StorageId where
    sendRequestWithJSONBody endpoint _ req = sendRequestWithJSONBodyImpl PUT endpoint req
