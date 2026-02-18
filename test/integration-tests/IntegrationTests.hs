{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies #-}

module IntegrationTests (runIntegrationTests) where

import Prelude hiding (id)
import           Data.Aeson
import           Data.ByteString       (ByteString)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BL
import           Data.Functor.Identity
import           Data.CaseInsensitive (original)
import           GHC.Exts
import           Network.HTTP.Simple
import           Test.Hspec
import           Test.HUnit
import           Data.Time.Clock.POSIX (getPOSIXTime)
import           Data.List (isInfixOf)
import           Data.Char (toLower)
import Model

-- ===================== Constants ==============================

noteEndpoint = "/note"
checklistEndpoint = "/checklist"

runIntegrationTests :: IO ()
runIntegrationTests = do
  let baseUsername = "integration-base-user"
      userA = "integration-user-a"
      userB = "integration-user-b"
      basePassword = "averystrongpass" :: String
  _ <- signupAndSignin baseUsername basePassword
  _ <- signupAndSignin userA basePassword
  _ <- signupAndSignin userB basePassword
  hspec $ do
    describe "Integration Tests" $ do
      it "should satisfy the basics, in one session, of the Very First User's needs, note-wise" $ do
        cookie <- signinOnly baseUsername basePassword
        let
          firstNoteContent = NoteContent { title = Just "First note", noteContent = "First note content" }
          firstNoteNewContent = NoteContent { title = Just "First note new title", noteContent = "This is a new content for the first note" }
        runCrudLifecycle cookie NoteEndpoint firstNoteContent firstNoteNewContent

      it "should satisfy the basics, in one session, of the Very First User's needs, checklist-wise" $ do
        cookie <- signinOnly baseUsername basePassword
        runCrudLifecycle cookie ChecklistEndpoint firstChecklistContent firstChecklistNewContent

      it "should enforce signup body size" $ do
        let oversizedPayload = BS.replicate 5000 'a'
        oversizedReq <- parseRequest "POST http://localhost:8081/api/signup"
        oversizedResponse <- httpBS $ setRequestMethod "POST" $ setRequestBodyLBS (BL.fromStrict oversizedPayload) oversizedReq
        assertStatusCode "Oversized signup body should be rejected" 413 oversizedResponse

      it "should support trash restore and purge for notes" $ do
        cookie <- signinOnly userA basePassword
        let notePayload = NoteContent { title = Just "trashable", noteContent = "to trash then restore" }
        created <- createNewContentReturningId cookie NoteEndpoint notePayload

        deleteItem cookie NoteEndpoint created
        assertNoItemAtEndpoint cookie NoteEndpoint

        restoreResponse <- restoreItem cookie NoteEndpoint created
        assertStatusCode "Restore should succeed" 200 restoreResponse
        _ <- assertGetWithContent cookie NoteEndpoint notePayload

        deleteItem cookie NoteEndpoint created
        purgeResponse <- purgeItem cookie NoteEndpoint created
        assertStatusCode "Purge should succeed" 200 purgeResponse

        restoreAfterPurge <- restoreItemRaw cookie NoteEndpoint created
        assertStatusCode "Restore after purge should fail with not found" 404 restoreAfterPurge

      it "should isolate note data between users" $ do
        let pass = "averystrongpass"
        cookieA <- signinOnly userA pass
        cookieB <- signinOnly userB pass
        let noteA = NoteContent { title = Just "private note", noteContent = "for user A only" }
        postResponse :: Response StorageId <- sendRequestWithJSONBodyImplWithCookie (Just cookieA) POST NoteEndpoint noteA
        assertStatusCode "Note creation for user A should succeed" 200 postResponse

        getForB :: Response [Identifiable NoteContent] <- sendRequestWithJSONBodyImplWithCookie (Just cookieB) GET NoteEndpoint ()
        assertEqual "User B should not see user A notes" [] (getResponseBody getForB)

      it "should create agendas and keep them private to the owner" $ do
        let pass = "averystrongpass"
            agendaPayload = AgendaContent
              { agendaName = "Agenda Pro"
              , agendaDescription = Just "Planning Q1"
              , agendaTimezone = "Europe/Paris"
              }
        cookieA <- signinOnly userA pass
        cookieB <- signinOnly userB pass
        agendaCreateRes :: Response StorageId <- sendRequestWithJSONBodyImplWithCookie (Just cookieA) POST AgendaEndpoint agendaPayload
        assertStatusCode "Agenda creation should succeed" 200 agendaCreateRes

        getA :: Response [Identifiable AgendaContent] <- sendRequestWithJSONBodyImplWithCookie (Just cookieA) GET AgendaEndpoint ()
        assertEqual "Owner should see one agenda" 1 (length $ getResponseBody getA)

        getB :: Response [Identifiable AgendaContent] <- sendRequestWithJSONBodyImplWithCookie (Just cookieB) GET AgendaEndpoint ()
        assertEqual "Another user should not see owner's agenda" [] (getResponseBody getB)

      it "should expose task creation/search/conflict endpoints scoped by agenda owner" $ do
        let pass = "averystrongpass"
            agendaPayload = AgendaContent
              { agendaName = "Agenda Searchable"
              , agendaDescription = Nothing
              , agendaTimezone = "Europe/Paris"
              }
            task1 = TaskContent
              { taskAgendaId = ""
              , taskTitle = "Préparer budget"
              , taskDescription = Just "Budget annuel"
              , taskStatus = "todo"
              , taskScheduledStartUtc = "2026-03-01T09:00:00Z"
              , taskScheduledEndUtc = "2026-03-01T11:00:00Z"
              , taskTimezone = "Europe/Paris"
              , taskEstimatedDurationMinutes = Just 60
              , taskTags = ["pro", "finance"]
              , taskRecurrence = Nothing
              , taskReminders = []
              , taskDeletedAt = Nothing
              }
            task2 = TaskContent
              { taskAgendaId = ""
              , taskTitle = "Revue budget"
              , taskDescription = Nothing
              , taskStatus = "todo"
              , taskScheduledStartUtc = "2026-03-01T10:00:00Z"
              , taskScheduledEndUtc = "2026-03-01T12:00:00Z"
              , taskTimezone = "Europe/Paris"
              , taskEstimatedDurationMinutes = Just 30
              , taskTags = ["pro"]
              , taskRecurrence = Nothing
              , taskReminders = []
              , taskDeletedAt = Nothing
              }
        cookieA <- signinOnly userA pass
        cookieB <- signinOnly userB pass
        agendaCreateRes :: Response StorageId <- sendRequestWithJSONBodyImplWithCookie (Just cookieA) POST AgendaEndpoint agendaPayload
        assertStatusCode "Agenda creation should succeed" 200 agendaCreateRes
        let createdAgendaId = id (getResponseBody agendaCreateRes)

        taskCreate1 <- postTask cookieA createdAgendaId task1
        assertStatusCode "Task #1 creation should succeed" 200 taskCreate1
        taskCreate2 <- postTask cookieA createdAgendaId task2
        assertStatusCode "Task #2 creation should succeed" 200 taskCreate2

        searchRes <- getAgendaSearch cookieA createdAgendaId [("tag", "finance"), ("status", "todo")]
        assertStatusCode "Search endpoint should succeed" 200 searchRes
        let foundTasks = getResponseBody searchRes
        assertEqual "Search should find exactly one task with tag finance" 1 (length foundTasks)

        conflictsRes <- getAgendaConflicts cookieA createdAgendaId
        assertStatusCode "Conflict endpoint should succeed" 200 conflictsRes
        let conflicts = getResponseBody conflictsRes
        assertEqual "Conflicts should detect one overlap pair" 1 (length conflicts)

        forbiddenSearch <- getAgendaSearchRaw cookieB createdAgendaId []
        assertStatusCode "Non-owner should be unauthorized on agenda search" 404 forbiddenSearch

      it "should authenticate signin using stored signup password hash" $ do
        signinResponse <- performSigninNoBody baseUsername basePassword
        assertStatusCode "Signin should succeed with valid credentials" 200 signinResponse

        invalidSigninResponse <- performSignin baseUsername "wrongpasswordbad"
        assertStatusCode "Signin should reject invalid password" 401 invalidSigninResponse

      it "should set secure session cookie attributes on signin" $ do
        signinResponse <- performSignin baseUsername basePassword
        assertStatusCode "Signin should succeed" 200 signinResponse
        let setCookie = BS.unpack <$> getFirstSetCookie signinResponse
        case setCookie of
          Nothing -> assertFailure "Expected Set-Cookie header"
          Just cookieHeader -> do
            assertBool "Cookie should be HttpOnly" ("HttpOnly" `isInfixOf` cookieHeader)
            assertBool "Cookie should be Secure" ("Secure" `isInfixOf` cookieHeader)
            assertBool "Cookie should set SameSite=Lax" ("SameSite=Lax" `isInfixOf` cookieHeader)

      it "should expire cookie on signout and support all=true revocation" $ do
        cookie1 <- signinOnly baseUsername basePassword
        cookie2 <- signinOnly baseUsername basePassword

        signoutReq <- parseRequest "POST http://localhost:8081/api/signout?all=true"
        signoutResponse <- httpBS $ setRequestMethod "POST"
                                  $ setRequestHeader "Cookie" [BS.pack cookie1]
                                  signoutReq
        assertStatusCode "Signout should succeed" 200 signoutResponse
        let signoutSetCookie = BS.unpack <$> getFirstSetCookie signoutResponse
        case signoutSetCookie of
          Nothing -> assertFailure "Expected Set-Cookie header on signout"
          Just cookieHeader -> do
            assertBool "Expired cookie should set Max-Age=0" ("Max-Age=0" `isInfixOf` cookieHeader)

        protectedReq <- parseRequest "GET http://localhost:8081/api/note"
        protectedResponse <- httpBS $ setRequestMethod "GET"
                                 $ setRequestHeader "Cookie" [BS.pack cookie2]
                                 protectedReq
        assertStatusCode "all=true should revoke sibling sessions" 401 protectedResponse

      it "should enforce signup rate limiting" $ do
        uniquenessSuffix <- round <$> getPOSIXTime
        statusCodes <- mapM (\i -> do
            let signupPayload = object [ "username" .= ("rl-" ++ show uniquenessSuffix ++ "-" ++ show i)
                                       , "password" .= ("averystrongpass" :: String)
                                       ]
            signupReq <- parseRequest "POST http://localhost:8081/api/signup"
            signupResponse <- httpNoBody $ setRequestMethod "POST"
                                      $ setRequestHeader "Content-Type" ["application/json"]
                                      $ setRequestBodyJSON signupPayload signupReq
            pure (getResponseStatusCode signupResponse)
          ) [1..8]
        assertBool "Expected at least one blocked signup due to rate limiting" (400 `elem` statusCodes)

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



getFirstSetCookie :: Response a -> Maybe ByteString
getFirstSetCookie response =
  case [v | (k, v) <- getResponseHeaders response, BS.map toLower (original k) == "set-cookie"] of
    [] -> Nothing
    (x:_) -> Just x

extractCookiePair :: ByteString -> String
extractCookiePair setCookieHeader =
  BS.unpack (cookieName <> "=" <> unquotedCookieValue)
  where
    cookiePair = BS.takeWhile (/= ';') setCookieHeader
    (cookieName, valueWithEq) = BS.break (== '=') cookiePair
    cookieValue = BS.drop 1 valueWithEq
    unquotedCookieValue
      | BS.length cookieValue >= 2 && BS.head cookieValue == '"' && BS.last cookieValue == '"' = BS.init (BS.tail cookieValue)
      | otherwise = cookieValue

signupAndSignin :: String -> String -> IO String
signupAndSignin username password = do
  signupReq <- parseRequest "POST http://localhost:8081/api/signup"
  signupResponse <- httpNoBody $ setRequestMethod "POST"
                $ setRequestHeader "Content-Type" ["application/json"]
                $ setRequestBodyJSON (authPayload username password) signupReq
  assertStatusCode "Signup should succeed" 200 signupResponse
  signinOnly username password

signinOnly :: String -> String -> IO String
signinOnly username password = do
  signinResponse <- performSignin username password
  assertStatusCode "Signin should succeed" 200 signinResponse
  case getFirstSetCookie signinResponse of
    Nothing -> assertFailure "Expected Set-Cookie header" >> pure ""
    Just header -> pure (extractCookiePair header)

authPayload :: String -> String -> Value
authPayload username password =
  object [ "username" .= username
         , "password" .= password
         ]

performSigninNoBody :: String -> String -> IO (Response ())
performSigninNoBody = performSigninWith httpNoBody

performSignin :: String -> String -> IO (Response ByteString)
performSignin = performSigninWith httpBS

performSigninWith :: (Request -> IO (Response a)) -> String -> String -> IO (Response a)
performSigninWith send username password = do
  signinReq <- parseRequest "POST http://localhost:8081/api/signin"
  send $ setRequestMethod "POST"
      $ setRequestHeader "Content-Type" ["application/json"]
      $ setRequestBodyJSON (authPayload username password) signinReq

runCrudLifecycle
  :: ( Content contentType
     , RequestType GET endpointType () [Identifiable contentType]
     , RequestType POST endpointType contentType StorageId
     , RequestType PUT endpointType (Identifiable contentType) StorageId
     )
  => String
  -> endpointType
  -> contentType
  -> contentType
  -> Expectation
runCrudLifecycle cookie endpoint initialContent updatedContent = do
  assertNoItemAtEndpoint cookie endpoint
  createNewContent cookie endpoint initialContent
  [createdItem] <- assertGetWithContent cookie endpoint initialContent
  modifyItem cookie endpoint $ Identifiable (storageId createdItem) updatedContent
  [updatedItem] <- assertGetWithContent cookie endpoint updatedContent
  deleteItem cookie endpoint $ (id . storageId) updatedItem
  assertNoItemAtEndpoint cookie endpoint

assertNoItemAtEndpoint :: (Content contentType, RequestType GET endpointType () [Identifiable contentType]) => String -> endpointType -> Expectation
assertNoItemAtEndpoint cookie endpoint = do
  getResponse :: Response [Identifiable contentType] <- sendRequestWithJSONBodyImplWithCookie (Just cookie) GET endpoint ()
  assertNoNoteInResponse "Failed to start with an empty server" getResponse

createNewContent :: (Content contentType, RequestType POST endpointType contentType StorageId) => String -> endpointType -> contentType -> Expectation
createNewContent cookie endpoint content = do
  postResponse :: Response StorageId <- sendRequestWithJSONBodyImplWithCookie (Just cookie) POST endpoint content
  assertStatusCode200 ("Failed to create item" ++ show content) postResponse

createNewContentReturningId :: (Content contentType, RequestType POST endpointType contentType StorageId) => String -> endpointType -> contentType -> IO String
createNewContentReturningId cookie endpoint content = do
  postResponse :: Response StorageId <- sendRequestWithJSONBodyImplWithCookie (Just cookie) POST endpoint content
  assertStatusCode200 ("Failed to create item" ++ show content) postResponse
  pure (id $ getResponseBody postResponse)

assertGetWithContent :: (Content contentType, RequestType GET endpointType () [Identifiable contentType]) => String -> endpointType -> contentType -> IO [Identifiable contentType]
assertGetWithContent cookie endpoint expectedContent = do
  getResponse <- sendRequestWithJSONBodyImplWithCookie (Just cookie) GET endpoint ()
  assertWithFoundContent ("Failed to retrieve created content " ++ show expectedContent) [expectedContent] getResponse

modifyItem :: (Content contentType, RequestType PUT endpointType (Identifiable contentType) StorageId) => String -> endpointType -> Identifiable contentType -> Expectation
modifyItem cookie endpoint update = do
  putResponse :: Response StorageId <- sendRequestWithJSONBodyImplWithCookie (Just cookie) PUT endpoint update
  assertStatusCode200 ("Failed to apply modification " ++ show update) putResponse

deleteItem :: Endpoint a => String -> a -> String -> Expectation
deleteItem cookie endpoint idToDelete = do
  req <- parseRequest ("DELETE http://localhost:8081" ++ getEndpoint endpoint ++ "/" ++ idToDelete)
  let deleteReq = setRequestMethod "DELETE"
                $ setRequestHeader "Cookie" [BS.pack cookie] req
  deleteResponse <- httpBS deleteReq
  assertStatusCode200 ("Failed to delete item" ++ show idToDelete) deleteResponse

restoreItem :: Endpoint a => String -> a -> String -> IO (Response Value)
restoreItem cookie endpoint idToRestore = do
  req <- parseRequest ("POST http://localhost:8081" ++ getEndpoint endpoint ++ "/" ++ idToRestore ++ "/restore")
  httpJSON $ setRequestMethod "POST"
          $ setRequestHeader "Cookie" [BS.pack cookie]
          $ setRequestHeader "Content-Type" ["application/json"]
          $ setRequestBodyJSON (object []) req

restoreItemRaw :: Endpoint a => String -> a -> String -> IO (Response ByteString)
restoreItemRaw cookie endpoint idToRestore = do
  req <- parseRequest ("POST http://localhost:8081" ++ getEndpoint endpoint ++ "/" ++ idToRestore ++ "/restore")
  httpBS $ setRequestMethod "POST"
       $ setRequestHeader "Cookie" [BS.pack cookie]
       $ setRequestHeader "Content-Type" ["application/json"]
       $ setRequestBodyJSON (object []) req

purgeItem :: Endpoint a => String -> a -> String -> IO (Response Value)
purgeItem cookie endpoint idToPurge = do
  req <- parseRequest ("DELETE http://localhost:8081" ++ getEndpoint endpoint ++ "/" ++ idToPurge ++ "/purge")
  httpJSON $ setRequestMethod "DELETE"
          $ setRequestHeader "Cookie" [BS.pack cookie] req

assertWithFoundContent :: Content a => String -> [a] -> Response [Identifiable a] -> IO [Identifiable a]
assertWithFoundContent errorPrefix expectedContents response = do
  assertStatusCode200 errorPrefix response
  assertEqual (errorPrefix ++ "\n\tExpected notes with contents:\n\t" ++ show expectedContents) expectedContents  (map content responseItems)
  return responseItems
  where
    responseItems = getResponseBody response

sendRequestWithJSONBodyImpl :: (RequestType methodType endpointType requestType responseType) =>
  methodType -> endpointType -> requestType -> IO (Response responseType)
sendRequestWithJSONBodyImpl =
    sendRequestWithJSONBodyImplWithCookie Nothing

sendRequestWithJSONBodyImplWithCookie :: (RequestType methodType endpointType requestType responseType) =>
  Maybe String -> methodType -> endpointType -> requestType -> IO (Response responseType)
sendRequestWithJSONBodyImplWithCookie mCookie method endpoint body = do
    req <- parseRequest ("http://localhost:8081" ++ getEndpoint endpoint)
    let withCookie :: Request -> Request
        withCookie = maybe (\req' -> req') (\cookie -> setRequestHeader "Cookie" [BS.pack cookie]) mCookie
    httpJSON $ (setRequestMethod (getMethod method) . withCookie . setRequestHeader "Content-Type" ["application/json"] . setRequestBodyJSON body) req


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

assertStatusCode :: String -> Int -> Response a -> Assertion
assertStatusCode errorPrefix expectedStatus response = assertEqual (errorPrefix ++ "Expected status code") expectedStatus (getResponseStatusCode response)

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
    getEndpoint NoteEndpoint = "/api/note"

data ChecklistEndpoint = ChecklistEndpoint
instance Endpoint ChecklistEndpoint where
    getEndpoint ChecklistEndpoint = "/api/checklist"

data AgendaEndpoint = AgendaEndpoint
instance Endpoint AgendaEndpoint where
    getEndpoint AgendaEndpoint = "/api/agenda"

class (ToJSON requestType, FromJSON responseType, Endpoint endpoint, Method methodType) => RequestType methodType endpoint requestType responseType | endpoint methodType -> requestType, endpoint methodType requestType -> responseType where
    sendRequestWithJSONBody :: endpoint -> methodType -> requestType -> IO (Response responseType)

instance RequestType GET NoteEndpoint () [Identifiable NoteContent] where
    sendRequestWithJSONBody endpoint _ = sendRequestWithJSONBodyImpl GET endpoint

instance RequestType POST NoteEndpoint NoteContent StorageId where
    sendRequestWithJSONBody endpoint _ = sendRequestWithJSONBodyImpl POST endpoint

instance RequestType PUT NoteEndpoint (Identifiable NoteContent) StorageId where
    sendRequestWithJSONBody endpoint _ = sendRequestWithJSONBodyImpl PUT endpoint

instance RequestType GET ChecklistEndpoint () [Identifiable ChecklistContent] where
    sendRequestWithJSONBody endpoint _ = sendRequestWithJSONBodyImpl GET endpoint

instance RequestType POST ChecklistEndpoint ChecklistContent StorageId where
    sendRequestWithJSONBody endpoint _ = sendRequestWithJSONBodyImpl POST endpoint

instance RequestType PUT ChecklistEndpoint (Identifiable ChecklistContent) StorageId where
    sendRequestWithJSONBody endpoint _ = sendRequestWithJSONBodyImpl PUT endpoint

instance RequestType GET AgendaEndpoint () [Identifiable AgendaContent] where
    sendRequestWithJSONBody endpoint _ = sendRequestWithJSONBodyImpl GET endpoint

instance RequestType POST AgendaEndpoint AgendaContent StorageId where
    sendRequestWithJSONBody endpoint _ = sendRequestWithJSONBodyImpl POST endpoint

instance RequestType PUT AgendaEndpoint (Identifiable AgendaContent) StorageId where
    sendRequestWithJSONBody endpoint _ = sendRequestWithJSONBodyImpl PUT endpoint

postTask :: String -> String -> TaskContent -> IO (Response StorageId)
postTask cookie agendaId payload = do
  req <- parseRequest ("POST http://localhost:8081/api/agenda/" ++ agendaId ++ "/task")
  let payloadWithAgenda = payload { taskAgendaId = agendaId }
  httpJSON
    $ setRequestMethod "POST"
    $ setRequestHeader "Cookie" [BS.pack cookie]
    $ setRequestHeader "Content-Type" ["application/json"]
    $ setRequestBodyJSON payloadWithAgenda req

getAgendaSearch :: String -> String -> [(String, String)] -> IO (Response [Identifiable TaskContent])
getAgendaSearch cookie agendaId queryParams = do
  req <- parseRequest ("GET http://localhost:8081/api/agenda/" ++ agendaId ++ "/search")
  let reqWithQuery = setRequestQueryString (map (\(k, v) -> (BS.pack k, Just (BS.pack v))) queryParams) req
  httpJSON
    $ setRequestMethod "GET"
    $ setRequestHeader "Cookie" [BS.pack cookie]
    $ reqWithQuery

getAgendaSearchRaw :: String -> String -> [(String, String)] -> IO (Response ByteString)
getAgendaSearchRaw cookie agendaId queryParams = do
  req <- parseRequest ("GET http://localhost:8081/api/agenda/" ++ agendaId ++ "/search")
  let reqWithQuery = setRequestQueryString (map (\(k, v) -> (BS.pack k, Just (BS.pack v))) queryParams) req
  httpBS
    $ setRequestMethod "GET"
    $ setRequestHeader "Cookie" [BS.pack cookie]
    $ reqWithQuery

getAgendaConflicts :: String -> String -> IO (Response [TaskConflict])
getAgendaConflicts cookie agendaId = do
  req <- parseRequest ("GET http://localhost:8081/api/agenda/" ++ agendaId ++ "/conflicts")
  httpJSON
    $ setRequestMethod "GET"
    $ setRequestHeader "Cookie" [BS.pack cookie] req
