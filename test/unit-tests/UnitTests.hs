{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module UnitTests (runUnitTests) where

import Prelude hiding(id)
import Test.HUnit.Lang
import Test.HUnit.Base(Counts(..), (@?), (~:), test, assertBool, assertFailure)
import Test.HUnit.Text (runTestTT)
import Crud (CRUDEngine(..), DiskFileStorageConfig(..), Error(..), CrudModificationException(..), CrudReadException(..), CrudWriteException(..))
import Model (Identifiable(..), NoteContent(..), ChecklistContent(..), ChecklistItem(..), StorageId(..), AgendaContent(..), TaskContent(..), TaskReminder(..), RecurrenceRule(..)) 
import System.Directory (removeDirectoryRecursive, createDirectory, createDirectoryIfMissing, doesDirectoryExist, doesFileExist, listDirectory, getCurrentDirectory, getPermissions, Permissions(..))
import Data.Maybe (fromJust)
import Data.Either (isRight)
import Data.List ((\\))
import Control.Monad (when)
import Control.Monad.Trans.Maybe (MaybeT, runMaybeT)
import Control.Monad.Trans.Except (runExceptT)
import Control.Exception (finally)
import Control.Concurrent (threadDelay)
import System.Exit (exitSuccess, exitFailure)
import NoteCrud (NoteServiceConfig(..))
import ChecklistCrud (ChecklistServiceConfig(..))
import AgendaCrud (AgendaServiceConfig(..))
import TaskCrud (TaskServiceConfig(..))
import qualified Auth
import qualified Session
import qualified Data.Text as Text
import Data.Time.Clock.POSIX (getPOSIXTime)
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified CrudStorage

runUnitTests :: IO ()
runUnitTests = runTestTTAndExit $ test [noteServiceTests, checklistServiceTests, agendaServiceTests, taskServiceTests, signupValidationTests, signinValidationTests, sessionTests]

runTestTTAndExit tests = do
  c <- runTestTT tests
  if (errors c == 0) && (failures c == 0)
    then exitSuccess
    else exitFailure

noteServiceTests = test [ "Creating a note should create a new file in storage directory" ~: withEmptyDir noteServiceConfig createTest
                        , "Getting all notes on an empty storage directory should give an empty list" ~: getEmptyDirTest noteServiceConfig
                        , "Creating then getting all notes should give back the note" ~: withEmptyDir noteServiceConfig createManyThenGetTest
                        , "Creating then deleting all notes should give back no note" ~: withEmptyDir noteServiceConfig createManyThenDeleteAllTest
                        , "Deleting on an empty storage should always be an error" ~: deleteNoteOnEmptyDir noteServiceConfig
                        , "Modifying an existing note should give back the modified note" ~: withEmptyDir noteServiceConfig modifyAnExistingNote
                        , "Modifying an non-existing note should give back a NotFoundError" ~: withEmptyDir noteServiceConfig modifyANonExistingNote
                        , "Modifying an existing note but with wrong current version should give back a NotCurrentVersion error" ~: withEmptyDir noteServiceConfig modifyWrongCurrentVersion
                        , "Deleted note should be restorable from trash" ~: withEmptyDir noteServiceConfig restoreDeletedItemTest
                        , "Purged note should not be restorable" ~: withEmptyDir noteServiceConfig purgeDeletedItemTest
                        ]

checklistServiceTests = test [ "Creating a checklist should create a new file in storage directory" ~: withEmptyDir checklistServiceConfig createTest
                             , "Getting all checklists on an empty storage directory should give an empty list" ~: getEmptyDirTest checklistServiceConfig 
                             , "Creating then getting all checklists should give back the note" ~: withEmptyDir checklistServiceConfig createManyThenGetTest
                             , "Creating then deleting all checklists should give back no note" ~: withEmptyDir checklistServiceConfig createManyThenDeleteAllTest
                             , "Deleting on an empty storage should always be an error" ~: deleteNoteOnEmptyDir checklistServiceConfig 
                             , "Modifying an existing checklist should give back the modified note" ~: withEmptyDir checklistServiceConfig modifyAnExistingNote
                             , "Modifying an non-existing checklist should give back a NotFoundError" ~: withEmptyDir checklistServiceConfig modifyANonExistingNote
                             , "Modifying an existing checklist but with wrong current version should give back a NotCurrentVersion error" ~: withEmptyDir checklistServiceConfig modifyWrongCurrentVersion
                             ]

agendaServiceTests = test [ "Creating an agenda should create a new file in storage directory" ~: withEmptyDir agendaServiceConfig createTest
                          , "Getting all agendas on an empty storage directory should give an empty list" ~: getEmptyDirTest agendaServiceConfig
                          , "Creating then getting all agendas should give back the agenda" ~: withEmptyDir agendaServiceConfig createManyThenGetTest
                          , "Creating then deleting all agendas should give back no agenda" ~: withEmptyDir agendaServiceConfig createManyThenDeleteAllTest
                          , "Modifying an existing agenda should give back the modified agenda" ~: withEmptyDir agendaServiceConfig modifyAnExistingNote
                          ]

taskServiceTests = test [ "Creating a task should create a new file in storage directory" ~: withEmptyDir taskServiceConfig createTest
                        , "Getting all tasks on an empty storage directory should give an empty list" ~: getEmptyDirTest taskServiceConfig
                        , "Creating then getting all tasks should give back the task" ~: withEmptyDir taskServiceConfig createManyThenGetTest
                        , "Creating then deleting all tasks should give back no task" ~: withEmptyDir taskServiceConfig createManyThenDeleteAllTest
                        , "Modifying an existing task should give back the modified task" ~: withEmptyDir taskServiceConfig modifyAnExistingNote
                        ]

createTest :: ContentGen crudConfig a => crudConfig -> IO ()
createTest config = do
    Right storageId <- runExceptT $ postItem config (generateExample config 1)
    dirContent <- retrieveContentInDir config
    let sid = case storageId of
                StorageId { id = sid' } -> sid'
    filePath config storageId `elem` dirContent @? "expected " ++ show dirContent ++ " to contain " ++ show (sid ++ ".txt")

getEmptyDirTest :: CRUDEngine crudConfig a => crudConfig -> IO ()
getEmptyDirTest config = withEmptyDir config (\conf ->do
    Right [] <- runExceptT $ getItems config
    return ())

createManyThenGetTest :: ContentGen crudConfig a => crudConfig -> IO ()
createManyThenGetTest config = do
    maybecreationIds <- mapM (runExceptT . postItem config) itemExamples
    let creationIds = map fromRight maybecreationIds
    Right potentialItems <- runExceptT $ getItems config
    items <- mapM (fmap fromRight . runExceptT) potentialItems
    assertEqual "Their should be one note retrieved" (length creationIds) (length items)
    assertEqualWithoutOrder "RetrievedNote should have the same id as the created note" creationIds (map storageId items)
    assertEqualWithoutOrder "RetrievedNote should have the same content as the created note"  itemExamples (map content items)
    where itemExamples = map (generateExample config) [1..5]

createManyThenDeleteAllTest :: ContentGen crudConfig a => crudConfig -> IO ()
createManyThenDeleteAllTest config = do
    eitherCreationIds <- mapM ((runExceptT . postItem config) . generateExample config) [1..5]
    let noteIds = map fromRight eitherCreationIds
    results <- mapM (runExceptT . delItem config . id) noteIds
    assertBool "all deletions should be a success" (all isRight results)
    dirContent <- retrieveContentInDir config
    assertEqual "note storage should be empty" [] dirContent

fromRight (Right a) = a
fromRight (Left _) = undefined

deleteNoteOnEmptyDir :: CRUDEngine crudConfig a => crudConfig -> IO ()
deleteNoteOnEmptyDir config = withEmptyDir noteServiceConfig (\conf -> do
        Right () <- runExceptT $ delItem conf "ArbitraryNoteId"
        return ())

modifyAnExistingNote :: ContentGen crudConfig a => crudConfig -> IO ()
modifyAnExistingNote config = do
    Right creationId <- runExceptT $ postItem config (generateExample config 1)
    Right newcreationId <- runExceptT $ putItem config (arbitraryItemUpdate creationId)
    assertEqual "Updated note id should be the same as original note" (id creationId) (id newcreationId)
    assertNotEqual "Updated note version should be different from original note's version" (version creationId) (version newcreationId)
    where
        arbitraryItemUpdate creationId = Identifiable creationId (generateExample config 20)

modifyANonExistingNote :: ContentGen crudConfig a => crudConfig -> IO ()
modifyANonExistingNote config = do
    Left error <- runExceptT $ putItem config arbitraryItemUpdate
    assertIsAReadingException "Error should be a NotFound of the requested id" error
    where
        arbitraryItemUpdate = Identifiable StorageId { id = "id", version = "" } (generateExample config 1)
        assertIsAReadingException s err = case err of
            CrudModificationReadingException (IOReadException _) -> assertBool s True
            _                                                    -> assertBool s False

modifyWrongCurrentVersion :: ContentGen crudConfig a => crudConfig -> IO ()
modifyWrongCurrentVersion config = do
    Right creationId <- runExceptT $ postItem config (generateExample config 1)
    Left error <- runExceptT $ putItem config (wrongVersionItemUpdate creationId)
    assertEqual "Error should be a WrongVersion of the storageId requested" (NotCurrentVersion $ wrongVersionStorageId creationId) error
    where
        wrongVersionStorageId StorageId { id = creationId, version = creationVersion } =
            StorageId { id = creationId, version = creationVersion ++ "make it wrong" }
        wrongVersionItemUpdate creationStorageId =
            Identifiable (wrongVersionStorageId creationStorageId) (generateExample config 20)

restoreDeletedItemTest :: ContentGen crudConfig a => crudConfig -> IO ()
restoreDeletedItemTest config = do
    Right creationId <- runExceptT $ postItem config (generateExample config 1)
    Right () <- runExceptT $ delItem config (id creationId)
    Right () <- runExceptT $ CrudStorage.restoreItem config (id creationId)
    Right potentialItems <- runExceptT $ getItems config
    items <- mapM (fmap fromRight . runExceptT) potentialItems
    assertEqual "Restored item should be visible again in active storage" 1 (length items)

purgeDeletedItemTest :: ContentGen crudConfig a => crudConfig -> IO ()
purgeDeletedItemTest config = do
    Right creationId <- runExceptT $ postItem config (generateExample config 1)
    Right () <- runExceptT $ delItem config (id creationId)
    Right () <- runExceptT $ CrudStorage.purgeItem config (id creationId)
    Left _ <- runExceptT $ CrudStorage.restoreItem config (id creationId)
    Right potentialItems <- runExceptT $ getItems config
    items <- mapM (fmap fromRight . runExceptT) potentialItems
    assertEqual "Purged item should stay absent from active storage" [] items

assertEqualWithoutOrder :: (Show a, Eq a) => String -> [a] -> [a] -> IO ()
assertEqualWithoutOrder s as bs = do
    assertBool (s ++ "\n\t" ++ show as ++ " should be equal in " ++ show bs) (null (as \\ bs))
    assertBool (s ++ "\n\t" ++ show bs ++ " should be equal in " ++ show as) (null (bs \\ as))

assertNotEqual s a b = assertBool s (a /= b)

filePath :: CRUDEngine crudConfig a => crudConfig -> StorageId -> String
filePath config storageId = getStorageDirectoryPath config ++ id storageId ++ ".txt"

retrieveContentInDir :: CRUDEngine crudConfig a => crudConfig -> IO [FilePath]
retrieveContentInDir config = do
    dirFileNames <- listDirectory dirPath
    return $ map (dirPath ++) dirFileNames
    where dirPath = getStorageDirectoryPath config

checklistServiceConfig :: ChecklistServiceConfig
checklistServiceConfig = ChecklistServiceConfig "target/.foucl/data/checklist/"

agendaServiceConfig :: AgendaServiceConfig
agendaServiceConfig = AgendaServiceConfig "target/.foucl/data/agenda/"

taskServiceConfig :: TaskServiceConfig
taskServiceConfig = TaskServiceConfig "target/.foucl/data/task/"

noteServiceConfig :: NoteServiceConfig
noteServiceConfig = NoteServiceConfig "target/.foucl/data/note/"

withEmptyDir :: CRUDEngine crudConfig a => crudConfig -> (crudConfig -> IO ()) -> IO ()
withEmptyDir config _test = do
    exists <- doesDirectoryExist dirPath
    if exists
        then do
            removeDirectoryRecursive dirPath
            _test config
        else do
            _test config
    where dirPath = getStorageDirectoryPath config

getStorageDirectoryPath :: CRUDEngine crudConfig a => crudConfig -> String
getStorageDirectoryPath config = "target/.foucl/data/" ++ crudTypeDenomination config ++ "/"

class CRUDEngine crudType a => ContentGen crudType a where
    generateExample :: crudType -> Int -> a

instance ContentGen NoteServiceConfig NoteContent where
    generateExample _ i = NoteContent { title = Just ("ExampleNoteTitle " ++ show i), noteContent = "Arbitrary note content " ++ show i } 

instance ContentGen ChecklistServiceConfig ChecklistContent where
    generateExample _ i = ChecklistContent { name = "ExampleNoteTitle " ++ show i, items = [ ChecklistItem { label = "Checklist label " ++ show i ++ "-" ++ show k, checked = even k } | k <- [1..5] ] }

instance ContentGen AgendaServiceConfig AgendaContent where
    generateExample _ i = AgendaContent { agendaName = "Agenda " ++ show i
                                        , agendaDescription = Just ("Agenda description " ++ show i)
                                        , agendaTimezone = "UTC"
                                        }

instance ContentGen TaskServiceConfig TaskContent where
    generateExample _ i = TaskContent { taskAgendaId = "agenda-" ++ show i
                                      , taskTitle = "Task " ++ show i
                                      , taskDescription = Just ("Task description " ++ show i)
                                      , taskStatus = "todo"
                                      , taskScheduledStartUtc = "2026-03-01T09:00:00Z"
                                      , taskScheduledEndUtc = "2026-03-01T12:00:00Z"
                                      , taskTimezone = "UTC"
                                      , taskEstimatedDurationMinutes = Just 45
                                      , taskTags = ["pro", "test-" ++ show i]
                                      , taskRecurrence = Just RecurrenceRule { frequency = "weekly"
                                                                             , interval = 1
                                                                             , untilUtc = Nothing
                                                                             , count = Nothing
                                                                             , byWeekday = Just ["MO"]
                                                                             , byMonthday = Nothing
                                                                         }
                                      , taskReminders = [TaskReminder { offsetMinutesBefore = 30 }]
                                      , taskDeletedAt = Nothing
                                      }

signupValidationTests = test [ "Signup should reject short passwords" ~: rejectShortPassword
                             , "Signup should reject invalid usernames" ~: rejectInvalidUsername
                             , "Signup should create a user profile on valid payload" ~: signupNominal
                             , "Signup should fail when user already exists" ~: signupAlreadyExistingUser
                             , "Signup should create restricted file permissions" ~: signupUsesRestrictedPermissions
                             ]

rejectShortPassword :: IO ()
rejectShortPassword = do
    result <- runExceptT $ Auth.createUser $ Auth.AuthRequest { Auth.username = "valid-user", Auth.password = Text.pack "short" }
    case result of
      Left (Auth.BadRequest Auth.PasswordTooShort) -> assertBool "PasswordTooShort expected" True
      _ -> assertFailure "Expected PasswordTooShort"

rejectInvalidUsername :: IO ()
rejectInvalidUsername = do
    result <- runExceptT $ Auth.createUser $ Auth.AuthRequest { Auth.username = "bad/name", Auth.password = Text.pack "averystrongpass" }
    case result of
      Left (Auth.BadRequest Auth.UsernameDoesNotRespectPattern) -> assertBool "UsernameDoesNotRespectPattern expected" True
      _ -> assertFailure "Expected UsernameDoesNotRespectPattern"


signupNominal :: IO ()
signupNominal = withCleanSignupUser "signup-nominal-user" $ \username -> do
    let validPassword = Text.pack "averystrongpass"
    result <- runExceptT $ Auth.createUser $ Auth.AuthRequest { Auth.username = username, Auth.password = validPassword }
    case result of
      Right () -> do
        cd <- getCurrentDirectory
        let profilePath = cd ++ "/data/users/" ++ username ++ "/profile.json"
        profileExists <- doesFileExist profilePath
        assertBool "Expected signup profile file to exist" profileExists
      _ -> assertFailure "Expected signup success"

signupAlreadyExistingUser :: IO ()
signupAlreadyExistingUser = withCleanSignupUser "signup-existing-user" $ \username -> do
    let validPassword = Text.pack "averystrongpass"
    firstTry <- runExceptT $ Auth.createUser $ Auth.AuthRequest { Auth.username = username, Auth.password = validPassword }
    case firstTry of
      Right () -> do
        secondTry <- runExceptT $ Auth.createUser $ Auth.AuthRequest { Auth.username = username, Auth.password = validPassword }
        case secondTry of
          Left Auth.UserAlreadyExists -> assertBool "UserAlreadyExists expected" True
          _ -> assertFailure "Expected UserAlreadyExists"
      _ -> assertFailure "Expected first signup to succeed"



signupUsesRestrictedPermissions :: IO ()
signupUsesRestrictedPermissions = withCleanSignupUser "signup-permissions-user" $ \username -> do
    let validPassword = Text.pack "averystrongpass"
    result <- runExceptT $ Auth.createUser $ Auth.AuthRequest { Auth.username = username, Auth.password = validPassword }
    case result of
      Right () -> do
        cd <- getCurrentDirectory
        let userDir = cd ++ "/data/users/" ++ username
            profilePath = userDir ++ "/profile.json"
        userDirPermissions <- getPermissions userDir
        profilePermissions <- getPermissions profilePath
        assertBool "Expected user dir to be owner-readable" (readable userDirPermissions)
        assertBool "Expected user dir to be owner-writable" (writable userDirPermissions)
        assertBool "Expected user dir to be searchable" (searchable userDirPermissions)
        assertBool "Expected profile file to be owner-readable" (readable profilePermissions)
        assertBool "Expected profile file to be owner-writable" (writable profilePermissions)
      _ -> assertFailure "Expected signup success"


signinValidationTests = test [ "Signin should authenticate with valid credentials" ~: signinNominal
                             , "Signin should reject invalid password" ~: signinRejectsInvalidPassword
                             , "Signin should reject unknown users" ~: signinRejectsUnknownUser
                             ]

signinNominal :: IO ()
signinNominal = withCleanSignupUser "signin-nominal-user" $ \username -> do
    let validPassword = Text.pack "averystrongpass"
    signupResult <- runExceptT $ Auth.createUser $ Auth.AuthRequest { Auth.username = username, Auth.password = validPassword }
    case signupResult of
      Right () -> do
        signinResult <- runExceptT $ Auth.signinUser $ Auth.AuthRequest { Auth.username = username, Auth.password = validPassword }
        case signinResult of
          Right () -> assertBool "Signin should succeed" True
          _ -> assertFailure "Expected successful signin"
      _ -> assertFailure "Expected signup success"

signinRejectsInvalidPassword :: IO ()
signinRejectsInvalidPassword = withCleanSignupUser "signin-invalid-password-user" $ \username -> do
    let validPassword = Text.pack "averystrongpass"
    signupResult <- runExceptT $ Auth.createUser $ Auth.AuthRequest { Auth.username = username, Auth.password = validPassword }
    case signupResult of
      Right () -> do
        signinResult <- runExceptT $ Auth.signinUser $ Auth.AuthRequest { Auth.username = username, Auth.password = Text.pack "wrongpassword!!" }
        case signinResult of
          Left Auth.InvalidCredentials -> assertBool "InvalidCredentials expected" True
          _ -> assertFailure "Expected InvalidCredentials"
      _ -> assertFailure "Expected signup success"

signinRejectsUnknownUser :: IO ()
signinRejectsUnknownUser = do
    signinResult <- runExceptT $ Auth.signinUser $ Auth.AuthRequest { Auth.username = "signin-unknown-user", Auth.password = Text.pack "averystrongpass" }
    case signinResult of
      Left Auth.InvalidCredentials -> assertBool "InvalidCredentials expected" True
      _ -> assertFailure "Expected InvalidCredentials"

withCleanSignupUser :: String -> (String -> IO ()) -> IO ()
withCleanSignupUser username action = do
    cd <- getCurrentDirectory
    let usersDir = cd ++ "/data/users"
        userDir = usersDir ++ "/" ++ username
    usersDirCreatedByTest <- ensureUsersDir usersDir
    cleanupSignupUserDir userDir
    action username `finally` do
      cleanupSignupUserDir userDir
      cleanupUsersDirIfCreatedByTest usersDirCreatedByTest usersDir

ensureUsersDir :: FilePath -> IO Bool
ensureUsersDir usersDir = do
    usersExists <- doesDirectoryExist usersDir
    if usersExists
      then pure False
      else createDirectoryIfMissing True usersDir >> pure True

cleanupUsersDirIfCreatedByTest :: Bool -> FilePath -> IO ()
cleanupUsersDirIfCreatedByTest usersDirCreatedByTest usersDir =
    when usersDirCreatedByTest $ removeDirectoryRecursive usersDir

cleanupSignupUserDir :: FilePath -> IO ()
cleanupSignupUserDir userDir = do
    userExists <- doesDirectoryExist userDir
    when userExists $ removeDirectoryRecursive userDir


sessionTests = test [ "Signed token should reject tampering" ~: signedTokenRejectsTampering
                    , "Revoked session should be rejected" ~: revokedSessionIsRejected
                    , "Idle timeout should expire session" ~: idleTimeoutExpiresSession
                    , "Sliding renewal should extend idle session" ~: slidingRenewalExtendsIdleSession
                    , "Revoking all sessions from one session should revoke sibling sessions" ~: revokeAllSessionsFromSession
                    , "Corrupted session handle should be rejected gracefully" ~: corruptedSessionHandleIsRejected
                    ]

signedTokenRejectsTampering :: IO ()
signedTokenRejectsTampering = do
    let token = Session.signSessionId "secret" "sid-1"
        tampered = token ++ "00"
    case Session.verifyAndExtractSessionId "secret" tampered of
      Nothing -> assertBool "Tampered token should be rejected" True
      Just _ -> assertFailure "Tampered token should not validate"

revokedSessionIsRejected :: IO ()
revokedSessionIsRejected = withSessionStore "revoked" 30 30 $ \store -> do
    sid <- Session.createSessionForUser store "user-revoked"
    _ <- Session.revokeSession store sid
    resolved <- Session.resolveSession store sid
    case resolved of
      Nothing -> assertBool "Revoked session should not resolve" True
      Just _ -> assertFailure "Expected revoked session to be rejected"

idleTimeoutExpiresSession :: IO ()
idleTimeoutExpiresSession = withSessionStore "idle-expiry" 30 1 $ \store -> do
    sid <- Session.createSessionForUser store "user-idle-expiry"
    threadDelay 1300000
    resolved <- Session.resolveSession store sid
    case resolved of
      Nothing -> assertBool "Session should expire on idle timeout" True
      Just _ -> assertFailure "Expected idle-expired session to be rejected"

slidingRenewalExtendsIdleSession :: IO ()
slidingRenewalExtendsIdleSession = withSessionStore "sliding" 30 1 $ \store -> do
    sid <- Session.createSessionForUser store "user-sliding"
    threadDelay 600000
    firstResolution <- Session.resolveSession store sid
    case firstResolution of
      Nothing -> assertFailure "Expected first session resolution to succeed"
      Just _ -> do
        threadDelay 600000
        secondResolution <- Session.resolveSession store sid
        case secondResolution of
          Nothing -> assertFailure "Expected sliding renewal to keep session active"
          Just _ -> assertBool "Sliding renewal should extend session" True

withSessionStore :: String -> Integer -> Integer -> (Session.SessionStore -> IO ()) -> IO ()
withSessionStore label absoluteTtl idleTtl action =
    withSessionStoreAndDir label absoluteTtl idleTtl (\_ store -> action store)

cleanupSessionDir :: FilePath -> IO ()
cleanupSessionDir baseDir = do
    exists <- doesDirectoryExist baseDir
    when exists $ removeDirectoryRecursive baseDir


revokeAllSessionsFromSession :: IO ()
revokeAllSessionsFromSession = withSessionStore "revoke-all" 30 30 $ \store -> do
    sid1 <- Session.createSessionForUser store "user-revoke-all"
    sid2 <- Session.createSessionForUser store "user-revoke-all"
    _ <- Session.revokeAllForSession store sid1
    resolved1 <- Session.resolveSession store sid1
    resolved2 <- Session.resolveSession store sid2
    case (resolved1, resolved2) of
      (Nothing, Nothing) -> assertBool "All sibling sessions should be revoked" True
      _ -> assertFailure "Expected both sessions to be revoked"

corruptedSessionHandleIsRejected :: IO ()
corruptedSessionHandleIsRejected = withSessionStoreAndDir "corrupted-handle" 30 30 $ \baseDir store -> do
    sid <- Session.createSessionForUser store "user-corrupted-handle"
    let handleFile = baseDir ++ "/handles/" ++ sid ++ ".json"
    BL8.writeFile handleFile (BL8.pack "{not-valid-json")
    resolved <- Session.resolveSession store sid
    case resolved of
      Nothing -> assertBool "Corrupted handle should be treated as invalid" True
      Just _ -> assertFailure "Expected corrupted session handle to be rejected"

withSessionStoreAndDir :: String -> Integer -> Integer -> (FilePath -> Session.SessionStore -> IO ()) -> IO ()
withSessionStoreAndDir label absoluteTtl idleTtl action = do
    cd <- getCurrentDirectory
    nonce <- round . (* 1000000) <$> getPOSIXTime
    let baseDir = cd ++ "/data/test-sessions/" ++ label ++ "-" ++ show (nonce :: Integer)
        sessionConfig = Session.defaultSessionConfig
          { Session.sessionSecret = "unit-test-secret"
          , Session.sessionAbsoluteTtlSeconds = fromInteger absoluteTtl
          , Session.sessionIdleTtlSeconds = fromInteger idleTtl
          }
    cleanupSessionDir baseDir
    store <- Session.mkFileSessionStore baseDir sessionConfig
    action baseDir store `finally` cleanupSessionDir baseDir
