{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import           Control.Exception
import qualified Control.Lens as L
import qualified Control.Monad as M
import           Control.Monad.Reader (ask)
import           Control.Monad.State (get, put)
import           Data.Acid
import           Data.Colour
import           Data.Colour.Names
import           Data.Default.Class
import           Data.IORef
import qualified Data.Map as Map
import           Data.Maybe
import           Data.SafeCopy
import           Data.Typeable
import qualified Graphics.Rendering.Chart as C
import           Graphics.Rendering.Chart.Gtk
import           Graphics.UI.Gtk hiding (get)
import           Graphics.UI.Gtk.ModelView as Model
import qualified Scraper
import           System.Environment (getArgs)
import           System.Glib.UTFString

-- ------------------------------------- Dictionary to hold Title-Song pairs.
type Key = String

type Value = String

data KeyValue = KeyValue !(Map.Map Key Value)
  deriving (Typeable)

-- Generate staff for safecopy.
$(deriveSafeCopy 0 'base ''KeyValue)

-- -------------------------------------- | Add new key to dictionary.
insertKey :: Key -> Value -> Update KeyValue ()
insertKey key value = do
  KeyValue m <- get
  put (KeyValue (Map.insert key value m))

-- | Lookuo for a key in dictionary.
lookupKey :: Key -> Query KeyValue (Maybe Value)
lookupKey key = do
  KeyValue m <- ask
  return (Map.lookup key m)

-- | Get all keys in dictionary.
getKeys :: Query KeyValue [Key]
getKeys = do
  KeyValue m <- ask
  return (Map.keys m)

-- | Remove pair from dictionary by number. Dictionary is ordered, so it is normal.
dropKeyAt :: Int -> Update KeyValue ()
dropKeyAt key = do
  KeyValue m <- get
  put (KeyValue (Map.deleteAt key m))

-- | Update value by key in dictionary.
updateValue :: Key -> Value -> Update KeyValue ()
updateValue key value = do
  KeyValue m <- get
  put
    (KeyValue
       (Map.updateWithKey
          (\k a -> if k == key
                     then return value
                     else Nothing)
          key
          m))

-- Generate staff for acid-state.
$(makeAcidic ''KeyValue ['insertKey, 'lookupKey, 'getKeys, 'dropKeyAt, 'updateValue])

-- -------------------------------------- | Helper function to manually get text from textView.
textViewGetValue :: TextViewClass self => self -> IO String
textViewGetValue tv = do
  buf <- textViewGetBuffer tv
  start <- textBufferGetStartIter buf
  end <- textBufferGetEndIter buf
  textBufferGetText buf start end True

-- | Add new entry to dictionary and GUI, collecting data from text edits.
addNewSong list storage entryTitle textViewSong = do
  title <- entryGetText entryTitle
  M.guard $ not (null title)
  mbKey <- query storage (LookupKey title)
  case mbKey of
    Nothing -> do
      text <- textViewGetValue textViewSong
      update storage (InsertKey title text)
      listStoreAppend list title
      putStrLn ("Add " ++ title ++ " to storage.") -- attention !!!
    Just _ -> putStrLn ("Key '" ++ title ++ "' is already in storage.")

-- | Delete entry from dictionary. Update GUI.
deleteSong list storage tree = do
  sel <- treeSelectionGetSelectedRows tree
  let s = head (head sel)
  in update storage (DropKeyAt s) >> listStoreRemove list s
  return ()

-- | Save pair (title, text) to dictionary.
saveSong list storage tree entryTitle textViewSong = do
  sel <- treeSelectionGetSelectedRows tree
  title <- entryGetText entryTitle
  text <- textViewGetValue textViewSong
  M.guard $ not (null title)
  let s = head (head sel)
  update storage (UpdateValue title text)
  listStoreSetValue list s title
  putStrLn $ "Update text of '" ++ title ++ "'"

-- return () | Download song text from web, and store it in textView.
downloadSong entryLink songBuffer = do
  url <- entryGetText entryLink
  songText <- Scraper.getSongText url
  M.guard $ not (null songText)
  textBufferSetText songBuffer (concat songText)

-- | Directory to keep database.
songsStorageDirectory :: String
songsStorageDirectory = "SongsStorageDir/"

-- | Create GUI, restore database, set listeners to events.
main :: IO ()
main = do
  initGUI
  window <- windowNew
  set window
    [ windowTitle := "SongsKeeper"
    , windowDefaultWidth := 900
    , windowDefaultHeight := 800
    , containerBorderWidth := 10
    ]

  hbMain <- hBoxNew False 0
  containerAdd window hbMain

  -- left: list of titles
  frame <- frameNew
  widgetSetSizeRequest frame 250 (-1)
  boxPackStart hbMain frame PackNatural 0
  -- containerAdd window frame right
  vb_right_part <- vBoxNew False 0
  boxPackStart hbMain vb_right_part PackGrow 20

  -- download panel
  hb_entryLink <- hBoxNew False 0
  boxPackStart vb_right_part hb_entryLink PackNatural 5
  entryLink <- entryNew
  boxPackStart hb_entryLink entryLink PackGrow 5

  hb_buttonDownload <- hBoxNew False 0
  boxPackStart vb_right_part hb_buttonDownload PackNatural 5
  buttonDownload <- buttonNewWithLabel "Download"
  boxPackEnd hb_buttonDownload buttonDownload PackNatural 10

  -- texts from right panel
  hb_controls <- hBoxNew False 0
  boxPackStart vb_right_part hb_controls PackNatural 20

  entryTitle <- entryNew
  boxPackStart vb_right_part entryTitle PackNatural 10

  textViewSong <- textViewNew
  boxPackStart vb_right_part textViewSong PackGrow 10
  textTags <- textTagTableNew
  songBuffer <- textBufferNew $ Just textTags
  textViewSetBuffer textViewSong songBuffer

  -- controls horizontal panel
  buttonSave <- buttonNewWithLabel "Save"
  boxPackEnd hb_controls buttonSave PackNatural 10

  buttonDelete <- buttonNewWithLabel "Delete"
  boxPackEnd hb_controls buttonDelete PackNatural 10

  buttonAdd <- buttonNewWithLabel "Add"
  boxPackEnd hb_controls buttonAdd PackNatural 10

  ------------------------------------------------
  storage <- openLocalStateFrom songsStorageDirectory (KeyValue Map.empty)

  titles <- query storage GetKeys

  list <- listStoreNew titles

  -- ---------------------------------------------- Make TreeView
  treeview <- Model.treeViewNewWithModel list
  Model.treeViewSetHeadersVisible treeview False
  col <- Model.treeViewColumnNew
  renderer <- Model.cellRendererTextNew
  Model.cellLayoutPackStart col renderer False
  Model.cellLayoutSetAttributes col renderer list $ \ind -> [Model.cellText := ind]
  Model.treeViewAppendColumn treeview col
  tree <- Model.treeViewGetSelection treeview
  Model.treeSelectionSetMode tree SelectionSingle

  containerAdd frame treeview

  let handler :: SomeException -> IO ()
      handler _ = return ()

  on tree treeSelectionSelectionChanged $ do
    sel <- treeSelectionGetSelectedRows tree
    let s = head (head sel)
    title <- listStoreGetValue list s
    entrySetText entryTitle title
    text <- query storage (LookupKey title)
    textBufferSetText songBuffer $ fromMaybe "" text

  onPressed buttonAdd $ catch (addNewSong list storage entryTitle textViewSong) handler
  onPressed buttonSave $ catch (saveSong list storage tree entryTitle textViewSong) handler
  onPressed buttonDelete $ catch (deleteSong list storage tree) handler

  onPressed buttonDownload $ catch (downloadSong entryLink songBuffer) handler

  widgetShowAll window
  onDestroy window mainQuit

  mainGUI
