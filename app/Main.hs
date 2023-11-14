module Main where

import Control.Monad (foldM_, void)
import Control.Monad.State (MonadIO (liftIO), MonadState (get, put), StateT (runStateT), void, when)
import qualified System.Posix.IO as IO
import qualified System.Posix.Terminal as T
import Term (getTermSize)
import Data.Char (chr)

data EditorState = EditorState
  { e_cursorX :: Int,
    e_cursorY :: Int,
    e_screenRows :: Int,
    e_screenCols :: Int,
    e_editing :: Bool,
    e_command :: Bool,
    e_filename :: String,
    e_contents :: [String] -- lines
  }
  deriving (Show)

type EditorM = StateT EditorState IO

editor :: IO ((), EditorState)
editor =
  let initEditorState =
        EditorState
          { e_cursorX = 0,
            e_cursorY = 0,
            e_screenRows = 0,
            e_screenCols = 0,
            e_editing = False,
            e_command = True,
            e_filename = "",
            e_contents = [""]
          }
   in runStateT loop initEditorState

moveCursor :: Int -> Int -> EditorM ()
moveCursor x y = do
  state <- get
  liftIO $ IO.fdWrite IO.stdOutput $ "\ESC[" ++ show y ++ ";" ++ show x ++ "H"
  return ()

refreshScreen :: EditorM ()
refreshScreen = do
  state <- get
  (rows, cols) <- liftIO getTermSize
  put $ state {e_screenRows = rows, e_screenCols = cols}
  liftIO $ do
    IO.fdWrite IO.stdOutput "\ESC[2J"
    IO.fdWrite IO.stdOutput "\ESC[H"
    -- file contents
    foldM_
      ( \_ y -> do
          IO.fdWrite IO.stdOutput "\ESC[K"
          IO.fdWrite IO.stdOutput y
          IO.fdWrite IO.stdOutput "\r\n"
          return ()
      )
      ()
      (e_contents state)
    -- bottom status bar
    IO.fdWrite IO.stdOutput "\ESC[7m"
    IO.fdWrite IO.stdOutput $ " " ++ e_filename state ++ " "
    IO.fdWrite IO.stdOutput "\ESC[m"
  -- if command mode
  if e_command state
    then do
      -- command prompt box
      moveCursor 1 (rows - 1)
      liftIO $ IO.fdWrite IO.stdOutput " : "
      moveCursor 4 (rows - 1)
    else
      if e_editing state
        then do
          moveCursor (e_cursorX state + 4) (e_cursorY state + 1)
        else do
          liftIO $ IO.fdWrite IO.stdOutput " "
          liftIO $ IO.fdWrite IO.stdOutput "\ESC[1m"
          liftIO $ IO.fdWrite IO.stdOutput " "
          liftIO $ IO.fdWrite IO.stdOutput "\ESC[m"
          return ()

clearScreen :: EditorM ()
clearScreen = do
  liftIO $ IO.fdWrite IO.stdOutput "\ESC[2J"
  liftIO $ IO.fdWrite IO.stdOutput "\ESC[H"
  return ()

handleEsc :: Char -> EditorM ()
handleEsc c = do
  state <- get
  case c of
    '[' -> do
      c' <- liftIO getChar
      case c' of
        'A' -> do
          when (e_cursorY state > 0) $ put $ state {e_cursorY = e_cursorY state - 1}
        'B' -> do
          when (e_cursorY state < e_screenRows state - 1) $ put $ state {e_cursorY = e_cursorY state + 1}
        'C' -> do
          when (e_cursorX state < e_screenCols state - 1) $ put $ state {e_cursorX = e_cursorX state + 1}
        'D' -> do
          when (e_cursorX state > 0) $ put $ state {e_cursorX = e_cursorX state - 1}
        _ -> return ()
    _ -> return ()

addChar :: Char -> EditorM ()
addChar c = do
  state <- get
  let line = e_contents state !! e_cursorY state
  let line' = take (e_cursorX state) line ++ [c] ++ drop (e_cursorX state) line
  let contents' = take (e_cursorY state) (e_contents state) ++ [line'] ++ drop (e_cursorY state) (e_contents state)
  put $ state {e_contents = contents'}
  put $ state {e_cursorX = e_cursorX state + 1}

handleKey :: Char -> EditorM ()
handleKey c = do
  state <- get
  if e_editing state
    then
      if c == chr 27
        then put $ state {e_editing = False}
        else addChar c >> loop
    else
      if e_command state
        then case c of
          'i' -> (put $ state {e_editing = True, e_command = False}) >> loop
          'q' -> clearScreen
          _ -> return ()
        else case c of
          ':' -> (put $ state {e_command = True}) >> loop
          _ -> return ()

loop :: EditorM ()
loop = do
  state <- get
  refreshScreen
  c <- liftIO getChar
  case c of
    '\ESC' ->
      do
        c' <- liftIO getChar
        handleEsc c' >> loop
    _ -> handleKey c

main :: IO ()
main = do
  attr <- T.getTerminalAttributes IO.stdInput
  let removeModes = [T.EnableEcho, T.ProcessInput, T.KeyboardInterrupts, T.StartStopOutput, T.ExtendedFunctions]
  let attr' = foldl T.withoutMode attr removeModes
  T.setTerminalAttributes IO.stdInput attr' T.Immediately
  void editor
