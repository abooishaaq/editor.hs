module Main where

import Control.Monad (foldM_, void)
import Control.Monad.State (MonadIO (liftIO), MonadState (get, put), StateT (runStateT), void)
import qualified System.Posix.IO as IO
import qualified System.Posix.Terminal as T

data EditorState = EditorState
  { e_cursorX :: Int,
    e_cursorY :: Int,
    e_screenRows :: Int,
    e_screenCols :: Int,
    e_editing :: Bool
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
            e_editing = False
          }
   in runStateT loop initEditorState

refreshScreen :: EditorM ()
refreshScreen = do
  state <- get
  liftIO $ do
    IO.fdWrite IO.stdOutput "\ESC[2J"
    IO.fdWrite IO.stdOutput "\ESC[H"
    foldM_
      ( \_ _ -> do
          IO.fdWrite IO.stdOutput "~\r\n"
          return ()
      )
      ()
      [1 .. e_screenRows state]
    IO.fdWrite IO.stdOutput $ "\ESC[" ++ show (e_cursorY state) ++ ";" ++ show (e_cursorX state + 1) ++ "H"
    return ()

loop :: EditorM ()
loop = do
  state <- get
  c <- liftIO getChar
  refreshScreen
  case c of
    -- enter
    '\n' -> do
      put $ state {e_cursorY = e_cursorY state + 1, e_cursorX = 0, e_screenRows = e_screenRows state + 1}
      loop
    '\ESC' ->
      do
        c' <- liftIO getChar
        case c' of
          '[' -> do
            c'' <- liftIO getChar
            case c'' of
              'A' -> do
                put $ state {e_cursorY = e_cursorY state - 1}
              'B' -> do
                put $ state {e_cursorY = e_cursorY state + 1}
              'C' -> do
                put $ state {e_cursorX = e_cursorX state + 1}
              'D' -> do
                put $ state {e_cursorX = e_cursorX state - 1}
              _ -> return ()
          _ -> return ()
        >> loop
    'q' -> return ()
    _ -> loop

main :: IO ()
main = do
  attr <- T.getTerminalAttributes IO.stdInput
  let removeModes = [T.EnableEcho, T.ProcessInput, T.KeyboardInterrupts, T.StartStopOutput, T.ExtendedFunctions]
  let attr' = foldl T.withoutMode attr removeModes
  T.setTerminalAttributes IO.stdInput attr' T.Immediately
  void editor
