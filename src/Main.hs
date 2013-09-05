import Graphics.UI.Gtk
import Graphics.UI.AppIndicator
import Control.Monad
import Control.Concurrent

import BuildData

getData = do
  builds <- getBuildData
  return $ take 10 builds

getStatus (name, build) = (show (state build)) ++ ": " ++ (package build)

getFirstStatus []  = ""
getFirstStatus lst = getStatus $ head lst

showBuildLog (name, _) = do
  buffer  <- textBufferNew Nothing
  textBufferSetText buffer "Loading..."
  forkIO $ loadLog buffer name
  view    <- textViewNewWithBuffer buffer
  set view [
    textViewEditable := False]
  window  <- windowNew
  set window [
    windowTitle          := name, 
    windowWindowPosition := WinPosCenterAlways,
    windowDefaultWidth   := 500, 
    windowDefaultHeight  := 600]
  scroll <- scrolledWindowNew Nothing Nothing
  scrolledWindowAddWithViewport scroll view

  notebook <- notebookNew
  notebookAppendPage notebook scroll "Log"

  containerAdd window notebook
  widgetShowAll window
  where
    loadLog buffer name = do
      log <- getBuildLog name
      postGUIAsync $ textBufferSetText buffer log


createMenuItem menu build = do
  item <- menuItemNewWithLabel $ getStatus build
  menuShellAppend menu item
  item `on` menuItemActivate $ do
    showBuildLog build
  widgetShow item

createExitMenuItem menu = do
  sep <- separatorMenuItemNew
  menuShellAppend menu sep
  widgetShow sep

  item <- menuItemNewWithLabel "Exit"
  menuShellAppend menu item
  item `on` menuItemActivate $ do
    mainQuit
  widgetShow item

update appInd = do
  indMenu <- menuNew 
  builds  <- getData
  set appInd [appIndicatorLabel := Just $ getFirstStatus builds]
  forM_ builds $ createMenuItem indMenu
  createExitMenuItem indMenu
  widgetShow indMenu
  appIndicatorSetMenu appInd indMenu
  return True

main = do
  initGUI
  timeoutAddFull (yield >> return True)
                  priorityDefaultIdle 50
  appInd <- appIndicatorNew "Smart Processing Build Indicator" "" AppIndicatorCategoryApplicationStatus
  appIndicatorSetStatus appInd AppIndicatorStatusActive 
  update appInd
  timeoutAdd (update appInd) 5000
  mainGUI