import Graphics.UI.Gtk
import Graphics.UI.AppIndicator
import Control.Monad

import BuildData

getData = do
  builds <- getBuildData
  return $ take 10 builds

getStatus (name, build) = (show (state build)) ++ ": " ++ name

createMenuItem menu build = do
  item <- menuItemNewWithLabel $ getStatus build
  menuShellAppend menu item
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
  set appInd [appIndicatorLabel := Just $ (getStatus . head) builds]
  forM_ builds $ createMenuItem indMenu
  createExitMenuItem indMenu
  widgetShow indMenu
  appIndicatorSetMenu appInd indMenu
  return True

main = do
  initGUI
  appInd <- appIndicatorNew "Smart Processing Build Indicator" "" AppIndicatorCategoryApplicationStatus
  appIndicatorSetStatus appInd AppIndicatorStatusActive 
  update appInd
  timeoutAdd (update appInd) 5000
  mainGUI