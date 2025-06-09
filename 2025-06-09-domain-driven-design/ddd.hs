-- based on: https://marcosh.github.io/post/2021/10/27/ddd-state-machines.html
import Control.Monad (forever)
import Data.IORef (newIORef, readIORef, writeIORef)

data ReadModel
data Command
data Event

aggregate :: Command -> [Event]
policy :: Event -> [Command]
projection :: ReadModel -> Event -> ReadModel
poolCommand :: IO Command

processACommand :: ReadModel -> Command -> ReadModel
processACommand rm cmd =
  let events = aggregate cmd
      commands = foldMap policy events
      rm' = foldl' projection rm events
  in foldl' processACommand rm commands

loop :: ReadModel -> IO ()
loop rm = do
  rmRef <- newIORef rm
  forever $ do
    rm' <- readIORef rmRef
    cmd <- poolCommand
    let rm'' = processACommand rm cmd
    writeIORef rmRef rm''

aggregate = error "TODO"
policy = error "TODO"
projection = error "TODO"
poolCommand = error "TODO"
