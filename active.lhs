
There are usually two ways to structure the main loop of GLFW programs. One is by actively polling events before processing them. The screen buffer is usually redrawn every time before swapBuffers is called. This is the simplest main loop often seen in game applications, and may waste CPU cycles even when there is no visual update. Note that swapBuffers by default calls pollEvents implicitly, so there is no need to do a separate poll.

\begin{code}

module Active where

import Graphics.Rendering.OpenGL as GL
import Graphics.UI.GLFW as GLFW
import Graphics.Rendering.OpenGL (($=))
import Data.IORef
import Control.Monad

import Render

data Action = Action (IO Action)

-- we start with waitForPress action
active lines = loop waitForPress
  where 
 
    loop action = do
      -- draw the entire screen
      Render.render lines
      -- swap buffer
      GLFW.swapBuffers
      -- check whether ESC is pressed for termination
      p <- GLFW.getKey GLFW.ESC
      unless (p == GLFW.Press) $
        do
            -- perform action
            Action action' <- action
            -- sleep for 1ms to yield CPU to other applications
            GLFW.sleep 0.001
 
            -- only continue when the window is not closed
            windowOpenStatus <- get $ windowParam Opened
            unless (windowOpenStatus == 0) $
              loop action' -- loop with next action
 
    waitForPress = do
      b <- GLFW.getMouseButton GLFW.ButtonLeft
      case b of
        GLFW.Release -> return (Action waitForPress)
        GLFW.Press   -> do
          -- when left mouse button is pressed, add the point
          -- to lines and switch to waitForRelease action.
          (GL.Position x y) <- GL.get GLFW.mousePos 
          modifyIORef lines (((x,y):) . ((x,y):))
          return (Action waitForRelease)
 
    waitForRelease = do
        -- keep track of mouse movement while waiting for button 
        -- release
        (GL.Position x y) <- GL.get GLFW.mousePos
        -- update the line with new ending position
        modifyIORef lines (((x,y):) . tail)
        b <- GLFW.getMouseButton GLFW.ButtonLeft
        case b of
          -- when button is released, switch back back to 
          -- waitForPress action
          GLFW.Release -> return (Action waitForPress)
          GLFW.Press   -> return (Action waitForRelease)

\end{code}
