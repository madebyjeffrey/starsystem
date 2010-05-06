There are usually two ways to structure the main loop of GLFW programs. One is by actively polling events before processing them. The screen buffer is usually redrawn every time before swapBuffers is called. This is the simplest main loop often seen in game applications, and may waste CPU cycles even when there is no visual update. Note that swapBuffers by default calls pollEvents implicitly, so there is no need to do a separate poll.

\begin{code}

module Passive where

import Graphics.Rendering.OpenGL as GL
import Graphics.UI.GLFW as GLFW
import Graphics.Rendering.OpenGL (($=))
import Data.IORef
import Control.Monad

import Render


passive lines = do
  -- disable auto polling in swapBuffers
  GLFW.disableSpecial GLFW.AutoPollEvent
 
  -- keep track of whether ESC has been pressed
  quit <- newIORef False
 
  -- keep track of whether screen needs to be redrawn
  dirty <- newIORef True
 
  -- mark screen dirty in refresh callback which is often called
  -- when screen or part of screen comes into visibility.
  GLFW.windowRefreshCallback $= writeIORef dirty True
 
  -- use key callback to track whether ESC is pressed
  GLFW.keyCallback $= \k s -> 
     when (fromEnum k == fromEnum GLFW.ESC && s == GLFW.Press) $ 
        writeIORef quit True
     
  -- Terminate the program if the window is closed
  GLFW.windowCloseCallback $= writeIORef quit True
 
  -- by default start with waitForPress
  waitForPress dirty
  loop dirty quit
  where
 
    loop dirty quit = do
        GLFW.waitEvents
        -- redraw screen if dirty
        d <- readIORef dirty
 
        when d $ 
          render lines >> GLFW.swapBuffers
 
        writeIORef dirty False
        -- check if we need to quit the loop
        q <- readIORef quit
        unless q $
          loop dirty quit
 
    waitForPress dirty =
      do
        GLFW.mousePosCallback    $= \_ -> return ()
 
        GLFW.mouseButtonCallback $= \b s -> 
            when (b == GLFW.ButtonLeft && s == GLFW.Press) $
              do
                -- when left mouse button is pressed, add the point
                -- to lines and switch to waitForRelease action.
                (GL.Position x y) <- GL.get GLFW.mousePos
                modifyIORef lines (((x,y):) . ((x,y):))
                waitForRelease dirty
 
    waitForRelease dirty = 
      do 
        GLFW.mousePosCallback $= \(Position x y) ->
          do
            -- update the line with new ending position
            modifyIORef lines (((x,y):) . tail)
            -- mark screen dirty
            writeIORef dirty True
 
        GLFW.mouseButtonCallback $= \b s ->
            -- when left mouse button is released, switch back to
            -- waitForPress action.
            when (b == GLFW.ButtonLeft && s == GLFW.Release) $
              waitForPress dirty


\end{code}
