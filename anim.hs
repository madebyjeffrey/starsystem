{-# LANGUAGE ForeignFunctionInterface #-}
import Graphics.Rendering.OpenGL as GL
import Graphics.UI.GLFW as GLFW
import Graphics.Rendering.OpenGL (($=))
import Data.IORef
import Control.Monad
import Foreign.C

import Active
import Passive
import Render

      
foreign import ccall unsafe "unbundled" c_unbundled :: IO ()



main = do
  c_unbundled
  GLFW.initialize
  -- open window
  GLFW.openWindow (GL.Size 800 600) [GLFW.DisplayAlphaBits 8] GLFW.Window
  GLFW.windowTitle $= "anim"
  GL.shadeModel    $= GL.Smooth
  -- enable antialiasing
  GL.lineSmooth $= GL.Enabled
  GL.blend      $= GL.Enabled
  GL.blendFunc  $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)
  GL.lineWidth  $= 1.5
  -- set the color to clear background
  GL.clearColor $= Color4 0 0 0 0
 
  -- set 2D orthogonal view inside windowSizeCallback because
  -- any change to the Window size should result in different
  -- OpenGL Viewport.
  GLFW.windowSizeCallback $= \ size@(GL.Size w h) ->
    do
      GL.viewport   $= (GL.Position 0 0, size)
      GL.matrixMode $= GL.Projection
      GL.loadIdentity
      GL.ortho 0 (realToFrac w) (realToFrac h) 0 0 1
      GL.matrixMode $= GL.Modelview 0
 
  -- keep all line strokes as a list of points in an IORef
  lines <- newIORef []
  -- invoke the passive drawing loop
  passive lines 
  -- finish up
  GLFW.closeWindow
  GLFW.terminate

