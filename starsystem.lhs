> {-# LANGUAGE ForeignFunctionInterface #-}

To demonstrate the usage of GLFW for OpenGL based Haskell applications, here is a sample program that allows user to draw lines by holding the left mouse button and move the mouse.

> import Graphics.Rendering.OpenGL as GL
> import Graphics.UI.GLFW as GLFW
> import Graphics.Rendering.OpenGL (($=))
> import Data.IORef
> import Control.Monad
> import Foreign.C

> import Active
> import Passive
> import Render

This function will allow us to have an unbundled app work like it is a regular bundle.

> foreign import ccall unsafe "unbundled" c_unbundled :: IO ()


Because the program needs to process user input, i.e., mouse button and movements, we'll use a continuation like structure for this purpose. The Action type represents an IO operation that returns the next Action to continue execution.

> data Action = Action (IO Action)

The main program is mostly book-keeping such as initializing OpenGL and GLFW, creating window, setting up viewport, etc.

\begin{code}

main :: IO ()
main = do
  c_unbundled
  GLFW.initialize
  -- open window
  GLFW.openWindow (GL.Size 800 600) [GLFW.DisplayAlphaBits 8] GLFW.Window
  GLFW.windowTitle $= "Star System"
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
  -- invoke the active drawing loop
  passive lines 
  -- finish up
  GLFW.closeWindow
  GLFW.terminate
  
\end{code}

