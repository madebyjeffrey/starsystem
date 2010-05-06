
\begin{code}

module Render where

import Graphics.Rendering.OpenGL as GL
import Graphics.UI.GLFW as GLFW
import Graphics.Rendering.OpenGL (($=))
import Data.IORef
import Control.Monad

render lines = do
  l <- readIORef lines
  GL.clear [GL.ColorBuffer]
  GL.color $ color3 1 0 0
  GL.renderPrimitive GL.Lines $ mapM_
      (\ (x, y) -> GL.vertex (vertex3 (fromIntegral x) (fromIntegral y) 0)) l
 
 
vertex3 :: GLfloat -> GLfloat -> GLfloat -> GL.Vertex3 GLfloat
vertex3 = GL.Vertex3
 
 
color3 :: GLfloat -> GLfloat -> GLfloat -> GL.Color3 GLfloat
color3 = GL.Color3

\end{code}