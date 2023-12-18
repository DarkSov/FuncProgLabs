{-# LANGUAGE RecordWildCards #-}

import Control.Monad (unless)
import Graphics.Rendering.OpenGL as GL
import Graphics.UI.GLFW as GLFW

data Ball = Ball
  { ballPosition :: (GLfloat, GLfloat)
  , ballVelocity :: (GLfloat, GLfloat)
  , ballRadius :: GLfloat
  }

initialBall :: Ball
initialBall = Ball
  { ballPosition = (0, 0)
  , ballVelocity = (0.02, 0.03)
  , ballRadius = 0.1
  }

initialize :: IO Ball
initialize = do
  GLFW.setErrorCallback $ Just simpleErrorCallback
  successfulInit <- GLFW.init
  unless successfulInit (error "GLFW initialization failed")

  GLFW.windowHint $ WindowHint'ContextVersionMajor 3
  GLFW.windowHint $ WindowHint'ContextVersionMinor 3
  GLFW.windowHint $ WindowHint'OpenGLProfile OpenGLProfile'Core
  GLFW.windowHint $ WindowHint'OpenGLForwardCompat True

  mWindow <- GLFW.createWindow 800 600 "Collision Simulation" Nothing Nothing
  case mWindow of
    Nothing -> do
      GLFW.terminate
      error "Failed to create GLFW window"
    Just win -> do
      GLFW.makeContextCurrent (Just win)
      GL.blend GL.$= Enabled
      GL.blendFunc GL.$= (GL.SrcAlpha, GL.OneMinusSrcAlpha)
      GL.viewport GL.$= (GL.Position 0 0, GL.Size 800 600)
      return initialBall

updateBall :: Ball -> Ball
updateBall Ball {..} = Ball
  { ballPosition = (x + vx, y + vy)
  , ballVelocity = (vx, vy)
  , ballRadius = ballRadius
  }
  where
    (x, y) = ballPosition
    (vx, vy) = ballVelocity

drawBall :: Ball -> IO ()
drawBall Ball {..} = preservingMatrix $ do
  translate $ Vector3 x y 0
  renderPrimitive TriangleFan $ do
    color $ Color3 1 0 0
    let segments = 30
    forM_ [0 .. segments] $ \i -> do
      let theta = 2 * pi * (fromIntegral i / fromIntegral segments)
          x' = x + ballRadius * cos theta
          y' = y + ballRadius * sin theta
      vertex $ Vertex3 x' y' 0

simpleErrorCallback :: GLFW.Error -> String -> IO ()
simpleErrorCallback _ description = putStrLn $ unwords ["GLFW error", show description]

main :: IO ()
main = do
  ball <- initialize
  mainLoop ball

mainLoop :: Ball -> IO ()
mainLoop ball = do
  GLFW.pollEvents
  drawBall ball
  GLFW.swapBuffers
  GLFW.swapInterval 1
  shouldClose <- GLFW.windowShouldClose =<< GLFW.currentContext
  unless shouldClose $ do
    let updatedBall = updateBall ball
    mainLoop updatedBall

