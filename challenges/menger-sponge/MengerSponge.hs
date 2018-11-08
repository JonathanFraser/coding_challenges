{-# LANGUAGE OverloadedStrings #-}
module Main (main) where 

import SDL (($=))
import qualified SDL 
import Control.Monad (unless) 
import Graphics.Rendering.OpenGL
import Graphics.Rendering.OpenGL.GLU.Matrix

main = do 
    SDL.version >>=print 
    SDL.initializeAll
    let glWindow = SDL.defaultWindow {SDL.windowOpenGL = Just SDL.defaultOpenGL}
    window <- SDL.createWindow "My SDL Application" glWindow
    ctx <- SDL.glCreateContext window
    colorMaterial $= Just (FrontAndBack, AmbientAndDiffuse)
    depthFunc          $= Just Less
    blend              $= Enabled
    blendFunc          $= (SrcAlpha, OneMinusSrcAlpha)
    glVersion >>= print
    lighting $= Enabled
    normalize $= Enabled 

    appLoop window 0.0
    SDL.glDeleteContext ctx
    SDL.destroyWindow window
    SDL.quit

square :: IO ()
square = renderPrimitive TriangleStrip $ do
                            normal $ Normal3 0 0 (1::GLfloat)
                            vertex $ Vertex3 (-1) (-1) (1::GLfloat)
                            vertex $ Vertex3 (-1) (1) (1::GLfloat)
                            vertex $ Vertex3 (1) (-1) (1::GLfloat)
                            vertex $ Vertex3 1 (1) (1::GLfloat) 

cube :: IO () 
cube  = preservingMatrix $ do 
            color (Color3 0.188 0.611 (0.007::GLfloat)) --green
            square
            rotate 90 (Vector3 0 1 (0::GLfloat))
            color (Color3 1.0 0.361 (0.0157::GLfloat)) --orange
            square
            rotate 90 (Vector3 0 1 (0::GLfloat))
            color (Color3 0.705 0.0157 (1.00::GLfloat)) --purple
            square
            rotate 90 (Vector3 0 1 (0::GLfloat))
            color (Color3 0.631 0.569 (0.008::GLfloat)) --yellow
            square
            rotate (-90) (Vector3 1 0 (0::GLfloat))
            color (Color3 1.0 0.0157 (0.196::GLfloat)) --red
            square
            rotate 180 (Vector3 1 0 (0::GLfloat))
            color (Color3 0.0157 0.3765 (1.00::GLfloat)) --blue
            square

menger :: [(GLfloat,GLfloat,GLfloat)]
menger = [
    (2,2,0),
    (2,-2,0),
    (-2,-2,0),
    (-2,2,0),
    (2,2,-2),
    (2,0,-2),
    (2,-2,-2),
    (0,-2,-2),
    (-2,-2,-2),
    (-2,0,-2),
    (-2,2,-2),
    (0,2,-2),
    (2,2,2),
    (2,0,2),
    (2,-2,2),
    (0,-2,2),
    (-2,-2,2),
    (-2,0,2),
    (-2,2,2),
    (0,2,2)]

renderMenger 0 = cube
renderMenger n = preservingMatrix $ do 
    scale (0.333::GLfloat) (0.333::GLfloat) (0.3333::GLfloat)
    let shift v = preservingMatrix $ do translate v; renderMenger (n-1)
    mapM_ (\(x,y,z) -> shift $ Vector3 x y z) menger


eventIsQPress :: SDL.Event -> Bool 
eventIsQPress event = case SDL.eventPayload event of 
                        SDL.KeyboardEvent keyboardEvent ->
                            SDL.keyboardEventKeyMotion keyboardEvent == SDL.Pressed && SDL.keysymKeycode (SDL.keyboardEventKeysym keyboardEvent) == SDL.KeycodeQ
                        _ -> False 

eventIsQuit :: SDL.Event -> Bool 
eventIsQuit event = case SDL.eventPayload event of 
                        SDL.QuitEvent -> True 
                        _ -> False 

appLoop :: SDL.Window -> GLfloat -> IO () 
appLoop win angle = do 
    events <- SDL.pollEvents
    --mapM (putStrLn.show) events
    let qPressed = any eventIsQPress events
    let quit = any eventIsQuit events
    

    clear [ColorBuffer,DepthBuffer]
    light (Light 0) $= Enabled
    position (Light 0) $= Vertex4 (10*sin angle) (0.5*cos angle) (10*cos angle) 1
    --ambient (Light 0) $= Color4 2 2 2.0 1
    diffuse (Light 0) $= Color4 0.5 0.5 0.5 1.0
    --specular (Light 0) $= Color4 1.0 1.0 1.0 1.0 
    

    matrixMode $= Projection
    loadIdentity
    perspective 90.0 1.0 1.0 10.0
    lookAt (Vertex3 1.6 1.6 1.6) (Vertex3 0.0 0.0 0.0) (Vector3 0.0 1.0 0.0)
    matrixMode $= Modelview 0 
    loadIdentity
    materialAmbientAndDiffuse FrontAndBack $= Color4 0.5 0.0 0.0 1.0
    --materialSpecular FrontAndBack $= (Color4 1.0 0.0 0.0 1.0)
    renderMenger 3
    flush
    SDL.glSwapWindow win
    unless (qPressed || quit) (appLoop win (angle+0.05))