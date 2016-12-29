{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import SDL.Pal hiding (glBindTexture, createTexture, updateTexture)
import Graphics.GL.Pal
import Data.Time
import Halive.Utils
import Halive.FileListener
import Control.Lens.Extra hiding (Context)

import Control.Monad.Trans
import Control.Monad
import Data.Monoid
import qualified Data.Set as S

import System.FilePath
import Graphics.GL.Ext.ARB.ShadingLanguageInclude
import Foreign.C

import Foreign

import Control.Concurrent

type Radius = GLfloat
type Size = GLfloat


type Transform = M44 GLfloat


data ShapeUniforms = ShapeUniforms
    { uTransform  :: UniformLocation Transform
    , uResolution :: UniformLocation (V2 GLfloat)
    , uMouse      :: UniformLocation (V2 GLfloat)
    , uTime       :: UniformLocation GLfloat
    , uTexture    :: UniformLocation GLint
    } deriving Data

main :: IO ()
main = do

    win <- reacquire 0 $ createGLWindow "flooid"

    swapInterval $= SynchronizedUpdates

    -- glEnable GL_DEPTH_TEST
    -- glBlendEquationSeparate GL_FUNC_ADD GL_FUNC_ADD
    -- glBlendFuncSeparate GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA GL_ONE GL_ZERO

    -- putStr "gl_ARB_shading_language_include support? "
    -- print gl_ARB_shading_language_include

    getWatchedResource <- makeWatchedResource "resources/shapes.frag" $ do
        getDir "resources" >>= \ss -> buildNamedStrings ss ("/resources/"++)

        shader <- createShaderProgramInclude
            "resources/shapes.vert" "resources/shapes.frag" ["/resources"]
        useProgram shader

        uniforms <- acquireUniforms shader

        (quadVAO, quadVertCount) <- makeScreenSpaceQuad shader
        return (shader, quadVAO, quadVertCount, uniforms)

    let (fbW, fbH) = (200, 200)
    (framebuffer, framebufferTexture) <- createFramebuffer fbW fbH

    whileWindow win $ \_events -> do
        V2 winFbW winFbH <- fmap fromIntegral <$> glGetDrawableSize win
        glViewport 0 0 winFbW winFbH

        glClearColor 0.1 0 0.1 1
        glClear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT .|. GL_STENCIL_BUFFER_BIT)

        now <- realToFrac . utctDayTime <$> getCurrentTime
        winSize <- getWindowSizeV2 win
        mouseLoc <- getMouseLocationV2

        projection <- getWindowProjection win 45 0.1 1000

        (shader, quadVAO, quadVertCount, ShapeUniforms{..}) <- getWatchedResource


        -- Draw one fullscreen quad
        glEnable GL_DEPTH_TEST

        let model = mkTransformation (axisAngle (V3 0 1 0) now) (V3 0 -1 -5)
        useProgram shader
        uniformV2 uResolution winSize
        uniformV2 uMouse (mouseLoc & _y %~ (`subtract` (winSize^._y)))
        uniformF uTime now
        uniformI uTexture 0
        uniformM44 uTransform (projection !*! model)
        withVAO quadVAO $
            glDrawArrays GL_TRIANGLE_STRIP 0 quadVertCount

        glSwapWindow win

makeScreenSpaceQuad :: (MonadIO m, Num t)
                    => Program -> m (VertexArrayObject, t)
makeScreenSpaceQuad shader = do

    let quadVertices =
            [ V2 -1.0 -1.0
            , V2 -1.0  1.0
            , V2 1.0  -1.0
            , V2 1.0   1.0
            ] :: [V2 GLfloat]

    let quadUVs =
            [ V2 0 0
            , V2 0  1.0
            , V2 1.0  0
            , V2 1.0   1.0
            ] :: [V2 GLfloat]

    quadVAO <- newVAO
    withVAO quadVAO $ do

        quadVerticesBuffer <- bufferData GL_STATIC_DRAW quadVertices
        withArrayBuffer quadVerticesBuffer $
            assignFloatAttribute shader "aPosition" GL_FLOAT 2
        quadUVsBuffer <- bufferData GL_STATIC_DRAW quadUVs
        withArrayBuffer quadUVsBuffer $
            assignFloatAttribute shader "aUV" GL_FLOAT 2

    return (quadVAO, fromIntegral $ length quadVertices)
