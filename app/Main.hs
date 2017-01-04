{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import SDL.Pal hiding (glBindTexture, createTexture, updateTexture, Surface)
import Graphics.GL.Pal
import Data.Time
import Halive.Utils
import Halive.FileListener
import Control.Lens.Extra hiding (Context, (<.>))

import Control.Monad.Trans
import Control.Monad.Reader
import Control.Monad
import Data.Monoid
import qualified Data.Set as S

import System.FilePath
import Foreign.C

-- import Foreign

import Control.Concurrent
import Data.IORef

import Quad

kCellSize               = 1.25
kViewportWidth          = 1024*2
kViewportHeight         = 768*2
kGridWidth              = kViewportWidth / 2
kGridHeight             = kViewportHeight / 2
kSplatRadius            = kGridWidth / 32

kAmbientTemperature     = 0
kImpulseTemperature     = 10
kImpulseDensity         = 1
-- kImpulseDensity         = 10
kNumJacobiIterations    = 40
kTimeStep               = 0.1
kSmokeBuoyancy          = 1
-- kSmokeBuoyancy          = 5
kSmokeWeight            = 0.05
kGradientScale          = 1 / kCellSize
kTemperatureDissipation = 0.99
kVelocityDissipation    = 0.99
kDensityDissipation     = 0.9999
kImpulsePosition        = V2 (kGridWidth / 2) (negate kSplatRadius / 2)

kPositionSlot           = 0


type Transform = M44 GLfloat
type TextureIDRaw = GLint

data ObstaclesU = ObstaclesU
    { uTime    :: UniformLocation GLfloat
    } deriving Data

data AdvectU = AdvectU
    { uVelocity    :: UniformLocation TextureIDRaw
    , uSource      :: UniformLocation TextureIDRaw
    , uObstacles   :: UniformLocation TextureIDRaw
    , uInverseSize :: UniformLocation (V2 GLfloat)
    , uTimeStep    :: UniformLocation GLfloat
    , uDissipation :: UniformLocation GLfloat
    } deriving Data

data BuoyancyU = BuoyancyU
    { uBVelocity          :: UniformLocation TextureIDRaw
    , uTemperature        :: UniformLocation TextureIDRaw
    , uDensity            :: UniformLocation TextureIDRaw
    , uAmbientTemperature :: UniformLocation GLfloat
    , uBTimeStep          :: UniformLocation GLfloat
    , uSigma              :: UniformLocation GLfloat
    , uKappa              :: UniformLocation GLfloat
    } deriving Data

data ComputeDivergenceU = ComputeDivergenceU
    { uCDVelocity          :: UniformLocation TextureIDRaw
    , uCDObstacles         :: UniformLocation TextureIDRaw
    , uHalfInverseCellSize :: UniformLocation GLfloat
    } deriving Data

data JacobiU = JacobiU
    { uJCPressure   :: UniformLocation TextureIDRaw
    , uJCDivergence :: UniformLocation TextureIDRaw
    , uJCObstacles  :: UniformLocation TextureIDRaw
    , uAlpha        :: UniformLocation GLfloat
    , uInverseBeta  :: UniformLocation GLfloat
    } deriving Data

data SplatU = SplatU
    { uPoint          :: UniformLocation (V2 GLfloat)
    , uRadius         :: UniformLocation GLfloat
    , uSplatFillColor :: UniformLocation (V3 GLfloat)
    } deriving Data

data SubtractGradientU = SubtractGradientU
    { uSGVelocity    :: UniformLocation TextureIDRaw
    , uSGPressure    :: UniformLocation TextureIDRaw
    , uSGObstacles   :: UniformLocation TextureIDRaw
    , uGradientScale :: UniformLocation GLfloat
    } deriving Data

data VisualizeU = VisualizeU
    { uSampler    :: UniformLocation TextureIDRaw
    , uFillColor  :: UniformLocation (V3 GLfloat)
    , uScale      :: UniformLocation (V2 GLfloat)
    , uIsVelocity :: UniformLocation GLint
    } deriving Data

makeFluidShader name = do
    let fragPath = "resources" </> name <.> "frag"
        vertPath = "resources/quad.vert"

    shader <- createShaderProgramInclude
        vertPath fragPath ["/resources"]

    uniforms <- acquireUniforms shader
    
    return (shader, uniforms)

formatForComponents 1 = GL_R16F
formatForComponents 2 = GL_RG16F
formatForComponents 3 = GL_RGB16F
formatForComponents 4 = GL_RGBA16F
formatForComponents n = error ("Requested unsupported number of components (" ++ show n ++ ")")

data Surface = Surface 
    { sfFramebuffer :: Framebuffer
    , sfTexture     :: TextureID 
    }

data Slab = Slab 
    { slbPing :: Surface
    , slbPong :: Surface 
    }

createSurface w h numComponents = do
    let format = formatForComponents numComponents
    (sfFramebuffer, sfTexture) <- createRenderTexture format w h
    return Surface{..}

createSlab w h numComponents = do
    slbPing <- createSurface w h numComponents
    slbPong <- createSurface w h numComponents
    newIORef Slab{..}

swapSurfaces slabRef = liftIO $ 
    modifyIORef' slabRef $ \Slab{..} ->
        Slab { slbPing = slbPong, slbPong = slbPing }



data FluidSurfaces = FluidSurfaces
    { sVelocity    :: IORef Slab
    , sDensity     :: IORef Slab
    , sPressure    :: IORef Slab
    , sTemperature :: IORef Slab
    , sDivergence  :: IORef Surface
    , sObstacles   :: IORef Surface
    }

data FluidPrograms = FluidPrograms
    { pAdvect            :: (Program, AdvectU)
    , pBuoyancy          :: (Program, BuoyancyU)
    , pComputeDivergence :: (Program, ComputeDivergenceU)
    , pJacobi            :: (Program, JacobiU)
    , pSplat             :: (Program, SplatU)
    , pSubtractGradient  :: (Program, SubtractGradientU)
    , pVisualize         :: (Program, VisualizeU)
    , pObstacles         :: (Program, ObstaclesU)
    }

data FluidData = FluidData { fdPrograms :: FluidPrograms, fdSurfaces :: FluidSurfaces }

createSurfaces w h = do
    sVelocity    <- createSlab w h 2
    sDensity     <- createSlab w h 1
    sPressure    <- createSlab w h 1
    sTemperature <- createSlab w h 1
    -- These refs never change, but gives us a more uniform treatment
    sDivergence  <- newIORef =<< createSurface w h 3
    sObstacles   <- newIORef =<< createSurface w h 3

    clearSurface (ping sTemperature) kAmbientTemperature

    return FluidSurfaces{..}

createShaders = do
    addShaderIncludeDir "resources"
    
    pAdvect            <- makeFluidShader "advect"
    pBuoyancy          <- makeFluidShader "buoyancy"
    pComputeDivergence <- makeFluidShader "compute-divergence"
    pJacobi            <- makeFluidShader "jacobi"
    pSplat             <- makeFluidShader "splat"
    pSubtractGradient  <- makeFluidShader "subtract-gradient"
    pVisualize         <- makeFluidShader "visualize"
    pObstacles         <- makeFluidShader "obstacles"

    return FluidPrograms{..}

main :: IO ()
main = do

    win <- reacquire 0 $ createGLWindow "flooid"
    windowSize win $= V2 1024 768

    swapInterval $= SynchronizedUpdates

    glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA

    (quadVAO, quadVertCount) <- makeScreenSpaceQuad
    glBindVertexArray (unVertexArrayObject quadVAO)

    programs <- createShaders
    surfaces <- reacquire 1 $ createSurfaces (floor kGridWidth) (floor kGridHeight)

    let fluidData = FluidData { fdPrograms = programs, fdSurfaces = surfaces }
    void . flip runReaderT fluidData . whileWindow win $ \_events -> do
        V2 winFbW winFbH <- fmap fromIntegral <$> glGetDrawableSize win
        glViewport 0 0 winFbW winFbH

        glClearColor 0.1 0 0.1 1
        glClear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT)

        -- now      <- realToFrac . utctDayTime <$> getCurrentTime
        -- winSize  <- getWindowSizeV2 win
        -- mouseLoc <- getMouseLocationV2

        fluidUpdate win
        fluidRender

        glSwapWindow win

pong slabRef = slbPong <$> liftIO (readIORef slabRef)
ping slabRef = slbPing <$> liftIO (readIORef slabRef)
boop surfaceRef = liftIO (readIORef surfaceRef)



fluidUpdate :: (MonadIO m, MonadReader FluidData m) => Window -> m ()
fluidUpdate win = do
    FluidSurfaces{..} <- asks fdSurfaces

    glViewport 0 0 (floor kGridWidth) (floor kGridHeight)

    obstacles (boop sObstacles)    

    advect (ping sVelocity) (ping sVelocity) (boop sObstacles) (pong sVelocity) kVelocityDissipation
    swapSurfaces sVelocity

    advect (ping sVelocity) (ping sTemperature) (boop sObstacles) (pong sTemperature) kTemperatureDissipation
    swapSurfaces sTemperature

    advect (ping sVelocity) (ping sDensity) (boop sObstacles) (pong sDensity) kDensityDissipation
    swapSurfaces sDensity

    applyBuoyancy (ping sVelocity) (ping sTemperature) (ping sDensity) (pong sVelocity)
    swapSurfaces sVelocity

    mouseLoc <- getMouseLocationV2
    winSize  <- getWindowSizeV2 win

    let impulsePosition = (mouseLoc / winSize) * V2 kGridWidth kGridHeight
            & _y %~ (kGridHeight -)
    applyImpulse (ping sTemperature) impulsePosition kImpulseTemperature
    applyImpulse (ping sDensity) impulsePosition kImpulseDensity

    computeDivergence (ping sVelocity) (boop sObstacles) (boop sDivergence)
    clearSurface (ping sPressure) 0

    replicateM_ kNumJacobiIterations $ do
        jacobi (ping sPressure) (boop sDivergence) (boop sObstacles) (pong sPressure)
        swapSurfaces sPressure

    subtractGradient (ping sVelocity) (ping sPressure) (boop sObstacles) (pong sVelocity)
    swapSurfaces sVelocity

clearSurface s v = do
    bindSurfaceDest s
    glClearColor v v v v
    glClear GL_COLOR_BUFFER_BIT

fluidRender :: (MonadIO m, MonadReader FluidData m) => m ()
fluidRender = do

    FluidSurfaces{..} <- asks fdSurfaces

    (p, VisualizeU{..}) <- asks (pVisualize . fdPrograms)
    useProgram p

    -- Bind visualization shader and set up blend state:
    glEnable GL_BLEND

    -- Set render target to the backbuffer:
    glViewport 0 0 (floor kViewportWidth) (floor kViewportHeight)

    glBindFramebuffer GL_FRAMEBUFFER 0
    glClearColor 0 0 0 1
    glClear GL_COLOR_BUFFER_BIT

    -- Draw ink:
    bindSurfaceSource GL_TEXTURE0 (ping sDensity)
    -- bindSurfaceSource GL_TEXTURE0 (ping sTemperature)
    -- bindSurfaceSource GL_TEXTURE0 (ping sPressure)
    -- bindSurfaceSource GL_TEXTURE0 (ping sVelocity) >> uniformI uIsVelocity 1

    uniformV3 uFillColor (V3 1 1 1)
    uniformV2 uScale  (1 / V2 kViewportWidth kViewportHeight)
    glDrawArrays GL_TRIANGLE_STRIP 0 4
    
    -- Draw obstacles:
    bindSurfaceSource GL_TEXTURE0 (boop sObstacles)
    uniformV3 uFillColor (V3 0.125 0.4 0.75)
    glDrawArrays GL_TRIANGLE_STRIP 0 4
    
    -- Disable blending:
    glDisable GL_BLEND


bindSurfaceDest :: MonadIO m => m Surface -> m ()
bindSurfaceDest surfaceRef = do
    surface <- surfaceRef
    glBindFramebuffer GL_FRAMEBUFFER (unFramebuffer (sfFramebuffer surface))

bindSurfaceSource :: MonadIO m => GLenum -> m Surface -> m ()
bindSurfaceSource textureSlot surfaceRef = do
    surface <- surfaceRef
    glActiveTexture textureSlot
    glBindTexture GL_TEXTURE_2D (unTextureID (sfTexture surface))

resetState :: MonadIO m => m ()
resetState = do
    glActiveTexture GL_TEXTURE2 >> glBindTexture GL_TEXTURE_2D 0
    glActiveTexture GL_TEXTURE1 >> glBindTexture GL_TEXTURE_2D 0
    glActiveTexture GL_TEXTURE0 >> glBindTexture GL_TEXTURE_2D 0
    glBindFramebuffer GL_FRAMEBUFFER 0
    glDisable GL_BLEND

obstacles dest = do
    (p, ObstaclesU{..}) <- asks (pObstacles . fdPrograms)
    useProgram p

    uniformF uTime =<< realToFrac . utctDayTime <$> liftIO getCurrentTime

    bindSurfaceDest dest
    glDrawArrays GL_TRIANGLE_STRIP 0 4
    resetState


advect velocity source obstacles dest dissipation = do

    (p, AdvectU{..}) <- asks (pAdvect . fdPrograms)
    useProgram p

    uniformV2 uInverseSize (V2 (1 / kGridWidth) (1 / kGridHeight))
    uniformF uTimeStep kTimeStep
    uniformF uDissipation dissipation

    uniformI uVelocity 0
    uniformI uSource 1
    uniformI uObstacles 2

    bindSurfaceDest dest

    bindSurfaceSource GL_TEXTURE0 velocity
    bindSurfaceSource GL_TEXTURE1 source
    bindSurfaceSource GL_TEXTURE2 obstacles
    
    glDrawArrays GL_TRIANGLE_STRIP 0 4
    
    resetState



jacobi pressure divergence obstacles dest = do
    (p, JacobiU{..}) <- asks (pJacobi . fdPrograms)
    useProgram p

    uniformF uAlpha (negate kCellSize * kCellSize)
    uniformF uInverseBeta 0.25

    uniformI uJCPressure 0
    uniformI uJCDivergence 1
    uniformI uJCObstacles 2

    bindSurfaceDest dest

    bindSurfaceSource GL_TEXTURE0 pressure
    bindSurfaceSource GL_TEXTURE1 divergence
    bindSurfaceSource GL_TEXTURE2 obstacles

    glDrawArrays GL_TRIANGLE_STRIP 0 4
    resetState


subtractGradient velocity pressure obstacles dest = do
    (p, SubtractGradientU{..}) <- asks (pSubtractGradient . fdPrograms)
    useProgram p

    uniformF uGradientScale kGradientScale
    
    uniformI uSGVelocity 0
    uniformI uSGPressure 1
    uniformI uSGObstacles 2

    bindSurfaceDest dest

    bindSurfaceSource GL_TEXTURE0 velocity
    bindSurfaceSource GL_TEXTURE1 pressure
    bindSurfaceSource GL_TEXTURE2 obstacles

    glDrawArrays GL_TRIANGLE_STRIP 0 4
    resetState

computeDivergence velocity obstacles dest = do
    (p, ComputeDivergenceU{..}) <- asks (pComputeDivergence . fdPrograms)
    useProgram p
    
    uniformF uHalfInverseCellSize (0.5 / kCellSize)
    
    uniformI uCDVelocity 0
    uniformI uCDObstacles 1

    bindSurfaceDest dest

    bindSurfaceSource GL_TEXTURE0 velocity
    bindSurfaceSource GL_TEXTURE1 obstacles

    glDrawArrays GL_TRIANGLE_STRIP 0 4
    resetState

applyImpulse dest position value = do
    (p, SplatU{..}) <- asks (pSplat . fdPrograms)
    useProgram p

    uniformV2 uPoint position
    uniformF uRadius kSplatRadius
    uniformV3 uSplatFillColor (V3 value value value)

    bindSurfaceDest dest
    glEnable GL_BLEND
    glDrawArrays GL_TRIANGLE_STRIP 0 4
    resetState

applyBuoyancy :: (MonadIO m, MonadReader FluidData m) 
              => m Surface -> m Surface -> m Surface -> m Surface -> m ()
applyBuoyancy velocity temperature density dest = do
    (p, BuoyancyU{..}) <- asks (pBuoyancy . fdPrograms)
    useProgram p

    uniformF uAmbientTemperature kAmbientTemperature
    uniformF uBTimeStep kTimeStep
    uniformF uSigma kSmokeBuoyancy
    uniformF uKappa kSmokeWeight

    uniformI uBVelocity 0
    uniformI uTemperature 1
    uniformI uDensity 2

    bindSurfaceDest dest

    bindSurfaceSource GL_TEXTURE0 velocity
    bindSurfaceSource GL_TEXTURE1 temperature
    bindSurfaceSource GL_TEXTURE2 density

    
    glDrawArrays GL_TRIANGLE_STRIP 0 4
    resetState

