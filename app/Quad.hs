{-# LANGUAGE NegativeLiterals #-}
module Quad where
import Graphics.GL.Pal
import Control.Monad.Trans

makeScreenSpaceQuad :: (MonadIO m, Num length)
                    => m (VertexArrayObject, length)
makeScreenSpaceQuad = do
    let positionAttr = AttributeLocation 0
        uvAttr       = AttributeLocation 1

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
            assignFloatAttributeFixed positionAttr GL_FLOAT 2
        quadUVsBuffer <- bufferData GL_STATIC_DRAW quadUVs
        withArrayBuffer quadUVsBuffer $
            assignFloatAttributeFixed uvAttr GL_FLOAT 2

    return (quadVAO, fromIntegral $ length quadVertices)
