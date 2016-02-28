import Control.Concurrent (threadDelay)
import Control.Exception
import Control.Monad
import Data.List (nub)
import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (($=))
import qualified Graphics.UI.GLFW as GLFW
import Prelude hiding (catch)

main = do
  -- initialize has to come first. If it doesn't return True,
  -- this crashes with a pattern match error.
  True <- GLFW.initialize

  -- Set the RGB bits to get a color window.
  -- See the GLFW-b docs for all the options
  True <- GLFW.openWindow GLFW.defaultDisplayOptions
          { GLFW.displayOptions_numRedBits   = 8
          , GLFW.displayOptions_numGreenBits = 8
          , GLFW.displayOptions_numBlueBits  = 8
          , GLFW.displayOptions_numDepthBits = 1
          , GLFW.displayOptions_width        = 640
          , GLFW.displayOptions_height       = 480
          }


  GLFW.setWindowSizeCallback $ resize

  -- Use `$=` for assigning to GL values, `get` to read them.
  -- These functions basically hide IORefs.

  GL.depthFunc $= Just GL.Less

  print $ nub $ map (uncurry strutLength) $ concatMap triToPairs (icotris_vx 3)


  -- Use `finally` so that `quit` is called whether or
  -- not `mainLoop` throws an exception
  finally mainLoop quit

-- | Resize the viewport and set the projection matrix
resize w h = do
  -- These are all analogous to the standard OpenGL functions
  GL.viewport     $= (GL.Position 0 0, GL.Size (fromIntegral w) (fromIntegral h))
  GL.matrixMode   $= GL.Projection
  GL.loadIdentity
  GL.perspective  45 (fromIntegral w / fromIntegral h) 1 100
  GL.matrixMode   $= GL.Modelview 0

-- | Close the window and terminate GLFW
quit = GLFW.closeWindow >> GLFW.terminate

-- | This will print and clear the OpenGL errors
printErrors = GL.get GL.errors >>= mapM_ print

-- | Draw the window and handle input
mainLoop = do
  now <- GLFW.getTime
  draw now

  -- Input is polled each time swapBuffers is called
  esc <- GLFW.keyIsPressed GLFW.KeyEsc
  isClosed <- fmap not GLFW.windowIsOpen
  unless (esc || isClosed) $ do
    -- Sleep for the rest of the frame
    frameLeft <- fmap (spf + now -) GLFW.getTime
    when (frameLeft > 0) $
        threadDelay (truncate $ 1000000 * frameLeft)

    mainLoop

  where
    -- maximum frame rate
    fps = 60
    spf = recip fps

tetrapoints :: [GL.Vertex3 GL.GLfloat]
tetrapoints =
    [ GL.Vertex3   1    1    1
    , GL.Vertex3   1  (-1) (-1)
    , GL.Vertex3 (-1)   1  (-1)
    , GL.Vertex3 (-1) (-1)   1
    ]

tetratris :: [[GL.Vertex3 GL.GLfloat]]
tetratris =
     [ [ tetrapoints !! 0
       , tetrapoints !! 1
       , tetrapoints !! 2
       ]

     , [ tetrapoints !! 0
       , tetrapoints !! 2
       , tetrapoints !! 3
       ]

     , [ tetrapoints !! 0
       , tetrapoints !! 3
       , tetrapoints !! 1
       ]

     , [ tetrapoints !! 1
       , tetrapoints !! 2
       , tetrapoints !! 3
       ]
     ]


octapoints :: [GL.Vertex3 GL.GLfloat]
octapoints =
    [ GL.Vertex3   0   1    0
    , GL.Vertex3 (-1)  0    0
    , GL.Vertex3   0   0    1
    , GL.Vertex3   1   0    0
    , GL.Vertex3   0   0  (-1)
    , GL.Vertex3   0 (-1)   0
    ]

octatris_v1 :: [[GL.Vertex3 GL.GLfloat]]
octatris_v1 =
    [ [ octapoints !! 0
      , octapoints !! 1
      , octapoints !! 2
      ]

    , [ octapoints !! 0
      , octapoints !! 2
      , octapoints !! 3
      ]

    , [ octapoints !! 0
      , octapoints !! 3
      , octapoints !! 4
      ]

    , [ octapoints !! 0
      , octapoints !! 4
      , octapoints !! 1
      ]

    , [ octapoints !! 5
      , octapoints !! 2
      , octapoints !! 1
      ]

    , [ octapoints !! 5
      , octapoints !! 3
      , octapoints !! 2
      ]

    , [ octapoints !! 5
      , octapoints !! 4
      , octapoints !! 3
      ]

    , [ octapoints !! 5
      , octapoints !! 1
      , octapoints !! 4
      ]

    ]

icopoints :: [GL.Vertex3 GL.GLfloat]
icopoints =
    let t = (1.0 + sqrt(5.0)) / 2.0
    in [ GL.Vertex3 (-1)   t   0
       , GL.Vertex3   1    t   0
       , GL.Vertex3 (-1) (-t)  0
       , GL.Vertex3   1  (-t)  0

       , GL.Vertex3   0 (-1)   t
       , GL.Vertex3   0   1    t
       , GL.Vertex3   0 (-1) (-t)
       , GL.Vertex3   0   1  (-t)

       , GL.Vertex3   t   0  (-1)
       , GL.Vertex3   t   0    1
       , GL.Vertex3 (-t)  0  (-1)
       , GL.Vertex3 (-t)  0    1
       ]

icotris_v1 :: [[GL.Vertex3 GL.GLfloat]]
icotris_v1 =
     [ -- faces around point 0
       [ icopoints !! 0
       , icopoints !! 11
       , icopoints !! 5
       ]

     , [ icopoints !! 0
       , icopoints !! 5
       , icopoints !! 1
       ]

     , [ icopoints !! 0
       , icopoints !! 1
       , icopoints !! 7
       ]
{-
     , [ icopoints !! 0
       , icopoints !! 1
       , icopoints !! 7
       ]
-}
     , [ icopoints !! 0
       , icopoints !! 7
       , icopoints !! 10
       ]

     , [ icopoints !! 0
       , icopoints !! 10
       , icopoints !! 11
       ]

       -- 5 adjacent faces

     , [ icopoints !! 1
       , icopoints !! 5
       , icopoints !! 9
       ]

     , [ icopoints !! 5
       , icopoints !! 11
       , icopoints !! 4
       ]

     , [ icopoints !! 11
       , icopoints !! 10
       , icopoints !! 2
       ]

     , [ icopoints !! 10
       , icopoints !! 7
       , icopoints !! 6
       ]

     , [ icopoints !! 7
       , icopoints !! 1
       , icopoints !! 8
       ]

       -- 5 adjacent faces  around point 3

     , [ icopoints !! 3
       , icopoints !! 9
       , icopoints !! 4
       ]

     , [ icopoints !! 3
       , icopoints !! 4
       , icopoints !! 2
       ]

     , [ icopoints !! 3
       , icopoints !! 2
       , icopoints !! 6
       ]

     , [ icopoints !! 3
       , icopoints !! 6
       , icopoints !! 8
       ]

     , [ icopoints !! 3
       , icopoints !! 8
       , icopoints !! 9
       ]

       -- 5 adjacent faces

     , [ icopoints !! 4
       , icopoints !! 9
       , icopoints !! 5
       ]

     , [ icopoints !! 2
       , icopoints !! 4
       , icopoints !! 11
       ]

     , [ icopoints !! 6
       , icopoints !! 2
       , icopoints !! 10
       ]

     , [ icopoints !! 8
       , icopoints !! 6
       , icopoints !! 7
       ]

     , [ icopoints !! 9
       , icopoints !! 8
       , icopoints !! 1
       ]

     ]

icotris_v2 :: [[GL.Vertex3 GL.GLfloat]]
icotris_v2 = map (map normalize) $ subdivide 2 icotris_v1

icotris_v3 :: [[GL.Vertex3 GL.GLfloat]]
icotris_v3 = map (map normalize) $ subdivide 3 icotris_v1

icotris_v6 :: [[GL.Vertex3 GL.GLfloat]]
icotris_v6 = map (map normalize) $ subdivide 6 icotris_v1

icotris_vx :: Int -> [[GL.Vertex3 GL.GLfloat]]
icotris_vx nu = map (map normalize) $ subdivide nu icotris_v1

triToPairs :: [GL.Vertex3 GL.GLfloat] -> [(GL.Vertex3 GL.GLfloat, GL.Vertex3 GL.GLfloat)]
triToPairs [v1, v2, v3] = [(v1, v2), (v2, v3), (v3, v1)]

midpoint :: GL.Vertex3 GL.GLfloat -> GL.Vertex3 GL.GLfloat -> GL.Vertex3 GL.GLfloat
midpoint (GL.Vertex3 x y z) (GL.Vertex3 x' y' z') =
    GL.Vertex3 ((x + x') / 2) ((y + y') / 2) ((z + z') / 2)

subdivide :: Int -> [[GL.Vertex3 GL.GLfloat]] -> [[GL.Vertex3 GL.GLfloat]]
subdivide frequency tris = -- concatMap subdivideTri tris
    iterate (concatMap subdivideTri) tris !! (frequency - 1)
    where
      subdivideTri [v1, v2, v3] =
          let a = midpoint v1 v2
              b = midpoint v2 v3
              c = midpoint v3 v1
          in [ [ v1, a, c ]
             , [ v2, b, a ]
             , [ v3, c, b ]
             , [  a, b, c ]
             ]

strutLength :: GL.Vertex3 GL.GLfloat -> GL.Vertex3 GL.GLfloat -> GL.GLfloat
strutLength (GL.Vertex3 x y z) (GL.Vertex3 x' y' z') =
    sqrt ((x - x')^2 + (y - y')^2 + (z - z')^2)

normalize :: GL.Vertex3 GL.GLfloat -> GL.Vertex3 GL.GLfloat
normalize (GL.Vertex3 x y z) =
    let length = sqrt (x^2 + y^2 + z^2)
    in GL.Vertex3 (x / length) (y / length) (z / length)

subNormal :: Int -> [[GL.Vertex3 GL.GLfloat]] -> [[GL.Vertex3 GL.GLfloat]]
subNormal nu tris = map (map normalize) $ subdivide nu tris

draw :: Double -> IO ()
draw t = do
  -- Again, the functions in GL almost all map to standard OpenGL functions
  GL.clear [GL.ColorBuffer, GL.DepthBuffer]

  GL.loadIdentity
  GL.translate $ GL.Vector3 0 0 (-50 :: GL.GLfloat)
  GL.scale 10 10 (1 :: GL.GLfloat)
  GL.rotate theta axis

  -- renderPrimitive wraps the supplied action with glBegin and glEnd.
  -- We'll stop using this when we switch to shaders and vertex buffers.

--  GL.renderPrimitive GL.Points $
--    mapM_ GL.vertex icopoints

--  GL.renderPrimitive GL.Triangles $
--    mapM_ GL.vertex (concat icotris)

--  mapM_ (GL.renderPrimitive GL.LineLoop . mapM_ GL.vertex) (subNormal 3 $ tetratris)
--  mapM_ (GL.renderPrimitive GL.LineLoop . mapM_ GL.vertex) (subNormal 3 $ octatris_v1)
  mapM_ (GL.renderPrimitive GL.LineLoop . mapM_ GL.vertex) (subNormal 3 $ icotris_v1)


  printErrors
  GL.flush
  GLFW.swapBuffers

    where
      -- GL.rotate takes the angle in degrees, not radians
      theta = realToFrac t * 10
      axis = GL.Vector3 0 1 1 :: GL.Vector3 GL.GLfloat

{-
-- | Draw a frame
draw :: Double -> IO ()
draw t = do
  -- Again, the functions in GL almost all map to standard OpenGL functions
  GL.clear [GL.ColorBuffer, GL.DepthBuffer]

  GL.loadIdentity
  GL.translate $ GL.Vector3 0 0 (-50 :: GL.GLfloat)
  GL.scale 10 10 (1 :: GL.GLfloat)
  GL.rotate theta axis

  -- renderPrimitive wraps the supplied action with glBegin and glEnd.
  -- We'll stop using this when we switch to shaders and vertex buffers.
  GL.renderPrimitive GL.LineLoop $
    -- Draw a unit square centered on the origin
    forM_ [(0, 0), (1, 0), (1, 1), (0, 1)] $ \(x, y) ->
        -- Note that we have to explicitly type Vertex* and Vector*, because
        -- they are polymorphic in number field.
        let vtx = GL.Vertex3 (x - 0.5) (y - 0.5) 0 :: GL.Vertex3 GL.GLfloat
        in GL.vertex vtx

  printErrors
  GL.flush
  GLFW.swapBuffers

    where
      -- GL.rotate takes the angle in degrees, not radians
      theta = realToFrac t * 360
      axis = GL.Vector3 0 1 0 :: GL.Vector3 GL.GLfloat


-}