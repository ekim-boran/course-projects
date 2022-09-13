{-# LANGUAGE NamedFieldPuns #-}

module Ex01.Main where

-- needed to display the picture in the playground
import Codec.Picture
-- our line graphics programming interface
import Ex01.ShapeGraphics

 

housePic :: Picture
housePic = [door, house]

-- these are the coordinates - convert them to a list of Point
houseCOs :: [(Float, Float)]
houseCOs =
  [ (300, 750),
    (300, 450),
    (270, 450),
    (500, 200),
    (730, 450),
    (700, 450),
    (700, 750)
  ]

doorCOs :: [(Float, Float)]
doorCOs = [(550, 750), (550, 550), (650, 550), (650, 750)]

chimneyCos :: [(Float, Float)]
chimneyCos = [(615, 325), (615, 250), (650, 250), (650, 363)]

windowCos :: [(Float, Float)]
windowCos = [(350, 650), (350, 550), (450, 550), (450, 650), (350, 650)]

cyan :: Colour
cyan = Colour 96 192 255 255

window :: PictureObject
window = Path (fmap (uncurry Point) windowCos) cyan Solid

house :: PictureObject
house = Path (fmap (uncurry Point) houseCOs) green Solid

door :: PictureObject
door = Path (fmap (uncurry Point) doorCOs) red Solid

chimney :: PictureObject
chimney = Path (fmap (uncurry Point) chimneyCos) green Solid

-- >>> writeToFile chimneyHouse
--

chimneyHouse :: Picture
chimneyHouse = [door, house, window, chimney]

-- Part 2
movePoint :: Point -> Vector -> Point
movePoint (Point x y) (Vector xv yv) = Point (x + xv) (y + yv)

movePictureObject :: Vector -> PictureObject -> PictureObject
movePictureObject vec (Path points colour lineStyle) =
  Path (fmap (`movePoint` vec) points) colour lineStyle
movePictureObject vec c@(Circle center _ _ _ _) =
  c {centerPO = movePoint center vec}
movePictureObject vec e@Ellipse {centerPO} =
  e {centerPO = movePoint centerPO vec}
movePictureObject vec p@Polygon {pointsPO} =
  p {pointsPO = fmap (`movePoint` vec) pointsPO}

-- add other cases

-- >>> testMove
--
testMove =
  let myRed = red {opacityC = 180}
      xy = (Point 400 400)
      circ = Circle xy 100 myRed Solid SolidFill
      v = (Vector 100 100)
   in writeToFile [circ, movePictureObject v circ]

-- Part 3

-- >>> writeToFile (simpleCirclePic (Colour 153 0 153 100) 22)
--



simpleCirclePic :: Colour -> Float -> Picture
simpleCirclePic col n =
  [ Circle (Point 400 400) (fromIntegral x * (400 / n)) col Solid SolidFill
    | x <- [1 .. (floor n)]
  ]

-- use 'writeToFile' to write a picture to file "ex01.png" to test your
-- program if you are not using Haskell for Mac
-- e.g., call
-- writeToFile [house, door]

writeToFile pic = writePng "ex01.png" (drawPicture 3 pic)

