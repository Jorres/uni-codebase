data Vector a = Vector2D a a
              | Vector3D a a a

pack :: Vector a -> [a]
pack (Vector2D a b) = [a, b]
pack (Vector3D a b c) = [a, b, c]

vecLen :: Vector Double -> Double
vecLen = sqrt . sum . map (^2) . pack

-- Constructors are still simple functions and they have types: :t
