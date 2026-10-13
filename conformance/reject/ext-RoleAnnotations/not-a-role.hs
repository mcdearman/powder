{-# LANGUAGE RoleAnnotations #-}
-- A parameter is nominal, representational or phantom
type role Box sideways

data Box a = Box a

main :: IO ()
main = return ()
