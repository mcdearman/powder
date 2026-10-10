-- Marshalling: the four libraries for it, and a way to treat an action
-- that only works on memory of its own as the value it gives.
module Foreign.Marshal
  ( module Foreign.Marshal.Alloc,
    module Foreign.Marshal.Array,
    module Foreign.Marshal.Error,
    module Foreign.Marshal.Utils,
    unsafeLocalState
  )
where

import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Marshal.Error
import Foreign.Marshal.Utils

unsafeLocalState :: IO a -> a
unsafeLocalState = primPerformIO
