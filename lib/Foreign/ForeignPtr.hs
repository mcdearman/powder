module Foreign.ForeignPtr
  ( ForeignPtr, FinalizerPtr, newForeignPtr, newForeignPtr_, addForeignPtrFinalizer, withForeignPtr, finalizeForeignPtr,
    unsafeForeignPtrToPtr, touchForeignPtr, castForeignPtr,
    mallocForeignPtr, mallocForeignPtrBytes, mallocForeignPtrArray, mallocForeignPtrArray0
  )
where

import Foreign.Base
