module Foreign.Ptr
  ( Ptr, nullPtr, castPtr, plusPtr, alignPtr, minusPtr,
    FunPtr, nullFunPtr, castFunPtr, castFunPtrToPtr, castPtrToFunPtr, freeHaskellFunPtr,
    IntPtr, ptrToIntPtr, intPtrToPtr, WordPtr, ptrToWordPtr, wordPtrToPtr
  )
where

import Foreign.Base
