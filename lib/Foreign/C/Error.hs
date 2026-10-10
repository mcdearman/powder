-- C's errno: the numbers it can be, and making an I/O error of it. The
-- numbers are Linux's; an error Linux has not got is -1, which errno
-- never is.
module Foreign.C.Error
  ( Errno (..), eOK, e2BIG, eACCES, eADDRINUSE, eADDRNOTAVAIL, eADV, eAFNOSUPPORT, eAGAIN, eALREADY, eBADF, eBADMSG, eBADRPC, eBUSY, eCHILD, eCOMM, eCONNABORTED, eCONNREFUSED, eCONNRESET, eDEADLK, eDESTADDRREQ, eDIRTY, eDOM, eDQUOT, eEXIST, eFAULT, eFBIG, eFTYPE, eHOSTDOWN, eHOSTUNREACH, eIDRM, eILSEQ, eINPROGRESS, eINTR, eINVAL, eIO, eISCONN, eISDIR, eLOOP, eMFILE, eMLINK, eMSGSIZE, eMULTIHOP, eNAMETOOLONG, eNETDOWN, eNETRESET, eNETUNREACH, eNFILE, eNOBUFS, eNODATA, eNODEV, eNOENT, eNOEXEC, eNOLCK, eNOLINK, eNOMEM, eNOMSG, eNONET, eNOPROTOOPT, eNOSPC, eNOSR, eNOSTR, eNOSYS, eNOTBLK, eNOTCONN, eNOTDIR, eNOTEMPTY, eNOTSOCK, eNOTTY, eNXIO, eOPNOTSUPP, ePERM, ePFNOSUPPORT, ePIPE, ePROCLIM, ePROCUNAVAIL, ePROGMISMATCH, ePROGUNAVAIL, ePROTO, ePROTONOSUPPORT, ePROTOTYPE, eRANGE, eREMCHG, eREMOTE, eROFS, eRPCMISMATCH, eRREMOTE, eSHUTDOWN, eSOCKTNOSUPPORT, eSPIPE, eSRCH, eSRMNT, eSTALE, eTIME, eTIMEDOUT, eTOOMANYREFS, eTXTBSY, eUSERS, eWOULDBLOCK, eXDEV,
    isValidErrno, getErrno, resetErrno, errnoToIOError, throwErrno,
    throwErrnoIf, throwErrnoIf_, throwErrnoIfRetry, throwErrnoIfRetry_, throwErrnoIfMinus1, throwErrnoIfMinus1_,
    throwErrnoIfMinus1Retry, throwErrnoIfMinus1Retry_, throwErrnoIfNull, throwErrnoIfNullRetry,
    throwErrnoIfRetryMayBlock, throwErrnoIfRetryMayBlock_, throwErrnoIfMinus1RetryMayBlock, throwErrnoIfMinus1RetryMayBlock_,
    throwErrnoIfNullRetryMayBlock
  )
where

import Foreign.Base
import Foreign.C.String
import Foreign.C.Types
import Powder.Base (Handle, IOError (..), IOErrorType (..))

newtype Errno = Errno CInt

instance Eq Errno where
  e@(Errno a) == f@(Errno b) = isValidErrno e && isValidErrno f && a == b

eOK :: Errno
eOK = Errno 0

e2BIG :: Errno
e2BIG = Errno (7)

eACCES :: Errno
eACCES = Errno (13)

eADDRINUSE :: Errno
eADDRINUSE = Errno (98)

eADDRNOTAVAIL :: Errno
eADDRNOTAVAIL = Errno (99)

eADV :: Errno
eADV = Errno (68)

eAFNOSUPPORT :: Errno
eAFNOSUPPORT = Errno (97)

eAGAIN :: Errno
eAGAIN = Errno (11)

eALREADY :: Errno
eALREADY = Errno (114)

eBADF :: Errno
eBADF = Errno (9)

eBADMSG :: Errno
eBADMSG = Errno (74)

eBADRPC :: Errno
eBADRPC = Errno (-1)

eBUSY :: Errno
eBUSY = Errno (16)

eCHILD :: Errno
eCHILD = Errno (10)

eCOMM :: Errno
eCOMM = Errno (70)

eCONNABORTED :: Errno
eCONNABORTED = Errno (103)

eCONNREFUSED :: Errno
eCONNREFUSED = Errno (111)

eCONNRESET :: Errno
eCONNRESET = Errno (104)

eDEADLK :: Errno
eDEADLK = Errno (35)

eDESTADDRREQ :: Errno
eDESTADDRREQ = Errno (89)

eDIRTY :: Errno
eDIRTY = Errno (-1)

eDOM :: Errno
eDOM = Errno (33)

eDQUOT :: Errno
eDQUOT = Errno (122)

eEXIST :: Errno
eEXIST = Errno (17)

eFAULT :: Errno
eFAULT = Errno (14)

eFBIG :: Errno
eFBIG = Errno (27)

eFTYPE :: Errno
eFTYPE = Errno (-1)

eHOSTDOWN :: Errno
eHOSTDOWN = Errno (112)

eHOSTUNREACH :: Errno
eHOSTUNREACH = Errno (113)

eIDRM :: Errno
eIDRM = Errno (43)

eILSEQ :: Errno
eILSEQ = Errno (84)

eINPROGRESS :: Errno
eINPROGRESS = Errno (115)

eINTR :: Errno
eINTR = Errno (4)

eINVAL :: Errno
eINVAL = Errno (22)

eIO :: Errno
eIO = Errno (5)

eISCONN :: Errno
eISCONN = Errno (106)

eISDIR :: Errno
eISDIR = Errno (21)

eLOOP :: Errno
eLOOP = Errno (40)

eMFILE :: Errno
eMFILE = Errno (24)

eMLINK :: Errno
eMLINK = Errno (31)

eMSGSIZE :: Errno
eMSGSIZE = Errno (90)

eMULTIHOP :: Errno
eMULTIHOP = Errno (72)

eNAMETOOLONG :: Errno
eNAMETOOLONG = Errno (36)

eNETDOWN :: Errno
eNETDOWN = Errno (100)

eNETRESET :: Errno
eNETRESET = Errno (102)

eNETUNREACH :: Errno
eNETUNREACH = Errno (101)

eNFILE :: Errno
eNFILE = Errno (23)

eNOBUFS :: Errno
eNOBUFS = Errno (105)

eNODATA :: Errno
eNODATA = Errno (61)

eNODEV :: Errno
eNODEV = Errno (19)

eNOENT :: Errno
eNOENT = Errno (2)

eNOEXEC :: Errno
eNOEXEC = Errno (8)

eNOLCK :: Errno
eNOLCK = Errno (37)

eNOLINK :: Errno
eNOLINK = Errno (67)

eNOMEM :: Errno
eNOMEM = Errno (12)

eNOMSG :: Errno
eNOMSG = Errno (42)

eNONET :: Errno
eNONET = Errno (64)

eNOPROTOOPT :: Errno
eNOPROTOOPT = Errno (92)

eNOSPC :: Errno
eNOSPC = Errno (28)

eNOSR :: Errno
eNOSR = Errno (63)

eNOSTR :: Errno
eNOSTR = Errno (60)

eNOSYS :: Errno
eNOSYS = Errno (38)

eNOTBLK :: Errno
eNOTBLK = Errno (15)

eNOTCONN :: Errno
eNOTCONN = Errno (107)

eNOTDIR :: Errno
eNOTDIR = Errno (20)

eNOTEMPTY :: Errno
eNOTEMPTY = Errno (39)

eNOTSOCK :: Errno
eNOTSOCK = Errno (88)

eNOTTY :: Errno
eNOTTY = Errno (25)

eNXIO :: Errno
eNXIO = Errno (6)

eOPNOTSUPP :: Errno
eOPNOTSUPP = Errno (95)

ePERM :: Errno
ePERM = Errno (1)

ePFNOSUPPORT :: Errno
ePFNOSUPPORT = Errno (96)

ePIPE :: Errno
ePIPE = Errno (32)

ePROCLIM :: Errno
ePROCLIM = Errno (-1)

ePROCUNAVAIL :: Errno
ePROCUNAVAIL = Errno (-1)

ePROGMISMATCH :: Errno
ePROGMISMATCH = Errno (-1)

ePROGUNAVAIL :: Errno
ePROGUNAVAIL = Errno (-1)

ePROTO :: Errno
ePROTO = Errno (71)

ePROTONOSUPPORT :: Errno
ePROTONOSUPPORT = Errno (93)

ePROTOTYPE :: Errno
ePROTOTYPE = Errno (91)

eRANGE :: Errno
eRANGE = Errno (34)

eREMCHG :: Errno
eREMCHG = Errno (78)

eREMOTE :: Errno
eREMOTE = Errno (66)

eROFS :: Errno
eROFS = Errno (30)

eRPCMISMATCH :: Errno
eRPCMISMATCH = Errno (-1)

eRREMOTE :: Errno
eRREMOTE = Errno (-1)

eSHUTDOWN :: Errno
eSHUTDOWN = Errno (108)

eSOCKTNOSUPPORT :: Errno
eSOCKTNOSUPPORT = Errno (94)

eSPIPE :: Errno
eSPIPE = Errno (29)

eSRCH :: Errno
eSRCH = Errno (3)

eSRMNT :: Errno
eSRMNT = Errno (69)

eSTALE :: Errno
eSTALE = Errno (116)

eTIME :: Errno
eTIME = Errno (62)

eTIMEDOUT :: Errno
eTIMEDOUT = Errno (110)

eTOOMANYREFS :: Errno
eTOOMANYREFS = Errno (109)

eTXTBSY :: Errno
eTXTBSY = Errno (26)

eUSERS :: Errno
eUSERS = Errno (87)

eWOULDBLOCK :: Errno
eWOULDBLOCK = Errno (11)

eXDEV :: Errno
eXDEV = Errno (18)

isValidErrno :: Errno -> Bool
isValidErrno (Errno n) = n /= -1

-- What errno was after the last C function the program called.
getErrno :: IO Errno
getErrno = primErrnoGet >>= \n -> return (Errno (fromInteger n))

resetErrno :: IO ()
resetErrno = primErrnoSet 0

foreign import ccall "string.h strerror" c_strerror :: CInt -> IO (Ptr CChar)

-- An I/O error for an errno: of the kind the number is of, saying what C
-- says of the number.
errnoToIOError :: String -> Errno -> Maybe Handle -> Maybe String -> IOError
errnoToIOError place errno@(Errno n) h file = IOError kind place said h file
  where
    said = primPerformIO (c_strerror n >>= peekCString)
    kind
      | errno `elem` [eEXIST, eISCONN, eADDRINUSE] = AlreadyExists
      | errno `elem` [eNOENT, eNXIO, eNODEV, eSRCH] = NoSuchThing
      | errno `elem` [eBUSY, eDEADLK, eTXTBSY, eAGAIN] = ResourceBusy
      | errno `elem` [eNOSPC, eNOMEM, eMFILE, eNFILE, eDQUOT, eMLINK, eNOBUFS] = ResourceExhausted
      | errno `elem` [eACCES, ePERM, eROFS] = PermissionDenied
      | errno == eNOTTY = IllegalOperation
      | otherwise = OtherError

throwErrno :: String -> IO a
throwErrno place = getErrno >>= \errno -> ioError (errnoToIOError place errno Nothing Nothing)

throwErrnoIf :: (a -> Bool) -> String -> IO a -> IO a
throwErrnoIf bad place act = act >>= \x -> if bad x then throwErrno place else return x

throwErrnoIf_ :: (a -> Bool) -> String -> IO a -> IO ()
throwErrnoIf_ bad place act = throwErrnoIf bad place act >> return ()

-- Again, for as long as what went wrong is that the call was interrupted.
throwErrnoIfRetry :: (a -> Bool) -> String -> IO a -> IO a
throwErrnoIfRetry bad place act =
  act >>= \x ->
    if bad x
      then getErrno >>= \errno -> if errno == eINTR then throwErrnoIfRetry bad place act else throwErrno place
      else return x

throwErrnoIfRetry_ :: (a -> Bool) -> String -> IO a -> IO ()
throwErrnoIfRetry_ bad place act = throwErrnoIfRetry bad place act >> return ()

-- Again too where the call would have had to wait, after doing what was
-- given to do then.
throwErrnoIfRetryMayBlock :: (a -> Bool) -> String -> IO a -> IO b -> IO a
throwErrnoIfRetryMayBlock bad place act blocked =
  act >>= \x ->
    if bad x
      then
        getErrno >>= \errno ->
          if errno == eINTR
            then throwErrnoIfRetryMayBlock bad place act blocked
            else if errno == eWOULDBLOCK || errno == eAGAIN then blocked >> throwErrnoIfRetryMayBlock bad place act blocked else throwErrno place
      else return x

throwErrnoIfRetryMayBlock_ :: (a -> Bool) -> String -> IO a -> IO b -> IO ()
throwErrnoIfRetryMayBlock_ bad place act blocked = throwErrnoIfRetryMayBlock bad place act blocked >> return ()

throwErrnoIfMinus1 :: (Eq a, Num a) => String -> IO a -> IO a
throwErrnoIfMinus1 = throwErrnoIf (== -1)

throwErrnoIfMinus1_ :: (Eq a, Num a) => String -> IO a -> IO ()
throwErrnoIfMinus1_ = throwErrnoIf_ (== -1)

throwErrnoIfMinus1Retry :: (Eq a, Num a) => String -> IO a -> IO a
throwErrnoIfMinus1Retry = throwErrnoIfRetry (== -1)

throwErrnoIfMinus1Retry_ :: (Eq a, Num a) => String -> IO a -> IO ()
throwErrnoIfMinus1Retry_ = throwErrnoIfRetry_ (== -1)

throwErrnoIfMinus1RetryMayBlock :: (Eq a, Num a) => String -> IO a -> IO b -> IO a
throwErrnoIfMinus1RetryMayBlock = throwErrnoIfRetryMayBlock (== -1)

throwErrnoIfMinus1RetryMayBlock_ :: (Eq a, Num a) => String -> IO a -> IO b -> IO ()
throwErrnoIfMinus1RetryMayBlock_ = throwErrnoIfRetryMayBlock_ (== -1)

throwErrnoIfNull :: String -> IO (Ptr a) -> IO (Ptr a)
throwErrnoIfNull = throwErrnoIf (== nullPtr)

throwErrnoIfNullRetry :: String -> IO (Ptr a) -> IO (Ptr a)
throwErrnoIfNullRetry = throwErrnoIfRetry (== nullPtr)

throwErrnoIfNullRetryMayBlock :: String -> IO (Ptr a) -> IO b -> IO (Ptr a)
throwErrnoIfNullRetryMayBlock = throwErrnoIfRetryMayBlock (== nullPtr)
