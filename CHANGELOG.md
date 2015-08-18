## 0.8

* Add Base64 variants (Luke Taylor)
* Fix compilation on Haiku (Jessica Hamilton)

## 0.7

* Fix fixed sized scrubber written too hastily, that would zero out memory, as the index
  was written through byte size, whereas the primitive would consider it as WordX type index.
  it would helps if Ghc.Prim had better documentation.

## 0.6

* Fix compilation on architecture where endianness is not a compile time define related
  to their cabal arch().

## 0.5

* Add Base32 support (Nicolas Di Prima)
* Fix build on 32 bit by simplifying scrubber, and adding Word64 type + operations compatibility

## 0.4

* Add Ord instances for SipHash and FnvHash (Nicolas Di Prima)
* Fix GHC-7.2 build by defining unsafeShiftL (Adam Bergmark)
* Fix show instance of Bytes to properly display each bytes instead of just the end one
* Add View type that emulate a view on a ByteArray type (Nicolas Di Prima)

## 0.3

* fix missing modules from tests on sdist

## 0.2

* make concat more generic as to what the output is going to be, and at the same
  time reduce the constraint on the input to just Access
* make all byte array operation safer related to negative size. now replicate, zero, and alloc will returns
  an empty byte array when asking for negative size
* replace 'pack' in Data.ByteArray.Pack by 'fill', as to not conflict with 'Data.ByteArray.pack'.
  Also swap the length and monadic action to be more naturally used
* add a deprecated 'pack' that alias to 'fill' for now
* loosen constraint of Data.ByteArray.Pack.putBytes from ByteArray to ByteArrayAccess

## 0.1

* Initial release
