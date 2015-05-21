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
