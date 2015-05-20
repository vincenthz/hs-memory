## 0.2

* replace 'pack' in Data.ByteArray.Pack by 'fill', as to not conflict with 'Data.ByteArray.pack'.
  Also swap the length and monadic action to be more naturally used
* add a deprecated 'pack' that alias to 'fill' for now
* loosen constraint of Data.ByteArray.Pack.putBytes from ByteArray to ByteArrayAccess

## 0.1

* Initial release
