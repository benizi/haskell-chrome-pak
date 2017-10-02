# haskell-chrome-pak

Tool for extracting and modifying Chrom(e|ium) ".pak" files

# Intro

# Usage

Example:

```haskell
import Pak (Archive)

import qualified Data.Binary as Binary
import qualified Data.ByteString as S

d <- S.readFile "/usr/lib/chromium/locales/en-US.pak"
Archive { pakEntries = es@(e:_) } = Binary.decode d
putStrLn $ entryData e d
```

# Checklist

- [x] Parse a `.pak` file to get contents of objects
- [x] Generate a `.pak` file from a map of ID => blob
- [x] Modify an existing `.pak` file
- [ ] Clean up module layout

# License

Copyright © 2017 Benjamin R. Haskell

Distributed under the MIT License (included in file: [LICENSE](LICENSE)).
