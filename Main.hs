import Control.Monad (join)

import Options.Applicative

import Commands (opts)


main :: IO ()
main = join $ execParser (info opts idm)
