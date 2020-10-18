module Utils
    (monadicFirst
    ) where
import Control.Monad

monadicFirst :: Monad m => m [a] -> m a
monadicFirst m = do
    x <- m
    return (x!!0)

