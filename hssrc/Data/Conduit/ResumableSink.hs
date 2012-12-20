module Data.Conduit.ResumableSink (
  ResumableSink(..), connectResumeSink, newResumableSink, closeResumableSink,
  (+$$), (++$$), (-++$$)
                                  )
where

import Data.Conduit.Internal
import Data.Conduit
import Data.Void

-- | 
data ResumableSink m i r = ResumableSink (Sink i m r)

-- | Connects a new source to a resumable sink. The result will be Right an updated
--   ResumableSink or Left result if the Sink completes.
connectResumeSink
  :: Monad m => Source m i -> ResumableSink m i r -> m (Either r (ResumableSink m i r))
connectResumeSink left0 (ResumableSink right0) =
    go (return ()) left0 right0
  where
    go :: Monad m => m () -> Source m i -> Sink i m r -> m (Either r (ResumableSink m i r))
    go leftFinal left right =
        case right of
            Done r -> leftFinal >> (return . Left $ r)
            PipeM mp -> mp >>= go leftFinal left
            HaveOutput _ _ o -> absurd o
            Leftover p i -> go leftFinal (HaveOutput left leftFinal i) p
            NeedInput rp _ ->
                case left of
                    Leftover p () -> go leftFinal p right
                    HaveOutput left' leftFinal' o -> go leftFinal' left' (rp o)
                    NeedInput _ lc -> go leftFinal (lc ()) right
                    Done () -> return . Right  $ ResumableSink right
                    PipeM mp -> mp >>= \left' -> go leftFinal left' right

-- | Converts a sink into a ResumableSink that can be used with ++$$
newResumableSink :: Monad m => Sink i m r -> ResumableSink m i r
newResumableSink s = ResumableSink s

-- | Closes a ResumableSink and gets the final result.
closeResumableSink :: Monad m => ResumableSink m i r -> m r
closeResumableSink (ResumableSink sink) =
  go sink
    where
      go right =
        case right of
          Leftover p i -> do
            res <- connectResumeSink (HaveOutput (return ()) (return ()) i) (ResumableSink p)
            case res of
              Left r -> return r
              Right rs -> closeResumableSink rs
          HaveOutput _ _ o -> absurd o
          NeedInput _ r -> go (r ())
          Done r -> return r
          PipeM mp -> mp >>= go

(+$$) :: Monad m => Source m i -> Sink i m r -> m (Either r (ResumableSink m i r))
source +$$ sink = source `connectResumeSink` (newResumableSink sink)

(++$$) :: Monad m => Source m i -> ResumableSink m i r -> m (Either r (ResumableSink m i r))
(++$$) = connectResumeSink

(-++$$) :: Monad m => Source m i -> ResumableSink m i r -> m r
source -++$$ (ResumableSink sink) = do
  r <- source $$ sink
  return r
