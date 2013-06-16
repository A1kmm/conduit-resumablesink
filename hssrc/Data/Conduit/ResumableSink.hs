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
        case unConduitM right of
            Done r -> leftFinal >> (return . Left $ r)
            PipeM mp -> mp >>= go leftFinal left . ConduitM
            HaveOutput _ _ o -> absurd o
            Leftover p i -> go leftFinal (ConduitM $ HaveOutput (unConduitM left) leftFinal i) $ ConduitM p
            NeedInput rp _ ->
                case unConduitM left of
                    Leftover p () -> go leftFinal (ConduitM p) right
                    HaveOutput left' leftFinal' o -> go leftFinal' (ConduitM left') (ConduitM $ rp o)
                    NeedInput _ lc -> go leftFinal (ConduitM $ lc ()) right
                    Done () -> return . Right  $ ResumableSink right
                    PipeM mp -> mp >>= \left' -> go leftFinal (ConduitM left') right

-- | Converts a sink into a ResumableSink that can be used with ++$$
newResumableSink :: Monad m => Sink i m r -> ResumableSink m i r
newResumableSink = ResumableSink

-- | Closes a ResumableSink and gets the final result.
closeResumableSink :: Monad m => ResumableSink m i r -> m r
closeResumableSink (ResumableSink sink) =
  go (unConduitM sink)
    where
      go right =
        case right of
          Leftover p i -> do
            res <- connectResumeSink (ConduitM $ HaveOutput (return ()) (return ()) i) (ResumableSink $ ConduitM p)
            case res of
              Left r -> return r
              Right rs -> closeResumableSink rs
          HaveOutput _ _ o -> absurd o
          NeedInput _ r -> go (r ())
          Done r -> return r
          PipeM mp -> mp >>= go

-- | Connects a source and a sink. The result will be Right a
--   ResumableSink or Left result if the Sink completes.
(+$$) :: Monad m => Source m i -> Sink i m r -> m (Either r (ResumableSink m i r))
source +$$ sink = source `connectResumeSink` newResumableSink sink

-- | Connects a new source to a resumable sink. The result will be Right an updated
--   ResumableSink or Left result if the Sink completes.
(++$$) :: Monad m => Source m i -> ResumableSink m i r -> m (Either r (ResumableSink m i r))
(++$$) = connectResumeSink

-- | Attaches a source to a resumable sink, finishing the sink and returning a result.
(-++$$) :: Monad m => Source m i -> ResumableSink m i r -> m r
source -++$$ ResumableSink sink = source $$ sink
