module Data.Conduit.ResumableSink (
  ResumableSink(..), connectResumeSink, newResumableSink, closeResumableSink,
  (+$$), (++$$), (-++$$)
                                  )
where

import Data.Conduit.Internal
import Data.Void

-- | 
data ResumableSink i m r = ResumableSink (Sink i m r)

-- | Connects a new source to a resumable sink. The result will be Right an updated
--   ResumableSink or Left result if the Sink completes.
connectResumeSink :: Monad m => Source m i -> ResumableSink i m r -> m (Either r (ResumableSink i m r))
connectResumeSink (ConduitM left') (ResumableSink (ConduitM right')) = go (return ()) (left' Done) (right' Done)
  where
    go :: Monad m
       => m ()
       -> Pipe () () i    () m ()
       -> Pipe i  i  Void () m r
       -> m (Either r (ResumableSink i m r))
    go final  (NeedInput cont0 _ )        right               = go final (cont0 ()) right
    go final  (Done ())                   right               = return . Right . ResumableSink $ ConduitM (\finalize -> right >>= finalize)
    go final  (PipeM pm)                  right               = pm >>= \left -> go final left right
    go final  (Leftover left ())          right               = go final left right
    go final0 (HaveOutput left1 final1 o) (NeedInput cont0 _) = go (final0 >> final1) left1 (cont0 o)
    go _      _                           (HaveOutput _ _ o)  = absurd o
    go final  _                           (Done r)            = Left r <$ final
    go final  left                        (PipeM pm)          = pm >>= go final left
    go final  left                        (Leftover right i)  = go final (HaveOutput left (return ()) i) right

-- | Converts a sink into a ResumableSink that can be used with ++$$
newResumableSink :: Monad m => Sink i m r -> ResumableSink i m r
newResumableSink = ResumableSink

-- | Closes a ResumableSink and gets the final result.
closeResumableSink :: Monad m => ResumableSink i m r -> m r
closeResumableSink (ResumableSink sink) = runConduit $ return () =$= sink

-- | Connects a source and a sink. The result will be Right a
--   ResumableSink or Left result if the Sink completes.
(+$$) :: Monad m => Source m i -> Sink i m r -> m (Either r (ResumableSink i m r))
source +$$ sink = source `connectResumeSink` newResumableSink sink

-- | Connects a new source to a resumable sink. The result will be Right an updated
--   ResumableSink or Left result if the Sink completes.
(++$$) :: Monad m => Source m i -> ResumableSink i m r -> m (Either r (ResumableSink i m r))
(++$$) = connectResumeSink

-- | Attaches a source to a resumable sink, finishing the sink and returning a result.
(-++$$) :: Monad m => Source m i -> ResumableSink i m r -> m r
source -++$$ ResumableSink sink = source $$ sink
