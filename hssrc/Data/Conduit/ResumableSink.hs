{-# LANGUAGE CPP, PatternSynonyms, Rank2Types #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module Data.Conduit.ResumableSink (
  ResumableSink(..), connectResumeSink, newResumableSink, closeResumableSink,
  (+$$), (++$$), (-++$$)
                                  )
where

import Data.Conduit.Internal
import Data.Void

#if MIN_VERSION_conduit(1,3,0)
pattern ConduitM :: forall i o m r. (forall b. (r -> Pipe i i o () m b) -> Pipe i i o () m b) -> ConduitT i o m r
pattern ConduitM act = ConduitT act
#else
(.|) :: Monad m => ConduitM a b m () -> ConduitM b c m r -> ConduitM a c m r
(.|) = (=$=)
#endif

newtype ResumableSink i m r = ResumableSink (ConduitM i Void m r)

#if MIN_VERSION_conduit(1,3,0)
-- | Connects a new source to a resumable sink. The result will be Right an updated
--   ResumableSink or Left result if the Sink completes.
connectResumeSink :: Monad m => ConduitM () i m () -> ResumableSink i m r -> m (Either r (ResumableSink i m r))
connectResumeSink (ConduitM left') (ResumableSink (ConduitM right')) = go (left' Done) (right' Done)
  where
    go :: Monad m
       => Pipe () () i    () m ()
       -> Pipe i  i  Void () m r
       -> m (Either r (ResumableSink i m r))
    go (NeedInput cont0 _ ) right               = go (cont0 ()) right
    go (Done ())            right               = return . Right . ResumableSink $ ConduitM (\finalize -> right >>= finalize)
    go (PipeM pm)           right               = pm >>= \left -> go left right
    go (Leftover left ())   right               = go left right
    go (HaveOutput left1 o) (NeedInput cont0 _) = go left1 (cont0 o)
    go _                    (HaveOutput _ o)    = absurd o
    go _                    (Done r)            = return $ Left r
    go left                 (PipeM pm)          = pm >>= go left
    go left                 (Leftover right i)  = go (HaveOutput left i) right
#else
-- | Connects a new source to a resumable sink. The result will be Right an updated
--   ResumableSink or Left result if the Sink completes.
connectResumeSink :: Monad m => ConduitM () i m () -> ResumableSink i m r -> m (Either r (ResumableSink i m r))
connectResumeSink (ConduitM left') (ResumableSink (ConduitM right')) = go (return ()) (left' Done) (right' Done)
  where
    go :: Monad m
       => m ()
       -> Pipe () () i    () m ()
       -> Pipe i  i  Void () m r
       -> m (Either r (ResumableSink i m r))
    go final  (NeedInput cont0 _ )        right               = go final (cont0 ()) right
    go final  (Done ())                   right               = (Right . ResumableSink $ ConduitM (\finalize -> right >>= finalize)) <$ final
    go final  (PipeM pm)                  right               = pm >>= \left -> go final left right
    go final  (Leftover left ())          right               = go final left right
    go final0 (HaveOutput left1 final1 o) (NeedInput cont0 _) = go (final0 >> final1) left1 (cont0 o)
    go _      _                           (HaveOutput _ _ o)  = absurd o
    go final  _                           (Done r)            = Left r <$ final
    go final  left                        (PipeM pm)          = pm >>= go final left
    go final  left                        (Leftover right i)  = go final (HaveOutput left (return ()) i) right
#endif

-- | Converts a sink into a ResumableSink that can be used with ++$$
newResumableSink :: Monad m => ConduitM i Void m r -> ResumableSink i m r
newResumableSink = ResumableSink

-- | Closes a ResumableSink and gets the final result.
closeResumableSink :: Monad m => ResumableSink i m r -> m r
closeResumableSink (ResumableSink sink) = runConduit $ return () .| sink

-- | Connects a source and a sink. The result will be Right a
--   ResumableSink or Left result if the Sink completes.
(+$$) :: Monad m => ConduitM () i m () -> ConduitM i Void m r -> m (Either r (ResumableSink i m r))
source +$$ sink = source `connectResumeSink` newResumableSink sink

-- | Connects a new source to a resumable sink. The result will be Right an updated
--   ResumableSink or Left result if the Sink completes.
(++$$) :: Monad m => ConduitM () i m () -> ResumableSink i m r -> m (Either r (ResumableSink i m r))
(++$$) = connectResumeSink

-- | Attaches a source to a resumable sink, finishing the sink and returning a result.
(-++$$) :: Monad m => ConduitM () i m () -> ResumableSink i m r -> m r
source -++$$ ResumableSink sink = runConduit $ source .| sink
