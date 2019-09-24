module Run.Halogen where

import Prelude

import Control.Monad.Free (hoistFree)
import Data.Maybe (Maybe(..), maybe)
import Data.Symbol (class IsSymbol, SProxy(..))
import Data.Tuple (Tuple(..))
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Halogen as H
import Halogen.Data.Slot as Slot
import Halogen.Query.ChildQuery as CQ
import Prim.Row (class Cons)
import Run (EFFECT, FProxy, Run(..), AFF, case_, interpret, lift, on, send)
import Run.Except (FAIL, fail)
import Run.State (STATE, State(..), _state)

_halogen = SProxy :: SProxy "halogen"
type HALOGEN state action slots output m = FProxy (H.HalogenF state action slots output m)

-- | de-variant an effect stack containing only the halogen effect, so that it can be used for query/action evaluation
runHalogen :: forall state action slots output m. Run (halogen :: HALOGEN state action slots output m) ~> H.HalogenM state action slots output m
runHalogen (Run x) = H.HalogenM $ hoistFree (on _halogen identity case_) x

-- | Inner NT for runStateHalogen
handleStateHalogen :: forall state action slots output m r. Functor m => State state ~> Run (halogen :: HALOGEN state action slots output m | r)
handleStateHalogen (State ss sa) = lift _halogen $ H.State $ Tuple <$> sa <*> ss

-- | Convert State effects to use the state of the halogen component.
runStateHalogen :: forall state action slots output m r a. Functor m => Run (state :: STATE state, halogen :: HALOGEN state action slots output m | r) a -> Run (halogen :: HALOGEN state action slots output m | r) a
runStateHalogen x = interpret (on _state handleStateHalogen send) x

_effect :: SProxy "effect"
_effect = SProxy

-- | Lifts the "effect" effect into HalogenF, so that you can use runHalogen
runEffectHalogen :: forall state action slots output m r a. MonadEffect m => Run (effect :: EFFECT, halogen :: HALOGEN state action slots output m | r) a -> Run (halogen :: HALOGEN state action slots output m | r) a
runEffectHalogen x = interpret (on _effect (lift _halogen <<< H.Lift <<< liftEffect) send) x

_aff :: SProxy "aff"
_aff = SProxy

-- | Lifts the "aff" effect into HalogenF, so that you can use runHalogen
runAffHalogen :: forall state action slots output m r a. MonadAff m => Run (aff :: AFF, halogen :: HALOGEN state action slots output m | r) a -> Run (halogen :: HALOGEN state action slots output m | r) a
runAffHalogen x = interpret (on _aff (lift _halogen <<< H.Lift <<< liftAff) send) x

-- | The query function from Halogen.Query.HalogenM without the Free wrapper, so it can be put into a variant.
query
  :: forall state action output m label slots query output' slot a _1
   . Cons label (Slot.Slot query output' slot) _1 slots
  => IsSymbol label
  => Ord slot
  => SProxy label
  -> slot
  -> query a
  -> H.HalogenF state action slots output m (Maybe a)
query label p q = H.ChildQuery $ CQ.mkChildQueryBox $
  CQ.ChildQuery (\k → maybe (pure Nothing) k <<< Slot.lookup label p) q identity

-- | Inner NT for runChildHalogen
handleChildHalogen :: forall state action output m r label slots f output' _1. Cons label (Slot.Slot f output' Unit) _1 slots => Functor m => IsSymbol label => SProxy label -> f ~> Run (halogen :: HALOGEN state action slots output m, except :: FAIL | r)
handleChildHalogen label f =
  (lift _halogen $ query label unit f) >>=
  case _ of
    Just y -> pure y
    Nothing -> fail

-- | For certain effects in the Run effect stack it can be useful to evaluate it using a component with the effect algebra as its query algebra, and using the effect id as the component slot id (with Unit as the index). runChildHalogen automates that.
runChildHalogen :: forall state action output m r label slots f output' _1 r' a. Cons label (Slot.Slot f output' Unit) _1 slots => Cons label (FProxy f) (halogen :: HALOGEN state action slots output m, except :: FAIL | r) r' => Functor m => IsSymbol label => SProxy label -> Run r' a -> Run (halogen :: HALOGEN state action slots output m, except :: FAIL | r) a
runChildHalogen label x = interpret (on label (handleChildHalogen label) send) x
