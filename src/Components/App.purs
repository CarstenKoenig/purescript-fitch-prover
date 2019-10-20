module Components.App (component) where

import Prelude

import Components.ApplyRuleModal as RuleDlg
import Components.NewExprButton as NewBtn
import Data.Array as Array
import Data.List (List, (:))
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..))
import Effect.Class (class MonadEffect)
import Environment (AssumptionStack)
import Environment as Env
import Expressions (Expr)
import FitchRules as Fitch
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML (HTML)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Rules (Rule, RuleInstance)
import Rules as Rules
import Scope (Scope)
import Scope as Scope

type State =
  { showRuleModal :: Maybe Rule
  , premisses :: Array Expr
  , currentStack :: AssumptionStack
  , history :: List HistoryItem
  }

data Action
  = ShowRuleModal Rule
  | AddConclusion Expr
  | HandleRuleModal RuleDlg.Message
  | HandleAssumeNew NewBtn.Message

type ChildSlots =
  ( newRuleModal :: RuleDlg.Slot Unit
  , assumeNew ::NewBtn.Slot Unit
  )

_newRuleModal :: SProxy "newRuleModal"
_newRuleModal = SProxy

_assumeNew :: SProxy "assumeNew"
_assumeNew = SProxy

component :: forall q i o m. MonadEffect m => H.Component HH.HTML q i o m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }

initialState :: forall i. i -> State
initialState _ =
  { showRuleModal: Nothing
  , premisses: []
  , currentStack:  Env.NoAssumptions Scope.empty
  , history: List.Nil
  }

render :: forall m. MonadEffect m => State -> H.ComponentHTML Action ChildSlots m
render state = HH.div_
  [ case state.showRuleModal of
      Just rule -> HH.slot _newRuleModal unit 
        RuleDlg.component 
          { scope: Env.scopeOf state.currentStack 
          , rule
          } 
        (Just <<< HandleRuleModal)
      Nothing -> HH.text "" 
  , HH.div_
    [ showHistory (List.toUnfoldable $ List.reverse state.history) 
    , showAssumeNew
    , showRuleButtons (Env.scopeOf state.currentStack) Fitch.rules
    ]
  ]
  where
  showAssumeNew =
    HH.div
      [ HP.class_ (ClassName "box") ] 
      [ HH.h1 [ HP.class_ (ClassName "subtitle") ] [ HH.text "assume" ]
      , HH.slot _assumeNew unit NewBtn.component unit (Just <<< HandleAssumeNew)
      , showImplButtons state
      ]

handleAction ::forall o m. Action -> H.HalogenM State Action ChildSlots o m Unit
handleAction = case _ of
  ShowRuleModal rule ->
    H.modify_ (\st -> st { showRuleModal = Just rule })
  HandleRuleModal RuleDlg.Canceled ->
    H.modify_ (\st -> st { showRuleModal = Nothing })
  HandleRuleModal (RuleDlg.NewRule r) ->
    H.modify_ (\st -> useRule r $ st { showRuleModal = Nothing })
  HandleAssumeNew (NewBtn.NewExpr expr) ->
    H.modify_ (addAssumption expr)
  AddConclusion expr ->
    H.modify_ (addConclusion expr)

showRuleButtons :: forall w. Scope -> Array Rule -> HTML w Action
showRuleButtons _ rules | Array.null rules = HH.text ""
showRuleButtons scope rules =
  HH.div
    [ HP.class_ (ClassName "box") ] 
    [ HH.h1 [ HP.class_ (ClassName "subtitle") ] [ HH.text "rules" ]
    , HH.div
      [ HP.class_ (ClassName "buttons is-marginless") ]
      (map showRuleButton rules)
    ]
  where
  showRuleButton rule = HH.button 
    [ HP.class_ (ClassName "button") 
    , HP.disabled (not $ Rules.isUsableWith rule.ruleRecipe scope)
    , HE.onClick (\_ -> Just (ShowRuleModal rule))
    ] 
    [ HH.text rule.ruleName ]

showImplButtons :: forall w. State -> HTML w Action
showImplButtons state | Env.stackDepth state.currentStack <= 0 = HH.text ""
showImplButtons state =
  HH.div
    [ HP.class_ (ClassName "box") ] 
    [ HH.h1 [ HP.class_ (ClassName "subtitle") ] [ HH.text "results" ]
    , HH.div
      [ HP.class_ (ClassName "buttons is-marginless") ]
      (map showImplButton $ Scope.toArray $ Env.scopeOf state.currentStack)
    ]
  where
  showImplButton expr = HH.button 
    [ HP.class_ (ClassName "button") 
    , HE.onClick (\_ -> Just $ AddConclusion expr)
    ]
    [ HH.text $ "=> " <> show expr ]

----------------------------------------------------------------------
-- History

data HistoryItem
  = UsedRule { ruleInstance :: RuleInstance, newFact :: Expr }
  | AddedPremisse { premisse :: Expr }
  | NewAssumption { assumption :: Expr }
  | FoundImplication { newFact :: Expr }

useRule :: { ruleInstance :: RuleInstance, newFact :: Expr } -> State -> State
useRule r state =
  let (Tuple _ newStack) = Env.runWith state.currentStack (Env.tryApply r.ruleInstance)
  in state 
    { currentStack = newStack
    , history = UsedRule r : state.history
    }

addPremisse :: Expr -> State -> State
addPremisse prem state =
  let (Tuple _ newStack) = Env.runWith state.currentStack (Env.addExpr prem)
  in state 
    { currentStack = newStack
    , history = AddedPremisse { premisse: prem } : state.history
    }

addAssumption :: Expr -> State -> State
addAssumption assumption state =
  let (Tuple _ newStack) = Env.runWith state.currentStack (Env.assume assumption)
  in state 
    { currentStack = newStack
    , history = NewAssumption { assumption } : state.history
    }

addConclusion :: Expr -> State -> State
addConclusion conclusion state =
  let (Tuple found newStack) = Env.runWith state.currentStack (Env.introduceImplication conclusion)
  in case found of
    Nothing -> state
    Just impl -> 
      state
        { currentStack = newStack
        , history = FoundImplication { newFact: impl } : state.history
        }


showHistory :: forall w i. Array HistoryItem -> HTML w i
showHistory items =
  HH.div
    [ HP.class_ (ClassName "box history") ] 
    [ HH.h1 [ HP.class_ (ClassName "subtitle") ] [ HH.text "history" ]
    , HH.ol_ (let res = go [] items in res.list)
    ]
  where
  go acc hs =
    case Array.uncons hs of
      Nothing -> { add: HH.text "", rest: [], list: acc }
      Just { head, tail } -> 
        case head of
          (AddedPremisse p) ->
            let new = HH.li_ [ HH.span_ [ HH.text "premisse: ", HH.strong_ [ HH.text $ show p.premisse ] ] ]
            in go (Array.snoc acc new) tail
          (UsedRule r) ->
            let 
              new = HH.li_ 
                [ HH.span_ 
                  [ HH.strong_ [ HH.text $ show r.newFact ] 
                  , HH.text " - "
                  , HH.em_ [ HH.text r.ruleInstance.description ] 
                  ]
                ] 
            in go (Array.snoc acc new) tail
          (NewAssumption a) ->
            let new = HH.li_ [ HH.span_ [ HH.text "assume: ", HH.strong_ [ HH.text $ show a.assumption ] ] ]
                res = go [] tail
                addSub = if Array.null res.list then identity else flip Array.snoc (HH.li_ [HH.ol_ res.list])
            in go (Array.snoc (addSub (Array.snoc acc new)) res.add) res.rest
          (FoundImplication impl) ->
            let new = HH.li_ [ HH.span_ [ HH.strong_ [ HH.text $ show impl.newFact ] ] ]
            in { add: new, rest: tail, list: acc }
