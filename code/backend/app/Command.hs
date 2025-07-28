module Command where

import Poplar
import Participant
import Credits
import Measurement
import Tree
import State
import Action
import Payment
import Time

executeCommands :: [Poplar -> Poplar] -> Poplar -> Poplar
executeCommands commands poplar = foldl (\p command -> command p) poplar commands

addParticipant :: String -> Poplar -> Poplar
addParticipant userName oldPoplar =
  let newParticipantId = nextId oldPoplar
      newParticipant =
        Participant {
        participantId = ParticipantId newParticipantId,
        Participant.name = userName,
        creditBalance = CreditsAmount 0,
        preferenceSpec = Nothing,
        measurementDefinitions = []
        }
  in oldPoplar {
    nextId = 1 + nextId oldPoplar,
    participants = newParticipant : participants oldPoplar
    }

defineNewMeasurement :: ParticipantId -> String -> String -> Poplar -> Poplar
defineNewMeasurement definerId measurementName measurementDescription poplar =
    let newDefinitionId = nextId poplar
        newMeasurementDefinition = MeasurementDefinition {
          measurementDefinitionId = MeasurementDefinitionId newDefinitionId,
          Measurement.name = measurementName,
          Measurement.description = measurementDescription
          }
        updatedParticipants =
          map (\participant ->
                  if participantId participant == definerId
                  then addDefinitionToParticipant newMeasurementDefinition participant
                  else participant)
          (participants poplar)
  in poplar {
      nextId = 1 + nextId poplar,
      participants = updatedParticipants
      }

setUserPreferenceToSpecificValue :: ParticipantId -> MeasurementDefinitionId -> Int -> Poplar -> Poplar
setUserPreferenceToSpecificValue participantId' measurementDefinitionId' requiredValue poplar =
  let updatedParticipants =
        map (\participant ->
                if participantId participant == participantId'
                then Participant.setUserPreferenceToSpecificValue measurementDefinitionId' requiredValue participant
                else participant)
        (participants poplar)
  in
    poplar {
    participants = updatedParticipants
    }

submitMeasurementForToday :: ParticipantId -> MeasurementDefinitionId -> Value -> Poplar -> Poplar
submitMeasurementForToday participantId' measurementDefinitionId' value' poplar =
  if not (participantDefinedMeasurement poplar participantId' measurementDefinitionId')
  then error "The given participant did not define the given measurement"
  else
    let measurementToAdd = Measurement {
          definitionId = measurementDefinitionId',
          value = value'
          }
        tree' = tree poplar
        (RootNode todayDay todayState children) = rootNode tree'
    in
      if measurementAlreadyProvided todayState measurementDefinitionId'
      then error "The given measurement has already been entered for today"
      else
        let updatedMeasurements = measurementToAdd : (measurements todayState)
            updatedState = todayState {measurements = updatedMeasurements}
            updatedRootNode = (RootNode todayDay updatedState children)
            updatedTree = tree' {rootNode = updatedRootNode}
        in poplar {tree = updatedTree}

measurementAlreadyProvided :: State -> MeasurementDefinitionId -> Bool
measurementAlreadyProvided state measurementDefinitionId' =
  any (\m -> definitionId m == measurementDefinitionId') (measurements state)

participantDefinedMeasurement :: Poplar -> ParticipantId -> MeasurementDefinitionId -> Bool
participantDefinedMeasurement poplar participantId' measurementDefinitionId' =
  let
    participants' = participants poplar
    matchingParticipants = filter (\p -> participantId p == participantId') participants'
  in
    case matchingParticipants of
      [participant] -> any (\md -> measurementDefinitionId md == measurementDefinitionId') (measurementDefinitions participant)
      _ -> False

startPlanningPhase :: Poplar -> Poplar
startPlanningPhase = error "nyi"

endPlanningPhase :: Poplar -> Poplar
endPlanningPhase = error "nyi"

addStateNode :: ParticipantId -> NodeReference -> Day -> [Measurement] -> Poplar -> Poplar
addStateNode _ parentNodeId (Day d) measurements' poplar =
  let
    (newNodeId, incrementedPoplar) = incrementId poplar
    newNode = StateNode (Day d) (State {
                                stateId = StateId newNodeId,
                                measurements = measurements'
                                }) []
  in
    addChildNode parentNodeId newNode (Just (Day (d - 1))) incrementedPoplar

addActionNode :: ParticipantId -> NodeReference -> String -> Poplar -> Poplar
addActionNode createdByParticipantId parentNodeId description' poplar =
  let
    (newNodeId, incrementedPoplar) = incrementId poplar
    newNode = ActionNode Action {
      actionId = ActionId newNodeId,
      createdBy = createdByParticipantId,
      Action.description = description'
      } []

  in
    addChildNode parentNodeId newNode Nothing incrementedPoplar

addPaymentNode :: ParticipantId -> NodeReference -> CreditsAmount -> Poplar -> Poplar
addPaymentNode createdByParticipantId parentNodeId paymentAmount poplar =
  let
    (newNodeId, incrementedPoplar) = incrementId poplar
    newNode = PaymentNode Payment {
      paymentId = PaymentId newNodeId,
      toParticipant = createdByParticipantId,
      amount = paymentAmount
      } []
    
  in
    addChildNode parentNodeId newNode Nothing incrementedPoplar

allocateCreditsToState :: Poplar -> Poplar
allocateCreditsToState = error "nyi"

-- TODO Optimise this
addChildNode :: NodeReference -> TreeNode -> Maybe Day -> Poplar -> Poplar
addChildNode parentNodeId nodeToAdd requiredParentDay poplar =
  let tree' = tree poplar
      (RootNode rootDay rootState children) = rootNode tree'
  in
    case parentNodeId of
      RootNodeRef ->
        -- Add to the root node
        let updatedRootNode = RootNode rootDay rootState (nodeToAdd : children)
            updatedTree = tree' {rootNode = updatedRootNode}
        in
          if dayMatchesRequirement rootDay requiredParentDay
          then poplar {tree = updatedTree}
          else error "Attempted to add a node with an invalid Day"
      NonRootNodeRef parentNodeId' ->
        let updatedRootChildren = map (addChildToTreeNode parentNodeId' nodeToAdd requiredParentDay rootDay) children
            updatedRootNode = RootNode rootDay rootState updatedRootChildren
        in
          poplar {tree = tree' {rootNode = updatedRootNode}}

dayMatchesRequirement :: Day -> Maybe Day -> Bool
dayMatchesRequirement _ Nothing = True
dayMatchesRequirement (Day d) (Just (Day requiredDay)) = d == requiredDay

addChildToTreeNode :: Int -> TreeNode -> Maybe Day -> Day -> TreeNode -> TreeNode
addChildToTreeNode parentNodeId nodeToAdd requiredParentDay currentDay possibleParentNode =
  case possibleParentNode of
    StateNode day state children ->
      let (StateId stateNodeId) = stateId state
      in
        if stateNodeId == parentNodeId
        then StateNode day state (nodeToAdd : children)
        else StateNode day state (map (addChildToTreeNode parentNodeId nodeToAdd requiredParentDay day) children)
    PaymentNode payment children ->
      let (PaymentId paymentNodeId) = paymentId payment
      in
        if paymentNodeId == parentNodeId
        then PaymentNode payment (nodeToAdd : children)
        else PaymentNode payment (map (addChildToTreeNode parentNodeId nodeToAdd requiredParentDay currentDay) children)
    ActionNode action children ->
      let (ActionId actionNodeId) = actionId action
      in
        if actionNodeId == parentNodeId
        then ActionNode action (nodeToAdd : children)
        else ActionNode action (map (addChildToTreeNode parentNodeId nodeToAdd requiredParentDay currentDay) children)
