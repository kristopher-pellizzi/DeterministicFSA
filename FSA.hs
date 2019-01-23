{-
    This program allows to scan an input string through a DETERMINISTIC Finite State Automaton.
    To define a FSA, the user must define its finalStates set, its initial state in the "currentState" field and a list containing all transitions.
    A single transition is defined by defining the starting state, the input character to read and the final state of the transition. In practice a transition of the type
    Transition q0 'a' q1 is a transition of the FSA of the kind q0 - a -> q1. The FSA must be deterministic: even if more than a transition reading a character c is defined in the same
    state, only the first one in the list is executed.
-}

newtype ID = ID [Char] deriving (Eq)

data State = State {
    id :: ID
} deriving (Eq)

data Transition t = Transition {
    startingState :: State, 
    inputSymbol :: t, 
    endingState :: State
}
data FSA t = FSA {
    finalStates :: [State],
    currentState :: State,
    transitions :: [Transition t]
}

isIn :: State -> [State] -> Bool
isIn _ [] = False
isIn s (l:ls) 
    | l == s = True
    | otherwise = isIn s ls

execute :: (Eq t) => [Transition t] -> State -> t -> State
execute trans state input = head states
    where
        states :: [State]
        states = (fmap (\t -> getTransitionResult t) (filter (\t -> (getInputSymbol t) == input && (getInitialState t) == state) trans))

getInputSymbol :: Transition t -> t
getInputSymbol (Transition s i e) = i

getInitialState :: Transition t -> State
getInitialState (Transition s i e) = s

getTransitionResult :: Transition t -> State
getTransitionResult (Transition s i e) = e

isDefined :: (Eq t) => [Transition t] -> State -> t -> Bool
isDefined [] _ _ = False
isDefined (l:ls) s t
    | t == (getInputSymbol l) && s == (getInitialState l) = True
    | otherwise = isDefined ls s t

scan :: (Eq t) => FSA t -> [t] -> Bool
scan (FSA finalSet current trans) [] = isIn current finalSet
scan (FSA finalSet current trans) (i:is)
    | isDefined trans current i = scan (FSA finalSet (execute trans current i) trans) is
    | otherwise = False