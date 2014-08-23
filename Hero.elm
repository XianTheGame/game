module Hero where



type State = {age : Int}

default = {age = 8}

getOlder : State -> State
getOlder st = {st | age <- st.age + 1}

getYounger st = {st | age <- st.age - 1}