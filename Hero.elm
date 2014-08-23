module Hero where

type State = {age : Int}


default = {age = 8}

oldHero = {age = 81}

medHero = {age = 22}

getOlder : State -> State
getOlder st = {st | age <- st.age + 1}

getYounger st = {st | age <- st.age - 1}