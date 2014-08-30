// state modifying arrays

let bindState sa fasb = fun newState ->
   let (ss,aa) = sa newState
   fasb ss aa

let returnState a = fun s -> (s,a)

type state<'s,'a> = 's -> 's * 'a

type StateClass() = 
    member kkk.Bind sa fasb = bindState sa fasb
    member kkk.Return a = returnState a
    
let state = new StateClass();;

// type Stack = int list // this is just an alias!

type Stack = StackContents of int list // good idea!

let stacco1 = StackContents [1;2;3;4;5]

let statePop s =
    match s with
    | StackContents (head::tail) -> 
        returnState (StackContents tail,head)

let push x s =
     match s with
    | StackContents items -> 
        returnState (StackContents (x::items),())

let impMonad = state {
    let! x = statePop
    do! statePop
    return statePush x
} 