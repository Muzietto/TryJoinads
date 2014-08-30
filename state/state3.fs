// state modifying arrays

let bindState sa fasb = fun newState ->
   let (ss,aa) = sa newState
   fasb ss aa
   
let (>>=) sa fasb = bindState sa fasb

let returnState a = fun s -> (s,a)

type state<'s,'a> = 's -> 's * 'a

type StateClass() = 
    member kkk.Bind sa fasb = bindState sa fasb
    member kkk.Return a = returnState a
    
let state = new StateClass();;

// type Stack = int list // this is just an alias!

type Stack = StackContents of int list // good idea!

let stacco1 = StackContents [1;2;3;4;5]

// push(x) is a state monad
let push x s = state {
    match s with
    | StackContents items -> 
        return (StackContents (x::items),())
}

// stack -> (cdr stack,car stack)
let statePop s = state {
    match s with
    | StackContents [] -> 
        return (StackContents [],0)
    | StackContents (head::tail) -> 
        return (StackContents tail,head)
}

// imperative monad

let im1 = statePop >>= (fun x -> returnState x)

//let impMonad = statePop >>= (fun x -> statePop >>= (fun y -> statePush x))

let wer = im1 [5,4,3]
