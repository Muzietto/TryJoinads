// state modifying arrays

let bindState sa fasb = fun newState ->
   let (ss,aa) = sa newState
   fasb ss aa
   
let (>>=) sa fasb = bindState sa fasb

let returnState a = fun s -> (s,a)

let getState = fun s -> (s,s)
let setState ss = fun _ -> (ss,())

type state<'s,'a> = 's -> 's * 'a

type StateClass() = 
    member this.Bind(sa: state<'s,'a>, fasb: 'a -> state<'s,'b>): state<'s,'b> = bindState sa fasb
    member this.Return(a: 'a): state<'s,'a> = returnState a
    member this.Zero() = returnState ()
    
let state = StateClass();;

// type Stack = int list // this is just an alias!

type Stack = StackContents of int list // good idea!

let stacco1 = StackContents [1;2;3;4;5]

// stack -> (cdr stack,car stack)
let statePop:state<Stack,int> = fun s -> state { 
    match s with
//    | StackContents [] -> return 0
    | StackContents (head::tail) ->
        
       // do! setState(StackContents tail)
       // return head

        setState(StackContents tail) >>= (fun _ -> state.Return head)
}

// push(x) is a state monad
let statePush(x:int):state<Stack,int> = state { fun s ->
    match s with
 //   | StackContents [] -> return 0
    | StackContents items -> 
    
       // do! setState(StackContents (x::items))
       // return ()
        
         setState(StackContents (x::items)) >>= (fun _ -> state.Return ())
}

// imperative monad

let qweqwe = statePop StackContents [1,2,3,4,5] 123

//let im1 = statePop >>= (fun x -> returnState x)
//let wer = im1 [5,4,3]

//let impMonad = statePop >>= (fun x -> statePop >>= (fun y -> statePush x))

