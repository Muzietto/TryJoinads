
// open Core.String // why compiler errors ?

// who says this is an infix operator?????
let (>>=) sa fasb = fun newState -> // why not 'stateBind' ?
    let (ss,aa) = sa newState
    fasb aa ss

let stateBind sa fasb = fun newState ->
    let (ss,aa) = sa newState
    fasb aa ss
    
let getState = fun s -> (s,s)
let setState ss = fun _ -> (ss,())

let returnS a = fun s -> (s,a)

type state<'s,'a> = 's -> 's * 'a  // state or State?

type StateClass() = 
    member t.Bind(sa: state<'s,'a>, fasb: 'a -> state<'s,'b>): state<'s,'b> = stateBind sa fasb // how come??? bind is a function, not an application!!!
    // remove 'sa fasb' --> "This construct causes code to be less generic"
//  member t.Bind(sa: state<'s,'a>, fasb: 'a -> state<'s,'b>): state<'s,'b> = sa >>= fasb // mind-blowing!
    member t.Return(a: 'a): state<'s,'a> = returnS a// was fun s -> (s,a)
    
let state = StateClass();;  // with or without new?

type Tree<'a> = // tree or Tree?
    | Leaf of 'a // why not leaf<'a> ?
    | Branch of Tree<'a> * Tree<'a>
    
let myTree =
    Branch(
        Leaf("a"),
        Branch(
            Branch(
                Leaf("b"),
                Leaf("c")),
            Leaf("d")))
            
let updateState = fun n -> (n+1,n)

let rec monadicLabeler tree =
    match tree with
    | Leaf ll -> updateState >>= (fun n -> state.Return(Leaf(n,ll))) // (fun s -> ((s + 1),Leaf(s,ll)))
    | Branch (left,right) -> 
        monadicLabeler left >>= 
            (fun leftPLB -> 
                monadicLabeler right >>= 
                    (fun rightPLB ->
                        state.Return(Branch(leftPLB,rightPLB))))

let rec sugaredLabeler tree = state {
    match tree with
        | Leaf ll -> 
            let! n = getState
            do! setState (n+1) // do! e in ce --> state.Bind(s,fun () -> Translation(ce))
            return Leaf(n,ll)
        | Branch(left,right) ->
            let! leftPLB = sugaredLabeler left
            let! rightPLB = sugaredLabeler right
            return Branch(leftPLB,rightPLB)
}

let (statte,labb) = monadicLabeler myTree 0
let (statte_sug,labb_sug) = sugaredLabeler myTree 0

