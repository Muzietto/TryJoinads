
// open Core.String // why compiler errors?

type state<'s,'a> = 's -> 's * 'a  // state or State? why all thos apexes?

type StateClass() = // is this a constructor? why call it TYPE?
    member t.Bind(sa: state<'s,'a>, fasb: 'a -> state<'s,'b>): state<'s,'b> = 
        fun newState ->
            let (ss,aa) = sa newState
            fasb aa ss
    member t.Return(a: 'a): state<'s,'a> = fun s -> (s,a)
    
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

let rec monadicLabeler = fun tree ->
    match tree with
    | Leaf l -> state.Bind(updateState, fun n -> state.Return(Leaf(l,n))) //fun s -> (s+1,Leaf (l,s))
    | Branch (left,right) -> 
        state.Bind(monadicLabeler left, fun leftPLB -> 
            state.Bind(monadicLabeler right, fun rightPLB ->
                state.Return(Branch(leftPLB,rightPLB))))
                
let (statte,labb) = monadicLabeler myTree 0
