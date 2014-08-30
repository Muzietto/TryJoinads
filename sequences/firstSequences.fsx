
let cons car cdr = Seq.append (Seq.singleton car) cdr;;

//let rec from i = cons i (from(i+1));; // endless loop

let rec from i = Seq.delay(fun () -> cons i (from(i+1)));

let res1 = Seq.nth 2 (from 6) // --> 8

let sift a sequence = Seq.filter(fun x -> x % a <> 0) sequence;;

// sift 1 (from 1) // returns nothing

let natsSince2 = Seq.initInfinite (fun x -> x+2);;

let res2 = sift 2 natsSince2

let rec sieve sequence = Seq.delay(fun () ->
        let car = Seq.nth 0 sequence
        let cdr = Seq.skip 1 sequence
        cons car (sieve (sift car cdr)));;


let primes = sieve natsSince2;;

let prime ord = Seq.nth ord primes;;

let res3 = prime 20;;

// SEQUENCE EXPRESSIONS --> lazy

let rec from' i = seq {
    yield i
    yield! from (i+1)
}

let res4 = Seq.nth 3 (from' 3) // --> 6

let rec sieve' sequence = seq {
    let car = Seq.nth 0 sequence
    let cdr = Seq.skip 1 sequence
    yield cons car (sieve (sift car cdr)) // why is this wrong?
};; // viene fuori una seq<seq<Int>>

let rec sieve'' sequence = seq {
    let car = Seq.nth 0 sequence
    let cdr = Seq.skip 1 sequence
    yield car
    yield! sieve(sift car cdr) // this is the good one!
}

let odds = Seq.filter (fun x -> x % 2 <> 0) (Seq.initInfinite (fun x -> x));;

let nats = Seq.initInfinite (fun x -> x);;

let res5 = (Seq.nth 2 nats) * 3 // 6
