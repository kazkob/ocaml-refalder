type ('a, 'r) t = ('a -> 'r) -> 'r

let pure : 'a -> ('a, 'r) t = fun x k -> k x
