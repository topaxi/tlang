const Option = {
    Some(value) {
        return {
            tag: "Some",
            "0": value,
        };
    },
    None: { tag: "None" },
};
const Result = {
    Ok(value) {
        return {
            tag: "Ok",
            "0": value,
        };
    },
    Err(error) {
        return {
            tag: "Err",
            "0": error,
        };
    },
};
// len(a[]) -> int
function len(list) {
    return list.length;
}
// random_int(int) -> int
function random_int(max) {
    return Math.floor(Math.random() * max);
}
// compose(fn(a) -> b, fn(b) -> c) -> fn(a) -> c
function compose(f, g) {
    return function(x) {
        return f(g(x));
    };
}
// map(a[], fn(a) -> b) -> b[]
function map(...args) {
    if (args[0].length === 0) {
        return [];
    } else if (args[0].length >= 1) {
        let f = args[1];
        let x = args[0][0];
        let xs = args[0].slice(1);
        return [f(x), ...map(xs, f)];
    }
}
// filter(a[], fn(a) -> bool) -> a[]
function filter(...args) {
    while (true) {
        if (args[0].length === 0) {
            return [];
        } else if (args[0].length >= 1 && args[1](args[0][0])) {
            let f = args[1];
            let x = args[0][0];
            let xs = args[0].slice(1);
            return [x, ...filter(xs, f)];
        } else if (args[0].length >= 1) {
            let f = args[1];
            let x = args[0][0];
            let xs = args[0].slice(1);
            let $tmp$b = xs;
            let $tmp$c = f;
            args[0] = $tmp$b;
            args[1] = $tmp$c;
        }
    }
}
// partition(a[], fn(a) -> bool) -> (a[], a[])
// partition(a[], fn(a) -> bool, a[], a[]) -> (a[], a[])
function partition(...args) {
    while (true) {
        if (args.length === 2 && args[0].length === 0) {
            return [[], []];
        } else if (args.length === 2) {
            let list = args[0];
            let predicate = args[1];
            return partition(list, predicate, [], []);
        } else if (args.length === 4 && args[0].length === 0) {
            // partition(a[], fn(a) -> bool, a[], a[]) -> (a[], a[])
            let satisfies = args[2];
            let does_not_satisfy = args[3];
            return [satisfies, does_not_satisfy];
        } else if (args.length === 4 && args[0].length >= 1 && args[1](args[0][0])) {
            let predicate = args[1];
            let satisfies = args[2];
            let does_not_satisfy = args[3];
            let x = args[0][0];
            let xs = args[0].slice(1);
            let $tmp$b = xs;
            let $tmp$c = predicate;
            let $tmp$d = [...satisfies, x];
            let $tmp$e = does_not_satisfy;
            args[0] = $tmp$b;
            args[1] = $tmp$c;
            args[2] = $tmp$d;
            args[3] = $tmp$e;
        } else if (args.length === 4 && args[0].length >= 1) {
            let predicate = args[1];
            let satisfies = args[2];
            let does_not_satisfy = args[3];
            let x = args[0][0];
            let xs = args[0].slice(1);
            let $tmp$b = xs;
            let $tmp$c = predicate;
            let $tmp$d = satisfies;
            let $tmp$e = [...does_not_satisfy, x];
            args[0] = $tmp$b;
            args[1] = $tmp$c;
            args[2] = $tmp$d;
            args[3] = $tmp$e;
        }
    }
}
// filter_map(a[], fn(a) -> Option(b)) -> b[]
function filter_map(...args) {
    let $tmp$a;
    let $tmp$b;
    while (true) {
        if (args[0].length === 0) {
            return [];
        } else if (args[0].length >= 1 && ($tmp$a = args[1](args[0][0])) && $tmp$a.tag === "Some" && (($tmp$b = $tmp$a[0]), true)) {
            let f = args[1];
            let x = args[0][0];
            let xs = args[0].slice(1);
            return [$tmp$b, ...filter_map(xs, f)];
        } else if (args[0].length >= 1) {
            let f = args[1];
            let x = args[0][0];
            let xs = args[0].slice(1);
            let $tmp$b = xs;
            let $tmp$c = f;
            args[0] = $tmp$b;
            args[1] = $tmp$c;
        }
    }
}
// foldl(a[], b, fn(b, a) -> b) -> b
function foldl(...args) {
    while (true) {
        if (args[0].length === 0) {
            let acc = args[1];
            return acc;
        } else if (args[0].length >= 1) {
            let acc = args[1];
            let f = args[2];
            let x = args[0][0];
            let xs = args[0].slice(1);
            let $tmp$b = xs;
            let $tmp$c = f(acc, x);
            let $tmp$d = f;
            args[0] = $tmp$b;
            args[1] = $tmp$c;
            args[2] = $tmp$d;
        }
    }
}
// foldr(a[], b, fn(a, b) -> b) -> b
function foldr(...args) {
    if (args[0].length === 0) {
        let acc = args[1];
        return acc;
    } else if (args[0].length >= 1) {
        let acc = args[1];
        let f = args[2];
        let x = args[0][0];
        let xs = args[0].slice(1);
        return f(x, foldr(xs, acc, f));
    }
}
// sum(int[] xs) -> int
function sum(...args) {
    if (args[0].length === 0) {
        return 0;
    } else if (args[0].length >= 1) {
        let x = args[0][0];
        let xs = args[0].slice(1);
        return x + sum(xs);
    }
}
// zip(a[], b[]) -> [a, b][]
function zip(...args) {
    if (args[0].length === 0 && args[1].length === 0) {
        return [];
    } else if (args[0].length >= 1 && args[1].length >= 1) {
        let x = args[0][0];
        let xs = args[0].slice(1);
        let y = args[1][0];
        let ys = args[1].slice(1);
        return [[x, y], ...zip(xs, ys)];
    }
}
