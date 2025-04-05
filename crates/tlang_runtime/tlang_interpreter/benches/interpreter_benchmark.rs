use criterion::{Criterion, criterion_group, criterion_main};
use tlang_ast_lowering::LoweringContext;
use tlang_hir::hir::Module;
use tlang_hir_opt::{
    constant_folding::ConstantFolder, constant_propagation::ConstantPropagator,
    dead_code_elimination::DeadCodeEliminator, hir_opt::HirOptimizer,
};
use tlang_interpreter::Interpreter;
use tlang_memory::prelude::TlangValue;
use tlang_parser::Parser;

struct BenchInterpreter {
    lowering_context: LoweringContext,
    interpreter: Interpreter,
    optimizer: HirOptimizer,
}

impl BenchInterpreter {
    fn new() -> Self {
        let mut optimizer = HirOptimizer::new();
        optimizer.add_pass(Box::new(ConstantFolder::new()));
        optimizer.add_pass(Box::new(ConstantPropagator::new()));
        optimizer.add_pass(Box::new(DeadCodeEliminator::new().with_preserve_root(true)));

        BenchInterpreter {
            lowering_context: LoweringContext::default(),
            interpreter: Interpreter::new(),
            optimizer,
        }
    }

    fn compile(&mut self, src: &str) -> Module {
        let mut parser = Parser::from_source(src);
        let ast = parser.parse().unwrap();
        let mut hir = self.lowering_context.lower_module_in_current_scope(&ast);
        self.optimizer.optimize_module(&mut hir);
        hir
    }

    fn eval(&mut self, hir: &Module) -> TlangValue {
        self.interpreter.eval(hir)
    }
}

// Helper function to compile and return both interpreter and module
fn compile_interpreter(initial_source: &str) -> (BenchInterpreter, Module) {
    let mut interpreter = BenchInterpreter::new();
    let module = interpreter.compile(initial_source);
    (interpreter, module)
}

// Benchmark for different Fibonacci implementations
fn fibonacci_benchmark(c: &mut Criterion) {
    let mut group = c.benchmark_group("Fibonacci Implementation");

    let fib_code = r#"
        fn naive_fib(n) {
            if n <= 1 {
                n
            } else {
                naive_fib(n - 1) + naive_fib(n - 2)
            }
        }

        fn pattern_fib(0) { 0 }
        fn pattern_fib(1) { 1 }
        fn pattern_fib(n) { pattern_fib(n - 1) + pattern_fib(n - 2) }

        fn tail_recursive_fib(n) { tail_recursive_fib(n, 0, 1) }
        fn tail_recursive_fib(0, a, _) { a }
        fn tail_recursive_fib(n, a, b) { rec tail_recursive_fib(n - 1, b, a + b) }
    "#;

    let naive_test = r#"
        naive_fib(10);
    "#;

    let pattern_test = r#"
        pattern_fib(10);
    "#;

    let tail_test = r#"
        tail_recursive_fib(10);
    "#;

    // Compile the initial module with all function definitions
    let (mut interp, module) = compile_interpreter(fib_code);
    // Evaluate the initial module to define the functions
    interp.eval(&module);

    // Compile the test modules
    let naive_module = interp.compile(naive_test);
    let pattern_module = interp.compile(pattern_test);
    let tail_module = interp.compile(tail_test);

    group.bench_function("naive_fib(10)", |b| {
        b.iter(|| interp.eval(&naive_module));
    });

    group.bench_function("pattern_fib(10)", |b| {
        b.iter(|| interp.eval(&pattern_module));
    });

    group.bench_function("tail_recursive_fib(10)", |b| {
        b.iter(|| interp.eval(&tail_module));
    });

    group.finish();
}

// Benchmark for closure operations
fn closure_benchmark(c: &mut Criterion) {
    let mut group = c.benchmark_group("Closures");

    let adder_code = r#"
        fn make_adder(x) {
            return fn(y) { x + y };
        }
        let add5 = make_adder(5);
    "#;

    let call_code = r#"
        add5(10);
    "#;

    // Compile the initial module with function definitions and closure creation
    let (mut interp, module) = compile_interpreter(adder_code);
    // Evaluate the initial module to define the functions and create the closure
    interp.eval(&module);

    // Compile the call module
    let call_module = interp.compile(call_code);

    group.bench_function("closure_call", |b| {
        b.iter(|| interp.eval(&call_module));
    });

    group.finish();
}

// Benchmark for list operations (which involve a lot of objects and cloning)
fn list_benchmark(c: &mut Criterion) {
    let mut group = c.benchmark_group("List Operations");

    let list_ops_code = r#"
        fn reverse(list) { reverse(list, []) }
        fn reverse([], acc) { acc }
        fn reverse([x, ...xs], acc) { rec reverse(xs, [x, ...acc]) }

        fn map(list, f) {
            map_helper(list, f, [])
        }

        fn map_helper([], f, acc) { acc }
        fn map_helper([x, ...xs], f, acc) {
            rec map_helper(xs, f, [...acc, f(x)])
        }

        let test_list = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20];
    "#;

    let reverse_code = r#"
        reverse(test_list);
    "#;

    let map_code = r#"
        map(test_list, fn(x) { x * 2 });
    "#;

    // Compile the initial module with function definitions and test data
    let (mut interp, module) = compile_interpreter(list_ops_code);
    // Evaluate the initial module to define the functions and create the test list
    interp.eval(&module);

    // Compile the test modules
    let reverse_module = interp.compile(reverse_code);
    let map_module = interp.compile(map_code);

    group.bench_function("list_reverse", |b| {
        b.iter(|| interp.eval(&reverse_module));
    });

    group.bench_function("list_map", |b| {
        b.iter(|| interp.eval(&map_module));
    });

    group.finish();
}

// Benchmark for struct and field access (which also uses cloning)
fn struct_benchmark(c: &mut Criterion) {
    let mut group = c.benchmark_group("Struct Operations");

    let struct_code = r#"
        struct Point {
            x: Int,
            y: Int,
        }

        fn Point::new(x, y) {
            Point { x: x, y: y }
        }

        fn distance(p1, p2) {
            let dx = p1.x - p2.x;
            let dy = p1.y - p2.y;
            math::sqrt(dx * dx + dy * dy)
        }

        let p1 = Point::new(0, 0);
        let p2 = Point::new(3, 4);
    "#;

    let creation_code = r#"
        Point::new(10, 20);
    "#;

    let field_access_code = r#"
        p1.x + p1.y;
    "#;

    let method_code = r#"
        distance(p1, p2);
    "#;

    // Compile the initial module with struct and function definitions
    let (mut interp, module) = compile_interpreter(struct_code);
    // Evaluate the initial module to define the struct, functions, and test points
    interp.eval(&module);

    // Compile the test modules
    let creation_module = interp.compile(creation_code);
    let field_access_module = interp.compile(field_access_code);
    let method_module = interp.compile(method_code);

    group.bench_function("struct_creation", |b| {
        b.iter(|| interp.eval(&creation_module));
    });

    group.bench_function("struct_field_access", |b| {
        b.iter(|| interp.eval(&field_access_module));
    });

    group.bench_function("struct_method", |b| {
        b.iter(|| interp.eval(&method_module));
    });

    group.finish();
}

// Benchmark for function calls with small argument lists
fn small_arg_call_benchmark(c: &mut Criterion) {
    let mut group = c.benchmark_group("Small Argument Function Calls");

    let setup_code = r#"
        fn call0() { 42 }
        fn call1(a) { a }
        fn call2(a, b) { a + b }
        fn call3(a, b, c) { a + b + c }
        fn call4(a, b, c, d) { a + b + c + d }
        fn call5(a, b, c, d, e) { a + b + c + d + e }
        fn call6(a, b, c, d, e, f) { a + b + c + d + e + f }
        fn nested_call(a, b) { call1(a) + call1(b) }
    "#;

    let call0_code = r#"
        call0();
    "#;

    let call1_code = r#"
        call1(1);
    "#;

    let call2_code = r#"
        call2(1, 2);
    "#;

    let call3_code = r#"
        call3(1, 2, 3);
    "#;

    let call4_code = r#"
        call4(1, 2, 3, 4);
    "#;

    let call5_code = r#"
        call5(1, 2, 3, 4, 5);
    "#;

    let call6_code = r#"
        call6(1, 2, 3, 4, 5, 6);
    "#;

    let nested_code = r#"
        nested_call(10, 20);
    "#;

    // Compile the initial module with function definitions
    let (mut interp, module) = compile_interpreter(setup_code);
    // Evaluate the initial module to define the functions
    interp.eval(&module);

    // Compile the test modules
    let call0_module = interp.compile(call0_code);
    let call1_module = interp.compile(call1_code);
    let call2_module = interp.compile(call2_code);
    let call3_module = interp.compile(call3_code);
    let call4_module = interp.compile(call4_code);
    let call5_module = interp.compile(call5_code);
    let call6_module = interp.compile(call6_code);
    let nested_module = interp.compile(nested_code);

    group.bench_function("call0_no_args", |b| {
        b.iter(|| interp.eval(&call0_module));
    });

    group.bench_function("call1_one_arg", |b| {
        b.iter(|| interp.eval(&call1_module));
    });

    group.bench_function("call2_two_args", |b| {
        b.iter(|| interp.eval(&call2_module));
    });

    group.bench_function("call3_three_args", |b| {
        b.iter(|| interp.eval(&call3_module));
    });

    group.bench_function("call4_four_args", |b| {
        b.iter(|| interp.eval(&call4_module));
    });

    group.bench_function("call5_five_args", |b| {
        b.iter(|| interp.eval(&call5_module));
    });

    group.bench_function("call6_six_args", |b| {
        b.iter(|| interp.eval(&call6_module));
    });

    group.bench_function("nested_calls", |b| {
        b.iter(|| interp.eval(&nested_module));
    });

    group.finish();
}

// Benchmark for binary search algorithm
fn binary_search_benchmark(c: &mut Criterion) {
    let mut group = c.benchmark_group("Binary Search");

    let setup_code = r#"
        // binary_search(a[], a) -> int
        fn binary_search(list, target) { binary_search(list, target, 0, len(list) - 1) }
        // binary_search(a[], a, int, int) -> int
        fn binary_search(_, _, low, high) if low > high; { -1 }
        // binary_search(a[], a, int, int) -> int
        fn binary_search(list, target, low, high) {
            let mid = math::floor((low + high) / 2);
            let midValue = list[mid];

            if midValue == target; {
                mid
            } else if midValue < target; {
                rec binary_search(list, target, mid + 1, high)
            } else {
                rec binary_search(list, target, low, mid - 1)
            }
        }

        // Create sorted lists of different sizes
        let small_list = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];
        let medium_list = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99, 100];
        let large_list = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 123, 124, 125, 126, 127, 128, 129, 130, 131, 132, 133, 134, 135, 136, 137, 138, 139, 140, 141, 142, 143, 144, 145, 146, 147, 148, 149, 150, 151, 152, 153, 154, 155, 156, 157, 158, 159, 160, 161, 162, 163, 164, 165, 166, 167, 168, 169, 170, 171, 172, 173, 174, 175, 176, 177, 178, 179, 180, 181, 182, 183, 184, 185, 186, 187, 188, 189, 190, 191, 192, 193, 194, 195, 196, 197, 198, 199, 200, 201, 202, 203, 204, 205, 206, 207, 208, 209, 210, 211, 212, 213, 214, 215, 216, 217, 218, 219, 220, 221, 222, 223, 224, 225, 226, 227, 228, 229, 230, 231, 232, 233, 234, 235, 236, 237, 238, 239, 240, 241, 242, 243, 244, 245, 246, 247, 248, 249, 250, 251, 252, 253, 254, 255, 256, 257, 258, 259, 260, 261, 262, 263, 264, 265, 266, 267, 268, 269, 270, 271, 272, 273, 274, 275, 276, 277, 278, 279, 280, 281, 282, 283, 284, 285, 286, 287, 288, 289, 290, 291, 292, 293, 294, 295, 296, 297, 298, 299, 300, 301, 302, 303, 304, 305, 306, 307, 308, 309, 310, 311, 312, 313, 314, 315, 316, 317, 318, 319, 320, 321, 322, 323, 324, 325, 326, 327, 328, 329, 330, 331, 332, 333, 334, 335, 336, 337, 338, 339, 340, 341, 342, 343, 344, 345, 346, 347, 348, 349, 350, 351, 352, 353, 354, 355, 356, 357, 358, 359, 360, 361, 362, 363, 364, 365, 366, 367, 368, 369, 370, 371, 372, 373, 374, 375, 376, 377, 378, 379, 380, 381, 382, 383, 384, 385, 386, 387, 388, 389, 390, 391, 392, 393, 394, 395, 396, 397, 398, 399, 400, 401, 402, 403, 404, 405, 406, 407, 408, 409, 410, 411, 412, 413, 414, 415, 416, 417, 418, 419, 420, 421, 422, 423, 424, 425, 426, 427, 428, 429, 430, 431, 432, 433, 434, 435, 436, 437, 438, 439, 440, 441, 442, 443, 444, 445, 446, 447, 448, 449, 450, 451, 452, 453, 454, 455, 456, 457, 458, 459, 460, 461, 462, 463, 464, 465, 466, 467, 468, 469, 470, 471, 472, 473, 474, 475, 476, 477, 478, 479, 480, 481, 482, 483, 484, 485, 486, 487, 488, 489, 490, 491, 492, 493, 494, 495, 496, 497, 498, 499, 500, 501, 502, 503, 504, 505, 506, 507, 508, 509, 510, 511, 512, 513, 514, 515, 516, 517, 518, 519, 520, 521, 522, 523, 524, 525, 526, 527, 528, 529, 530, 531, 532, 533, 534, 535, 536, 537, 538, 539, 540, 541, 542, 543, 544, 545, 546, 547, 548, 549, 550, 551, 552, 553, 554, 555, 556, 557, 558, 559, 560, 561, 562, 563, 564, 565, 566, 567, 568, 569, 570, 571, 572, 573, 574, 575, 576, 577, 578, 579, 580, 581, 582, 583, 584, 585, 586, 587, 588, 589, 590, 591, 592, 593, 594, 595, 596, 597, 598, 599, 600, 601, 602, 603, 604, 605, 606, 607, 608, 609, 610, 611, 612, 613, 614, 615, 616, 617, 618, 619, 620, 621, 622, 623, 624, 625, 626, 627, 628, 629, 630, 631, 632, 633, 634, 635, 636, 637, 638, 639, 640, 641, 642, 643, 644, 645, 646, 647, 648, 649, 650, 651, 652, 653, 654, 655, 656, 657, 658, 659, 660, 661, 662, 663, 664, 665, 666, 667, 668, 669, 670, 671, 672, 673, 674, 675, 676, 677, 678, 679, 680, 681, 682, 683, 684, 685, 686, 687, 688, 689, 690, 691, 692, 693, 694, 695, 696, 697, 698, 699, 700, 701, 702, 703, 704, 705, 706, 707, 708, 709, 710, 711, 712, 713, 714, 715, 716, 717, 718, 719, 720, 721, 722, 723, 724, 725, 726, 727, 728, 729, 730, 731, 732, 733, 734, 735, 736, 737, 738, 739, 740, 741, 742, 743, 744, 745, 746, 747, 748, 749, 750, 751, 752, 753, 754, 755, 756, 757, 758, 759, 760, 761, 762, 763, 764, 765, 766, 767, 768, 769, 770, 771, 772, 773, 774, 775, 776, 777, 778, 779, 780, 781, 782, 783, 784, 785, 786, 787, 788, 789, 790, 791, 792, 793, 794, 795, 796, 797, 798, 799, 800, 801, 802, 803, 804, 805, 806, 807, 808, 809, 810, 811, 812, 813, 814, 815, 816, 817, 818, 819, 820, 821, 822, 823, 824, 825, 826, 827, 828, 829, 830, 831, 832, 833, 834, 835, 836, 837, 838, 839, 840, 841, 842, 843, 844, 845, 846, 847, 848, 849, 850, 851, 852, 853, 854, 855, 856, 857, 858, 859, 860, 861, 862, 863, 864, 865, 866, 867, 868, 869, 870, 871, 872, 873, 874, 875, 876, 877, 878, 879, 880, 881, 882, 883, 884, 885, 886, 887, 888, 889, 890, 891, 892, 893, 894, 895, 896, 897, 898, 899, 900, 901, 902, 903, 904, 905, 906, 907, 908, 909, 910, 911, 912, 913, 914, 915, 916, 917, 918, 919, 920, 921, 922, 923, 924, 925, 926, 927, 928, 929, 930, 931, 932, 933, 934, 935, 936, 937, 938, 939, 940, 941, 942, 943, 944, 945, 946, 947, 948, 949, 950, 951, 952, 953, 954, 955, 956, 957, 958, 959, 960, 961, 962, 963, 964, 965, 966, 967, 968, 969, 970, 971, 972, 973, 974, 975, 976, 977, 978, 979, 980, 981, 982, 983, 984, 985, 986, 987, 988, 989, 990, 991, 992, 993, 994, 995, 996, 997, 998, 999, 1000];
    "#;

    let small_best_code = r#"
        binary_search(small_list, 5);
    "#;

    let small_worst_code = r#"
        binary_search(small_list, 11);
    "#;

    let medium_best_code = r#"
        binary_search(medium_list, 50);
    "#;

    let medium_worst_code = r#"
        binary_search(medium_list, 101);
    "#;

    let large_code = r#"
        binary_search(large_list, 750);
    "#;

    // Compile the initial module with function definitions and test data
    let (mut interp, module) = compile_interpreter(setup_code);
    // Evaluate the initial module to define the functions and create the test lists
    interp.eval(&module);

    // Compile the test modules
    let small_best_module = interp.compile(small_best_code);
    let small_worst_module = interp.compile(small_worst_code);
    let medium_best_module = interp.compile(medium_best_code);
    let medium_worst_module = interp.compile(medium_worst_code);
    let large_module = interp.compile(large_code);

    group.bench_function("small_list_best_case", |b| {
        b.iter(|| interp.eval(&small_best_module));
    });

    group.bench_function("small_list_worst_case", |b| {
        b.iter(|| interp.eval(&small_worst_module));
    });

    group.bench_function("medium_list_best_case", |b| {
        b.iter(|| interp.eval(&medium_best_module));
    });

    group.bench_function("medium_list_worst_case", |b| {
        b.iter(|| interp.eval(&medium_worst_module));
    });

    group.bench_function("large_list_search", |b| {
        b.iter(|| interp.eval(&large_module));
    });

    group.finish();
}

// Benchmark for quicksort algorithm
fn quicksort_benchmark(c: &mut Criterion) {
    let mut group = c.benchmark_group("Quicksort");

    let setup_code = r#"
        // filter(a[], fn(a) -> bool) -> a[]
        fn filter([], _) { [] }
        fn filter([x, ...xs], f) if f(x) { [x, ...filter(xs, f)] }
        fn filter([_, ...xs], f) { rec filter(xs, f) }

        // deterministic quicksort for benchmarking (uses middle element as pivot)
        fn quicksort([]) { [] }
        fn quicksort([x]) { [x] }
        fn quicksort(list) {
            let pivotIndex = math::floor(len(list) / 2);
            let pivot = list[pivotIndex];
            let list = [...list.slice(0, pivotIndex), ...list.slice(pivotIndex+1)];
            let smaller = filter(list, fn(y) { y <= pivot });
            let greater = filter(list, fn(y) { y > pivot });
            [...quicksort(smaller), pivot, ...quicksort(greater)]
        }

        // Create lists to sort
        let sorted_small = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];
        let reverse_small = [10, 9, 8, 7, 6, 5, 4, 3, 2, 1];
        let random_small = [5, 8, 1, 3, 9, 4, 7, 2, 6, 10];
        let random_medium = [42, 17, 89, 36, 91, 53, 28, 76, 15, 62, 39, 84, 27, 63, 71, 
                            48, 19, 95, 33, 60, 8, 74, 51, 22, 68, 44, 56, 81, 12, 30];
    "#;

    let sorted_code = r#"
        quicksort(sorted_small);
    "#;

    let reverse_code = r#"
        quicksort(reverse_small);
    "#;

    let random_small_code = r#"
        quicksort(random_small);
    "#;

    let random_medium_code = r#"
        quicksort(random_medium);
    "#;

    // Compile the initial module with function definitions and test data
    let (mut interp, module) = compile_interpreter(setup_code);
    // Evaluate the initial module to define the functions and create the test lists
    interp.eval(&module);

    // Compile the test modules
    let sorted_module = interp.compile(sorted_code);
    let reverse_module = interp.compile(reverse_code);
    let random_small_module = interp.compile(random_small_code);
    let random_medium_module = interp.compile(random_medium_code);

    group.bench_function("sorted_list", |b| {
        b.iter(|| interp.eval(&sorted_module));
    });

    group.bench_function("reverse_list", |b| {
        b.iter(|| interp.eval(&reverse_module));
    });

    group.bench_function("random_small_list", |b| {
        b.iter(|| interp.eval(&random_small_module));
    });

    group.bench_function("random_medium_list", |b| {
        b.iter(|| interp.eval(&random_medium_module));
    });

    group.finish();
}

// Benchmark for tree traversal operations
fn tree_traversal_benchmark(c: &mut Criterion) {
    let mut group = c.benchmark_group("Tree Traversal");

    let setup_code = r#"
        // Define a binary tree structure using enums
        enum Tree {
            Empty,
            Node { value, left, right },
        }

        fn Tree::leaf(value) {
            Tree::Node {
                value: value,
                left: Option::None,
                right: Option::None,
            }
        }

        fn Tree::node(value, left, right) {
            Tree::Node {
                value: value,
                left: left,
                right: right,
            }
        }

        // In-order traversal of a binary tree
        fn inorder(Option::None) { [] }
        fn inorder(Tree::Node { value, left, right }) {
            [...inorder(left), value, ...inorder(right)]
        }

        // Pre-order traversal of a binary tree
        fn preorder(Option::None) { [] }
        fn preorder(Tree::Node { value, left, right }) {
            [value, ...preorder(left), ...preorder(right)]
        }

        // Post-order traversal of a binary tree
        fn postorder(Option::None) { [] }
        fn postorder(Tree::Node { value, left, right }) {
            [...postorder(left), ...postorder(right), value]
        }

        // Calculate the maximum depth of a tree
        fn max_depth(Option::None) { 0 }
        fn max_depth(Tree::Node { value, left, right }) {
            1 + math::max(max_depth(left), max_depth(right))
        }

        // Count the number of nodes in a tree
        fn count_nodes(Option::None) { 0 }
        fn count_nodes(Tree::Node { value, left, right }) {
            1 + count_nodes(left) + count_nodes(right)
        }

        // Create a balanced tree for testing
        let leaf1 = Tree::leaf(1);
        let leaf2 = Tree::leaf(3);
        let leaf3 = Tree::leaf(5);
        let leaf4 = Tree::leaf(7);
        
        let node1 = Tree::node(2, leaf1, leaf2);
        let node2 = Tree::node(6, leaf3, leaf4);
        
        let root = Tree::node(4, node1, node2);

        // Create a deeply nested tree to test performance with deeper recursion
        let deep_leaf = Tree::leaf(1);
        let deep_node1 = Tree::node(2, deep_leaf, Option::None);
        let deep_node2 = Tree::node(3, deep_node1, Option::None);
        let deep_node3 = Tree::node(4, deep_node2, Option::None);
        let deep_node4 = Tree::node(5, deep_node3, Option::None);
        let deep_node5 = Tree::node(6, deep_node4, Option::None);
        let deep_node6 = Tree::node(7, deep_node5, Option::None);
        let deep_root = Tree::node(8, deep_node6, Option::None);
    "#;

    let inorder_code = r#"
        inorder(root);
    "#;

    let preorder_code = r#"
        preorder(root);
    "#;

    let postorder_code = r#"
        postorder(root);
    "#;
    let max_depth_code = r#"
        max_depth(root);
    "#;

    let count_nodes_code = r#"
        count_nodes(root);
    "#;

    let deep_traversal_code = r#"
        inorder(deep_root);
    "#;

    // Compile the initial module with function definitions and test data
    let (mut interp, module) = compile_interpreter(setup_code);
    // Evaluate the initial module to define the functions and create the test trees
    interp.eval(&module);

    // Compile the test modules
    let inorder_module = interp.compile(inorder_code);
    let preorder_module = interp.compile(preorder_code);
    let postorder_module = interp.compile(postorder_code);
    let max_depth_module = interp.compile(max_depth_code);
    let count_nodes_module = interp.compile(count_nodes_code);
    let deep_traversal_module = interp.compile(deep_traversal_code);

    group.bench_function("inorder_traversal", |b| {
        b.iter(|| interp.eval(&inorder_module));
    });

    group.bench_function("preorder_traversal", |b| {
        b.iter(|| interp.eval(&preorder_module));
    });

    group.bench_function("postorder_traversal", |b| {
        b.iter(|| interp.eval(&postorder_module));
    });

    group.bench_function("max_depth_calculation", |b| {
        b.iter(|| interp.eval(&max_depth_module));
    });

    group.bench_function("count_nodes", |b| {
        b.iter(|| interp.eval(&count_nodes_module));
    });

    group.bench_function("deep_tree_traversal", |b| {
        b.iter(|| interp.eval(&deep_traversal_module));
    });

    group.finish();
}

// Benchmark for constant folding and propagation
fn constant_optimization_benchmark(c: &mut Criterion) {
    let mut group = c.benchmark_group("Constant Optimization");

    let complex_constants = r#"
        fn complex_constants() {
            let a = 1 + 2 * 3 - 4 / 2;
            let b = (a + 5) * 2;
            let c = b - a * 3;
            let d = c + 10;
            let e = d * 2 - b;
            let f = e / 5 + a;
            let g = f * 3 - c;
            let h = g + d - e;
            let i = h * 2 / f;
            let j = i + g - h;
            j
        }
    "#;

    let chained_constants = r#"
        fn chained_constants() {
            let x = 1;
            let y = x + 2;
            let z = y * 3;
            let a = z - 4;
            let b = a / 5;
            let c = b + 6;
            let d = c * 7;
            let e = d - 8;
            let f = e / 9;
            f
        }
    "#;

    let dead_code = r#"
        fn dead_code() {
            let unused1 = 1 + 2;
            let unused2 = 3 * 4;
            let unused3 = 5 - 6;
            let result = 42;
            let unused4 = result * 2;
            result
        }
    "#;

    // Compile the test cases
    let (mut interp_complex, module_complex) = compile_interpreter(complex_constants);
    let (mut interp_chained, module_chained) = compile_interpreter(chained_constants);
    let (mut interp_dead, module_dead) = compile_interpreter(dead_code);

    // Evaluate the initial modules to define the functions
    interp_complex.eval(&module_complex);
    interp_chained.eval(&module_chained);
    interp_dead.eval(&module_dead);

    // Compile the test modules
    let complex_test = interp_complex.compile("complex_constants();");
    let chained_test = interp_chained.compile("chained_constants();");
    let dead_test = interp_dead.compile("dead_code();");

    group.bench_function("complex_constants", |b| {
        b.iter(|| interp_complex.eval(&complex_test));
    });

    group.bench_function("chained_constants", |b| {
        b.iter(|| interp_chained.eval(&chained_test));
    });

    group.bench_function("dead_code", |b| {
        b.iter(|| interp_dead.eval(&dead_test));
    });

    group.finish();
}

// Benchmark for mixed constant and variable operations
fn mixed_operations_benchmark(c: &mut Criterion) {
    let mut group = c.benchmark_group("Mixed Operations");

    let mixed_code = r#"
        fn calculate(x) {
            let a = x + 1;
            let b = a * 2;
            let c = b - 3;
            let d = c / 4;
            let e = d + 5;
            let f = e * 6;
            let g = f - 7;
            let h = g / 8;
            h
        }
    "#;

    let test_code = r#"
        calculate(10);
    "#;

    // Compile the initial module with function definitions
    let (mut interp, module) = compile_interpreter(mixed_code);
    // Evaluate the initial module to define the functions
    interp.eval(&module);

    // Compile the test module
    let test_module = interp.compile(test_code);

    group.bench_function("mixed_operations", |b| {
        b.iter(|| interp.eval(&test_module));
    });

    group.finish();
}

criterion_group!(
    benches,
    fibonacci_benchmark,
    closure_benchmark,
    list_benchmark,
    struct_benchmark,
    small_arg_call_benchmark,
    binary_search_benchmark,
    quicksort_benchmark,
    tree_traversal_benchmark,
    constant_optimization_benchmark,
    mixed_operations_benchmark,
);
criterion_main!(benches);
