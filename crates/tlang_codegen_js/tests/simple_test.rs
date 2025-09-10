use indoc::indoc;

fn main() {
    // Let's compile this problematic case and see what specific HIR is being generated
    println!("Testing nested for-loop compilation...");

    let result = std::panic::catch_unwind(|| {
        let output = compile!(indoc! {"
            fn matrix_sum() {
                let total = 0;
                for row in [[1, 2], [3, 4]] {
                    for val in row {
                        total = total + val
                    }
                }
                total
            }
        "});

        println!("JavaScript output:");
        println!("{}", output);
    });

    match result {
        Ok(_) => println!("✅ SUCCESS: JavaScript was generated"),
        Err(_) => println!(
            "❌ FAILED: Got the 'Match arm expressions should be transformed to statements' error"
        ),
    }
}

#[macro_use]
mod common;
