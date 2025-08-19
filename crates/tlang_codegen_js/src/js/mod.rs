mod keywords;


pub fn safe_js_variable_name(name: &str) -> String {
    let mut name = name.to_string();

    // Replace / with $$
    if name.contains('/') {
        name = name.replace('/', "$$");
    }

    // We might want to special case `this` in codegeneration, currently
    // we just define `this` when needed.
    if keywords::is_keyword(&name) && name != keywords::kw::This {
        return "$".to_string() + &name;
    }

    name
}
