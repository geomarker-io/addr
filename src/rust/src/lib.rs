use extendr_api::prelude::*;

/// run usaddress::parse on a vector of strings
#[extendr]
fn usaddress_tag(x: Vec<String>) -> Robj {
    let ta: Vec<_> = x
        .iter()
        .map(|x| usaddress::parse(x).unwrap())
        .map(|pairs| {
            let (tokens, tags): (Vec<String>, Vec<String>) =
                pairs.into_iter().map(|(token, tag)| (token, tag)).unzip();
            let mut v = Strings::from_values(tokens);
            v.set_names(tags).unwrap();
            v.into_robj()
        })
        .collect();
    return r!(List::from_values(ta));
}

// Macro to generate exports.
// This ensures exported functions are registered with R.
// See corresponding C code in `entrypoint.c`.
extendr_module! {
    mod addr;
    fn usaddress_tag;
}
