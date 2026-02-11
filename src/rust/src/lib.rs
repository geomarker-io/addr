use extendr_api::prelude::*;
use std::sync::Once;

static WARN_ONCE: Once = Once::new();

fn warn_parse_failure_once() {
    WARN_ONCE.call_once(|| {
        let _ = call!(
            "warning",
            r!("usaddress::parse failed for at least one input; returning empty addr components for those entries.")
        );
    });
}

fn empty_tagged_tokens() -> Robj {
    let v = Strings::from_values(Vec::<String>::new());
    v.into_robj()
}

/// Run usaddress::parse in Rust on a character vector in R
///
/// Use `tag_usaddress()` instead of this function directly.
#[extendr]
fn usaddress_tag(x: Vec<String>) -> Robj {
    let ta: Vec<_> = x
        .iter()
        .map(|x| match usaddress::parse(x) {
            Ok(pairs) => {
                let (tokens, tags): (Vec<String>, Vec<String>) =
                    pairs.into_iter().map(|(token, tag)| (token, tag)).unzip();
                let mut v = Strings::from_values(tokens);
                if v.set_names(tags).is_err() {
                    warn_parse_failure_once();
                    return empty_tagged_tokens();
                }
                v.into_robj()
            }
            Err(_) => {
                warn_parse_failure_once();
                empty_tagged_tokens()
            }
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
