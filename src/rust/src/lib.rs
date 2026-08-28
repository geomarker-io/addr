use extendr_api::prelude::*;
use extendr_api::Result;
use flate2::read::DeflateDecoder;
use std::collections::HashMap;
use std::fs::File;
use std::io::{Read, Take};
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
/// @keywords internal
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

fn nad_error(message: impl Into<String>) -> Error {
    Error::Other(message.into())
}

fn read_u16_le(bytes: &[u8]) -> u16 {
    u16::from_le_bytes([bytes[0], bytes[1]])
}

fn read_u32_le(bytes: &[u8]) -> u32 {
    u32::from_le_bytes([bytes[0], bytes[1], bytes[2], bytes[3]])
}

fn read_u64_le(bytes: &[u8]) -> u64 {
    u64::from_le_bytes([
        bytes[0], bytes[1], bytes[2], bytes[3], bytes[4], bytes[5], bytes[6], bytes[7],
    ])
}

fn zip64_sizes(
    extra: &[u8],
    needs_uncompressed: bool,
    needs_compressed: bool,
) -> Result<(u64, u64)> {
    let mut position = 0usize;
    while position + 4 <= extra.len() {
        let field_id = read_u16_le(&extra[position..position + 2]);
        let field_size = read_u16_le(&extra[position + 2..position + 4]) as usize;
        position += 4;
        if position + field_size > extra.len() {
            return Err(nad_error("invalid ZIP extra-field length"));
        }
        if field_id == 0x0001 {
            let field = &extra[position..position + field_size];
            let mut field_position = 0usize;
            let uncompressed = if needs_uncompressed {
                if field_position + 8 > field.len() {
                    return Err(nad_error("missing ZIP64 uncompressed size"));
                }
                let value = read_u64_le(&field[field_position..field_position + 8]);
                field_position += 8;
                value
            } else {
                0
            };
            let compressed = if needs_compressed {
                if field_position + 8 > field.len() {
                    return Err(nad_error("missing ZIP64 compressed size"));
                }
                read_u64_le(&field[field_position..field_position + 8])
            } else {
                0
            };
            return Ok((uncompressed, compressed));
        }
        position += field_size;
    }
    Err(nad_error("missing ZIP64 size metadata"))
}

fn nad_zip_member(path: &str, expected_member: &str) -> Result<DeflateDecoder<Take<File>>> {
    let mut source = File::open(path)
        .map_err(|error| nad_error(format!("could not open NAD source: {error}")))?;
    let mut header = [0u8; 30];
    source
        .read_exact(&mut header)
        .map_err(|error| nad_error(format!("could not read NAD ZIP header: {error}")))?;
    if &header[0..4] != b"PK\x03\x04" {
        return Err(nad_error(
            "NAD source does not begin with a ZIP local-file header",
        ));
    }
    let flags = read_u16_le(&header[6..8]);
    if flags & 0x0001 != 0 {
        return Err(nad_error("encrypted NAD ZIP members are not supported"));
    }
    if flags & 0x0008 != 0 {
        return Err(nad_error(
            "NAD ZIP members with data descriptors are not supported",
        ));
    }
    let compression_method = read_u16_le(&header[8..10]);
    if compression_method != 8 {
        return Err(nad_error(format!(
            "unsupported NAD ZIP compression method: {compression_method}"
        )));
    }
    let compressed_32 = read_u32_le(&header[18..22]);
    let uncompressed_32 = read_u32_le(&header[22..26]);
    let filename_size = read_u16_le(&header[26..28]) as usize;
    let extra_size = read_u16_le(&header[28..30]) as usize;
    let mut filename = vec![0u8; filename_size];
    source
        .read_exact(&mut filename)
        .map_err(|error| nad_error(format!("could not read NAD ZIP member name: {error}")))?;
    if filename != expected_member.as_bytes() {
        return Err(nad_error(format!(
            "expected first NAD ZIP member `{expected_member}`, found `{}`",
            String::from_utf8_lossy(&filename)
        )));
    }
    let mut extra = vec![0u8; extra_size];
    source
        .read_exact(&mut extra)
        .map_err(|error| nad_error(format!("could not read NAD ZIP metadata: {error}")))?;
    let needs_uncompressed = uncompressed_32 == u32::MAX;
    let needs_compressed = compressed_32 == u32::MAX;
    let (zip64_uncompressed, zip64_compressed) = if needs_uncompressed || needs_compressed {
        zip64_sizes(&extra, needs_uncompressed, needs_compressed)?
    } else {
        (0, 0)
    };
    let compressed_size = if needs_compressed {
        zip64_compressed
    } else {
        compressed_32 as u64
    };
    let uncompressed_size = if needs_uncompressed {
        zip64_uncompressed
    } else {
        uncompressed_32 as u64
    };
    if compressed_size == 0 || uncompressed_size == 0 {
        return Err(nad_error("NAD ZIP member has an invalid zero size"));
    }
    Ok(DeflateDecoder::new(source.take(compressed_size)))
}

fn strings_with_na(values: Vec<Option<String>>) -> Strings {
    let mut output = Strings::new(values.len());
    for (index, value) in values.into_iter().enumerate() {
        output.set_elt(index, Rstr::from(value));
    }
    output
}

/// Stream one county from the compressed NAD flat source.
///
/// Use `nad_read()` instead of this function directly.
/// @keywords internal
#[extendr]
fn nad_flat_extract(
    path: &str,
    member: &str,
    state: &str,
    county: &str,
    fields: Vec<String>,
) -> List {
    match nad_flat_extract_impl(path, member, state, county, fields) {
        Ok(output) => output,
        Err(error) => throw_r_error(error.to_string()),
    }
}

fn nad_flat_extract_impl(
    path: &str,
    member: &str,
    state: &str,
    county: &str,
    fields: Vec<String>,
) -> Result<List> {
    let source = nad_zip_member(path, member)?;
    let mut reader = csv::ReaderBuilder::new()
        .has_headers(true)
        .from_reader(source);
    let headers = reader
        .headers()
        .map_err(|error| nad_error(format!("could not read NAD CSV header: {error}")))?
        .clone();
    let field_positions: Vec<usize> = fields
        .iter()
        .map(|field| {
            headers
                .iter()
                .position(|header| header == field)
                .ok_or_else(|| nad_error(format!("NAD CSV is missing field `{field}`")))
        })
        .collect::<Result<Vec<_>>>()?;
    let state_position = headers
        .iter()
        .position(|header| header == "State")
        .ok_or_else(|| nad_error("NAD CSV is missing field `State`"))?;
    let county_position = headers
        .iter()
        .position(|header| header == "County")
        .ok_or_else(|| nad_error("NAD CSV is missing field `County`"))?;
    let mut columns: Vec<Vec<Option<String>>> = fields.iter().map(|_| Vec::new()).collect();
    for record in reader.records() {
        let record = record
            .map_err(|error| nad_error(format!("could not parse NAD CSV record: {error}")))?;
        if record.get(state_position) != Some(state) || record.get(county_position) != Some(county)
        {
            continue;
        }
        for (column, position) in columns.iter_mut().zip(field_positions.iter()) {
            let value = record
                .get(*position)
                .ok_or_else(|| nad_error("NAD CSV record has fewer fields than its header"))?;
            column.push(if value.is_empty() {
                None
            } else {
                Some(value.to_owned())
            });
        }
    }
    let values: Vec<Robj> = columns
        .into_iter()
        .map(|column| strings_with_na(column).into_robj())
        .collect();
    let mut output = List::from_values(values);
    output.set_names(fields)?;
    Ok(output)
}

/// Inventory state and county labels in the compressed NAD flat source.
///
/// Use `nad_catalog()` instead of this function directly.
/// @keywords internal
#[extendr]
fn nad_flat_catalog(path: &str, member: &str) -> List {
    match nad_flat_catalog_impl(path, member) {
        Ok(output) => output,
        Err(error) => throw_r_error(error.to_string()),
    }
}

fn nad_flat_catalog_impl(path: &str, member: &str) -> Result<List> {
    let source = nad_zip_member(path, member)?;
    let mut reader = csv::ReaderBuilder::new()
        .has_headers(true)
        .from_reader(source);
    let headers = reader
        .headers()
        .map_err(|error| nad_error(format!("could not read NAD CSV header: {error}")))?
        .clone();
    let state_position = headers
        .iter()
        .position(|header| header == "State")
        .ok_or_else(|| nad_error("NAD CSV is missing field `State`"))?;
    let county_position = headers
        .iter()
        .position(|header| header == "County")
        .ok_or_else(|| nad_error("NAD CSV is missing field `County`"))?;
    let mut counts: HashMap<(String, String), u64> = HashMap::new();
    for record in reader.records() {
        let record = record
            .map_err(|error| nad_error(format!("could not parse NAD CSV record: {error}")))?;
        let state = record
            .get(state_position)
            .ok_or_else(|| nad_error("NAD CSV record has no `State` value"))?;
        let county = record
            .get(county_position)
            .ok_or_else(|| nad_error("NAD CSV record has no `County` value"))?;
        if state.is_empty() || county.is_empty() {
            return Err(nad_error(
                "NAD CSV record has an empty `State` or `County` value",
            ));
        }
        let count = counts
            .entry((state.to_owned(), county.to_owned()))
            .or_insert(0);
        *count = count
            .checked_add(1)
            .ok_or_else(|| nad_error("NAD county row count overflowed"))?;
    }
    let mut rows: Vec<((String, String), u64)> = counts.into_iter().collect();
    rows.sort_by(|a, b| a.0.cmp(&b.0));
    let mut states = Vec::with_capacity(rows.len());
    let mut counties = Vec::with_capacity(rows.len());
    let mut source_row_counts = Vec::with_capacity(rows.len());
    for ((state, county), count) in rows {
        states.push(state);
        counties.push(county);
        source_row_counts.push(count as f64);
    }
    let values = vec![
        Strings::from_values(states).into_robj(),
        Strings::from_values(counties).into_robj(),
        source_row_counts.into_robj(),
    ];
    let mut output = List::from_values(values);
    output.set_names(["state", "source_county", "source_row_count"])?;
    Ok(output)
}

// Macro to generate exports.
// This ensures exported functions are registered with R.
// See corresponding C code in `entrypoint.c`.
extendr_module! {
    mod addr;
    fn usaddress_tag;
    fn nad_flat_extract;
    fn nad_flat_catalog;
}
