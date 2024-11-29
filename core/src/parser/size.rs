use std::num::ParseIntError;

#[derive(Debug, PartialEq)]
pub enum ParseSizeError {
    InvalidFormat,
    InvalidSuffix,
    NumberTooLarge,
    ParseError(ParseIntError),
}

pub fn parse_number(input: &str) -> Result<u64, ParseIntError> {
    if let Some(stripped) = input.strip_prefix("0x") {
        u64::from_str_radix(stripped, 16)
    } else {
        input.parse()
    }
}

pub fn parse_size(input: &str) -> Result<u64, ParseSizeError> {
    if input.is_empty() {
        return Err(ParseSizeError::InvalidFormat);
    }

    let (num_part, suffix) = input.trim().split_at(
        input
            .find(|c: char| !c.is_ascii_digit() && c != 'x')
            .unwrap_or(input.len()),
    );

    let num = parse_number(num_part).map_err(ParseSizeError::ParseError)?;

    let multiplier = match suffix.trim() {
        "B" => 1,
        "kB" => 1_024,
        "MB" => 1_024 * 1_024,
        "GB" => 1_024 * 1_024 * 1_024,
        "" => return Err(ParseSizeError::InvalidFormat),
        _ => return Err(ParseSizeError::InvalidSuffix),
    };

    let result = num
        .checked_mul(multiplier as u64)
        .ok_or(ParseSizeError::NumberTooLarge)?;

    if result > u32::MAX as u64 {
        return Err(ParseSizeError::NumberTooLarge);
    }

    Ok(result)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_number_decimal() {
        assert_eq!(parse_number("123"), Ok(123));
        assert_eq!(parse_number("0"), Ok(0));
    }

    #[test]
    fn test_parse_number_hexadecimal() {
        assert_eq!(parse_number("0x10"), Ok(16));
        assert_eq!(parse_number("0x1F4"), Ok(500));
    }

    #[test]
    fn test_parse_number_invalid() {
        assert!(parse_number("not_a_number").is_err());
        assert!(parse_number("0xZZZ").is_err());
    }

    #[test]
    fn test_parse_size_with_hexadecimal() {
        assert_eq!(parse_size("0x100B"), Ok(256));
        assert_eq!(parse_size("0x1kB"), Ok(1_024));
        assert_eq!(parse_size("0x2MB"), Ok(2 * 1_024 * 1_024));
    }

    #[test]
    fn test_valid_sizes() {
        assert_eq!(parse_size("0B"), Ok(0));
        assert_eq!(parse_size("256B"), Ok(256));
        assert_eq!(parse_size("1kB"), Ok(1_024));
        assert_eq!(parse_size("2MB"), Ok(2 * 1_024 * 1_024));
        assert_eq!(parse_size("1GB"), Ok(1_024 * 1_024 * 1_024));
    }

    #[test]
    fn test_invalid_formats() {
        assert_eq!(parse_size(""), Err(ParseSizeError::InvalidFormat));
        assert_eq!(parse_size("256"), Err(ParseSizeError::InvalidFormat));
        assert_eq!(parse_size("256MBExtra"), Err(ParseSizeError::InvalidSuffix));
    }

    #[test]
    fn test_invalid_suffix() {
        assert_eq!(parse_size("256mB"), Err(ParseSizeError::InvalidSuffix));
        assert_eq!(parse_size("256TB"), Err(ParseSizeError::InvalidSuffix));
    }

    #[test]
    fn test_parse_errors() {
        assert!(matches!(
            parse_size("not_a_numberMB"),
            Err(ParseSizeError::ParseError(_))
        ));
        assert!(matches!(
            parse_size("0xnot_hexB"),
            Err(ParseSizeError::ParseError(_))
        ));
    }
}
