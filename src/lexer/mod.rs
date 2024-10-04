mod error;
pub use error::{Error, Result};

use std::fmt::Display;

use crate::token::Token;

#[derive(Debug)]
pub struct Lexer {
	position: usize,
	read_position: usize,
	ch: u8,
	input: Vec<u8>,
}

impl Display for Lexer {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "Lexer {{\n\tposition: {},\n\tread_position: {},\n\tch: {},\n}}", self.position, self.read_position, String::from_utf8_lossy(&[self.ch]).to_string())
	}
}

impl Lexer {
	pub fn new(input: String) -> Self {
		let mut lexer = Self {
			position: 0,
			read_position: 0,
			ch: 0,
			input: input.into_bytes(),
		};

		lexer.read_char();

		return lexer;
	}

	pub fn next_token(&mut self) -> Result<Token> {
		self.skip_whitespace();

		let token = match self.ch {
			b'{' => Token::LBrace,
			b'}' => Token::RBrace,
			b'(' => Token::LParen,
			b')' => Token::RParen,
			b',' => Token::Comma,
			b';' => Token::Semicolon,
			b'+' => Token::Plus,
			b'-' => Token::Dash,
			b'!' => {
				if self.peek() == b'=' {
					self.read_char();
					Token::NotEqual
				} else {
					Token::Bang
				}
			},
			b'>' => {
				if self.peek() == b'=' {
					self.read_char();
					Token::GreaterThanOrEqual
				} else {
					Token::GreaterThan
				}
			},
			b'<' => {
				if self.peek() == b'=' {
					self.read_char();
					Token::LessThanOrEqual
				} else {
					Token::LessThan
				}
			},
			b'*' => Token::Asterisk,
			b'/' => Token::ForwardSlash,
			b'=' => {
				if self.peek() == b'=' {
					self.read_char();
					Token::Equal
				} else {
					Token::Assign
				}
			},
			b'"' => {
				self.read_char();

				Token::String(self.read_string())
			},
			b'a'..=b'z' | b'A'..=b'Z' | b'_' => {
				let ident = self.read_ident();

				return Ok(match ident.as_str() {
					"let" => Token::Let,
					"fn" => Token::Function,
					"return" => Token::Return,
					"if" => Token::If,
					"else" => Token::Else,
					"true" => Token::True,
					"false" => Token::False,
					_ => Token::Ident(ident),
				});
			},
			b'0'..=b'9' => {
				return Ok(self.read_digit());
			},
			0 => Token::Eof,
			_ => Token::Illegal,
		};

		self.read_char();

		return Ok(token);
	}

	fn peek(&self) -> u8 {
		if self.read_position >= self.input.len() {
			return 0;
		} else {
			return self.input[self.read_position];
		}
	}

	fn read_char(&mut self) {
		if self.read_position >= self.input.len() {
			self.ch = 0;
		} else {
			self.ch = self.input[self.read_position];
		}

		self.position = self.read_position;
		self.read_position += 1;
	}

	fn skip_whitespace(&mut self) {
		while self.ch.is_ascii_whitespace() {
			self.read_char();
		}
	}

	fn read_ident(&mut self) -> String {
		let pos = self.position;
		
		while self.ch.is_ascii_alphanumeric() || self.ch == b'_' {
			self.read_char();
		}

		return String::from_utf8_lossy(&self.input[pos..self.position]).to_string();
	}

	fn read_string(&mut self) -> String {
		let pos = self.position;

		while self.ch != b'"' {
			self.read_char();
		}

		return String::from_utf8_lossy(&self.input[pos..self.position]).to_string();
	}

	fn read_digit(&mut self) -> Token {
		if self.ch == b'0' && self.peek() == b'x' {
			return Token::Hex(self.read_hex());
		}
		else {
			return self.read_int_or_float();
		}
	}

	fn read_int_or_float(&mut self) -> Token {
		let pos = self.position;

		let mut is_float = false;

		while self.ch.is_ascii_digit() || self.ch == b'.' || self.ch == b'E' || self.ch == b'e' || self.ch == b'-' || self.ch == b'+' {
			match self.ch {
				b'.' | b'E' | b'e' => {
					if !is_float { is_float = true; }
				},
				_ => {}
			}
			self.read_char();
		}

		let val = String::from_utf8_lossy(&self.input[pos..self.position]).to_string();

		if is_float {
			Token::Float(val)
		} else {
			Token::Int(val)
		}
	}

	fn read_hex(&mut self) -> String {
		let pos = self.position;

		while self.ch.is_ascii_hexdigit() || self.ch == b'x' {
			self.read_char();
		}

		return String::from_utf8_lossy(&self.input[pos..self.position]).to_string();
	}
}

#[cfg(test)]
mod test {
	use crate::token::Token;
	use super::{Lexer, Result};

	#[test]
	fn test_tokens() -> Result<()> {
		let input = r#"=+(){},;"#;

		let tokens = [
			Token::Assign,
			Token::Plus,
			Token::LParen,
			Token::RParen,
			Token::LBrace,
			Token::RBrace,
			Token::Comma,
			Token::Semicolon,
			Token::Eof,
		];

		let mut lexer = Lexer::new(input.to_string());

		for token in tokens {
			let next_token = lexer.next_token()?;

			assert_eq!(token, next_token, "Next token does not match expected token ({}, {})", next_token, token);
		}

		Ok(())
	}

	#[test]
	fn test_example() -> Result<()> {
		let input = r#"let five = 5;
			let ten = 10;

			let add = fn(x, y) {
				return x + y;
			};

			let result = add(five, ten);

			!-/*5;
			5 < 10 > 5;
			1e-9;
			10.54;
			0xA3;

			if (5 < 10) {
				return true;
			} else {
				return false;
			}

			10 == 10;
			10 != 9;
			"foobar"
			"foo bar"
		"#;

		let mut lexer = Lexer::new(input.to_string());

		let tokens = [
			Token::Let,
			Token::Ident(String::from("five")),
			Token::Assign,
			Token::Int(String::from("5")),
			Token::Semicolon,
			Token::Let,
			Token::Ident(String::from("ten")),
			Token::Assign,
			Token::Int(String::from("10")),
			Token::Semicolon,
			Token::Let,
			Token::Ident(String::from("add")),
			Token::Assign,
			Token::Function,
			Token::LParen,
			Token::Ident(String::from("x")),
			Token::Comma,
			Token::Ident(String::from("y")),
			Token::RParen,
			Token::LBrace,
			Token::Return,
			Token::Ident(String::from("x")),
			Token::Plus,
			Token::Ident(String::from("y")),
			Token::Semicolon,
			Token::RBrace,
			Token::Semicolon,
			Token::Let,
			Token::Ident(String::from("result")),
			Token::Assign,
			Token::Ident(String::from("add")),
			Token::LParen,
			Token::Ident(String::from("five")),
			Token::Comma,
			Token::Ident(String::from("ten")),
			Token::RParen,
			Token::Semicolon,
			Token::Bang,
			Token::Dash,
			Token::ForwardSlash,
			Token::Asterisk,
			Token::Int(String::from("5")),
			Token::Semicolon,
			Token::Int(String::from("5")),
			Token::LessThan,
			Token::Int(String::from("10")),
			Token::GreaterThan,
			Token::Int(String::from("5")),
			Token::Semicolon,
			Token::Float(String::from("1e-9")),
			Token::Semicolon,
			Token::Float(String::from("10.54")),
			Token::Semicolon,
			Token::Hex(String::from("0xA3")),
			Token::Semicolon,
			Token::If,
			Token::LParen,
			Token::Int(String::from("5")),
			Token::LessThan,
			Token::Int(String::from("10")),
			Token::RParen,
			Token::LBrace,
			Token::Return,
			Token::True,
			Token::Semicolon,
			Token::RBrace,
			Token::Else,
			Token::LBrace,
			Token::Return,
			Token::False,
			Token::Semicolon,
			Token::RBrace,
			Token::Int(String::from("10")),
			Token::Equal,
			Token::Int(String::from("10")),
			Token::Semicolon,
			Token::Int(String::from("10")),
			Token::NotEqual,
			Token::Int(String::from("9")),
			Token::Semicolon,
			Token::String(String::from("foobar")),
			Token::String(String::from("foo bar")),
			Token::Eof,
		];

		for token in tokens {
			let next_token = lexer.next_token()?;

			// println!("next: {}, expected: {}", next_token, token);

			assert_eq!(token, next_token, "Next token does not match expected token ({}, {})", next_token, token);
		}

		Ok(())
	}
}