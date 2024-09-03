use std::io::Write;

use crate::{lexer::Lexer, token::Token};

const PROMPT: &str = ">> ";

pub fn start() -> std::io::Result<()> {
	let stdin = std::io::stdin();
	
	loop {
		let mut input = String::from("");

		print!("{}", PROMPT);
		std::io::stdout().flush()?;
		stdin.read_line(&mut input)?;

		if let Some('\n') = input.chars().next_back() {
			input.pop();
		}
		if let Some('\r') = input.chars().next_back() {
			input.pop();
		}

		match input.as_str() {
			"" => {},
			".exit" => return Ok(()),
			_ => {
				let mut lexer = Lexer::new(input);

				while let Ok(token) = lexer.next_token() {
					println!("{}", token);

					if token == Token::Eof {
						break;
					}
				}
			}
		}
	}
}