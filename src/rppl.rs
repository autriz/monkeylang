use std::io::Write;

use crate::{lexer::Lexer, parser::Parser};

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
                let mut parser = Parser::new(&mut lexer).unwrap();

                let program = parser.parse();

                if parser.errors.len() > 0 {
                    let error_string = parser.errors.iter()
                        .map(|x| x.to_string())
                        .collect::<Vec<String>>()
                        .join("\n");

                    println!("{}", error_string);

                    continue;
                }

				println!("{}", program);
			}
		}
	}
}