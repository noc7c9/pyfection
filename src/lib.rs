use std::error::Error;
use std::fs::File;
use std::path::Path;
use std::io::{BufReader, Read};


// const INLINE_COMMENT: &str = "//";
// const BLOCK_COMMENT_BEGIN: &str = "/*";
// const BLOCK_COMMENT_END: &str = "*/";


#[derive(PartialEq, Debug)]
enum Token {
    Code(String),
    Newline,
    Indent(String), Dedent,
    // BlockComment(String), InlineComment(String),
}


#[derive(Debug)]
struct TokenList {
    tokens: Vec<Token>,
    indent_stack: Vec<String>,
}

#[derive(Debug, Clone, Copy)]
enum TokenizerState {
    LineStart,
    Waiting,
    ReadingCode,
    ReadingIndent,
}

impl TokenList {
    fn new() -> TokenList {
        return TokenList {
            tokens: Vec::new(),
            indent_stack: Vec::new(),
        };
    }

    fn tokenize(&mut self, code: &str) {
        use TokenizerState::*;

        // println!("Tokenize: {:?}", code);
        let mut buf = String::with_capacity(code.len());
        let mut iter = code.chars().peekable();
        let mut state = LineStart;
        loop {
            // { println!("={:?} {:?}", iter.peek(), state); }

            // TODO: this is order dependant, it shouldn't be
            // source = https://stackoverflow.com/a/26927642
            let r = iter.peek().map(|c| *c);
            match (state, r) {
                // stop reading, ie. ReadingCode exit handler
                (ReadingCode, Some('\n')) |
                (ReadingCode, None) => {
                    self.tokens.push(Token::Code(buf.clone()));
                    buf.clear();
                    state = Waiting;
                },

                // handle indentation
                (LineStart, Some(' ')) => {
                    state = ReadingIndent;
                }
                (ReadingIndent, Some(' ')) => {
                    iter.next();
                    buf.push(' ');
                },

                // stop reading indent, ReadingIndent exit handler
                (ReadingIndent, _) => {
                    self.tokens.push(Token::Indent(buf.clone()));
                    self.indent_stack.push(buf.clone());
                    buf.clear();
                    state = Waiting;
                },

                // handle newline
                (LineStart, Some('\n')) |
                (Waiting, Some('\n')) => {
                    iter.next();
                    self.tokens.push(Token::Newline);
                    state = LineStart;
                },

                // handle reading code
                (LineStart, Some(_)) => {
                    // check if the indent stack is empty,
                    // otherwise add the necessary dedents
                    for _ in self.indent_stack.drain(..) {
                        self.tokens.push(Token::Dedent);
                    }
                    state = Waiting;
                },
                (Waiting, Some(_)) => {
                    state = ReadingCode;
                },
                (ReadingCode, Some(ch)) => {
                    iter.next();
                    buf.push(ch);
                },

                // termination
                (LineStart, None) |
                (Waiting, None) => {
                    // add last dedents
                    for _ in 0..self.indent_stack.len() {
                        self.tokens.push(Token::Dedent);
                    }

                    break;
                },
            }
        }
    }
}

fn tokenize(code: &str) -> Vec<Token> {
    let mut token_list = TokenList::new();

    token_list.tokenize(code);

    return token_list.tokens;
}

fn process(code: &str) -> String {
    let tokens = tokenize(code);

    return format!("{:?}", tokens);
}


pub fn process_str(code: &str) -> String {
    return process(code);
}

pub fn process_file(filepath: &str) -> String {
    let path = Path::new(filepath);
    let display = path.display();

    // open file
    let file = match File::open(&path) {
        Err(why) => panic!("couldn't open {}: {}", display,
                                                why.description()),
        Ok(file) => file,
    };
    let mut file = BufReader::new(file);

    // read file
    let mut buf = String::new();
    return match file.read_to_string(&mut buf) {
        Err(why) => panic!("couldn't read {}", why.description()),
        Ok(_) => process(&buf),
    }
}

#[cfg(test)]
mod tests {
    extern crate unindent;
    use self::unindent::unindent;

    use super::*;

    /***
     * Helpers
     */

    macro_rules! tok {
        (code $string : expr) => (Token::Code(String::from($string)));
        (nl) => (Token::Newline);
        (indent $string : expr) => (Token::Indent(String::from($string)));
        (dedent) => (Token::Dedent);
    }

    /***
     * Tokenizer tests
     */

    #[test]
    fn test_tokenize_single_statement_no_newline() {
        let code = "statement;";
        let expected = vec![
            tok!(code "statement;"),
        ];
        let actual = tokenize(code);
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_tokenize_single_statement() {
        let code = "statement;\n";
        let expected = vec![
            tok!(code "statement;"),
            tok!(nl),
        ];
        let actual = tokenize(&code);
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_tokenize_newline() {
        let code = unindent("
            statement;
            statement;
            ");
        let expected = vec![
            tok!(code "statement;"),
            tok!(nl),

            tok!(code "statement;"),
            tok!(nl),
        ];
        let actual = tokenize(&code);
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_tokenize_single_indent_and_dedent() {
        let code = unindent("
            if condition
                something happens;");
        let expected = vec![
            tok!(code "if condition"),
            tok!(nl),

            tok!(indent "    "),
            tok!(code "something happens;"),

            tok!(dedent),
        ];
        let actual = tokenize(&code);
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_tokenize_basic_if_else_structure() {
        let code = unindent("
            if condition
                something happens;
            else
                something else happens;");
        let expected = vec![
            tok!(code "if condition"),
            tok!(nl),

            tok!(indent "    "),
            tok!(code "something happens;"),
            tok!(nl),

            tok!(dedent),
            tok!(code "else"),
            tok!(nl),

            tok!(indent "    "),
            tok!(code "something else happens;"),

            tok!(dedent),
        ];
        let actual = tokenize(&code);
        assert_eq!(expected, actual);
    }

}
