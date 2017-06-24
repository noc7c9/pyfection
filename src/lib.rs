use std::error::Error;
use std::fs::File;
use std::path::Path;
use std::io::{BufReader, Read};


// const INLINE_COMMENT: &str = "//";
// const BLOCK_COMMENT_BEGIN: &str = "/*";
// const BLOCK_COMMENT_END: &str = "*/";


#[derive(PartialEq, Debug, Clone)]
enum Token {
    Code(String), Whitespace(String),
    Newline,
    Indent(String), Dedent,
    OpenBrace, CloseBrace,
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

            // source: https://stackoverflow.com/a/26927642
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
                    // if there are Indent tokens on the stack
                    // compare with those tokens
                    let mut index = 0;
                    while let Some(ref indent) = self.indent_stack.get(index) {
                        loop {
                            match iter.peek() {
                                Some(&' ') => {
                                    iter.next();
                                    buf.push(' ');
                                    if buf.len() == indent.len() {
                                        self.tokens.push(Token::Whitespace(buf.clone()));
                                        buf.clear();
                                        break; // go to next indent
                                    }
                                },
                                _ => {
                                    // code started before reaching same indent level
                                    state = Waiting;
                                    break;
                                }
                            }
                        }
                        if let Waiting = state {
                            break;
                        }
                        index += 1;
                    }

                    // check all the indent levels were reached
                    if let Waiting = state {
                        // nope, so need to add dedent tokens
                        // and update indent stack
                        for _ in self.indent_stack.drain(index..) {
                            self.tokens.push(Token::Dedent);
                        }
                    } else {
                        // yes, so any remaining whitespace is new indentation
                        while let Some(&' ') = iter.peek() {
                            iter.next();
                            buf.push(' ');
                        }
                    }
                },

                // stop reading indent, ReadingIndent exit handler
                (ReadingIndent, _) => {
                    if buf.len() > 0 {
                        self.tokens.push(Token::Indent(buf.clone()));
                        self.indent_stack.push(buf.clone());
                        buf.clear();
                    }
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

fn index_of_last_code_token(tokens: &Vec<Token>) -> Option<usize> {
    // find last code token in the output
    let mut index = tokens.len() - 1;
    loop {
        index = match tokens.get(index) {
            Some(&Token::Code(_)) => return Some(index),
            Some(_) => index - 1,
            None => return None,
        }
    }
}

fn transform(mut input: Vec<Token>) -> Vec<Token> {
    let mut output = Vec::<Token>::new();

    for tok in input.drain(..) {
        match tok {
            Token::Indent(_) => {
                let index = match index_of_last_code_token(&output) {
                    Some(index) => index,
                    None => output.len() - 1,
                };

                // insert brace immediately after that token
                output.insert(index + 1, Token::Whitespace(String::from(" ")));
                output.insert(index + 2, Token::OpenBrace);

                // push indent token onto the end
                output.push(tok);
            },
            Token::Dedent => {
                let index = match index_of_last_code_token(&output) {
                    Some(index) => index,
                    None => output.len() - 1,
                };

                // insert brace immediately after that token
                output.insert(index + 1, Token::Newline);
                output.insert(index + 2, Token::CloseBrace);

                // make sure token after brace is a newline
                if output.get(index + 3) != Some(&Token::Newline) {
                    output.insert(index + 3, Token::Newline);
                }
            },
            _ => output.push(tok),
        }
    }

    return output;
}

fn generate_code(tokens: &Vec<Token>) -> String {
    let mut code = String::new();

    for tok in tokens {
        match tok {
            &Token::Code(ref string) => code.push_str(&string),
            &Token::Whitespace(ref string) => code.push_str(&string),
            &Token::Newline => code.push('\n'),
            &Token::Indent(ref string) => code.push_str(&string),
            &Token::Dedent => (),
            &Token::OpenBrace => code.push('{'),
            &Token::CloseBrace => code.push('}'),
        }
    }

    return code;
}

fn process(code: &str) -> String {
    let tokens = tokenize(code);
    let tokens = transform(tokens);
    let code = generate_code(&tokens);

    return code;
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
#[allow(unused_variables)]
mod tests {
    extern crate unindent;
    use self::unindent::unindent;

    use super::*;

    /***
     * Helpers
     */

    // uses the tt-muncher pattern described here:
    // https://danielkeep.github.io/tlborm/book/pat-incremental-tt-munchers.html
    macro_rules! tokens {
        // recursive processing of token list
        ($vec:ident;) => {};
        ($vec:ident; c$code:expr, $($tail:tt)*) => {
            $vec.push(Token::Code(String::from($code)));
            tokens!($vec; $($tail)*);
        };
        ($vec:ident; w$code:expr, $($tail:tt)*) => {
            $vec.push(Token::Whitespace(String::from($code)));
            tokens!($vec; $($tail)*);
        };
        ($vec:ident; nl, $($tail:tt)*) => {
            $vec.push(Token::Newline);
            tokens!($vec; $($tail)*);
        };
        ($vec:ident; >$whitespace:expr, $($tail:tt)*) => {
            $vec.push(Token::Indent(String::from($whitespace)));
            tokens!($vec; $($tail)*);
        };
        ($vec:ident; <, $($tail:tt)*) => {
            $vec.push(Token::Dedent);
            tokens!($vec; $($tail)*);
        };
        ($vec:ident; ob, $($tail:tt)*) => {
            $vec.push(Token::OpenBrace);
            tokens!($vec; $($tail)*);
        };
        ($vec:ident; cb, $($tail:tt)*) => {
            $vec.push(Token::CloseBrace);
            tokens!($vec; $($tail)*);
        };

        // external entry point
        ($($tokens:tt)*) => {{
            let mut temp_vec = Vec::<Token>::new();
            tokens!(temp_vec; $($tokens)*);
            temp_vec
        }};
    }

    fn full_process_assert(input: &str,
                           expected_tokens: Option<Vec<Token>>,
                           expected_transform: Option<Vec<Token>>,
                           expected_code: Option<&str>) {
        let generated = tokenize(input);
        let transformed = transform(generated.clone());
        let final_code = generate_code(&transformed);

        // make sure at least one of the expected outputs is given
        assert!(
            expected_tokens != None ||
            expected_transform != None ||
            expected_code != None, "All expected output args are None.");

        // assert that generated token list is the same as expected token list
        if let Some(expected_tokens) = expected_tokens {
            assert_eq!(expected_tokens, generated,
                "\n\nGenerated token list didn't match expected list.\
                \nExpected:\n{:?}\nActual:\n{:?}\n",
                expected_tokens, generated);
        }

        // assert that untransformed generated code matches the input code
        assert_eq!(input, generate_code(&generated),
            "Untransformed token list didn't match input code.");

        // assert that transformed token list matches expected output
        if let Some(expected_transform) = expected_transform {
            assert_eq!(expected_transform, transformed,
                "\n\nTransformed token list didn't match expected list.\
                \nExpected:\n{:?}\nActual:\n{:?}\n",
                expected_transform, transformed);
        }

        // assert that transformed generated code matches the expected output
        if let Some(expected_code) = expected_code {
            assert_eq!(expected_code, final_code,
                    "Final output didn't match expected output.");
        }
    }

    /***
     * Tests
     */

    #[test]
    fn test_single_statement_no_newline() {
        let input = "statement;";
        let tokens = tokens![
            c"statement;",
        ];
        let transform = tokens.clone();
        let output = input.clone();

        full_process_assert(&input, Some(tokens), Some(transform), Some(&output));
    }

    #[test]
    fn test_single_statement() {
        let input = "statement;\n";
        let tokens = tokens![
            c"statement;", nl,
        ];
        let transform = tokens.clone();
        let output = input.clone();

        full_process_assert(&input, Some(tokens), Some(transform), Some(&output));
    }

    #[test]
    fn test_newline() {
        let input = unindent("
            statement;
            statement;
            ");
        let tokens = tokens![
            c"statement;", nl,
            c"statement;", nl,
        ];
        let transform = tokens.clone();
        let output = input.clone();

        full_process_assert(&input, Some(tokens), Some(transform), Some(&output));
    }

    #[test]
    fn test_single_indent() {
        let input = unindent("
            if condition
                something happens;
            ");
        let tokens = tokens![
            c"if condition", nl,
            >"    ", c"something happens;", nl,
            <,
        ];
        let transform = tokens![
            c"if condition", w" ", ob, nl,
            >"    ", c"something happens;", nl,
            cb, nl,
        ];
        let output = unindent("
            if condition {
                something happens;
            }
            ");

        full_process_assert(&input, Some(tokens), Some(transform), Some(&output));
    }

    #[test]
    fn test_multiple_lines_single_indent() {
        let input = unindent("
            if condition
                something happens;
                something happens;
                something happens;
            ");
        let tokens = tokens![
            c"if condition", nl,
            >"    ", c"something happens;", nl,
            w"    ", c"something happens;", nl,
            w"    ", c"something happens;", nl,
            <,
        ];
        let transform = tokens![
            c"if condition", w" ", ob, nl,
            >"    ", c"something happens;", nl,
            w"    ", c"something happens;", nl,
            w"    ", c"something happens;", nl,
            cb, nl,
        ];
        let output = unindent("
            if condition {
                something happens;
                something happens;
                something happens;
            }
            ");

        full_process_assert(&input, Some(tokens), Some(transform), Some(&output));
    }

    #[test]
    fn test_single_indent_no_newline() {
        let input = unindent("
            if condition
                something happens;");
        let tokens = tokens![
            c"if condition", nl,
            >"    ", c"something happens;",
            <,
        ];
        let transform = tokens![
            c"if condition", w" ", ob, nl,
            >"    ", c"something happens;", nl,
            cb, nl,
        ];
        let output = unindent("
            if condition {
                something happens;
            }
            ");

        full_process_assert(&input, Some(tokens), Some(transform), Some(&output));
    }

    #[test]
    fn test_basic_if_else_structure() {
        let input = unindent("
            if condition
                something happens;
            else
                something else happens;
            ");
        let tokens = tokens![
            c"if condition", nl,
            >"    ", c"something happens;", nl,
            <, c"else", nl,
            >"    ", c"something else happens;", nl,
            <,
        ];
        let transform = tokens![
            c"if condition", w" ", ob, nl,
            >"    ", c"something happens;", nl,
            cb, nl,
            c"else", w" ", ob, nl,
            >"    ", c"something else happens;", nl,
            cb, nl,
        ];
        let output = unindent("
            if condition {
                something happens;
            }
            else {
                something else happens;
            }
            ");

        full_process_assert(&input, Some(tokens), Some(transform), Some(&output));
    }

    #[test]
    fn test_gap_indent() {
        let input = unindent("
            if condition

                something happens;");
        let tokens = tokens![
            c"if condition", nl,
            nl,
            >"    ", c"something happens;",
            <,
        ];
        let transform = tokens![
            c"if condition", w" ", ob, nl,
            nl,
            >"    ", c"something happens;", nl,
            cb, nl,
        ];
        let output = unindent("
            if condition {

                something happens;
            }
            ");

        full_process_assert(&input, Some(tokens), Some(transform), Some(&output));
    }

    #[test]
    fn test_double_indent() {
        let input = unindent("
            if condition 1
                if condition 2
                    statement;
            ");
        let tokens = tokens![
            c"if condition 1", nl,
            >"    ", c"if condition 2", nl,
            w"    ", >"    ", c"statement;", nl,
            <, <,
        ];
        let transform = tokens![
            c"if condition 1", w" ", ob, nl,
            >"    ", c"if condition 2", w" ", ob, nl,
            w"    ", >"    ", c"statement;", nl,
            cb, nl,
            cb, nl,
        ];
        let output = unindent("
            if condition 1 {
                if condition 2 {
                    statement;
            }
            }
            ");

        full_process_assert(&input, Some(tokens), Some(transform), Some(&output));
    }

    #[test]
    fn test_double_if_else() {
        let input = unindent("
            if condition 1
                if condition 2
                    statement 1;
                else
                    statement 2;
            else
                statement 3;
            ");
        let tokens = tokens![
            c"if condition 1", nl,
            >"    ", c"if condition 2", nl,
            w"    ", >"    ", c"statement 1;", nl,
            w"    ", <, c"else", nl,
            w"    ", >"    ", c"statement 2;", nl,
            <, <, c"else", nl,
            >"    ", c"statement 3;", nl,
            <,
        ];
        let transform = tokens![
            c"if condition 1", w" ", ob, nl,
            >"    ", c"if condition 2", w" ", ob, nl,
            w"    ", >"    ", c"statement 1;", nl,
            cb, nl,
            w"    ", c"else", w" ", ob, nl,
            w"    ", >"    ", c"statement 2;", nl,
            cb, nl,
            cb, nl,
            c"else", w" ", ob, nl,
            >"    ", c"statement 3;", nl,
            cb, nl,
        ];
        let output = unindent("
            if condition 1 {
                if condition 2 {
                    statement 1;
            }
                else {
                    statement 2;
            }
            }
            else {
                statement 3;
            }
            ");

        full_process_assert(&input, Some(tokens), Some(transform), Some(&output));
    }

    #[test]
    fn test_complex_indentation() {
        let input = unindent("
            if cond
                if cond
                    if cond
                        if cond
                            if cond
                                statement;
                            else
                                statement;
                        else
                            statement;
                        if cond
                            if cond
                                if cond
                                    if cond
                                        statement;
                                else
                                    statement;
                                if cond
                                    if cond
                                        statement;
                                    else
                                        statement;
                                else
                                    statement;
                            else
                                statement;
                    else
                        if cond
                            if cond
                                statement;
                            else
                                statement;
                        else
                            statement;
                else
                    if cond
                        statement;
            else
                statement;
            ");
        let output = unindent("
            if cond {
                if cond {
                    if cond {
                        if cond {
                            if cond {
                                statement;
            }
                            else {
                                statement;
            }
            }
                        else {
                            statement;
            }
                        if cond {
                            if cond {
                                if cond {
                                    if cond {
                                        statement;
            }
            }
                                else {
                                    statement;
            }
                                if cond {
                                    if cond {
                                        statement;
            }
                                    else {
                                        statement;
            }
            }
                                else {
                                    statement;
            }
            }
                            else {
                                statement;
            }
            }
            }
                    else {
                        if cond {
                            if cond {
                                statement;
            }
                            else {
                                statement;
            }
            }
                        else {
                            statement;
            }
            }
            }
                else {
                    if cond {
                        statement;
            }
            }
            }
            else {
                statement;
            }
            ");

        full_process_assert(&input, None, None, Some(&output));
    }

}
