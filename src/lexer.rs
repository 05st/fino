use std::fmt::Display;

use logos::{skip, FilterResult, Lexer, Logos};

#[derive(Clone, Debug, PartialEq, Default)]
pub enum LexerError {
    UnexpectedIndent(usize),
    #[default]
    Default,
}

#[derive(Clone, Debug, Logos, PartialEq)]
#[logos(extras = Vec<usize>, error = LexerError)]
pub enum Token {
    #[token("module")]
    KwModule,
    #[token("import")]
    KwImport,
    #[token("export")]
    KwExport,

    #[token("fn")]
    KwFn,
    #[token("type")]
    KwType,

    #[token("let")]
    KwLet,
    #[token("in")]
    KwIn,
    #[token("if")]
    KwIf,
    #[token("then")]
    KwThen,
    #[token("else")]
    KwElse,
    #[token("match")]
    KwMatch,
    #[token("do")]
    KwDo,

    #[token("unit")]
    KwUnit,
    #[token("bool")]
    KwBool,
    #[token("char")]
    KwChar,
    #[token("str")]
    KwStr,
    #[token("i32")]
    KwI32,
    #[token("i64")]
    KwI64,
    #[token("u32")]
    KwU32,
    #[token("u64")]
    KwU64,
    #[token("f32")]
    KwF32,
    #[token("f64")]
    KwF64,

    #[regex(r#""([^"\\]|\\["\\bnfrt]|u[a-fA-F0-9]{4})*""#, |lex| lex.slice().to_owned())]
    LitString(String),
    #[regex(r"[0-9]+", |lex| lex.slice().parse::<u64>().unwrap())]
    LitDecimal(u64),
    #[regex(r"[0-9]+\.[0-9]+", |lex| lex.slice().parse::<f64>().unwrap())]
    LitFloat(f64),
    #[token("true", |_| true)]
    #[token("false", |_| false)]
    LitBool(bool),
    // TODO:
    // Properly parse escape sequences
    #[regex(r"'.'", |lex| lex.slice().chars().nth(1).to_owned())]
    LitChar(char),

    #[token("()")]
    ClosedParens,
    #[token("(")]
    LeftParen,
    #[token(")")]
    RightParen,

    #[token("=")]
    Equal,
    #[token("==")]
    EqualEqual,

    #[token("+")]
    Plus,
    #[token("-")]
    Dash,
    #[token("*")]
    Asterisk,
    #[token("/")]
    Slash,

    #[token(".")]
    Dot,
    #[token("|")]
    Bar,

    #[token(":")]
    Colon,
    #[token("::")]
    ColonColon,

    #[token("\\")]
    Backslash,

    #[token("->")]
    SmallArrow,
    #[token("=>")]
    BigArrow,

    // The indent token also consumes a newline to distinguish it from whitespace
    // which is skipped by the Space token.
    #[regex(r"\n[ \t]*", whitespace_callback)]
    Indent,
    Dedent,
    Newline,

    #[regex(r"[ \t]+", skip)]
    Space,

    #[regex(r"[A-Z]\w*", |lex| lex.slice().to_owned())]
    UpperIdentifier(String),
    #[regex(r"[a-z]\w*", |lex| lex.slice().to_owned())]
    LowerIdentifier(String),
}

fn whitespace_callback(lexer: &mut Lexer<Token>) -> FilterResult<Token, LexerError> {
    let cur_col = lexer
        .extras
        .last()
        .copied()
        .unwrap_or(0);

    let new_col = lexer
        .slice()
        .chars()
        .filter(|ch| *ch == ' ' || *ch == '\t')
        .count();

    match new_col.cmp(&cur_col) {
        std::cmp::Ordering::Less => {
            if lexer.extras.contains(&new_col) || new_col == 0 {
                // Drop all indentation levels greater than new_col
                while lexer.extras.last().copied().unwrap_or(0) != new_col {
                    lexer.extras.pop();
                }

                FilterResult::Emit(Token::Dedent)
            } else {
                FilterResult::Error(LexerError::UnexpectedIndent(new_col))
            }
        }

        std::cmp::Ordering::Equal => FilterResult::Emit(Token::Newline),

        std::cmp::Ordering::Greater => {
            lexer.extras.push(new_col);
            FilterResult::Emit(Token::Indent)
        }
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        todo!()
    }
}
