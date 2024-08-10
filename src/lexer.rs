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
    #[regex(r"-?[0-9]+", |lex| lex.slice().parse::<i64>().unwrap())]
    LitInteger(i64),
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
    LitUnit,

    #[token("(")]
    LeftParen,
    #[token(")")]
    RightParen,

    #[token("=")]
    Equal,
    #[token("|")]
    Bar,
    #[token(":")]
    Colon,
    #[token("->")]
    SmallArrow,
    #[token("=>")]
    BigArrow,

    // Used for indexing
    #[token(".")]
    Dot,
    // Function composition
    #[token(" .")]
    SpacedDot,

    // Priority is set lower for these three because the patterns can also match
    // reserved keywords / operators.
    #[regex(r"[\+\-*\/^!|<>=?$@#%:]+", priority = 1, callback = |lex| lex.slice().to_owned())]
    Operator(String),
    #[regex(r"[A-Z]\w*", priority = 1, callback = |lex| lex.slice().to_owned())]
    UpperIdentifier(String),
    #[regex(r"[a-z]\w*", priority = 1, callback = |lex| lex.slice().to_owned())]
    LowerIdentifier(String),

    // The indent token also consumes a newline to distinguish it from whitespace
    // which is skipped by the Space token.
    #[regex(r"\n[ \t]*", whitespace_callback)]
    Indent,
    Dedent,
    Newline,

    #[regex(r"[ \t]+", skip)]
    Space,

    // Created by parser only
    Eof,
}

fn whitespace_callback(lexer: &mut Lexer<Token>) -> FilterResult<Token, LexerError> {
    let cur_col = lexer.extras.last().copied().unwrap_or(0);

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
        match self {
            Token::KwModule => write!(f, "'module'"),
            Token::KwImport => write!(f, "'import'"),
            Token::KwExport => write!(f, "'export'"),
            Token::KwFn => write!(f, "'fn'"),
            Token::KwType => write!(f, "'type'"),
            Token::KwLet => write!(f, "'let'"),
            Token::KwIn => write!(f, "'in'"),
            Token::KwIf => write!(f, "'if'"),
            Token::KwThen => write!(f, "'then'"),
            Token::KwElse => write!(f, "'else'"),
            Token::KwMatch => write!(f, "'match'"),
            Token::KwDo => write!(f, "'do'"),
            Token::KwUnit => write!(f, "'unit'"),
            Token::KwBool => write!(f, "'bool'"),
            Token::KwChar => write!(f, "'char'"),
            Token::KwStr => write!(f, "'str'"),
            Token::KwI32 => write!(f, "'i32'"),
            Token::KwI64 => write!(f, "'i64'"),
            Token::KwU32 => write!(f, "'u32'"),
            Token::KwU64 => write!(f, "'u64'"),
            Token::KwF32 => write!(f, "'f32'"),
            Token::KwF64 => write!(f, "'f64'"),
            Token::LitString(_) => write!(f, "string"),
            Token::LitInteger(_) => write!(f, "integer"),
            Token::LitFloat(_) => write!(f, "float"),
            Token::LitBool(_) => write!(f, "boolean"),
            Token::LitChar(_) => write!(f, "character"),
            Token::LitUnit => write!(f, "'()'"),
            Token::LeftParen => write!(f, "'('"),
            Token::RightParen => write!(f, "')'"),
            Token::Equal => write!(f, "'='"),
            Token::Bar => write!(f, "'|'"),
            Token::Colon => write!(f, "':'"),
            Token::SmallArrow => write!(f, "'->'"),
            Token::BigArrow => write!(f, "'=>'"),
            Token::Dot => write!(f, "'.'"),
            Token::SpacedDot => write!(f, "' .'"),
            Token::Operator(_) => write!(f, "operator"),
            Token::UpperIdentifier(_) => write!(f, "uppercase identifier"),
            Token::LowerIdentifier(_) => write!(f, "lowercase identifier"),
            Token::Indent => write!(f, "indentation"),
            Token::Dedent => write!(f, "de-indentation"),
            Token::Newline => write!(f, "newline"),
            Token::Space => write!(f, "space"),
            Token::Eof => write!(f, "end of file"),
        }
    }
}
