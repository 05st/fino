use std::{collections::HashMap, fmt::Display, path::PathBuf};

use logos::{skip, Filter, FilterResult, Lexer, Logos};

use crate::{error::{Error, ErrorKind}, location::{Location, Span}, parser::Precedence};

#[derive(Clone, Debug, PartialEq, Default)]
pub enum LexerError {
    UnexpectedIndent(usize),
    #[default]
    Default,
}

pub struct LexerState<'a> {
    indents: Vec<usize>,
    operator_map: &'a mut HashMap<String, Precedence>,
}

#[derive(Clone, Debug, Logos, PartialEq)]
#[logos(extras = LexerState<'s>, error = LexerError)]
pub enum Token {
    #[token("extern")]
    KwExtern,

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
    #[token("int")]
    KwInt,
    #[token("float")]
    KwFloat,

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
    #[token("\\")]
    Backslash,
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

    // Regex for operator identifier should be the same as the one for the Operator
    // token
    #[regex(r"(infl|infr|pref|post)\s+[\+\-*\/^!|<>=?$@#%:]+\s+[0-9]+\n", fixity_decl_callback)]
    FixityDecl,

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

fn fixity_decl_callback(lexer: &mut Lexer<Token>) -> Filter<()> {
    let parts = lexer.slice().split_ascii_whitespace().collect::<Vec<&str>>();
    let prec = parts[2].parse::<u8>().expect("Failed to parse operator precedence");

    // Left associative operators need higher right precedence,
    // right associative operators need higher left precedence
    let entry = match parts[0] {
        "pref" => Precedence::Prefix(prec * 2),
        "infl" => Precedence::Infix(prec * 2, prec * 2 + 1),
        "infr" => Precedence::Infix(prec * 2 + 1, prec * 2),
        "post" => Precedence::Postfix(prec * 2),
        // This should never happen, regex should not match other
        other => panic!("Bad fixity {:?}", other),
    };

    lexer.extras.operator_map.insert(String::from(parts[1]), entry);

    Filter::Skip
}

fn whitespace_callback(lexer: &mut Lexer<Token>) -> FilterResult<Token, LexerError> {
    let new_col = lexer
        .slice()
        .chars()
        .filter(|ch| *ch == ' ' || *ch == '\t')
        .count();

    let indents = &mut lexer.extras.indents;
    let cur_col = indents.last().copied().unwrap_or(0);

    match new_col.cmp(&cur_col) {
        std::cmp::Ordering::Less => {
            if indents.contains(&new_col) || new_col == 0 {
                // Drop all indentation levels greater than new_col
                while indents.last().copied().unwrap_or(0) != new_col {
                    indents.pop();
                }

                FilterResult::Emit(Token::Dedent)
            } else {
                FilterResult::Error(LexerError::UnexpectedIndent(new_col))
            }
        }

        std::cmp::Ordering::Equal => FilterResult::Emit(Token::Newline),

        std::cmp::Ordering::Greater => {
            indents.push(new_col);
            FilterResult::Emit(Token::Indent)
        }
    }
}

pub fn tokenize(source: &String, filepath: PathBuf, operator_map: &mut HashMap<String, Precedence>) -> Result<Vec<(Token, Span)>, Error> {
    let mut tokens = Vec::new();

    let lexer = Token::lexer_with_extras(source, LexerState {
        indents: Vec::new(),
        operator_map,
    }).spanned();
    
    for lex in lexer {
        match lex {
            (Ok(token), span) => tokens.push((token, span)),
            (Err(lexer_error), span) => return Err(
                // Translate lexer errors
                match lexer_error {
                    LexerError::UnexpectedIndent(size) => {
                        Error::new(ErrorKind::UnexpectedIndent(size), Location::new(filepath, span))
                    }
                    LexerError::Default => {
                        Error::new(ErrorKind::UnknownToken, Location::new(filepath, span))
                    }
                }
            ),
        }
    }

    Ok(tokens)
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::KwExtern => write!(f, "'extern'"),
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
            Token::KwInt => write!(f, "'int'"),
            Token::KwFloat => write!(f, "'float'"),
            Token::LitString(_) => write!(f, "string literal"),
            Token::LitInteger(_) => write!(f, "integer literal"),
            Token::LitFloat(_) => write!(f, "float literal"),
            Token::LitBool(_) => write!(f, "boolean literal"),
            Token::LitChar(_) => write!(f, "character literal"),
            Token::LitUnit => write!(f, "'()'"),
            Token::LeftParen => write!(f, "'('"),
            Token::RightParen => write!(f, "')'"),
            Token::Equal => write!(f, "'='"),
            Token::Bar => write!(f, "'|'"),
            Token::Colon => write!(f, "':'"),
            Token::Backslash => write!(f, "'\\'"),
            Token::SmallArrow => write!(f, "'->'"),
            Token::BigArrow => write!(f, "'=>'"),
            Token::Dot => write!(f, "'.'"),
            Token::SpacedDot => write!(f, "' .'"),
            Token::FixityDecl => write!(f, "fixity declaration"),
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
