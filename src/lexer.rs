use logos::{skip, Logos};

#[derive(Clone, Debug, Logos, PartialEq)]
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
    #[token("true", |_| true)]
    #[token("false", |_| false)]
    LitBool(bool),

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
    DoubleColon,

    #[token("\\")]
    Backslash,

    #[token("->")]
    SmallArrow,
    #[token("=>")]
    BigArrow,

    #[regex(r"\n")]
    Newline,
    /* Note: The indent token also consumes a newline
     *       to distinguish it from whitespace which
     *       is skipped by the Space token.
     */
    #[regex(r"\n(?:  |\t)(?: \t)*")]
    Indent,
    #[regex(r"[ \t]+", skip)]
    Space,

    #[regex(r"[a-zA-Z]\w*", |lex| lex.slice().to_owned())]
    Identifier(String),
}
