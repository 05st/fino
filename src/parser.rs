use logos::Logos;

#[derive(Logos, Debug, PartialEq)]
enum Token {
    #[regex("[A-Z][a-zA-Z]*", priority = 3)]
    CapIdent,
    #[regex("[a-zA-Z][a-zA-Z0-9_]*")]
    Ident,

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
    #[token("else")]
    KwElse,
    #[token("match")]
    KwMatch,

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

    #[token("true")]
    KwTrue,
    #[token("false")]
    KwFalse,

    #[regex(r"\n")]
    Newline,
    #[regex(r"[ \t]+")]
    Whitespace,

    #[token("(")]
    LeftParen,
    #[token(")")]
    RightParen,

    #[token("=")]
    Equal,
    #[token("==")]
    EqualEqual,

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
}
