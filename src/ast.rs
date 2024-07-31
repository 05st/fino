use crate::types::Type;

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct Var(pub u32);

#[derive(Debug)]
pub struct TypedVar(pub Var, pub Type);

#[derive(Debug)]
pub enum Expr<V> {
    Var(V),
    Int(i64),
    Lam(V, Box<Self>),
    App(Box<Self>, Box<Self>),
}

#[derive(Debug)]
struct FunctionId(u32);

#[derive(Debug)]
struct Function<V> {
    id: FunctionId,
    params: Vec<V>,
    body: Expr<V>,
}
