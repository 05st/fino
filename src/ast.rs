use crate::types::Type;

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct Var(pub u32);

#[derive(Debug)]
pub struct TypedVar(pub Var, pub Type);

#[derive(Debug)]
pub enum Ast<V> {
    Var(V),
    Int(i64),
    Lam(V, Box<Self>),
    App(Box<Self>, Box<Self>),
}
