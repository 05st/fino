use crate::types::Type;

pub struct Var(u32);

pub struct TypedVar(Var, Type);

pub enum Ast<V> {
    Var(V),
    Int(i64),
    Fun(V, Box<Self>),
    App(Box<Self>, Box<Self>),
}
