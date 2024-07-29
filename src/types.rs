use ena::unify::{EqUnifyValue, UnifyKey};

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq, PartialOrd)]
pub struct TypeVar(pub u32);

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq, PartialOrd)]
pub struct TypeUniVar(pub u32);

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Type {
    Var(TypeVar),
    UniVar(TypeUniVar),
    Unit,
    Int,
    Fun(Box<Self>, Box<Self>),
}

impl EqUnifyValue for Type {}

impl UnifyKey for TypeUniVar {
    type Value = Option<Type>;

    fn index(&self) -> u32 {
        self.0
    }

    fn from_index(u: u32) -> Self {
        Self(u)
    }

    fn tag() -> &'static str {
        todo!()
    }
}

impl Type {
    pub fn occurs_check(&self, uvar: TypeUniVar) -> Result<(), Type> {
        match self {
            Type::Unit | Type::Int | Type::Var(_) => Ok(()),
            Type::UniVar(uvar_self) => {
                if *uvar_self == uvar {
                    Err(Type::UniVar(*uvar_self))
                } else {
                    Ok(())
                }
            }
            Type::Fun(param, ret) => {
                param.occurs_check(uvar).map_err(|_| self.clone())?;
                ret.occurs_check(uvar).map_err(|_| self.clone())
            }
        }
    }
}
