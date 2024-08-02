use ena::unify::{EqUnifyValue, UnifyKey};

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq, PartialOrd)]
pub struct TypeVar(u32);

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq, PartialOrd)]
pub struct TypeUniVar(u32);

// It is important to note the distinction made between type variables and
// unification variables. A type variable is rigid, they are universally
// quantified, and they should not be present anywhere during unification. They
// are only introduced through type annotations as of now. When generalized
// types (type schemes) are instantiated, any occurence of a type variable will
// be replaced by fresh unification variables. A unification variable is
// flexible, they are placeholders for a rigid, concrete type. This could be a
// type constant, function type, or a type variable as well.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Type {
    Var(TypeVar),
    UniVar(TypeUniVar),
    Const(String), // Type constant
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
            Type::Var(_) | Type::Const(_) => Ok(()),

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
