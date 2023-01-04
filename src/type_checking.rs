use crate::Token;

#[derive(Debug)]
pub(crate) enum TypeCheckError {
}

pub(crate) fn type_check(tokens: &[Token]) -> Result<(), TypeCheckError> {
    Ok(())
}
