use crate::Transformer;
use lowlang_syntax::*;
use lowlang_syntax::visit::VisitorMut;

pub struct SwitchRemover {
    changed: bool
}

impl SwitchRemover {
    pub fn new() -> SwitchRemover {
        SwitchRemover {
            changed: false
        }
    }
}

impl<'t> Transformer<'t> for SwitchRemover {
    fn transform(&mut self, package: &mut Package<'t>) -> bool {
        self.visit_package(package);
        self.changed
    }

    fn reset(&mut self) {
        self.changed = false;
    }
}

impl<'t> VisitorMut<'t> for SwitchRemover {
    fn visit_term(&mut self, term: &mut Terminator<'t>) {
        if let Terminator::Switch(Operand::Constant(c), values, targets) = term {
            match c {
                Const::Scalar(value, _) => {
                    let mut target = *targets.last().unwrap();

                    for (v, t) in values.iter().zip(targets.iter()) {
                        if v == value {
                            target = *t;
                            break;
                        }
                    }

                    *term = Terminator::Jump(target);
                    self.changed = true;
                },
                _ => unreachable!(),
            }
        }
    }
}
