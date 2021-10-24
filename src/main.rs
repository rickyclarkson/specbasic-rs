use std::collections::{BTreeMap, HashMap};

enum Expression {
    IntExpression(i32),
    NumberVariableExpression(String)
}
impl Expression {
    fn to_string(&self, env: &Env) -> String {
        match self {
            Expression::IntExpression(value) => value.to_string(),
            Expression::NumberVariableExpression(number_variable_name) => env.number_variables.get(number_variable_name).unwrap().to_string(),
        }
    }

    fn to_f64(&self, env: &Env) -> f64 {
        match self {
            Expression::IntExpression(value) => *value as f64,
            Expression::NumberVariableExpression(number_variable_name) => *env.number_variables.get(number_variable_name).unwrap(),
        }
    }
}

struct Env {
    number_variables: HashMap<String, f64>
}
impl Env {
    fn new() -> Env {
        Env {
            number_variables: HashMap::new()
        }
    }
}

trait Command {
    fn run(&self, env: &mut Env) -> String;
}

struct LetCommand {
    variable_name: String,
    value: Expression
}
impl Command for LetCommand {
  fn run(&self, env: &mut Env) -> String {
      env.number_variables.insert(self.variable_name.clone(), self.value.to_f64(&env));
      String::new()
  }
}

struct PrintCommand {
    expression: Expression
}
impl Command for PrintCommand {
    fn run(&self, env: &mut Env) -> String {
        self.expression.to_string(&env) + "\n"
    }
}

struct Program {
    lines: BTreeMap<i32, Box<dyn Command>>
}

impl Program {
    fn new() -> Program {
        Program {
            lines: BTreeMap::new()
        }
    }

    fn add_line(&mut self, line_number: i32, command: Box<dyn Command>) {
      self.lines.insert(line_number, command);
    }

    fn run(&self) -> String {
        let mut output = String::new();
        let mut env = Env::new();

        let lines = &self.lines;
        for (line_num, command) in lines {
            output += &(command.run(&mut env));
        }
        output
    }
}

#[cfg(test)]
mod tests {
    use crate::*;

    #[test]
    fn let_and_print() {
        let mut program = Program::new();
        program.add_line(20, Box::new(PrintCommand { expression: Expression::NumberVariableExpression("a".to_string()) }));
        program.add_line(10, Box::new(LetCommand {
                variable_name: "a".to_string(),
                value: Expression::IntExpression(10)
            }));
        let output = program.run();
        assert_eq!(output, "10\n");
    }
}

fn main() {
    println!("Hello, world!");
}