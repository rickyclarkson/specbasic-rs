use std::collections::hash_map::Entry;
use std::collections::{BTreeMap, HashMap};
use std::io::stdin;
use std::ops;
use std::result::Result;
use std::vec::Vec;

#[derive(Clone)]
enum Expression {
    Integer(i32),
    Text(String),
    NumberVariable(String),
    Plus(Box<Expression>, Box<Expression>),
    Subtract(Box<Expression>, Box<Expression>),
    Multiply(Box<Expression>, Box<Expression>),
    Divide(Box<Expression>, Box<Expression>),
    AreEqual(Box<Expression>, Box<Expression>),
    LessThan(Box<Expression>, Box<Expression>),
    GreaterThan(Box<Expression>, Box<Expression>),
}

impl ops::Add for Expression {
    type Output = Expression;

    fn add(self, rhs: Self) -> Self::Output {
        Expression::Plus(Box::from(self), Box::from(rhs))
    }
}

impl ops::Sub for Expression {
    type Output = Expression;

    fn sub(self, rhs: Self) -> Self::Output {
        Expression::Subtract(Box::from(self), Box::from(rhs))
    }
}

impl ops::Mul for Expression {
    type Output = Expression;

    fn mul(self, rhs: Self) -> Self::Output {
        Expression::Multiply(Box::from(self), Box::from(rhs))
    }
}

impl ops::Div for Expression {
    type Output = Expression;

    fn div(self, rhs: Self) -> Self::Output {
        Expression::Divide(Box::from(self), Box::from(rhs))
    }
}

impl Expression {
    fn text(value: &str) -> Expression {
        Expression::Text(value.to_string())
    }
    fn number_variable(name: &str) -> Expression {
        Expression::NumberVariable(name.to_string())
    }
    fn greater_than(self, other: Expression) -> Expression {
        Expression::GreaterThan(Box::from(self), Box::from(other))
    }
    fn are_equal(self, other: Expression) -> Expression {
        Expression::AreEqual(Box::from(self), Box::from(other))
    }
    fn less_than(self, other: Expression) -> Expression {
        Expression::LessThan(Box::from(self), Box::from(other))
    }
    fn to_string(&self, env: &Env) -> Result<String, String> {
        match self {
            Expression::Integer(value) => Ok(value.to_string()),
            Expression::Text(text) => Ok(text.to_string()),
            Expression::NumberVariable(number_variable_name) => Ok(env
                .number_variables
                .get(number_variable_name)
                .unwrap()
                .to_string()),
            Expression::Plus(_, _) => self.to_f64(env).map(|v| v.to_string()),
            Expression::Subtract(_, _) => self.to_f64(env).map(|v| v.to_string()),
            Expression::Multiply(_, _) => self.to_f64(env).map(|v| v.to_string()),
            Expression::Divide(_, _) => self.to_f64(env).map(|v| v.to_string()),
            Expression::AreEqual(_, _) => self.to_string(env).map(|v| v.to_string()),
            Expression::LessThan(_, _) => self.to_string(env).map(|v| v.to_string()),
            Expression::GreaterThan(_, _) => self.to_string(env).map(|v| v.to_string()),
        }
    }

    fn to_f64(&self, env: &Env) -> Result<f64, String> {
        match self {
            Expression::Integer(value) => Ok(*value as f64),
            Expression::Text(_text) => {
                Err("Expected a number, found something else, but I can't figure out how to include that in the error".to_string())
            }
            Expression::NumberVariable(number_variable_name) =>
                match env.number_variables.get(number_variable_name) {
                    Some(value) => Ok(*value),
                    None => {
                        Err(format!("No variable {} found.", number_variable_name))
                    },
                },
            Expression::Plus(left, right) => match (left.to_f64(env), right.to_f64(env)) {
                (Ok(l), Ok(r)) => Ok(l + r),
                (Ok(_), Err(e)) => Err(e),
                (Err(e), _) => Err(e),
            },
            Expression::Subtract(left, right) => match (left.to_f64(env), right.to_f64(env)) {
                (Ok(l), Ok(r)) => Ok(l - r),
                (Ok(_), Err(e)) => Err(e),
                (Err(e), _) => Err(e),
            },
            Expression::Multiply(left, right) => match (left.to_f64(env), right.to_f64(env)) {
                (Ok(l), Ok(r)) => Ok(l * r),
                (Ok(_), Err(e)) => Err(e),
                (Err(e), _) => Err(e),
            },
            Expression::Divide(left, right) => match (left.to_f64(env), right.to_f64(env)) {
                (Ok(l), Ok(r)) => Ok(l / r),
                (Ok(_), Err(e)) => Err(e),
                (Err(e), _) => Err(e),
            },
            Expression::AreEqual(_, _) => Err("AreEqual gives a bool, not an f64".to_string()),
            Expression::LessThan(_, _) => Err("LessThan gives a bool, not an f64".to_string()),
            Expression::GreaterThan(_, _) => Err("GreaterThan gives a bool, not an f64".to_string()),
        }
    }

    fn to_bool(&self, env: &Env) -> Result<bool, String> {
        match self {
            Expression::Integer(value) => Err(value.to_string()),
            Expression::Text(value) => Err(value.to_string()),
            Expression::NumberVariable(value) => Err(value.to_string()),
            Expression::Plus(_, _) => {
                Err("Cannot convert the result of a plus operation to boolean".to_string())
            }
            Expression::Subtract(_, _) => {
                Err("Cannot convert the result of a subtract operation to boolean".to_string())
            }
            Expression::Multiply(_, _) => {
                Err("Cannot convert the result of a multiply operation to boolean".to_string())
            }
            Expression::Divide(_, _) => {
                Err("Cannot convert the result of a divide operation to boolean".to_string())
            }
            Expression::AreEqual(left, right) => match (left.to_f64(env), right.to_f64(env)) {
                (Ok(l), Ok(r)) => Ok(l == r),
                (_, _) => match (left.to_string(env), right.to_string(env)) {
                    (Ok(l), Ok(r)) => Ok(l == r),
                    (_, Err(e)) => Err(e.to_string()),
                    (Err(e), _) => Err(e.to_string()),
                },
            },
            Expression::LessThan(left, right) => match (left.to_f64(env), right.to_f64(env)) {
                (Ok(l), Ok(r)) => Ok(l < r),
                (_, Err(e)) => Err(e.to_string()),
                (Err(e), _) => Err(e.to_string()),
            },
            Expression::GreaterThan(left, right) => match (left.to_f64(env), right.to_f64(env)) {
                (Ok(l), Ok(r)) => Ok(l > r),
                (_, Err(e)) => Err(e.to_string()),
                (Err(e), _) => Err(e.to_string()),
            },
        }
    }
}

enum UserInputReader {
    RealStdin,
    FakeStdin(String),
}

impl UserInputReader {
    fn next(&self) -> String {
        match self {
            UserInputReader::RealStdin => {
                let mut output = String::new();
                stdin().read_line(&mut output).unwrap();
                output
            }
            UserInputReader::FakeStdin(input) => input.to_string(),
        }
    }
}

struct Env {
    number_variables: HashMap<String, f64>,
    user_input_reader: UserInputReader,
    loop_line_numbers: HashMap<String, i32>,
    current_line_number: i32,
}

impl Env {
    fn new(user_input_reader: UserInputReader) -> Env {
        Env {
            number_variables: HashMap::new(),
            user_input_reader,
            loop_line_numbers: HashMap::new(),
            current_line_number: 0,
        }
    }

    fn read_line(&self) -> String {
        self.user_input_reader.next()
    }
}

enum CommandResult {
    Output(String),
    Jump(i32),
    Stop(String),
}

#[derive(Clone)]
enum Command {
    Let(String, Expression),
    Goto(i32),
    Print(Vec<Expression>),
    Input(Vec<Expression>),
    Rem(String),
    If(Expression, Vec<Command>),
    Stop,
    For(String, Expression, Expression, Expression),
    Next(String),
}

impl Command {
    fn let_equal(name: &str, value: Expression) -> Command {
        Command::Let(name.to_string(), value)
    }
    fn for_loop(
        variable_name: &str,
        begin: Expression,
        end: Expression,
        step: Expression,
    ) -> Command {
        Command::For(variable_name.to_string(), begin, end, step)
    }
    fn next(variable_name: &str) -> Command {
        Command::Next(variable_name.to_string())
    }
    fn run(&self, env: &mut Env) -> Result<CommandResult, String> {
        match self {
            Command::Let(variable_name, value) => match value.to_f64(env) {
                Ok(number) => {
                    env.number_variables.insert(variable_name.clone(), number);
                    Ok(CommandResult::Output(String::new()))
                }
                Err(msg) => Err(msg.to_string()),
            },
            Command::Goto(line_number) => Ok(CommandResult::Jump(*line_number)),
            Command::Print(expressions) => Ok(CommandResult::Output(
                expressions
                    .iter()
                    .flat_map(|e| e.to_string(env))
                    .collect::<Vec<String>>()
                    .join(" ")
                    + "\n",
            )),
            Command::Input(expressions) => {
                let mut output = String::new();
                for expression in expressions {
                    match expression {
                        Expression::Text(text) => {
                            output.push_str(text);
                            output.push('\n');
                        }
                        Expression::NumberVariable(name) => {
                            let value = env.read_line();
                            match value.parse::<f64>() {
                                Ok(v) => {
                                    env.number_variables.insert(name.to_owned(), v);
                                }
                                Err(msg) => {
                                    return Err(msg.to_string());
                                }
                            }
                        }
                        _ => {
                            return Err("Unexpected expression type found, and I don't know how to report which".to_string());
                        }
                    }
                }
                Ok(CommandResult::Output(output))
            }
            Command::Rem(_) => Ok(CommandResult::Output(String::new())),
            Command::If(test, then) => match test.to_bool(env) {
                Ok(false) => Ok(CommandResult::Output("".to_string())),
                Ok(true) => {
                    let mut output = String::new();
                    for command in then {
                        match &(command.run(env)) {
                            Ok(CommandResult::Output(text)) => output.push_str(text),
                            Ok(CommandResult::Jump(line_number)) => {
                                return Ok(CommandResult::Jump(*line_number))
                            }
                            Ok(CommandResult::Stop(text)) => {
                                output.push_str(text);
                                return Ok(CommandResult::Stop(output));
                            }
                            Err(msg) => return Err(msg.to_string()),
                        }
                    }
                    Ok(CommandResult::Output(output))
                }
                Err(e) => Err(e),
            },
            Command::Stop => Ok(CommandResult::Stop("".to_string())),
            Command::For(variable_name, begin, end, step) => {
                let begin = begin.to_f64(env);
                let end = end.to_f64(env);
                let step = step.to_f64(env);
                match (
                    env.number_variables.entry(variable_name.to_string()),
                    begin,
                    end,
                    step,
                ) {
                    (_, Err(e), _, _) => Err(e.to_string()),
                    (_, _, Err(e), _) => Err(e.to_string()),
                    (_, _, _, Err(e)) => Err(e.to_string()),
                    (Entry::Vacant(v), Ok(b), _, _) => {
                        v.insert(b);
                        env.loop_line_numbers
                            .insert(variable_name.to_string(), env.current_line_number);
                        Ok(CommandResult::Output("".to_string()))
                    }
                    (Entry::Occupied(mut o), Ok(_), Ok(e), Ok(s)) => {
                        o.insert(o.get() + s);
                        if *o.get() == e {
                            env.loop_line_numbers.remove(variable_name.as_str());
                        }
                        Ok(CommandResult::Output("".to_string()))
                    }
                }
            }
            Command::Next(variable_name) => match env.loop_line_numbers.get(variable_name) {
                None => Ok(CommandResult::Output("".to_string())),
                Some(line_number) => Ok(CommandResult::Jump(*line_number)),
            },
        }
    }
}

enum LinesLimit {
    NoLimit,
    Limit(i32),
}

impl LinesLimit {
    fn decrement(&self) -> LinesLimit {
        match self {
            LinesLimit::NoLimit => LinesLimit::NoLimit,
            LinesLimit::Limit(x) => LinesLimit::Limit(x - 1),
        }
    }
}

#[derive(Default)]
struct Program {
    lines: BTreeMap<i32, Command>,
}

impl Program {
    fn new() -> Program {
        Program {
            lines: BTreeMap::new(),
        }
    }

    fn add_line(&mut self, line_number: i32, command: Command) -> &mut Self {
        self.lines.insert(line_number, command);
        self
    }

    fn run(
        &self,
        lines_limit: LinesLimit,
        user_input_reader: UserInputReader,
    ) -> Result<String, String> {
        let mut output = String::new();
        let mut env = Env::new(user_input_reader);

        self.run_helper(lines_limit, &mut output, &mut env, self.lines.clone())
    }

    fn run_helper(
        &self,
        lines_limit: LinesLimit,
        output: &mut String,
        env: &mut Env,
        lines: BTreeMap<i32, Command>,
    ) -> Result<String, String> {
        for (_, command) in lines {
            match &(command.run(env)) {
                Ok(CommandResult::Output(text)) => output.push_str(text),
                Ok(CommandResult::Jump(line_number)) => {
                    match lines_limit {
                        LinesLimit::NoLimit => {}
                        LinesLimit::Limit(x) if x >= 1 => {}
                        LinesLimit::Limit(_) => return Ok(output.to_string()),
                    }

                    let mut child_lines: BTreeMap<i32, Command> = BTreeMap::new();
                    for (l, command) in &self.lines {
                        if l >= line_number {
                            let unboxed_command: Command = command.clone();
                            child_lines.insert(*l, unboxed_command);
                        }
                    }
                    return self.run_helper(
                        lines_limit.decrement(),
                        output,
                        env,
                        child_lines.clone(),
                    );
                }
                Ok(CommandResult::Stop(text)) => {
                    output.push_str(text);
                    return Ok(output.to_string());
                }
                Err(msg) => return Err(msg.to_string()),
            }
        }
        Ok(output.to_string())
    }
}

#[cfg(test)]
mod tests {
    use crate::UserInputReader::RealStdin;
    use crate::*;
    use Command::*;

    #[test]
    fn let_and_print() {
        let mut program = Program::new();
        program.add_line(20, Print(vec![Expression::number_variable("a")]));
        program.add_line(10, Let("a".to_string(), Expression::Integer(10)));
        let output = program.run(LinesLimit::NoLimit, UserInputReader::RealStdin);
        assert_eq!(output, Ok("10\n".to_string()));
    }

    #[test]
    fn print_adding_vars() {
        let mut program = Program::new();
        let number_variable = Expression::number_variable;
        program.add_line(20, Print(vec![number_variable("a") + number_variable("b")]));
        program.add_line(10, Let("a".to_string(), Expression::Integer(10)));
        program.add_line(15, Let("b".to_string(), Expression::Integer(15)));

        assert_eq!(
            program.run(LinesLimit::NoLimit, UserInputReader::RealStdin),
            Ok("25\n".to_string())
        );
    }

    #[test]
    fn print_with_comma() {
        let mut program = Program::new();
        program.add_line(10, Let("a".to_string(), Expression::Integer(10)));
        program.add_line(15, Let("b".to_string(), Expression::Integer(15)));
        program.add_line(
            20,
            Print(vec![
                Expression::number_variable("a"),
                Expression::number_variable("b"),
            ]),
        );

        assert_eq!(
            program.run(LinesLimit::NoLimit, UserInputReader::RealStdin),
            Ok("10 15\n".to_string())
        );
    }

    #[test]
    fn rem() {
        let mut program = Program::new();
        program.add_line(10, Rem(String::new()));

        assert_eq!(
            program.run(LinesLimit::NoLimit, UserInputReader::RealStdin),
            Ok(String::new())
        );
    }

    #[test]
    fn print_with_no_expressions() {
        let mut program = Program::new();
        program.add_line(10, Print(vec![]));

        assert_eq!(
            program.run(LinesLimit::NoLimit, UserInputReader::RealStdin),
            Ok("\n".to_string())
        );
    }

    // PRINT 90-32
    #[test]
    fn subtract() {
        let mut program = Program::new();
        program.add_line(
            10,
            Print(vec![Expression::Integer(90) - Expression::Integer(32)]),
        );

        assert_eq!(
            program.run(LinesLimit::NoLimit, UserInputReader::RealStdin),
            Ok("58\n".to_string())
        );
    }

    // PRINT 58*5
    #[test]
    fn multiply() {
        let mut program = Program::new();
        program.add_line(
            10,
            Print(vec![Expression::Integer(58) * Expression::Integer(5)]),
        );

        assert_eq!(
            program.run(LinesLimit::NoLimit, UserInputReader::RealStdin),
            Ok("290\n".to_string())
        );
    }

    // 10 PRINT (41-32)*5/9
    #[test]
    fn print_arithmetic() {
        let mut program = Program::new();
        program.add_line(
            10,
            Print(vec![
                (Expression::Integer(41) - Expression::Integer(32)) * Expression::Integer(5)
                    / Expression::Integer(9),
            ]),
        );

        assert_eq!(
            program.run(LinesLimit::NoLimit, UserInputReader::RealStdin),
            Ok("5\n".to_string())
        );
    }

    // 10 PRINT "Hello, World!"
    // 20 GO TO 10
    #[test]
    fn goto() {
        let mut program = Program::new();
        program.add_line(10, Print(vec![Expression::text("Hello, World!")]));
        program.add_line(20, Goto(10));

        assert_eq!(
            program.run(LinesLimit::Limit(1), UserInputReader::RealStdin),
            Ok("Hello, World!\nHello, World!\n".to_string())
        );
    }

    // 10 INPUT "Enter deg F", F
    // 20 PRINT "You gave ", F
    #[test]
    fn input() {
        let mut program = Program::new();
        program.add_line(
            10,
            Input(vec![
                Expression::text("Enter deg F"),
                Expression::number_variable("F"),
            ]),
        );
        program.add_line(
            20,
            Print(vec![
                Expression::text("You gave"),
                Expression::number_variable("F"),
            ]),
        );

        assert_eq!(
            program.run(
                LinesLimit::NoLimit,
                UserInputReader::FakeStdin("42.0".to_string())
            ),
            Ok("Enter deg F\nYou gave 42\n".to_string())
        );
    }

    // 10 REM temperature conversion
    // 20 PRINT "deg F", "deg C"
    // 30 PRINT
    // 40 INPUT "Enter deg F", F
    // 50 PRINT F,(F-32)*5/9
    // 60 GO TO 40
    #[test]
    fn goto_plus_input() {
        let mut program = Program::new();
        program.add_line(10, Rem(String::from("temperature conversion")));
        program.add_line(
            20,
            Print(vec![Expression::text("deg F"), Expression::text("deg C")]),
        );
        program.add_line(30, Print(vec![]));
        program.add_line(
            40,
            Input(vec![
                Expression::text("Enter deg F"),
                Expression::number_variable("F"),
            ]),
        );
        program.add_line(
            50,
            Print(vec![
                Expression::number_variable("F"),
                (Expression::number_variable("F") - Expression::Integer(32))
                    * Expression::Integer(5)
                    / Expression::Integer(9),
            ]),
        );
        program.add_line(60, Goto(40));

        assert_eq!(
            program.run(LinesLimit::Limit(2), UserInputReader::FakeStdin("42.0".to_string())),
            Ok("deg F deg C\n\nEnter deg F\n42 5.555555555555555\nEnter deg F\n42 5.555555555555555\nEnter deg F\n42 5.555555555555555\n".to_string())
        );
    }

    // 26 INPUT a
    // 27 CLS
    // 30 INPUT “Guess the number”, b
    // 40 IF b=a THEN PRINT “That is correct”:,STOP
    // 56 IF b<a THEN PRINT “That is too small, try again”
    // 66 IF b>a THEN PRINT “That is too big, try again”
    // 70 GO TO 36
    #[test]
    fn guess_number() {
        let mut program = Program::new();
        program.add_line(26, Input(vec![Expression::number_variable("a")]));
        program.add_line(
            30,
            Input(vec![
                Expression::text("Guess the number"),
                Expression::number_variable("b"),
            ]),
        );
        program.add_line(
            40,
            If(
                Expression::number_variable("b").are_equal(Expression::number_variable("a")),
                vec![Print(vec![Expression::text("That is correct")]), Stop],
            ),
        );
        program.add_line(
            56,
            If(
                Expression::number_variable("b").less_than(Expression::number_variable("a")),
                vec![Print(vec![Expression::text(
                    "That is too small, try again",
                )])],
            ),
        );
        program.add_line(
            66,
            If(
                Expression::number_variable("b").greater_than(Expression::number_variable("a")),
                vec![Print(vec![Expression::text("That is too big, try again")])],
            ),
        );
        program.add_line(70, Goto(36));

        assert_eq!(
            program.run(
                LinesLimit::NoLimit,
                UserInputReader::FakeStdin("42.0".to_string())
            ),
            Ok("Guess the number\nThat is correct\n".to_string())
        );
    }

    // 5 PRINT "Expected"
    // 10 IF 4 > 3 THEN STOP
    // 20 PRINT "Failed to stop"
    #[test]
    fn stop() {
        let mut program = Program::new();
        program.add_line(5, Print(vec![Expression::text("Expected")]));
        program.add_line(
            10,
            If(
                Expression::Integer(4).greater_than(Expression::Integer(3)),
                vec![Stop],
            ),
        );

        assert_eq!(
            program.run(LinesLimit::NoLimit, UserInputReader::RealStdin),
            Ok("Expected\n".to_string())
        );
    }

    // 10 IF 4 > 3 THEN PRINT "Foo": PRINT "Bar"
    #[test]
    fn if_with_two_commands() {
        let mut program = Program::new();
        program.add_line(
            10,
            If(
                Expression::Integer(4).greater_than(Expression::Integer(3)),
                vec![
                    Print(vec![Expression::text("Foo")]),
                    Print(vec![Expression::text("Bar")]),
                ],
            ),
        );

        assert_eq!(
            program.run(LinesLimit::NoLimit, UserInputReader::RealStdin),
            Ok("Foo\nBar\n".to_string())
        );
    }

    // 10 IF 4 > 3 THEN PRINT "Foo": STOP
    #[test]
    fn if_with_print_and_stop() {
        let mut program = Program::new();
        program.add_line(
            10,
            If(
                Expression::Integer(4).greater_than(Expression::Integer(3)),
                vec![Print(vec![Expression::text("Foo")]), Stop],
            ),
        );
    }

    // 10 FOR n=1 TO 10
    // 20 PRINT n
    // 30 NEXT n"
    #[test]
    fn test_for_loop() {
        let mut program = Program::new();
        program.add_line(
            10,
            Command::for_loop(
                "n",
                Expression::Integer(1),
                Expression::Integer(10),
                Expression::Integer(1),
            ),
        );
        program.add_line(20, Print(vec![Expression::number_variable("n")]));
        program.add_line(30, Command::next("n"));

        assert_eq!(
            program.run(LinesLimit::Limit(100), RealStdin),
            Ok("1\n2\n3\n4\n5\n6\n7\n8\n9\n10\n".to_string())
        );
    }

    // 10 FOR n=10 TO 1 STEP -1
    // 20 PRINT n
    // 30 NEXT n
    #[test]
    fn for_with_step() {
        let mut program = Program::new();
        program.add_line(
            10,
            Command::for_loop(
                "n",
                Expression::Integer(10),
                Expression::Integer(1),
                Expression::Integer(-1),
            ),
        );
        program.add_line(20, Print(vec![Expression::number_variable("n")]));
        program.add_line(30, Command::next("n"));

        assert_eq!(
            program.run(LinesLimit::Limit(100), RealStdin),
            Ok("10\n9\n8\n7\n6\n5\n4\n3\n2\n1\n".to_string())
        );
    }

    // 5 LET n = 0
    // 10 FOR a = 1 TO 2
    // 20 FOR b = 2 TO 3
    // 30 LET c = a*b
    // 40 IF c>n THEN LET n = c
    // 50 NEXT b
    // 60 NEXT a
    // 70 PRINT n
    #[test]
    fn nested_for() {
        let mut program = Program::new();
        program.add_line(5, Command::let_equal("n", Expression::Integer(0)));
        program.add_line(
            10,
            Command::for_loop(
                "a",
                Expression::Integer(1),
                Expression::Integer(2),
                Expression::Integer(1),
            ),
        );
        program.add_line(
            20,
            Command::for_loop(
                "b",
                Expression::Integer(2),
                Expression::Integer(3),
                Expression::Integer(1),
            ),
        );
        program.add_line(
            30,
            Command::let_equal(
                "c",
                Expression::number_variable("a") * Expression::number_variable("b"),
            ),
        );
        program.add_line(
            40,
            If(
                Expression::number_variable("c").greater_than(Expression::number_variable("n")),
                vec![Command::let_equal("n", Expression::number_variable("c"))],
            ),
        );
        program.add_line(50, Command::next("b"));
        program.add_line(60, Command::next("a"));
        program.add_line(70, Print(vec![Expression::number_variable("n")]));
        assert_eq!(
            program.run(LinesLimit::NoLimit, RealStdin),
            Ok("6\n".to_string())
        );
    }
}

fn main() {
    println!("Hello, world!");
}
