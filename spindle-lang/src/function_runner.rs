use std::io::Stdout;
use std::sync::{Arc, LockResult, Mutex};
use std::time::Duration;
use futures::executor::block_on;
use crate::analyzer::Pipe::Standard;
use crate::environment::Environment;
use crate::evaluator::EvaluationError;
use crate::types::{Function, ReturnState, Type};
use crate::values::Value;



#[derive(Clone)]
enum FunctionRunner {
    ValueContainer(Value),
    VarReference(String),
    Assignment(String, Box<FunctionRunner>),
    PipeFunction {
        func : Function,
        args : Vec<FunctionRunner>,
        target : Box<Option<FunctionRunner>>,
    },
    PullFunction {
        func : Function,
        args : Vec<FunctionRunner>,
        target : Box<Option<FunctionRunner>>,
    },
    PushFunction {
        func : Function,
        args : Vec<FunctionRunner>,
        target : Box<Option<FunctionRunner>>,
    }
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum RunProgress {
    InProgress,
    Done,
}

pub struct RunStatus {
    pub progress_message : Option<String>,
    pub progress : RunProgress,
    pub call_trace : Vec<String>,
}

impl RunStatus {
    pub fn new() -> Arc<Mutex<RunStatus>> {
        Arc::new(Mutex::new(RunStatus {
            progress_message : None,
            progress: RunProgress::InProgress,
            call_trace : vec![],
        }))
    }

    pub fn done(&mut self) {
        self.progress = RunProgress::Done
    }
}


impl FunctionRunner {

    pub async fn start(&mut self, env : Arc<Mutex<Environment>>, run_status : Arc<Mutex<RunStatus>>) -> Result<Value, EvaluationError> {
        let result = self.start_sync(env, run_status.clone(), None);
        match run_status.lock() {
            Ok(mut rs) => {
                rs.done();
            }
            Err(e) => {
                panic!("Can't get run_status lock for some reason: {}", e)
            }
        }

        result
    }

    fn start_sync(&mut self, env : Arc<Mutex<Environment>>, run_status : Arc<Mutex<RunStatus>>, carry_val : Option<Value>) -> Result<Value, EvaluationError> {
        match self {
            FunctionRunner::ValueContainer(v) => {
                Ok(v.to_owned())
            }
            FunctionRunner::PipeFunction {
                func,
                args,
                target } => {
                Self::start_pipe_sync(env, func, args, target, run_status, carry_val)
            }
            FunctionRunner::PullFunction {
                func,
                args,
                target } => {
                Self::start_pull_sync(env, func, args, target, run_status, carry_val)
            }
            FunctionRunner::PushFunction {
                func,
                args,
                target } => {

                // treat it like a normal pipe, and handle the 'Collect' phase when the pull is complete
                Self::start_pipe_sync(env, func, args, target, run_status, carry_val)
            }
            FunctionRunner::VarReference(id) => {
                let e = env.lock().expect("Cant get env lock for some reason");
                match e.resolve_val(id.clone().as_str()) {
                    None => {
                        Err(EvaluationError::new(format!("Invalid variable identifier [{}]", id).as_str()))
                    }
                    Some(v) => {
                        Ok(v)
                    }
                }
            }
            FunctionRunner::Assignment(id, func) => {
                let v = func.start_sync(env.clone(), run_status, carry_val)?;
                env.lock().expect("Can't lock environment!").store_value(id, v.clone());
                Ok(v)
            }
        }
    }


    fn start_pipe_sync(
        env : Arc<Mutex<Environment>>,
        func: &mut Function,
        args: &mut Vec<FunctionRunner>,
        target: &mut Box<Option<FunctionRunner>>,
        run_status: Arc<Mutex<RunStatus>>,
        carry_val: Option<Value>) -> Result<Value, EvaluationError> {
        let arg_values = Self::run_arg_functions(env.clone(), args, &run_status, carry_val)?;

        Self::record_call_trace(func, &run_status);

        let fcall = func.create_call(arg_values);
        let val = fcall.run()?;
        if let Some(mut target_runner) = *target.clone() {
            target_runner.start_sync(env.clone(), run_status.clone(), Some(val))
        } else {
            Ok(val)
        }
    }

    fn start_pull_sync(
        env : Arc<Mutex<Environment>>,
        func: &mut Function,
        args: &mut Vec<FunctionRunner>,
        target: &mut Box<Option<FunctionRunner>>,
        run_status: Arc<Mutex<RunStatus>>,
        carry_val: Option<Value>) -> Result<Value, EvaluationError> {
        let arg_values = Self::run_arg_functions(env.clone(), args, &run_status, carry_val)?;

        Self::record_call_trace(func, &run_status);

        let fcall = func.create_call(arg_values);
        let mut result_vals = vec![];
        let mut item_type = func.sig.value.clone();
        while fcall.mut_state().lock().expect("Can't unlock function state!").has_more() {
            let val = fcall.run()?;
            if let Some(mut target_runner) = *target.clone() {
                let v = target_runner.start_sync(
                    env.clone(), run_status.clone(), Some(val))?;
                result_vals.push(v.clone());
            } else {
                result_vals.push(val.clone());
            }
        }

        Ok(Value::ValueList {
            item_type,
            vals: result_vals,
        })
    }

    /// collect the results of a downstream push function
    fn collect_sync(&mut self,
                    env : Arc<Mutex<Environment>>,
                    run_status : Arc<Mutex<RunStatus>>,
                    carry_val : Option<Value>) -> Result<Option<Value>, EvaluationError> {
        match self {
            FunctionRunner::PushFunction { func, args, target } => {
                let arg_values = FunctionRunner::run_arg_functions(env.clone(), args, &run_status, carry_val)?;
                let result = func.create_call(arg_values).collect()?;

                match *target.clone() {
                    None => { Ok(Some(result))}
                    Some(mut t) => {
                        Ok(Some(t.start_sync(env, run_status, Some(result))?))
                    }
                }
            }
            FunctionRunner::PipeFunction { func, args, target } |
            FunctionRunner::PullFunction { func, args, target } => {
                match *target.clone() {
                    None => {
                        Ok(None)
                    }
                    Some(mut t) => {
                        t.collect_sync(env, run_status, None)
                    }
                }
            }
            _ => {
                // nothing to collect
                Ok(None)
            }
        }
    }

    fn record_call_trace(func: &mut Function, run_status: &Arc<Mutex<RunStatus>>) {
        match run_status.lock() {
            Ok(mut rs) => {
                rs.call_trace.push(func.name.clone())
            }
            Err(e) => {
                panic!("Can't get run_status lock for some reason: {}", e)
            }
        }
    }

    fn run_arg_functions(
        env: Arc<Mutex<Environment>>,
        args: &mut Vec<FunctionRunner>,
        run_status: &Arc<Mutex<RunStatus>>,
        carry_val: Option<Value>) -> Result<Vec<Value>, EvaluationError> {

        let mut arg_values = vec![];
        for arg in args {
            let v = arg.start_sync(env.clone(), run_status.clone(), None)?;
            arg_values.push(v);
        }
        if let Some(v) = carry_val {
            arg_values.push(v)
        }
        Ok(arg_values)
    }
}

#[cfg(test)]
mod tests {
    use std::sync::{Arc, Mutex};
    use std::time::Duration;
    use futures::executor::block_on;
    use num_bigint::BigInt;
    use crate::environment::Environment;
    use crate::function_runner::{FunctionRunner, RunProgress, RunStatus};
    use crate::functions;
    use crate::functions::get_builtins;
    use crate::types::{Function, Type};
    use crate::value_store::InMemoryValueStore;
    use crate::values::Value;

    fn setup_env() -> Arc<Mutex<Environment>> {
        let mut env = Environment::new(Box::new(InMemoryValueStore::create()));
        env.put_functions(get_builtins());
        Arc::new(Mutex::new(env))
    }

    #[test]
    fn test_with_value() {
        let env = setup_env();
        let mut runner = FunctionRunner::PipeFunction {
            func: functions::SpecialFunctions::id(),
            args: vec![FunctionRunner::ValueContainer(Value::str_val("hi"))],
            target: Box::new(None)
        };

        let run_status = RunStatus::new();
        let f = async {
            runner.start(env, run_status.clone()).await
        };
        if let Ok(Value::ValueString { val }) = block_on(f) {
            assert_eq!(val, String::from("hi"))
        } else {
            panic!("Expected match")
        }

        let unlocked = run_status.lock().expect("Unable to unlock run status!");
        assert_eq!(1, unlocked.call_trace.len());
        assert_eq!("id", unlocked.call_trace.get(0).unwrap().as_str());
    }

    #[test]
    fn test_with_wait() {
        let env = setup_env();
        let mut runner = FunctionRunner::PipeFunction {
            func: functions::SpecialFunctions::wait(),
            args: vec![FunctionRunner::ValueContainer(Value::int_val(2))],
            target: Box::new(None)
        };

        let run_status = RunStatus::new();
        if let Ok(Value::ValueVoid) = block_on(async {
            let result = runner.start(env, run_status.clone());
            assert_eq!(RunProgress::InProgress, run_status.lock().unwrap().progress);
            result.await
        }) {
            // expected
        } else {
            panic!("Expected void result")
        }

        let unlocked = run_status.lock().expect("Unable to unlock run status!");
        assert_eq!(1, unlocked.call_trace.len());
        assert_eq!("wait", unlocked.call_trace.get(0).unwrap().as_str());
        assert_eq!(RunProgress::Done, unlocked.progress)
    }

    #[test]
    fn test_with_value_pipe() {
        let env = setup_env();
        let add_fn = find_int_valued(env.lock().expect("Unable to lock environment!").get_funcs("+"));
        let mult_fn = find_int_valued(env.lock().expect("Unable to lock environment!").get_funcs("*"));
        let inc_fn = find_int_valued(env.lock().expect("Unable to lock environment!").get_funcs("inc"));

        let mut target_runner_1 = FunctionRunner::PipeFunction {
            func: inc_fn,
            args: vec![],
            target: Box::new(None)
        };

        let mut inner_runner_1 = FunctionRunner::PipeFunction {
            func: mult_fn,
            args: vec![
                FunctionRunner::ValueContainer(Value::int_val(2)),
                FunctionRunner::ValueContainer(Value::int_val(3)),
            ],
            target: Box::new(None)
        };

        let mut runner = FunctionRunner::PipeFunction {
            func: add_fn,
            args: vec![
                FunctionRunner::ValueContainer(Value::int_val(1)),
                inner_runner_1,
            ],
            target: Box::new(Some(target_runner_1))
        };

        let run_status = RunStatus::new();
        let f = async {
            runner.start(env, run_status.clone()).await
        };
        if let Ok(Value::ValueIntegral { val }) = block_on(f) {
            assert_eq!(val, BigInt::from(8))
        } else {
            panic!("Expected match")
        }

        let unlocked = run_status.lock().expect("Unable to unlock run status!");
        assert_eq!(3, unlocked.call_trace.len());
    }

    fn find_int_valued(fns: Option<&Vec<Function>>) -> Function {
        match fns {
            None => { }
            Some(fns) => {
                for a_fn in fns {
                    if a_fn.sig.value.eq(&Type::Integral) {
                        return a_fn.clone();
                    }
                }
            }
        }
        panic!("Can't find expected int valued function");
    }

}