
use std::sync::RwLock;

static LOG_ENABLED: std::sync::RwLock<bool> = RwLock::new(false);


/// Sets whether debug log is enable
pub fn set_log_enabled(val: bool) {
    let mut v = LOG_ENABLED.write().unwrap();
    *v = val;
}

/// Returns true if log is enabled
pub fn is_log_enabled() -> bool {
    return *LOG_ENABLED.read().unwrap();
}

/// Writes message to log
macro_rules! debug_log {
    ($($x:tt)*) => {
        if is_log_enabled() {
            println!($($x)*)
        }
    }
}

pub(crate) use debug_log;
