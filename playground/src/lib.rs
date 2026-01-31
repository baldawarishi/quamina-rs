use quamina::Quamina;
use serde::Serialize;
use std::cell::RefCell;
use wasm_bindgen::prelude::*;

// Thread-local Quamina instance for the playground
thread_local! {
    static MATCHER: RefCell<Quamina<String>> = RefCell::new(Quamina::new());
}

#[wasm_bindgen]
extern "C" {
    #[wasm_bindgen(js_namespace = console)]
    fn log(s: &str);
}

#[derive(Serialize)]
pub struct AddPatternResult {
    pub success: bool,
    pub error: Option<String>,
    pub time_us: f64,
}

#[derive(Serialize)]
pub struct MatchResult {
    pub matches: Vec<String>,
    pub time_us: f64,
    pub error: Option<String>,
}

/// Clear all patterns and reset the matcher
#[wasm_bindgen]
pub fn clear_patterns() {
    MATCHER.with(|m| {
        *m.borrow_mut() = Quamina::new();
    });
}

/// Add a pattern to the matcher
/// Returns JSON with { success, error?, time_us }
#[wasm_bindgen]
pub fn add_pattern(id: &str, pattern_json: &str) -> JsValue {
    let start = js_sys::Date::now();

    let result = MATCHER.with(|m| {
        let mut matcher = m.borrow_mut();
        matcher.add_pattern(id.to_string(), pattern_json)
    });

    let elapsed = js_sys::Date::now() - start;

    let result = match result {
        Ok(()) => AddPatternResult {
            success: true,
            error: None,
            time_us: elapsed * 1000.0,
        },
        Err(e) => AddPatternResult {
            success: false,
            error: Some(e.to_string()),
            time_us: elapsed * 1000.0,
        },
    };

    serde_wasm_bindgen::to_value(&result).unwrap()
}

/// Match an event against all patterns
/// Returns JSON with { matches: string[], time_us, error? }
#[wasm_bindgen]
pub fn match_event(event_json: &str) -> JsValue {
    let start = js_sys::Date::now();

    let result = MATCHER.with(|m| {
        let matcher = m.borrow();
        matcher.matches_for_event(event_json.as_bytes())
    });

    let elapsed = js_sys::Date::now() - start;

    let result = match result {
        Ok(matches) => MatchResult {
            matches,
            time_us: elapsed * 1000.0,
            error: None,
        },
        Err(e) => MatchResult {
            matches: vec![],
            time_us: elapsed * 1000.0,
            error: Some(e.to_string()),
        },
    };

    serde_wasm_bindgen::to_value(&result).unwrap()
}

/// Get pattern count (for debugging)
#[wasm_bindgen]
pub fn pattern_count() -> usize {
    MATCHER.with(|m| {
        let _matcher = m.borrow();
        // We don't have a direct count method, so this is approximate
        0 // TODO: expose pattern count from Quamina if needed
    })
}
