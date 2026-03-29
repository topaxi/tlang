mod event_loop;
mod io_worker;
mod runtime;
mod stdlib;

use runtime::Runtime;

fn main() -> std::io::Result<()> {
    env_logger::builder()
        .filter_level(log::LevelFilter::Warn)
        .parse_default_env()
        .init();

    let mut runtime = Runtime::new()?;

    log::info!("octarine runtime started");

    runtime.run()?;

    log::info!("octarine runtime finished");

    Ok(())
}
