use anyhow::Result;
use criterion::{black_box, criterion_group, criterion_main, Criterion};

use emu86rs::decode::{decode, decode_execute, DecodeSettings, ExecuteSettings};
use emu86rs::file_to_byte_vec;

fn benchmark_decode(input: &str) -> Result<()> {
    let decode_settings = DecodeSettings {
        ..Default::default()
    };

    let (inst_stream, _) = file_to_byte_vec(&Some(input.to_string()), false)?;
    let _insts = decode(inst_stream, &decode_settings);
    Ok(())
}

fn benchmark_decode_execute(input: &str) -> Result<()> {
    let decode_settings = DecodeSettings {
        ..Default::default()
    };
    let execute_settings = ExecuteSettings {
        ..Default::default()
    };

    let (inst_stream, program_length) = file_to_byte_vec(&Some(input.to_string()), true)?;
    let _output = decode_execute(
        inst_stream,
        program_length,
        &decode_settings,
        &execute_settings,
        None,
    );
    Ok(())
}

fn criterion_benchmark(c: &mut Criterion) {
    c.bench_function("Decode-only of listing 42", |b| {
        b.iter(|| {
            benchmark_decode(black_box(
                "files/bench-data/listing_0042_completionist_decode",
            ))
        })
    });
    c.bench_function("Decode-and-simulate of listing 45", |b| {
        b.iter(|| {
            benchmark_decode_execute(black_box(
                "files/bench-data/listing_0045_challenge_register_movs",
            ))
        })
    });
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
