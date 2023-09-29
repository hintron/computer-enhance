use anyhow::Result;
use criterion::{black_box, criterion_group, criterion_main, Criterion};

use computer_enhance::decode::{decode, decode_execute};
use computer_enhance::inst_stream_from_file;

fn benchmark_decode(input: &str) -> Result<()> {
    let inst_stream = inst_stream_from_file(&Some(input.to_string()))?;
    let _insts = decode(inst_stream, false, false);
    Ok(())
}

fn benchmark_decode_execute(input: &str) -> Result<()> {
    let inst_stream = inst_stream_from_file(&Some(input.to_string()))?;
    let _output = decode_execute(inst_stream, false, false);
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
