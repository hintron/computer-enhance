// Internal imports
use crate::decode::{decode, DecodeSettings};

#[test]
fn test_mov_reg_reg() {
    println!("===========================Test 1.1=============================");
    let inst_stream = vec![0x89, 0xD9, 0x88, 0xD9];
    let expected_insts = vec!["mov cx, bx", "mov cl, bl"];
    check_decode(inst_stream, expected_insts);
}

fn check_decode(inst_stream: Vec<u8>, expected_insts: Vec<&str>) {
    let decode_settings = DecodeSettings {
        verbose: true,
        ..Default::default()
    };
    let insts = decode(inst_stream, &decode_settings);
    let mut inst_num = 1;
    for (inst, expected_inst) in std::iter::zip(insts, expected_insts) {
        // Unwrap as ref to borrow inst.text, so we can print inst later
        let actual_inst = inst.text.as_ref().unwrap();
        println!("-------------------------Inst {inst_num}-------------------------------");
        println!("Expected inst: {}", expected_inst);
        println!("Actual   inst: {}", actual_inst);
        if actual_inst != expected_inst {
            println!("++++++++++++++++++++++++++");
            println!("{:?}", expected_inst.chars());
            println!("{:?}", actual_inst.chars());
            println!("{:#?}", inst);
            println!("---------------------------------------------------------------------");
        }
        assert!(actual_inst == expected_inst);
        inst_num += 1;
    }
    println!("---------------------------------------------------------------------");
}
